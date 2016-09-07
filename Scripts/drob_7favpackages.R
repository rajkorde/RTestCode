library(twitteR)
library(purrr)
library(dplyr)
library(stringr)
library(BiocInstaller)
library(tidytext)

consumerKey = "0RgrnAZ9wsdQaSbngpzsNvUwh"
consumerSecret = "UHUZXy43SoE7y8COSLm8k9fkd6AV13QKSWXEoYlAFmlGESLZIm"
accessToken = "2732354256-xCXzZeczCMX32427KU9pV7mhPAul1wIZlVfAF1A"
accessTokenSecret = "zR7j7yKN0rps5DJAxIh4zSUi2bMvu91iIWoPSV0Hs2X6K"

setup_twitter_oauth(consumerKey, consumerSecret, accessToken, accessTokenSecret)

# You'd need to set up authentication before running this
# See help(setup_twitter_oauth)
tweets <- searchTwitter("#7FavPackages", n = 3200) %>%
  map_df(as.data.frame)

# Grab only the first for each user (some had followups), and ignore retweets
tweets <- tweets %>%
  filter(!str_detect(text, "^RT ")) %>%
  arrange(created) %>%
  distinct(screenName, .keep_all = TRUE)

built_in <- tolower(sessionInfo()$basePkgs)
cran_pkgs <- tolower(rownames(available.packages()))
bioc_pkgs <- tolower(rownames(available.packages(repos = biocinstallRepos()[1:3])))
blacklist <- c("all")

spl_re <- "[^a-zA-Z\\d\\@\\#\\.]"
link_re <- "https://t.co/[A-Za-z\\d]+|&amp;"

packages <- tweets %>%
  mutate(text = str_replace_all(text, link_re, "")) %>%
  unnest_tokens(package, text, token = "regex", pattern = spl_re) %>%
  filter(package %in% c(cran_pkgs, bioc_pkgs, built_in)) %>%
  distinct(id, package) %>%
  filter(!package %in% blacklist)

pkg_counts <- packages %>% count(package, sort = TRUE)

library(ggplot2)
theme_set(theme_bw())

pkg_counts %>%
  filter(n >= 1) %>%
  mutate(package = reorder(package, n)) %>%
  ggplot(aes(package, n)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  ylab("Number of #7FavPackages mentions")

#correlations
library(widyr)


pkg_counts <- packages %>%
  count(package) %>%
  filter(n>=2)

pkg_correlations <- packages %>%
  semi_join(pkg_counts, by = "package") %>%
  pairwise_cor(package, id, sort = TRUE, upper = FALSE)
           
library(ggraph)
library(igraph)

set.seed(2016)

pkg_correlations %>%
  filter(correlation > .2) %>%
  graph_from_data_frame(vertices = pkg_counts) %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation)) +
  geom_node_point(aes(size = n), color = "lightblue") +
  theme_void() +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme(legend.position = "none")

#popularity based on Depend, Import and Suggest
library(tidyr)

requirements  <- available.packages() %>% 
  tbl_df() %>%
  unite(Requires, Depends, Imports, Suggests, sep = ",") %>%
  transmute(Package = as.character(Package),
            Requires = as.character(Requires)) %>%
  unnest(Requires = str_split(Requires, ",")) %>%
  mutate(Requires = str_replace(Requires, "\n", "")) %>%
  mutate(Requires = str_trim(str_replace(Requires, "\\(.*", ""))) %>%
  filter(!(Requires %in% c("R", "NA", "", built_in)))

package_info <- requirements %>%
  count(Package = Requires) %>%
  rename(NRequiredBy = n) %>%
  left_join(count(requirements, Package), by = "Package") %>%
  rename(NRequires = n) %>%
  replace_na(list(NRequires = 0))

packages %>%
  count(package) %>%
  inner_join(package_info, by = c(package = "Package")) %>%
  ggplot(aes(NRequiredBy, n)) +
  geom_point() +
  geom_text(aes(label = package), vjust = 1, hjust = 1,
            check_overlap = TRUE) +
  scale_x_log10() +
  xlab("Number of CRAN packages that Depend/Import/Suggest this") +
  ylab("Number of #7FavPackages mentions")

correlations <- requirements %>%
  group_by(Requires) %>%
  filter(n() >= 20) %>%
  ungroup() %>%
  pairwise_cor(Requires, Package, sort = TRUE)

cors <- correlations %>%
  filter(correlation > .2)

vertices <- package_info %>%
  filter(Package %in% cors$item1 |
           Package %in% cors$item2)

set.seed(2016)

graph_from_data_frame(cors, directed = FALSE, vertices) %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_width = correlation, edge_alpha = correlation)) +
  geom_node_point(aes(size = NRequiredBy), color = "skyblue") +
  geom_node_text(aes(label = name), size = 3,
                 check_overlap = TRUE, vjust = 1, hjust = 1) +
  scale_size_continuous(range = c(.5, 10)) +
  scale_edge_width(range = c(.5, 3)) +
  ggforce::theme_no_axes() +
  theme(legend.position = "none")

#join with hashtag data

vertices2 <- pkg_counts %>%
  rename(Favorites = n) %>%
  right_join(vertices, by = c(package = "Package")) %>%
  replace_na(list(Favorites = 0))

set.seed(2016)

graph_from_data_frame(cors, directed = FALSE, vertices2) %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_width = correlation, edge_alpha = correlation),
                 show.legend = FALSE) +
  geom_node_point(aes(size = NRequiredBy * 1.2), color = "gray") +
  geom_node_point(aes(size = NRequiredBy, color = Favorites)) +
  geom_node_text(aes(label = name), size = 3,
                 check_overlap = TRUE, vjust = 1, hjust = 1) +
  scale_size_continuous(range = c(.5, 10), guide = FALSE) +
  scale_color_gradient2(low = "white", high = "red") +
  scale_edge_width(range = c(.5, 3)) +
  ggforce::theme_no_axes()

