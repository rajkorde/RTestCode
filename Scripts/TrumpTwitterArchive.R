library(jsonlite)
library(tidyverse)
library(rvest)
library(feather)

raw <- fromJSON("D:/Temp/trumptwitter/TrumpTwitterArchive.json", 
                simplifyDataFrame = TRUE,
                flatten = TRUE)

d <- mutate(raw, url = paste0("https://twitter.com/realDonaldTrump/status/", id_str)) %>%
  filter(is_retweet == FALSE)

#url <- "https://twitter.com/realDonaldTrump/status/919005534883328000"
#badurl1 <- "https://twitter.com/realDonaldTrump/status/91900553488332800"
#badurl2 <- "https://twitter.com/realDonaldTrump/status/761986616973942784"

get_stats <- function(url) {
  tryCatch({
    page <- read_html(url)
    d <- html_nodes(page, 
                    xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "stream-item-footer", " " ))]') %>%
      html_nodes(
        xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "ProfileTweet-actionCount", " " ))]') %>%
      html_attr(name = "data-tweet-stat-count") %>%
      as.numeric() 
    
    replies <<- d[1]
    retweets <<- d[2]
    favorites <<- d[3] 
    
  }, error = function(err) {
    print(paste("error getting", url, " !!!"))
    replies <<- NA
    retweets <<- NA
    favorites <<- NA 
  })

  list(replies = replies, retweets = retweets, favorites = favorites)
}


tab <- d %>%
  rowwise() %>%
  bind_cols(do(., {
    stats <- get_stats(.$url)
    print(paste("getting ", .$url))
    data.frame(replies = stats$replies,
                  retweets = stats$retweets,
                  favorites = stats$favorites)
  }))

write_feather(tab, path = "trumptwitter.feather")


# Analysis

invisible_tweets <- filter(tab, grepl(pattern = "private lunch|LIMITED EDITION", x = text)) %>%
  select(id_str)

ratio_tweets <- filter(tab, replies > 50 & favorites > 50) %>%
  filter(!id_str %in% invisible_tweets$id_str) %>%
  mutate(ratio = replies/favorites) %>%
  arrange(desc(ratio))



## graph by week



library(stringr)
library(lubridate)

month_lookup <- 1:12
names(month_lookup) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                         "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

fix_date <- function(dt) {
  y <- str_split(dt, " ")[[1]][c(2, 3, 6)]
  y[1] <- unname(month_lookup[y[1]])
  return(mdy(paste(y, collapse = " ")))
}

d <- ratio_tweets %>% 
  rowwise() %>%
  bind_cols(do(., {
    data.frame(date = fix_date(.$created_at))
  })) %>%
  ungroup() %>%
  mutate(week = week(date))


by_week <- d %>%
  filter(date > mdy("1/20/2017")) %>%
  group_by(week) %>%
  summarise(replies = sum(replies), favorites = sum(favorites)) %>%
  mutate(ratio = replies/favorites) 


ggplot(by_week, aes(week, ratio)) + 
  geom_line() +
  geom_smooth(se = FALSE, method = "lm") + 
  theme_bw()


