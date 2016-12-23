library(ggplot2)
library(acs)
library(dplyr)
library(reshape2)
library(readr)
library(tidytext)
library(tidyr)
library(ggrepel)
library(broom)

theme_set(theme_light())

stategeo <- geo.make(state = "*")
popfetch <- acs.fetch(geography = stategeo,
                      endyear = 2014,
                      span = 5,
                      table.number = "B01003",
                      col.names = "pretty",
                      key = "7b1813dab9b8a225b26c45b147e02b93e7891ee7")

pop_df <- tbl_df(melt(estimate(popfetch))) %>%
  mutate(name = Var1,
         state_name = tolower(Var1),
         pop2014 = value) %>%
  select(name, state_name, pop2014) %>%
  filter(state_name != "puerto rico")

song_lyrics <- read_csv("Data/billboard_lyrics_1964-2015.csv")
tidy_lyrics <- bind_rows(song_lyrics %>% 
                           unnest_tokens(state_name, Lyrics),
                         song_lyrics %>%
                           unnest_tokens(state_name, Lyrics,
                                         token = "ngrams", n = 2))

tidy_lyrics <- inner_join(tidy_lyrics, pop_df) %>%
  distinct(Rank, Song, Artist, Year, state_name, .keep_all = TRUE)

state_counts <- tidy_lyrics %>%
  group_by(state_name) %>%
  dplyr::summarise(n = n())

state_counts$n[state_counts$state_name == "maine"] <- 1

state_counts <- pop_df %>%
  left_join(state_counts) %>%
  filter(!is.na(n)) %>%
  select(-state_name) %>%
  dplyr::rename(state_name = name) %>%
  mutate(rate = n/pop2014)

state_counts %>%
  arrange(desc(rate)) %>%
  top_n(10)

ggplot(state_counts, aes(rate)) +
  geom_histogram(binwidth = 2e-7, alpha = 0.8, fill = "midnightblue") +
  labs(x = "rate of mentions per population") +
  theme_minimal(base_family = "RobotoCondensed-Regular")

#method of moments to fit a beta distribution
x <- state_counts$n/state_counts$pop2014
mu <- mean(x)
sigma2 <- var(x)
alpha0 <- ((1 - mu)/sigma2 - 1/mu) * mu^2
beta0 <- alpha0 * (1/mu - 1)

ggplot(state_counts) +
  geom_histogram(aes(rate, y = ..density..), binwidth = 2e-7, alpha = 0.8,
                 fill = "midnightblue") +
  stat_function(fun = function(x) dbeta(x, alpha0, beta0), color = "red",
                size = 1) +
  labs(x = "rate of mentions per population")

#empirical bayes estimate
state_counts <- state_counts %>%
  mutate(rate_estimate = 1e6 * (n + alpha0) / (pop2014 + alpha0 + beta0),
         rate = 1e6 * rate)


ggplot(state_counts, aes(rate, rate_estimate, color = n)) +
  geom_abline(intercept = 0, slope = 1, color = "gray70", linetype = 2) +
  geom_text_repel(aes(label = state_name), color = "black",
                  box.padding = unit(0.5, 'lines'),
                  family = "RobotoCondensed-Regular") +
  geom_point(size = 4) +
  scale_color_gradient(low = "midnightblue", high = "pink",
                       name="Number\nof songs") +
  labs(title = "States in Song Lyrics with Empirical Bayes",
       subtitle = "States like Montana and Hawaii (high rates, few mentions) are shifted the most",
       x = "Measured rate of mentions per million population",
       y = "Empirical Bayes estimate of rate per million population") +
  theme_minimal(base_family = "RobotoCondensed-Regular") +
  theme(plot.title=element_text(family="Roboto-Bold"))


#posterior distribution for each state
state_counts <- state_counts %>%
  mutate(alpha1 = n + alpha0, beta1 = pop2014 - n + beta0)

counts_beta <- state_counts %>%
  arrange(desc(rate_estimate)) %>%
  top_n(5, rate_estimate) %>%
  inflate(x = seq(1e-7, 5e-6, 2e-8)) %>%
  ungroup() %>%
  mutate(density = dbeta(x, alpha1, beta1))

ggplot(counts_beta, aes(x, density, color = state_name)) +
  geom_line(size = 1.2, alpha = 0.8) +
  stat_function(fun = function(x) dbeta(x, alpha0, beta0),
                lty = 2, color = "black") +
  labs(x = "Rate of mentions per population",
       y = "Density",
       title = "Prior and Posterior Distributions",
       subtitle = "The posterior distribution for a few example states are shown\nThe prior distribution is shown as a dashed line") +
  theme_minimal(base_family = "Roboto") +
  theme(plot.title=element_text(family="Roboto")) +
  theme(legend.title=element_blank())

state_counts <- state_counts %>%
  mutate(low  = 1e6*qbeta(.025, alpha1, beta1),
         high = 1e6*qbeta(.975, alpha1, beta1))

state_counts %>% 
  arrange(desc(rate_estimate)) %>% 
  mutate(state_name = factor(state_name, levels = rev(unique(state_name)))) %>%
  select(state_name, 'Measured rate' = rate, 'Empirical Bayes estimate' = rate_estimate, low, high) %>% 
  gather(type, rate, `Measured rate`, `Empirical Bayes estimate`) %>%
  ggplot(aes(rate, state_name, color = type)) +
  geom_errorbarh(aes(xmin = low, xmax = high), color = "gray50") +
  geom_point(size = 3) +
  xlim(0, NA) +
  labs(x = "Rate of mentions per million population",
       y = NULL, title = "Measured Rates, Empirical Bayesian Estimates, and Credible Intervals",
       subtitle = "The 95% credible intervals are shown for these states") +
  theme_minimal(base_family = "Roboto") +
  theme(plot.title=element_text(family="Roboto")) +
  theme(legend.title=element_blank())
