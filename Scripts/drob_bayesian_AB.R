library(dplyr)
library(tidyr)
library(Lahman)

pitchers = Pitching %>%
  group_by(playerID) %>%
  summarize(gamesPitched = sum(G)) %>%
  filter(gamesPitched > 3)