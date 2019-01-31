library(tidyverse)
library(bursts)

offsets <- c(seq(0, 400, 100), seq(410, 450, 5), seq(451, 470, 2),
             seq(480, 600, 5), 700, seq(710, 800, 5), 900, 1000)
bursts <- kleinberg(offsets)
plot.bursts(bursts)

data <- data.frame(time = seq(0, 1000)) %>%
  mutate(event = ifelse(time %in% offsets, 1, 0))


par(mfrow=c(2, 1))
plot(data$time, data$event, type = 'b')
plot.bursts(bursts)
