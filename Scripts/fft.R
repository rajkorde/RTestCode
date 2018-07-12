#https://anomaly.io/detect-seasonality-using-fourier-transform-r/

library(TSA)
library(lubridate)
library(tidyverse)
raw <- read.csv("C:/Users/rajkorde.NTDEV/Downloads/20131120-20151110-google-analytics.csv")
p = periodogram(raw$Visite)

dd = data.frame(freq=p$freq, spec=p$spec)
order = dd[order(-dd$spec),]
top2 = head(order, 2)

time = 1/top2$f
time


simulate_timestamps <- function(starttime = ymd_hms("2018-07-01 8:05:23 AM"),
                     periodicity = 7,
                     jitter = 60,
                     n = 7) {
  starttime + minutes(as.integer((1:n)*periodicity*24*60 + (2*(runif(n) - 0.5))*jitter))
}

x <- simulate_timestamps(periodicity = 15)


p <- periodogram(ts(x))
data.frame(freq=p$freq, spec=p$spec) %>%
  arrange(desc(spec))
