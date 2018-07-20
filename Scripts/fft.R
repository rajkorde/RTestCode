#https://anomaly.io/detect-seasonality-using-fourier-transform-r/

library(TSA)
library(lubridate)
library(tidyverse)
library(zoo)

options(scipen = 9999)

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

complete_ts <- function(ts, start = ymd_hms("2018-07-05 1:00:00 AM"),
                        end = ymd_hms("2018-08-05 23:00:00 PM")) {
  allts <- seq.POSIXt(as.POSIXct(start, tz="UTC"), as.POSIXct(end, tz="UTC"), by = "hours")
  
  for (i in 1:length(ts)) {
    minute(ts[i]) <- 0
    second(ts[i]) <- 0
  }
  
  data.frame(TimeStamp = allts) %>%
    mutate(Event = ifelse(TimeStamp %in% x1, 1, 0))
}

x <- simulate_timestamps(n = 5)
dx <- complete_ts(x)
plot(zoo(dx$Event, dx$TimeStamp))

a <- acf(dx$Event, lag.max = 200)

tibble(lag = a$lag[, 1, 1], acf = a$acf[, 1, 1]) %>%
  arrange(desc(acf)) %>%
  top_n(1)

data.frame(Date = x) %>%
  arrange(Date) %>%
  mutate(Next = lead(Date)) %>%
  mutate(Diff = as.numeric(difftime(Next, Date, units = c("mins"))))

z <- zoo(dx$Event, dx$TimeStamp)
plot(z)

p <- periodogram(z)
data.frame(freq=p$freq, spec=p$spec) %>%
  arrange(desc(spec))

acf(zoo(x))

dx <- data.frame(Date = as.Date(x), Event = 1)

dx <- data.frame(Date = seq.Date(as.Date(min(x)), as.Date(max(x)), by="day")) %>%
             left_join(dx) %>%
  replace_na(list(Event = 0))
  

