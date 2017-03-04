library(prophet)
library(dplyr)

df <- read.csv("Data/prophet/example_wp_peyton_manning.csv") %>%
  mutate(y = log(y))
m <- prophet(df)

future <- make_future_dataframe(m, periods = 365)
forecast <- predict(m, future)
tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
plot(m, forecast)


future <- make_future_dataframe(m, periods = 365*10)
forecast <- predict(m, future)
tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
plot(m, forecast)
prophet_plot_components(m, forecast)


#forecasting growth
df <- read.csv("Data/prophet/example_wp_R.csv")
df$y <- log(df$y)

df$cap <- 8.5

m <- prophet(df, growth = "logistic")
future <- make_future_dataframe(m, periods = 1826)
future$cap <- 8.5
fcst <- predict(m, future)
plot(m, fcst)