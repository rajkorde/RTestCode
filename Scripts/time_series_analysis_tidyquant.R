# http://www.business-science.io/timeseries-analysis/2017/07/02/tidy-timeseries-analysis.html

library(tidyquant)
library(cranlogs)

pkgs <- c(
  "tidyr", "lubridate", "dplyr", 
  "broom", "tidyquant", "ggplot2", "purrr", 
  "stringr", "knitr"
)

tidyverse_downloads <- cran_downloads(packages = pkgs,
                                      from = "2017-01-01",
                                      to = "2017-06-30")
tidyverse_downloads %<>% tibble::as_tibble() %>%
  group_by(package)

tidyverse_downloads %>%
  ggplot(aes(x = date, y = count, color = package)) +
  geom_point() +
  labs(title = "tidyverse packages: Daily downloads", x = "") +
  facet_wrap(~ package, ncol = 3, scale = "free_y") +
  scale_color_tq() +
  theme_tq() +
  theme(legend.position="none")

# applying functions by period
tq_transmute_fun_options()$xts
mean_tidyverse_downloads_w <- tidyverse_downloads %>%
  tq_transmute(
    select = count,
    mutate_fun = apply.weekly,
    FUN = mean,
    na.rm = TRUE,
    col_rename = "mean_count"
  )
mean_tidyverse_downloads_w

mean_tidyverse_downloads_w %>%
  ggplot(aes(x = date, y = mean_count, color = package)) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(title = "tidyverse packages: Average daily downloads by week", x = "", 
       y = "Mean Daily Downloads by Week") +
  facet_wrap(~ package, ncol = 3, scale = "free_y") +
  expand_limits(y = 0) +
  theme_tq() +
  scale_color_tq() +
  theme(legend.position = "none")

# using custom functions in period aggregations

custom_stat_fun <- function(x, na.rm = TRUE, ...) {
  c(mean = mean(x, na.rm = na.rm),
    stdev = sd(x, na.rm = na.rm),
    quantile(x, na.rm = na.rm, ...))
}

probs <- c(0, 0.025, 0.25, 0.5, 0.75, 0.975, 1)

stats_tidyverse_downloads_w <- tidyverse_downloads %>%
  tq_transmute(
    select = count,
    mutate_fun = apply.weekly,
    FUN = custom_stat_fun,
    na.rm = TRUE,
    probs = probs)

stats_tidyverse_downloads_w %>%
  ggplot(aes(x = date, y = `50%`, color = package)) +
  geom_ribbon(aes(ymin = `25%`, ymax = `75%`), color = palette_light()[[1]],
              fill = palette_light()[[1]], alpha = 0.5) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  labs(title = "tidyverse packages: Median daily downloads by week", x = "",
       subtitle = "Range of 1st and 3rd quartile to show volatility",
       y = "Median Daily Downloads By Week") +
  facet_wrap(~ package, ncol = 3, scale = "free_y") +
  expand_limits(y = 0) +
  scale_color_tq(theme = "dark") +
  theme_tq() +
  theme(legend.position="none")

stats_tidyverse_downloads_w %>%
  ggplot(aes(x = stdev, y = mean, color = package)) +
  geom_point() +
  geom_smooth(method = "lm") + 
  labs(title = "tidyverse packages: Mean vs standard deviation of daily downloads by week") +
  facet_wrap(~ package, ncol = 3, scale = "free") +
  scale_color_tq() +
  theme_tq() +
  theme(legend.position="none")

# rolling apply

tq_mutate_fun_options()

tidyverse_downloads_rollmean <- tidyverse_downloads %>%
  tq_mutate(
    select = count,
    mutate_fun = rollapply,
    width = 28,
    alight = "right",
    FUN = mean,
    na.rm = TRUE,
    col_rename = "mean_28"
  ) %>%
  tq_mutate(
    select = count,
    mutate_fun = rollapply,
    width = 84,
    alight = "right",
    FUN = mean,
    na.rm = TRUE,
    col_rename = "mean_84"
  )

tidyverse_downloads_rollmean %>%
  ggplot(aes(x = date, y = count, color = package)) +
  # Data
  geom_point(alpha = 0.1) +
  geom_line(aes(y = mean_28), color = palette_light()[[1]], size = 1) +
  geom_line(aes(y = mean_84), color = palette_light()[[2]], size = 1) +
  facet_wrap(~ package, ncol = 3, scale = "free_y") +
  # Aesthetics
  labs(title = "tidyverse packages: Daily Downloads", x = "",
       subtitle = "28 and 84 Day Moving Average") +
  scale_color_tq() +
  theme_tq() +
  theme(legend.position="none")

tidyverse_downloads_rollmean %>%
  ggplot(aes(x = date, color = package)) +
  # Data
  # geom_point(alpha = 0.5) +  # Drop "count" from plots
  geom_line(aes(y = mean_28), color = palette_light()[[1]], linetype = 1, size = 1) +
  geom_line(aes(y = mean_84), color = palette_light()[[2]], linetype = 1, size = 1) +
  facet_wrap(~ package, ncol = 3, scale = "free_y") +
  # Aesthetics
  labs(title = "tidyverse packages: Daily downloads", x = "", y = "",
       subtitle = "Zoomed In: 28 and 84 Day Moving Average") +
  scale_color_tq() +
  theme_tq() +
  theme(legend.position="none")

# custom functions for rolling apply

custom_stat_fun_2 <- function(x, na.rm = TRUE) {
  m  <- mean(x, na.rm = na.rm)
  s  <- sd(x, na.rm = na.rm)
  hi <- m + 2*s
  lo <- m - 2*s
  
  ret <- c(mean = m, stdev = s, hi.95 = hi, lo.95 = lo) 
  return(ret)
}

tidyverse_downloads_rollstats <- tidyverse_downloads %>%
  tq_mutate(
    select     = count,
    mutate_fun = rollapply, 
    # rollapply args
    width      = 28,
    align      = "right",
    by.column  = FALSE,
    FUN        = custom_stat_fun_2,
    # FUN args
    na.rm      = TRUE
  )

tidyverse_downloads_rollstats %>%
  ggplot(aes(x = date, color = package)) +
  # Data
  geom_point(aes(y = count), color = "grey40", alpha = 0.5) +
  geom_ribbon(aes(ymin = lo.95, ymax = hi.95), alpha = 0.4) +
  geom_point(aes(y = mean), size = 1, alpha = 0.5) +
  facet_wrap(~ package, ncol = 3, scale = "free_y") +
  # Aesthetics
  labs(title = "tidyverse packages: Volatility and Trend", x = "",
       subtitle = "28-Day Moving Average with 95% Confidence Interval Bands (+/-2 Standard Deviations)") +
  scale_color_tq(theme = "light") +
  theme_tq() +
  theme(legend.position="none")