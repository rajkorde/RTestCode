#http://multithreaded.stitchfix.com/blog/2016/01/13/market-watch/



library(devtools)
library(ggplot2)

install.packages("dtw")
install_github("google/CausalImpact")
install_github("klarsen1/MarketMatching", build_vignettes = TRUE)


library(dtw)
library(CausalImpact)
library(MarketMatching)

data(weather, package = "MarketMatching")

ggplot(weather, aes(Date, Mean_TemperatureF, col = Area)) + geom_line()

mm <- best_matches(data = weather,
                   id_variable = "Area",
                   date_variable = "Date",
                   matching_variable = "Mean_TemperatureF",
                   parallel = TRUE,
                   warping_limit = 1,
                   dtw_emphasis = 1,
                   matches = 5, 
                   start_match_period = "2014-01-01",
                   end_match_period = "2014-10-01")

dplyr::filter(weather, Area %in% c("BOM", "MIA")) %>%
  ggplot(aes(Date, Mean_TemperatureF, col = Area)) + geom_line()

results <- MarketMatching::inference(matched_markets = mm, test_market = "CPH",
                                     end_post_period = "2015-10-01")