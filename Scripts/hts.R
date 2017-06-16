library(hts)
bts <- ts(5 + matrix(sort(rnorm(500)), ncol = 5, nrow = 100))

y <- hts(bts, nodes=list(2, c(3, 2))) # creates a 3 level heirarchical time series
ally <- aggts(y)
somey <- aggts(y, levels = c(0, 2))
s <- smatrix(y)

plot(y, levels = c(0, 1, 2))

# actual forecasting
plot(infantgts)
infantforecast <- forecast(infantgts, h = 10, method = "bu")
plot(infantforecast, include = 10)

allts_infant <- aggts(infantgts)
allf <- matrix(, nrow = 10, ncol = ncol(allts_infant))
for(i in 1:ncol(allts_infant))
  allf[,i] <- forecast(auto.arima(allts_infant[,i]), h=10, PI=FALSE)$mean
allf <- ts(allf, start=2004)
