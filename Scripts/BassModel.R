#http://www.michalhron.net/modeling-ipad-sales-with-the-bass-model-in-r/
#millions of units
units_sold <- c(3.27,4.19,7.33,4.69,9.25,11.12,15.43,11.8,17.04,14.04,22.86,19.48,14.62,14.08)
names(units_sold) <- c("Q32010", "Q42010","Q12011","Q22011","Q32011", "Q42011","Q12012","Q22012","Q32012","Q42012", "Q12013","Q22013","Q32013","Q32013")
dates <- 1:length(units_sold)

Bass.nls <- nls(units_sold ~ M * (((P + Q)^2/P) * exp(-(P + Q) * dates))/(1 + (Q/P) * exp(-(P + Q) * dates))^2, start = list(M = 500, P = 0.03, Q = 0.38))


m <- coef(Bass.nls)[1] 
p <- coef(Bass.nls)[2]
q <- coef(Bass.nls)[3]

summary(Bass.nls)


modeltimes <- seq(1,14)
myForecast <- m * (((p +q)^2/p) * exp(-(p + q) * modeltimes))/(1 + (q/p) * exp(-(p + q) * modeltimes))^2


plot(myForecast, type="l", xlab="quarters",ylab="sales in million units", main="iPad sales model vs actual")
points(dates,units_sold)

#looking into the future

modeltimes2 <- seq(1,35)
myForecast2 <- m * (((p +q)^2/p) * exp(-(p + q) * modeltimes2))/(1 + (q/p) * exp(-(p + q) * modeltimes2))^2


plot(myForecast2, type="l", xlab="quarters",ylab="sales in million units", main="iPad sales model vs actual")
points(dates,units_sold)


plot(ecdf(myForecast2))
