library(mgcv)
library(demandr)
library(grid)
library(ggplot2)
library(splines)

set.seed(3)
x <- seq(0,2*pi,0.1)
z <- sin(x)
y <- z + rnorm(mean=0, sd=0.5*sd(z), n=length(x))
d <- cbind.data.frame(x,y,z)

d1 <- cbind.data.frame(data.frame(predict(smooth.spline(x=d, spar=0), x)), z)
e <- sqrt(sum((d1$z-d1$y)**2))
ggplot(data=d, aes(x=x, y=y)) + geom_point() + geom_line(data=d1, aes(x=x, y=y), linetype=1) + geom_line(aes(x=x, y=z), linetype=2) + ggtitle(paste0("Lambda=0, Dist = ", round(e,2)))


d2 <- cbind.data.frame(data.frame(predict(smooth.spline(x=d, spar=0.3), x)), z)
e <- sqrt(sum((d2$z-d2$y)**2))
ggplot(data=d, aes(x=x, y=y)) + geom_point() + geom_line(data=d1, aes(x=x, y=y), linetype=1) + geom_line(aes(x=x, y=z), linetype=2) + ggtitle(paste0("Lambda=0.3, Dist = ", round(e,2)))


d3 <- cbind.data.frame(data.frame(predict(smooth.spline(x=d, spar=0.6), x)), z)
e <- sqrt(sum((d3$z-d3$y)**2))
ggplot(data=d, aes(x=x, y=y)) + geom_point() + geom_line(data=d3, aes(x=x, y=y), linetype=1) + ylab("") + geom_line(aes(x=x, y=z), linetype=2) + ggtitle(paste0("Lambda=0.6, Dist = ", round(e,2)))

d4 <- cbind.data.frame(data.frame(predict(smooth.spline(x=d, spar=1), x)), z)
e <- sqrt(sum((d4$z-d4$y)**2))
ggplot(data=d, aes(x=x, y=y)) + geom_point() + geom_line(data=d4, aes(x=x, y=y), linetype=1) + ylab("") + geom_line(aes(x=x, y=z), linetype=2) + ggtitle(paste0("Lambda=1, Dist = ", round(e,2)))

d5 <- cbind.data.frame(data.frame(ksmooth(d$x, d$y, kernel="box", n.points=length(x), bandwidth=1.5)), z)
ggplot(data=d, aes(x=x, y=y)) + geom_point() + geom_line(data=d5, aes(x=x, y=y), linetype=1) + ylab("") + geom_line(aes(x=x, y=z), linetype=2) + ggtitle("Basic Runnuing Mean")

d6 <- cbind.data.frame(loess(y ~ x, data=d, span=0.6)$fitted, z, y, x)
names(d6) <- c("loess", "z", "y", "x")
ggplot(data=d, aes(x=x, y=y)) + geom_point() + geom_line(data=d6, aes(x=x, y=loess), linetype=1) + ylab("") + geom_line(aes(x=x, y=z), linetype=2) + ggtitle("Loess")

min(x)
max(x)
quantile(x, probs=c(0.25, .50, .75))

B <- bs(x, degree=3, intercept=TRUE, Boundary.knots=c(0, 6.2), knots=c(1.55, 3.10, 4.65))
model <- lm(y~0 + B)
model$coef
d7 <- cbind.data.frame(d, B, model$fitted)
names(d7) <- c("x", "y", "z", "B13", "B23", "B33", "B43", "B53", "B63", "B73", "Spline")
for (i in 1:7){
  d7[,3+i] <- d7[,3+i] * model$coef[i]
}

ggplot(data=d7, aes(x=x, y=y)) + geom_point() + geom_line(data=d7, aes(x=x, y=Spline), linetype=1) + ylab("") +  geom_line(aes(x=x, y=z), linetype=2)

##### model #####

library(randomForestSRC)
library(e1071)
library(mgcv)
library(ggplot2)
library(grid)
library(kknn)
library(Information)
library(ClustOfVar)
library(reshape2)
library(plyr)

options(scipen=10)
ProjectLocation <- "F:/Temp/temp"

source(paste0(ProjectLocation, "/miscfunctions.R"))
nvars <- 20
train <- readRDS("Data/gampost/train.rda")
valid <- readRDS("Data/gampost/valid.rda")

IV <- Information::create_infotables(data=train, NULL, "PURCHASE", 10)
