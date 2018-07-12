#https://github.com/bgreenwell/pdp
library(pdp)
library(randomForest)
data("boston")
set.seed(101)
library(visreg)
library(tidyverse)

boston.rf <- randomForest(cmedv ~ ., data = boston)
pd <- partial(boston.rf, pred.var = c("lstat", "rm"), chull = TRUE)

plotPartial(pd)

library(ggplot2)
autoplot(pd, contour = TRUE, legend.title = "Partial\ndependence")

pd <- partial(boston.rf, pred.var = "lstat", chull = TRUE)
autoplot(pd)
plotPartial(pd)

pd <- partial(boston.rf, pred.var = "rm", chull = TRUE)
autoplot(pd)
plotPartial(pd)

p1 <- ggplot(boston, aes(rm, cmedv)) + geom_point(alpha = 0.1) + geom_smooth(se = FALSE)
p2 <- autoplot(pd)
grid.arrange(p1, p2, ncol = 2)

pd <- partial(boston.rf, pred.var = "tax", chull = TRUE)
autoplot(pd)
plotPartial(pd)

p1 <- ggplot(boston, aes(tax, cmedv)) + geom_point(alpha = 0.1) + geom_smooth(se = FALSE)
p2 <- autoplot(pd)
grid.arrange(p1, p2, ncol = 2)

library(mgcv)
boston.gam <- gam(cmedv ~ s(lon) + s(lat) + s(crim) + s(zn) + s(indus) +
                    chas + s(nox) + s(rm) + s(age) + s(dis) + rad + 
                    s(tax) + s(ptratio) + s(b) + s(lstat), data = boston)

visreg(boston.gam, "rm")


library(ICEbox)
bhd.ice = ice(object = boston.rf, 
              X = select(boston, -cmedv), 
              y = boston$cmedv, 
              predictor = "tax", frac_to_build = 0.1)

plot(bhd.ice)

library(kernlab)
data(pima)
pima.svm <- ksvm(diabetes ~ ., data = pima, type = "C-svc", kernel = "rbfdot",
                 C = 0.5, prob.model = TRUE)
pd.glucose <- partial(pima.svm, pred.var = "glucose", train = pima)
pd.glucose.prob <- partial(pima.svm, pred.var = "glucose", prob = TRUE, 
                           train = pima)
grid.arrange(autoplot(pd.glucose, main = "Logit scale"), 
             autoplot(pd.glucose.prob, main = "Probability scale"), 
             ncol = 2)

#https://journal.r-project.org/archive/2017/RJ-2017-016/RJ-2017-016.pdf

