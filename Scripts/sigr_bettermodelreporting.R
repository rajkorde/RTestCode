#http://www.win-vector.com/blog/2016/10/adding-polished-significance-summaries-to-papers-using-r/
#devtools::install_github('WinVector/sigr')

library(sigr)
d <- data.frame(x=c(1,2,3,4,5,6,7,7),
                y=c(TRUE,FALSE,FALSE,FALSE,TRUE,TRUE,TRUE,FALSE))
model <- glm(y~x,data=d,family=binomial)
summary(model)
formatChiSqTest(model,pLargeCutoff=1)
