

prob <- seq(.01, .99, length = 100)

odds <- prob/(1-prob)
logodds <- log(prob/(1-prob))

d <- data.frame(prob = prob, odds = odds, logodds = logodds)

ggplot(d, aes(prob, logodds)) + geom_line()
ggplot(d, aes(prob, odds)) + geom_line()

d <- mtcars

d$mpgfac <- ifelse(d$mpg < 20, 0, 1)

table(d$mpgfac)
p = 14/32

log(p/(1-p))

glm0 <- glm(mpgfac~1, d, family = binomial(link = "logit"))


#####

N  <- 100               # generate some data
X1 <- rnorm(N, 175, 7)
X2 <- rnorm(N,  30, 8)
X3 <- abs(rnorm(N, 60, 30))
Y  <- 0.5*X1 - 0.3*X2 - 0.4*X3 + 10 + rnorm(N, 0, 12)
Yfac   <- cut(Y, breaks=c(-Inf, median(Y), Inf), labels=c("lo", "hi"))
Yfacint <- ifelse(Yfac == "hi", 1, 0)

lmfit <- lm(Y ~ X1 + X2 + X3)
lmfit <- lm(Yfacint ~ X1 + X2 + X3)


glmFit <- glm(Y ~ X1 + X2 + X3)
glmFit <- glm(Yfac ~ X1 + X2 + X3, family=binomial(link="logit"))

exp(coefficients(glmFit))


Yhat <- fitted(glmFit)
thresh  <- 0.5  # threshold for dichotomizing according to predicted probability
YhatFac <- cut(Yhat, breaks=c(-Inf, thresh, Inf), labels=c("lo", "hi"))
cTab    <- table(Yfac, YhatFac)

addmargins(cTab)
sum(diag(cTab)) / sum(cTab)


library(vcd)

OR <- oddsratio(cTab, log = FALSE)
(cTab[1,1]/cTab[1,2])/(cTab[2,1]/cTab[2,2])

glm0 <- glm(Yfac ~ 1, family=binomial(link="logit"))
anova(glm0, glmFit, test="Chisq")


#marginal effects
mex = mean(dlogis(predict(glmFit, type = "link")))
mex * coef(glmFit)


#http://www.ats.ucla.edu/stat/r/dae/logit.htm

library(aod)
library(ggplot2)
library(Rcpp)
mydata <- read.csv("http://www.ats.ucla.edu/stat/data/binary.csv")
head(mydata)
sapply(mydata, sd)

xtabs(~ admit + rank, data = mydata)

#logistic regression
mydata$rank <- factor(mydata$rank)

mylogit <- glm(admit~., data = mydata, family = "binomial")
summary(mylogit)

##             Estimate Std. Error z value Pr(>|z|)    
## (Intercept) -3.98998    1.13995   -3.50  0.00047 ***
## gre          0.00226    0.00109    2.07  0.03847 *  
## gpa          0.80404    0.33182    2.42  0.01539 *  
## rank2       -0.67544    0.31649   -2.13  0.03283 *  
## rank3       -1.34020    0.34531   -3.88  0.00010 ***
## rank4       -1.55146    0.41783   -3.71  0.00020 ***

# * For every one unit change in gre, the log odds of admission 
# (versus non-admission) increases by 0.002.
# * For a one unit increase in gpa, the log odds of being admitted to 
# graduate school increases by 0.804.
# * The indicator variables for rank have a slightly different interpretation. 
# For example, having attended an undergraduate institution with rank of 2, 
# versus an institution with a rank of 1, changes the log odds of 
# admission by -0.675.

confint(mylogit)
wald.test(b = coef(mylogit), Sigma = vcov(mylogit), Terms = 4:6)

# check if rank 2 and rank 3 are same
l <- cbind(0,0,0,1,-1,0)
wald.test(b = coef(mylogit), Sigma = vcov(mylogit), L = l)

## odds ratios only
exp(coef(mylogit))
## odds ratios and 95% CI
exp(cbind(OR = coef(mylogit), confint(mylogit)))

# OR       2.5 %    97.5 %
#   (Intercept) 0.0185001 0.001889165 0.1665354
# gre         1.0022670 1.000137602 1.0044457
# gpa         2.2345448 1.173858216 4.3238349
# rank2       0.5089310 0.272289674 0.9448343
# rank3       0.2617923 0.131641717 0.5115181
# rank4       0.2119375 0.090715546 0.4706961

#one unit increase in gpa, the odds of being admitted to grad school 
#increase by a factor of 2.23

newdata1 <- with(mydata,
                 data.frame(gre = mean(gre), gpa = mean(gpa), rank = factor(1:4)))

newdata1$rankP <- predict(mylogit, newdata = newdata1, type = "response")
newdata1

newdata2 <- with(mydata,
                 data.frame(gre = rep(seq(from = 200, to = 800, length.out = 100), 4),
                            gpa = mean(gpa), rank = factor(rep(1:4, each = 100))))

newdata3 <- cbind(newdata2, predict(mylogit, newdata = newdata2, type="link", se=TRUE))
newdata3 <- within(newdata3, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})

ggplot(newdata3, aes(x = gre, y = PredictedProb)) +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = rank), alpha = .2) +
  geom_line(aes(colour = rank), size=1)

# testing the overall significance of the model
with(mylogit, null.deviance - deviance)
with(mylogit, df.null - df.residual)

# null.deviance - deviance is the chisq test statistic comparing null model and 
#model with all features
with(mylogit, pchisq(null.deviance - deviance, df.null - df.residual, 
                     lower.tail = FALSE))
with(mylogit, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
#0.00000007578
#same test as 
anova(glm(admit~1, mydata, family = "binomial"), mylogit, test = "Chisq")
#0.00000007578


#check model's log likelihood
logLik(mylogit)

# Nature of log odds ratio line
prob <- seq(.01, .99, length = 100)

odds <- prob/(1-prob)
logodds <- log(prob/(1-prob))

d <- data.frame(prob = prob, odds = odds, logodds = logodds)

ggplot(d, aes(prob, logodds)) + geom_line()
ggplot(d, aes(prob, odds)) + geom_line()


#http://www.ats.ucla.edu/stat/mult_pkg/faq/general/odds_ratio.htm
#interpreting coefficients
d <- read.csv(file = "Data/glmsample.csv", header = TRUE)
dim(d)
head(d)
summary(d)

#outcome variable is hon

#LR with no variables
table(d$hon)
# 0   1 
# 151  49
p = 49/nrow(d)
logodds = log(p/(1-p))
glm0 = glm(hon~1, d, family = binomial(link = "logit"))

all.equal(tidy(glm0)$estimate, logodds)


#LR with 1 factor variable
xtabs(~ hon + female, d)
# female
# hon  0  1
# 0 74 77
# 1 17 32

p_female_hons = 32/77
p_male_hons = 17/74

logodds_female_male = log(p_female_hons/p_male_hons)

glmfac = glm(hon ~ female, d, family = binomial(link = "logit"))

all.equal(logodds_female_male, tidy(glmfac)$estimate[2])

#LR with 1 continuos variable
glmcont = glm(hon ~ math, d, family = binomial(link = "logit"))

#intercept gives the log odds of a student with math score zero
#one unit increase in math score increases the log offs by .1563404
#odds(math=55)/odds(math=54) = exp(.1563404) = 1.1692241.
# Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -9.79394    1.48174  -6.610 3.85e-11 ***
#   math         0.15634    0.02561   6.105 1.03e-09 ***

#LR with multiple predictor variables and no interaction terms
glmcont = glm(hon ~ math + female + read, d, 
              family = binomial(link = "logit"))
#This fitted model says that, holding math and reading at a fixed value, the 
#odds of getting into an honors class for females (female = 1)over the odds of 
#getting into an honors class for males (female = 0) is exp(.979948) = 2.66.  
#In terms of percent change, we can say that the odds for females are 166% 
#higher than the odds for males.  The coefficient for math says that, holding 
#female and reading at a fixed value, we will see 13% increase in the odds of 
#getting into an honors class for a one-unit increase in math score since 
#exp(.1229589) = 1.13.

#LR with an interaction term
glmcont = glm(hon ~ math + female + female*math, d, 
              family = binomial(link = "logit"))

#In the presence of interaction term of female by math, we can no longer 
#talk about the effect of female, holding all other variables at certain value, 
#since it does not make sense to fix math and femalexmath at certain value and 
#still allow female change from 0 to 1!
# we contruct two equations, one for males and one for females
#So we can say that the coefficient for math is the effect of math when 
#female = 0.  More explicitly, we can say that for male students, 
#a one-unit increase in math score yields a change in log odds of 0.13.  
#On the other hand, for the female students, a one-unit increase in math score 
#yields a change in log odds of (.13 + .067) = 0.197.
