
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
