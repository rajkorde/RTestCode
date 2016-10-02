# From https://stats.stackexchange.com/questions/224045/using-lm-for-2-sample-proportion-test
# original code https://gist.github.com/mine-cetinkaya-rundel/055c3377c394e82e00c16edb7711d2f3library(dplyr)
library(broom)
set.seed(12345)

n_A <- 5000
n_B <- 5000

outcome <- rbinom(
  n = n_A + n_B,
  size = 1,
  prob = 0.5
)

treatment <- c(rep("A", n_A), rep("B", n_B))

df <- tbl_df(data.frame(outcome = outcome, treatment = treatment))

#by hand 2 sample prop test
p_A <- sum(df$outcome[df$treatment == "A"])/n_A
p_B <- sum(df$outcome[df$treatment == "B"])/n_B

p_pooled <- sum(df$outcome)/(n_A + n_B)
z_pooled <- (p_B - p_A)/sqrt(p_pooled * (1 - p_pooled) * ((1/n_A) + (1/n_B)))

pvalue_pooled <- 2*(1-pnorm(abs(z_pooled)))

z_unpooled <- (p_B - p_A)/sqrt((p_A * (1-p_A))/n_A + (p_B * (1-p_B))/n_B)
pvalue_unpooled <- 2*(1-pnorm(abs(z_unpooled)))


#using prop.test

res_prop_test <- tidy(prop.test(
  x = c(sum(df$outcome[df$treatment == "A"]), 
        sum(df$outcome[df$treatment == "B"])),
  n = c(n_A, n_B),
  correct = FALSE
))

all.equal(res_prop_test$p.value, pvalue_pooled)

res_glm_binomial <- df %>%
  do(tidy(glm(outcome ~ treatment, family = binomial(link = "identity")))) %>%
  filter(term == "treatmentB")

all.equal(res_glm_binomial$p.value, pvalue_unpooled)
  
#explore differences
## calculate lm standard error
res_lm_nobroom <- lm(outcome ~ treatment, data = df)

y <- res_lm_nobroom$model$outcome
y_hat_lm <- res_lm_nobroom$fitted.values 

x <- ifelse(res_lm_nobroom$model$treatment == "A", 0, 1)
x_bar <- mean(x)
n <- nrow(df)

se_lm <- sqrt(sum((y - y_hat_lm)^2) / (n - 2)) / sqrt(sum((x - x_bar)^2))

# glm and lm gaussian
res_glm <- df %>%
  do(tidy(glm(outcome ~ treatment))) %>%
  filter(term == "treatmentB")

all.equal(res_glm$p.value, pvalue_unpooled)
all.equal(res_glm$p.value, pvalue_pooled)

res_lm <- df %>%
  do(tidy(lm(outcome ~ treatment))) %>% 
  filter(term == "treatmentB")
res_lm
all.equal(res_lm$p.value, pvalue_unpooled)
all.equal(res_lm$p.value, pvalue_pooled)

all.equal(res_lm$p.value, res_glm$p.value)

# calculate lm standard error
res_lm_nobroom <- lm(outcome ~ treatment, data = df)

y <- res_lm_nobroom$model$outcome
y_hat_lm <- res_lm_nobroom$fitted.values
x <- ifelse(res_lm_nobroom$model$treatment == "A", 0, 1)
x_bar <- mean(x)
n <- nrow(df)

se_lm <- sqrt(sum((y - y_hat_lm)^2) / (n - 2)) / sqrt(sum((x - x_bar)^2))


# calculate glm_binomial standard error
res_glm_binomial_nobroom <- glm(outcome ~ treatment, 
                                data = df, family = binomial(link = "identity"))

y_hat_glm_binomial <- res_glm_binomial_nobroom$fitted.values
se_glm_binomial <- sqrt(sum((y - y_hat_glm_binomial)^2) / (n)) / 
  sqrt(sum((x - x_bar)^2))

## calculate se_unpooled
se_unpooled <- sqrt( (p_A * (1 - p_A))/n_A + (p_B * (1 - p_B))/n_B )

all.equal(se_lm, res_lm$std.error)
all.equal(se_lm, res_glm$std.error)

all.equal(se_unpooled, res_glm_binomial$std.error)
all.equal(se_glm_binomial, res_glm_binomial$std.error) 

all.equal(se_lm, se_glm_binomial)
all.equal(res_lm$std.error, res_glm_binomial$std.error) 
all.equal(res_glm$std.error, res_glm_binomial$std.error)
