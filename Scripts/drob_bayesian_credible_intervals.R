library(dplyr)
library(tidyr)
library(Lahman)

career <- Batting %>%
  filter(AB > 0) %>%
  anti_join(Pitching, by = "playerID") %>%
  group_by(playerID) %>%
  summarize(H = sum(H), AB = sum(AB)) %>%
  mutate(average = H / AB)

career <- Master %>%
  tbl_df() %>%
  select(playerID, nameFirst, nameLast) %>%
  unite(name, nameFirst, nameLast, sep = " ") %>%
  inner_join(career, by = "playerID")

career_filtered <- career %>% filter(AB >= 500)

m <- MASS::fitdistr(career_filtered$average, dbeta,
                    start = list(shape1 = 1, shape2 = 10))

alpha0 <- m$estimate[1]
beta0 <- m$estimate[2]

career_eb <- career %>%
  mutate(eb_estimate = (H + alpha0) / (AB + alpha0 + beta0))

career_eb <- career_eb %>%
  mutate(alpha1 = H + alpha0,
         beta1 = AB - H + beta0)

yankee_1998 <- c("brosisc01", "jeterde01", "knoblch01", "martiti02", "posadjo01", "strawda01", "willibe02")

yankee_1998_career <- career_eb %>%
  filter(playerID %in% yankee_1998)

library(broom)
library(ggplot2)

yankee_beta <- yankee_1998_career %>%
  inflate(x = seq(.18, .33, .0002)) %>%
  ungroup() %>%
  mutate(density = dbeta(x, alpha1, beta1))

ggplot(yankee_beta, aes(x, density, color = name)) +
  geom_line() +
  stat_function(fun = function(x) dbeta(x, alpha0, beta0),
                lty = 2, color = "black")



ggplot(yankee_beta, aes(x, density, color = name)) +
  geom_line() +
  stat_function(fun = function(x) dnorm(x),
                lty = 2, color = "black")


yankee_1998_career <- yankee_1998_career %>%
  mutate(low = qbeta(.025, alpha1, beta1),
         high = qbeta(.975, alpha1, beta1))

yankee_1998_career %>%
  mutate(name = reorder(name, average)) %>%
  ggplot(aes(average, name)) +
  geom_point() +
  geom_errorbarh(aes(xmin = low, xmax = high)) +
  geom_vline(xintercept = alpha0 / (alpha0 + beta0), color = "red", lty = 2) +
  xlab("Estimated batting average (w/ 95% interval)") +
  ylab("Player")

#confidence interval vs credible intervals
career_eb <- career_eb %>%
  mutate(low = qbeta(.025, alpha1, beta1),
         high = qbeta(.975, alpha1, beta1))

library(broom)
set.seed(2015)

some <- career_eb %>%
  sample_n(20) %>%
  mutate(name = paste0(name, " (", H, "/", AB, ")"))

frequentist <- some %>%
  group_by(playerID, name, AB) %>%
  do(tidy(binom.test(.$H, .$AB))) %>%
  select(playerID, name, estimate, low = conf.low, high = conf.high) %>%
  mutate(method = "Confidence")

bayesian <- some %>%
  select(playerID, name, AB, estimate = eb_estimate,
         low = low, high = high) %>%
  mutate(method = "Credible")

combined <- bind_rows(frequentist, bayesian)

combined %>%
  ungroup() %>%
  mutate(name = reorder(name, -AB)) %>%
  ggplot(aes(estimate, name, color = method, group = method)) +
  geom_point() +
  geom_errorbarh(aes(xmin = low, xmax = high)) +
  geom_vline(xintercept = alpha0 / (alpha0 + beta0), color = "red", lty = 2) +
  xlab("Estimated batting average") +
  ylab("Player") +
  labs(color = "")
