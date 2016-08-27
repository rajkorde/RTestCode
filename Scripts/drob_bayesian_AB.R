library(tidyr)
library(Lahman)
library(MASS)
library(broom)
library(ggplot2)
library(dplyr)
theme_set(theme_bw())

pitchers <- Pitching %>%
  group_by(playerID) %>%
  summarize(gamesPitched = sum(G)) %>%
  filter(gamesPitched > 3)

career <- Batting %>%
  filter(AB > 0) %>%
  anti_join(pitchers, by = "playerID") %>%
  group_by(playerID) %>%
  summarize(H = sum(H), AB = sum(AB)) %>%
  mutate(average = H/AB)

career <- Master %>%
  tbl_df() %>%
  select(playerID, nameFirst, nameLast) %>%
  unite(name, nameFirst, nameLast, sep = " ") %>%
  inner_join(career, by = "playerID")

career_filtered <- career %>% filter(AB >= 500)
m <- fitdistr(career_filtered$average, dbeta, 
             start = list(shape1 = 1, shape2 = 10))

alpha0 <- m$estimate[1]
beta0 <- m$estimate[2]

career_eb <- career %>%
  mutate(eb_estimate = (H + alpha0)/(AB + alpha0 + beta0)) %>%
  mutate(alpha1 = H + alpha0, beta1 = AB - H + beta0) %>%
  arrange(desc(eb_estimate))

aaron <- career_eb %>% filter(name == "Hank Aaron")
piazza <- career_eb %>% filter(name == "Mike Piazza")
two_players <- bind_rows(aaron, piazza)

two_players %>%
  inflate(x = seq(.28, .33, .00025)) %>%
  mutate(density = dbeta(x, alpha1, beta1)) %>%
  ggplot(aes(x, density, color = name)) + 
  geom_line() +
  labs(x = "Batting average", color = "")

#three players
career_eb %>%
  filter(name %in% c("Hank Aaron", "Mike Piazza", "Hideki Matsui")) %>%
  inflate(x = seq(.28, .33, .00025)) %>%
  mutate(density = dbeta(x, alpha1, beta1)) %>%
  ggplot(aes(x, density, color = name)) + 
  geom_line() +
  labs(x = "Batting average", color = "")

#simulation of posterior draws
piazza_simulation <- rbeta(1e6, piazza$alpha1, piazza$beta1)
aaron_simulation <- rbeta(1e6, aaron$alpha1, aaron$beta1)

sim <- mean(piazza_simulation > aaron_simulation)
sim

#integration
x <- seq(.29, .318, .0002)

crossing(piazza_x = x, aaron_x = x) %>%
  mutate(piazza_density = dbeta(piazza_x, piazza$alpha1, piazza$beta1),
         aaron_density = dbeta(aaron_x, aaron$alpha1, aaron$beta1),
         joint = piazza_density * aaron_density) %>%
  ggplot(aes(piazza_x, aaron_x, fill = joint)) + 
  geom_tile() +
  geom_abline() +
  scale_fill_gradient2(low = "white", high = "red") +
  labs(x = "Piazza batting average",
       y = "Aaron batting average",
       fill = "Joint Density") +
  theme(legend.position = "none")

d <- .00002
limits <- seq(0.29, 0.33, d)
sum(outer(limits, limits, function(x, y) {
  (x > y) * dbeta(x, piazza$alpha1, piazza$beta1) *
    dbeta(y, aaron$alpha1, aaron$beta1) *
    d ^ 2
}))

#closed form solution

h <- function(alpha_a, beta_a, alpha_b, beta_b) {
  j <- seq.int(0, round(alpha_b) - 1)
  log_vals <- (lbeta(alpha_a + j, beta_a + beta_b) - log(beta_b + j) -
                 lbeta(1 + j, beta_b) - lbeta(alpha_a, beta_a))
  1 - sum(exp(log_vals))
}

h(piazza$alpha1, piazza$beta1,
  aaron$alpha1, aaron$beta1)

#closed form approximation for large alpha and betas
h_approx <- function(alpha_a, beta_a,
                     alpha_b, beta_b) {
  u1 <- alpha_a / (alpha_a + beta_a)
  u2 <- alpha_b / (alpha_b + beta_b)
  var1 <- alpha_a * beta_a / ((alpha_a + beta_a) ^ 2 * (alpha_a + beta_a + 1))
  var2 <- alpha_b * beta_b / ((alpha_b + beta_b) ^ 2 * (alpha_b + beta_b + 1))
  pnorm(0, u2 - u1, sqrt(var1 + var2))
}

h_approx(piazza$alpha1, piazza$beta1, aaron$alpha1, aaron$beta1)

#confidence and credible intervals

prop.test(two_players$H, two_players$AB)

credible_interval_approx <- function(a, b, c, d) {
  u1 <- a / (a + b)
  u2 <- c / (c + d)
  var1 <- a * b / ((a + b) ^ 2 * (a + b + 1))
  var2 <- c * d / ((c + d) ^ 2 * (c + d + 1))
  
  mu_diff <- u2 - u1
  sd_diff <- sqrt(var1 + var2)
  
  data_frame(posterior = pnorm(0, mu_diff, sd_diff),
             estimate = mu_diff,
             conf.low = qnorm(.025, mu_diff, sd_diff),
             conf.high = qnorm(.975, mu_diff, sd_diff))
}

credible_interval_approx(piazza$alpha1, piazza$beta1, aaron$alpha1, aaron$beta1)

set.seed(2016)
intervals <- career_eb %>%
  filter(AB > 10) %>%
  sample_n(20) %>%
  group_by(name, H, AB) %>%
  do(credible_interval_approx(piazza$alpha1, piazza$beta1, 
                              .$alpha1, .$beta1)) %>%
  ungroup() %>%
  mutate(name = reorder(paste0(name, " (", H, "/", AB, ")"), -estimate))
  
f <- function(H, AB) broom::tidy(prop.test(c(H, piazza$H),
                                             c(AB, piazza$AB)))

prop_tests <- purrr::map2_df(intervals$H, intervals$AB, f) %>%
  mutate(estimate = estimate1 - estimate2, name = intervals$name)

all_intervals <- bind_rows(
  mutate(intervals, type = "Credible"),
  mutate(prop_tests, type = "Confidence")
)

ggplot(all_intervals, aes(x = estimate, y = name, color = type)) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) +
  xlab("Piazza average - player average") +
  ylab("")

#FDR control
career_eb_vs_piazza <- bind_cols(
  career_eb,
  credible_interval_approx(piazza$alpha1, piazza$beta1,
                           career_eb$alpha1, career_eb$beta1)) %>%
  select(name, posterior, conf.low, conf.high)

career_eb_vs_piazza <- career_eb_vs_piazza %>%
  arrange(posterior) %>%
  mutate(qvalue = cummean(posterior))

better <- career_eb_vs_piazza %>%
  filter(qvalue < .05)

