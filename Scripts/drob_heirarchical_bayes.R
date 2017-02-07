library(gamlss)
library(dplyr)
library(tidyr)
library(Lahman)
library(ggplot2)
theme_set(theme_bw())

# Grab career batting average of non-pitchers
# (allow players that have pitched <= 3 games, like Ty Cobb)
pitchers <- Pitching %>%
  group_by(playerID) %>%
  summarize(gamesPitched = sum(G)) %>%
  filter(gamesPitched > 3)

# in this setup, we're keeping some extra information for later in the post:
# a "bats" column and a "year" column
career <- Batting %>%
  filter(AB > 0) %>%
  anti_join(pitchers, by = "playerID") %>%
  group_by(playerID) %>%
  summarize(H = sum(H), AB = sum(AB), year = mean(yearID)) %>%
  mutate(average = H / AB)

# Add player names
career <- Master %>%
  tbl_df() %>%
  dplyr::select(playerID, nameFirst, nameLast, bats) %>%
  unite(name, nameFirst, nameLast, sep = " ") %>%
  inner_join(career, by = "playerID")

library(gamlss)

fit <- gamlss(cbind(H, AB - H) ~ log(AB),
              data = dplyr::select(career, -bats),
              family = BB(mu.link = "identity"))

career_eb <- career %>%
  mutate(mu = fitted(fit, "mu"),
         sigma = fitted(fit, "sigma"),
         alpha0 = mu / sigma,
         beta0 = (1 - mu) / sigma,
         alpha1 = alpha0 + H,
         beta1 = beta0 + AB - H,
         estimate = alpha1 / (alpha1 + beta1))

career %>%
  count(bats)

career2 <- career %>%
  filter(!is.na(bats)) %>%
  mutate(bats = relevel(bats, "R"))

fit2 <- gamlss(cbind(H, AB - H) ~ log(AB) + bats,
               data = career2,
               family = BB(mu.link = "identity"))

library(broom)
tidy(fit2)

sigma <- fitted(fit2, "sigma")[1]
crossing(bats = c("L", "R"),
         AB = c(1, 10, 100, 1000, 10000)) %>%
  augment(fit2, newdata = .) %>%
  rename(mu = .fitted) %>%
  crossing(x = seq(.1, .36, .0005)) %>%
  mutate(alpha = mu / sigma,
         beta = (1 - mu) / sigma,
         density = dbeta(x, alpha, beta)) %>%
  ggplot(aes(x, density, color = factor(AB), lty = bats)) +
  geom_line() +
  labs(x = "Batting average",
       y = "Prior density",
       color = "AB",
       lty = "Batting hand")

crossing(bats = c("L", "R"),
         AB = c(10, 100, 1000, 10000)) %>%
  augment(fit2, newdata = .) %>%
  mutate(H = .3 * AB,
         alpha0 = .fitted / sigma,
         beta0 = (1 - .fitted) / sigma,
         alpha1 = alpha0 + H,
         beta1 = beta0 + AB - H,
         estimate = alpha1 / (alpha1 + beta1),
         conf.low = qbeta(.025, alpha1, beta1),
         conf.high = qbeta(.975, alpha1, beta1),
         record = paste(H, AB, sep = " / ")) %>%
  ggplot(aes(estimate, record, color = bats)) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) +
  labs(x = "Estimate w/ 95% credible interval",
       y = "Batting record",
       color = "Batting hand")

#over time
career2 %>%
  mutate(decade = factor(round(year - 5, -1))) %>%
  filter(AB >= 500) %>%
  ggplot(aes(decade, average)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylab("Batting average")


library(splines)
fit3 <- gamlss(cbind(H, AB - H) ~ 0 + ns(year, df = 5) + bats + log(AB),
               data = career2,
               family = BB(mu.link = "identity"))

plot_gamlss_fit <- function(f) {
  career2 %>%
    dplyr::select(year, bats) %>%
    distinct() %>%
    filter(bats != "B") %>%
    mutate(AB = 1000) %>%
    augment(f, newdata = .) %>%
    rename(mu = .fitted) %>%
    mutate(sigma = fitted(fit3, "sigma")[1],
           alpha0 = mu / sigma,
           beta0 = (1 - mu) / sigma,
           conf_low = qbeta(.025, alpha0, beta0),
           conf_high = qbeta(.975, alpha0, beta0)) %>%
    ggplot(aes(year, mu, color = bats, group = bats)) +
    geom_line() +
    geom_ribbon(aes(ymin = conf_low, ymax = conf_high), linetype = 2, alpha = .1) +
    labs(x = "Year",
         y = "Prior distribution (median + 95% quantiles)",
         color = "Batting hand")
}

plot_gamlss_fit(fit3)


#account for effect of handedness changing over time by adding interaction term
fit4 <- gamlss(cbind(H, AB - H) ~ 0 + ns(year, 5) * bats + log(AB),
               data = career2,
               family = BB(mu.link = "identity"))
plot_gamlss_fit(fit4)


Pitching %>%
  dplyr::select(playerID, yearID, GS) %>%
  distinct() %>%
  inner_join(dplyr::select(Master, playerID, throws)) %>%
  count(yearID, throws, wt = GS) %>%
  filter(!is.na(throws)) %>%
  mutate(percent = n / sum(n)) %>%
  filter(throws == "L") %>%
  ggplot(aes(yearID, percent)) +
  geom_line() +
  geom_smooth() +
  scale_y_continuous(labels = scales::percent_format()) +
  xlab("Year") +
  ylab("% of games with left-handed pitcher")

players <- crossing(year = c(1915, 1965, 2015),
                    bats = c("L", "R"),
                    H = 30,
                    AB = 100)

players_posterior <- players %>%
  mutate(mu = predict(fit4, what = "mu", newdata = players),
         sigma = predict(fit4, what = "sigma", newdata = players, type = "response"),
         alpha0 = mu / sigma,
         beta0 = (1 - mu) / sigma,
         alpha1 = alpha0 + H,
         beta1 = beta0 + AB - H)

players_posterior %>%
  crossing(x = seq(.15, .3, .001)) %>%
  mutate(density = dbeta(x, alpha1, beta1)) %>%
  ggplot(aes(x, density, color = bats)) +
  geom_line() +
  facet_wrap(~ year) +
  xlab("Batting average") +
  ylab("Posterior density") +
  ggtitle("Posterior distributions for batters with 30 / 100")

#whats next mixture models

fit_bb_mle <- function(x, n) {
  ll <- function(alpha, beta) {
    -sum(VGAM::dbetabinom.ab(x, n, alpha, beta, log = TRUE))
  }
  m <- stats4::mle(ll, start = list(alpha = 30, beta = 100), method = "L-BFGS-B",
                   lower = c(0.0001, .1))
  ab <- stats4::coef(m)
  data_frame(alpha = ab[1], beta = ab[2])
}

batting_w_pitchers <- Batting %>%
  filter(AB >= 50, lgID == "NL", yearID > 1985) %>%
  group_by(playerID) %>%
  summarize(H = sum(H), AB = sum(AB), year = mean(yearID)) %>%
  mutate(average = H / AB,
         isPitcher = ifelse(playerID %in% pitchers$playerID, "Pitcher", "Non-Pitcher"),
         isPitcher = relevel(factor(isPitcher), "Pitcher"))

fit <- fit_bb_mle(batting_w_pitchers$H, batting_w_pitchers$AB)

batting_w_pitchers %>%
  ggplot(aes(average, fill = isPitcher)) +
  geom_histogram(aes(y = ..density..)) +
  stat_function(fun = function(x) dbeta(x, fit$alpha, fit$beta), lty = 2) +
  labs(fill = "")