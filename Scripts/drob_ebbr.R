library(Lahman)
library(dplyr)
library(tidyr)
library(ebbr)
library(scales)

theme_set(theme_bw())

# Grab career batting average of non-pitchers
# (allow players that have pitched <= 3 games, like Ty Cobb)
pitchers <- Pitching %>%
  group_by(playerID) %>%
  summarize(gamesPitched = sum(G)) %>%
  filter(gamesPitched > 3)

# Add player names
player_names <- Master %>%
  tbl_df() %>%
  dplyr::select(playerID, nameFirst, nameLast, bats) %>%
  unite(name, nameFirst, nameLast, sep = " ")

# include the "bats" (handedness) and "year" column for later
career_full <- Batting %>%
  filter(AB > 0) %>%
  anti_join(pitchers, by = "playerID") %>%
  group_by(playerID) %>%
  summarize(H = sum(H), AB = sum(AB), year = mean(yearID)) %>%
  inner_join(player_names, by = "playerID") %>%
  filter(!is.na(bats))

# We don't need all this data for every step
career <- career_full %>%
  select(-bats, -year)

career %>%
  filter(AB >= 100) %>%
  ggplot(aes(H / AB)) +
  geom_histogram()

prior <- career %>%
  filter(AB >= 500) %>%
  ebb_fit_prior(H, AB)

augment(prior, data = career)

# one step process
eb_career <- career %>%
  add_ebb_estimate(H, AB, prior_subset = AB >= 500)

eb_career %>%
  ggplot(aes(.raw, .fitted, color = AB)) +
  geom_point() +
  geom_abline(color = "red") +
  scale_color_continuous(trans = "log", breaks = c(1, 10, 100, 1000)) +
  geom_hline(yintercept = tidy(prior)$mean, color = "red", lty = 2) +
  labs(x = "Raw batting average",
       y = "Shrunken batting average")

yankee_1998 <- c("brosisc01", "jeterde01", "knoblch01",
                 "martiti02", "posadjo01", "strawda01", "willibe02")

eb_career %>%
  filter(playerID %in% yankee_1998) %>%
  mutate(name = reorder(name, .fitted)) %>%
  ggplot(aes(.fitted, name)) +
  geom_point() +
  geom_errorbarh(aes(xmin = .low, xmax = .high)) +
  labs(x = "Estimated batting average (w/ 95% confidence interval)",
       y = "Player")

#hierarchical modeling

career %>%
  filter(AB >= 10) %>%
  ggplot(aes(AB, H / AB)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_x_log10()

eb_career_ab <- career %>%
  add_ebb_estimate(H, AB, method = "gamlss", mu_predictors = ~ log10(AB))

eb_career_ab %>%
  filter(AB > 10) %>%
  rename(Raw = .raw, Shrunken = .fitted) %>%
  gather(type, estimate, Raw, Shrunken) %>%
  ggplot(aes(AB, estimate)) +
  geom_point() +
  facet_wrap(~ type) +
  scale_x_log10()

library(splines)


eb_career_prior <- career_full %>%
  ebb_fit_prior(H, AB, method = "gamlss",
                mu_predictors = ~ 0 + ns(year, df = 5) * bats + log(AB))

fake_data <- crossing(H = 300,
                      AB = 1000,
                      year = seq(1885, 2013),
                      bats = c("L", "R"))

augment(eb_career_prior, newdata = fake_data) %>%
  mutate(prior = .alpha0 / (.alpha0 + .beta0),
         prior.low = qbeta(.025, .alpha0, .beta0),
         prior.high = qbeta(.975, .alpha0, .beta0)) %>%
  ggplot(aes(year, prior, color = bats)) +
  geom_line() +
  geom_ribbon(aes(ymin = prior.low, ymax = prior.high), alpha = .1, lty = 2) +
  ylab("Prior distribution (mean + 95% quantiles)")


#A/B testing

test_300 <- career %>%
  add_ebb_estimate(H, AB, method = "gamlss", mu_predictors = ~ log10(AB)) %>%
  add_ebb_prop_test(0.3, sort = TRUE)

#player - player A/B testing

piazza <- eb_career_ab %>%
  filter(name == "Mike Piazza")

piazza_params <- c(piazza$.alpha1, piazza$.beta1)
piazza_params

compare_piazza <- eb_career_ab %>%
  add_ebb_prop_test(piazza_params, approx = TRUE, sort = TRUE)

compare_piazza %>%
  select(name, H, AB, .fitted, .low, .high, .pep, .qvalue)


#Mixture models
career_w_pitchers <- Batting %>%
  filter(AB >= 25, lgID == "NL", yearID >= 1980) %>%
  group_by(playerID) %>%
  summarize(H = sum(H), AB = sum(AB), year = mean(yearID)) %>%
  mutate(isPitcher = playerID %in% pitchers$playerID) %>%
  inner_join(player_names, by = "playerID")

ggplot(career_w_pitchers, aes(H / AB)) +
  geom_histogram()


set.seed(2017)
mm <- ebb_fit_mixture(career_w_pitchers, H, AB, clusters = 2)
tidy(mm)

ggplot(mm$assignments, aes(H / AB, fill = .cluster)) +
  geom_histogram(alpha = 0.8, position = "identity")

#mixture models across iterations
fits <- mm$iterations$fits

fits

fits %>%
  gather(parameter, value, alpha, beta, mean) %>%
  ggplot(aes(iteration, value, color = parameter, lty = cluster)) +
  geom_line() +
  facet_wrap(~ parameter, scales = "free_y")

assignments <- mm$iterations$assignments

assignments

assignments %>%
  ggplot(aes(H / AB, fill = .cluster)) +
  geom_histogram(alpha = 0.8, position = "identity") +
  facet_wrap(~ iteration)
