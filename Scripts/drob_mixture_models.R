library(ggplot2)
library(scales)
library(Lahman)
library(tidyr)
library(dplyr)

theme_set(theme_bw())

pitchers <- Pitching %>%
  group_by(playerID) %>%
  summarise(gamesPitched = sum(G)) %>%
  filter(gamesPitched > 3)

career <- Batting %>%
  filter(AB > 0, lgID == "NL", yearID >= 1980) %>%
  group_by(playerID) %>%
  summarise(H = sum(H), AB = sum(AB), year = mean(yearID)) %>%
  mutate(average = H/AB, isPitcher = playerID %in% pitchers$playerID)

career <- Master %>%
  tbl_df() %>%
  dplyr::select(playerID, nameFirst, nameLast, bats) %>%
  unite(name, nameFirst, nameLast, sep = " ") %>%
  inner_join(career, by = "playerID")

fit_bb_mle <- function(x, n) {
  ll <- function(alpha, beta) {
    -sum(VGAM::dbetabinom.ab(x, n, alpha, beta, log = TRUE))
  }
  m <- stats4::mle(ll, start = list(alpha = 30, beta = 100),
                   method = "L-BFGS-B", lower = c(0.0001, 0.1))
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
  geom_histogram(bins = 30) +
  stat_function(fun = function(x) 30 * dbeta(x, fit$alpha, fit$beta), lty = 2) +
  xlim(0, 0.4) +
  labs(fill = "", x = "Batting average (H / AB)")


#Expectation Maximization
#cluster initialization
set.seed(2017)
starting_data <- career %>%
  filter(AB >= 20) %>%
  select(-year, -bats, -isPitcher) %>%
  mutate(cluster = factor(sample(c("A", "B"), n(), replace = TRUE)))

starting_data %>%
  ggplot(aes(average, color = cluster)) +
  geom_density()

library(VGAM)
fit_bb_mle <- function(x, n) {
  # dbetabinom.ab is the likelihood function for a beta-binomial
  # using n, alpha and beta as parameters
  ll <- function(alpha, beta) {
    -sum(dbetabinom.ab(x, n, alpha, beta, log = TRUE))
  }
  m <- stats4::mle(ll, start = list(alpha = 3, beta = 10), method = "L-BFGS-B",
                   lower = c(0.001, .001))
  ab <- stats4::coef(m)
  data_frame(alpha = ab[1], beta = ab[2], number = length(x))
}

fits <- starting_data %>%
  group_by(cluster) %>%
  do(fit_bb_mle(.$H, .$AB)) %>%
  ungroup()

fits <- fits %>%
  mutate(prior = number/sum(number))

fits %>%
  crossing(x = seq(0, 0.4, 0.0001)) %>%
  mutate(density = dbeta(x, alpha, beta)) %>%
  ggplot() +
  geom_histogram(aes(average, y = ..density.., fill = cluster), data = starting_data, alpha = 0.2) +
  geom_line(aes(x, density, color = cluster)) +
  ggtitle("")

crosses <- starting_data %>%
  select(-cluster) %>%
  crossing(fits) %>%
  mutate(likelihood = prior * VGAM::dbetabinom.ab(H, AB, alpha, beta))

assignments <- starting_data %>%
  select(-cluster) %>%
  crossing(fits) %>%
  mutate(likelihood = prior * VGAM::dbetabinom.ab(H, AB, alpha, beta)) %>%
  group_by(playerID) %>%
  top_n(1, likelihood) %>%
  ungroup()

assignments

ggplot(assignments, aes(average, fill = cluster)) +
  geom_histogram()

assignments %>%
  ggplot(aes(AB, average, color = cluster)) +
  geom_point() +
  scale_x_log10()

#repeat
assignments %>%
  group_by(cluster) %>%
  do(fit_bb_mle(.$H, .$AB)) %>%
  ungroup() %>%
  mutate(prior = number/sum(number)) %>%
  crossing(x = seq(0, .4, .0001)) %>%
  mutate(density = .01 * nrow(assignments) * prior * dbeta(x, alpha, beta)) %>%
  ggplot() +
  geom_histogram(aes(average, fill = cluster), data = assignments, alpha = .25, binwidth = .01) +
  geom_line(aes(x, density, color = cluster))

#do it in an iteration
set.seed(1337)

iterate_em <- function(state, ...) {
  fits <- state$assignments %>%
    group_by(cluster) %>%
    do(mutate(fit_bb_mle(.$H, .$AB), number = nrow(.))) %>%
    ungroup() %>%
    mutate(prior = number / sum(number))
  
  assignments <- assignments %>%
    select(playerID:average) %>%
    crossing(fits) %>%
    mutate(likelihood = prior * VGAM::dbetabinom.ab(H, AB, alpha, beta)) %>%
    group_by(playerID) %>%
    top_n(1, likelihood) %>%
    ungroup()
  
  list(assignments = assignments, fits = fits)
}

library(purrr)
iterations <- accumulate(1:5, iterate_em, .init = list(assignments = starting_data))

assignment_iterations <- iterations %>%
  map_df("assignments", .id = "iteration")

assignment_iterations %>%
  ggplot(aes(average, fill = cluster)) +
  geom_histogram() +
  facet_wrap(~ iteration)

fit_iterations <- iterations %>%
  map_df("fits", .id = "iteration")

fit_iterations %>%
  crossing(x = seq(.001, .4, .001)) %>%
  mutate(density = prior * dbeta(x, alpha, beta)) %>%
  ggplot(aes(x, density, color = iteration, group = iteration)) +
  geom_line() +
  facet_wrap(~ cluster)

#assigning players to clusters
