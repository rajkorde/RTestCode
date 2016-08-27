options(tibble.width = Inf)

library(plyr)
library(dplyr)
library(tidyr)
library(stringr)

load(url("http://varianceexplained.org/files/like_dislike.rda"))
library(ebbinom)


prior <- like_dislike %>%
  filter(total > 250) %>%
  with(estimate_beta_binom(dislikes, total))

eb <- like_dislike %>%
  mutate(alpha1 = dislikes + prior$alpha,
         beta1 = likes + prior$beta,
         eb_disliked = alpha1/(alpha1 + beta1),
         conf_low = qbeta(.025, alpha1, beta1),
         conf_high = qbeta(.975, alpha1, beta1)) %>%
  arrange(desc(eb_disliked)) %>%
  mutate(tag = reorder(tag, desc(eb_disliked)))

#most mentioned
eb %>%
  top_n(25, total) %>%
  ggplot(aes(eb_disliked, tag)) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf_low, xmax = conf_high)) +
  xlab("# Disliked/Total") +
  ylab("Tag")

#most disliked
eb %>%
  filter(total >= 200, conf_low > .5, !(tag %in% c("none", "nothing"))) %>%
  mutate(tag = reorder(tag, eb_disliked)) %>%
  ggplot(aes(eb_disliked, tag)) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf_low, xmax = conf_high)) +
  xlab("# Disliked / Total") +
  ylab("Tag")

#popular techs that are liked
eb %>%
  filter(total >= 2000, conf_high < .01, !(tag %in% c("none", "nothing"))) %>%
  mutate(tag = reorder(tag, -eb_disliked)) %>%
  ggplot(aes(eb_disliked, tag)) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf_low, xmax = conf_high)) +
  xlab("# Disliked / Total") +
  ylab("Tag")

prior <- average_dislike %>%
  filter(number > 500) %>%
  with(MASS::fitdistr(average, dgamma, list(shape = 10, rate = 20)))

alpha0 <- prior$estimate[1]
beta0 <- prior$estimate[2]


average_dislike <- average_dislike %>%
  mutate(alpha1 = average * number + alpha0,
         beta1 = number + beta0,
         shrunken_average = alpha1/beta1) %>%
  mutate(low = qgamma(.025, alpha1, beta1),
         high = qgamma(.975, alpha1, beta1)) %>%
  arrange(desc(shrunken_average)) %>%
  mutate(tag = factor(tag, levels = rev(tag)))

filt <- average_dislike %>%
  filter(number > 2000)

interval_plot <- function(dat) {
  ggplot(dat, aes(shrunken_average, tag)) +
    geom_point() +
    geom_errorbarh(aes(xmin = low, xmax = high)) +
    xlab("Average # of dislikes among people who liked this") +
    ylab("Tag")
}

filt %>%
  top_n(20, shrunken_average) %>%
  interval_plot()

filt %>%
  top_n(20, desc(shrunken_average)) %>%
  mutate(tag = reorder(tag, desc(shrunken_average))) %>%
  interval_plot()
