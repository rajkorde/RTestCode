library(dplyr)
library(tidyr)
library(Lahman)

options(tibble.width = Inf)

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
  mutate(eb_estimate = (H + alpha0) / (AB + alpha0 + beta0)) %>%
  mutate(alpha1 = H + alpha0,
         beta1 = AB - H + beta0)

career_eb %>%
  filter(name == "Hank Aaron") %>%
  do(data_frame(x = seq(.27, .33, .0002),
                density = dbeta(x, .$alpha1, .$beta1))) %>%
  ggplot(aes(x, density)) +
  geom_line() +
  geom_ribbon(aes(ymin = 0, ymax = density * (x < .3)),
              alpha = .1, fill = "red") +
  geom_vline(color = "red", lty = 2, xintercept = .3)

career_eb %>% filter(name == "Hank Aaron")
#posterior error probability
pbeta(.3, 3850, 8818)

career_eb <- career_eb %>%
  mutate(PEP = pbeta(.3, alpha1, beta1))

career_eb %>%
  ggplot(aes(eb_estimate, PEP, color = AB)) +
  geom_point(size = 1) +
  xlab("(Shrunken) batting average estimate") +
  ylab("Posterior Error Probability (PEP)") +
  geom_vline(color = "red", lty = 2, xintercept = .3) +
  scale_colour_gradient(trans = "log", breaks = 10 ^ (1:5))

top_players <- career_eb %>%
  arrange(PEP) %>%
  head(100)

#total number of false positives
sum(top_players$PEP)
  
career_eb <- career_eb %>%
  arrange(PEP) %>%
  mutate(qvalue = cummean(PEP))

hall_of_fame <- career_eb %>%
  filter(qvalue < .05)

career_eb %>%
  filter(qvalue < 0.25) %>%
  ggplot(aes(qvalue, rank(PEP))) +
  geom_line() +
  xlab("q-value cutoff") +
  ylab("Numver of players included")
