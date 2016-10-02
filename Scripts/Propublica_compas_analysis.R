library(dplyr)
library(ggplot2)
library(coefplot)
library(fmsb)

options(scipen = 9999)

raw_data <- read.csv("Data/compas-scores-two-years.csv")
nrow(raw_data)

df <- dplyr::select(raw_data, age, c_charge_degree, race, age_cat, score_text, 
                    sex, priors_count, days_b_screening_arrest, decile_score, 
                    is_recid, two_year_recid, c_jail_in, c_jail_out) %>% 
  filter(days_b_screening_arrest <= 30) %>%
  filter(days_b_screening_arrest >= -30) %>%
  filter(is_recid != -1) %>%
  filter(c_charge_degree != "O") %>%
  filter(score_text != 'N/A')
nrow(df)

df$length_of_stay <- as.numeric(as.Date(df$c_jail_out) - as.Date(df$c_jail_in))
cor(df$length_of_stay, df$decile_score)

summary(df$age_cat)
summary(df$race)

df %>%
  count(race) %>%
  mutate(percent = (n*100)/nrow(df))

summary(df$score_text)

xtabs(~ sex + race, data=df)
summary(df$sex)

nrow(filter(df, two_year_recid == 1))
nrow(filter(df, two_year_recid == 1)) / nrow(df) * 100

library(grid)
library(gridExtra)
pblack <- ggplot(data = filter(df, race == "African-American"),
                 aes(ordered(decile_score))) +
  geom_bar() +
  xlab("Decile Score") +
  ylim(0, 650) +
  ggtitle("Black defendant's Decile Scores")

pwhite <- ggplot(data = filter(df, race == "Caucasian"),
                 aes(ordered(decile_score))) +
  geom_bar() +
  xlab("Decile Score") +
  ylim(0, 650) +
  ggtitle("White defendant's Decile Scores")
grid.arrange(pblack, pwhite, ncol = 2)

xtabs(~ decile_score + race, data=df)


#glm
df <- mutate(df, crime_factor = factor(c_charge_degree)) %>%
  mutate(age_factor = as.factor(age_cat)) %>%
  within(age_factor <- relevel(age_factor, ref = 1)) %>%
  mutate(race_factor = factor(race)) %>%
  within(race_factor <- relevel(race_factor, ref = 3)) %>%
  mutate(gender_factor = factor(sex, labels= c("Female","Male"))) %>%
  within(gender_factor <- relevel(gender_factor, ref = 2)) %>%
  mutate(score_factor = factor(score_text != "Low", 
                               labels = c("LowScore","HighScore")))

model <- glm(score_factor ~ gender_factor + age_factor + race_factor +
               priors_count + crime_factor + two_year_recid, 
             family="binomial", data=df)
summary(model)
coefplot(model, intercept = FALSE, sort = "magnitude") + theme_bw()
NagelkerkeR2(model)

control <- exp(-1.52554) / (1+ exp(-1.52554))
exp(0.47721) / (1-control + control * exp(0.47721))
#Black defendants are 45% more likely than white defendants to receive a 
#higher score correcting for the seriousness of their crime, previous arrests, 
#and future criminal behavior.

exp(0.22127) / (1 - control + (control * exp(0.22127)))
exp(1.30839) / (1 - control + (control * exp(1.30839)))

#violent recidivism

raw_data <- read.csv("Data/compas-scores-two-years-violent.csv")
nrow(raw_data)

df <- dplyr::select(raw_data, age, c_charge_degree, race, age_cat, v_score_text, sex, priors_count, 
                    days_b_screening_arrest, v_decile_score, is_recid, two_year_recid) %>% 
  filter(days_b_screening_arrest <= 30) %>%
  filter(days_b_screening_arrest >= -30) %>% 
  filter(is_recid != -1) %>%
  filter(c_charge_degree != "O") %>%
  filter(v_score_text != 'N/A')
nrow(df)

summary(df$age_cat)
summary(df$race)
summary(df$v_score_text)
nrow(filter(df, two_year_recid == 1)) / nrow(df) * 100
nrow(filter(df, two_year_recid == 1))

library(grid)
library(gridExtra)
pblack <- ggplot(data=filter(df, race =="African-American"), aes(ordered(v_decile_score))) + 
  geom_bar() + xlab("Violent Decile Score") +
  ylim(0, 700) + ggtitle("Black Defendant's Violent Decile Scores")
pwhite <- ggplot(data=filter(df, race =="Caucasian"), aes(ordered(v_decile_score))) + 
  geom_bar() + xlab("Violent Decile Score") +
  ylim(0, 700) + ggtitle("White Defendant's Violet Decile Scores")
grid.arrange(pblack, pwhite,  ncol = 2)

df <- mutate(df, crime_factor = factor(c_charge_degree)) %>%
  mutate(age_factor = as.factor(age_cat)) %>%
  within(age_factor <- relevel(age_factor, ref = 1)) %>%
  mutate(race_factor = factor(race,
                              labels = c("African-American", 
                                         "Asian",
                                         "Caucasian", 
                                         "Hispanic", 
                                         "Native American",
                                         "Other"))) %>%
  within(race_factor <- relevel(race_factor, ref = 3)) %>%
  mutate(gender_factor = factor(sex, labels= c("Female","Male"))) %>%
  within(gender_factor <- relevel(gender_factor, ref = 2)) %>%
  mutate(score_factor = factor(v_score_text != "Low", 
                               labels = c("LowScore","HighScore")))
model <- glm(score_factor ~ gender_factor + age_factor + race_factor +
               priors_count + crime_factor + two_year_recid, 
             family="binomial", data=df)
summary(model)
coefplot(model, intercept = FALSE, sort = "magnitude") + theme_bw()
NagelkerkeR2(model)


control <- exp(-2.24274) / (1 + exp(-2.24274))
exp(0.65893) / (1 - control + (control * exp(0.65893)))

exp(3.14591) / (1 - control + (control * exp(3.14591)))


#predictive accuracy of COMPAS
library(survival)
library(ggfortify)

data <- read.csv("Data/cox-parsed.csv")

data <- data %>% filter(score_text != "N/A") %>%
  filter(end > start) %>%
  mutate(race_factor = factor(race,
                              labels = c("African-American", 
                                         "Asian",
                                         "Caucasian", 
                                         "Hispanic", 
                                         "Native American",
                                         "Other"))) %>%
  within(race_factor <- relevel(race_factor, ref = 3)) %>%
  mutate(score_factor = factor(score_text)) %>%
  within(score_factor <- relevel(score_factor, ref=2))

grp <- data[!duplicated(data$id),]
nrow(grp)

summary(grp$score_factor)
summary(grp$race_factor)

f <- Surv(start, end, event, type="counting") ~ score_factor
model <- coxph(f, data=data)
summary(model)

decile_f <- Surv(start, end, event, type="counting") ~ decile_score
dmodel <- coxph(decile_f, data=data)
summary(dmodel)

f2 <- Surv(start, end, event, type="counting") ~ race_factor + score_factor + 
  race_factor * score_factor
model <- coxph(f2, data=data)
print(summary(model))

exp(-0.18976 + 1.28350)
exp(1.28350)
exp(0.84286-0.17261)
exp(0.84286)

fit <- survfit(f, data=data)

plotty <- function(fit, title) {
  return(autoplot(fit, conf.int=T, censor=F) + 
           ggtitle(title) + 
           ylim(0,1) +
           theme_bw())
}

plotty(fit, "Overall")

white <- filter(data, race == "Caucasian")
white_fit <- survfit(f, data=white)
black <- filter(data, race == "African-American")
black_fit <- survfit(f, data=black)
grid.arrange(plotty(white_fit, "White defendants"), 
             plotty(black_fit, "Black defendants"), ncol=2)

summary(fit, times=c(730))
summary(black_fit, times=c(730))
summary(white_fit, times=c(730))

summary(coxph(f, data=white))
summary(coxph(f, data=black))

#predictive accuracy of COMPAS violent recidivism
violent_data <- read.csv("Data/cox-violent-parsed.csv")
violent_data <- violent_data %>% 
  filter(end > start) %>%
  filter(score_text != "N/A")  %>%
  mutate(race_factor = factor(race,
                              labels = c("African-American", 
                                         "Asian",
                                         "Caucasian", 
                                         "Hispanic", 
                                         "Native American",
                                         "Other"))) %>%
  within(race_factor <- relevel(race_factor, ref = 3)) %>%
  mutate(score_factor = factor(score_text)) %>%
  within(score_factor <- relevel(score_factor, ref=2))


vf <- Surv(start, end, event, type="counting") ~ score_factor
vmodel <- coxph(vf, data=violent_data)
vgrp <- violent_data[!duplicated(violent_data$id),]
print(nrow(vgrp))
summary(vmodel)

vf2 <- Surv(start, end, event, type="counting") ~ race_factor + 
  race_factor * score_factor
vmodel <- coxph(vf2, data=violent_data)
summary(vmodel)
summary(coxph(vf, data=filter(violent_data, race == "African-American")))
summary(coxph(vf, data=filter(violent_data, race == "Caucasian")))

white <- filter(violent_data, race == "Caucasian")
white_fit <- survfit(vf, data=white)

black <- filter(violent_data, race == "African-American")
black_fit <- survfit(vf, data=black)

grid.arrange(plotty(white_fit, "White defendants"), 
             plotty(black_fit, "Black defendants"), ncol=2)
