library(tidyverse)
library(lavaan)

# one factor models

data("HolzingerSwineford1939")

text.model <- 'textspeed =~ x4 + x5 + x6 + x7 + x8 + x9'

text.fit <- cfa(model = text.model, data = HolzingerSwineford1939)

summary(text.fit)

visual.model <- 'visual =~ x1 + a*x2 + a*x3'
visual.fit <- cfa(model = visual.model,
                  data = HolzingerSwineford1939)    
summary(visual.fit, standardized = TRUE, fit.measures = TRUE)

# two factor models
twofactor.model <- 'visual =~ x1 + x2 + x3 
    speed =~ x7 + x8 + x9'

twofactor.fit <- cfa(model = twofactor.model,
                     data = HolzingerSwineford1939)
summary(twofactor.fit, standardized = TRUE, fit.measures = TRUE)

modificationindices(twofactor.fit, sort = TRUE)

twofactor.model <- 'visual =~ x1 + x2 + x3 
speed =~ x7 + x8 + x9
x7 ~~ x8'

twofactor.fit <- cfa(model = twofactor.model,
                     data = HolzingerSwineford1939)
summary(twofactor.fit, standardized = TRUE, fit.measures = TRUE)


# forcing the correlation between the latent variables to be zero
summary(twofactor.fit, standardized = TRUE, fit.measures = TRUE)
twofactor.model <- 'visual =~ x1 + x2 + x3
    speed =~ x7 + x8 + x9
    speed ~~ 0*visual'

twofactor.fit <- cfa(model = twofactor.model,
                     data = HolzingerSwineford1939)
summary(twofactor.fit, standardized = TRUE, fit.measures = TRUE)

# forcing the regression between two latent variables
summary(twofactor.fit, standardized = TRUE, fit.measures = TRUE)
twofactor.model <- 'visual =~ x1 + x2 + x3
    speed =~ x7 + x8 + x9
    speed ~ visual'

twofactor.fit <- cfa(model = twofactor.model,
                     data = HolzingerSwineford1939)
summary(twofactor.fit, standardized = TRUE, fit.measures = TRUE)

# comparing models

twofactor.model <- 'visual =~ x1 + x2 + x3 
                        speed =~ x7 + x8 + x9'
twofactor.model1 <- 'visual =~ x1 + x2 + x3 
                        speed =~ x7 + x8 + x9
                        x7 ~~ x8'
twofactor.fit <- cfa(model = twofactor.model,
                     data = HolzingerSwineford1939)
twofactor.fit1 <- cfa(model = twofactor.model1,
                      data = HolzingerSwineford1939)
anova(twofactor.fit, twofactor.fit1) # only works for nested models.

# for non-nested models, use fitmeasures
# typically used aic and ecvi
# aic lower is better and ecvi should be closer to zero
fitmeasures(twofactor.fit)

fitmeasures(twofactor.fit, c("aic", "ecvi"))
fitmeasures(twofactor.fit1, c("aic", "ecvi"))

### pysch data
library(psych)
data(epi)
data(epi.dictionary)

epi.dictionary

# Specify a three-factor model with one correlation set to zero
epi.model <- 'extraversion =~ V1 + V3 + V5 + V8
neuroticism =~ V2 + V4 + V7 + V9
lying =~ V6 + V12 + V18 + V24
extraversion ~~ 0*neuroticism'

# Run the model
epi.fit <- cfa(model = epi.model, data = epi)
# Examine the output 
summary(epi.fit, standardized = TRUE, fit.measures = TRUE)

# is lying predicted by neuroticism or viceversa

epi.model <- 'extraversion =~ V1 + V3 + V5 + V8
neuroticism =~ V2 + V4 + V7 + V9
lying =~ V6 + V12 + V18 + V24
lying ~ neuroticism'

# Run the model
epi.fit <- cfa(model = epi.model, data = epi)

# Examine the output 
summary(epi.fit, standardized = TRUE, fit.measures = TRUE)





data("PoliticalDemocracy")
pol.model <- 'poldemo =~ y1 + y2 + y3 + y4'
pol.fit <- cfa(model = pol.model, data = PoliticalDemocracy)
summary(pol.fit)

#std.all - values close to 1 indicate strong relation between latent variable and manifest variable
#std.lv should be greater than 0.3
summary(pol.fit, standardized = TRUE)
summary(pol.fit, standardized = TRUE, fit.measures = TRUE)



# multifactor models