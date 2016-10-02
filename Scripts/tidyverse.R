library(dplyr)
library(nycflights13)
library(gapminder)
library(ggplot2)
library(magrittr)

library(readr)
library(forcats)
library(tidyr)
library(modelr)
library(broom)
library(purrr)

###dplyr
dim(flights)
head(flights)


#filter
filter(flights, month==1, day==1)
filter(flights, day==1 | day==2)
slice(flights, 1:10)

#arrange
arrange(flights, year, -month, -day)
arrange(flights, desc(arr_delay))


#select
select(flights, year, month, day)
select(flights, year:day)
select(flights, -(year:day))

#rename
rename(flights, tail_num=tailnum)

#distinct
distinct(flights, tailnum)
distinct(flights, origin, dest)

#mutate
mutate(flights,
       gain = arr_delay - dep_delay,
       speed = distance / air_time * 60)
transmute(flights,
          gain = arr_delay - dep_delay,
          gain_per_hour = gain / (air_time / 60))

#summarise
summarise(flights, delay=mean(dep_delay, na.rm=TRUE))

#sample_n and sample_frac()
sample_n(flights, 5)
sample_frac(flights, .05)


#group_by
bytailnum = group_by(flights, tailnum)
delay = summarise(bytailnum, count=n(), dist=mean(distance, na.rm=TRUE),
                  delay=mean(arr_delay, na.rm=TRUE))

ggplot(delay, aes(dist, delay)) +
  geom_point(aes(size = count), alpha = 1/2) +
  geom_smooth() +
  scale_size_area()

daily <- group_by(flights, year, month, day)
per_day   <- summarise(daily, flights = n())

#chaining
flights %>%
  group_by(year, month, day) %>%
  select(arr_delay, dep_delay) %>%
  summarise(
    arr = mean(arr_delay, na.rm = TRUE),
    dep = mean(dep_delay, na.rm = TRUE)
  ) %>%
  filter(arr > 30 | dep > 30)

#glimpse
glimpse(flights)

#count

#do
flights %>%
  group_by(tailnum) %>%
  do(head(., 2))

#broom: glance, augment, tidy
fits <- gapminder %>% 
  group_by(country, continent) %>%
  do(fit = lm(lifeExp ~ I(year - 1952), .))
fits %>%
  glance(fit)
fits %>% 
  augment(fit)
fits %>% 
  tidy(fit)

flights %>%
  group_by(year, month, day) %>%
  count(year, month, day)

### readr

read_csv("x,y\n1,2\n3,4")

#useful params: col_skip, col_date(format), na, n_max
#useful functions: parse_date, read_lines()

mtcars2 <- read_csv(readr_example("mtcars.csv"))

#forcats

gss_cat %>% count(partyid)

gss_cat %>% mutate(partyid = fct_recode(partyid,
                            "Republican, strong"    = "Strong republican",
                            "Republican, weak"      = "Not str republican",
                            "Independent, near rep" = "Ind,near rep",
                            "Independent, near dem" = "Ind,near dem",
                            "Democrat, weak"        = "Not str democrat",
                            "Democrat, strong"      = "Strong democrat")) %>%
  count(partyid)

gss_cat %>% count(relig)

gss_cat %>% 
  mutate(relig = fct_lump(relig)) %>%
  count(relig)

# keep 4 commonest
gss_cat %>% 
  mutate(relig = fct_lump(relig, n = 5)) %>%
  count(relig)

#keep 4 rarest
gss_cat %>% 
  mutate(relig = fct_lump(relig, n = -5)) %>%
  count(relig)

gss_cat %>% 
  mutate(relig = fct_lump(relig, prop = 0.10)) %>% 
  count(relig)

#change level order
#relevel, inorder, infreq, rev, reorder, reorder2

relig <- gss_cat %>%
  group_by(relig) %>%
  summarise(age = mean(age, na.rm = TRUE),
            tvhours = mean(tvhours, na.rm = TRUE),
            n = n())

#bad
ggplot(relig, aes(tvhours, relig)) + geom_point()
#good
ggplot(relig, aes(tvhours, fct_reorder(relig, tvhours))) + geom_point()

by_age <- gss_cat %>%
  filter(!is.na(age)) %>%
  group_by(age, marital) %>%
  count() %>%
  mutate(prop = n / sum(n))

#bad
ggplot(by_age, aes(age, prop)) +
  geom_line(aes(colour = marital))

#good
ggplot(by_age, aes(age, prop)) +
  geom_line(aes(colour = fct_reorder2(marital, age, prop))) +
  labs(color = "marital")

####tidyr
#gather - gather multiple columns into key-value pairs, make wide data longer
messy <- data.frame(
  name = c("Wilbur", "Petunia", "Gregory"),
  a = c(67, 80, 64),
  b = c(56, 90, 50)
)

tidy <- messy %>%
  gather(drug, heartrate, a:b)

#spread - long data wider

tidy %>%
  spread(drug, heartrate)

#separate - tease apart two vars in one column.. see also extract
set.seed(10)
messy <- data.frame(
  id = 1:4,
  trt = sample(rep(c('control', 'treatment'), each = 2)),
  work.T1 = runif(4),
  home.T1 = runif(4),
  work.T2 = runif(4),
  home.T2 = runif(4)
)



messy %>%
  gather(key, time, -id, -trt) %>%
  separate(key, into = c("location", "time"), sep = '\\.')

df %>% 
  separate(y, c("y1", "y2", "y3"), sep = ",", fill = "right")
#separate()
df <- data_frame(x = 1:2, y = c("a,b", "d,e,f"))

df %>% 
  separate_rows(y, sep = ",")

#expand - generate all possible combinations of a grid, see also expand.grid()

sales <- dplyr::data_frame(
  year = rep(c(2012, 2013), c(4, 2)),
  quarter = c(1, 2, 3, 4, 2, 3), 
  sales = sample(6) * 100
)

sales %>% 
  expand(year, quarter) %>%
  left_join(sales)

resources <- frame_data(
  ~year, ~metric, ~value,
  1999, "coal", 100,
  2001, "coal", 50,
  2001, "steel", 200
)

resources %>% complete(year, metric)
resources %>% complete(year = full_seq(year, 1L), metric)

#unnest

raw <- dplyr::data_frame(
  x = 1:3,
  y = c("a", "d,e,f", "g,h")
)

raw %>% mutate(y = strsplit(y, ",")) %>% unnest(y)

df <- data_frame(
  x = 1:2,
  y1 = list(
    data_frame(y = 1),
    data_frame(y = 2)
  ),
  y2 = list(
    data_frame(y = "a"),
    data_frame(y = "b")
  )
)

df %>% unnest()

df <- data_frame(
  x = 1:2,
  y = list(
    a = 1:3,
    b = 3:1
  )
)

df %>% unnest()
df %>% unnest(.id = "id")

#fill(), replace_na(), complete()
df <- dplyr::data_frame(
  year = c(2015, NA, NA, NA), 
  trt = c("A", NA, "B", NA)
)
df %>% fill(year, trt)

df <- dplyr::data_frame(
  x = c(1, 2, NA), 
  y = c("a", NA, "b")
)

df %>% replace_na(list(x = 0, y = "unknown"))

df <- data_frame(
  group = c(1:2, 1),
  item_id = c(1:2, 2),
  item_name = c("a", "b", "b"),
  value1 = 1:3,
  value2 = 4:6
)

df %>% complete(group, nesting(item_id, item_name))
df %>% complete(group, nesting(item_id, item_name), fill = list(value1 = 0))


#nest - nest into data frames
ggplot(gapminder, aes(year, lifeExp)) +
  geom_line(aes(group = country))

by_country <- gapminder %>%
  group_by(continent, country) %>%
  nest()

by_country$data[[1]]
by_country <- by_country %>%
  mutate(model = purrr::map(data, ~lm(lifeExp ~ year, data = .)))

by_country <- by_country %>%
  unnest(model %>% purrr::map(broom::glance))
by_country %>% unnest(model %>% purrr::map(broom::tidy))

by_country %>% unnest(model %>% purrr::map(broom::augment))

#drop_na
df <- tibble(x = c(1, 2, NA), y = c("a", NA, "b"))
df %>% drop_na()
df %>% drop_na(x)

#modelr
#add_predictions
df <- tibble::data_frame(
  x = sort(runif(100)),
  y = 5 * x + 0.5 * x ^ 2 + 3 + rnorm(length(x))
)
plot(df)
m1 <- lm(y ~ x, data = df)
grid <- data.frame(x = seq(0, 1, length = 10))
grid %>% add_predictions(m1)

m2 <- lm(y ~ poly(x, 2), data = df)
grid %>% spread_predictions(m1, m2)
grid %>% gather_predictions(m1, m2)

#add predictors

df <- tibble::data_frame(
  x1 = sort(runif(100)),
  x2 = sort(runif(100)),
  y = 5 * x1 + 0.5 * x2 + 3 + rnorm(length(x1))
)
f <- y ~ x1
m2 <- lm(add_predictors(f, ~x2), data = df)
m2 <- lm(add_predictors(f, ~x2, fun = "*"), data = df)

#add residuals
df <- tibble::data_frame(
  x = sort(runif(100)),
  y = 5 * x + 0.5 * x ^ 2 + 3 + rnorm(length(x))
)
plot(df)
m1 <- lm(y ~ x, data = df)

df %>% add_residuals(m1)

m2 <- lm(y ~ poly(x, 2), data = df)
df %>% spread_residuals(m1, m2)
df %>% gather_residuals(m1, m2)

#bootstrap replicates, resample, resample_bootstrap, resample_partition
resample(mtcars, 1:10)
as.integer(b)
as.data.frame(b)
b <- resample_bootstrap(mtcars)

boot <- bootstrap(mtcars, 100)
models <- map(boot$strap, ~ lm(mpg ~ wt, data = .)) %>%
  map_df(broom::tidy, .id = "id")

hist(subset(models, term == "wt")$estimate)
hist(subset(models, term == "(Intercept)")$estimate)

#add residuals

df %>% add_residuals(m1)

#cross_kfold

cv1 <- crossv_kfold(mtcars, 5)
cv2 <- crossv_mc(mtcars, 100)
models <- map(cv2$train, ~ lm(mpg ~ wt, data = .))
errs <- map2_dbl(models, cv2$test, rmse)
hist(errs)


#data_grid
data_grid(mtcars, vs, am)

mod <- lm(mpg ~ wt + cyl + vs, data = mtcars)
data_grid(mtcars, .model = mod)
data_grid(mtcars, cyl = seq_range(cyl, 9), .model = mod)


#fit_with

disp_fits <- mtcars %>% 
  fit_with(lm, formulas(~disp,
                        additive = ~drat + cyl,
                        interaction = ~drat * cyl,
                        full = add_predictors(interaction, ~am, ~vs)))

mtcars %>% fit_with(glm, list(am ~ disp), family = binomial)


#formulas

f <- formulas(~disp,
         additive = ~drat + cyl,
         interaction = ~drat * cyl,
         full = add_predictors(interaction, ~am, ~vs))
disp_fits <- mtcars %>% 
  fit_with(lm, f)

#geom_ref_line
ggplot(mtcars, aes(cyl, mpg)) + geom_bar(stat = "identity") + 
  geom_ref_line(v = 5, size = 2, colour = "blue")

#model_quality

mod <- lm(mpg ~ wt, data = mtcars)
rmse(mod, mtcars)
rsquare(mod, mtcars)
mae(mod, mtcars)
qae(mod, mtcars)

#model_matrix - expand factors to dummy vars
model_matrix(mtcars, mpg ~ cyl)
model_matrix(iris, Sepal.Length ~ Species)

model_matrix(iris, Sepal.Length ~ Species - 1)


#na.warn
df <- tibble::tibble(
  x = 1:10,
  y = c(5.1, 9.7, NA, 17.4, 21.2, 26.6, 27.9, NA, 36.3, 40.4)
)

m1 <- lm(y ~ x, data = df)
resid(m1)
m2 <- lm(y ~ x, data = df, na.action = na.warn)
resid(m2)


#seq_range
x <- rcauchy(100)
seq_range(x, n = 10)
y <- rnorm(100)
seq_range(y, n = 10, pretty = TRUE)
seq_range(y, n = 10, expand = 0.5, pretty = TRUE)

#sim1-5 simulated data sets

#typical - most common values
typical(rpois(100, lambda = 10))
x <- sample(c("a", "b", "c"), 100, prob = c(0.6, 0.2, 0.2), replace = TRUE)
typical(x)
typical(factor(x))
x <- c("a", "a", "b", "b", "c")
typical(x)

######## broom
lmfit <- lm(mpg ~ wt, mtcars)
lmfit
summary(lmfit)
library(broom)
tidy(lmfit)
head(augment(lmfit))
glance(lmfit)


glmfit <- glm(am ~ wt, mtcars, family="binomial")
tidy(glmfit)
head(augment(glmfit))


nlsfit <- nls(mpg ~ k / wt + b, mtcars, start=list(k=1, b=0))
tidy(nlsfit)
head(augment(nlsfit, mtcars))

#t.test, cor.test, wilcox.test
tt <- t.test(wt ~ am, mtcars)
tidy(tt) #same as glance

set.seed(2014)
centers <- data.frame(cluster=factor(1:3), 
                      size=c(100, 150, 50), 
                      x1=c(5, 0, -3), x2=c(-1, 1, -2))
points <- centers %>% group_by(cluster) %>%
  do(data.frame(x1=rnorm(.$size[1], .$x1[1]),
                x2=rnorm(.$size[1], .$x2[1])))
ggplot(points, aes(x1, x2, color=cluster)) + geom_point()

points.matrix <- cbind(x1 = points$x1, x2 = points$x2)
kclust <- kmeans(points.matrix, 3)
kclust

head(augment(kclust, points.matrix))
tidy(kclust)

glance(kclust)

#exploratory clustering
kclusts <- data.frame(k=1:9) %>% 
  group_by(k) %>% 
  do(kclust=kmeans(points.matrix, .$k))

clusters <- kclusts %>% group_by(k) %>% do(tidy(.$kclust[[1]]))
assignments <- kclusts %>% group_by(k) %>% 
  do(augment(.$kclust[[1]], points.matrix))
clusterings <- kclusts %>% group_by(k) %>% do(glance(.$kclust[[1]]))

p1 <- ggplot(assignments, aes(x1, x2)) + 
  geom_point(aes(color=.cluster)) + 
  facet_wrap(~ k)

p1 + geom_point(data=clusters, size=10, shape="x")
ggplot(clusterings, aes(k, tot.withinss)) + geom_line()

#### broom and dplyr
library(broom)
library(dplyr)
data(Orange)

dim(Orange)
cor(Orange$age, Orange$circumference)
ggplot(Orange, aes(age, circumference, color = Tree)) + geom_line()
Orange %>% group_by(Tree) %>% summarize(correlation = cor(age, circumference))

cor.test(Orange$age, Orange$circumference)
Orange %>% group_by(Tree) %>% do(tidy(cor.test(.$age, .$circumference)))
Orange %>% group_by(Tree) %>% do(tidy(lm(age ~ circumference, data=.)))

mtcars %>% group_by(am) %>% do(tidy(lm(wt ~ mpg + qsec + gear, .)))
