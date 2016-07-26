library(dplyr)
library(nycflights13)
library(gapminder)
library(broom)
library(ggplot2)
library(magrittr)

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