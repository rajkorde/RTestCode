library(hrbrthemes)
library(gcookbook)
library(tidyverse)

ggplot(mtcars, aes(mpg, wt)) +
  geom_point() +
  labs(x="Fuel effiiency (mpg)", y="Weight (tons)",
       title="Seminal ggplot2 scatterplot example",
       subtitle="A plot that is only useful for demonstration purposes",
       caption="Brought to you by the letter 'g'") +
  theme_ipsum()

ggplot(mtcars, aes(mpg, wt)) +
  geom_point() +
  labs(x="Fuel effiiency (mpg)", y="Weight (tons)",
       title="Seminal ggplot2 scatterplot example",
       subtitle="A plot that is only useful for demonstration purposes",
       caption="Brought to you by the letter 'g'") + 
  theme_ipsum_rc()

ggplot(mtcars, aes(mpg, wt)) +
  geom_point(aes(color=factor(carb))) +
  labs(x="Fuel effiiency (mpg)", y="Weight (tons)",
       title="Seminal ggplot2 scatterplot example",
       subtitle="A plot that is only useful for demonstration purposes",
       caption="Brought to you by the letter 'g'") + 
  scale_color_ipsum() +
  theme_ipsum_rc()

count(mpg, class) %>% 
  mutate(pct=n/sum(n)) %>% 
  ggplot(aes(class, pct)) +
  geom_col() +
  scale_y_percent() +
  labs(x="Fuel effiiency (mpg)", y="Weight (tons)",
       title="Seminal ggplot2 column chart example with percents",
       subtitle="A plot that is only useful for demonstration purposes",
       caption="Brought to you by the letter 'g'") + 
  theme_ipsum(grid="Y")

library(Cairo)
CairoPNG(width = 1600, height = 1200, file = "test.png")
ggplot(uspopage, aes(x=Year, y=Thousands, fill=AgeGroup)) + 
  geom_area() +
  scale_fill_ipsum() +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_comma() +
  labs(title="Age distribution of population in the U.S., 1900-2002",
       subtitle="Example data from the R Graphics Cookbook",
       caption="Source: R Graphics Cookbook") +
  theme_ipsum_rc(grid="XY") +
  theme(axis.text.x=element_text(hjust=c(0, 0.5, 0.5, 0.5, 1))) +
  theme(legend.position="bottom")
dev.off()