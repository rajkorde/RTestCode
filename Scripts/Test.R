library(purrr)

mtcars %>% split(.$cyl) %>%
  map(~lm(mpg~wt, data=.)) %>%
  map(summary) %>%
  map("df") %>%
  map("[1]")

1:10 %>%
  keep(x > 2)
