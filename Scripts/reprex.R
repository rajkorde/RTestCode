library(reprex)



reprex(rbinom(3, size = 10, prob = 0.5))

reprex({
  (x <- 1:4)
  (y <- 2:5)
  x + y
})