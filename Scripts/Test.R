c1 <- c()

f <- function(x) {
  c1 <<- c(c1, x) 
}

f(5)
f(10)
c1
f(11)