fizz_buzz <- function(input) {
  x <- c(1:input)
  f <- 0 == x %% 3
  b <- 0 == x %% 5
  x[f & !b] <- "Fizz"
  x[b & !f] <- "Buzz"
  x[f & b] <- "Fizz Buzz"
  x
}