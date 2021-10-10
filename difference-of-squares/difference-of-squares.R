difference_of_squares <- function(n) {
  v <- c(1:n)
  sum(v)^2 - sum(v^2)
}