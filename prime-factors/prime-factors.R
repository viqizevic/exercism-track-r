prime_factors <- function(number, k=2, x=c()) {
  if (number <= 1) return(x)
  while (number %% k != 0) k <- k+1
  prime_factors(number/k, k, c(x,k))
}