square <- function(n) {
  if (n < 1 | 64 < n) stop("n not between 1 and 64")
  return(2^(n-1))
}

total <- function() {
  sum(sapply(c(1:64), square))
}