sum_of_multiples <- function(factors, limit) {
  if (limit <= 1) return(0)
  v <- seq.int(1, limit-1, 1)
  w <- sapply(v, function(x) any((x %% factors) == 0))
  sum(v[w])
}