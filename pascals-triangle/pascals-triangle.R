pascals_triangle <- function(n) {
  stopifnot(n >= 0)
  if (n == 0) return(list())
  if (n == 1) return(list(1))
  # SMART: lapply(0:(n - 1), function(m) choose(m, 0:m))
  p <- pascals_triangle(n-1)
  if (n == 2) p[[2]] <- c(1,1)
  else {
    v <- p[[n-1]]
    x <- sapply(c(1:(n-2)), function(k) v[k]+v[k+1])
    p[[n]] <- c(1,x,1)
  }
  return(p)
}