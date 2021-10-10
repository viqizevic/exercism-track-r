number_type <- function(n){
  stopifnot(n > 0)
  v <- c(1:(n-1))
  s <- ifelse(n==1, 0, sum(v[n %% v == 0]))
  if (s == n) return("perfect")
  else if (s > n) return("abundant")
  else return("deficient")
}