collatz_step_counter <- Vectorize(function(num, k=0) {
  if (k==0 & num<=0) stop("Not expected input <= 0")
  if (num == 1) return(k)
  collatz_step_counter(ifelse(num%%2==0, num/2, 3*num+1), k+1)
})