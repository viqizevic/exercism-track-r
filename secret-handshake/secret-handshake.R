handshake <- function(n) {
  x <- as.integer(intToBits(n))[1:5]
  s <- c("wink", "double blink", "close your eyes", "jump")
  r <- s[x[1:4]==1]
  if (length(r) == 0) return(c())
  if (x[5]==0) return(r)
  else return(rev(r))
}