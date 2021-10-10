sieve <- function(limit) {
  if(limit < 2) return(NULL)
  v <- c(1:limit)
  marked <- rep_len(FALSE, length.out = limit)
  marked[1] <- TRUE
  p <- 2
  while (p <= limit) {
    if (2*p <= limit) {
      s <- seq.int(from = 2*p, to = limit, by = p)
      marked[s] <- TRUE
    }
    w <- v[!marked & v>p]
    p <- ifelse(length(w)>0, min(w), limit+1)
  }
  v[!marked]
}