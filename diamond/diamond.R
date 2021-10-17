diamond <- function(letter) {
  k <- which.max(letter == LETTERS)
  n <- 2*k-1
  dline <- function(j) {
    os <- paste(rep.int(' ', abs(j-k)), collapse = '')
    lt <- LETTERS[min(j,2*k-j)]
    nis <- max(0, n - 2*nchar(os) - 2)
    is <- paste(rep.int(' ', nis), collapse = '')
    paste(os,lt,is,ifelse(nis>0,lt,''),os, sep='')
  }
  paste(unlist(lapply(1:n, dline)), collapse = '\n')
}