normalized_plaintext <- function(input) {
  gsub("[^0-9a-z]","",tolower(input))
}

plaintext_segments <- function(input) {
  x <- normalized_plaintext(input)
  n <- nchar(x)
  col <- ceiling(sqrt(n))
  if (n == 0) return("")
  sapply(c(1:ceiling(n/col)), function(r) substr(x, ((r-1)*col+1), min(r*col,n)))
}

encoded <- function(input) {
  gsub(" ", "", ciphertext(input))
}

ciphertext <- function(input) {
  p <- plaintext_segments(input)
  col <- nchar(p[1])
  getcol <- function(k)
    paste(sapply(p, function(q) ifelse(nchar(q) < k, " ", substr(q,k,k))), collapse = '')
  if (col == 0) return("")
  paste0(sapply(c(1:col), function(j) getcol(j)), collapse = " ")
}