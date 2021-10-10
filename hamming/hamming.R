# This is a stub function to take two strings
# and calculate the hamming distance
hamming <- function(strand1, strand2) {
  if (nchar(strand1) != nchar(strand2)) stop("Sequences of different lengths are not acceptable!")
  spl <- function(s) unlist(strsplit(s,''))
  sum(spl(strand1) != spl(strand2))
}