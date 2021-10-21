largest_series_product <- function(digits, span){
  n <- nchar(digits)
  stopifnot(n >= span)
  stopifnot(grepl("^\\d*$", digits))
  if (n == 0 | span == 0) return(1)
  x <- c(1:(n-span+1))
  d <- as.numeric(unlist(strsplit(digits,'')))
  max(sapply(x, function(k) prod(d[seq.int(k,k+span-1,1)])))
}