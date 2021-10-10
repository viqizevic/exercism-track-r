nucleotide_count <- function(input) {
  stopifnot(grepl("^[ACGT]*$",input))
  v <- unlist(strsplit(input,''))
  sapply(c("A", "C", "G", "T"), function(x) sum(v == x), simplify = FALSE)
}