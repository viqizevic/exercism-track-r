to_rna <- function(dna) {
  stopifnot(grepl("^[CGTA]*$", dna))
  chartr(old = "CGTA", new = "GCAU", dna)
}