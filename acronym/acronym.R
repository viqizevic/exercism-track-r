acronym <- function(input) {
  v <- unlist(strsplit(input, "[-_, ]", ""))
  paste(gsub("^(\\w).*", "\\1", toupper(v)), collapse = '')
}