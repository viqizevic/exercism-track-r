is_isogram <- function(word) {
  x <- sort(unlist(strsplit(tolower(gsub("[ -]","",word)), '')))
  length(x) == length(unique(x))
}