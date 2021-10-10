word_count <- function(input) {
  v <- gsub("[^a-z0-9']", "", unlist(strsplit(tolower(input), "\\s")))
  as.list(table(v[v != ""]))
}