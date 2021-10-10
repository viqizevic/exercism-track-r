anagram <- function(subject, candidates) {
  li <- tolower(candidates) %>% lapply(utf8ToInt) %>% lapply(sort)
  y <- sort(utf8ToInt(tolower(subject)))
  v <- c()
  for (i in seq_along(candidates)) {
    cand <- candidates[i]
    x <- li[i][[1]]
    if (length(x) == length(y) && all(x == y) && tolower(subject) != tolower(cand)) {
      v <- append(v, cand)
    }
  }
  v
}