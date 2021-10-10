is_pangram <- function(input) {
  input <- gsub('[^a-z]+',"",tolower(input))
  26 == length(unique(sort(unlist(strsplit(input,"")))))
}