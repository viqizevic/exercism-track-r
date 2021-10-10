raindrops <- function(number) {
  v <- c(3,5,7)
  w <- c("Pling","Plang","Plong")
  x <- (0 == number %% v)
  ifelse(any(x), paste(w[x],collapse=''), as.character(number))
}
