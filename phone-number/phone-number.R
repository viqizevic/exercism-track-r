parse_phone_number <- function(number_string) {
  t <- gsub("^1","", gsub("\\D","", number_string))
  cd <- c("0","1")
  if (substr(t,1,1) %in% cd) return(NULL)
  if (substr(t,4,4) %in% cd) return(NULL)
  if (10 == nchar(t)) return(t)
}