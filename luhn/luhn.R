# Determine whether the number is valid.
is_valid <- function(input) {
  # Remove spaces
  input <- gsub("\\s+","",input)
  # If any non-digit then not valid
  if (grepl("\\D",input)) return(FALSE)
  # Drop starting zero
  input <- gsub("^0","",input)
  # If less than 2 characters, not valid
  if (nchar(input) <= 1) return(FALSE)
  # Get digits separated
  x <- as.numeric(unlist(strsplit(input,"")))
  # Get the odds
  odds <- c(1:length(x)) %% 2 == 1
  # Double the odds
  y <- x[odds]*2
  # Minus 9 if greater than 9
  x[odds] <- ifelse(y > 9, y - 9, y)
  # Sum is divisible by 10 or not
  sum(x) %% 10 == 0
}