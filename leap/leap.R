leap <- function(year) {
  if (0 == year %% 400) return(TRUE)
  if (0 == year %% 100) return(FALSE)
  0 == year %% 4
}
