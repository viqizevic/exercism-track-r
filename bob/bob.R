bob <- function(input) {
  just_whitespace <- grepl("^\\s*$", input)
  in_uppercase <- input == toupper(input)
  a_question <- grepl("\\?\\s*$", input)
  not_number <- grepl("[A-Z]", input, ignore.case = TRUE)
  if (just_whitespace) return("Fine. Be that way!")
  if (in_uppercase & not_number) {
    if (!a_question) return("Whoa, chill out!")
    return("Calm down, I know what I'm doing!")
  }
  if (a_question) return("Sure.")
  return("Whatever.")
}