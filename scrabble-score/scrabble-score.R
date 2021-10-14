scrabble_score <- function(input){
  score <- function(x) {
    if (grepl('[AEIOULNRST]',x)) return(1)
    else if (grepl('[DG]',x)) return(2)
    else if (grepl('[BCMP]',x)) return(3)
    else if (grepl('[FHVWY]',x)) return(4)
    else if (grepl('[K]',x)) return(5)
    else if (grepl('[JX]',x)) return(8)
    else if (grepl('[QZ]',x)) return(10)
    else return(0)
  }
  if (nchar(input) == 0) return(0)
  sum(sapply(unlist(strsplit(toupper(input), '')), score))
}