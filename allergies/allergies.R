ALLERGIES <- c("eggs", "peanuts", "shellfish", "strawberries", "tomatoes", "chocolate", "pollen", "cats")

allergy <- function(num) {
  as.logical(intToBits(num))
}

allergic_to <- function(allergy_object, allergy) {
  allergy %in% list_allergies(allergy_object)
}

list_allergies <- function(allergy_object) {
  na.exclude(ALLERGIES[allergy_object])
}