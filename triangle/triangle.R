triangle <- function(x, y, z) {
  stopifnot(min(x,y,z) > 0)
  v <- sort(c(x,y,z))
  stopifnot(v[1] + v[2] >= v[3])
  isosceles <- function(x, class=character()) {
    structure(x, class=c(class, "isosceles"))
  }
  if (x == y & y == z) class(v) <- "equilateral"
  if (x == y | x == z | y == z) v <- isosceles(v, class=class(v))
  else class(v) <- "scalene"
  return(v)
}