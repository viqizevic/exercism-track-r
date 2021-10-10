lyrics <- function(first, last) {
  x <- seq.int(from = first, to = last, by = -1)
  paste(sapply(x, verse), collapse = '\n')
}

verse <- function(n) {
  nomore <- "No more"
  btl <- function(b) return(paste0(b," bottle",ifelse(b==1,"","s")," of beer"))
  ncurrent <- ifelse(n>=1, n, nomore)
  l1 <- paste0(btl(ncurrent), " on the wall, ", btl(tolower(ncurrent)), ".")
  onedown <- ifelse(n>1, "one", "it")
  takedown <- paste0("Take ",onedown," down and pass it around,")
  donext <- ifelse(n>0, takedown, "Go to the store and buy some more,")
  nleft <- ifelse(n>1, n-1, ifelse(n==1,tolower(nomore), 99) )
  l2 <- paste(donext, btl(nleft), "on the wall.")
  paste0(l1,'\n',l2, '\n')
}