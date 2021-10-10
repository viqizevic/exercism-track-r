tournament <- function(input) {
  # get valid lines
  games <- strsplit(input[grepl("^.+;.+;(win|draw|loss)$", input)], ";")
  # get team names
  teams <- unlist(games)
  teams <- sort(unique(teams[!(teams %in% c("win","draw","loss"))]))
  # initialize data frame
  df <- data.frame(Team=teams, MP=0, W=0, D=0, L=0)
  # iterate over the lines
  for (g in games) {
    # add every counts as per the status of the result
    if      (g[3] == "draw") df <- rbind(df, c(g[1], 1, 0, 1, 0), c(g[2], 1, 0, 1, 0))
    else if (g[3] == "win")  df <- rbind(df, c(g[1], 1, 1, 0, 0), c(g[2], 1, 0, 0, 1))
    else if (g[3] == "loss") df <- rbind(df, c(g[1], 1, 0, 0, 1), c(g[2], 1, 1, 0, 0))
    else warning("Unexpected result")
  }
  # get the sum of the results
  df[,2:5] <- sapply(df[,2:5], as.numeric)
  res <- aggregate(. ~ Team, data = df, sum)
  # calculate the points
  res$P = 3*res$W + res$D
  # order by the points
  res <- res[order(-res$P),]
  # reset row names
  row.names(res) <- NULL
  return(res)
}