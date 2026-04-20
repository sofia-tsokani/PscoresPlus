# Helpers -----------------
poth2 <- function(pscores) {

  n <- length(pscores)

  sq <- pscores - mean(pscores)
  p <- sum(sq*sq)/n*12*(n-1)/(n+1)

  return(p)

}
