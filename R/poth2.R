# Helpers -----------------
poth2 <- function(pscores, extended = FALSE) {

  n <- length(pscores)

  classic <- (n+1)/(n-1)/12

  # need to manually find maximum
  if(extended) {

    ubound <- ssqbound(n)

  } else {

    ubound <- classic

  }

  sq <- pscores - mean(pscores)
  p <- sum(sq*sq)/n/ubound

  return(p)

}

ssqbound <- function(n) {

  ssq <- numeric(n-1) # don't need to check nth value bc it will be 0

  for(m in 1:(n-1)) { #

    ssq[m] <- varfunc(m,n)

  }

  ub <- max(ssq)

  return(ub)

}

varfunc <- function(m,n) {

  # if(m>(n/2)) {
  #
  #   print(paste0("m = ", m))
  #
  #   oldmean <- (sum((m:n)-1))/(n-1)/n
  #   olddist <- (m-1)/(n-1)-oldmean
  #   newdist <- (sum(((m+1):n)-1))/(n-1)/n
  #
  #   print(paste0("Old distance: ", olddist, " New distance: ", newdist))
  #
  # }


  num <- n*(n-1)*(2*n-1)/6 - m*(m-1)*(2*m-1)/6 - (n^2-n-m^2+m)^2/n/4

  return(num/n/(n-1)^2)

}
