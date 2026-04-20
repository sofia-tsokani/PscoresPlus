#' Calculate AUPC
#'
#' @name aupc
#' @title Calculate AUPC and rankings based on this.
#' @export

aupc <- function(x,excel=FALSE) {

  df <- x$pscores
  civs <- x$CIVs

  labels <- colnames(df)

  # check
  if(nrow(df) != nrow(civs)) {

    stop("Number of rows of df and civs must be equal")

  } else if(nrow(civs) < 2) {

    stop("More than one combo of CIVs must be considered to calculate the area")

  }

  # Drop the rows which only have one CIV

  nval <- apply(civs, 2, function(x) length(unique(x)))

  civs <- as.matrix(civs[,which(nval>1)])

  hs <- apply(civs, 2, function(x) (max(x)-min(x))/length(unique(x)))

  auc <- apply(df, 2, function(x, h) sum(x*prod(h)), h = hs)

  names(auc) <- labels

  ranking <- rank(-auc)

  res<-(data.frame(AUPC = auc, Treatment = labels, ranking = ranking)) # average probability that it beats all other treatments, averaged over the CIVs

  if (isTRUE(excel)) {
    filename <- paste0("aupc_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
    writexl::write_xlsx(list("AUPC Results" = res), path = filename)
    message("Results saved to: ", filename)
  }

  return(res)
}
