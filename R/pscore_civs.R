#' Compute extended P-scores across CIV combinations
#'
#' @description
#' Computes extended P-scores for all treatments across combinations of
#' clinically important values (CIVs) for one or more outcomes. For each
#' CIV combination, treatment ranking, Precision of Treatment Hierarchy
#' (POTH) and P-score deviances are also computed. Optionally, results can be exported to an
#' Excel file.
#'
#' @param x A list of \code{netmeta} objects, one per outcome.
#' @param CIVs A list of numeric vectors specifying the CIV values to explore,
#'   one per outcome. Its length must equal the number of outcomes.
#' @param correlation A square numeric correlation matrix of dimension k x k,
#'   where k is the number of outcomes. Use \code{NULL} or \code{diag(k)} to
#'   assume independence.
#' @param small.values A character vector of length equal to the number of outcomes,
#'   specifying whether small values indicate a beneficial ("desirable") or harmful ("undesirable") effect.
#' @param excel Logical. If \code{TRUE}, exports results to a timestamped
#'   Excel file in the working directory. Default is \code{FALSE}.
#'
#' @return An object of class \code{pscore_civs}
#'
#'
#' @seealso
#' \code{\link{aupc}}, \code{\link{plot_civsummary}},
#' \code{\link{plot_pscores}}, \code{\link{pscores_heatplot}},
#' \code{\link{pscores_pothplot}}, \code{\link{plot_aupc_pscore}}
#'
#' @export
#'
#' @import dplyr
#'
#' @examples
#' \dontrun{
#' # Efficacy
#' # Estimating P-scores for a CIV of 0.5.
#' data("efficacy")
#' out_eff <- pscore_civs(
#'   list(efficacy),
#'   CIVs        = 0.5,
#'   correlation = NULL,
#'   small.values = "desirable"
#' )
#' out_eff
#'
#' # Weight Gain
#' # Estimating P-scores for a CIV of -0.5.
#' data("weight_gain")
#' out_wg <- pscore_civs(
#'   list(weight_gain),
#'   CIVs        = -0.5,
#'   correlation = NULL,
#'   small.values= "desirable"
#' )
#' out_wg
#'
#' # Both outcomes
#' # Correlation between efficacy and weight gain assumed to be -0.5.
#' cor_mat <- matrix(c( 1.0, -0.5,
#'                     -0.5,  1.0),
#'                   nrow  = 2,
#'                   byrow = TRUE)
#'
#' # Single CIV per outcome
#' out1 <- pscore_civs(
#'   list(efficacy, weight_gain),
#'   CIVs        = list(0.5, -0.5),
#'   correlation = cor_mat,
#'   small.values        = c("desirable", "desirable")
#' )
#' out1
#'
#' # Multiple CIVs for weight gain; export results to Excel
#' out2 <- pscore_civs(
#'   list(efficacy, weight_gain),
#'   CIVs        = list(0.5, c(-0.3, -0.5)),
#'   correlation = cor_mat,
#'   small.values= c("desirable", "desirable"),
#'   excel       = TRUE
#' )
#' out2
#' }

pscore_civs<- function(x, CIVs, correlation, small.values, excel = FALSE) {

  prepare_data <- prep(x)
  outcomes <- prepare_data$outcomes
  var.outcomes <- prepare_data$var.outcomes
  comm <- prepare_data$comm

  if (length(CIVs) != dim(outcomes)[3]) {
    stop("CIVs must be a list with the same length as the number of outcomes")
  }

  if(!all(small.values %in% c("desirable", "undesirable"))) {

    stop("Valid values of small.values are desirable and undesirable.")

  }

  CIV_mat <- expand.grid(CIVs)
  names(CIV_mat) <- paste0("CIV", seq_along(CIVs))
  pscore_df <- matrix(nrow = nrow(CIV_mat), ncol = length(comm),
                      dimnames = list(list(), comm))

  # use upper bound for extended P-scores when there are more than 1 outcome or
  # CIV used (other than 0)
  if(ncol(CIV_mat) > 1) {

    extended <- rep(TRUE, nrow(CIV_mat))

  } else {

    extended <- apply(CIV_mat, 1, function(x) any(x !=0))

  }



  for (i in 1:nrow(CIV_mat)) {
    pscore_df[i,] <- pscores(outcomes = outcomes,
                             var.outcomes = var.outcomes,
                             correlation = correlation,
                             beta = as.numeric(-CIV_mat[i,]),
                             type = ifelse(small.values == "desirable", "H", "B"),
                             label = as.vector(comm))
  }


  res <- cbind(pscore_df, CIV_mat, extended) %>%
    pivot_longer(cols = 1:length(comm), names_to = "Treatment", values_to = "Pscore") %>%
    group_by(across(all_of(names(CIV_mat)))) %>%
    mutate(ranking = rank(-Pscore), poth = poth2(Pscore, extended = unique(extended)),
           deviance=Pscore-mean(Pscore)) %>% #deviances calculation
    dplyr::select(-c(extended))


  # excel export argument, default is FALSE
  if (isTRUE(excel)) {
    filename <- paste0("pscore_civs_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")

    sheets <- list(
      "All Results" = as.data.frame(res)
    )
    writexl::write_xlsx(sheets, path = filename)
    message("Results saved to: ", filename)
  }

  structure(
    list(all = res, pscores = pscore_df, CIVs = CIV_mat, labels = comm),
    class = "pscore_civs"
  )

}
# Only `all` (res) is shown on auto-print
#' Print a pscore_civs object
#'
#' Prints the `all` component of a `pscore_civs` object.
#'
#' @param x An object of class `pscore_civs`.
#' @param ... Additional arguments passed to `print()`.
#'
#' @export
print.pscore_civs <- function(x, ...) {
  print(x$all)
  invisible(x)
}

