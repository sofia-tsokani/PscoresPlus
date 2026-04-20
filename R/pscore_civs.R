#' Compute extended P-scores across CIV combinations
#'
#' Computes P-scores for all treatments across combinations of clinically
#' important values (CIVs) for one or more outcomes.
#'
#' @param x A list of `netmeta` objects.
#' @param CIVs A list of CIV values to explore. Its length must match the
#'   number of outcomes.
#' @param correlation The correlation matrix describing the correlation
#'   between the outcomes, or `NULL` to assume zero correlation.
#' @param type A character vector describing the type of outcomes:
#'   `"H"` for harmful and `"B"` for beneficial.
#' @param excel Logical; if `TRUE`, exports the full results to an Excel file.
#'
#' @return An object of class `pscore_civs`, containing:
#' \itemize{
#'   \item `all`: long-format results including P-scores, rankings, POTH, and residuals.
#'   \item `pscores`: matrix of P-scores across CIV combinations.
#'   \item `CIVs`: data frame of CIV combinations.
#'   \item `labels`: treatment labels.
#' }
#'
#' @export

pscore_civs<- function(x, CIVs, correlation, type, excel = FALSE) {

  prepare_data <- prep(x)
  outcomes <- prepare_data$outcomes
  var.outcomes <- prepare_data$var.outcomes
  comm <- prepare_data$comm

  if (length(CIVs) != dim(outcomes)[3]) {
    stop("CIVs must be a list with the same length as the number of outcomes")
  }

  CIV_mat <- expand.grid(CIVs)
  names(CIV_mat) <- paste0("CIV", seq_along(CIVs))
  pscore_df <- matrix(nrow = nrow(CIV_mat), ncol = length(comm),
                      dimnames = list(list(), comm))

  for (i in 1:nrow(CIV_mat)) {
    pscore_df[i,] <- pscores(outcomes = outcomes,
                             var.outcomes = var.outcomes,
                             correlation = correlation,
                             beta = as.numeric(-CIV_mat[i,]),
                             type = type,
                             label = as.vector(comm))
  }

  res <- cbind(pscore_df, CIV_mat) %>%
   pivot_longer(cols = 1:length(comm), names_to = "Treatment", values_to = "Pscore") %>%
    group_by(across(all_of(names(CIV_mat)))) %>%
   mutate(ranking = rank(-Pscore), poth = poth2(Pscore),
           residuals=Pscore-mean(Pscore)) #residuals calculation

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

