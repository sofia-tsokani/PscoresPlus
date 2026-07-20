#' Compare APC and P-score treatment rankings
#'
#' @description
#' Creates a scatter plot comparing treatment rankings from APC and P-score
#' analyses. Only treatments common to both inputs are shown.
#'
#' @param apc A data frame returned by \code{apc()}.
#' @param pscore A \code{pscore_civs} object.
#' @param outcome_name Optional character string for the plot title, depicting
#'   the outcome name. Defaults to \code{""} (no title).
#'
#' @details
#' The plot depicts treatment rankings according to APC and P-scores, draws
#' the identity line y = x, and labels each treatment directly on the plot.
#' In brief, it is a scatterplot of treatment ranks using two different methods
#' of creating the hierarchy.
#'
#' @return A \code{ggplot2} object showing APC ranks on the x-axis and
#'   P-score ranks on the y-axis.
#'
#' @seealso \code{\link{apc}}, \code{\link{pscore_civs}}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data("efficacy")
#'
#' # Extended P-scores at a single CIV of 0.30
#' eff_rank <- pscore_civs(
#'   list(efficacy),
#'   CIVs        = 0.30,
#'   correlation = NULL,
#'   small.values = "desirable"
#' )
#'
#' # APC over a CIV range of 0.20 to 0.40
#' pscore_range <- pscore_civs(
#'   list(efficacy),
#'   CIVs        = list(seq(0.2, 0.4, 0.1)),
#'   correlation = NULL,
#'   small.values = "desirable"
#' )
#' apc_res <- apc(pscore_range)
#'
#' plot_apc_pscore(apc_res, eff_rank, outcome_name = "Efficacy")
#' }

plot_apc_pscore <- function(apc, pscore,
                                outcome_name="") {

  master <- merge(
    apc[, c("Treatment", "ranking")],
    pscore$all[, c("Treatment", "ranking")],
    by = "Treatment",
    suffixes = c(".apc", ".pscore")
  )

  ggplot2::ggplot(master, ggplot2::aes(ranking.apc, ranking.pscore)) +
    ggplot2::geom_abline(slope = 1, intercept = 0) +
    ggplot2::geom_label(
      ggplot2::aes(label = Treatment),
      fill = "cornsilk",
      size = 5
    ) +
    ggplot2::labs(x = "APC Rank", y = "P-score Rank",title=outcome_name) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.title = ggplot2::element_text(size = 12),
      axis.text = ggplot2::element_text(size = 12)
)
}
