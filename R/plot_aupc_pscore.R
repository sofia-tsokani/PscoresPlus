#' Compare AUPC and P-score treatment rankings
#'
#' @description
#' Creates a scatter plot comparing treatment rankings from AUPC and P-score
#' analyses. Only treatments common to both inputs are shown.
#'
#' @param aupc A data frame returned by \code{aupc()}.
#' @param pscore A \code{pscore_civs} object.
#' @param outcome_name Optional character string for the plot title, depicting
#'   the outcome name. Defaults to \code{""} (no title).
#'
#' @details
#' The plot depicts treatment rankings according to AUPC and P-scores, draws
#' the identity line y = x, and labels each treatment directly on the plot.
#' In brief, it is a scatterplot of treatment ranks using two different methods
#' of creating the hierarchy.
#'
#' @return A \code{ggplot2} object showing AUPC ranks on the x-axis and
#'   P-score ranks on the y-axis.
#'
#' @seealso \code{\link{aupc}}, \code{\link{pscore_civs}}
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
#'   type        = "H"
#' )
#'
#' # AUPC over a CIV range of 0.20 to 0.40
#' pscore_range <- pscore_civs(
#'   list(efficacy),
#'   CIVs        = list(seq(0.2, 0.4, 0.1)),
#'   correlation = NULL,
#'   type        = "H"
#' )
#' aupc_res <- aupc(pscore_range)
#'
#' plot_aupc_pscore(aupc_res, eff_rank, outcome_name = "Efficacy")
#' }

plot_aupc_pscore <- function(aupc, pscore,
                                outcome_name="") {

  master <- merge(
    aupc[, c("Treatment", "ranking")],
    pscore$all[, c("Treatment", "ranking")],
    by = "Treatment",
    suffixes = c(".aupc", ".pscore")
  )

  ggplot2::ggplot(master, ggplot2::aes(ranking.aupc, ranking.pscore)) +
    ggplot2::geom_abline(slope = 1, intercept = 0) +
    ggplot2::geom_label(
      ggplot2::aes(label = Treatment),
      fill = "cornsilk",
      size = 5
    ) +
    ggplot2::labs(x = "AUPC Rank", y = "P-score Rank",title=outcome_name) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.title = ggplot2::element_text(size = 12),
      axis.text = ggplot2::element_text(size = 12)
)
}
