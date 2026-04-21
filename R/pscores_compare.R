#' Compare treatment rankings across two P-score analyses
#'
#' @description
#' Compares treatment rankings from two P-score analyses (e.g., two outcomes
#' or the same outcome with different CIVs) by plotting a scatter of ranks
#' for each treatment. Treatments are labelled on the plot, and the identity
#' line y = x is added to visualise agreement or disagreement in rankings.
#'
#' @param x1 A \code{pscore_civs} object as returned by \code{pscore_civs()}.
#' @param x2 A second \code{pscore_civs} object with the same structure as
#'   \code{x1}.
#' @param outcome1 Character string for the x-axis label (first outcome).
#'   Defaults to \code{"Outcome 1"}.
#' @param outcome2 Character string for the y-axis label (second outcome).
#'   Defaults to \code{"Outcome 2"}.
#'
#' @details
#' The function creates a scatter plot of rankings for common treatments across
#' the two \code{pscore_civs} objects, using the identity line y = x and
#' treatment labels directly on the plot.
#'
#' @return A \code{ggplot2} scatter plot with rankings for outcome 1 on the
#'   x-axis and rankings for outcome 2 on the y-axis.
#'
#' @seealso \code{\link{pscore_civs}}, \code{\link{pscores_heatplot}},
#'   \code{\link{plot_aupc_pscore}}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data("efficacy")
#' data("weight_gain")
#'
#' out_eff <- pscore_civs(
#'   list(efficacy),
#'   CIVs        = 0.5,
#'   correlation = NULL,
#'   type        = "H"
#' )
#'
#' out_wg <- pscore_civs(
#'   list(weight_gain),
#'   CIVs        = -0.5,
#'   correlation = NULL,
#'   type        = "H"
#' )
#'
#' pscores_compare(
#'   out_eff,
#'   out_wg,
#'   outcome1 = "Efficacy",
#'   outcome2 = "Weight Gain"
#' )
#' }
pscores_compare <- function(x1, x2, outcome1="Outcome 1", outcome2="Outcome 2") {
  x1_tibble <- x1$all
  df1<-x1_tibble %>%
    select(Treatment, ranking)
  x2_tibble <- x2$all
  df2<-x2_tibble %>%
    select(Treatment, ranking)
  master <- left_join(df1, df2, join_by(Treatment == Treatment))

  g <-ggplot(master, aes(x = ranking.x, y = ranking.y)) +
    # geom_point(size = 4, col = "black", shape = 22) +
    geom_abline(slope = 1, intercept = 0) +
    geom_label(aes(label = Treatment), fill = "lightyellow") +
    guides(fill = guide_legend(override.aes = aes(label = "  "))) +
    labs(x = paste0("Ranks for ",outcome1), y = paste0("Ranks for ",outcome2)) +
    # scale_fill_viridis_d(option = "turbo", begin = 0.05)+
    theme_bw()

  print(g)


}
