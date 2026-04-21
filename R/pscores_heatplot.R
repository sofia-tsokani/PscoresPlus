#' Heat plot of treatment rankings across CIV combinations
#'
#' @description
#' Visualises treatment rankings across a grid of clinically important values
#' (CIV) for exactly two outcomes as a heat plot. Each facet represents one
#' treatment, with the two CIV axes on the x and y axes and the rank based on
#' the extended P-score shown as a colour gradient. This provides granular
#' information about the specific regions of CIV combinations where a treatment
#' performs better or worse than others, complementing the AUPC summary.
#'
#' @param x An object of class \code{"pscore_civs"} as returned by
#'   \code{pscore_civs()}, containing the extended P-scores and rankings
#'   across all CIV combinations.
#' @param title Optional character string for the plot title. Defaults to
#'   \code{""} (no title).
#' @param outcome1 Optional character string for the x-axis label for the
#'   first outcome. Defaults to \code{"Outcome 1"}.
#' @param outcome2 Optional character string for the y-axis label for the
#'   second outcome. Defaults to \code{"Outcome 2"}.
#'
#' @details
#' The function requires exactly two outcomes. Treatments are ordered in the
#' facets by their mean P-score across all CIV combinations, from highest
#' to lowest.
#'
#' @return A \code{ggplot2} object. The plot is not printed automatically and
#'   can be displayed by calling the returned object or saved with
#'   \code{ggplot2::ggsave()}.
#'
#' @seealso \code{\link{pscore_civs}}, \code{\link{pscores_pothplot}},
#'   \code{\link{aupc}}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data("efficacy")
#' data("weight_gain")
#'
#' cor_mat <- matrix(c(1, -0.5, -0.5, 1), nrow = 2, byrow = TRUE)
#'
#' out <- pscore_civs(
#'   list(efficacy, weight_gain),
#'   CIVs        = list(seq(0,  0.5, length.out = 50),
#'                      seq(-0.5, 0, length.out = 50)),
#'   correlation = cor_mat,
#'   type        = c("H", "H")
#' )
#'
#' pscores_heatplot(
#'   out,
#'   title    = "Efficacy & Weight Gain",
#'   outcome1 = "Efficacy",
#'   outcome2 = "Weight Gain"
#' )
#' }
pscores_heatplot <- function(x, title = "", outcome1 = "Outcome 1", outcome2 = "Outcome 2") {

  if(ncol(x$CIVs) != 2) {

    stop("Only implemented for 2 outcomes")

  }

  x$all$Treatment <- factor(x$all$Treatment, levels = x$all$Treatment[order(colMeans(x$pscores), decreasing = TRUE)])

  civ1_name <- outcome1
  civ2_name <- outcome2

  ggplot2:: ggplot(x$all, aes(x = !!sym(names(x$CIVs)[1]),
                    y = !!sym(names(x$CIVs)[2]),
                    fill = ranking)) +
    facet_wrap(~Treatment) +
    geom_tile() +
    scale_fill_viridis_c() +
      labs(x = paste0("CIV (", civ1_name, ")"),
           y = paste0("CIV (", civ2_name, ")"),
         fill = "Rank based on Extended P-score",
         title = title)

}
