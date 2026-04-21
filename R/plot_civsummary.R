#' Plot summary statistics over CIVs
#'
#' @description
#' Creates a bar plot of a summary measure returned by \code{aupc()}.
#' Treatments can be ordered by AUPC value or kept in the original order.
#'
#' @param obj A data frame returned by \code{aupc()}, containing
#'   treatment labels and a summary measure in the first column.
#' @param ordering Character string controlling the order of treatments on
#'   the x-axis. Use \code{"ranking"} to order treatments by AUPC or
#'   \code{"asis"} to keep the original order. Default is \code{"ranking"}.
#' @param title Optional character string for the plot title.
#'   Default is \code{""}.
#'
#' @details
#' If \code{ordering = "ranking"}, treatments are reordered according to the
#' first column of \code{aupc}. The function uses the first column name as
#' the plotted measure.
#'
#' @return A \code{ggplot2} bar plot object showing the summary AUPC by
#'   treatment.
#'
#' @seealso \code{\link{aupc}}, \code{\link{pscore_civs}}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data("efficacy")
#'
#' out_eff <- pscore_civs(
#'   list(efficacy),
#'   CIVs        = list(seq(0, 0.5, length.out = 50)),
#'   correlation = NULL,
#'   type        = "H"
#' )
#'
#' obj <- aupc(out_eff)
#'
#' plot_civsummary(obj, ordering = "asis", title = "CIV Profile for Efficacy Outcome")
#' plot_civsummary(obj, title = "CIV Profile for Efficacy Outcome")
#' }
plot_civsummary <- function(obj, ordering = "ranking", title = "") {

  measure <- colnames(obj)[1]

  if(ordering == "ranking") { # reorder factor so the x axis is in order of least to most preferred

    obj$Treatment <- reorder(obj$Treatment, obj[,colnames(obj) == measure])

  }

 ggplot2:: ggplot(obj, aes(x = Treatment, y = !!sym(measure))) +
    geom_col(col = "black", fill = "skyblue3") +
    geom_hline(yintercept = 0) +
    labs(title = title) +
    theme_bw()

}
