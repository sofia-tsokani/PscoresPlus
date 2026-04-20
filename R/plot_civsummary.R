#' Plot CIV summary
#'
#' @name plot_civsummary
#' @title Plot CIV summary
#' @description Creates a bar plot of a summary measure returned by AUPC.
#'
#' @param obj an object from running either aupc() or abpmc()
#' @param ordering either "ranking" for treatments on the x axis to be ordered by the measure, or "asis" to keep the same order of treatments as in obj
#' @param title optional title for the plot.
#' @return A `ggplot2` object.
#' @export
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
