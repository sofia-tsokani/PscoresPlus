#' Heatplots
#'
#' @name pscores_heatplot
#' @title Heatplot for 2 outcomes
#' @param x object from running pscore_civs
#' @param title optional title for plot
#' @param outcome1 Name for outcome 1
#' @param outcome2 Name for outcome 2
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
