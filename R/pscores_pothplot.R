#' Bubble plot of POTH values across CIV combinations
#'
#' @description
#' Creates a bubble plot visualising Precision of Treatment Hierarchy (POTH)
#' values across a grid of clinically important values (CIVs) for exactly two
#' outcomes. Bubble size represents POTH magnitude, with the largest and
#' smallest values optionally highlighted in distinct colours. This reveals
#' regions where treatment rankings are most or least precise.
#'
#' @param x An object of class \code{"pscore_civs"} as returned by
#'   \code{pscore_civs()}, containing POTH values across CIV combinations.
#' @param newgridsize Grid size for downsampling. \code{0} (default) uses the
#'   original grid from \code{x}. A positive integer \code{n} creates an
#'   \code{n x n}  grid.
#' @param highlight Logical. If \code{TRUE} (default), highlights the maximum
#'   (blue) and minimum (pink) POTH values with distinct colours.
#' @param title Optional character string for the plot title. Defaults to
#'   \code{"POTH plot"}.
#' @param outcome1 Optional character string for the x-axis label.
#'   Defaults to \code{"Outcome 1"}.
#' @param outcome2 Optional character string for the y-axis label.
#'   Defaults to \code{"Outcome 2"}.
#'
#' @details
#' The function requires exactly two CIV dimensions (columns in \code{x$CIVs}).
#'
#' @return A list containing:
#' \describe{
#'   \item{plot}{A \code{ggplot2} bubble plot object.}
#'   \item{grid}{A tibble of the displayed CIV combinations and their POTH
#'     values.}
#' }
#'
#' @seealso \code{\link{pscore_civs}}, \code{\link{pscores_heatplot}}
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
#'   CIVs        = list(seq(0, 0.5, length.out = 50),
#'                      seq(-0.5, 0, length.out = 50)),
#'   correlation = cor_mat,
#'   type        = c("H", "H")
#' )
#'
#' # Full grid with highlighting
#' pscores_pothplot(out)
#'
#' # Downsampled grid
#' pscores_pothplot(out, newgridsize = 10)
#'
#' # Custom labels
#' pscores_pothplot(
#'   out,
#'   newgridsize = 10,
#'   title       = "POTH Plot",
#'   outcome1    = "Efficacy",
#'   outcome2    = "Weight Gain"
#' )
#'
#' # Save plot
#' g <- pscores_pothplot(out)
#' ggplot2::ggsave("pothplot.png", g$plot, width = 8, height = 6)
#' }
pscores_pothplot <- function(x, newgridsize = 0, highlight = TRUE, title = "POTH plot",outcome1 = "Outcome 1", outcome2 = "Outcome 2") {

  if(ncol(x$CIVs) != 2) {

    stop("Function only designed for 2-outcome p-scores right now")

  }

  bub <- x$all %>% dplyr::summarise(POTH = unique(poth), .groups = "keep")

  if(newgridsize > 0) {

    keepx <- quantile(x$CIVs[,1], probs = 0:(newgridsize-1)/(newgridsize-1), type = 1)
    keepy <- quantile(x$CIVs[,2], probs = 0:(newgridsize-1)/(newgridsize-1), type = 1)

    ix <- which(as.data.frame(bub[,1])[,1] %in% keepx & as.data.frame(bub[,2])[,1] %in% keepy)

    bub <- bub[ix,]

  }


  if(highlight) {

    plt <- dplyr::ungroup(bub) %>%
      dplyr::mutate(status = dplyr::case_when(
        POTH == max(POTH) ~ "Largest",
        POTH == min(POTH) ~ "Smallest",
        TRUE              ~ "other"
      ))

  } else {

    plt <- bub %>% mutate(status = "other")

  }


  g <-ggplot(plt, aes(x = !!sym(names(x$CIVs)[1]),
                      y = !!sym(names(x$CIVs)[2]),
                      size = POTH, col = status, fill = status)) +
    geom_point(alpha = 0.7, shape = 21) +
    scale_size(range = c(1, 20)) +
    scale_fill_manual(breaks = c("Largest", "Smallest"),
                      values = c(Largest = "lightskyblue3", Smallest = "hotpink3", other = "lightyellow")) +
    scale_color_manual(breaks = c("Largest", "Smallest"),
                       values = c(Largest = "lightskyblue4", Smallest = "hotpink4", other = "black")) +
    guides(size = guide_legend(override.aes = list(fill = "lightyellow"), order = 1),
           fill = guide_legend(override.aes = list(size = 10),
                               title = "Useful POTH Values"),
           colour = guide_legend(title = "Useful POTH Values")) +
    coord_cartesian(clip = "off") +
    labs(x = paste0("CIV (", outcome1, ")"),
         y = paste0("CIV (", outcome2, ")"),
         title = title) +
    theme_bw()


  print(g)

  return(list(plot = g, grid = bub))


}
