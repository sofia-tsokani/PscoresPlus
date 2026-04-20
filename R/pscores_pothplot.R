#' Plot POTH
#'
#' @name pscores_pothplot
#' @title Pothplots
#' @param x object from running pscore_civs. Should include exactly 2 outcomes
#' @param newgridsize Grid size. 0 indicates to use the grid size of the original object
#' @param hightlight logical; should the minimum and maximum POTH values in the new grid be highlighted?
#' @param title optional title for plot
pscores_pothplot <- function(x, newgridsize = 0, highlight = TRUE, title = "POTH plot",outcome1 = "Outcome 1", outcome2 = "Outcome 2") {

  if(ncol(x$CIVs) != 2) {

    stop("Function only designed for 2-outcome p-scores right now")

  }

  bub <- x$all %>%summarise(POTH = unique(poth), .groups = "keep")

  if(newgridsize > 0) {

    keepx <- quantile(x$CIVs[,1], probs = 0:(newgridsize-1)/(newgridsize-1), type = 1)
    keepy <- quantile(x$CIVs[,2], probs = 0:(newgridsize-1)/(newgridsize-1), type = 1)

    ix <- which(as.data.frame(bub[,1])[,1] %in% keepx & as.data.frame(bub[,2])[,1] %in% keepy)

    bub <- bub[ix,]

  }


  if(highlight) {

    plt <- ungroup(bub) %>%
      mutate(status = case_when(POTH == max(POTH) ~ "Largest",
                                POTH == min(POTH) ~ "Smallest",
                                TRUE ~ "other"))

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
