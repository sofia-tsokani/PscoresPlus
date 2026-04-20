#' @name pscores_compare
#' @title Compare P-scores for two outcomes
#' @param x1 data.frame with column names `Treatment` and `ranking`
#' @param x2 data.frame with column names `Treatment` and `ranking`
#' @param name1 Name of the first group of rankings (x axis label)
#' @param name2 Name of the second group of rankings (y axis label)
#' @return A `ggplot2` object.
#' @export
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
