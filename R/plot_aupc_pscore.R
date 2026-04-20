#' Compare AUPC and P-score treatment rankings
#'
#' @name plot_aupc_pscore
#' @title Compare AUPC and P-score treatment rankings
#' @description Compare treatment rankings derived from AUPC and extended P-scores.
#'
#' @param aupc A data frame returned by `aupc()`.
#' @param pscore A `pscore_civs` object.
#' @param outcome_name Optional title for the plot depicting the outcome name.
#'
#' @return A `ggplot2` object.
#' @export

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
