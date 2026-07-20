pscore_civ_profile  <- function(x, beta, type, top_n = 5){

  pscores_applied <- matrix(
    rep(0, length(beta) * length(prep(x)$comm)),
    nrow = length(beta),
    ncol = length(prep(x)$comm)
  )
  colnames(pscores_applied) <- prep(x)$comm

  for (i in seq_along(beta)) {
    pscores_applied[i, ] <- pscores(
      prep(x)$outcomes,
      prep(x)$var.outcomes,
      1,
      -beta[i],
      type,
      prep(x)$comm
    )
  }

  df <- data.frame(pscore = as.numeric(pscores_applied[1, ]), interv = prep(x)$comm)
  df_sort <- df[order(df$pscore), ]
  top_n <- min(top_n, nrow(df_sort))
  best <- tail(df_sort, n = top_n)$interv

  pscores_applied_1 <- pscores_applied[, best, drop = FALSE]
  pscores_applied_2 <- pscores_applied[, setdiff(colnames(pscores_applied), best), drop = FALSE]

  df_long <- data.frame(
    beta   = rep(beta, times = length(prep(x)$comm)),
    pscore = as.vector(pscores_applied),
    interv = rep(prep(x)$comm, each = length(beta))
  )
  df_long$is_best <- df_long$interv %in% best

  df_mean <- aggregate(pscore ~ beta, data = subset(df_long, is_best), FUN = mean)

  colr_b <- c("red","green3","blue","orange","purple",
              "brown","cyan3","magenta","gold3","dodgerblue3")
  colr_b <- rep(colr_b, length.out = length(best))
  names(colr_b) <- best

  col_map <- setNames(rep("black", length(prep(x)$comm)), prep(x)$comm)
  col_map[best] <- colr_b

  gg <- ggplot(df_long, aes(x = beta, y = pscore, group = interv, color = interv)) +
    geom_line(data = subset(df_long, !is_best), linewidth = 0.5, alpha = 0.65) +
    geom_line(data = subset(df_long,  is_best), linewidth = 1.0) +
    geom_point(data = subset(df_long, is_best), size = 1.6) +
    geom_line(
      data = df_mean,
      aes(x = beta, y = pscore, group = 1),
      inherit.aes = FALSE,
      linewidth = 1,
      linetype = "dashed"
    ) +
    scale_color_manual(values = col_map, breaks = best) +
    coord_cartesian(xlim = c(min(beta), max(beta)), ylim = c(0, 1)) +

    scale_y_continuous(breaks = seq(0, 1, by = 0.1), minor_breaks = seq(0, 1, by = 0.05)) +
    scale_x_continuous(breaks = pretty(beta, 10), minor_breaks = pretty(beta, 20)) +

    labs(
      x = "CIV",
      y = "P-score",
      color = paste0(length(best)," Top ranked Interventions"),
      title = "P-scores across CIV values",
      subtitle = paste0(
        "Colored lines: Top ", length(best),
        " interventions. Dashed line: mean P-score."
      )
    ) +
    theme_minimal() +
    theme(
      # make sure EVERYTHING is grey (panel + outer + legend keys)
      panel.background = element_rect(fill = "grey90", colour = NA),
      plot.background  = element_rect(fill = "grey95", colour = NA),

      legend.background = element_rect(fill = "grey95", colour = NA),
      legend.key        = element_rect(fill = "grey95", colour = NA),

      panel.border = element_rect(colour = "grey80", fill = NA, linewidth = 0.4),

      panel.grid.major = element_line(linewidth = 0.35, colour = "grey80"),
      panel.grid.minor = element_line(linewidth = 0.25, colour = "grey80"),

      text          = element_text(colour = "grey30"),
      plot.title    = element_text(colour = "grey20", face = "bold"),
      plot.subtitle = element_text(colour = "grey35"),
      axis.text     = element_text(colour = "grey35"),
      legend.title  = element_text(colour = "grey30"),
      legend.text   = element_text(colour = "grey35")
    )
  print(gg)

  invisible(list(
    pscores_applied   = pscores_applied,
    best              = best,
    pscores_applied_1 = pscores_applied_1,
    pscores_applied_2 = pscores_applied_2,
    plot              = gg
  ))
}
#pscore_graph_t(x=schiz,beta=seq(0,1,0.05),type="H",top_n = 7)
