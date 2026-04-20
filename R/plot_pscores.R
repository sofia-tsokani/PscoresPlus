#' Plotting for a single outcome
#' @name plot_pscores
#' @title Plot Pscores
#' @description Creates a graph of Pscores or residual Pscores
#' @param x object from running pscore_civs. only accommodates objects where
#' only one outcome has varying CIV
#' @param CIVs numeric vector of CIV values
#' @param type "B" or "H" (outcome type)
#' @param top_n number of top interventions to highlight (default: 5)
#' @param residuals Logical - should P-score residuals or raw p-scores be plotted?
#' Default is TRUE
#'
#' @return A `ggplot2` object.
#' @export
plot_pscores <- function(x, CIVs, type, top_n = 5, residuals = TRUE) {

  # ---- checks that should stop execution ----
  if (missing(x) || is.null(x)) {
    stop("`x` must be provided; x should be a list of a single netmeta object.", call. = FALSE)
  }

  if (is.list(x) && length(x) > 1) {
    stop("This function is meant for single-outcome P-scores.
         Please provide x as a list containing one netmeta object.", call. = FALSE)
  }

  if (missing(CIVs) || length(CIVs) == 0) {
    stop("`CIVs` must be a numeric vector with at least one value.", call. = FALSE)
  }

  if (!is.numeric(CIVs) || any(!is.finite(CIVs))) {
    stop("`CIVs` must be a numeric vector of finite values.", call. = FALSE)
  }

  if (missing(type) || length(type) != 1 || !is.character(type)) {
    stop("`type` must be a single character string 'B' or 'H'.", call. = FALSE)
  }

  if (length(top_n) != 1 || !is.numeric(top_n) || !is.finite(top_n) || top_n < 1) {
    stop("`top_n` must be a single positive number.", call. = FALSE)
  }

  top_n <- as.integer(top_n)

  px <- prep(x)

  required_names <- c("comm", "outcomes", "var.outcomes")
  missing_names <- setdiff(required_names, names(px))
  if (length(missing_names) > 0) {
    stop(
      paste0("`prep(x)` must contain: ", paste(required_names, collapse = ", "),
             ". Missing: ", paste(missing_names, collapse = ", "), "."),
      call. = FALSE
    )
  }

  # ---- warnings ----
  if (is.unsorted(CIVs, strictly = FALSE)) {
    warning(
      "`CIVs` is not sorted in increasing order and values have been reordered.",
      call. = FALSE
    )
  }

  n_trt <- length(px$comm)
  if (top_n > n_trt) {
    warning(
      paste0("`top_n` (", top_n, ") is larger than the number of interventions (", n_trt,
             "). Highlighting all interventions, i.e. ", n_trt, " in total."),
      call. = FALSE
    )
    top_n <- n_trt
  }

  # ---- compute p-scores ----
  pscores_applied <- matrix(
    0,
    nrow = length(CIVs),
    ncol = n_trt
  )
  colnames(pscores_applied) <- px$comm

  for (i in seq_along(CIVs)) {
    pscores_applied[i, ] <- pscores(
      px$outcomes,
      px$var.outcomes,
      1,
      -CIVs[i],
      type,
      px$comm
    )
  }

  # ---- residuals if requested ----
  if (residuals) {
    row_means <- rowMeans(pscores_applied)
    pscores_applied <- pscores_applied - row_means
  }

  # ---- identify best interventions at first CIVs ----
  df <- data.frame(
    pscore = as.numeric(pscores_applied[1, ]),
    interv = px$comm,
    stringsAsFactors = FALSE
  )

  df_sort <- df[order(df$pscore), ,drop = FALSE]
  best <- tail(df_sort, n = top_n)$interv

  pscores_applied_1 <- pscores_applied[, best, drop = FALSE]
  pscores_applied_2 <- pscores_applied[, setdiff(colnames(pscores_applied), best), drop = FALSE]

  # ---- long data for ggplot ----
  df_long <- data.frame(
    CIVs   = rep(CIVs, times = n_trt),
    pscore = as.vector(pscores_applied),
    interv = rep(px$comm, each = length(CIVs)),
    stringsAsFactors = FALSE
  )
  df_long$is_best <- df_long$interv %in% best

  # ---- mean line ----
  if (residuals) {
    df_mean <- aggregate(pscore ~ CIVs, data = df_long, FUN = mean)
  } else {
    df_mean <- aggregate(pscore ~ CIVs, data = subset(df_long, is_best), FUN = mean)
  }

  # ---- colors ----
  colr_b <- c(
    "red", "green3", "blue", "orange", "purple",
    "brown", "cyan3", "magenta", "gold3", "dodgerblue3"
  )
  colr_b <- rep(colr_b, length.out = length(best))
  names(colr_b) <- best

  col_map <- setNames(rep("black", n_trt), px$comm)
  col_map[best] <- colr_b

  # ---- y scale ----
  if (residuals) {
    y_limits <- range(df_long$pscore, na.rm = TRUE)
    if (diff(y_limits) == 0) {
      y_limits <- y_limits + c(-0.05, 0.05)
    }
    y_breaks_major <- pretty(y_limits, 10)
    y_breaks_minor <- pretty(y_limits, 20)
    y_label <- "Residual P-score"
    plot_title <- "Residual P-scores across CIV values"
    plot_subtitle <- paste0(
      "Colored lines: Top ", length(best),
      " interventions at the first CIV ( " , CIVs[1] ," ). Dashed line: Residual Average (approximately 0)."
    )
  } else {
    y_limits <- c(0, 1)
    y_breaks_major <- seq(0, 1, by = 0.1)
    y_breaks_minor <- seq(0, 1, by = 0.05)
    y_label <- "P-score"
    plot_title <- "P-scores across CIV values"
    plot_subtitle <- paste0(
      "Colored lines: Top ", length(best),
      " interventions at the first CIV ( " , CIVs[1] ," ). Dashed line: average P-score of highlighted interventions."
    )
  }

  suppressPackageStartupMessages(library(ggplot2))

  gg <- ggplot(df_long, aes(x = CIVs, y = pscore, group = interv, color = interv)) +
    geom_line(data = subset(df_long, !is_best), linewidth = 0.5, alpha = 0.65) +
    geom_line(data = subset(df_long,  is_best), linewidth = 1.0) +
    geom_point(data = subset(df_long, is_best), size = 1.6) +
    geom_line(
      data = df_mean,
      aes(x = CIVs, y = pscore, group = 1),
      inherit.aes = FALSE,
      linewidth = 1,
      linetype = "dashed",
      color = "black"
    ) +
    scale_color_manual(values = col_map, breaks = best) +
    coord_cartesian(
      xlim = c(min(CIVs, na.rm = TRUE), max(CIVs, na.rm = TRUE)),
      ylim = y_limits
    ) +
    scale_y_continuous(
      breaks = y_breaks_major,
      minor_breaks = y_breaks_minor
    ) +
    scale_x_continuous(
      breaks = pretty(CIVs, 10),
      minor_breaks = pretty(CIVs, 20)
    ) +
    labs(
      x = "CIV",
      y = y_label,
      color = paste0(length(best), " Top ranked interventions at CIV = " , CIVs[1] ,"."),
      title = plot_title,
      subtitle = plot_subtitle
    ) +
    theme_minimal() +
    theme(
      panel.background  = element_rect(fill = "grey90", colour = NA),
      plot.background   = element_rect(fill = "grey95", colour = NA),
      legend.background = element_rect(fill = "grey95", colour = NA),
      legend.key        = element_rect(fill = "grey95", colour = NA),
      panel.border      = element_rect(colour = "grey80", fill = NA, linewidth = 0.4),
      panel.grid.major  = element_line(linewidth = 0.35, colour = "grey80"),
      panel.grid.minor.y = element_line(linewidth = 0.25, colour = "grey80"),
      panel.grid.minor.x = element_blank(),
      text              = element_text(colour = "grey30"),
      plot.title        = element_text(colour = "grey20", face = "bold"),
      plot.subtitle     = element_text(colour = "grey35"),
      axis.text         = element_text(colour = "grey35"),
      legend.title      = element_text(colour = "grey30"),
      legend.text       = element_text(colour = "grey35")
    )

  print(gg)

# invisible(list(
# pscores_applied   = pscores_applied,
# best              = best,
# pscores_applied_1 = pscores_applied_1,
# pscores_applied_2 = pscores_applied_2,
# plot              = gg
#  ))
}
