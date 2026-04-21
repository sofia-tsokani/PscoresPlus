#' Weight Gain Dataset
#'
#' @description
#' Network of 86 randomized controlled trials (RCTs)
#' comparing 15 antipsychotic drugs and placebo, focusing on weight gain.
#' Weight gain is treated as a harmful outcome; treatments with smaller
#' increases in weight are ranked more favorably (Leucht et al., 2013).
#'
#' @format A \code{netmeta} object including treatment effects (\code{TE}),
#'   standard errors (\code{seTE}), treatment comparisons, and study labels.
#'   Treatment effects are expressed as standardized mean differences (SMD).
#'
#' @details
#' \emph{Reference:} Leucht S, et al. Comparative efficacy and tolerability
#' of 15 antipsychotic drugs in schizophrenia: a multiple-treatments
#' meta-analysis. \emph{Lancet}. 2013;382(9896):951–962.
#' doi:10.1016/S0140-6736(13)60733-3.
#'
#' @examples
#' \dontrun{
#' data("weight_gain")
#' }
"weight_gain"


#' Efficacy Dataset
#'
#' @description
#' Network of 167 RCTs comparing 15 antipsychotic treatments and placebo
#' on treatment efficacy, measured as overall change in symptoms across
#' several validated scales. Coded as a harmful outcome: a greater reduction
#' in symptoms reflects better treatment performance (Leucht et al., 2013).
#'
#' @format A \code{netmeta} object including treatment effects (\code{TE}),
#'   standard errors (\code{seTE}), treatment comparisons, and study labels.
#'   Treatment effects are expressed as standardized mean differences (SMD).
#'
#' @details
#' \emph{Reference:} Leucht S, et al. Comparative efficacy and tolerability
#' of 15 antipsychotic drugs in schizophrenia: a multiple-treatments
#' meta-analysis. \emph{Lancet}. 2013;382(9896):951–962.
#' doi:10.1016/S0140-6736(13)60733-3.
#'
#' @examples
#' \dontrun{
#' data("efficacy")
#' }
"efficacy"


efficacy <- readRDS("data/Efficacy.rds")
weight_gain <- readRDS("data/Weight Gain.rds")

# Save them in package‑compatible format
save(efficacy, file = "data/efficacy.rda")
save(weight_gain, file = "data/weight_gain.rda")
