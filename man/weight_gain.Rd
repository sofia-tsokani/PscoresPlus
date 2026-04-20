\name{weight_gain}
\alias{weight_gain}
\title{Weight Gain Dataset}
\description{
Network of 212 randomized controlled trials (RCTs) with 43,049 participants
comparing 15 antipsychotic drugs and placebo, focusing on weight gain.
Weight gain is treated as a harmful outcome, so treatments with smaller increases in weight are ranked more favorably.
(Leucht et al., 2013).
}
\usage{
data("weight_gain")
}
\format{
A \code{netmeta} object including treatment effects (\code{TE}), standard errors
(\code{seTE}), treatment comparisons, and study labels.
}
\details{
The dataset contains the weight gain outcome, measured as a continuous variable.
Treatment effects are expressed as standardized mean differences (SMD).

\emph{Reference:} Leucht S, Cipriani A, Spineli L, Mavridis D, Orey D, Richter F, Samara M,
Barbui C, Engel RR, Geddes JR, Kissling W, Stapf MP, Lässig B, Salanti G, Davis JM.
\emph{Comparative efficacy and tolerability of 15 antipsychotic drugs in schizophrenia:
a multiple-treatments meta-analysis}. Lancet. 2013;382(9896):951–962.
doi:10.1016/S0140-6736(13)60733-3.
}
\examples{
\dontrun{
# import dataset
data("weight_gain")
}
}
