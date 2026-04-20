\name{efficacy}
\alias{efficacy}
\title{Efficacy Dataset for Network Meta-Analysis}
\description{
Network of 212 randomized controlled trials (RCTs) with 43,049 participants
comparing 15 antipsychotic drugs and placebo, focusing on treatment efficacy
(Leucht et al., 2013). Efficacy is considered a harmful outcome as it is measured through the symptoms using various scales.
A greater reduction in symptoms indicates a better intervention.
}
\usage{
data("efficacy")
}
\format{
A \code{netmeta} object including treatment effects (\code{TE}), standard errors
(\code{seTE}), treatment comparisons, and study labels.
}
\details{
The dataset contains the primary efficacy outcome, defined as the overall change
in symptoms on a validated scale. Treatment effects are expressed as standardized
mean differences (SMD).

\code{Reference: Leucht S, Cipriani A, Spineli L, Mavridis D, Orey D, Richter F, Samara M,
Barbui C, Engel RR, Geddes JR, Kissling W, Stapf MP, Lässig B, Salanti G, Davis JM.
Comparative efficacy and tolerability of 15 antipsychotic drugs in schizophrenia:
a multiple-treatments meta-analysis. Lancet. 2013;382(9896):951–962.
doi:10.1016/S0140-6736(13)60733-3.}
}
\examples{
\dontrun{
#import dataset
data("efficacy")
}
}
