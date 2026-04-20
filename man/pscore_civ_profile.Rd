\name{pscore_civ_profile}
\alias{pscore_civ_profile}
\title{Plot P-scores across a range of CIV values}
\description{
Computes and visualises P-scores for all treatments across a sequence of
clinically important values (CIVs) for a single outcome. The top-ranked
treatments are highlighted with distinct colours, and their mean P-score
is shown as a dashed line.
}
\usage{
pscore_civ_profile(x, beta, type, top_n = 5)
}
\arguments{
  \item{x}{A list containing a single \code{netmeta} object, as returned by
  \code{netmeta()} from the \pkg{netmeta} package, representing the results
  of a network meta-analysis for one outcome.}

  \item{beta}{A numeric vector defining the range of CIV values to explore.
  For example, \code{seq(0, 2, by = 0.05)} spans the range 0 to 2 in steps
  of 0.05.}

  \item{type}{A character string specifying the direction of the outcome.
  Use \code{"H"} for harmful outcomes and \code{"B"} for beneficial outcomes.}

  \item{top_n}{A positive integer specifying the number of top-ranked
  treatments to highlight. Default is \code{5}. If fewer treatments are
  available, all treatments are highlighted.}
}
\value{
A \code{ggplot2} plot object that can be further modified or saved with
\code{ggsave()}.
}
\details{
P-scores are computed for each value in \code{beta} using \code{pscores()}.
The top \code{top_n} treatments are identified based on their P-score at the
first value of \code{beta} and highlighted with distinct colours. All other
treatments are shown as thin grey lines. A dashed line represents the mean
P-score of the top \code{top_n} treatments across all CIV values.

The function prints the plot automatically. The returned plot object can be
captured for further use, such as saving or further modification.
}
\examples{
\dontrun{
# Plot P-scores across CIV values 0 to 1 for a harmful outcome,
# highlighting the top 7 treatments
out <- pscore_civ_profile(
  x = list(schiz),
  beta = seq(0, 1, by = 0.05),
  type = "H",
  top_n = 7
)
}
}
\seealso{
\code{\link{pscore_civs}}, \code{\link{pscores_compare}}
}
