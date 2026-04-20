\name{pscores_compare}
\alias{pscores_compare}
\title{Compare treatment rankings across two P-score analyses}
\description{
Compares treatment rankings from two P-score analyses (e.g., two outcomes
or the same outcome with different CIVs) by plotting a scatter of ranks
for each treatment. Treatments are labelled on the plot, and the identity
line \code{y = x} is added to visualise agreement or disagreement in rankings.
}
\usage{
pscores_compare(x1, x2, outcome1 = "Outcome 1", outcome2 = "Outcome 2")
}
\arguments{
  \item{x1}{A P-score object, as returned by
  \code{pscore_civs()}.}

  \item{x2}{A second P-score object with the same structure as \code{x1}.}

  \item{outcome1}{Character; label for the first outcome (x-axis).
  Defaults to \code{"Outcome 1"}.}

  \item{outcome2}{Character; label for the second outcome (y-axis).
  Defaults to \code{"Outcome 2"}.}
}
\value{
A scatter plot for comparing the rankings from the two inputs,
where rankings for outcome 1 are plotted on the x‑axis and rankings for outcome 2 on the y‑axis.}
\details{
The function creates a scatter plot of the rankings for the common treatments across the two outcomes,
using the identity line \code{y = x} and treatment labels.
}
\examples{
\dontrun{
# Example: efficacy vs weight gain
data("efficacy")
data("weight_gain")

# Run pscore_civs for each outcome
out_eff <- pscore_civs(
  list(efficacy),
  CIVs = 0.5,
  type = "H")

out_wg <- pscore_civs(
  list(weight_gain),
  CIVs = -0.5,
  type = "H"
)

# Compare rankings
pscores_compare(out_eff, out_wg, outcome1 = "Efficacy", outcome2 = "Weight gain")

}
}
\seealso{
\code{\link{pscore_civs}}, \code{\link{pscores_pothplot}}
}
