\name{pscores_heatplot}
\alias{pscores_heatplot}
\title{Heat plot of treatment rankings across CIV combinations}
\description{
Visualises treatment rankings across a grid of clinically important values
(CIV) for exactly two outcomes as a heat plot. Each facet represents one
treatment, with the two CIV axes on the x and y axes and the rank based on
the extended P-score shown as a colour gradient. This provides granular
information about the specific regions of CIV combinations where a treatment
performs better or worse than others, complementing the AUPC summary.
}
\usage{
pscores_heatplot(x, title = "", outcome1 = "Outcome 1", outcome2 = "Outcome 2")
}
\arguments{
  \item{x}{An object of class \code{"pscore_civs"} as returned by
  \code{pscore_civs()}, containing the extended P-scores and rankings
  across all CIV combinations.}
  \item{title}{An optional character string specifying the plot title.
  Defaults to \code{""} (no title).}
   \item{outcome1}{An optional character string used as the x-axis label for
  the first outcome.Defaults to \code{"Outcome 1"}.}

  \item{outcome2}{An optional character string used as the y-axis label for
  the second outcome.Defaults to \code{"Outcome 2"}.}
}
\value{
A \code{ggplot2} object. The plot is not printed automatically and can be
displayed by calling the returned object or saved with
\code{ggplot2::ggsave()}.
}
\details{
The function requires exactly two outcomes. Treatments are ordered in the facets by their
mean P-score across all CIV combinations, from highest to lowest.
}

\examples{
\dontrun{

# Efficacy and Weight Gain outcomes
#Import data
dataset("efficacy")
datase("weight_gain")

#correlation matrix between the two outcomes (the assumed correlation between efficacy and weight gain is −0.5)
cor_mat <- matrix(c(1, -0.5,
                   -0.5, 1),
                    nrow = 2,
                    byrow = TRUE)

# Run pscores_civs with a grid of 50 values between 0 and 0.5 for efficacy and -0.5 and 0 for weight gain.

out <- pscore_civs(
  list(efficacy, weight_gain),
  CIVs = list(seq(0, 0.5, length.out = 50),
              seq(-0.5, 0, length.out = 50)),
  correlation = cor_mat,
  type = c("H", "H")
)

# Heat plot of rankings
pscores_heatplot(out, title = "Efficacy & Weight Gain Heatplot", outcome1="Efficacy", outcome2="Weight Gain")

}
}
\seealso{
\code{\link{pscore_civs}}, \code{\link{pscores}}, \code{\link{pscore_civ_profile}}
}
