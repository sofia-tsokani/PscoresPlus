\name{plot_pscores}
\alias{plot_pscores}
\title{Plot P-scores or residual P-scores across CIV values for a single outcome}
\description{
The graph created plots the extended P-scores or residual P-scores across a range of clinically
important values (CIV) for a single outcome. The top-ranked interventions are highlighted with distinct colours.
A dashed reference line is added to aid interpretation; for extended P-scores it corresponds to average
P-score while for residual P-scores it is approximately zero.
}
\usage{
plot_pscores_n(x, CIVs, type, top_n = 5, residuals = TRUE)
}
\arguments{
  \item{x}{A list including a \code{netmeta} object that corresponds to single outcome only. If a list of
  multiple outcomes is supplied, the function stops with an error.}

  \item{CIVs}{A numeric vector of CIV values at which P-scores are evaluated.}

  \item{type}{A single character string specifying the P-score type, either
  \code{"B"} or \code{"H"}.}

  \item{top_n}{A positive integer indicating the number of top-ranked
  interventions to highlight. Defaults to \code{5}.}

  \item{residuals}{Logical. If \code{TRUE}, residual P-scores are plotted by
  subtracting the row mean across interventions at each CIV value. If
  \code{FALSE}, raw P-scores are plotted. Defaults to \code{FALSE}.}

}
\value{
Produces a plot where the extended P-scores of the residual P-scores are depicted across a list of CIVs.
}
\details{
This function is intended for single-outcome objects only.

When \code{residuals = TRUE}, each treatment's P-score is centred by
subtracting the row mean across all treatments at each CIV value. This
highlights relative rather than absolute performance, and the dashed mean
line is approximately zero.

When \code{residuals = FALSE}, raw P-scores are plotted. In both cases, the
top \code{top_n} interventions are identified at the first CIV value and
highlighted in colour, while the remaining interventions are shown in black.
}
\examples{
\dontrun{
# Import datasets (netmeta objects)

data("efficacy")
data("weight_gain")

#Extended P-scores Plot for efficacy outcome

plot_pscores(
      list(efficacy),
      CIVs = (seq(0,0.5,0.1)),
      type = c("H"), #efficacy is considered harmful outcome
      top_n = 5,
      residuals = FALSE) #P-scores plotted

#Residual P-scores Plot for efficacy outcome

plot_pscores(
      list(efficacy),
      CIVs = (seq(0,0.5,0.1)),
      type = c("H"), #efficacy is considered harmful outcome
      top_n = 5,
      residuals = TRUE) #Residuals plotted
)
}
}
