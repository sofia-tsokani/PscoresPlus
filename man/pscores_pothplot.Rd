\name{pscores_pothplot}
\alias{pscores_pothplot}
\title{Bubble plot of POTH values across CIV combinations}
\description{
Creates a bubble plot visualising Precision of Treatment Hierarchy (POTH) values
across a grid of clinically important values (CIVs) for exactly two outcomes.
Bubble size represents POTH magnitude, with the largest and smallest values
optionally highlighted in distinct colours. This reveals regions where treatment
rankings are most or least precise.
}
\usage{
pscores_pothplot(x, newgridsize = 0, highlight = TRUE, title = "POTH plot",
                 outcome1 = NULL, outcome2 = NULL)
}
\arguments{
  \item{x}{An object of class \code{"pscore_civs"} as returned by
  \code{pscore_civs()}, containing POTH values across CIV combinations.}

  \item{newgridsize}{Grid size for downsampling. \code{0} (default) uses the
  original grid from \code{x}. Positive integer \code{n} creates an \code{n} x
  \code{n} quantile grid selecting points closest to the quantiles.}

  \item{highlight}{Logical; if \code{TRUE} (default), highlights maximum
  (light blue) and minimum (hot pink) POTH values with distinct colours.}

  \item{title}{An optional character string specifying the plot title.
  Defaults to \code{"POTH plot"}.}

  \item{outcome1}{An optional character string used as the x-axis label for
  the first outcome. Defaults to \code{"Outcome 1"}.}

  \item{outcome2}{An optional character string used as the y-axis label for
  the second outcome. Defaults to \code{"Outcome 2"}.}
}
\value{
A list containing:
\itemize{
  \item \code{plot}: \code{ggplot2} object (bubble plot)
  \item \code{grid}: tibble of displayed CIV combinations and their POTH values
}
}
\details{
The function requires exactly two CIV dimensions (columns in \code{x$CIVs}).
Optionally downsamples via quantiles for cleaner visualisation. Bubble size
scales with POTH (\code{range = c(1, 20)}).
}
\examples{
\dontrun{
# Efficacy and Weight Gain outcomes
dataset("efficacy")
dataset("weight_gain")

# Correlation matrix (assumed correlation = -0.5)
cor_mat <- matrix(c(1, -0.5, -0.5, 1), nrow = 2, byrow = TRUE)

# Run pscore_civs with 50x50 grid
out <- pscore_civs(
  list(efficacy, weight_gain),
  CIVs = list(seq(0, 0.5, length.out = 50),
              seq(-0.5, 0, length.out = 50)),
  correlation = cor_mat,
  type = c("H", "H")
)

# Full grid POTH plot with highlighting
pscores_pothplot(out)

# Change gridsize
pscores_pothplot(out, newgridsize = 10)

# Custom labels
pscores_pothplot(out, newgridsize = 10, title = "POTH Plot",
                 outcome1 = "Efficacy", outcome2 = "Weight Gain")

# Save plot
g <- pscores_pothplot(out)
ggplot2::ggsave("pothplot.png", g$plot, width = 8, height = 6)
}
}
\seealso{
\code{\link{pscore_civs}}, \code{\link{pscores_heatplot}}
}
