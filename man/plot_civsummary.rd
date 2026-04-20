\name{plot_civsummary}
\alias{plot_civsummary}
\title{Plot summary statistics over CIVs}
\description{
Creates a bar plot of a summary measure returned by \code{aupc()}.
Treatments can be ordered by AUPC value or kept in the original order.
}
\usage{
plot_civsummary(obj, ordering = "ranking", title = "")
}
\arguments{
  \item{obj}{A data frame returned by \code{aupc()} or \code{abpmc()}, containing
  treatment labels and a summary measure in the first column.}

  \item{ordering}{Character string controlling the order of treatments on the x-axis.
  Use \code{"ranking"} to order treatments by \code{AUPC} or \code{"asis"} to keep the original
  order.}

  \item{title}{Optional title for the plot. Default is \code{NULL}.}
}
\value{
A \code{ggplot2} bar plot object showing the summary AUPC by treatment.
}
\details{
If \code{ordering = "ranking"}, treatments are reordered according to the first
column of \code{obj}. The function uses the first column name as the plotted
measure.
}
\examples{
\dontrun{
# Import datasets (netmeta objects)
data("efficacy")
data("weight_gain")

# Efficacy
out_eff <- pscore_civs(
  list(efficacy),
  CIVs = list(seq(0, 0.5, length.out = 50)),
  correlation = NULL,
  type = c("H")
)

obj <- aupc(out_eff)

plot_civsummary(obj, ordering = "asis", title = "CIV Profile for Efficacy Outcome")
plot_civsummary(obj, title = "CIV Profile for Efficacy Outcome")
}
}
\seealso{
\code{\link{aupc}}}
