\name{plot_aupc_pscore}
\alias{plot_aupc_pscore}
\title{Compare AUPC and P-score treatment rankings}
\description{
Creates a scatter plot comparing treatment rankings from AUPC and P-score analyses.
Only treatments common to both inputs are shown.
}
\usage{
plot_aupc_pscore(aupc, pscore, outcome_name = "")
}
\arguments{
  \item{aupc}{A \code{aupc()} object. }

  \item{pscore}{A \code{pscore_civs} object.}

  \item{outcome_name}{Optional title for the plot to display the outcome name. Defaulted at no title.}
}
\value{
A \code{ggplot2} object showing AUPC ranks on the x-axis and P-score ranks on the y-axis.
}
\details{
The plot depicts the treatment rankings according to AUPC and P-scores, draws the identity line \code{y = x},
and labels each treatment directly on the plot. In brief, it is a scatterplot of the treatment ranks using two different methods of creating the hierarchy.
}
\examples{
\dontrun{
#Import dataset for efficacy
data("efficacy")

#Calculate treatment ranks for efficacy outcome using the extended P-scores with a CIV of 0.30.
eff_rank <- pscore_civs(list(efficacy), CIVs = 0.30, correlation = NULL, type = "H")

#Calculate AUPC for efficacy with CIV ranges from 0.20 to 0.40.
pscore_range <- pscore_civs(list(efficacy), CIVs = list(seq(0.2,0.4,0.1)), correlation = NULL, type = "H")
aupc_res<-aupc(pscore_range)

plot_aupc_pscore(aupc_res,eff_rank, outcome_name = "Efficacy")
}
}
\seealso{
\code{\link{aupc}}, \code{\link{pscore_civs}}
}
