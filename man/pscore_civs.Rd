\name{pscore_civs}
\alias{pscore_civs}
\title{Compute extended P-scores across CIV combinations}
\description{
Calculates extended P-scores for each treatment of the network across combinations of
clinically important values (CIV) for a single or multiple outcomes. For each CIV
combination and for treatment, ranking and Precision of Treatment Hierarchy (POTH)
are computed. Optionally, results can be exported to an Excel file.
}
\usage{
pscore_civs(x, CIVs, correlation, type, excel = FALSE)
}
\arguments{
  \item{x}{List of NMA results as \code{netmeta} objects.}
  \item{CIVs}{A  list of numeric vectors, one per outcome, specifying the
clinically important value(s) (CIVs) to explore for each outcome. The length of the
list must equal the number of outcomes. Each element contains one or more
numeric values representing the CIVs considered for that outcome.

For example, for 3 outcomes:
\preformatted{
CIVs <- list(
  outcome1 = c(0.2, 0.5),   # two CIV values tested for outcome 1
  outcome2 = 0.1,           # one CIV value tested for outcome 2
  outcome3 = 0.7            # one CIV value tested for outcome 3)
}
This produces \code{2 x 1 x 1 = 2} CIV combinations to evaluate.
}
  \item{correlation}{A square numeric correlation matrix of dimension \code{k x k},
where \code{k} is the number of outcomes. Diagonal elements should equal 1.
Off-diagonal elements represent pairwise correlations between outcomes.
Use \code{diag(k)} to assume independence.}
  \item{type}{A character vector of length equal to the number of outcomes,
specifying whether each outcome is harmful or beneficial. Accepted values are
\code{"H"} (harmful) and \code{"B"} (beneficial). For example,
\code{c("H", "B", "H")} indicates that the first and third outcomes are harmful
and the second is beneficial.}
  \item{excel}{Logical. If \code{TRUE}, results are exported to an Excel file with
  an auto-generated timestamped filename (e.g.,
  \code{"pscore_civs_20260227_152100.xlsx"}) in the working directory.
  Default is \code{FALSE}.}
}

\value{
An object of class \code{"pscore_civs"} is a named list with the
following elements:

\describe{

  \item{all}{A tibble in long format with one row per treatment per CIV combination,
containing the P-score (\code{Pscore}), rank (\code{ranking}),
Precision of Treatment Hierarchy (\code{poth}), P-scores residuals (\code{residuals}) and the corresponding CIV values.}
  \item{CIVs}{A data frame of all CIV combinations evaluated.}
  \item{Treatment}{Treatment label}
  \item{Pscore}{raw P-score for the row treatment under the CIV combination.}
  \item{ranking}{Rank of corresponding treatment (1 = highest, descending)}
  \item{residuals}{A character vector of residual P-scores.}
  \item{poth}{Precision of Treatment Hierarchy (PoTH) value}
}


}


\details{
The function calculates P-scores across all combinations of CIV values.
}

\examples{
\dontrun{

#Efficacy
#Estimating P-scores for a CIV of 0.5.

#import data
data("efficacy")
out_eff <- pscore_civs(list(efficacy),CIVs = 0.5,correlation = NULL, type="H") #efficacy is considered harmful outcome
out_eff

#Weight Gain
#Estimating P-scores for a CIV of -0.5.

#import data
data("weight_gain")
out_wg<-pscore_civs(list(weight_gain),CIVs = -0.5,correlation = NULL, type="H") #weight gain is considered harmful outcome
out_wg

#Both outcomes
#Estimating P-scores for both outcomes, assuming a correlation between efficacy and weight gain of −0.5 and CIVs 0.5 for efficacy and -0.5 for weight gain.

#Correlation matrix between the two outcomes (the assumed correlation between efficacy and weight gain is −0.5)
cor_mat <- matrix(c(1, -0.5,
                   -0.5, 1),
                    nrow = 2,
                    byrow = TRUE)

out1 <- pscore_civs(
  list(efficacy, weight_gain),
  CIVs = list(0.5,-0.5),
  correlation = cor_mat,
  type = c("H", "H"))
out1

#Estimating P-scores for both outcomes, assuming a correlation between efficacy and weight gain of −0.5 and CIVs 0.5 for efficacy and -0.5 and -0.3 for weight gain.


out2 <- pscore_civs(
  list(efficacy, weight_gain),
  CIVs = list(0.5,c(-.3,-0.5)),
  correlation = cor_mat,
  type = c("H", "H"), excel=TRUE)  #save results in excel format

out2

}
}
