% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/credit-class.R
\name{ZeroHazardRate}
\alias{ZeroHazardRate}
\title{Builds a \code{ZeroHazardRate}}
\usage{
ZeroHazardRate(values, compounding, day_basis, specs)
}
\arguments{
\item{values}{a numeric vector containing zero hazard rate values (as decimals).}

\item{compounding}{a numeric vector representing the \link{compounding} frequency.}

\item{day_basis}{a character vector representing the day basis associated
with the interest rate and hazard rate(see \code{\link[fmdates:year_frac]{fmdates::year_frac()}})}

\item{specs}{CDS curve specifications that inherits from \code{\link[=CDSSpec]{CDSSpec()}}}
}
\value{
returns an object of type \code{hazard_rates}
}
\description{
This will allow you to create a harzard rate curve. This will typically be
bootstrapped or implied from a \code{\link[=CDSCurve]{CDSCurve()}} or \code{\link[=SurvivalProbabilities]{SurvivalProbabilities()}}.
}
\examples{
curve_specs <- CDSMarkitSpec(
  rating = "AAA",
  region = "Japan",
  sector = "Utilities"
)
ZeroHazardRate(values = c(0.04, 0.05), compounding = c(2, 4),
day_basis =  'act/365', specs = curve_specs )

}
\seealso{
Other CDS curve helpers: \code{\link{CDSCurve}},
  \code{\link{CDSMarkitSpec}},
  \code{\link{CDSSingleNameSpec}}, \code{\link{CDSSpec}},
  \code{\link{SurvivalProbabilities}},
  \code{\link{is.CDSCurve}}, \code{\link{is.CDSSpec}}
}
\concept{CDS curve helpers}
