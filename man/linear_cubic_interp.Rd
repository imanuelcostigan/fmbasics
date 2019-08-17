% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{linear_cubic_interp}
\alias{linear_cubic_interp}
\title{Linear-Cubic Interpolation}
\usage{
linear_cubic_interp(interp_data, x0, y0)
}
\arguments{
\item{interp_data}{\code{data.frame} object with three vectors x, y and z}

\item{x0}{numeric vector containing the points at which to perform the interpolation along the first dimension}

\item{y0}{numeric vector containing the points at which to perform the interpolation along the second dimension}
}
\description{
This function performs a two-dimentional interpolation linear on the first dimension
and natural cubic spline on the second dimension.
}
\examples{
vols <- build_vol_quotes()
 interp_data <- tibble::tibble(x = fmdates::year_frac(vols$reference_date,
as.Date(vols$maturity,"\%Y\%m\%d"), "act/365"),
y = vols$smile,
z = vols$value)
x0 <- c(2.5, 3.5)
y0 <- c(22, 55)
linear_cubic_interp(interp_data = interp_data, x0 = x0, y0 = y0)
}