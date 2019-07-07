#' Inherits from CDSSpec
#'
#' Checks whether object inherits from `CDSSpec` class
#'
#' @param x an R object
#' @return `TRUE` if `x` inherits from the `CDSSpec` class; otherwise `FALSE`
#' @export
#' @examples
#' curve_specs <- CDSMarkitSpec(
#'   rating = "AAA",
#'   region = "Japan",
#'   sector = "Utilities"
#' )
#' is.CDSSpec(curve_specs)
#' @family CDS curve helpers
#' @export
is.CDSSpec <- function(x) inherits(x, "CDSSpec")

#' Inherits from CDSCurve
#'
#' Checks whether object inherits from `CDSCurve` class
#'
#' @param x an R object
#' @return `TRUE` if `x` inherits from the `CDSCurve` class; otherwise `FALSE`
#' @export
#' @examples
#' curve_specs <- CDSMarkitSpec(
#'   rating = "AAA",
#'   region = "Japan",
#'   sector = "Utilities"
#' )
#' cds_curve <- CDSCurve(
#'   as.Date("2019-06-29"),
#'   tenors = c(1, 3, 5, 7),
#'   spreads = c(0.0050, 0.0070, 0.0090, 0.0110),
#'   lgd = 0.6,
#'   premium_frequency = 4,
#'   specs = curve_specs
#' )
#' is.CDSCurve(cds_curve)
#' @family CDS curve helpers
is.CDSCurve <- function(x) inherits(x, "CDSCurve")

bootstrap_survprob <- function(cds_curve, zero_curve, ...) UseMethod("bootstrap_survprob")

#' Bootstraps Survival Probabilitie from a CDS curve
#' Using \href{https://www.rdocumentation.org/packages/credule/versions/0.1.3}{credule package.}
#'
#' @param cds_curve An object of type `CDSCurve`
#' @param zero_curve An object of type `ZeroCurve`
#' @param num_timesteps_pa It represents the number of timesteps used to perform the numerical integral
#'  required while computing the default leg value. It is shown that a monthly discretisation
#'   usually gives a good approximation (Ref. Valuation of Credit Default Swaps, Dominic O Kane and
#'   Stuart Turnbull)
#' @param accrued_premium If set to TRUE, the accrued premium will be taken into account in the calculation of the premium leg value.
#'
#' @return An object of type `SurvivalCurve`
#' @examples
#' zero_curve <- build_zero_curve()
#' specs <- CDSMarkitSpec(rating = "AAA", region = "Japan", sector = "Utilities")
#' cds_curve <- CDSCurve(
#'   reference_date = zero_curve$reference_date,
#'   tenors = c(1, 3, 5, 7), spreads = c(0.0050, 0.0070, 0.0090, 0.0110), lgd = .6,
#'   premium_frequency = 4, specs = specs
#' )
#' bootstrap_survprob(cds_curve = cds_curve, zero_curve = zero_curve)
bootstrap_survprob.CDSCurve <- function(cds_curve,
                                        zero_curve,
                                        num_timesteps_pa = 12,
                                        accrued_premium = TRUE) {
  if (cds_curve$reference_date != zero_curve$reference_date) {
    stop("The reference dates for CDS Curve and the Zero Curve are different", call. = FALSE)
  }
  if (!is.ZeroCurve(zero_curve)) {
    stop("zero_curve must be an object of type ZeroCurve", call. = FALSE)
  }

  sp_output <- credule::bootstrapCDS(
    yieldcurveTenor = zero_curve$pillar_times,
    yieldcurveRate = zero_curve$pillar_zeros,
    cdsTenors = cds_curve$tenors,
    cdsSpreads = cds_curve$spread,
    recoveryRate = 1 - cds_curve$lgd,
    numberPremiumPerYear = cds_curve$premium_frequency,
    numberDefaultIntervalPerYear = num_timesteps_pa,
    accruedPremium = accrued_premium
  )

  SurvivalCurve(
    reference_date = cds_curve$reference_date,
    tenors = cds_curve$tenors,
    probabilities = sp_output$survprob,
    specs = cds_curve$specs
  )
}


#' Bootstraps Hazard Rate from a CDS curve
#' Using \href{https://www.rdocumentation.org/packages/credule/versions/0.1.3}{credule package.}
#'
#' @param cds_curve An object of type `CDSCurve`
#' @param zero_curve An object of type `ZeroCurve`
#' @param num_timesteps_pa It represents the number of timesteps used to perform the numerical integral
#'  required while computing the default leg value. It is shown that a monthly discretisation
#'   usually gives a good approximation (Ref. Valuation of Credit Default Swaps, Dominic O Kane and
#'   Stuart Turnbull)
#' @param accrued_premium If set to TRUE, the accrued premium will be taken into account in the calculation of the premium leg value.
#'
#' @return An object of type `HazardCurve`
#' @examples
#' zero_curve <- build_zero_curve()
#' specs <- CDSMarkitSpec(rating = "AAA", region = "Japan", sector = "Utilities")
#' cds_curve <- CDSCurve(
#'   reference_date = zero_curve$reference_date,
#'   tenors = c(1, 3, 5, 7), spreads = c(0.0050, 0.0070, 0.0090, 0.0110), lgd = .6,
#'   premium_frequency = 4, specs = specs
#' )
#' bootstrap_hazardrate(cds_curve = cds_curve, zero_curve = zero_curve)
bootstrap_hazardrate <- function(cds_curve, zero_curve, ...)
  UseMethod("bootstrap_hazardrate")

bootstrap_hazardrate.CDSCurve <- function(cds_curve,
                                          zero_curve,
                                          num_timesteps_pa = 12,
                                          accrued_premium = TRUE) {
  if (cds_curve$reference_date != zero_curve$reference_date) {
    stop("The reference dates for CDS Curve and the Zero Curve are different",
      call. = FALSE
    )
  }
  if (!is.ZeroCurve(zero_curve)) {
    stop("zero_curve must be an object of type ZeroCurve", call. = FALSE)
  }

  hr_output <- credule::bootstrapCDS(
    yieldcurveTenor = zero_curve$pillar_times,
    yieldcurveRate = zero_curve$pillar_zeros,
    cdsTenors = cds_curve$tenors,
    cdsSpreads = cds_curve$spread,
    recoveryRate = 1 - cds_curve$lgd,
    numberPremiumPerYear = cds_curve$premium_frequency,
    numberDefaultIntervalPerYear = num_timesteps_pa,
    accruedPremium = accrued_premium
  )

  HazardCurve(
    reference_date = cds_curve$reference_date,
    tenors = cds_curve$tenors,
    hazard_rates = hr_output$hazrate,
    specs = cds_curve$specs
  )
}

format.CDSSpec <- function(x,...){
  paste0("<Curve Specification> \n",
    "Rank: ", x$rank, "\n")
}

print.CDSSpec <- function(x, ...){
  cat(format(x, ...), "\n")
}

format.CDSMarkitSpec <- function(x,...){
  paste0("<Curve Specification: Markit CDS Sector Curve> \n",
    "Rank: SNR \n",
    "Rating: ", x$rating, "\n",
    "Region: " , x$region, "\n",
    "Sector: " , x$sector, "\n")
}

print.CDSMarkitSpec <- function(x, ...){
  cat(format(x, ...), "\n")
}

format.CDSSingleNameSpec <- function(x,...){
  paste0("<Curve Specification: Single Name CDS Curve> \n",
    "Rank: ", x$rank, "\n",
    "Name: ", x$name, "\n")
}

print.CDSSingleNameSpec <- function(x, ...){
  cat(format(x, ...), "\n")
}


format.CDSCurve <- function(x, ...){
  paste0(
    "<CDSCurve as of ", x$reference_date, "> \n",
    "Tenors: ", paste(x$tenors, collapse = " "), "\n",
    "Spreads: ", paste(x$spread, collapse = " "), "\n",
    "LGD: ", x$LGD, "\n",
    "Premium Frequency: ", x$premium_frequency, "\n",
    "--------------\n"
  )
}

print.CDSCurve <- function(x, ...){
  cat(format(x, ...), "\n")
  cat(format(x$specs,...), "\n")
}

format.SurvivalCurve <- function(x, ...){
  paste0(
    "<CDSCurve as of ", x$reference_date, "> \n",
    "Tenors: ", paste(x$tenors, collapse = " "), "\n",
    "Survival Probabilities: ", paste(x$probabilities, collapse = " "), "\n",
    "--------------\n"
  )
}

print.SurvivalCurve <- function(x, ...){
  cat(format(x, ...), "\n")
  cat(format(x$specs,...), "\n")
}

format.HazardCurve <- function(x, ...){
  paste0(
    "<CDSCurve as of ", x$reference_date, "> \n",
    "Tenors: ", paste(x$tenors, collapse = " "), "\n",
    "Survival Hazard Rate: ", paste(x$hazard_rates, collapse = " "), "\n",
    "--------------\n"
  )
}

print.HazardCurve <- function(x, ...){
  cat(format(x, ...), "\n")
  cat(format(x$specs,...), "\n")
}