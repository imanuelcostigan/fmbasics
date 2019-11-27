#' Coerce to InterestRate
#'
#' You can coerce objects to the `SurvivalProbabilities` class using this method.
#'
#' @param x object to coerce
#' @param ... other parameters passed to methods
#' @return an `SurvivalProbabilities` object
#' @export
as_SurvivalProbabilities <- function(x, ...) UseMethod("as_SurvivalProbabilities")

#' Bootstraps Survival Probabilitie from a CDS curve
#' Using \href{https://www.rdocumentation.org/packages/credule/versions/0.1.3}{credule package.}
#' The output of bootstrapping is a vector of cumulative survival probabilities.
#'
#' @param x An object of type `CDSCurve`
#' @param zero_curve An object of type `ZeroCurve`
#' @param num_timesteps_pa It represents the number of timesteps used to perform the numerical integral
#'  required while computing the default leg value. It is shown that a monthly discretisation
#'   usually gives a good approximation (Ref. Valuation of Credit Default Swaps, Dominic O Kane and
#'   Stuart Turnbull)
#' @param accrued_premium If set to TRUE, the accrued premium will be taken into account in the calculation of the premium leg value.
#'
#' @return An object of type `SurvivalProbabilitiesCurve`

as_SurvivalProbabilities.CDSCurve <- function(x,
  zero_curve,
  num_timesteps_pa = 12,
  accrued_premium = TRUE) {
  if (x$reference_date != zero_curve$reference_date) {
    stop("The reference dates for CDS Curve and the Zero Curve are different",
      call. = FALSE)
  }
  if (!is.ZeroCurve(zero_curve)) {
    stop("zero_curve must be an object of type ZeroCurve", call. = FALSE)
  }

  sp_output <- credule::bootstrapCDS(
    yieldcurveTenor = zero_curve$pillar_times,
    yieldcurveRate = zero_curve$pillar_zeros,
    cdsTenors = x$tenors,
    cdsSpreads = x$spread,
    recoveryRate = 1 - x$lgd,
    numberPremiumPerYear = x$premium_frequency,
    numberDefaultIntervalPerYear = num_timesteps_pa,
    accruedPremium = accrued_premium
  )


  SurvivalProbabilities(
    d1 = x$reference_date,
    d2 = x$reference_date + 365 * x$tenors,
    values = sp_output$survprob,
    specs = x$specs
  )
}


#' @inheritParams SurvivalProbabilities
#' @rdname as_SurvivalProbabilities
#' @examples
#' curve_specs <- CDSMarkitSpec(
#'   rating = "AAA",
#'   region = "Japan",
#'   sector = "Utilities"
#' )
#' HR <- ZeroHazardRate(values = c(0.04, 0.05), compounding = c(2, 4),
#' day_basis = 'act/365', specs = curve_specs)
#' as_SurvivalProbabilities(HR, lubridate::ymd(20160202), lubridate::ymd(20160302))
#' @export
as_SurvivalProbabilities.ZeroHazardRate <- function(x, d1, d2, ...) {
  assertthat::assert_that(
    lubridate::is.Date(d1),
    lubridate::is.Date(d2)
  )
  # year_frac is vectorised
  term <- fmdates::year_frac(d1, d2, x$day_basis)
  # determine compounding frequency for each x value
  is_cc <- is.infinite(x$compounding)
  is_simple <- x$compounding == 0
  is_tbill <- x$compounding == -1
  is_pc <- !(is_cc | is_simple | is_tbill)
  # determine discount factors
  df <- vector("numeric", NROW(x$value))
  df[is_cc] <- exp(-x$value * term)
  df[is_simple] <- 1 / (1 + x$value * term)
  df[is_tbill] <- 1 - x$value * term
  df[is_pc] <- 1 / ((1 + x$value / x$compounding) ^
      (x$compounding * term))
  new_SurvivalProbabilities(values = df, d1 = d1, d2 = d2, specs = x$specs)
}




###########

#' Coerce to ZeroHazardRate
#'
#' You can coerce objects to the `ZeroHazardRate` class using this method.
#'
#' @param x object to coerce
#' @param ... other parameters passed to methods
#' @return an `ZeroHazardRate` object
#' @examples
#' library("lubridate")
#' as_ZeroHazardRate(SurvivalProbabilities(0.95, ymd(20130101), ymd(20140101), CDSSpec("Empty")),
#'   compounding = 2, day_basis = "act/365")
#' @export
as_ZeroHazardRate <- function(x, ...) UseMethod("as_ZeroHazardRate")

#' @inheritParams ZeroHazardRate
#' @rdname as_ZeroHazardRate
#' @export
as_ZeroHazardRate.SurvivalProbabilities <- function(x, compounding, day_basis, ...) {
  assertthat::assert_that(
    fmdates::is_valid_day_basis(day_basis),
    is_valid_compounding(compounding)
  )
  term <- fmdates::year_frac(x$start_date, x$end_date, day_basis)
  is_cc <- is.infinite(compounding)
  is_simple <- compounding == 0
  is_tbill <- compounding == -1
  is_pc <- !(is_cc | is_simple | is_tbill)
  rate <- vector("numeric", NROW(x$value))
  rate[is_cc] <- -log(x$value) / term
  rate[is_simple] <- (1 / x$value - 1) / term
  rate[is_tbill] <- (1 - x$value) / term
  rate[is_pc] <- compounding *
    ((1 / x$value) ^ (1 / (compounding * term)) - 1)
  new_ZeroHazardRate(values = rate, compounding = compounding, day_basis = day_basis,
    specs = x$specs)
}

#' @inheritParams ZeroHazardRate
#' @rdname as_ZeroHazardRate
#' @export
as_ZeroHazardRate.ZeroHazardRate <- function(x, compounding = NULL, day_basis = NULL, ...) {
  if (!all(is.null(compounding), is.null(day_basis))) {
    # start and end dates here don't matter.
    sp <- as_SurvivalProbabilities(x, as.Date("2013-01-01"), as.Date("2014-01-01"))
    if (!is.null(compounding)) {
      compounding <- rep(compounding, length(x$compounding))
    } else {
      compounding <- x$compounding
    }
    if (!is.null(day_basis)) {
      day_basis <- rep(day_basis, length(x$day_basis))
    } else {
      day_basis <- x$day_basis
    }
    return(as_ZeroHazardRate(sp, compounding, day_basis, x$specs))
  } else {
    return(x)
  }
}



