#' Build a `CDSSpec`
#'
#' This class will enable you to specify CDS curves. It is used by
#' [SurvivalProbabilitiesCurve()] and [ZeroHazardCurve()].
#'
#' @param rank Seniority of the reference debt. Must be one of the following
#'   options: "SNR" for Senior, "SubTier3" for Subordinate Tier 3,
#'   "SubUpperTier2" for Subordinate Upper Tier 2, "SubLowerTier2" for
#'   Subordinate Lower Tier 2 "SubTier1" for Subordinate Tier 1. "Empty" rank can be
#'   used for a generic instance of the class.
#' @param ... parameters passed to other `CDSSpec` constructors
#' @param subclass the name of a `CDSSpec` subclass. Defaults to `NULL`
#' @return Object of type `CDSSpec`
#' @examples
#' CDSSpec(rank = "SubTier3")
#' @family CDS curve helpers
#' @export
CDSSpec <- function(rank, ..., subclass = NULL) {
  validate_CDSSpec(new_CDSSpec(rank, ..., subclass = subclass))
}

new_CDSSpec <- function(rank, ..., subclass = NULL) {
  structure(
    list(
      rank = rank,
      ...
    ),
    class = c(subclass, "CDSSpec")
  )
}

validate_CDSSpec <- function(x) {
  assertthat::assert_that(
    assertthat::is.string(x$rank),
    x$rank %in% c("SNR", "SubTier1", "SubUpTier2", "SubLowTier2", "SubTier3", "Empty")
  )
  x
}


#' Builds a `CDSSingleNameSpec`
#'
#' A subclass of [CDSSpec()], that implements specifications for single name
#' CDS curves
#'
#' @inheritParams CDSSpec
#' @param name Reference debt issuer. Must be a string.
#' @return An object of type `CDSSingleNameSpec`
#' @examples
#' CDSSingleNameSpec(rank = "SNR", name = "Westpac")
#' @family CDS curve helpers
#' @export
CDSSingleNameSpec <- function(rank, name) {
  validate_CDSSingleNameSpec(new_CDSSingleNameSpec(rank, name))
}

new_CDSSingleNameSpec <- function(rank, name) {
  CDSSpec(
    rank = rank,
    name = name,
    subclass = "CDSSingleNameSpec"
  )
}
validate_CDSSingleNameSpec <- function(x) {
  assertthat::assert_that(assertthat::is.string(x$name))
  x
}

#' Build a `CDSMarkitSpec`
#'
#' A subclass of [CDSSpec()], only for Markit sector curves. Note that the
#' paramter `rank` is fixed to be "SNR", as per Markit's methodology documents
#'
#' @param rating valid options are "AAA", "AA", "A", "BBB", "BB", "B", "CCC"
#' @param region valid options are "AsiaExJapan", "EastEurope", "Europe",
#'   "Japan", "LatinAmerica", "NorthAmerica", "MiddleEast", "Oceania"
#' @param sector valid options are "BasicMaterials", "ConsumerGoods",
#'   "ConsumerServices", "Energy", "Financials", "Government", "Healtcare",
#'   "Technology", "TeleCom", "Utilities"
#' @return An object of type `CDSMarkitSpec`
#' @examples
#' CDSMarkitSpec(rating = "AAA", region = "Japan", sector = "Utilities")
#' @family CDS curve helpers
#' @export
CDSMarkitSpec <- function(rating, region, sector) {
  validate_CDSMarkitSpec(new_CDSMarkitSpec(rating, region, sector))
}

new_CDSMarkitSpec <- function(rating, region, sector) {
  CDSSpec(
    rank = "SNR",
    rating = rating,
    region = region,
    sector = sector,
    subclass = "CDSMarkitSpec"
  )
}

validate_CDSMarkitSpec <- function(x) {
  assertthat::assert_that(
    all(x$rating %in% c("AAA", "AA", "A", "BBB", "BB", "B", "CCC")),
    all(x$region %in% c(
      "AsiaExJapan", "EastEurope", "Europe", "Japan", "LatinAmerica",
      "NorthAmerica", "MiddleEast", "Oceania"
    )),
    all(x$sector %in% c(
      "BasicMaterials", "ConsumerGoods", "ConsumerServices",
      "Energy", "Financials", "Government", "Healtcare", "Technology",
      "TeleCom", "Utilities"
    ))
  )
  x
}


#' Builds a `CDSCurve`
#'
#' This will allow you to create an instance of a CDS curve.
#'
#' @param reference_date the curve's reference date as a [base::Date]
#' @param tenors a numeric vector of pillar points time steps expressed in years
#' @param spreads a numeric vector of creadit default spreads expressed in
#'   decimals. Must be the same length as `tenors`
#' @param lgd the loss given default associated with the curve as supplied by
#'   Markit and expressed as a decimal value
#' @param premium_frequency represents the number of premiums payments per annum
#'   expressed as an integer. Must be one of 1, 2, 4 or 12.
#' @param specs CDS curve specifications that inherits from [CDSSpec()]
#' @return An object of type `CDSCurve`
#' @examples
#' curve_specs <- CDSMarkitSpec(
#'   rating = "AAA",
#'   region = "Japan",
#'   sector = "Utilities"
#' )
#'
#' CDSCurve(
#'   as.Date("2019-06-29"),
#'   tenors = c(1, 3, 5, 7),
#'   spreads = c(0.0050, 0.0070, 0.0090, 0.0110),
#'   lgd = 0.6,
#'   premium_frequency = 4,
#'   specs = curve_specs
#' )
#' @family CDS curve helpers
#' @export
CDSCurve <- function(reference_date, tenors, spreads, lgd, premium_frequency,
  specs) {

  validate_CDSCurve(
    new_CDSCurve(reference_date, tenors, spreads, lgd, premium_frequency, specs)
  )

}

new_CDSCurve <- function(reference_date, tenors, spreads, lgd, premium_frequency,
  specs) {
  structure(
    list(
      reference_date = reference_date,
      lgd = lgd,
      tenors = tenors,
      spread = spreads,
      specs = specs,
      premium_frequency = premium_frequency
    ),
    class = "CDSCurve"
  )

}

validate_CDSCurve <- function(x) {
  assertthat::assert_that(
    assertthat::is.date(x$reference_date),
    is.numeric(x$tenors),
    #is.numeric(x$spreads),
    #length(x$tenors) == length(x$spreads),
    assertthat::is.number(x$lgd),
    is.CDSSpec(x$specs),
    x$premium_frequency %in% c(12, 4, 2, 1)
  )
  x
}

#' Builds a `SurvivalProbabilitiesCurve`
#'
#' This will allow you to create a survival probability curve. This will
#' typically be bootstrapped from a [CDSCurve()].
#'
#' @inheritParams CDSCurve
#' @param survival_probabilities a vector of survival probabilities corresponding to each
#'   time step in `tenors`.
#' @param d1 a `Date` vector containing the as of date
#' @param d2 a `Date` vector containing the date to which the survival probability
#'   applies
#' @param specs CDS curve specifications that inherits from [CDSSpec()]
#' @return returns an object of type `SurvivalProbabilitiesCurve`
#' @examples
#'SurvivalProbabilities(0.97, Sys.Date(), Sys.Date() + 30, CDSSpec("Empty"))
#' @family CDS curve helpers
#' @export
SurvivalProbabilities <- function(values, d1, d2, specs) {
  validate_SurvivalProbabilities(
    new_SurvivalProbabilities(values, d1, d2, specs)
  )
}

new_SurvivalProbabilities <- function(values, d1, d2, specs) {
  structure(
    list(
      specs = specs,
      values = values,
      start_date = d1,
      end_date = d2
    ),
    class = "SurvivalProbabilities"
  )
}

validate_SurvivalProbabilities <- function(x) {
  assertthat::assert_that(
    assertthat::is.date(x$start_date),
    is.numeric(x$values),
    assertthat::is.date(x$end_date),
    all(x$values >= 0, x$values <= 1),
    all(x$start_date <= x$end_date),
    is.CDSSpec(x$specs)
  )
  x
}


#' Builds a `ZeroHazardRate`
#'
#' This will allow you to create a harzard rate curve. This will typically be
#' bootstrapped or implied from a [CDSCurve()] or [SurvivalProbabilitiesCurve()].
#'
#' @inheritParams CDSCurve
#' @param hazard_rates a vector of hazard rates corresponding to each time step
#' in `tenors`
#' @param value a numeric vector containing zero hazard rate values (as decimals).
#' @param compounding a numeric vector representing the [compounding] frequency.
#' @param day_basis a character vector representing the day basis associated
#'   with the interest rate and hazard rate(see [fmdates::year_frac()])
#' @param specs CDS curve specifications that inherits from [CDSSpec()]
#' @return returns an object of type `hazard_rates`
#' @examples
#'
#' ZeroHazardRate(values = c(0.04, 0.05), compounding = c(2, 4),
#' day_basis =  'act/365', specs = curve_specs )
#'
#' @family CDS curve helpers
#' @export
ZeroHazardRate <- function(values, compounding, day_basis, specs) {
  validate_ZeroHazardRate(new_ZeroHazardRate(values, compounding, day_basis, specs))
}

new_ZeroHazardRate <- function(values, compounding, day_basis, specs) {

  structure(
    list(
      specs = specs,
      values = values,
      day_basis = day_basis,
      compounding = compounding
    ),
    class = "ZeroHazardRate"
  )
}

validate_ZeroHazardRate <- function(x) {
  assertthat::assert_that(
    all(is.numeric(x$values)),
    is.CDSSpec(x$specs),
    fmdates::is_valid_day_basis(x$day_basis),
    is_valid_compounding(x$compounding)
  )
  x
}



#' CreditCurve class
#'
#' A class that defines the bare bones of a credit curve pricing
#' structure.
#'
#' A term structure of credit spread is a curve showing
#' several credit spreads across different contract lengths (2 month,
#' 2 year, 20 year, etc...) for a similar debt contract. The curve shows the
#' relation between the (level of) crdit spread and the
#' time to maturity, known as the "term", of the debt for a given borrower in a
#' given currency. When the effect of coupons on spreads are stripped away, one has a
#' zero-coupon credit curve.
#'
#' The following interpolation schemes are supported by `ZeroCurve`:
#' `ConstantInterpolation`, `LinearInterpolation`, `LogDFInterpolation` and
#' `CubicInterpolation`. Points outside the calibration region use constant
#' extrapolation on the zero hazard rate.
#'
#' @param survival_probabilities a [`SurvivalProbabilities`] object. These are converted to
#'   continuously compounded zero coupon interest rates with an `act/365` day
#'   basis for internal storage purposes
#' @param reference_date a `Date` object
#' @param interpolation an [`Interpolation`] object
#' @param specs CDS curve specifications that inherits from [CDSSpec()]
#'
#' @return a `CreditCurve` object
#'
#' @examples
#' curve_specs <- CDSMarkitSpec(rating = "AAA", region = "Japan", sector = "Utilities")
#' zero_curve <- build_zero_curve()
#' ref_date <- zero_curve$reference_date
#' specs <- CDSMarkitSpec(rating = "AAA", region = "Japan", sector = "Utilities")
#' cds_curve <- CDSCurve(reference_date = ref_date, tenors = c(1, 3, 5, 7),
#' spreads = c(0.0050, 0.0070, 0.0090, 0.0110), lgd = .6, premium_frequency = 4,
#' specs = curve_specs)
#' sp <- as_SurvivalProbabilities(x = cds_curve, zero_curve = zero_curve)
#' CreditCurve(survival_probabilities = sp, reference_date =ref_date,
#'  interpolation =  CubicInterpolation(), specs = curve_specs)
#' @export
#' @seealso [Interpolation]
CreditCurve <- function(survival_probabilities, reference_date, interpolation,
  specs) {
  validate_CreditCurve(new_CreditCurve(survival_probabilities, reference_date,
    interpolation, specs))
}


new_CreditCurve <-
  function(survival_probabilities, reference_date, interpolation, specs) {
    assertthat::assert_that(
      is.ConstantInterpolation(interpolation) ||
        is.LinearInterpolation(interpolation) ||
        is.LogDFInterpolation(interpolation) ||
        is.CubicInterpolation(interpolation)
    )

    # Will internally calculate zero_hazard_rates are calculated using act/365 basis and
    # compounded continuously for speed purposes.

    db <- "act/365"
    cp <- Inf

    dt <-
      fmdates::year_frac(reference_date, survival_probabilities$end_date, db)
    r <- as_ZeroHazardRate(survival_probabilities, cp, db)$value

    f <- function(t) {
      before_first <- t < dt[1]
      after_last <- t > utils::tail(dt, 1)
      in_support <- !before_first & !after_last

      res <- vector("numeric", length = length(t))

      # Constant extrapolation on zeros before and after region of calibration.
      # This could be loosened by user paramterisation in the future.

      if (is.ConstantInterpolation(interpolation)) {
        g <- stats::approxfun(dt, r, method = "constant", rule = 2)
        return(g(t))
      }

      if (is.LinearInterpolation(interpolation)) {
        g <- stats::approxfun(dt, r, method = "linear", rule = 2)
        return(g(t))
      }

      if (is.LogDFInterpolation(interpolation)) {
        # rule = 2 is used to force approxfun to return a non-NA value outside
        # calibration region. But uses constant extrapolation on zeros outside
        # the region rather than constant on -r * t
        g <-
          stats::approxfun(dt, -dt * r, method = "linear", rule = 2)
        res[before_first] <- r[1]
        res[after_last] <- utils::tail(r, 1)
        res[in_support] <- -g(t[in_support]) / t[in_support]
        return(res)
      }
      if (is.CubicInterpolation(interpolation)) {
        g <- stats::splinefun(dt, r, method = "natural")
        # Override extarapolation to use constant extrapolation on zeros
        res[before_first] <- r[1]
        res[after_last] <- utils::tail(r, 1)
        res[in_support] <- g(t[in_support])
        return(res)
      }
    }


    structure(
      list(
        reference_date = reference_date,
        survival_probabilities = survival_probabilities,
        pillar_times = dt,
        pillar_values = r,
        interpolator = f,
        day_basis = db,
        compounding = cp,
        specs = specs
      ),
      class = "CreditCurve"
    )

  }

validate_CreditCurve <- function(x) {
  assertthat::assert_that(
    all(x$reference_date <= x$survival_probabilities$end_date),
    assertthat::is.date(x$reference_date),
    !is.unsorted(x$survival_probabilities$end_date),
    is.CDSSpec(x$specs)
  )
  x
}


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

#' Inherits from ZeroHazardRate
#'
#' Checks whether object inherits from `ZeroHazardRate` class
#'
#' @param x an R object
#' @return `TRUE` if `x` inherits from the `ZeroHazardRate` class; otherwise `FALSE`
#' @examples
#' is.ZeroHazardRate(ZeroHazardRate(0.04, 2, "act/365", CDSSpec("Empty")))
#' @export

is.ZeroHazardRate <- function(x) inherits(x, "ZeroHazardRate")

#' Inherits from SurvivalProbabilities
#'
#' Checks whether object inherits from `SurvivalProbabilities` class
#'
#' @param x an R object
#' @return `TRUE` if `x` inherits from the `SurvivalProbabilities` class; otherwise `FALSE`
#' @examples
#' is.SurvivalProbabilities(SurvivalProbabilities(0.97, Sys.Date(), Sys.Date() + 30, CDSSpec("Empty")))
#' @export

is.SurvivalProbabilities <- function(x) inherits(x, "SurvivalProbabilities")

as.list.CDSSpec <- function(x) {
  out <- list()
  for(i in seq_along(x)) {
    out[i] <- x[i]
  }
  out
}


#' @export
format.CDSSpec <- function(x,...){
  paste0(
    "<Curve Specification> \n",
    "Rank: ", x$rank
  )
}

#' @export
print.CDSSpec <- function(x, ...){
  cat(format(x, ...), "\n")
}

#' @export
format.CDSMarkitSpec <- function(x,...){
  paste0(
    "<Curve Specification: Markit CDS Sector Curve> \n",
    "Rank: SNR \n",
    "Rating: ", x$rating, "\n",
    "Region: " , x$region, "\n",
    "Sector: " , x$sector
  )
}

#' @export
print.CDSMarkitSpec <- function(x, ...){
  cat(format(x, ...), "\n")
}

#' @export
format.CDSSingleNameSpec <- function(x,...) {
  paste0(
    "<Curve Specification: Single Name CDS Curve> \n",
    "Rank: ", x$rank, "\n",
    "Name: ", x$name
  )
}

#' @export
print.CDSSingleNameSpec <- function(x, ...) {
  cat(format(x, ...), "\n")
}


#' @export
format.CDSCurve <- function(x, ...) {
  paste0(
    "<CDSCurve as of ", x$reference_date, "> \n",
    "Tenors: ", paste(x$tenors, collapse = " "), "\n",
    "Spreads: ", paste(x$spread, collapse = " "), "\n",
    "LGD: ", x$LGD, "\n",
    "Premium Frequency: ", x$premium_frequency, "\n"
  )
}

#' @export
print.CDSCurve <- function(x, ...) {
  cat(format(x, ...), "\n")
  cat(format(x$specs,...), "\n")
}


#' @export
format.SurvivalProbabilities <- function(x, ...) {
  paste0("<SurvivalProbabilities> ", x$value, ', ',
    x$start_date, '--', x$end_date, collapse = '\n')
}

#' @export
print.SurvivalProbabilities <- function(x, ...) {cat(format(x), "\n"); invisible(x)}


#' @export
format.ZeroHazardRate <- function(x, ...) {
  rp <- format(x$value * 100, nsmall = 5)
  cmp <- compounding_as_string(x$compounding)
  db <- x$day_basis
  paste0("<ZeroHazardRate> ", toupper(paste0(rp, "%, ", cmp, ", ", db)),
    collapse = '\n')
}

#' @export
print.ZeroHazardRate <- function(x, ...) {cat(format(x), "\n"); invisible(x)}


#' @export
is.CreditCurve <- function(x) {
  inherits(x, "CreditCurve")
}

#' @export
format.CreditCurve <- function(x, ...) {
  paste0("<CreditCurve> @ ", format(x$reference_date, "%e %B %Y"))
}

#' @export
print.CreditCurve <- function(x, ...) {
  cat(format(x), "\n")
  print(tibble::as_tibble(x))
}

#' CreditCurve attributes as a data frame
#'
#' Create a `tibble` that contains the pillar point maturities in years (using
#' the `act/365` convention) and the corresponding continuously compounded zero
#' rates.
#'
#' @param x a `CreditCurve` object
#' @param ... other parameters that are not used by this methods
#' @return a `tibble` with two columns named `Years` and `Zero Hazard Rates`.
#' @seealso [tibble::tibble()]
#' @importFrom tibble as_tibble
#' @examples
#' curve_specs <- CDSMarkitSpec(rating = "AAA", region = "Japan", sector = "Utilities")
#' zero_curve <- build_zero_curve()
#' ref_date <- zero_curve$reference_date
#' specs <- CDSMarkitSpec(rating = "AAA", region = "Japan", sector = "Utilities")
#' cds_curve <- CDSCurve(reference_date = ref_date,
#' tenors = c(1, 3, 5, 7), spreads = c(0.0050, 0.0070, 0.0090, 0.0110), lgd = .6,
#' premium_frequency = 4, specs = curve_specs)
#' sp <- as_SurvivalProbabilities(x = cds_curve, zero_curve = zero_curve)
#' cc <- CreditCurve(survival_probabilities = sp,
#'  reference_date =ref_date, interpolation =  LinearInterpolation(),
#'  specs = curve_specs)
#'  as_tibble(cc)
#' @export
as_tibble.CreditCurve <- function(x, ...) {
  tibble::tibble(
    Years = x$pillar_times,
    `Zero Hazard Rates` = x$pillar_values
  )
}


#' Interpolate a `CreditCurve`
#'
#' There are two key interpolation schemes available in the `stats` package:
#' constant and linear interpolation via [stats::approxfun()] and
#' spline interpolation via [stats::splinefun()]. The `interpolate()` method
#' is a simple wrapper around these methods that are useful for the purposes
#' of interpolation financial market objects like credit curves.
#'
#' @param x a `CreditCurve` object
#' @param at a non-negative numeric vector representing the years at which to
#'   interpolate the Credit curve
#' @param ... unused in this method
#' @return a numeric vector of zero rates (continuously compounded, act/365)
#' @examples
#' zc <- build_zero_curve(LogDFInterpolation())
#' interpolate(zc, c(1.5, 3))
#' @export
#' @family interpolate functions
interpolate.CreditCurve <- function(x, at, ...) {
  assertthat::assert_that(is.numeric(at), all(at >= 0))
  x$interpolator(at)
}

#' @rdname interpolate_zeros *edit interpolate zero
#' @export
interpolate_zeros.CreditCurve <- function(x, at, compounding = NULL, day_basis = NULL, ...) {

  assertthat::assert_that(
    is.CreditCurve(x),
    assertthat::is.date(at),
    is.null(compounding) || is_valid_compounding(compounding),
    is.null(day_basis) || fmdates::is_valid_day_basis(day_basis)
  )

  tt <- year_frac(x$reference_date, at, x$day_basis)
  cr <- ZeroHazardRate(interpolate(x, tt), x$compounding, x$day_basis, specs = x$specs)
  if (is.null(compounding) && is.null(day_basis)) {
    return(cr)
  } else {
    as_ZeroHazardRate(cr, compounding = compounding, day_basis = day_basis, specs = x$specs)
  }
}

#' @rdname interpolate_dfs
#' @export
interpolate_dfs.CreditCurve <- function(x, from, to, ...) {
  assertthat::assert_that(
    is.CreditCurve(x),
    assertthat::is.date(from),
    assertthat::is.date(to),
    all(from <= to)
  )
  r1 <- interpolate_zeros(x, from)
  r2 <- interpolate_zeros(x, to)
  df_start <- as_SurvivalProbabilities(r1, x$reference_date, from, x$specs)
  df_end <- as_SurvivalProbabilities(r2, x$reference_date, to, x$specs)
  df_end / df_start
}


#' @rdname interpolate_dfs
#' @export
interpolate_fwds.CreditCurve <- function(x, from, to, ...) {
  assertthat::assert_that(
    is.CreditCurve(x),
    assertthat::is.date(from),
    assertthat::is.date(to),
    all(from < to)
  )
  forward_dfs <- interpolate_dfs(x, from, to, ...)
  as_ZeroHazardRate(forward_dfs, 0, x$day_basis, x$specs)
}





