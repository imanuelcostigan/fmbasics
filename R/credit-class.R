#' Build a `CDSSpec`
#'
#' This class will enable you to specify CDS curves. It is used by
#' [SurvivalCurve()] and [HazardCurve()].
#'
#' @param rank Seniority of the reference debt. Must be one of the following
#'   options: "SNR" for Senior, "SubTier3" for Subordinate Tier 3,
#'   "SubUpperTier2" for Subordinate Upper Tier 2, "SubLowerTier2" for
#'   Subordinate Lower Tier 2 "SubTier1" for Subordinate Tier 1
#' @param ... parameters passed to other `CDSSpec` constructors
#' @param subclass the name of a `CDSSpec` subclass. Defaults to `NULL`
#' @return Object of type `CDSSpec`
#' @export
#' @examples
#' CDSSpec(rank = "SubTier3")
#' @family CDS curve helpers
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
    x$rank %in% c("SNR", "SubTier1", "SubUpTier2", "SubLowTier2", "SubTier3")
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
#' @export
#' @examples
#' CDSSingleNameSpec(rank = "SNR", name = "Westpac")
#' @family CDS curve helpers

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

#' Build a `CDSMarkitSpecs`
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
#' @return An object of type `CDSMarkitSpecs`
#' @export
#' @examples
#' CDSMarkitSpecs(rating = "AAA", region = "Japan", sector = "Utilities")
#' @family CDS curve helpers
CDSMarkitSpecs <- function(rating, region, sector) {
  valitate_CDSMarkitSpecs(new_CDSMarkitSpecs(rating, region, sector))
}
new_CDSMarkitSpecs <- function(rating, region, sector) {
  CDSSpec(
    rank = "SNR",
    rating = rating,
    region = region,
    sector = sector,
    subclass = "CDSMarkitSpecs"
  )
}
valitate_CDSMarkitSpecs <- function(x) {
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
#' @export
#' @examples
#' curve_specs <- CDSMarkitSpecs(
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
    is.numeric(x$spreads),
    length(x$tenors) == length(x$spreads),
    assertthat::is.number(x$lgd),
    is.CDSSpec(x$specs),
    x$premium_frequency %in% c(12, 4, 2, 1)
  )
  x
}


#' Builds a `SurvivalCurve`
#'
#' This will allow you to create a survival probability curve. This will
#' typically be bootstrapped from a [CDSCurve()].
#'
#' @inheritParams CDSCurve
#' @param probabilities a vector of survival probabilities corresponding to each
#'   time step in `tenors`.
#' @return returns an object of type `SurvivalCurve`
#' @export
#' @examples
#' curve_specs <- CDSMarkitSpecs(
#'   rating = "AAA",
#'   region = "Japan",
#'   sector = "Utilities"
#' )
#'
#' SurvivalCurve(
#'   reference_date = as.Date("2019-06-29"),
#'   tenors = c(1, 3, 5, 7),
#'   probabilities = c(0.99, 0.99, 0.99, 0.99),
#'   specs = curve_specs
#' )
#' @family CDS curve helpers
SurvivalCurve <- function(reference_date, tenors, probabilities, specs) {
  validate_SurvivalCurve(
    new_SurvivalCurve(reference_date, tenors, probabilities, specs)
  )
}

new_SurvivalCurve <- function(reference_date, tenors, probabilities, specs) {
  structure(
    list(
      specs = specs,
      reference_date = reference_date,
      tenors = tenors,
      probabilities = probabilities
    ),
    class = "SurvivalCurve"
  )
}

validate_SurvivalCurve <- function(x) {
  assertthat::assert_that(
    assertthat::is.date(x$reference_date),
    is.numeric(x$tenors),
    is.numeric(x$probabilities),
    length(x$tenors) == length(x$probabilities),
    all(x$probabilities >= 0, x$probabilities <= 1),
    is.CDSSpec(x$specs)
  )
  x
}



#' Builds a `HazardCurve`
#'
#' This will allow you to create a harzard rate curve. This will typically be
#' bootstrapped or implied from a [CDSCurve()] or [SurvivalCurve()].
#'
#' @inheritParams CDSCurve
#' @param hazard_rates a vector of hazard rates corresponding to each time step
#' in `tenors`
#' @return returns an object of type `hazard_rates`
#' @export
#' @examples
#' curve_specs <- CDSMarkitSpecs(
#'   rating = "AAA",
#'   region = "Japan",
#'   sector = "Utilities"
#' )
#'
#' HazardCurve(
#'   reference_date = as.Date("2019-06-29"),
#'   tenors = c(1, 3, 5, 7),
#'   hazard_rates = c(0.05, 0.05, 0.05, 0.05),
#'   specs = curve_specs
#' )
#' @family CDS curve helpers
HazardCurve <- function(reference_date, tenors, hazard_rates, specs) {
  validate_HazardCurve(
    new_HazardCurve(reference_date, tenors, hazard_rates, specs)
  )
}

new_HazardCurve <- function(reference_date, tenors, hazard_rates, specs) {

  structure(
    list(
      specs = specs,
      reference_date = reference_date,
      tenors = tenors,
      hazard_rates = hazard_rates
    ),
    class = "HazardCurve"
  )
}

validate_HazardCurve <- function(x) {
  assertthat::assert_that(
    assertthat::is.date(x$reference_date),
    is.numeric(x$tenors),
    is.numeric(x$hazard_rates),
    length(x$tenors) == length(x$hazard_rates),
    is.CDSSpec(x$specs)
  )
  x
}
