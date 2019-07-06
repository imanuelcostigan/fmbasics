#' Build a `CDSSpecs`
#'
#' This class will enable you to specify CDS curves. It is used by
#' [SurvivalProbCurve()] and [HazardRate()].
#'
#' @param rank Seniority of the reference debt. Must be one of the following
#'   options: "SNR" for Senior, "SubTier3" for Subordinate Tier 3,
#'   "SubUpperTier2" for Subordinate Upper Tier 2, "SubLowerTier2" for
#'   Subordinate Lower Tier 2 "SubTier1" for Subordinate Tier 1
#' @param ... parameters passed to other `CDSSpecs` constructors
#' @param subclass the name of a `CDSSpecs` subclass. Defaults to `NULL`
#' @return Object of type `CDSSpecs`
#' @export
#' @examples
#' CDSSpecs(rank = "SubTier3")
CDSSpecs <- function(rank, ..., subclass = NULL) {
  validate_CDSSpecs(new_CDSSpecs(rank, ..., subclass = subclass))
}

new_CDSSpecs <- function(rank, ..., subclass = NULL) {
  structure(
    list(
      rank = rank,
      ...
    ),
    class = c(subclass, "CDSSpecs")
  )
}

validate_CDSSpecs <- function(x) {
  assertthat::assert_that(
    assertthat::is.string(x$rank),
    x$rank %in% c("SNR", "SubTier1", "SubUpTier2", "SubLowTier2", "SubTier3")
  )
  x
}


#' Builds a `CDSSingleNameSpecs`
#'
#' A subclass of [CDSSpecs()], that implements specifications for single name
#' CDS curves
#'
#' @inheritParams CDSSpecs
#' @param name Reference debt issuer. Must be a string.
#' @return An object of type `CDSSingleNameSpecs`
#' @export
#' @examples
#' CDSSingleNameSpecs(rank = "SNR", name = "Westpac")
CDSSingleNameSpecs <- function(rank, name) {
  validate_CDSSingleNameSpecs(new_CDSSingleNameSpecs(rank, name))
}

new_CDSSingleNameSpecs <- function(rank, name) {
  CDSSpecs(
    rank = rank,
    name = name,
    subclass = "CDSSingleNameSpecs"
  )
}
validate_CDSSingleNameSpecs <- function(x) {
  assertthat::assert_that(assertthat::is.string(x$name))
  x
}

#' Build a `CDSMarkitSpecs`
#'
#' A subclass of [CDSSpecs()], only for Markit sector curves. Note that the
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
CDSMarkitSpecs <- function(rating, region, sector) {
  valitate_CDSMarkitSpecs(new_CDSMarkitSpecs(rating, region, sector))
}
new_CDSMarkitSpecs <- function(rating, region, sector) {
  CDSSpecs(
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
#' @param reference_date the curve's reference date as a [base::Date]
#' @param tenors a numeric vector of pillar points time steps expressed in years
#' @param spreads a numeric vector of creadit default spreads expressed in
#'   decimals. Must be the same length as `tenors`
#' @param lgd the loss given default associated with the curve as supplied by
#'   Markit and expressed as a decimal value
#' @param premium_frequency represents the number of premiums payments per annum
#'   expressed as an integer. Must be one of 1, 2, 4 or 12.
#' @param specs CDS curve specifications that inherits from [CDSSpecs()]
#' @return An object of type `CDSCurve`
#' @export
#' @examples
#' curve_specs <- CDSMarkitSpecs(
#'   rating = "AAA",
#'   region = "Japan",
#'   sector = "Utilities"
#' )
#'
#' cds_curve <- CDSCurve(
#'   as.Date("2019-06-29"),
#'   tenors = c(1, 3, 5, 7),
#'   spreads = c(0.0050, 0.0070, 0.0090, 0.0110),
#'   lgd = 0.6,
#'   premium_frequency = 4,
#'   specs = curve_specs
#' )
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
    is.CDSSpecs(x$specs),
    x$premium_frequency %in% c(12, 4, 2, 1)
  )
  x
}


#' Builds a `SurvivalProbCurve`
#'
#' @param reference_date Curves's reference date
#' @param tenors pillar points expressed in year fraction
#' @param probabilities Survival Probabilities
#' @param specs An input of type CDSSpec. Contains curve specifications.
#'
#' @return returns an object of type `SurvivalProbCurve`
#'
#' @examples
#' curve_specs <- CDSMarkitSpecs(rating = "AAA", region = "Japan", sector = "Utilities")
#'
#' sp_curve <- SurvivalProbCurve(as.Date("2019-06-29"),
#'   tenors = c(1, 3, 5, 7),
#'   probabilities = c(0.99, 0.99, 0.99, 0.99),
#'   specs = curve_specs
#' )
SurvivalProbCurve <- function(reference_date, tenors, probabilities, specs) {
  validate_SurvivalProbCurve(new_SurvivalProbCurve(
    reference_date, tenors,
    probabilities, specs
  ))
}

new_SurvivalProbCurve <- function(reference_date, tenors, probabilities, specs) {
  n <- max(NROW(tenors), NROW(probabilities))
  structure(list(
    specs = specs,
    reference_date = reference_date,
    tenors = rep_len(tenors, n),
    probabilities = rep_len(probabilities, n)
  ),
  class = "SurvivalProbCurve"
  )
}

validate_SurvivalProbCurve <- function(x) {
  assertthat::assert_that(
    lubridate::is.Date(x$reference_date),
    is.numeric(x$tenors),
    all(is.numeric(x$probabilities), x$probabilities >= 0, x$probabilities <= 1),
    is.CDSSpecs(x$specs)
  )
  x
}



#' Builds a `HazardRate`
#'
#' @param reference_date Curves's reference date
#' @param tenors pillar points expressed in year fraction
#' @param hazard_rates hazard_rates
#' @param specs An input of type CDSSpec. Contains curve specifications.
#'
#' @return returns an object of type `hazard_rates`
#'
#' @examples
#' curve_specs <- CDSMarkitSpecs(rating = "AAA", region = "Japan", sector = "Utilities")
#'
#' hr_curve <- HazardRate(as.Date("2019-06-29"),
#'   tenors = c(1, 3, 5, 7),
#'   hazard_rates = c(0.05, 0.05, 0.05, 0.05),
#'   specs = curve_specs
#' )
HazardRate <- function(reference_date, tenors, hazard_rates, specs) {
  validate_HazardRate(new_HazardRate(
    reference_date, tenors,
    hazard_rates, specs
  ))
}

new_HazardRate <- function(reference_date, tenors, hazard_rates, specs) {
  n <- max(NROW(tenors), NROW(hazard_rates))
  structure(list(
    specs = specs,
    reference_date = reference_date,
    tenors = rep_len(tenors, n),
    hazard_rates = rep_len(hazard_rates, n)
  ),
  class = "HazardRate"
  )
}

validate_HazardRate <- function(x) {
  assertthat::assert_that(
    lubridate::is.Date(x$reference_date),
    is.numeric(x$tenors),
    is.numeric(x$hazard_rates),
    is.CDSSpecs(x$specs)
  )
  x
}
