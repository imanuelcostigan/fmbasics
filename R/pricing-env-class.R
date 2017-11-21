#
# PricingEnv <- function(curves, fx_rates) {
#   structure(list(
#     curves = curves,
#     fx_rates = fx_rates
#   ))
# }
#
#
# get_zero_curve(env, currency, index)
#
#
# get_fx_rate(env, currency_pair)
#
#


new_FXRates <- function(currency_pairs, rates) {
new_FXRates <- function(isos, rates) {
  # Store ISOs as an index on which to search
  structure(tibble::tibble(
    isos = isos,
    rates = rates),
    class = c("FXRates", "PricingEnv", "tbl_df", "tbl", "data.frame")
  )
}

validate_FXRates <- function(x) {
  assertthat::assert_that(
    is.numeric(x$rates),
    is.character(x$isos),
    anyDuplicated(x$isos) == 0,
    length(x$isos) == length(x$rates)
  )
  x
}

FXRates <- function(isos, rates) {
  validate_FXRates(new_FXRates(isos, rates))
}

is.FXRates <- function(x) {
  inherits(x, "FXRates")
}

#' @importFrom tibble as_tibble
#' @export
as_tibble.FXRates <- function(x) {
  class(x) <- utils::tail(class(x), -2)
  x
}

#' @importFrom tibble tbl_sum
#' @export
tbl_sum.FXRates <- function(x) {
  paste("<FXRates> of length", nrow(x))
}


ZeroCurves <- function(names, curves) {
  validate_ZeroCurves(new_ZeroCurves(names, curves))
}

new_ZeroCurves <- function(names, curves) {
  structure(tibble::tibble(
    names = names,
    curves = curves),
    class = c("ZeroCurves", "PricingEnv", "tbl_df", "tbl", "data.frame")
  )
}

validate_ZeroCurves <- function(x) {
  assertthat::assert_that(
    is.character(x$names),
    anyDuplicated(x$names) == 0,
    is_atomic_list(x$curves, is.ZeroCurve),
    length(unique(vapply(x$curves, "[[", numeric(1), "reference_date"))) == 1
  )
  x
}

as_tibble.ZeroCurves <- function(x) {
  class(x) <- utils::tail(class(x), -2)
  x
}

tbl_sum.ZeroCurves <- function(x) {
  paste("<ZeroCurves> of length", nrow(x))
}


get_fx_rate <- function(fx_rates, ccy_pair) {
  res <- fx_rates[fx_rates$isos == iso(ccy_pair), ][["rates"]]
  if(length(res) == 0) return(NA_real_) else return(res)
}
