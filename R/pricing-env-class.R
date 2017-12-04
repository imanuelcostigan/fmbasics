# PricingEnv and subclasses -----------------------------------------------

#' Create a `PricingEnv`
#'
#' A `PricingEnv` is a container of objects that are used for the pricing and
#' valuation of financial market contracts. Supported pricing objects include:
#' * [`ZeroCurves`][ZeroCurves] and
#' * [`FXRates`][FXRates]
#'
#' @param zero_curves a [`ZeroCurves`][ZeroCurves] object or `NULL`
#' @param fx_rates a [`FXRates`][FXRates] object or `NULL`
#' @param reference_date the `Date` on which these pricing objects are snapped.
#' Defaults to `NULL` in which case this evaluates to the `reference_date` of
#' the first zero curve in `zero_curves`.
#' @return a `PricingEnv` object
#' @examples
#' build_pricing_env()
#' @export
#' @family pricing environments
PricingEnv <- function(zero_curves, fx_rates, reference_date = NULL) {
  validate_PricingEnv(new_PricingEnv(zero_curves, fx_rates, reference_date))
}

new_PricingEnv <- function(zero_curves, fx_rates, reference_date) {
  structure(list(
    reference_date = reference_date %||% zero_curves$curves[[1]]$reference_date,
    zero_curves = zero_curves,
    fx_rates = fx_rates),
    class = "PricingEnv"
  )
}

validate_PricingEnv <- function(x) {
  assertthat::assert_that(
    lubridate::is.Date(x$reference_date),
    is.null(x$zero_curves) || is.ZeroCurves(x$zero_curves),
    is.null(x$fx_rates) || is.FXRates(x$fx_rates)
  )
  x
}

#' Inherits from PricingEnv
#'
#' Checks whether object inherits from `PricingEnv` class
#'
#' @param x an R object
#' @return `TRUE` if `x` inherits from the `PricingEnv` class; otherwise `FALSE`
#' @examples
#' is.PricingEnv(build_pricing_env())
#' @export
#' @family pricing environments
is.PricingEnv <- function(x) {
  inherits(x, "PricingEnv")
}

#' @export
format.PricingEnv <- function(x, ...) {
  paste0(
    "<PricingEnv> @ ", x$reference_date, "\n",
    "  Zero Curves: ", paste0(x$zero_curves$names, collapse = ", "), "\n",
    "  FX rates: ", paste0(x$fx_rates$isos, collapse = ", ")
  )
}

#' @export
print.PricingEnv <- function(x, ...) {
  cat(format(x), "\n")
}

#' Create a `FXRates` pricing object
#'
#' A `FXRates` object is effectively a [tibble::tibble()] containing currency
#' pair ISOs and their associated values (usually spot FX).
#'
#' @param isos a character vector of ISO codes representing currency pairs (e.g.
#' "AUDUSD") and must not contain duplicate values
#' @param rates a numeric vector representing the values of the FX rates. This
#' must be the same length as `isos`
#' @return a `FXRates` object that extends a tibble
#' @examples
#' build_fx_rates()
#' @export
#' @family pricing environments
FXRates <- function(isos, rates) {
  validate_FXRates(new_FXRates(isos, rates))
}

new_FXRates <- function(isos, rates) {
  # Store ISOs as an index on which to search
  structure(tibble::tibble(
    isos = isos,
    rates = rates),
    class = c("FXRates", "tbl_df", "tbl", "data.frame")
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

#' Inherits from FXRates
#'
#' Checks whether object inherits from `FXRates` class
#'
#' @param x an R object
#' @return `TRUE` if `x` inherits from the `FXRates` class; otherwise `FALSE`
#' @examples
#' is.FXRates(build_fx_rates())
#' @export
#' @family pricing environments
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

#' Create a `ZeroCurves` pricing object
#'
#' A `ZeroCurves` object is effectively a [tibble::tibble()] containing zero
#' curves.
#'
#' @param names a character vector of curve labels with no duplicate values
#' @param curves a list of [ZeroCurve][ZeroCurve] each of which has the same
#' `reference_date`
#' @return a `ZeroCurves` object that extends a tibble
#' @examples
#' build_zero_curves()
#' @export
#' @family pricing environments

ZeroCurves <- function(names, curves) {
  validate_ZeroCurves(new_ZeroCurves(names, curves))
}

new_ZeroCurves <- function(names, curves) {
  structure(tibble::tibble(
    names = names,
    curves = curves),
    class = c("ZeroCurves", "tbl_df", "tbl", "data.frame")
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

#' Inherits from ZeroCurves
#'
#' Checks whether object inherits from `ZeroCurves` class
#'
#' @param x an R object
#' @return `TRUE` if `x` inherits from the `ZeroCurves` class; otherwise `FALSE`
#' @examples
#' is.ZeroCurves(build_zero_curves())
#' @export
#' @family pricing environments
is.ZeroCurves <- function(x) {
  inherits(x, "ZeroCurves")
}

#' @importFrom tibble as_tibble
#' @export
as_tibble.ZeroCurves <- function(x) {
  class(x) <- utils::tail(class(x), -2)
  x
}

#' @importFrom tibble tbl_sum
#' @export
tbl_sum.ZeroCurves <- function(x) {
  paste("<ZeroCurves> of length", nrow(x))
}


# PricingEnv methods ------------------------------------------------------


#' Pick out element from PricingEnv
#'
#' This will allow you to pick an element out of a [PricingEnv][PricingEnv] given
#' a set of parameters.
#'
#' @param from a [PricingEnv][PricingEnv]
#' @param what a string specifying where the pricing element can be found. This
#'   corresponds to the `PricingEnv` field names which are currently
#'   `zero_curves` and `fx_rates`.
#' @param ... other parameters to full specify the pricing element to extract.
#'   For `zero_curves` this includes the parameter `name` which  specifies the
#'   name of the `ZeroCurve` to extract. For `fx_rates` this includes `iso`
#'   which specifies the ISO code of the currency pair of the FX rate (numeric)
#'   that is to be extracted.
#' @return a `ZeroCurve` when `what` is "zero_curves" and a number when `what`
#'   is "fx_rates".
#' @examples
#' pe <- build_pricing_env()
#' pick(pe, "zero_curves", "CC1_3M")
#' pick(pe, "fx_rates", "AUDUSD")
#' @export
#' @family pricing environments
pick <- function(from, what, ...) {
  picker(from[[what]], ...)
}

picker <- function(x, ...) UseMethod("picker")

picker.FXRates <- function(x, iso, ...) {
  res <- x[x$isos == iso, ][["rates"]]
  if(length(res) == 0) return(NA_real_) else return(res)
}

picker.ZeroCurves <- function(x, name, ...) {
  selector <- which(x$names == name)
  if (length(selector) == 0) return(NA) else x$curves[[selector]]
}

