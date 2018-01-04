#' Get ISO
#'
#' The default method assumes the ISO can be accessed as if it were an attribute
#' with name `iso` (e.g. `x$iso`). The method for `CurrencyPair` concatenates
#' the ISOs of the constituent currencies (e.g. `iso(AUDUSD())` returns
#' "AUDUSD") while the methods for `CashIndex` and `IborIndex` return the ISO of
#' the index's currency.
#'
#' @param x object from which to extract an ISO
#' @return a string of the ISO
#' @examples
#' library("lubridate")
#' iso(AUD())
#' iso(AUDUSD())
#' iso(AUDBBSW(months(3)))
#' iso(AONIA())
#' @export
iso <- function(x) UseMethod("iso")
#' @rdname iso
#' @export
iso.default <- function(x) x$iso



#' Index date shifters
#'
#' A collection of methods that shift dates according to index conventions.
#'
#' The following describes the default methods. `to_reset()` treats the input
#' dates as value dates and shifts these to the corresponding reset or fixing
#' dates using the index's spot lag; `to_value()` treats the input dates as
#' reset or fixing dates and shifts them to the corresponding value dates using
#' the index's spot lag; and `to_maturity()` treats the input dates as value
#' dates and shifts these to the index's corresponding maturity date using the
#' index's tenor.
#'
#' @param dates a vector of dates to shift
#' @param index an instance of an object that inherits from the `Index` class.
#' @return a vector of shifted dates
#' @examples
#' library(lubridate)
#' to_reset(ymd(20170101) + days(0:30), AUDBBSW(months(3)))
#' to_value(ymd(20170101) + days(0:30), AUDBBSW(months(3)))
#' to_maturity(ymd(20170101) + days(0:30), AUDBBSW(months(3)))
#' @name indexshifters
#' @export
to_reset <- function(dates, index) UseMethod("to_reset", index)
#' @rdname indexshifters
#' @export
to_value <- function(dates, index) UseMethod("to_value", index)
#' @rdname indexshifters
#' @export
to_maturity <- function(dates, index) UseMethod("to_maturity", index)
#' @rdname indexshifters
#' @export
to_reset.default <- function(dates, index) {
  fmdates::shift(dates, -index$spot_lag,
    index$day_convention, index$calendar, index$is_eom)
}
#' @rdname indexshifters
#' @export
to_value.default <- function(dates, index) {
  fmdates::shift(dates, index$spot_lag,
    index$day_convention, index$calendar, index$is_eom)
}
#' @rdname indexshifters
#' @export
to_maturity.default <- function(dates, index) {
  fmdates::shift(dates, index$tenor,
    index$day_convention, c(index$pfc_calendar, index$calendar), index$is_eom)
}


#' Interpolate values from an object
#'
#' @param x the object to interpolate.
#' @param ... other parameters that defines how to interpolate the object
#' @return an interpolated value or set of values
#' @export
#' @family interpolate functions
interpolate <- function(x, ...) UseMethod("interpolate")



#' Interpolate zeros
#'
#' This interpolates zero rates from either a [ZeroCurve][ZeroCurve()] or some
#' other object that contains such an object.
#'
#' @param x the object to interpolate
#' @param at a [Date] vector representing the date at which to interpolate a
#'   value
#' @param compounding a valid [compounding][is_valid_compounding()] string.
#'   Defaults to `NULL` which uses the curve's native compounding basis
#' @param day_basis a valid [day basis][fmdates::is_valid_day_basis()] string.
#'   Defaults to `NULL` which uses the curve's native day basis.
#' @param ... further arguments passed to specific methods
#' @return an [InterestRate][InterestRate()] object of interpolated zero rates
#'   with the `compounnding` and `day_basis` requested.
#' @export
#' @family interpolate functions

interpolate_zeros <- function(x, at, compounding = NULL, day_basis = NULL, ...) {
  UseMethod("interpolate_zeros")
}


#' Interpolate forward rates and discount factors
#'
#' This interpolates forward rates and forward discount factors from either a
#' [ZeroCurve][ZeroCurve()] or some other object that contains such an object.
#'
#' @param x the object to interpolate
#' @param from a [Date] vector representing the start of the forward period
#' @param to a [Date] vector representing the end of the forward period
#' @param ... further arguments passed to specific methods
#' @return `interpolate_dfs` returns a [DiscountFactor][DiscountFactor()] object
#'   of forward discount factors while `interpolate_fwds` returns an
#'   [InterestRate][InterestRate()] object of interpolated simply compounded
#'   forward rates.
#' @export
#' @aliases interpolate_fwds
#' @family interpolate functions
interpolate_dfs <- function(x, from, to, ...) UseMethod("interpolate_dfs")

#' @export
#' @rdname interpolate_dfs
interpolate_fwds <- function(x, from, to, ...) UseMethod("interpolate_fwds")