#' ISO
#'
#' Get ISO from an object. The default method assumes the ISO can be accessed
#' as if it were an attribute with name `iso` (e.g. `x$iso`). The method for
#' `CurrencyPair` concatenates the ISOs of the constituent currencies (e.g.
#' `iso(AUDUSD())` returns "AUDUSD") while the methods for `CashIndex` and
#' `IborIndex` return the ISO of the index's currency.
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
interpolate <- function(x, ...) UseMethod("interpolate")
