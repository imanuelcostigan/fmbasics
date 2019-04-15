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


#' Interpolate (& extrapolates) values from an object: Extending stat::approx()
#'
#' @param x the object to interpolate.
#' @param ... other parameters that defines how to interpolate the object
#' @return an interpolated value or set of values
#' @export
#' @family interpolate functions
interpolate <- function(x, ...) UseMethod("interpolate")

#' Interpolation method for an object of class DiscountFactor
#'
#' @param x DiscountFactor
#' @param dates_out output dates
#'
#' @return interpolated discount factors on dates_out
#' @export
#'
#' @examples
#'
#' df <- fmbasics::DiscountFactor(c(0.95, 0.94, 0.93), ymd(20130101),
#' ymd(20140101, 20150101, 20160101))
#'
#' interpolate(x = df, dates_out = ymd(20150201))
#'
#'
interpolate.DiscountFactor <- function(x, dates_out){
  assertthat::assert_that(lubridate::is.Date(dates_out))

  df_intrp <- exp(Hmisc::approxExtrap(
    as.numeric(x$value_date),
    log(x$value),
    xout = as.numeric(dates_out)
  )$y)

  out_makt_date <- x$market_date[1:length(dates_out)]
  return(DiscountFactor(df_intrp, out_makt_date, dates_out))
}

#' Interpolation method for an object of class InterestRate
#'
#' @param x InterestRate
#' @param dates_in Input dates
#' @param market_date Current market date
#' @param dates_out Output dates
#' @param compounding Compunding Frequency
#' @param day_basis Day basis
#'
#' @return
#' @export
#'
#' @examples
#' ir <- InterestRate(c(0.04, 0.05), c(2, 4), 'act/365')

#' interpolate(ir,dates_in = c(ymd(20100101), ymd(20100303)),
#'  market_date = ymd(20091202), dates_out = ymd(20100201), compounding = c(2,4),
#'  day_basis = "act/365")


interpolate.InterestRate <- function(x, dates_in, market_date,
                                     dates_out, compounding, day_basis){
  if(missing(dates_in))
    stop("An InterestRate requires the dates_in, specified as the date of tenor points")

  if(missing(market_date))
    stop("An InterestRate requires the market_date, specified as current market date")

  if(missing(compounding))
    stop("An InterestRate requires the compounding, expressed numerically.")

  if(missing(day_basis))
    stop("An InterestRate requires valid day_basis.")

  assertthat::assert_that(lubridate::is.Date(dates_in),
                            lubridate::is.Date(dates_out),
                            lubridate::is.Date(market_date),
                            is.double(compounding),
                            fmdates::is_valid_day_basis(day_basis))

  df <- as_DiscountFactor(x,market_date, dates_in)

  df_intrp <- suppressMessages(interpolate(x = df, dates_out = dates_out))

  return(as_InterestRate(df_intrp, compounding, day_basis))

}

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