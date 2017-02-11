#' CurrencyPair class
#'
#' Create an object of class `CurrencyPair`
#'
#' @param base_ccy a [Currency][Currency()] object
#' @param quote_ccy a [Currency][Currency()] object
#' @param calendar a [JointCalendar][JointCalendar()] object. Defaults to
#' `NULL` which sets this to the joint calendar of the two currencies and
#' removes any [USNYCalendar][USNYCalendar()] object to allow currency pair
#' methods to work correctly
#' @return a `CurrencyPair` object
#' @examples
#' CurrencyPair(AUD(), USD())
#' @export

CurrencyPair <- function (base_ccy, quote_ccy, calendar = NULL) {
  assertthat::assert_that(is.Currency(base_ccy), is.Currency(quote_ccy),
    is.null(calendar) || fmdates::is.JointCalendar(calendar))
  if (is.null(calendar)) {
    calendar <- c(base_ccy$calendar, quote_ccy$calendar)
  }
  structure(list(base_ccy = base_ccy, quote_ccy = quote_ccy,
    calendar = remove_usny(calendar)), class = "CurrencyPair")
}

#' CurrencyPair methods
#'
#' A collection of methods related to currency pairs.
#'
#' The methods are sumarised as follows:
#'
#' * `is_t1`: Returns `TRUE` if the currency pair settles one good day after
#' trade. This includes the following currencies crossed with the USD: CAD, TRY,
#' PHP, RUB, KZT and PKR
#' * `to_spot`: The spot dates are usually two non-NY good day after today.
#' `is_t1()` identifies the pairs whose spot dates are conventionally one good
#' non-NYC day after today. In both cases, if those dates are not a good NYC
#' day, they are rolled to good NYC and non-NYC days using the Following
#' convention.
#' * `to_spot_next`: The spot next dates are one good NYC and non-NYC day after
#' spot rolled using the Following convention if necessary.
#' * `to_forward`: Forward dates are determined using the calendar's `shift()`
#' method rolling bad NYC and non-NYC days using the Following convention. The
#' end-to-end convention applies.
#' * `to_today`: Today is simply dates which are good NYC and non-NYC days.
#' Otherwise today is undefined and returns `NA`.
#' * `to_tomorrow`: Tomorrow is one good NYC and non-NYC day except where that
#' is on or after spot. In that case, is is undefined and returns `NA`.
#' * `to_value`: Determine common value dates. The supported value date `tenors`
#' are: "spot", "spot_next", "today", "tomorrow" and the usual
#' "forward" dates (e.g. `lubridate::months(3)`).
#' * `invert`: Inverts the currency pair and returns new `CurrencyPair` object.
#' * `is.CurrencyPair`:  Returns `TRUE` if `x` inherits from the `CurrencyPair`
#' class; otherwise `FALSE`
#'
#' @param x a `CurrencyPair` object
#' @param dates a vector of dates from which forward dates are calculated
#' @param tenor the tenor of the value date which can be one of the following:
#' "spot", "spot_next", "today", "tomorrow" and the usual "forward" dates (e.g.
#' `lubridate::months(3)`)
#' @examples
#' library(lubridate)
#' is_t1(AUDUSD())
#' dts <- lubridate::ymd(20170101) + lubridate::days(0:30)
#' to_spot(dts, AUDUSD())
#' to_spot_next(dts, AUDUSD())
#' to_today(dts, AUDUSD())
#' to_tomorrow(dts, AUDUSD())
#' to_fx_value(dts, months(3), AUDUSD())
#' @name CurrencyPairMethods
NULL

#' @rdname CurrencyPairMethods
#' @export
is_t1 <- function(x) {
  t1_ccy <- c('cad', 'try', 'php', 'rub', 'kzt', 'pkr')
  t1_pair <- paste0('usd', t1_ccy)
  t1_pair <- c(t1_pair, paste0(t1_ccy, 'usd'))
  iso(x) %in% t1_pair
}

#' @rdname CurrencyPairMethods
#' @export
to_spot <- function(dates, x) {
  delay <- if (is_t1(x)) lubridate::days(1) else lubridate::days(2)
  spot <- shift(dates, delay, 'f', x$calendar, FALSE)
  shift(spot, lubridate::days(0), 'f', add_usny(x$calendar), FALSE)
}

#' @rdname CurrencyPairMethods
#' @export
to_spot_next <- function(dates, x) {
  shift(to_spot(dates, x), lubridate::days(1), 'f',
    add_usny(x$calendar), FALSE)
}

#' @rdname CurrencyPairMethods
#' @export
to_forward = function(dates, tenor, x) {
  shift(to_spot(dates, x), tenor, 'f', add_usny(x$calendar), TRUE)
}

#' @rdname CurrencyPairMethods
#' @export
to_today <- function(dates, x) {
  dates[!is_good(dates, x$calendar)] <- NA
  dates
}

#' @rdname CurrencyPairMethods
#' @export
to_tomorrow = function(dates, x) {
  tom <- shift(dates, lubridate::days(1), 'f', x$calendar, FALSE)
  tom[!(tom < to_spot(dates, x))] <- NA
  tom
}

#' @rdname CurrencyPairMethods
#' @export
to_fx_value = function(dates, tenor, x) {
  if (identical(tenor, 'spot'))
    to_spot(dates, x)
  else if (identical(tenor, 'spot_next'))
    to_spot_next(dates, x)
  else if (identical(tenor, 'today'))
    to_today(dates, x)
  else if (identical(tenor, 'tomorrow'))
    to_tomorrow(dates, x)
  else
    to_forward(dates, tenor, x)
}

#' @rdname CurrencyPairMethods
#' @export
invert = function (x) {
  CurrencyPair(x$quote_ccy, x$base_ccy, x$calendar)
}

#' @rdname CurrencyPairMethods
#' @export
is.CurrencyPair <- function(x) inherits(x, "CurrencyPair")
#' @rdname iso
#' @export
iso.CurrencyPair <- function(x) {paste0(iso(x$base_ccy), iso(x$quote_ccy))}
#' @export
locale.CurrencyPair <- function(x) locale(add_usny(x$calendar))
#' @export
format.CurrencyPair <- function(x, ...) {paste("<CurrencyPair>", iso(x))}
#' @export
print.CurrencyPair <- function(x, ...) {cat(format(x), "\n"); invisible(x)}


#' Handy CurrencyPair constructors
#'
#' These handy `CurrencyPair` constructors use their [single currency
#' counterparts][CurrencyConstructors] in the obvious fashion.
#'
#' @examples
#' AUDUSD()
#' @name CurrencyPairConstructors
#' @family constructors
NULL

#' @rdname CurrencyPairConstructors
#' @export
AUDUSD <- function () CurrencyPair(AUD(), USD())
#' @rdname CurrencyPairConstructors
#' @export
EURUSD <- function () CurrencyPair(EUR(), USD())
#' @rdname CurrencyPairConstructors
#' @export
NZDUSD <- function () CurrencyPair(NZD(), USD())
#' @rdname CurrencyPairConstructors
#' @export
GBPUSD <- function () CurrencyPair(GBP(), USD())
#' @rdname CurrencyPairConstructors
#' @export
USDJPY <- function () CurrencyPair(USD(), JPY())
#' @rdname CurrencyPairConstructors
#' @export
GBPJPY <- function () CurrencyPair(GBP(), JPY())
#' @rdname CurrencyPairConstructors
#' @export
EURGBP <- function () CurrencyPair(EUR(), GBP())
#' @rdname CurrencyPairConstructors
#' @export
AUDNZD <- function () CurrencyPair(AUD(), NZD())
#' @rdname CurrencyPairConstructors
#' @export
EURCHF <- function () CurrencyPair(EUR(), CHF())
#' @rdname CurrencyPairConstructors
#' @export
USDCHF <- function () CurrencyPair(USD(), CHF())
#' @rdname CurrencyPairConstructors
#' @export
USDHKD <- function () CurrencyPair(USD(), HKD())
#' @rdname CurrencyPairConstructors
#' @export
EURNOK <- function () CurrencyPair(EUR(), NOK())
#' @rdname CurrencyPairConstructors
#' @export
USDNOK <- function () CurrencyPair(USD(), NOK())


add_usny <- function(calendar) {
  c(USNYCalendar(), calendar)
}

remove_usny <- function(calendar) {
  is_not_usny <- !(locale(calendar) %in% "USNY")
  calendar[is_not_usny]
}

