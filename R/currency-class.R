#' Build a Currency
#'
#' A currency refers to money in any form when in actual use or circulation, as
#' a medium of exchange, especially circulating paper money. This package
#' includes handy constructors for common currencies.
#'
#' @param iso a three letter code representing the currency (see [ISO
#'   4217](https://en.wikipedia.org/wiki/ISO_4217))
#' @param calendar a [JointCalendar][fmdates::JointCalendar()]
#' @return an object of class `Currency`
#'
#' @references
#' \href{http://en.wikipedia.org/w/index.php?title=Currency&oldid=598027200}{Currency. (2014, March 3). In Wikipedia}
#' @examples
#' library("fmdates")
#' Currency("AUD", c(AUSYCalendar()))
#' @seealso [CurrencyConstructors]
#' @export

Currency <- function(iso, calendar) {
  assertthat::assert_that(assertthat::is.string(iso), nchar(iso) == 3,
    is.JointCalendar(calendar))
  structure(list(iso = toupper(iso), calendar = calendar), class = "Currency")
}

#' Handy Currency constructors
#'
#' These constructors use the following conventions:
#'
#' \tabular{ll}{
#' \bold{Creator} \tab \bold{Joint calendars} \cr
#' `AUD()` \tab `AUSYCalendar` \cr
#' `EUR()` \tab `EUTACalendar` \cr
#' `GBP()` \tab `GBLOCalendar` \cr
#' `JPY()` \tab `JPTOCalendar` \cr
#' `NZD()` \tab `NZAUCalendar`, `NZWECalendar` \cr
#' `USD()` \tab `USNYCalendar` \cr
#' `CHF()` \tab `CHZHCalendar` \cr
#' `HKD()` \tab `HKHKCalendar` \cr
#' `NOK()` \tab `NOOSCalendar`
#' }
#'
#' @examples
#' AUD()
#' @name CurrencyConstructors
#' @family constructors
NULL

#' @rdname CurrencyConstructors
#' @export
AUD <- function() Currency("AUD", c(AUSYCalendar()))
#' @rdname CurrencyConstructors
#' @export
EUR <- function() Currency("EUR", c(EUTACalendar()))
#' @rdname CurrencyConstructors
#' @export
GBP <- function() Currency("GBP", c(GBLOCalendar()))
#' @rdname CurrencyConstructors
#' @export
JPY <- function() Currency("JPY", c(JPTOCalendar()))
#' @rdname CurrencyConstructors
#' @export
NZD <- function() Currency("NZD", c(NZAUCalendar(), NZWECalendar()))
#' @rdname CurrencyConstructors
#' @export
USD <- function() Currency("USD", c(USNYCalendar()))
#' @rdname CurrencyConstructors
#' @export
CHF <- function() Currency("CHF", c(CHZHCalendar()))
#' @rdname CurrencyConstructors
#' @export
HKD <- function() Currency("HKD", c(HKHKCalendar()))
#' @rdname CurrencyConstructors
#' @export
NOK <- function() Currency("NOK", c(NOOSCalendar()))


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
#' @export
locale.Currency <- function(x) locale(x$calendar)
#' @export
as.character.Currency <- function(x, ...) iso(x)
#' @export
format.Currency <- function(x, ...) {paste("<Currency>", x$iso)}
#' @export
print.Currency <- function(x, ...) {cat(format(x), "\n"); invisible(x)}

#' Inherits from Currency
#'
#' Checks whether object inherits from `Currency` class
#'
#' @param x an R object
#' @return `TRUE` if `x` inherits from the `Currency` class; otherwise `FALSE`
#' @examples
#' is.Currency(AUD())
#' @export
is.Currency <- function(x) inherits(x, "Currency")
