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
  validate_Currency(new_Currency(iso, calendar))
}

new_Currency <- function(iso, calendar) {
  structure(list(iso = toupper(iso), calendar = calendar), class = "Currency")
}

validate_Currency <- function(x) {
  assertthat::assert_that(assertthat::is.string(x$iso), nchar(x$iso) == 3,
    is.JointCalendar(x$calendar))
  x
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
AUD <- function() new_Currency("AUD", c(AUSYCalendar()))
#' @rdname CurrencyConstructors
#' @export
EUR <- function() new_Currency("EUR", c(EUTACalendar()))
#' @rdname CurrencyConstructors
#' @export
GBP <- function() new_Currency("GBP", c(GBLOCalendar()))
#' @rdname CurrencyConstructors
#' @export
JPY <- function() new_Currency("JPY", c(JPTOCalendar()))
#' @rdname CurrencyConstructors
#' @export
NZD <- function() new_Currency("NZD", c(NZAUCalendar(), NZWECalendar()))
#' @rdname CurrencyConstructors
#' @export
USD <- function() new_Currency("USD", c(USNYCalendar()))
#' @rdname CurrencyConstructors
#' @export
CHF <- function() new_Currency("CHF", c(CHZHCalendar()))
#' @rdname CurrencyConstructors
#' @export
HKD <- function() new_Currency("HKD", c(HKHKCalendar()))
#' @rdname CurrencyConstructors
#' @export
NOK <- function() new_Currency("NOK", c(NOOSCalendar()))
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
