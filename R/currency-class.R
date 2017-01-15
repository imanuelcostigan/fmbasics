#' @include date-methods.R
NULL

#' Class \code{"Currency"}
#'
#' A currency refers to money in any form when in actual use or circulation, as
#' a medium of exchange, especially circulating paper money.
#'
#' @section Methods:
#' \describe{
#' \item{initialize(iso, calendar)}{Creates a Currency object with \code{iso}
#' (a string) and \code{calendar} (inherits from \code{\link{JointCalendar}})}
#' \item{as.character()}{A string representation}
#' \item{print()}{Prints the string representation to the console}
#' }
#' @section Active binds:
#' \code{Currency} has two active bindings: \code{iso} and \code{calendar}.
#' The former returns/sets the currency's ISO string while the latter returns/sets
#' the currency's financial centre (as a \code{\link{JointCalendar}}).
#'
#' @references
#' \href{http://en.wikipedia.org/w/index.php?title=Currency&oldid=598027200}{Currency. (2014, March 3). In Wikipedia}
#' \href{http://en.wikipedia.org/wiki/ISO_currency_code}{ISO 4217. (2014, February 25). In Wikipedia}
#'
#' @examples
#' AUD_USER <- Currency$new("AUD", c(AUSYCalendar$new()))
#'
#' @docType class
#' @format A \code{\link{R6Class}} generator object
#' @keywords data
#' @seealso \code{\link{FinancialMarkets}}
#' @export

Currency <- R6::R6Class (
  classname = 'Currency',
  active = list(
    iso = function (value) {
      if (missing(value)) {
        return(private$.iso)
      } else {
        assertthat::assert_that(all(is.character(value)),
          all(nchar(value) == 3),
          all(length(value) == length(private$.calendar)))
        private$.iso <- toupper(value)
      }
    },
    calendar = function (value) {
      if (missing(value)) {
        return(private$.calendar)
      } else {
        assertthat::assert_that(is(value, "JointCalendar"),
          all(length(private$.iso) == length(value)))
        private$.calendar <- value
      }
    }
  ),
  public = list(
    initialize = function (iso, calendar) {
      assertthat::assert_that(
        assertthat::is.string(iso), nchar(iso) == 3,
        is(calendar, "JointCalendar"))
      private$.iso <- toupper(iso)
      private$.calendar <- calendar
    },
    as.character = function () {
      private$.iso
    },
    print = function () {
      cat("<Currency>", self$as.character(), "\n")
    }
  ),
  private = list(
    .iso = NA,
    .calendar = NA)
)

rep_currency <- function (ccy, times, deep = FALSE) {
  ccys <- vector("list", times)
  for (i in seq_along(ccys)) {
    ccys[[i]] <- ccy$clone(deep)
  }
  ccys
}
