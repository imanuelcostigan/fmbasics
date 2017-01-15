#' @include currency-class.R
NULL

#' Class \code{'CurrencyPair'}
#'
#' A currency pair is the quotation of the relative value of a currency unit
#' against the unit of another currency in the foreign exchange market.
#' The currency that is used as the reference is called the quote currency and
#' the currency that is quoted in relation (the numeraire) is called the base
#' currency.
#'
#' @section Methods:
#' \describe{
#' \item{\code{initialize(base_ccy, quote_ccy, calendar = NULL)}}{Creates a
#' currency pair from the arguments \code{base_ccy}, \code{quote_ccy} and
#' \code{calendar}. The first two arguments must be \code{\link{Currency}}
#' objects, while \code{calendar} defaults to \code{NULL} and inherits its
#' calendar as the \code{\link{JointCalendar}} of the \code{base_ccy} and
#' \code{quote_ccy} calendars.  Note that USNY calendars are stripped out from
#' the resulting \code{calendar} and that a pair is has T+1 spot days if it
#' includes one of the currencies documented in the \code{is_t1} method.
#' Otherwise it has T=2 spot days.}
#' \item{\code{iso()}}{return ISO code of currency pair and is used by the
#' \code{print} method}
#' \item{\code{is_t1()}}{Returns TRUE if the currency pair settles one good day
#' after trade. At the moment no supported currency is a T1 currency. However,
#' the pairs against the following currencies are T1: CAD, TRY, PHP, RUB, KZT
#' and PKR}
#' \item{\code{spot(dates)}}{The spot dates are usually two non-NYC good day
#' after today. The method \code{is_t1} identifies the pairs whose spot dates
#' are conventionally one good non-NYC day after today. In both cases, if those
#' dates are not a good NYC day, they are rolled to good NYC and non-NYC days
#' using the Following convention.}
#' \item{\code{spot_next(dates)}}{The spot next dates are one good NYC and
#' non-NYC day after spot rolled using the Following convention if necessary.}
#' \item{\code{forward(dates)}}{Forward dates are determined using the
#' calendar's \code{shift()} method rolling bad NYC and non-NYC days using the
#' Following convention. The end-to-end convention applies.}
#' \item{\code{today(dates)}}{Today is simply dates which are good NYC and
#' non-NYC days. Otherwise today is undefined and returns \code{NA}.}
#' \item{\code{tomorrow(dates, tenor = 'spot')}}{Tomorrow is one good NYC and
#' non-NYC day except where that is on or after spot. In that case, is is
#' undefined and returns \code{NA}.}
#' \item{\code{value_dates(dates)}}{Determine common value dates. The supported
#' value date \code{tenors} are: "spot", "spot_next", "today", "tomorrow" and
#' the usual "forward" dates (e.g. \code{lubridate::months(3)}).}
#' \item{\code{invert()}}{Inverts the currency pair and returns new
#' \code{CurrencyPair} object. Does not mutate instance against which this method
#' is called.}
#' }
#'
#' @section Active methods:
#' There are two active methods: \code{base_ccy(value)} and \code{quote_ccy(value)}
#' which return the base and quote currency respectively when \code{value} is
#' missing or otherwise set the fields. When the setter is called, the resulting
#' currency pair's calendar is the joint calendar of the base and quote
#' currency (with any New York calendars removed).
#'
#' @section Fields:
#' There are no public fields.
#'
#' @examples
#' CurrencyPair$new(AUD(), USD())
#' @references
#' TDG Practitioner Guide: Forex Market, Reddi & Yesuthasen, 2009
#' @docType class
#' @format A \code{\link{R6Class}} generator object
#' @keywords data
#' @export

CurrencyPair <- R6::R6Class (
  classname = 'CurrencyPair',
  public = list(
    initialize = function (base_ccy, quote_ccy, calendar = NULL) {
      assertthat::assert_that(is(base_ccy, "Currency"),
        is(quote_ccy, "Currency"))
      private$.base_ccy <- base_ccy
      private$.quote_ccy <- quote_ccy
      if (!is.null(calendar)) {
        assertthat::assert_that(is(calendar, "JointCalendar"))
        private$.calendar <- calendar
      } else {
        private$.calendar <- c(base_ccy$calendar, quote_ccy$calendar)
      }
      private$.remove_usny()
      invisible(self)
    },
    iso = function () {
      paste0(private$.base_ccy$iso, private$.quote_ccy$iso)
    },
    print = function () {
      cat("<CurrencyPair>", self$iso(), "\n")
    },
    is_t1 = function () {
      t1_ccy <- c('cad', 'try', 'php', 'rub', 'kzt', 'pkr')
      t1_pair <- paste0('usd', t1_ccy)
      t1_pair <- c(t1_pair, paste0(t1_ccy, 'usd'))
      self$iso() %in% t1_pair
    },
    spot = function (dates) {
      delay <- if (self$is_t1()) lubridate::days(1) else lubridate::days(2)
      spot <- private$.calendar$shift(dates, delay, 'f', FALSE)
      private$.add_usny()
      res <- private$.calendar$shift(spot, lubridate::days(0), 'f', FALSE)
      private$.remove_usny()
      res
    },
    spot_next = function (dates) {
      private$.add_usny()
      res <- private$.calendar$shift(self$spot(dates), lubridate::days(1),
        'f', FALSE)
      private$.remove_usny()
      res
    },
    forward = function (dates, period) {
      private$.add_usny()
      res <- private$.calendar$shift(self$spot(dates), period, 'f', TRUE)
      private$.remove_usny()
      res
    },
    today = function (dates) {
      dates[!(private$.calendar$is_good(dates))] <- NA
      dates
    },
    tomorrow = function (dates) {
      tom <- private$.calendar$shift(dates, lubridate::days(1), 'f', FALSE)
      tom[!(tom < self$spot(dates))] <- NA
      tom
    },
    value_dates = function (dates, tenor = 'spot') {
      if (identical(tenor, 'spot'))
        self$spot(dates)
      else if (identical(tenor, 'spot_next'))
        self$spot_next(dates)
      else if (identical(tenor, 'today'))
        self$today(dates)
      else if(identical(tenor, 'tomorrow'))
        self$tomorrow(dates)
      else
        self$forward(dates, tenor)
    },
    invert = function () {
      CurrencyPair$new(self$quote_ccy, self$base_ccy, private$.calendar)
    }
  ),
  active = list(
    base_ccy = function (value) {
      if (missing(value)) {
        private$.base_ccy
      } else {
        assertthat::assert_that(is(value, "Currency"),
          value$as.character() != private$.quote_ccy$as.character())
        private$.base_ccy <- value
        private$.calendar <- c(private$.base_ccy$calendar,
          private$.quote_ccy$calendar)
        private$.remove_usny()
      }
    },
    quote_ccy = function (value) {
      if (missing(value)) {
        private$.quote_ccy
      } else {
        assertthat::assert_that(is(value, "Currency"),
          value$as.character() != private$.base_ccy$as.character())
        private$.quote_ccy <- value
        private$.calendar <- c(private$.base_ccy$calendar,
          private$.quote_ccy$calendar)
        private$.remove_usny()
      }
    }
  ),
  private = list(
    .base_ccy = NA,
    .quote_ccy = NA,
    .spot_days = NA,
    .calendar = NA,
    .remove_usny = function () {
      is_usny <- private$.calendar$locales() %in% "USNYCalendar"
      private$.calendar <- private$.calendar[!is_usny]
    },
    .add_usny = function () {
      private$.calendar <- c(USNYCalendar$new(), private$.calendar)
    }
  )
)
