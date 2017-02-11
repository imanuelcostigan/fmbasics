
make_index <- function(type, ...) {
  assertthat::assert_that(is.character(type))
  structure(list(...), class = c(paste0(type, "Index"), "Index"))
}


#' IborIndex class
#'
#' This can be used to represent IBOR like indices (e.g. LIBOR, BBSW, CDOR)
#' and extends the `Index` class.
#'
#' @param name the name of the index as a string
#' @param currency the currency associated with the index as a
#' [Currency][Currency()] object
#' @param tenor the term of the index as a [period][lubridate::period]
#' @param spot_lag the period between the index's fixing and the start of
#' the index's term
#' @param calendar the calendar used to determine whether the index fixes on a
#' given date as a [Calendar][fmdates::Calendar]
#' @param day_basis the day basis associated with the index (e.g. "act/365")
#' @param day_convention the day convention associated with the index (e.g. "mf")
#' @param is_eom a flag indicating whether or not the maturity date of the index
#' is subject to the end-to-end convention.
#' @return an object of class `IborIndex` that inherits from `Index`
#' @examples
#' library(lubridate)
#' library(fmdates)
#' # 3m AUD BBSW
#' IborIndex("BBSW", AUD(), months(3), days(0), c(AUSYCalendar()),
#'   "act/365", "ms", FALSE)
#' @export

IborIndex <- function(name, currency, tenor, spot_lag, calendar, day_basis,
  day_convention, is_eom) {

  assertthat::assert_that(
    assertthat::is.string(name),
    is.Currency(currency),
    lubridate::is.period(tenor),
    lubridate::is.period(spot_lag),
    fmdates::is.JointCalendar(calendar),
    fmdates::is_valid_day_basis(day_basis),
    fmdates::is_valid_bdc(day_convention),
    assertthat::is.flag(is_eom)
  )

  make_index("Ibor", name = name, currency = currency, tenor = tenor,
    spot_lag = spot_lag, calendar = calendar, pfc_calendar = locale(calendar),
    day_basis = day_basis, day_convention = day_convention, is_eom = is_eom
  )

}


#' CashIndex class
#'
#' This can be used to represent ONIA like indices (e.g. AONIA, FedFunds)
#' and extends the \code{InterestRateIndex} class.
#'
#' @inheritParams IborIndex
#'
#' @return an object of class `CashIndex` that inherits from `Index`
#' @examples
#' library(lubridate)
#' library(fmdates)
#' # RBA cash overnight rate
#' CashIndex("AONIA", AUD(), days(0), c(AUSYCalendar()), "act/365", "f")
#' @export

CashIndex <- function(name, currency, spot_lag, calendar, day_basis,
  day_convention) {

  assertthat::assert_that(
    assertthat::is.string(name),
    is.Currency(currency),
    lubridate::is.period(spot_lag),
    fmdates::is.JointCalendar(calendar),
    fmdates::is_valid_day_basis(day_basis),
    fmdates::is_valid_bdc(day_convention)
  )

  make_index("Cash", name = name, currency = currency,
    tenor = lubridate::days(1), spot_lag = spot_lag, calendar = calendar,
    pfc_calendar = locale(calendar), day_basis = day_basis,
    day_convention = day_convention, is_eom = FALSE
  )

}

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

#' Index class checkers
#'
#' @param x an object
#' @return `TRUE` if object inherits from tested class
#' @examples
#' is.Index(AONIA())
#' is.CashIndex(AONIA())
#' is.IborIndex(AONIA())
#' @name indexcheckers
#' @export
is.Index <- function(x) inherits(x, "Index")
#' @rdname indexcheckers
#' @export
is.IborIndex <- function(x) inherits(x, "IborIndex")
#' @rdname indexcheckers
#' @export
is.CashIndex <- function(x) inherits(x, "CashIndex")

#' @rdname iso
#' @export
iso.IborIndex <- function(x) iso(x$currency)
#' @rdname iso
#' @export
iso.CashIndex <- function(x) iso(x$currency)
#' @export
format.IborIndex <- function(x, ...) {
  paste("<IborIndex>", pretty_format_period(x$tenor), iso(x), x$name)
}
#' @export
format.CashIndex <- function(x, ...) {
  paste("<CashIndex>", x$name)
}
#' @export
print.Index <- function(x, ...) {cat(format(x), "\n"); invisible(x)}


pretty_format_period <- function(x) {
  if (length(x@.Data) == 0)
    return("Period(0)")
  if (x == lubridate::period(0))
    return("0d")
  show <- vector(mode = "character")
  na <- is.na(x)
  show <- paste(x@year, "y ", x@month, "m ", x@day, "d ", x@hour,
    "H ", x@minute, "M ", x@.Data, "S", sep = "")
  rx <- paste0("((", paste0(0, c("y", "m", "d", "H", "M", "S"), collapse = "|"),
    ")\\s*)")
  show <- gsub(rx, "", show)
  show <- trimws(show, "right")
  start <- regexpr("[-1-9]|(0\\.)", show)
  show <- ifelse(start > 0, substr(show, start, nchar(show)), "0S")
  show[na] <- NA
  show
}

# Indices -----------------------------------------------------------------

#' Standard IBOR
#'
#' These functions create commonly used IBOR indices with standard market
#' conventions.
#'
#' The key conventions are tabulated below.
#'
#' \tabular{llllll}{
#' \bold{Creator} \tab \bold{Spot lag (days)} \tab \bold{Fixing calendars} \tab
#' \bold{Day basis} \tab \bold{Day convention} \tab \bold{EOM} \cr
#' AUDBBSW() \tab 0 \tab AUSYCalendar \tab act/365 \tab ms \tab FALSE \cr
#' EURIBOR() \tab 2 \tab EUTACalendar \tab act/360 \tab mf \tab TRUE \cr
#' GBPLIBOR() \tab 0 \tab GBLOCalendar \tab act/365 \tab mf \tab TRUE \cr
#' JPYLIBOR() \tab 2 \tab GBLOCalendar \tab act/360 \tab mf \tab TRUE \cr
#' JPYTIBOR() \tab 2 \tab JPTOCalendar \tab act/365 \tab mf \tab FALSE \cr
#' NZDBKBM() \tab 0 \tab NZWECalendar, NZAUCalendar \tab act/365 \tab mf \tab FALSE \cr
#' USDLIBOR() \tab 2 \tab USNYCalendar, GBLOCalendar \tab act/360 \tab mf \tab TRUE \cr
#' CHFLIBOR() \tab 2 \tab GBLOCalendar \tab act/360 \tab mf \tab TRUE \cr
#' HKDHIBOR() \tab 0 \tab HKHKCalendar \tab act/365 \tab mf \tab FALSE \cr
#' NOKNIBOR() \tab 2 \tab NOOSCalendar \tab act/360 \tab mf \tab FALSE
#' }
#'
#' There are some nuances to this. Sub-1m LIBOR and TIBOR spot lags are zero
#' days (excepting spot-next rates) and use the following day convention and the
#' overnight USDLIBOR index uses both USNYCalendar and GBLOCalendar calendars.
#'
#' @references
#' \href{http://www.afma.com.au/standards/market-conventions/Bank\%20Bill\%20Swap\%20(BBSW)\%20Benchmark\%20Rate\%20Conventions.pdf}{BBSW}
#' \href{http://www.emmi-benchmarks.eu/assets/files/Euribor_tech_features.pdf}{EURIBOR}
#' \href{https://www.theice.com/iba/libor}{ICE LIBOR}
#' \href{http://www.bbalibor.com/technical-aspects/fixing-value-and-maturity}{BBA LIBOR}
#' \href{http://www.jbatibor.or.jp/english/public/pdf/JBA\%20TIBOR\%20Operational\%20RulesE.pdf}{TIBOR}
#' \href{http://www.nzfma.org/includes/download.aspx?ID=130053}{NZD BKBM}
#' \href{http://opnga.ma/conventions}{OpenGamma Interest Rate Instruments and Market Conventions Guide}
#' \href{https://bank.hangseng.com/1/2/rates/hibor}{HKD HIBOR}
#' @name iborindices
#' @aliases AUDBBSW EURIBOR GBPLIBOR JPYLIBOR JPYTIBOR NZDBKBM USDLIBOR
#' CHFLIBOR HKDHIBOR NOKNIBOR
#' @family constructors
NULL

#' @param tenor the tenor of the IBOR index (e.g. \code{months(3)})
#' @rdname iborindices
#' @export
AUDBBSW <- function (tenor) {
  IborIndex("AUDBBSW", AUD(), tenor, lubridate::days(0),
    c(AUSYCalendar()), "act/365", "ms", FALSE)
}

#' @rdname iborindices
#' @export
AUDBBSW1b <- function (tenor) {
  IborIndex("AUDBBSW1b", AUD(), tenor, lubridate::days(1),
    c(AUSYCalendar()), "act/365", "ms", FALSE)
}

#' @rdname iborindices
#' @export
EURIBOR <- function (tenor) {
  IborIndex("EURIBOR", EUR(), tenor, lubridate::days(2),
    c(EUTACalendar()), "act/360", "mf", TRUE)
}

#' @rdname iborindices
#' @export
GBPLIBOR <- function (tenor) {
  if (tenor < months(1)) {
    day_convention <- "f"
  } else {
    day_convention <- "mf"
  }
  IborIndex("GBPLIBOR", GBP(), tenor, lubridate::days(0),
    c(GBLOCalendar()), "act/365", day_convention, TRUE)
}

#' @rdname iborindices
#' @export
JPYLIBOR <- function (tenor) {
  spot_lag <- lubridate::days(2)
  if (tenor < months(1)) {
    if (tenor == lubridate::days(1)) {
      spot_lag <- lubridate::days(0)
    }
    day_convention <- "f"
  } else {
    day_convention <- "mf"
  }
  IborIndex("JPYLIBOR", JPY(), tenor, spot_lag,
    c(GBLOCalendar()), "act/360", day_convention, TRUE)
}

#' @rdname iborindices
#' @export
JPYTIBOR <- function (tenor) {
  spot_lag <- lubridate::days(2)
  if (tenor < months(1)) {
    if (tenor == lubridate::days(1)) {
      spot_lag <- lubridate::days(0)
    }
    day_convention <- "f"
  } else {
    day_convention <- "mf"
  }
  IborIndex("JPYTIBOR", JPY(), tenor, spot_lag,
    c(JPTOCalendar()), "act/365", day_convention, FALSE)
}

#' @rdname iborindices
#' @export
NZDBKBM <- function (tenor) {
  IborIndex("NZDBKBM", NZD(), tenor, lubridate::days(0),
    c(NZAUCalendar(), NZWECalendar()), "act/365", "mf", FALSE)
}

#' @rdname iborindices
#' @export
USDLIBOR <- function (tenor) {
  spot_lag <- lubridate::days(2)
  if (tenor < months(1)) {
    day_convention <- "f"
    if (tenor == lubridate::days(1)){
      spot_lag <- lubridate::days(0)
      calendar <- c(USNYCalendar(), GBLOCalendar())
    } else {
      calendar <- c(GBLOCalendar())
    }
  } else {
    day_convention <- "mf"
    calendar <- c(GBLOCalendar())
  }
  IborIndex("USDLIBOR", USD(), tenor, spot_lag,
    calendar, "act/360", day_convention, TRUE)
}

#' @rdname iborindices
#' @export
CHFLIBOR <- function (tenor) {
  if (tenor < months(1)) {
    if (tenor == lubridate::days(1)){
      # 1d is spot-next rate
      spot_lag <- lubridate::days(2)
    } else {
      spot_lag <- lubridate::days(0)
    }
    day_convention <- "f"
  } else {
    spot_lag <- lubridate::days(2)
    day_convention <- "mf"
  }
  IborIndex("CHFLIBOR", CHF(), tenor, spot_lag, c(GBLOCalendar()),
    "act/360", day_convention, TRUE)
}

CHFLIBOR3mF <- function () {
  IborIndex("CHFLIBOR", CHF(), months(3), lubridate::days(1),
    c(GBLOCalendar()), "act/360", "mf", TRUE)
}

#' @rdname iborindices
#' @export
HKDHIBOR <- function (tenor) {
  # Not much info is provided. Assume day convention follows usual pattern
  if (tenor < months(1)) {
    day_convention <- "f"
  } else {
    day_convention <- "mf"
  }
  IborIndex("HKDHIBOR", HKD(), tenor, lubridate::days(0),
    c(HKHKCalendar()), "act/365", day_convention, FALSE)
}

#' @rdname iborindices
#' @export
NOKNIBOR <- function (tenor) {
  # https://www.fno.no/en/interest-rates/nibor---the-norwegian-interbank-offered-rate/
  # Not much info is provided. Assume day convention follows usual pattern
  if (tenor < months(1)) {
    if (tenor == lubridate::days(1)) {
      # Is tom/next rate
      spot_lag <- lubridate::days(1)
    } else {
      spot_lag <- lubridate::days(2)
    }
    day_convention <- "f"
  } else {
    spot_lag <- lubridate::days(2)
    day_convention <- "mf"
  }
  IborIndex("NOKNIBOR", NOK(), tenor, spot_lag,
    c(NOOSCalendar()), "act/360", day_convention, FALSE)
}

#' Standard ONIA
#'
#' These functions create commonly used ONIA indices with standard market
#' conventions.
#'
#' The key conventions are tabulated below. All have a zero day spot lag excepting
#' \code{CHFTOIS} which has a one day lag (it is a tom-next rate, per 2006 ISDA
#' definitions).
#'
#' \tabular{llllll}{
#' \bold{Creator} \tab \bold{Fixing calendars} \tab
#' \bold{Day basis} \tab \bold{Day convention} \cr
#' AONIA() \tab AUSYCalendar \tab act/365 \tab f \cr
#' EONIA() \tab EUTACalendar \tab act/360 \tab f \cr
#' SONIA() \tab GBLOCalendar \tab act/365 \tab f \cr
#' TONAR() \tab JPTOCalendar \tab act/365 \tab f \cr
#' NZIONA() \tab NZWECalendar, NZAUCalendar \tab act/365 \tab f \cr
#' FedFunds() \tab USNYCalendar \tab act/360 \tab f \cr
#' CHFTOIS() \tab CHZHCalendar \tab act/360 \tab f \cr
#' HONIX() \tab HKHKCalendar \tab act/365 \tab f \cr
#' }
#'
#' Note that for some ONIA indices, the overnight rate is not published until
#' the following date (i.e. it has publication lag of one day).
#'
#' @references
#' \href{http://www.rba.gov.au/mkt-operations/resources/cash-rate-methodology/}{AONIA}
#' \href{http://www.emmi-benchmarks.eu/assets/files/Eonia\%20Technical\%20Features.pdf}{EONIA}
#' \href{https://www.wmba.org.uk/pages/index.cfm?page_id=31}{SONIA}
#' \href{https://www.boj.or.jp/en/statistics/market/short/mutan/index.htm/}{TONAR}
#' \href{http://rbnz.govt.nz/statistics/tables/b2/}{NZIONA}
#' \href{http://www.federalreserve.gov/releases/H15/Current/#fn2}{FedFunds}
#' \href{http://opnga.ma/conventions}{OpenGamma Interest Rate Instruments and Market Conventions Guide}
#' @name oniaindices
#' @aliases AONIA EONIA SONIA TONAR NZIONA FedFunds CHFTOIS HONIX
#' @family constructors
NULL

#' @rdname oniaindices
#' @export
AONIA <- function () {
  CashIndex("AONIA", AUD(), lubridate::days(0),
    c(AUSYCalendar()), "act/365", "f")
}

#' @rdname oniaindices
#' @export
EONIA <- function () {
  CashIndex("EONIA", EUR(), lubridate::days(0),
    c(EUTACalendar()), "act/360", "f")
}

#' @rdname oniaindices
#' @export
SONIA <- function () {
  CashIndex("SONIA", GBP(), lubridate::days(0),
    c(GBLOCalendar()), "act/365", "f")
}

#' @rdname oniaindices
#' @export
TONAR <- function () {
  CashIndex("TONAR", JPY(), lubridate::days(0),
    c(JPTOCalendar()), "act/365", "f")
}

#' @rdname oniaindices
#' @export
NZIONA <- function () {
  CashIndex("NZIONA", NZD(), lubridate::days(0),
    c(NZAUCalendar(), NZWECalendar()),
    "act/365", "f")
}

#' @rdname oniaindices
#' @export
FedFunds <- function () {
  CashIndex("FedFunds", USD(), lubridate::days(0),
    c(USNYCalendar()), "act/360", "f")
}

#' @rdname oniaindices
#' @export
CHFTOIS <- function () {
  CashIndex("CHFTOIS", CHF(), lubridate::days(1),
    c(CHZHCalendar()), "act/360", "f")
}

#' @rdname oniaindices
#' @export
HONIX <- function () {
  CashIndex("HONIX", HKD(), lubridate::days(0),
    c(HKHKCalendar()), "act/365", "f")
}
