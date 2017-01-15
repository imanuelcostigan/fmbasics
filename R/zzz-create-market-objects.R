# FX ----------------------------------------------------------------------

#' Standard currency pairs
#'
#' These functions create currency pairs with standard market conventions which
#' are inherited from the constituent currencies.
#'
#' @return A \code{\link{CurrencyPair}} object
#' @examples
#' AUDUSD()
#' USDJPY()
#' @name ccypaircreators
#' @aliases AUDUSD EURUSD NZDUSD GBPUSD USDJPY GBPJPY EURGBP AUDNZD EURCHF USDCHF
#' EURNOK USDNOK
NULL

#' @rdname ccypaircreators
#' @export
AUDUSD <- function () CurrencyPair$new(AUD(), USD())
#' @rdname ccypaircreators
#' @export
EURUSD <- function () CurrencyPair$new(EUR(), USD())
#' @rdname ccypaircreators
#' @export
NZDUSD <- function () CurrencyPair$new(NZD(), USD())
#' @rdname ccypaircreators
#' @export
GBPUSD <- function () CurrencyPair$new(GBP(), USD())
#' @rdname ccypaircreators
#' @export
USDJPY <- function () CurrencyPair$new(USD(), JPY())
#' @rdname ccypaircreators
#' @export
GBPJPY <- function () CurrencyPair$new(GBP(), JPY())
#' @rdname ccypaircreators
#' @export
EURGBP <- function () CurrencyPair$new(EUR(), GBP())
#' @rdname ccypaircreators
#' @export
AUDNZD <- function () CurrencyPair$new(AUD(), NZD())
#' @rdname ccypaircreators
#' @export
EURCHF <- function () CurrencyPair$new(EUR(), CHF())
#' @rdname ccypaircreators
#' @export
USDCHF <- function () CurrencyPair$new(USD(), CHF())
#' @rdname ccypaircreators
#' @export
USDHKD <- function () CurrencyPair$new(USD(), HKD())
#' @rdname ccypaircreators
#' @export
EURNOK <- function () CurrencyPair$new(EUR(), NOK())
#' @rdname ccypaircreators
#' @export
USDNOK <- function () CurrencyPair$new(USD(), NOK())

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
#' \href{http://www.opengamma.com/sites/default/files/interest-rate-instruments-and-market-conventions.pdf}{OpenGamma Interest Rate Instruments and Market Conventions Guide}
#' \href{https://bank.hangseng.com/1/2/rates/hibor}{HKD HIBOR}
#' @name iborindices
#' @aliases AUDBBSW EURIBOR GBPLIBOR JPYLIBOR JPYTIBOR NZDBKBM USDLIBOR
#' CHFLIBOR HKDHIBOR NOKNIBOR
NULL

#' @param tenor the tenor of the IBOR index (e.g. \code{months(3)})
#' @rdname iborindices
#' @export
AUDBBSW <- function (tenor) {
  IborIndex$new("AUDBBSW", AUD(), tenor, lubridate::days(0),
    c(AUSYCalendar$new()), "act/365", "ms", FALSE)
}

#' @rdname iborindices
#' @export
AUDBBSW1b <- function (tenor) {
  IborIndex$new("AUDBBSW1b", AUD(), tenor, lubridate::days(1),
    c(AUSYCalendar$new()), "act/365", "ms", FALSE)
}

#' @rdname iborindices
#' @export
EURIBOR <- function (tenor) {
  IborIndex$new("EURIBOR", EUR(), tenor, lubridate::days(2),
    c(EUTACalendar$new()), "act/360", "mf", TRUE)
}

#' @rdname iborindices
#' @export
GBPLIBOR <- function (tenor) {
  if (tenor < months(1)) {
    day_convention <- "f"
  } else {
    day_convention <- "mf"
  }
  IborIndex$new("GBPLIBOR", GBP(), tenor, lubridate::days(0),
    c(GBLOCalendar$new()), "act/365", day_convention, TRUE)
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
  IborIndex$new("JPYLIBOR", JPY(), tenor, spot_lag,
    c(GBLOCalendar$new()), "act/360", day_convention, TRUE)
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
  IborIndex$new("JPYTIBOR", JPY(), tenor, spot_lag,
    c(JPTOCalendar$new()), "act/365", day_convention, FALSE)
}

#' @rdname iborindices
#' @export
NZDBKBM <- function (tenor) {
  IborIndex$new("NZDBKBM", NZD(), tenor, lubridate::days(0),
    c(NZAUCalendar$new(), NZWECalendar$new()), "act/365", "mf", FALSE)
}

#' @rdname iborindices
#' @export
USDLIBOR <- function (tenor) {
  spot_lag <- lubridate::days(2)
  if (tenor < months(1)) {
    day_convention <- "f"
    if (tenor == lubridate::days(1)){
      spot_lag <- lubridate::days(0)
      calendar <- c(USNYCalendar$new(), GBLOCalendar$new())
    } else {
      calendar <- c(GBLOCalendar$new())
    }
  } else {
    day_convention <- "mf"
    calendar <- c(GBLOCalendar$new())
  }
  IborIndex$new("USDLIBOR", USD(), tenor, spot_lag,
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
  IborIndex$new("CHFLIBOR", CHF(), tenor, spot_lag, c(GBLOCalendar$new()),
    "act/360", day_convention, TRUE)
}

CHFLIBOR3mF <- function () {
  IborIndex$new("CHFLIBOR", CHF(), months(3), lubridate::days(1),
    c(GBLOCalendar$new()), "act/360", "mf", TRUE)
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
  IborIndex$new("HKDHIBOR", HKD(), tenor, lubridate::days(0),
    c(HKHKCalendar$new()), "act/365", day_convention, FALSE)
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
  IborIndex$new("NOKNIBOR", NOK(), tenor, spot_lag,
    c(NOOSCalendar$new()), "act/360", day_convention, FALSE)
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
#' \href{http://www.rba.gov.au/mkt-operations/resources/tech-notes/interbank-survey.html}{AONIA}
#' \href{http://www.emmi-benchmarks.eu/assets/files/Eonia\%20Technical\%20Features.pdf}{EONIA}
#' \href{https://www.wmba.org.uk/pages/index.cfm?page_id=31}{SONIA}
#' \href{https://www.boj.or.jp/en/statistics/market/short/mutan/index.htm/}{TONAR}
#' \href{http://rbnz.govt.nz/statistics/tables/b2/}{NZIONA}
#' \href{http://www.federalreserve.gov/releases/H15/Current/#fn2}{FedFunds}
#' \href{http://www.opengamma.com/sites/default/files/interest-rate-instruments-and-market-conventions.pdf}{OpenGamma Interest Rate Instruments and Market Conventions Guide}
#' @name oniaindices
#' @aliases AONIA EONIA SONIA TONAR NZIONA FedFunds CHFTOIS HONIX
NULL

#' @rdname oniaindices
#' @export
AONIA <- function () {
  CashIndex$new("AONIA", AUD(), lubridate::days(0),
    c(AUSYCalendar$new()), "act/365", "f")
}

#' @rdname oniaindices
#' @export
EONIA <- function () {
  CashIndex$new("EONIA", EUR(), lubridate::days(0),
    c(EUTACalendar$new()), "act/360", "f")
}

#' @rdname oniaindices
#' @export
SONIA <- function () {
  CashIndex$new("SONIA", GBP(), lubridate::days(0),
    c(GBLOCalendar$new()), "act/365", "f")
}

#' @rdname oniaindices
#' @export
TONAR <- function () {
  CashIndex$new("TONAR", JPY(), lubridate::days(0),
    c(JPTOCalendar$new()), "act/365", "f")
}

#' @rdname oniaindices
#' @export
NZIONA <- function () {
  CashIndex$new("NZIONA", NZD(), lubridate::days(0),
    c(NZAUCalendar$new(), NZWECalendar$new()),
    "act/365", "f")
}

#' @rdname oniaindices
#' @export
FedFunds <- function () {
  CashIndex$new("FedFunds", USD(), lubridate::days(0),
    c(USNYCalendar$new()), "act/360", "f")
}

#' @rdname oniaindices
#' @export
CHFTOIS <- function () {
  CashIndex$new("CHFTOIS", CHF(), lubridate::days(1),
    c(CHZHCalendar$new()), "act/360", "f")
}

#' @rdname oniaindices
#' @export
HONIX <- function () {
  CashIndex$new("HONIX", HKD(), lubridate::days(0),
    c(HKHKCalendar$new()), "act/365", "f")
}
