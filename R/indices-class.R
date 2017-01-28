
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
    fmdates:::is_valid_day_basis(day_basis),
    fmdates:::is_valid_bdc(day_convention),
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
    fmdates:::is_valid_day_basis(day_basis),
    fmdates:::is_valid_bdc(day_convention)
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
#' to_reset(ymd(20170101) + days(0:30), AUDBBSW())
#' to_value(ymd(20170101) + days(0:30), AUDBBSW())
#' to_maturity(ymd(20170101) + days(0:30), AUDBBSW())
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

#' @export
iso.IborIndex <- function(x) iso(x$currency)
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