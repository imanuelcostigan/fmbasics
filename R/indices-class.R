#' @include date-methods.R
NULL

#' Class \code{"Index"}
#'
#' This class sets the basic structure of all other indices.
#'
#' @section Active bindings:
#'
#' One active binding: \code{name} which either gets or sets the name of the
#' index (e.g. \code{"AUDBBSW"})
#'
#' @docType class
#' @format A \code{\link{R6}} generator object
#' @keywords data
#' @export

Index <- R6::R6Class (
  classname = "Index",
  private = list(
    .name = NA
  ),
  active = list(
    name = function (value) {
      if (missing(value)) {
        private$.name
      } else {
        private$.name <- value
      }
    }
  )
)

#' Class \code{"InterestRateIndex"}
#'
#' This can be used to describe any type of interest rate index. It can be used
#' to describe Ibor indices, or constant maturity swap indices and extends the
#' \code{\link{Index}} class.
#'
#' @section Active bindings:
#' \describe{
#' \item{currency}{Gets currency. Setter is unsupported.}
#' \item{calendar}{Gets calendar. Setter is unsupported.}
#' \item{day_basis}{Gets day basis. Setter is unsupported.}
#' \item{pfc_calendar}{Gets currency's calendar. Setter is unsupported.}
#' \item{spot_lag}{Gets spot lag. Setter is unsupported.}
#' \item{tenor}{Gets tenor. Setter requires a \code{Period} object.}
#' }
#'
#' @section Inherited bindings:
#'
#' One active binding: \code{name} which either gets or sets the name of the
#' index (e.g. \code{"AUDBBSW"})
#'
#' @docType class
#' @format A \code{\link{R6}} generator object
#' @keywords data
#' @export

InterestRateIndex <- R6::R6Class (
  classname = "InterestRateIndex",
  inherit = Index,
  active = list(
    currency = function (value) {
      if (missing(value)) {
        return(private$.currency)
      } else {
        stop("Cannot set currency member.")
      }
    },
    calendar = function (value) {
      if (missing(value)) {
        return(private$.calendar)
      } else {
        stop("Cannot set calendar member.")
      }
    },
    day_basis = function (value) {
      if (missing(value)) {
        return(private$.day_basis)
      } else {
        stop("Cannot set day_basis member.")
      }
    },
    pfc_calendar = function (value) {
      if (missing(value)) {
        return(private$.currency$calendar)
      } else {
        stop("Cannot set the currency's calendar member.")
      }
    },
    spot_lag = function (value) {
      if (missing(value)) {
        return(private$.spot_lag)
      } else {
        stop("Cannot set the spot_lag member.")
      }
    },
    tenor = function (value) {
      if (missing(value)) {
        return(private$.tenor)
      } else {
        assertthat::assert_that(is(value, "Period"))
        private$.tenor <- value
      }
    }
  ),
  private = list(
    .currency = NA,
    .tenor = NA,
    .spot_lag = NA,
    .calendar = NA,
    .day_basis = NA)
)

#' Class \code{"IborIndex"}
#'
#' This can be used to represent IBOR like indices (e.g. LIBOR, BBSW, CDOR)
#' and extends the \code{InterestRateIndex} class.
#'
#' @section Public members:
#' \describe{
#' \item{initialize(name, currency, tenor, spot_lag, calendar, day_basis,
#' day_convention, is_eom)}{Creating a new IBOR index. \code{tenor} must be a
#' \code{Period}}
#' \item{reset_dates(dates)}{Determines the reset date out of the \code{dates}
#' by shifting the \code{dates} back by index's spot lag.}
#' \item{value_dates(dates)}{Determines the value date out of the \code{dates}
#' by shifting the \code{dates} by the index's spot lag.}
#' \item{maturity_dates(dates)}{Determines the maturity date out of the \code{dates}
#' by shifting the \code{dates} by the index's tenor. NB: \code{dates} are
#' assumed to have been shifted by \code{value_dates()}}
#' \item{as.character()}{Represent an Index as a character vector}
#' \item{print()}{Prints an IborIndex object to the REPL}
#' }
#'
#' @section Active bindings:
#' \describe{
#' \item{currency}{Gets currency. Setter is unsupported.}
#' \item{calendar}{Gets calendar. Setter is unsupported.}
#' \item{day_basis}{Gets day basis. Setter is unsupported.}
#' \item{pfc_calendar}{Gets currency's calendar. Setter is unsupported.}
#' \item{spot_lag}{Gets spot lag. Setter is unsupported.}
#' \item{tenor}{Gets tenor. Setter requires a \code{Period} object.}
#' \item{day_convention}{Gets day convention. Setter unsupported.}
#' \item{is_eom}{Gets EOM convention. Setter unsupported.}
#' }
#'
#' @section Inherited bindings:
#'
#' One active binding: \code{name} which either gets or sets the name of the
#' index (e.g. \code{"AUDBBSW"})
#'
#' @docType class
#' @format A \code{\link{R6}} generator object
#' @keywords data
#' @export

IborIndex <- R6::R6Class (
  classname = "IborIndex",
  inherit = InterestRateIndex,
  active = list(
    day_convention = function (value) {
      if (missing(value)) {
        return(private$.day_convention)
      } else {
        stop("Cannot set the day_convention member.")
      }
    },
    is_eom = function (value) {
      if (missing(value)) {
        return(private$.is_eom)
      } else {
        stop("Cannot set the is_eom member.")
      }
    }
  ),
  public = list(
    initialize = function (name, currency, tenor, spot_lag, calendar,
      day_basis, day_convention, is_eom) {
      assertthat::assert_that(is(tenor, "Period"))
      private$.name <- name
      private$.currency <- currency
      private$.tenor <- tenor
      private$.spot_lag <- spot_lag
      private$.calendar <- calendar
      private$.day_basis <- day_basis
      private$.day_convention <- day_convention
      private$.is_eom <- is_eom
      invisible(self)
    },
    reset_dates = function (dates) {
      self$calendar$shift(dates, -private$.spot_lag,
        private$.day_convention, private$.is_eom)
    },
    value_dates = function (dates) {
      self$calendar$shift(dates, private$.spot_lag,
        private$.day_convention, private$.is_eom)
    },
    maturity_dates = function (dates) {
      self$pfc_calendar$shift(dates, private$.tenor,
        private$.day_convention, private$.is_eom)
    },
    as.character = function () {
      paste("<IborIndex>", as.character(private$.tenor), private$.name)
    },
    print = function () {
      cat(self$as.character(), "\n")
    }
  ),
  private = list(
    .day_convention = NA,
    .is_eom = NA)
)

#' Class \code{"CashIndex"}
#'
#' This can be used to represent ONIA like indices (e.g. AONIA, FedFunds)
#' and extends the \code{InterestRateIndex} class.
#'
#' @section Public members:
#' \describe{
#' \item{initialize(name, currency, spot_lag, calendar, day_basis,
#' day_convention)}{Creating a new cash index. The \code{tenor} is set to
#' one day.}
#' \item{reset_dates(dates)}{Determines the reset date out of the \code{dates}
#' by shifting the \code{dates} back by index's spot lag.}
#' \item{value_dates(dates)}{Determines the value date out of the \code{dates}
#' by shifting the \code{dates} by the index's spot lag.}
#' \item{maturity_dates(dates)}{Determines the maturity date out of the \code{dates}
#' by shifting the \code{dates} by the index's tenor. NB: \code{dates} are
#' assumed to have been shifted by \code{value_dates()}}
#' \item{as.character()}{Represent an Index as a character vector}
#' \item{print()}{Prints a CashIndex object to the REPL}
#' }
#'
#' @section Active bindings:
#' \describe{
#' \item{currency}{Gets currency. Setter is unsupported.}
#' \item{calendar}{Gets calendar. Setter is unsupported.}
#' \item{day_basis}{Gets day basis. Setter is unsupported.}
#' \item{pfc_calendar}{Gets currency's calendar. Setter is unsupported.}
#' \item{spot_lag}{Gets spot lag. Setter is unsupported.}
#' \item{tenor}{Gets tenor. Setter requires a \code{Period} object.}
#' \item{day_convention}{Gets day convention. Setter unsupported.}
#' }
#'
#' @section Inherited bindings:
#'
#' One active binding: \code{name} which either gets or sets the name of the
#' index (e.g. \code{"AUDBBSW"})
#'
#' @docType class
#' @format A \code{\link{R6}} generator object
#' @keywords data
#' @export

CashIndex <- R6::R6Class (
  classname = 'CashIndex',
  inherit = InterestRateIndex,
  active = list(
    day_convention = function (value) {
      if (missing(value)) {
        return(private$.day_convention)
      } else {
        stop("Cannot set the day_convention member.")
      }
    }
  ),
  public = list(
    initialize = function (name, currency, spot_lag, calendar,
      day_basis, day_convention) {
      private$.name <- name
      private$.currency <- currency
      private$.tenor <- lubridate::days(1)
      private$.spot_lag <- spot_lag
      private$.calendar <- calendar
      private$.day_basis <- day_basis
      private$.day_convention <- day_convention
      invisible(self)
    },
    reset_dates = function (dates) {
      self$calendar$shift(dates, -private$.spot_lag,
        private$.day_convention, private$.is_eom)
    },
    value_dates = function (dates) {
      self$calendar$shift(dates, private$.spot_lag,
        private$.day_convention, FALSE)
    },
    maturity_dates = function (dates) {
      self$pfc_calendar$shift(dates, private$.tenor,
        private$.day_convention, FALSE)
    },
    as.character = function () {
      private$.name
    },
    print = function () {
      cat("<CashIndex>", self$as.character(), "\n")
    }
  ),
  private = list(
    .day_convention = NA)
)
