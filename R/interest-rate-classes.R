#' @include date-methods.R
NULL

#' DiscountFactor class
#'
#' The \code{DiscountFactor} class is designed to represent discount factors.
#' Discount factor operations and conversion methods have been defined.
#' The generator object is exported.
#'
#' @section Methods:
#' \describe{
#' \item{\code{initialize(df, d1, d2)}}{Checks whether: \code{d1} is less than
#' \code{d1}, elementwise, and that both are Date vectors; and \code{df}
#' is greater than zero and is a numeric vector. An error is thrown if any of
#' these are not true. The elements of each argument are recycled such that
#' each resulting vectors have equivalent lengths.}
#' \item{\code{to_rate(compounding, day_basis)}}{Convert a DiscountFactor to an
#' InterestRate}
#' \item{\code{as.character()}}{Used by \code{show} method to display the discount
#'  factor.}
#' \item{\code{as.numeric()}}{Extracts the discount factor}
#' }
#'
#' @section Active bindings:
#' There are three active bindings: \code{discount_factor}, \code{start_date} and
#' \code{end_date}. Each of these check the validity of the input \code{value}
#' if not missing. If missing, returns the corresponding private field.
#'
#' @section Fields:
#' \code{DiscountFactor} has no public fields.
#'
#' @examples
#' library("lubridate")
#' (df <- DiscountFactor$new(c(0.95, 0.94, 0.93), ymd(20130101), ymd(20140101, 20150101)))
#' df$to_rate(2, "act/365")
#' @docType class
#' @format A \code{\link{R6Class}} generator object
#' @keywords data
#' @export


DiscountFactor <- R6::R6Class (
  classname = 'DiscountFactor',
  active = list(
    discount_factor = function (value) {
      if (missing(value)) {
        return(private$.discount_factor)
      } else {
        assertthat::assert_that(all(is.numeric(value)))
        private$.discount_factor <- value
      }
    },
    start_date = function (value) {
      if (missing(value)) {
        return(private$.start_date)
      } else {
        assertthat::assert_that(all(value <= private$.end_date))
        private$.start_date <- value
      }
    },
    end_date = function (value) {
      if (missing(value)) {
        return(private$.end_date)
      } else {
        assertthat::assert_that(all(value >= private$.start_date))
        private$.end_date <- value
      }
    }
  ),
  public = list(
    initialize = function (df, d1, d2) {
      assertthat::assert_that(
        all(is.numeric(df)),
        all(lubridate::is.Date(d1)),
        all(lubridate::is.Date(d2)),
        all(df > 0), all(d1 <= d2))
      n <- max(NROW(df), NROW(d1), NROW(d2))
      private$.discount_factor <- rep(df, length.out = n)
      private$.start_date <- rep(d1, length.out = n)
      private$.end_date <- rep(d2, length.out = n)
      invisible(self)
    },
    to_rate = function (compounding, day_basis) {
      term <- year_frac(private$.start_date, private$.end_date, day_basis)
      is_cc <- compounding == 1000
      is_simple <- compounding == 0
      is_tbill <- compounding == -1
      is_pc <- !(is_cc | is_simple | is_tbill)
      rate <- vector("numeric", NROW(private$.discount_factor))
      rate[is_cc] <- -log(private$.discount_factor) / term
      rate[is_simple] <- (1 / private$.discount_factor - 1) / term
      rate[is_tbill] <- (1 - private$.discount_factor) / term
      rate[is_pc] <- compounding *
        ((1 / private$.discount_factor) ^ (1 / (compounding * term)) - 1)
      InterestRate$new(rate, compounding, day_basis)
    },
    as.character = function () {
      paste0('<DiscountFactor>\n', private$.discount_factor, ', ',
        private$.start_date, '--', private$.end_date, collapse = '\n')
    },
    as.numeric = function () {
      private$.discount_factor
    },
    print = function() {
      cat(self$as.character(), '\n')
    }
  ),
  private = list(
    .discount_factor = NA,
    .start_date = NA,
    .end_date = NA
  )
)


#' InterestRate class
#'
#' The \code{InterestRate} class is designed to represent interest
#' rates. Interest rate operations and conversion methods have been defined.
#' The generator object is exported.
#'
#' @section Methods:
#' \describe{
#' \item{\code{initialize(rate, compounding, day_basis)}}{Checks whether: the
#' \code{day_basis} is valid; and the \code{compounding} is valid. An error is
#' thrown if any of these are not true. The elements of each argument are
#' recycled such that each resulting vectors have equivalent lengths.}
#' \item{\code{to_df(d1, d2)}}{Convert an \code{InterestRate} to a
#' \code{DiscountFactor} starting and ending at dates \code{d1} and \code{d2}
#' respectively.}
#' \item{\code{convert(compounding = NULL, day_basis = NULL)}}{Convert one
#' InterestRate into another basis or compounding frequency}
#' \item{\code{equivalent_rate(base_rate)}}{Determine the equivalent rate from
#' \code{base_rate}}
#' \item{\code{as.character()}}{Used by \code{show} method to display the
#' discount factor.}
#' \item{\code{as.numeric()}}{Extracts the rate}
#' }
#'
#' @section Active bindings:
#' There are three active bindings: \code{rate}, \code{day_basis} and
#' \code{compounding}. Each of these check the validity of the input \code{value}
#' if not missing. If missing, returns the corresponding private field.
#'
#' @section Fields:
#' \code{DiscountFactor} has no public fields.
#'
#' @examples
#' library (lubridate)
#' rate <- InterestRate$new(0.04, 2, 'act/365')
#' rate$to_df(ymd(20140101), ymd(20150101))
#' rate$convert(compounding=4, day_basis='act/365')
#' InterestRate$new(c(0.04, 0.05), c(2, 4), 'act/365')
#' @docType class
#' @format A \code{\link{R6Class}} generator object
#' @keywords data
#' @export

InterestRate <- R6::R6Class (
  classname = "InterestRate",
  active = list(
    rate = function (value) {
      if (missing(value)) {
        return(private$.rate)
      } else {
        assertthat::assert_that(all(is.numeric(value)))
        private$.rate <- value
      }
    },
    day_basis = function (value) {
      if (missing(value)) {
        return(private$.day_basis)
      } else {
        assertthat::assert_that(is_valid_day_basis(value))
        private$.day_basis <- value
      }
    },
    compounding = function (value) {
      if (missing(value)) {
        return(private$.compounding)
      } else {
        assertthat::assert_that(is_valid_compounding(value))
        private$.compounding <- value
      }
    }
  ),
  public = list(
    initialize = function (rate, compounding, day_basis) {
      assertthat::assert_that(
        all(is.numeric(rate)),
        is_valid_day_basis(day_basis),
        is_valid_compounding(compounding))
      n <- max(NROW(rate), NROW(day_basis), NROW(compounding))
      private$.rate <- rep_len(rate, n)
      private$.day_basis <- rep_len(day_basis, n)
      private$.compounding <- rep_len(compounding, n)
      invisible(self)
    },
    to_df = function (d1, d2) {
      # year_frac is vectorised
      term <- year_frac(d1, d2, private$.day_basis)
      # determine compounding frequency for each interest_rate value
      is_cc <- private$.compounding == 1000
      is_simple <- private$.compounding == 0
      is_tbill <- private$.compounding == -1
      is_pc <- !(is_cc | is_simple | is_tbill)
      # determine discount factors
      df <- vector("numeric", NROW(private$.rate))
      df[is_cc] <- exp(-private$.rate * term)
      df[is_simple] <- 1 / (1 + private$.rate * term)
      df[is_tbill] <- 1 - private$.rate * term
      df[is_pc] <- 1 / ((1 + private$.rate / private$.compounding) ^
          (private$.compounding * term))
      DiscountFactor$new(df, d1, d2)
    },
    convert = function (compounding = NULL, day_basis = NULL) {
      if (!all(is.null(compounding), is.null(day_basis))) {
        # start and end dates here don't matter.
        df <- self$to_df(lubridate::dmy(01012013), lubridate::dmy(01012014))
        if (!is.null(compounding)) {
          private$.compounding <- rep(compounding, length(private$.compounding))
        } else {
          # day_basis must not be null here because of nested if clauses.
          private$.day_basis <- rep(day_basis, length(private$.day_basis))
        }
        new_rate <- df$to_rate(private$.compounding, private$.day_basis)
        private$.rate <- new_rate$rate
        invisible(self)
      } else {
        invisible(self)
      }
    },
    equivalent_rate = function (base_rate) {
      is_same_day_basis <- private$.day_basis %in% base_rate$day_basis
      is_same_compounding <- private$.compounding %in% base_rate$compounding
      if (!all(is_same_day_basis, is_same_compounding)) {
        needs_conversion <- !is_same_day_basis | !is_same_compounding
        self$convert(base_rate$compounding[needs_conversion],
          base_rate$day_basis[needs_conversion])
      }
      invisible(self)
    },
    as.numeric = function () {
      private$.rate
    },
    as.character = function () {
      rp <- format(private$.rate * 100, nsmall = 5)
      cmp <- compounding_as_string(private$.compounding)
      db <- private$.day_basis
      paste0(rp, "%,", cmp, ",", db, collapse = '\n') %>%
        toupper %>%
        paste0("<InterestRate>\n", .)
    },
    print = function () {
      cat(self$as.character(), '\n')
    }
  ),
  private = list(
    .rate = NA,
    .compounding = NA,
    .day_basis = NA)
)

#' Compounding frequencies
#'
#' A non-exported function that checks whether compounding values frequencies
#' are supported.
#'
#' Valid compounding values are:
#' \tabular{ll}{
#'   \bold{Value}  \tab \bold{Frequency}  \cr
#'   -1            \tab Simply, T-bill discounting    \cr
#'   0             \tab Simply                        \cr
#'   1             \tab Annually                      \cr
#'   2             \tab Semi-annually                 \cr
#'   3             \tab Tri-annually                  \cr
#'   4             \tab Quarterly                     \cr
#'   6             \tab Bi-monthly                    \cr
#'   12            \tab Monthly                       \cr
#'   24            \tab Fortnightly                   \cr
#'   52            \tab Weekly                        \cr
#'   365           \tab Daily                         \cr
#'   1000          \tab Continuously                  \cr
#' }
#'
#' @param compounding a numeric vector representing the compounding
#' frequency
#' @return a flag (\code{TRUE} or \code{FALSE}) if all the supplied compounding
#' frequencies are supported.
#' @aliases compounding

is_valid_compounding <- function (compounding) {
  COMPOUNDINGS <- c(-1, 0, 1, 2, 3, 4, 6, 12, 24, 52, 365, 1000)
  all(compounding %in% COMPOUNDINGS)
}

compounding_as_string <- function (compounding) {
  all_freq <- c(-1, 0, 1, 2, 3, 4, 6, 12, 24, 52, 365, 1000)
  all_string  <- c("SimpleT",
    "Simple",
    "Annual",
    "Semi-annual",
    "Tri-annual",
    "Quarterly",
    "Bi-monthly",
    "Monthly",
    "Fortnightly",
    "Weekly",
    "Daily",
    "Continuous")
  all_string[all_freq %in% compounding]
}
