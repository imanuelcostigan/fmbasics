#' DiscountFactor class
#'
#' The `DiscountFactor` class is designed to represent discount factors. Checks
#' whether: `d1` is less than `d2`, elementwise, and that both are `Date`
#' vectors; and `value` is greater than zero and is a numeric vector. An error
#' is thrown if any of these are not true. The elements of each argument are
#' recycled such that each resulting vectors have equivalent lengths.
#'
#' @param value a numeric vector containing discount factor values. Must be
#'   greater than zero
#' @param d1 a `Date` vector containing the as of date
#' @param d2 a `Date` vector containing the date to which the discount factor
#'   applies
#' @return a (vectorised) `DiscountFactor` object
#' @examples
#' library("lubridate")
#' df <- DiscountFactor(c(0.95, 0.94, 0.93),
#'         ymd(20130101), ymd(20140101, 20150101))
#' as_InterestRate(df, 2, "act/365")
#' @export

DiscountFactor <- function(value, d1, d2) {
  assertthat::assert_that(
    all(is.numeric(value)),
    all(lubridate::is.Date(d1)),
    all(lubridate::is.Date(d2)),
    all(value > 0), all(d1 <= d2)
  )
  n <- max(NROW(value), NROW(d1), NROW(d2))
  structure(list(
    value = rep(value, length.out = n),
    start_date = rep(d1, length.out = n),
    end_date = rep(d2, length.out = n)),
    class = "DiscountFactor"
  )
}

#' InterestRate class
#'
#' The `InterestRate` class is designed to represent interest rates. Checks
#' whether: the `day_basis` is valid; and the `compounding` is valid. An error
#' is thrown if any of these are not true. The elements of each argument are
#' recycled such that each resulting vectors have equivalent lengths.
#'
#' @param value a numeric vector containing interest rate values (as decimals).
#' @param compounding a numeric vector representing the [compounding] frequency.
#' @param day_basis a character vector representing the day basis associated
#'   with the interest rate (see [fmdates::year_frac()])
#' @return a vectorised `InterestRate` object
#' @examples
#' library("lubridate")
#' InterestRate(c(0.04, 0.05), c(2, 4), 'act/365')
#' rate <- InterestRate(0.04, 2, 'act/365')
#' as_DiscountFactor(rate, ymd(20140101), ymd(20150101))
#' as_InterestRate(rate, compounding = 4, day_basis = 'act/365')
#' @export

InterestRate <- function(value, compounding, day_basis) {
  assertthat::assert_that(
    all(is.numeric(value)),
    fmdates::is_valid_day_basis(day_basis),
    is_valid_compounding(compounding)
  )
  n <- max(NROW(value), NROW(day_basis), NROW(compounding))
  structure(list(
    value = rep_len(value, n),
    day_basis = rep_len(day_basis, n),
    compounding = rep_len(compounding, n)),
    class = "InterestRate"
  )
}

#' Coerce to InterestRate
#'
#' You can coerce objects to the `InterestRate` class using this method.
#'
#' @param x object to coerce
#' @param ... other parameters passed to methods
#' @return an `InterestRate` object
#' @examples
#' library("lubridate")
#' as_InterestRate(DiscountFactor(0.95, ymd(20130101), ymd(20140101)),
#'   compounding = 2, day_basis = "act/365")
#' as_InterestRate(InterestRate(c(0.04, 0.05), c(2, 4), 'act/365'),
#'   compounding = 4, day_basis = 'act/365')
#' @export
as_InterestRate <- function(x, ...) UseMethod("as_InterestRate")

#' @inheritParams InterestRate
#' @rdname as_InterestRate
#' @export
as_InterestRate.DiscountFactor <- function(x, compounding, day_basis, ...) {
  term <- fmdates::year_frac(x$start_date, x$end_date, day_basis)
  is_cc <- is.infinite(compounding)
  is_simple <- compounding == 0
  is_tbill <- compounding == -1
  is_pc <- !(is_cc | is_simple | is_tbill)
  rate <- vector("numeric", NROW(x$value))
  rate[is_cc] <- -log(x$value) / term
  rate[is_simple] <- (1 / x$value - 1) / term
  rate[is_tbill] <- (1 - x$value) / term
  rate[is_pc] <- compounding *
    ((1 / x$value) ^ (1 / (compounding * term)) - 1)
  InterestRate(rate, compounding, day_basis)
}

#' @inheritParams InterestRate
#' @rdname as_InterestRate
#' @export
as_InterestRate.InterestRate <- function(x, compounding = NULL, day_basis = NULL, ...) {
  if (!all(is.null(compounding), is.null(day_basis))) {
    # start and end dates here don't matter.
    df <- as_DiscountFactor(x, as.Date("2013-01-01"), as.Date("2014-01-01"))
    if (!is.null(compounding)) {
      compounding <- rep(compounding, length(x$compounding))
    } else {
      compounding <- x$compounding
    }
    if (!is.null(day_basis)) {
      day_basis <- rep(day_basis, length(x$day_basis))
    } else {
      day_basis <- x$day_basis
    }
    return(as_InterestRate(df, compounding, day_basis))
  } else {
    return(x)
  }
}

#' Coerce to DiscountFactor
#'
#' You can coerce objects to the `DiscountFactor` class using this method.
#'
#' @param x object to coerce
#' @param ... other parameters passed to methods
#' @return a `DiscountFactor` object
#' @examples
#' library("lubridate")
#' as_DiscountFactor(InterestRate(c(0.04, 0.05), c(2, 4), 'act/365'),
#'   ymd(20140101), ymd(20150101))
#' @export
as_DiscountFactor <- function(x, ...) UseMethod("as_DiscountFactor")
#' @inheritParams DiscountFactor
#' @rdname as_DiscountFactor
#' @export
as_DiscountFactor.InterestRate <- function(x, d1, d2, ...) {
  # year_frac is vectorised
  term <- fmdates::year_frac(d1, d2, x$day_basis)
  # determine compounding frequency for each x value
  is_cc <- is.infinite(x$compounding)
  is_simple <- x$compounding == 0
  is_tbill <- x$compounding == -1
  is_pc <- !(is_cc | is_simple | is_tbill)
  # determine discount factors
  df <- vector("numeric", NROW(x$value))
  df[is_cc] <- exp(-x$value * term)
  df[is_simple] <- 1 / (1 + x$value * term)
  df[is_tbill] <- 1 - x$value * term
  df[is_pc] <- 1 / ((1 + x$value / x$compounding) ^
      (x$compounding * term))
  DiscountFactor(df, d1, d2)
}

#' Inherits from InterestRate
#'
#' Checks whether object inherits from `InterestRate` class
#'
#' @param x an R object
#' @return `TRUE` if `x` inherits from the `InterestRate` class; otherwise `FALSE`
#' @examples
#' is.InterestRate(InterestRate(0.04, 2, "act/365"))
#' @export

is.InterestRate <- function(x) inherits(x, "InterestRate")

#' Inherits from DiscountFactor
#'
#' Checks whether object inherits from `DiscountFactor` class
#'
#' @param x an R object
#' @return `TRUE` if `x` inherits from the `DiscountFactor` class; otherwise `FALSE`
#' @examples
#' is.DiscountFactor(DiscountFactor(0.97, Sys.Date(), Sys.Date() + 30))
#' @export

is.DiscountFactor <- function(x) inherits(x, "DiscountFactor")

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
#'   Inf           \tab Continuously                  \cr
#' }
#'
#' @param compounding a numeric vector representing the compounding frequency
#' @return a flag (\code{TRUE} or \code{FALSE}) if all the supplied compounding
#'   frequencies are supported.
#' @aliases compounding

is_valid_compounding <- function(compounding) {
  COMPOUNDINGS <- c(-1, 0, 1, 2, 3, 4, 6, 12, 24, 52, 365, Inf)
  all(compounding %in% COMPOUNDINGS)
}

assertthat::on_failure(is_valid_compounding) <- function (call, env) {
  paste0(eval(deparse(call$compounding)), " is not a valid compounding frequency.")
}

#' @export
as.double.DiscountFactor <- function(x, ...) x$value
#' @export
as.double.InterestRate <- function(x, ...) x$value
#' @export
format.DiscountFactor <- function(x, ...) {
  paste0("<DiscountFactor> ", x$value, ', ',
    x$start_date, '--', x$end_date, collapse = '\n')
}
#' @export
format.InterestRate <- function(x, ...) {
  rp <- format(x$value * 100, nsmall = 5)
  cmp <- compounding_as_string(x$compounding)
  db <- x$day_basis
  paste0("<InterestRate> ", toupper(paste0(rp, "%, ", cmp, ", ", db)),
    collapse = '\n')
}
#' @export
print.DiscountFactor <- function(x, ...) {cat(format(x), "\n"); invisible(x)}
#' @export
print.InterestRate <- function(x, ...) {cat(format(x), "\n"); invisible(x)}

compounding_as_string <- function (compounding) {
  all_freq <- c(-1, 0, 1, 2, 3, 4, 6, 12, 24, 52, 365, Inf)
  all_string  <- c(
    "SimpleT",
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

