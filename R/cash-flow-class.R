#' Create a CashFlow
#'
#' This allows you to create a `CashFlow` object.
#'
#' @param dates a [Date][Date()] vector with either the same length as `monies`
#' or a vector of length one that is recycled
#' @param monies a [MultiCurrencyMoney][MultiCurrencyMoney()] object
#' @return a `CashFlow` object that extends [tibble::tibble()]
#' @export
#' @examples
#' CashFlow(as.Date("2017-11-15"),
#'   MultiCurrencyMoney(list(SingleCurrencyMoney(1, AUD())))
#' )
#' @family money functions
CashFlow <- function(dates, monies) {
  assertthat::assert_that(is.MultiCurrencyMoney(monies))
  validate_CashFlow(new_CashFlow(dates, monies))
}

new_CashFlow <- function(dates, monies) {
  structure(tibble::add_column(
    monies,
    dates = dates,
    .before = 1),
    class = c("CashFlow", "tbl_df", "tbl", "data.frame")
  )
}

validate_CashFlow <- function(x) {
  assertthat::assert_that(
    lubridate::is.Date(x$dates),
    length(x$dates) == length(x$values) || length(x$dates) == 1
  )
  x
}


#' Inherits from CashFlow
#'
#' Checks whether object inherits from `CashFlow` class
#'
#' @param x an R object
#' @return `TRUE` if `x` inherits from the `CashFlow` class; otherwise `FALSE`
#' @export
#' @examples
#' is.CashFlow(CashFlow(as.Date("2017-11-15"),
#'   MultiCurrencyMoney(list(SingleCurrencyMoney(1, AUD())))))
#' @family money functions
is.CashFlow <- function(x) {
  inherits(x, "CashFlow")
}

#' @export
as_tibble.CashFlow <- function(x, ...) {
  class(x) <- utils::tail(class(x), -1)
  x
}

#' @importFrom tibble tbl_sum
#' @export
tbl_sum.CashFlow <- function(x) {
  paste("<CashFlow> of length", nrow(x))
}
