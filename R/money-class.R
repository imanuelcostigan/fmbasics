# SingleCurrencyMoney -----------------------------------------------------

#' SingleCurrencyMoney
#'
#' This class associates a numeric value with a currency.
#'
#' @param value a single numeric value
#' @param currency a single [Currency][Currency()] object
#' @return a `SingleCurrencyMoney` object
#' @export
#'
#' @examples
#' SingleCurrencyMoney(100, AUD())
#' @family money functions
SingleCurrencyMoney <- function(value, currency) {
  validate_SingleCurrencyMoney(new_SingleCurrencyMoney(value, currency))
}

new_SingleCurrencyMoney <- function(value, currency) {
  structure(value, currency = currency, class = "SingleCurrencyMoney")
}

validate_SingleCurrencyMoney <- function(x) {
  assertthat::assert_that(
    assertthat::is.number(x),
    is.Currency(attr(x, "currency"))
  )
  x
}

#' Inherits from SingleCurrencyMoney
#'
#' Checks whether object inherits from `SingleCurrencyMoney` class
#'
#' @param x an R object
#' @return `TRUE` if `x` inherits from the `SingleCurrencyMoney` class; otherwise `FALSE`
#' @export
#' @examples
#' is.SingleCurrencyMoney(SingleCurrencyMoney(1, AUD()))
#' @family money functions
is.SingleCurrencyMoney <- function(x) {
  inherits(x, "SingleCurrencyMoney")
}

#' @export
format.SingleCurrencyMoney <- function(x, ...) {
  paste("<SingleCurrencyMoney>", attr(x, "currency"),
    paste0(format(unclass(x)), collapse = " "))
}

#' @export
print.SingleCurrencyMoney <- function(x, ...) {
  cat(format(x), "\n")
}

#' @export
type_sum.Currency <- function(x) {
  # Needed to print MultiCurrencyMoney properly
  paste0("Currency: ", iso(x))
}

#' @export
iso.SingleCurrencyMoney <- function(x) {
  iso(x$currency)
}

# MultiCurrencyMoney ------------------------------------------------------

#' MultiCurrencyMoney
#'
#' This class associated a vector of numeric values with a list of currencies.
#'
#' @param values a vector of numeric values
#' @param currencies a list of [Currency][Currency()] objects with the same
#' length as `values`
#' @return a `MultiCurrencyMoney` object that extends [tibble::tibble()]
#' @export
#' @examples
#' MultiCurrencyMoney(c(100, 200), list(AUD(), USD()))
#' @family money functions

MultiCurrencyMoney <- function(values, currencies) {
  validate_MultiCurrencyMoney(new_MultiCurrencyMoney(values, currencies))
}

new_MultiCurrencyMoney <- function(values, currencies) {
  structure(tibble::tibble(
    values = values,
    currencies = currencies),
    class = c("MultiCurrencyMoney", "tbl_df", "tbl", "data.frame")
  )
}

validate_MultiCurrencyMoney <- function(x) {
  assertthat::assert_that(
    is.numeric(x$values),
    is_atomic_list(x$currencies, is.Currency),
    length(x$values) == length(x$currencies)
  )
  x
}

#' Inherits from MultiCurrencyMoney
#'
#' Checks whether object inherits from `MultiCurrencyMoney` class
#'
#' @param x an R object
#' @return `TRUE` if `x` inherits from the `MultiCurrencyMoney` class; otherwise `FALSE`
#' @export
#' @examples
#' is.MultiCurrencyMoney(MultiCurrencyMoney(1, list(AUD())))
#' @family money functions
is.MultiCurrencyMoney <- function(x) {
  inherits(x, "MultiCurrencyMoney")
}

#' @export
print.MultiCurrencyMoney <- function(x, ...) {
  cat("<MultiCurrencyMoney>\n")
  NextMethod()
}

#' @export
obj_sum.MultiCurrencyMoney <- function(x) {
  rep("MCMoney", length(x))
}
