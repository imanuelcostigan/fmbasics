# SingleCurrencyMoney -----------------------------------------------------

#' SingleCurrencyMoney
#'
#' This class associates a numeric vector with a currency. This is useful for
#' example in representing the value of a derivative. You can concatenate a set
#' `SingleCurrencyMoney` objects and return a
#' [MultiCurrencyMoney][MultiCurrencyMoney()] object (see example below)
#'
#' @param value a numeric vector of values
#' @param currency a single [Currency][Currency()] object
#' @return a `SingleCurrencyMoney` object
#' @export
#'
#' @examples
#' SingleCurrencyMoney(1:5, AUD())
#' c(SingleCurrencyMoney(1, AUD()), SingleCurrencyMoney(100, USD()))
#' @family money functions
SingleCurrencyMoney <- function(value, currency) {
  validate_SingleCurrencyMoney(new_SingleCurrencyMoney(value, currency))
}

new_SingleCurrencyMoney <- function(value, currency) {
  structure(value, currency = currency, class = "SingleCurrencyMoney")
}

validate_SingleCurrencyMoney <- function(x) {
  assertthat::assert_that(
    is.numeric(x),
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
#' is.SingleCurrencyMoney(SingleCurrencyMoney(1:5, AUD()))
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


#' @importFrom tibble type_sum
#' @export
type_sum.Currency <- function(x) {
  # Needed to print MultiCurrencyMoney properly
  paste("CCY:", iso(x))
}

#' @export
iso.SingleCurrencyMoney <- function(x) {
  iso(attr(x, "currency"))
}

#' @export
c.SingleCurrencyMoney <- function(...) {
  assertthat::assert_that(is_atomic_list(list(...), is.SingleCurrencyMoney))
  new_MultiCurrencyMoney(list(...))
}

# MultiCurrencyMoney ------------------------------------------------------

#' MultiCurrencyMoney
#'
#' This class associates a vector of numeric values with a list of currencies.
#' This can be useful for example to store value of cash flows. Internally it
#' represents this information as an extension to a [tibble][tibble::tibble()].
#' You are able to bind `MultiCurrencyMoney` objects by using [rbind()] (see
#' example below).
#'
#' @param monies a list of [SingleCurrencyMoney][SingleCurrencyMoney()]
#' @return a `MultiCurrencyMoney` object that extends [tibble::tibble()]
#' @export
#' @examples
#' mcm <- MultiCurrencyMoney(list(
#'   SingleCurrencyMoney(1, AUD()),
#'   SingleCurrencyMoney(2, USD())
#' ))
#' rbind(mcm, mcm)
#' @family money functions

MultiCurrencyMoney <- function(monies) {
  validate_MultiCurrencyMoney(new_MultiCurrencyMoney(monies))
}

new_MultiCurrencyMoney <- function(monies) {
  assertthat::assert_that(is_atomic_list(monies, is.SingleCurrencyMoney))
  values <- vapply(monies, as.numeric, numeric(1), "value", USE.NAMES = FALSE)
  currencies <- lapply(monies, attr, "currency")
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
    length(x$values) == length(x$currencies) || length(x$currencies) == 1
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
#' is.MultiCurrencyMoney(MultiCurrencyMoney(list(SingleCurrencyMoney(1, AUD()))))
#' @family money functions
is.MultiCurrencyMoney <- function(x) {
  inherits(x, "MultiCurrencyMoney")
}

#' @export
as_tibble.MultiCurrencyMoney <- function(x, ...) {
  class(x) <- utils::tail(class(x), -1)
  x
}

#' @importFrom tibble tbl_sum
#' @export
tbl_sum.MultiCurrencyMoney <- function(x) {
  paste("<MultiCurrencyMoney> of length", nrow(x))
}
