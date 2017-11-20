
new_FXRates <- function(currency_pairs, rates) {
  structure(tibble::tibble(
    rates = rates,
    currency_pairs = currency_pairs),
    class = c("FXRates", "tbl_df", "tbl", "data.frame")
  )
}

validate_FXRates <- function(x) {
  pair_isos <- vapply(x$currency_pairs, iso, character(1), USE.NAMES = FALSE)
  assertthat::assert_that(
    is.numeric(x$rates),
    is_atomic_list(x$currency_pairs, is.CurrencyPair),
    anyDuplicated(pair_isos) == 0,
    length(x$currency_pairs) == length(x$rates)
  )
  x
}

FXRates <- function(currency_pairs, rates) {
  validate_FXRates(new_FXRates(currency_pairs, rates))
}

is.FXRates <- function(x) {
  inherits(x, "FXRates")
}

#' @importFrom tibble tbl_sum
#' @export
tbl_sum.FXRates <- function(x) {
  paste("<FXRates> of length", nrow(x))
}
