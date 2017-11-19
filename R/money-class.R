
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

SingleCurrencyMoney <- function(value, currency) {
  validate_SingleCurrencyMoney(new_SingleCurrencyMoney(value, currency))
}

is.SingleCurrencyMoney <- function(x) {
  inherits(x, "SingleCurrencyMoney")
}

format.SingleCurrencyMoney <- function(x, ...) {
  paste("<SingleCurrencyMoney>", attr(x, "currency"),
    paste0(format(unclass(x)), collapse = " "))
}

print.SingleCurrencyMoney <- function(x, ...) {
  cat(format(x), "\n")
}

new_MultiCurrencyMoney <- function(dates, amounts, ccys) {
  structure(tibble::tibble(
    dates = dates,
    amounts = amounts,
    ccys = ccys),
    class = c("MultiCurrencyMoney", "tbl_df", "tbl", "data.frame")
  )
}

validate_MultiCurrencyMoney <- function(x) {
  assertthat::assert_that(
    lubridate::is.Date(x$dates),
    is.numeric(x$amounts),
    is_atomic_list(x$ccys, is.Currency)
  )
  x
}

MultiCurrencyMoney <- function(dates, amounts, ccys) {
  validate_MultiCurrencyMoney(new_MultiCurrencyMoney(dates, amounts, ccys))
}

is.MultiCurrencyMoney <- function(x) {
  inherits(x, "MultiCurrencyMoney")
}

type_sum.Currency <- function(x) {
  paste0("Currency: ", iso(x))
}

obj_sum.MultiCurrencyMoney <- function(x) {
  rep("MCMoney", length(x))
}


# format.MultiCurrencyMoney <- function(x, ...) {
#   paste0("<MultiCurrencyMoney>\n", paste0("  ",
#     vapply(unclass(x), format, character(1), USE.NAMES = FALSE), collapse = "\n")
#   )
# }
#
# print.MultiCurrencyMoney <- function(x, ...) {
#   cat(format(x), "\n")
# }