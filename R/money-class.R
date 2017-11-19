
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

new_MultiCurrencyMoney <- function(...) {
  structure(list(...), class = "MultiCurrencyMoney")
}

validate_MultiCurrencyMoney <- function(x) {
  is_atomic_list(unclass(x), is.SingleCurrencyMoney)
  x
}

MultiCurrencyMoney <- function(...) {
  validate_MultiCurrencyMoney(new_MultiCurrencyMoney(...))
}

is.MultiCurrencyMoney <- function(x) {
  inherits(x, "MultiCurrencyMoney")
}

format.MultiCurrencyMoney <- function(x, ...) {
  paste0("<MultiCurrencyMoney>\n", paste0("  ",
    vapply(unclass(x), format, character(1), USE.NAMES = FALSE), collapse = "\n")
  )
}

print.MultiCurrencyMoney <- function(x, ...) {
  cat(format(x), "\n")
}