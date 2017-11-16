

new_Money <- function(value, currency) {
  structure(value, currency = currency, class = "Money")
}


validate_Money <- function(x) {
  assertthat::assert_that(
    is.numeric(x),
    is.Currency(attr(x, "currency"))
  )
  x
}


Money <- function(value, currency) {
  validate_Money(new_Money(value, currency))
}



format.Money <- function(x, ...) {
  paste("<Money>", attr(x, "currency"),
    paste0(format(unclass(x)), collapse = " "))
}


#' @export
print.Money <- function(x, ...) {
  cat(format(x), "\n")
}
