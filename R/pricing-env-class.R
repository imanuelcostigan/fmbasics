# PricingEnv and subclasses -----------------------------------------------

PricingEnv <- function(zero_curves, fx_rates, reference_date = NULL) {
  validate_PricingEnv(new_PricingEnv(zero_curves, fx_rates, reference_date))
}

new_PricingEnv <- function(zero_curves, fx_rates, reference_date) {
  structure(list(
    reference_date = reference_date %||% zero_curves$curves[[1]]$reference_date,
    zero_curves = zero_curves,
    fx_rates = fx_rates),
    class = "PricingEnv"
  )
}

validate_PricingEnv <- function(x) {
  assertthat::assert_that(
    lubridate::is.Date(x$reference_date),
    is.ZeroCurves(x$zero_curves),
    is.FXRates(x$fx_rates)
  )
  x
}

is.PricingEnv <- function(x) {
  inherits(x, "PricingEnv")
}

format.PricingEnv <- function(x, ...) {
  paste0(
    "<PricingEnv> @ ", x$reference_date, "\n",
    "  Zero Curves: ", paste0(x$zero_curves$names, collapse = ", "), "\n",
    "  FX rates: ", paste0(x$fx_rates$isos, collapse = ", ")
  )
}

print.PricingEnv <- function(x, ...) {
  cat(format(x), "\n")
}

FXRates <- function(isos, rates) {
  validate_FXRates(new_FXRates(isos, rates))
}

new_FXRates <- function(isos, rates) {
  # Store ISOs as an index on which to search
  structure(tibble::tibble(
    isos = isos,
    rates = rates),
    class = c("FXRates", "tbl_df", "tbl", "data.frame")
  )
}

validate_FXRates <- function(x) {
  assertthat::assert_that(
    is.numeric(x$rates),
    is.character(x$isos),
    anyDuplicated(x$isos) == 0,
    length(x$isos) == length(x$rates)
  )
  x
}

is.FXRates <- function(x) {
  inherits(x, "FXRates")
}

#' @importFrom tibble as_tibble
#' @export
as_tibble.FXRates <- function(x) {
  class(x) <- utils::tail(class(x), -2)
  x
}

#' @importFrom tibble tbl_sum
#' @export
tbl_sum.FXRates <- function(x) {
  paste("<FXRates> of length", nrow(x))
}


ZeroCurves <- function(names, curves) {
  validate_ZeroCurves(new_ZeroCurves(names, curves))
}

new_ZeroCurves <- function(names, curves) {
  structure(tibble::tibble(
    names = names,
    curves = curves),
    class = c("ZeroCurves", "tbl_df", "tbl", "data.frame")
  )
}

validate_ZeroCurves <- function(x) {
  assertthat::assert_that(
    is.character(x$names),
    anyDuplicated(x$names) == 0,
    is_atomic_list(x$curves, is.ZeroCurve),
    length(unique(vapply(x$curves, "[[", numeric(1), "reference_date"))) == 1
  )
  x
}

is.ZeroCurves <- function(x) {
  inherits(x, "ZeroCurves")
}

as_tibble.ZeroCurves <- function(x) {
  class(x) <- utils::tail(class(x), -2)
  x
}

tbl_sum.ZeroCurves <- function(x) {
  paste("<ZeroCurves> of length", nrow(x))
}



# PricingEnv methods ------------------------------------------------------


pricing_element <- function(env, type, ...) {
  getter(env[[type]], ...)
}

getter <- function(x, ...) UseMethod("getter")

getter.FXRates <- function(x, iso, ...) {
  res <- x[x$isos == iso, ][["rates"]]
  if(length(res) == 0) return(NA_real_) else return(res)
}

getter.ZeroCurves <- function(x, currency, index, ...) {
  name <- paste0(currency, "_", index)
  selector <- which(x$names == name)
  if (length(selector) == 0) return(NA) else x$curves[[selector]]
}

