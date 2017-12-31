#' Source supplied financial market data
#'
#' The following files are supplied by the package:
#' * `zerocurve.csv` has four fields for one curve: `start`, `end`, `zeros` and
#' `dfs` representing the start and end dates of the pillar instruments and
#' semi-annually compounded zero coupon rates and discount factors
#' * `zerocurves.csv` has five fields for curve sets associated with two
#' currencies: `start`, `end`, `zeros` and `dfs` as above as well as `name`
#' representing the name of the curve. The name of the curve is specified as
#' `CCY_INDEX` where `CCY` is the ISO code of the curve's currency and `INDEX` is
#' the index associated with the curve.
#'
#' @param file the name of the file containing the data set.
#' @return a `tibble`
#' @examples
#' fmdata_example("zerocurve.csv")
#' @export
#' @family build object helpers

fmdata_example <- function(file) {
  filepath <- system.file("extdata", file, package = "fmbasics")
  df <- utils::read.csv(filepath, header = TRUE, stringsAsFactors = FALSE)
  return(tibble::as_tibble(df))
}

#' Build a `ZeroCurve` from example data set
#'
#' This creates a [`ZeroCurve`][ZeroCurve] object from the example data set
#' `zerocurve.csv`.
#'
#' @param interpolation an `Interpolation` object
#' @return a `ZeroCurve` object using data from `zerocurve.csv`
#' @examples
#' build_zero_curve(LogDFInterpolation())
#' @export
#' @family build object helpers

build_zero_curve <- function(interpolation = NULL) {
  zc_df <- fmdata_example("zerocurve.csv")
  values <- zc_df$dfs
  starts <- as.Date(as.character(zc_df[["start"]]), "%Y%m%d")
  ends <- as.Date(as.character(zc_df[["end"]]), "%Y%m%d")
  dfs <- DiscountFactor(values, starts, ends)
  ZeroCurve(dfs, starts[1], interpolation %||% LogDFInterpolation())
}

`%||%` <- function (x, y) if (is.null(x)) y else x


is_atomic_list <- function(ll, .p) {
  is.list(ll) && all(vapply(ll, .p, logical(1), USE.NAMES = FALSE))
}

assertthat::on_failure(is_atomic_list) <- function(call, env) {
  "Element of the list are not all of the same class"
}