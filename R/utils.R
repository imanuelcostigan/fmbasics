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
  filepath <- system.file("extdata", "zerocurve.csv", package = "fmbasics")
  df <- utils::read.csv(filepath, header = TRUE, stringsAsFactors = FALSE)
  zc_df <- tibble::as_tibble(df)
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