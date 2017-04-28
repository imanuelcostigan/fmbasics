#' Source supplied financial market data
#'
#' The following files are supplied by the package:
#' * `zerocurve.csv` has four fields for one curve: `start`, `end`, `zeros` and
#' `dfs` representing the start and end dates of the pillar instruments and
#' semi-annually compounded zero coupon rates and discount factors
#'
#' @param file the name of the file containing the data set.
#' @return a `tibble`
#' @examples
#' fmdata_example("zerocurve.csv")
#' @export
#' @seealso [build_zero_curve()]

fmdata_example <- function(file) {
  filepath <- system.file("extdata", file, package = "fmbasics")
  df <- utils::read.csv(filepath, header = TRUE, stringsAsFactors = FALSE)
  return(tibble::as_tibble(df))
}

#' Build a `ZeroCurve` from example data set
#'
#' This creates a `ZeroCurve` object from the example data set `zerocurve.csv`.
#'
#' @param interpolation an `Interpolation` object
#' @return a `ZeroCurve` object using data from `zerocurve.csv`
#' @examples
#' build_zero_curve(LogDFInterpolation())
#' @export
#' @seealso [fmdata_example()]

build_zero_curve <- function(interpolation = NULL) {
  zc_df <- fmdata_example("zerocurve.csv")
  values <- zc_df$dfs
  starts <- as.Date(as.character(zc_df[["start"]]), "%Y%m%d")
  ends <- as.Date(as.character(zc_df[["end"]]), "%Y%m%d")
  dfs <- DiscountFactor(values, starts, ends)
  ZeroCurve(dfs, starts[1], interpolation %||% LogDFInterpolation())
}

`%||%` <- function (x, y) if (is.null(x)) y else x