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

`%||%` <- function(x, y) if (is.null(x)) y else x


is_atomic_list <- function(ll, .p) {
  is.list(ll) && all(vapply(ll, .p, logical(1), USE.NAMES = FALSE))
}

assertthat::on_failure(is_atomic_list) <- function(call, env) {
  "Element of the list are not all of the same class"
}

#' Build a `VolQuotes` object from an example data set
#'
#' This creates an object of class `VolQuotes` from the example data set
#' `volsurface.csv`.
#'
#' @return a `VolQuotes` object from package built-in data
#' @family build vol object helpers
#' @examples build_vol_quotes()
#' @export
build_vol_quotes <- function() {
  filepath <- system.file("extdata", "volsurface.csv", package = "fmbasics")
  tbl <- readr::read_csv(filepath, col_types = readr::cols(.default = "d"))
  reference_date <- lubridate::ymd(colnames(tbl)[1])
  colnames(tbl)[1] <- "maturity"
  tbl <- tidyr::gather(tbl, key = "smile", value = "value", -1)
  VolQuotes(
    lubridate::ymd(tbl$maturity),
    as.numeric(tbl$smile) / 100,
    tbl$value,
    reference_date,
    "strike",
    "ABC.AX"
  )
}



#' Build a `VolSurface` from an example date set
#'
#' This creates a [`VolSurface`][`VolSurface`] object from the example data set
#' `volsurface.csv`.
#'
#' @return a `VolSurface` object using data from `volsurface.csv`
#' @examples build_vol_surface()
#' @export
#' @family build vol object helpers

build_vol_surface <- function() {
  VolSurface(
    vol_quotes = build_vol_quotes(),
    interpolation = LinearCubicTimeVarInterpolation()
  )
}