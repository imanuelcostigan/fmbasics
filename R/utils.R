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


#'Build a `VolSurface` from an example date set
#'
#'This creates a [`VolSurface`][`VolSurface`] object from the example data set
#' `volsurface.csv`.
#'
#' @return a `VolSurface` object using data from `volsurface.csv`
#' @examples build_vol_surface()
#' @export
#' @family build object helpers

build_vol_surface <- function(){

  filepath <- system.file("extdata", "volsurface.csv", package = "fmbasics")
  vols <- utils::read.csv(file = filepath, header = T )
  spot <- 97.62
  reference_date <- as.Date("2019-04-26")
  tenors <- as.character(vols$Date)
  strikes <- seq(from = 0.05, to = 2.55, by= 0.05)*spot
  imp_vols <- vols[,-c(1:4) ]/100

  vol_interp <- LinearTimeVarInterpolation()
  vol_quotes <- tibble::tibble(
    maturity = rep(tenors, length(strikes)),
    strike = rep(strikes, each = length(tenors)),
    value  = as.vector(as.matrix.data.frame(imp_vols))
    )

    VolSurface(reference_date, vol_quotes, "RIOASX", vol_interp)

}




















