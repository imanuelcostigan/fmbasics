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
#' @param interpolation an interpolation object
#' @return a `VolSurface` object using data from `volsurface.csv`
#' @examples build_vol_surface(LinearTimeVarInterpolation())
#' @export
#' @family build object helpers

build_vol_surface <- function(){

  filepath <- system.file("extdata", "volsurface.csv", package = "fmbasics")
  vols <- read.csv(file = filepath, header = T )
  spot <- 97.62
#  vol <- vols_src[which(vols_src$Code == "RIO ASX"), ]
  tenors <- lubridate::ymd(vol$Date)
  strikes <- seq(from = 0.05, to = 2.55, by= 0.05)*spot
  vol_quotes <- vol[,-c(1:4) ]
  colnames(vol_quotes) <- strikes
  rownames(vol_quotes) <- tenors
  vol_quotes <- t(vol_quotes)

  vol_interp <- fmbasics::LinearTimeVarInterpolation()
  vol_obj <- fmbasics::VolSurface(reference_date = reference_date, vol_quotes = vol_quotes,
                                  ticker = "RIOASX", surface_type = "strike/tenor", interpolation = vol_interp)

}




















