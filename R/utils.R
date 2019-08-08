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
  vol_data <- readr::read_csv(filepath,
    col_names = F,
    col_types = readr::cols(.default = "d", X1 = "c")
  )
  reference_date <- as.Date(x = vol_data$X1[1], format = "%Y%m%d")
  dates <- vol_data$X1[-1]
  strike <- as.numeric(vol_data[1, -1])
  maturities <- as.Date(rep(dates, length(strike)), format = "%Y%m%d")
  strikes <- rep(strike, each = length(dates))
  imp_vols <- as.vector(as.matrix.data.frame(vol_data[-1, -1 ] / 100))
  VolQuotes(reference_date, maturities, strikes, "strike", imp_vols)
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
    reference_date = as.Date("2019-04-26"),
    vol_quotes = build_vol_quotes(),
    ticker = "RIOASX",
    interpolation = LinearCubicTimeVarInterpolation()
  )
}


#' Linear-Cubic Interpolation
#'
#' This function performs a two-dimentional interpolation linear on the first dimension
#' and natural cubic spline on the second dimension.
#' @param interp_data `data.frame` object with three vectors x, y and z
#' @param x0 numeric vector containing the points at which to perform the interpolation along the first dimension
#' @param y0 numeric vector containing the points at which to perform the interpolation along the second dimension
#' @examples vols <- build_vol_quotes()
#'  interp_data <- tibble::tibble(x = fmdates::year_frac(vols$reference_date,
#' as.Date(vols$maturity,"%Y%m%d"), "act/365"),
#' y = vols$smile,
#' z = vols$value)
#' x0 <- c(2.5, 3.5)
#' y0 <- c(22, 55)
#' linear_cubic_interp(interp_data = interp_data, x0 = x0, y0 = y0)
#' @export

linear_cubic_interp <- function(interp_data, x0, y0) {
  assertthat::assert_that(
    length(x0) == length(y0)
  )

  res <- rep(NA, length(x0))

  for (i in seq_along(x0)) {
    xx <- x0[i]
    yy <- y0[i]
    smile <- rep(NA, length(unique(interp_data$y)))

    for (k in 1:length(smile)) {
      g <- stats::approxfun(
        x = unique(interp_data$x),
        y = interp_data$z[(1 + (k - 1) *
          length(unique(interp_data$x))):(k * length(unique(interp_data$x)))],
        method = "linear",
        rule = 2
      )
      smile[k] <- g(xx)
    }
    if (yy >= min(interp_data$y) & yy <= max(interp_data$y)) {
      interpolated_value <- stats::spline(
        x = unique(interp_data$y),
        y = smile, method = "natural", xout = yy
      )$y
    }
    if (yy < min(interp_data$y)) {
      interpolated_value <- smile[1]
    }
    if (yy > max(interp_data$y)) {
      interpolated_value <- utils::tail(smile, 1)
    }
    res[i] <- interpolated_value
  }
  res
}
