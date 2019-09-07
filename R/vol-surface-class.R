#' VolSurface class
#'
#' The `VolSurface` class is designed to capture implied volatility information
#' along with information about how to interpolate an implied volatility between nodes.
#'
#' @param vol_quotes object of class [VolQuotes()] containing the volatility
#'   data.
#' @param interpolation Interplation method, given as an object of class
#'   interpolation [Interpolation()]. At this time only the
#'   [LinearCubicTimeVarInterpolation()] is supported. This is a two-dimensional
#'   interpolator that uses linear interpolation in the time dimension and cubic
#'   splines in the smile dimension with the values interpolated being the
#'   square of the implied volatilities. Return values are implied volatilies
#' @return a `VolSurface` object
#' @seealso [interpolate.VolSurface], [build_vol_surface()]
#' @examples
#' build_vol_surface()
#' @export

VolSurface <- function(vol_quotes, interpolation) {
  validate_VolSurface(
    new_VolSurface(vol_quotes, interpolation)
  )
}

new_VolSurface <- function(vol_quotes, interpolation) {
  assertthat::assert_that(
    is.LinearCubicTimeVarInterpolation(interpolation)
  )

  db <- "act/365"
  rd <- attr(vol_quotes, "reference_date")

  f <- function(at) {
    if (is.LinearCubicTimeVarInterpolation(interpolation)) {
      x0 <- fmdates::year_frac(rd, at$maturity, db)
      y0 <- at$smile
      tt <- fmdates::year_frac(rd, vol_quotes$maturity, db)
      tbl <- tibble::tibble(
        x = tt,
        y = vol_quotes$smile,
        z = tt * vol_quotes$value ^ 2
      )
      sqrt(linear_cubic_interp(tbl, x0, y0) / x0)
    }
  }

  structure(
    list(
      vol_quotes = vol_quotes,
      interpolator = f,
      day_basis = db
    ),
    class = "VolSurface"
  )
}


validate_VolSurface <- function(x) {
  assertthat::assert_that(
    is.VolQuotes(x$vol_quotes)
  )
  x
}


#' Inherits from VolSurface
#'
#' Checks whether object inherits from `VolSurface` class
#'
#' @param x an R object
#' @return `TRUE` if `x` inherits from the `VolSurface` class; otherwise `FALSE`
#' @export
is.VolSurface <- function(x) {
  inherits(x, "VolSurface")
}


#' @export
format.VolSurface <- function(x, ...) {
  cat(
    paste0(
      "<VolSurface> @ ",
      format(attr(x$vol_quotes, "reference_date"), "%e %B %Y")
    ), "\n",
    paste0(
      attr(x$vol_quotes, "ticker"), "  ",
      x$interpolation
    )
  )
}

#' @export
print.VolSurface <- function(x, ...) {
  cat(format(x), "\n")
  print(x$vol_quotes)
}


#' VolQuotes class
#'
#' `VolQuotes` class is designed to capture volatility data. Checks that the
#' inputs are of the correct type and stores the values in a [tibble::tibble()].
#'
#' @param maturity Date vector that captures the maturity pillar
#'   points.
#' @param smile numeric vector containing the values of the second dimension
#'   of the volatility surface. The elements of the vector can either contain
#'   the strikes, the moneyness or the delta. The input type is specified in
#'   `type` parameter. Must be the same length as `maturity`
#' @param value numeric vector containing the values of the volatilities. Should
#'   typically be represented as a decimal value (e.g. 30\% should be 0.3) and
#'   must be the same length as `maturity`
#' @param reference_date `Date` that captures the as of date. This is stored as
#'   an attribute to the tibble and can be extracted by calling
#'   `attr(x, "reference_date")`
#' @param type string defining the second dimension of the VolSurface. The
#'   values accepted in `type` parameters are "strike", "delta" and "moneyness.
#'   This is stored as an attribute to the tibble and can be extracted by
#'   calling `attr(x, "type")`
#' @param ticker string that represents the underlying asset. This is stored as
#'   an attribute to the tibble and can be extracted by calling
#'   `attr(x, "ticker")`
#' @return object of class `VolQuotes`
#' @examples
#' pillars <- seq(as.Date("2019-04-26") + 1, by = "month", length.out = 3)
#' VolQuotes(
#'   maturity = rep(pillars, 4),
#'   smile = rep(seq(10, 20, length.out = 4), each = 3),
#'   value = seq(1, 0.1, length.out = 12),
#'   reference_date = as.Date("2019-04-26"),
#'   type = "strike",
#'   ticker = "ABC.AX"
#' )
#' @seealso [VolSurface()], [build_vol_quotes()]
#' @export

VolQuotes <- function(maturity, smile, value, reference_date, type, ticker) {
  validate_VolQuotes(
    new_VolQuotes(maturity, smile, value, reference_date, type, ticker)
  )
}

new_VolQuotes <- function(maturity, smile, value, reference_date, type, ticker,
  ..., sub_class = NULL) {

  tibble::new_tibble(
    x = list(
      maturity = maturity,
      smile = smile,
      value = value
    ),
    nrow = length(maturity),
    reference_date = reference_date,
    type = type,
    ticker = ticker,
    ...,
    class = c(sub_class, "VolQuotes")
  )

}

validate_VolQuotes <- function(x) {
  assertthat::assert_that(
    assertthat::is.date(attr(x, "reference_date")),
    length(x$value) == length(x$maturity),
    length(x$value) == length(x$smile),
    all(lubridate::is.Date(x$maturity)),
    all(is.numeric(x$value)),
    all(is.numeric(x$smile)),
    all(x$value > 0),
    all(x$maturity > attr(x, "reference_date")),
    attr(x, "type") %in% c("strike", "delta", "moneyness"),
    assertthat::is.string(attr(x, "ticker"))
  )
  x
}

#' Inherits from VolQuotes
#'
#' Checks whether the object inherits from `VolQuotes` class
#'
#' @param x an R object
#' @return `TRUE` if `x` inherits from the `VolQuotes` class; otherwise `FALSE`
#' @export
is.VolQuotes <- function(x) {
  inherits(x, "VolQuotes")
}
