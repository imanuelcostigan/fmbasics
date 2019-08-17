#' VolSurface class
#'
#' The `VolSurface` class is designed to capture the information about volatilities.
#' Checks whether the data members are of the correct type.
#'
#' @param reference_date `Date` value that represents the as of date.
#' @param vol_quotes object of class [VolQuotes()] containing the volatility
#'   data.
#' @param ticker a string representing the vol object.
#' @param interpolation Interplation method, given as an object of class
#'   interpolation [Interpolation()].
#' @seealso [interpolate.VolSurface], [build_vol_surface()]
#' @examples vol_surface <- build_vol_surface()
#' @export

VolSurface <- function(reference_date, vol_quotes, ticker, interpolation) {
  validate_VolSurface(
    new_VolSurface(reference_date, vol_quotes, ticker, interpolation)
  )
}

new_VolSurface <- function(reference_date, vol_quotes, ticker, interpolation) {
  assertthat::assert_that(
    is.LinearCubicTimeVarInterpolation(interpolation)
  )

  db <- "act/365"

  f <- function(at) {
    if (is.LinearCubicTimeVarInterpolation(interpolation)) {
      x0 <- fmdates::year_frac(reference_date, at$term, db)
      y0 <- at$smile
      tt <- fmdates::year_frac(reference_date, vol_quotes$maturity, db)
      interp_data <- tibble::tibble(
        x = tt,
        y = vol_quotes$smile,
        z = tt * vol_quotes$value ^ 2
      )
      res <- sqrt(linear_cubic_interp(interp_data, x0, y0) / x0)
      return(res)
    }
  }

  structure(
    list(
      reference_date = reference_date,
      vol_quotes = vol_quotes,
      ticker = ticker,
      interpolator = f,
      day_basis = db
    ),
    class = "VolSurface"
  )
}


validate_VolSurface <- function(x) {
  assertthat::assert_that(
    assertthat::is.date(x$reference_date),
    is.VolQuotes(x$vol_quotes),
    x$vol_quotes$reference_date == x$reference_date,
    assertthat::is.string(x$ticker),
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
    paste0("<VolSurface> @", format(x$reference_date, "%e %B %Y")), "\n",
    paste0(x$ticker, "  ", format(x$interpolation))
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
#' inputs are of the correct type.
#'
#' @param reference_date object of class `Date` that captures the as of date.
#' @param maturity  vector of class `Date` that captures the maturity pillar
#'   points.
#' @param smile  `numeric` vector containing the values of the second dimension
#'   of the volatility surface. The elements of the vector can either contain
#'   the strikes, the moneyness or the delta. The input type is specified in
#'   `type` parameter.
#' @param type `character` string defining the second dimension of the
#'   VolSurface. The values accepted in `type` parameters are "strike", "delta"
#'   and "moneyness.
#' @param value `numeric` vector containing the values of the volatilities.
#' @return object of class `VolQuotes`.
#' @examples
#' vq <-  VolQuotes(reference_date = as.Date("2019-04-26"),
#' maturity = rep(seq(as.Date("2019-04-26")+1, by = "month", length.out = 3), 4),
#' smile = rep(seq(10, 20, length.out = 4), each = 3),
#' type = "strike",
#' value = seq(1, 0.1, length.out = 12 ))
#' @seealso [VolSurface()], [build_vol_quotes()]
#' @export

VolQuotes <- function(reference_date, maturity, smile, type, value) {
  validate_VolQuotes(new_VolQuotes(reference_date, maturity, smile, type, value))
}


new_VolQuotes <- function(reference_date, maturity, smile, type, value, ...,
                          sub_class = NULL) {
  n <- NROW(maturity)
  structure(list(
    reference_date = reference_date,
    maturity = maturity,
    smile = smile,
    type = type,
    value = value
  ),
  class = c(sub_class, "VolQuotes")
  )
}


validate_VolQuotes <- function(x) {
  assertthat::assert_that(
    lubridate::is.Date(x$reference_date),
    length(x$value) == length(x$maturity),
    length(x$value) == length(x$smile),
    all(lubridate::is.Date(x$maturity)),
    all(is.numeric(x$value)),
    all(is.numeric(x$smile)),
    all(x$value > 0),
    all(x$maturity > x$reference_date),
    !is.unsorted(unique(x$maturity)),
    !is.unsorted(unique(x$smile)),
    length(x$smile) == length(unique(x$smile)) *
      length(unique(x$maturity)),
    x$type %in% c("strike", "delta", "moneyness")
  )
  x
}


#' @export
format.VolQuotes <- function(x, ...) {
  vol_quotes <- tibble::tibble(
    reference_date = format(x$reference_date, "%Y%m%d"),
    maturity = format(x$maturity, "%Y%m%d"),
    smile = x$smile,
    value = x$value
  )
  colnames(vol_quotes)[3] <- x$type

  vol_quotes
}




#' @export
format.VolQuotes <- function(x, ...) {
  paste0("<VolQuotes> @ ", format(x$reference_date, "%e %B %Y"))
}

#' @export
print.VolQuotes <- function(x, ...) {
  vols_tibble <- tibble::as_tibble(x)

  cat(format(x), "\n")
  print(tibble::as_tibble(x))
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



#' VolQuotes attributes as a data frame
#'
#' Create a `tibble` that contains the pillar point maturities and strikes for
#' VolQuotes object with the corresponding volatility.
#'
#' @param x a `VolQuotes` object
#' @param ... other parameters that are not used by this methods
#' @return a `tibble` that contains the volatility per maturity and strike.
#' @seealso [tibble::tibble()]
#' @importFrom tibble as_tibble
#' @export
as_tibble.VolQuotes <- function(x, ...) {
  vols_tibble <- tibble::tibble(
    maturity = format(x$maturity, "%Y%m%d"),
    smile = x$smile,
    value = x$value
  )
  colnames(vols_tibble)[2] <- x$type

  vols_tibble
}



# VolSurface methods --------------------------------


#' Interpolate a `VolSurface` object.
#'
#' This method is used to interpolate a `VolSurface` object at multiple points of
#' the plane. The interpolation depends on the type of the surface, if the vols are
#' given by strikes, delta, moneyness.
#'
#' @param x object of class `VolSurface` to be interpolated.
#' @param at indicates the coordinates at which the interpolation is performed.
#' `at` should be given as a named list of length two, where the two members are
#' vectors with the same length and are named `term` and `smile`. e.g.
#' list(term = c(1, 2), smile = c(72, 92)).
#' @param ... unused in this model.
#' @return `numeric` vector with length equal to the length of `at` members.
#' @examples vol_surface <- build_vol_surface()
#' interpolation_points <- list(term = c(as.Date("2020-03-31"), as.Date("2021-03-31")),
#' smile = c(40, 80))
#' implied_vols <- interpolate(x = vol_surface, at  = interpolation_points)
#' @family interpolate functions
#' @export

interpolate.VolSurface <- function(x, at, ...) {
  assertthat::assert_that(
    all.equal(names(at), c("term", "smile")),
    length(at$term) == length(at$smile),
    assertthat::is.date(at$term),
    is.numeric(at$smile)
  )

  x$interpolator(at)
}
