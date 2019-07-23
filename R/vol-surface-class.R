#' VolSurface class
#'
#' The `VolSurface` class is designed to represent volatility surfaces.
#' Checks whether the dimensions of the surface correspond to the right type of axis: moneyness, delta or strike
#'
#' @param reference_date `Date` value that represents the as of date.
#' @param vol_quotes object of class `tibble` containing the volatility data.
#' The tibble contains 3 columns: \cr
#' 1. `maturity`: containing the maturities (tenors) of the vol surface, and is
#' of class `character` with format "yyyymmdd", \cr
#' 2. `strike` or `delta` or `moneyness`, this column is of class `numeric` and
#' the name of this column informs on the type of the volatility surface. The moneyness
#' is defined as strike/spot. \cr
#' 3. `value`, this column contains the volatility values, and is of class
#' `numeric`.
#' @param ticker a label for the vol object, this should be similar ticker of the underlying index
#' @param interpolation Interplation method, given as an object of class
#' interpolation [fmbasics::Interpolation()].
#' @export
#' @examples vol_surface <- build_vol_surface()

VolSurface <- function(reference_date, vol_quotes, ticker, interpolation) {
  validate_VolSurface(new_VolSurface(
    reference_date, vol_quotes, ticker, interpolation
  ))
}

new_VolSurface <- function(reference_date, vol_quotes, ticker, interpolation) {
  assertthat::assert_that(is.Interpolation(interpolation))

  structure(
    list(
      reference_date = reference_date,
      vol_quotes = vol_quotes,
      ticker = ticker,
      interpolation = interpolation
    ),
    class = "VolSurface"
  )
}


validate_VolSurface <- function(x) {
  assertthat::assert_that(
    assertthat::is.date(x$reference_date),
    colnames(x$vol_quotes)[1] == "maturity",
    colnames(x$vol_quotes)[2] %in% c("strike", "delta", "moneyness"),
    colnames(x$vol_quotes)[3] == "value",
    tibble::is_tibble(x$vol_quotes),
    is.character(x$vol_quotes$maturity),
    assertthat::is.date(as.Date(x$vol_quotes$maturity, format = "%Y%m%d")),
    is.double(x$vol_quotes$strike),
    is.double(x$vol_quotes$value),
    !is.unsorted(unique(x$vol_quotes$strike), na.rm = F),
    !is.unsorted(as.Date(unique(x$vol_quotes$maturity), format = "%Y%m%d"), na.rm = F),
    length(x$vol_quotes$strike) == length(unique(x$vol_quotes$strike)) *
      length(unique(x$vol_quotes$maturity)),
    is.character(x$ticker),
    is.Interpolation(x$interpolation)
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
#' vectors with the same length and are named `term` and `strike`. e.g.
#' list(term = c(1, 2), smile = c(72, 92)).
#' @param ... unused in this model.
#' @return `numeric` vector with length equal to the length of `at` members.
#' @family interpolate functions
#' @export
interpolate.VolSurface <- function(x, at, ...) {
  assertthat::assert_that(
    all.equal(names(at), c("term", "strike")),
    length(at$term) == length(at$strike),
    assertthat::is.date(at$term),
    is.numeric(at$strike)
  )

  if (is.LinearTimeVarInterpolation(x$interpolation) & colnames(x$vol_quotes)[2] == "strike") {
    vols <- x$vol_quotes$value
    variance <- vols^2
    tenors <- as.Date(x$vol_quotes$maturity, format = "%Y%m%d")
    tenors_yf <- fmdates::year_frac(
      date1 = x$reference_date, date2 = tenors,
      day_basis = "act/365"
    )

    time_variance <- variance * tenors_yf

    imp_vol <- rep(NA, length(at$term))


    for (i in seq_along(at$term)) {
      tt <- at$term[i]
      K <- at$strike[i]
      smile <- rep(NA, length(unique(x$vol_quotes$strike)))
      tt_yf <- fmdates::year_frac(
        date1 = x$reference_date, date2 = tt,
        day_basis = "act/365"
      )
      for (k in 1:length(smile)) {
        g <- stats::approxfun(
          x = unique(tenors_yf),
          y = time_variance[(1 + (k - 1) * length(unique(tenors_yf))):(k * length(unique(tenors_yf)))],
          method = "linear",
          rule = 2
        )
        smile[k] <- g(tt_yf)
      }
      if (K >= min(x$vol_quotes$strike) & K <= max(x$vol_quotes$strike)) {
        interpolated_time_var <- stats::spline(
          x = unique(x$vol_quotes$strike),
          y = smile, method = "natural", xout = K
        )$y
      }
      if (K < min(x$vol_quotes$strike)) {
        interpolated_time_var <- smile[1]
      }
      if (K > max(x$vol_quotes$strike)) {
        interpolated_time_var <- utils::tail(smile, 1)
      }
      imp_vol[i] <- sqrt(interpolated_time_var / tt_yf)
    }
  } else {
    print("Interpolation not implemented for this surface type")
  }

  imp_vol
}
