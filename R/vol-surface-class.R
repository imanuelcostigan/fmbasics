#' VolSurface class
#'
#' The `VolSurface`` class is designed to represent volatility surfaces.
#' Checks whether the dimentions of the surface correspond to the right type of axis: moneyness, delta or strike
#'
#' @param reference_date `Date` value that represents the as of date
#' @param surface_type type of the vol dimentions. Available types: `strike/term`
#' @param vol_quotes `matrix` of class `numeric` holding the vol values.
#' @param dim1 `numeric` vector containing the surface strikes, deltas or
#' moneyness depending on the surface type.
#' @param dim2 `Date` vector containing the tenors of the surface.+
#'
#' @param ticker label for the vol object, this should be the ticker of the underlying index
#' @param interpolation object of type interpolation [fmbasics::Interpolation()]
#' @export
#' @examples vol_surface <- build_vol_surface()

VolSurface <- function(reference_date, surface_type, vol_quotes, dim1, dim2,
                       ticker, interpolation){
  validate_VolSurface(new_VolSurface(reference_date, surface_type, vol_quotes,
                                     dim1, dim2, ticker, interpolation))
}

new_VolSurface <- function(reference_date, surface_type, vol_quotes, dim1, dim2,
                           ticker, interpolation){

  assertthat::assert_that(is.Interpolation(interpolation))

  structure(
    list(
      reference_date = reference_date,
      surface_type = surface_type,
      vol_quotes = vol_quotes,
      dim1 = dim1,
      dim2 = dim2,
      ticker = ticker,
      interpolation = interpolation
    ),
    class = "VolSurface"
  )
}


validate_VolSurface <- function(x) {
  assertthat::assert_that(
    assertthat::is.date(x$reference_date),
    x$surface_type %in% c("strike/term", "delta/term", "moneyness/term"),
    all.equal(dim(x$vol_quotes), c(length(x$dim1), length(x$dim2))),
    !is.unsorted(x$dim1),!is.unsorted(x$dim2)
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
is.VolSurface <- function(x){
  inherits(x, "VolSurface")
}




# VolSurface methods --------------------------------

#'@rdname interpolate_vol
#'@export
interpolate_vol.VolSurface <- function(x, maturity, strike, ...){

  assertthat::assert_that(
    is.VolSurface(x),
    assertthat::is.date(maturity),
    is.numeric(strike),
    length(maturity) == length(strike)
  )

  surface_type <- x$surface_type
  vols <- x$vol_quotes
  variance <- vols^2
  tenors <- x$dim2
  tenors_yf <- fmdates::year_frac(date1 = x$reference_date, date2 = tenors ,
                      day_basis = "act/365")

  time_variance <- sweep(variance, MARGIN = 2, tenors_yf, `*`)
  imp_vol <- rep(NA, length(maturity))


  if(is.LinearTimeVarInterpolation(x$interpolation) & surface_type == "strike/term"){

    for(i in seq_along(maturity)){

      tt <- maturity[i]
      K <- strike[i]
      smile <- rep(NA, nrow(time_variance))
      tt_yf <- fmdates::year_frac(date1 = x$reference_date, date2 = tt,
                                  day_basis = "act/365")
      for (k in 1:length(smile)) {
        g <- stats::approxfun(x = tenors_yf,
                              y = time_variance[k,], method = "linear",rule = 2)
        smile[k] <- g(tt_yf)
      }
      if (K >= min(x$dim1) & K <= max(x$dim1)) {
        interpolated_time_var <- stats::spline(x = x$dim1,
                                    y = smile, method = "natural", xout = K )$y
      }
      if(K < min(x$dim1)){interpolated_time_var <- smile[1]}
      if(K > max(x$dim1)){interpolated_time_var <- utils::tail(smile,1)}
      imp_vol[i] <- sqrt(interpolated_time_var/tt_yf)
    }

  }else{

    print("Interpolation not implemented for this surface type")

  }

  imp_vol

}

















