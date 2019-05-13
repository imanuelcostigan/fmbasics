#' VolSurface class
#'
#' The `VolSurface`` class is designed to represent volatility surfaces.
#' Checks whether the dimentions of the surface correspond to the right type of axis: moneyness, delta or strike
#'
#' @param reference_date a `Date` value that represents the as of date
#' @param vol_quotes a numeric matrix holding the vol values
#' @param ticker a label for the vol object, this should be the ticker of the underlying index
#' @param surface_type the type of the vol dimentions: "strike/term", "delta/term" or "moneyness/term"
#' @param interpolation a object of type interpolation [fmbasics::Interpolation()]
#' @export

VolSurface <- function(reference_date, vol_quotes, ticker, surface_type, interpolation){
  validate_VolSurface(new_VolSurface(reference_date, vol_quotes, ticker, surface_type, interpolation))
}

new_VolSurface <- function(reference_date, vol_quotes, ticker, surface_type, interpolation){

  assertthat::assert_that(is.Interpolation(interpolation))

  structure(list(reference_date = reference_date,
                 vol_quotes = vol_quotes,
                 ticker = ticker,
                 surface_type = surface_type,
                 interpolation = interpolation),
                class = "VolSurface"
            )
}


validate_VolSurface <- function(x){
  strikes <- as.numeric()
  assertthat::assert_that(assertthat::is.date(x$reference_date),
                !is.unsorted(as.numeric(row.names(x$vol_quotes))),
                !is.unsorted(colnames(x$vol_quotes)))
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
  vols <- x$vol_quotes/100
  variance <- vols^2
  tenors <- as.Date(colnames(x$vol_quotes))
  tenors_yf <- fmdates::year_frac(date1 = x$reference_date, date2 = tenors , day_basis = "act/365")

  time_variance <- sweep(variance, MARGIN = 2, tenors_yf, `*`)
  imp_vol <- rep(NA, length(maturity))


  if(is.LinearTimeVarInterpolation(x$interpolation) & surface_type == "strike/tenor"){

    for(i in seq_along(maturity)){

      tt <- maturity[i]
      K <- strike[i]
      smile <- rep(NA, nrow(time_variance))
      tt_yf <- fmdates::year_frac(date1 = x$reference_date, date2 = tt, day_basis = "act/365")
      for (k in 1:length(smile)) {
        g <- stats::approxfun(x = tenors_yf,
                              y = time_variance[k,], method = "linear",rule = 2)
        smile[k] <- g(tt_yf)
      }
      interpolated_time_var <- stats::spline(x = as.numeric(row.names(time_variance)),
                                             y = smile, method = "natural", xout = K )$y
      imp_vol[i] <- sqrt(interpolated_time_var/tt_yf)

    }
  }

  imp_vol

}

















