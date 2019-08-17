
# Interpolation -----------------------------------------------------------

#' Interpolation
#'
#' These are lightweight interpolation classes that are used to specify
#' typical financial market interpolation schemes. Their behaviour is
#' dictated by the object in which they defined.
#'
#' @return an object that inherits from the `Interpolation` class.
#' @examples
#' ConstantInterpolation()
#' @name Interpolation
NULL

Interpolation <- function(method, what) {
  scheme <- paste0(method, "_", what)
  prefix <- switch(scheme,
    constant_zeros = "Constant",
    constant_forwards = "LogDF",
    linear_zeros = "Linear",
    natural_cubic_zeros = "Cubic",
    linear_cubic_time_var = "Linear_Cubic"
  )
  structure(list(),
    class = c(paste0(prefix, "Interpolation"), "Interpolation")
  )
}

#' @rdname Interpolation
#' @export
ConstantInterpolation <- function() Interpolation("constant", "zeros")
#' @rdname Interpolation
#' @export
LogDFInterpolation <- function() Interpolation("constant", "forwards")
#' @rdname Interpolation
#' @export
LinearInterpolation <- function() Interpolation("linear", "zeros")
#' @rdname Interpolation
#' @export
CubicInterpolation <- function() Interpolation("natural_cubic", "zeros")
#' @rdname Interpolation
#' @export
LinearCubicTimeVarInterpolation <- function() Interpolation("linear_cubic", "time_var")

#' Check Interpolation class
#'
#' These methods check whether an interpolation is of a particular scheme.
#'
#' @param x an object
#' @return a logical flag
#' @examples
#' is.Interpolation(CubicInterpolation())
#' is.CubicInterpolation(CubicInterpolation())
#' @export
is.Interpolation <- function(x) inherits(x, "Interpolation")
check_interpolation <- function(prefix) {
  function(x) methods::is(x, paste0(prefix, "Interpolation"))
}
#' @rdname is.Interpolation
#' @export
is.ConstantInterpolation <- check_interpolation("Constant")
#' @rdname is.Interpolation
#' @export
is.LogDFInterpolation <- check_interpolation("LogDF")
#' @rdname is.Interpolation
#' @export
is.LinearInterpolation <- check_interpolation("Linear")
#' @rdname is.Interpolation
#' @export
is.CubicInterpolation <- check_interpolation("Cubic")
#' @rdname is.Interpolation
#' @export
is.LinearCubicTimeVarInterpolation <- check_interpolation("Linear_Cubic")
#' @export
format.Interpolation <- function(x, ...) paste0("<", class(x)[1], ">")
#' @export
print.Interpolation <- function(x, ...) cat(format(x), "\n")

#' Interpolate a `ZeroCurve`
#'
#' There are two key interpolation schemes available in the `stats` package:
#' constant and linear interpolation via [stats::approxfun()] and
#' spline interpolation via [stats::splinefun()]. The `interpolate()` method
#' is a simple wrapper around these methods that are useful for the purposes
#' of interpolation financial market objects like zero coupon interest rate
#' curves.
#'
#' @param x a `ZeroCurve` object
#' @param at a non-negative numeric vector representing the years at which to
#'   interpolate the zero curve
#' @param ... unused in this method
#' @return a numeric vector of zero rates (continuously compounded, act/365)
#' @examples
#' zc <- build_zero_curve(LogDFInterpolation())
#' interpolate(zc, c(1.5, 3))
#' @export
#' @family interpolate functions
interpolate.ZeroCurve <- function(x, at, ...) {
  assertthat::assert_that(is.numeric(at), all(at >= 0))
  x$interpolator(at)
}

#' @importFrom tibble type_sum
#' @export
type_sum.ZeroCurve <- function(x) {
  "ZeroCurve"
}



# Curve methods -----------------------------------------------------------

#' @rdname interpolate_zeros
#' @export
interpolate_zeros.ZeroCurve <- function(x, at, compounding = NULL, day_basis = NULL, ...) {
  assertthat::assert_that(
    is.ZeroCurve(x),
    assertthat::is.date(at),
    is.null(compounding) || is_valid_compounding(compounding),
    is.null(day_basis) || fmdates::is_valid_day_basis(day_basis)
  )

  tt <- year_frac(x$reference_date, at, x$day_basis)
  zr <- InterestRate(interpolate(x, tt), x$compounding, x$day_basis)
  if (is.null(compounding) && is.null(day_basis)) {
    return(zr)
  } else {
    as_InterestRate(zr, compounding = compounding, day_basis = day_basis)
  }
}

#' @rdname interpolate_dfs
#' @export
interpolate_fwds.ZeroCurve <- function(x, from, to, ...) {
  assertthat::assert_that(
    is.ZeroCurve(x),
    assertthat::is.date(from),
    assertthat::is.date(to),
    all(from < to)
  )
  forward_dfs <- interpolate_dfs(x, from, to, ...)
  as_InterestRate(forward_dfs, 0, x$day_basis)
}

#' @rdname interpolate_dfs
#' @export
interpolate_dfs.ZeroCurve <- function(x, from, to, ...) {
  assertthat::assert_that(
    is.ZeroCurve(x),
    assertthat::is.date(from),
    assertthat::is.date(to),
    all(from <= to)
  )
  r1 <- interpolate_zeros(x, from, ...)
  r2 <- interpolate_zeros(x, to, ...)
  df_start <- as_DiscountFactor(r1, x$reference_date, from)
  df_end <- as_DiscountFactor(r2, x$reference_date, to)
  df_end / df_start
}
