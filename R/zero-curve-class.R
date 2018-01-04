
# ZeroCurve ---------------------------------------------------------------


#' ZeroCurve class
#'
#' A class that defines the bare bones of a zero-coupon yield curve pricing
#' structure.
#'
#' A term structure of interest rates (or yield curve) is a curve showing
#' several yields or interest rates across different contract lengths (2 month,
#' 2 year, 20 year, etc...) for a similar debt contract. The curve shows the
#' relation between the (level of) interest rate (or cost of borrowing) and the
#' time to maturity, known as the "term", of the debt for a given borrower in a
#' given currency. For example, the U.S. dollar interest rates paid on U.S.
#' Treasury securities for various maturities are closely watched by many
#' traders, and are commonly plotted on a graph. More formal mathematical
#' descriptions of this relation are often called the term structure of interest
#' rates. When the effect of coupons on yields are stripped away, one has a
#' zero-coupon yield curve.
#'
#' The following interpolation schemes are supported by `ZeroCurve`:
#' `ConstantInterpolation`, `LinearInterpolation`, `LogDFInterpolation` and
#' `CubicInterpolation`. Points outside the calibration region use constant
#' extrapolation on the zero rate.
#'
#' @param discount_factors a [`DiscountFactor`] object. These are converted to
#'   continuously compounded zero coupon interest rates with an `act/365` day
#'   basis for internal storage purposes
#' @param reference_date a `Date` object
#' @param interpolation an [`Interpolation`] object
#' @return a `ZeroCurve` object
#' @examples
#' build_zero_curve()
#' @export
#' @seealso [Interpolation]

ZeroCurve <- function(discount_factors, reference_date, interpolation) {
  validate_ZeroCurve(new_ZeroCurve(discount_factors, reference_date, interpolation))
}


new_ZeroCurve <- function(discount_factors, reference_date, interpolation) {

  assertthat::assert_that(
    is.ConstantInterpolation(interpolation) ||
      is.LinearInterpolation(interpolation) ||
      is.LogDFInterpolation(interpolation) ||
      is.CubicInterpolation(interpolation)
  )

  # Will internally store zero rates are calculated using act/365 basis and
  # compounded continuously for speed purposes.

  db <- "act/365"
  cp <- Inf

  dt <- fmdates::year_frac(reference_date, discount_factors$end_date, db)
  r <- as_InterestRate(discount_factors, cp, db)$value

  f <- function(t) {

    before_first <- t < dt[1]
    after_last <- t > utils::tail(dt, 1)
    in_support <- !before_first & !after_last

    res <- vector("numeric", length = length(t))

    # Constant extrapolation on zeros before and after region of calibration.
    # This could be loosened by user paramterisation in the future.
    if (is.ConstantInterpolation(interpolation)) {
      g <- stats::approxfun(dt, r, method = "constant", rule = 2)
      return(g(t))
    }

    if (is.LinearInterpolation(interpolation)) {
      g <- stats::approxfun(dt, r, method = "linear", rule = 2)
      return(g(t))
    }

    if (is.LogDFInterpolation(interpolation)) {
      # rule = 2 is used to force approxfun to return a non-NA value outside
      # calibration region. But uses constant extrapolation on zeros outside
      # the region rather than constant on -r * t
      g <- stats::approxfun(dt, -dt * r, method = "linear", rule = 2)
      res[before_first] <- r[1]
      res[after_last] <- utils::tail(r, 1)
      res[in_support] <- -g(t[in_support]) / t[in_support]
      return(res)
    }

    if (is.CubicInterpolation(interpolation)) {
      g <- stats::splinefun(dt, r, method = "natural")
      # Override extarapolation to use constant extrapolation on zeros
      res[before_first] <- r[1]
      res[after_last] <- utils::tail(r, 1)
      res[in_support] <- g(t[in_support])
      return(res)
    }
  }

  structure(list(
    reference_date = reference_date,
    discount_factors = discount_factors,
    pillar_times = dt,
    pillar_zeros = r,
    interpolator = f,
    day_basis = db,
    compounding = cp),
    class = "ZeroCurve"
  )

}

validate_ZeroCurve <- function(x) {
  assertthat::assert_that(
    all(x$reference_date <= x$discount_factors$end_date),
    is.DiscountFactor(x$discount_factors),
    assertthat::is.date(x$reference_date),
    !is.unsorted(x$discount_factors$end_date)
  )
  x
}

#' Inherits from ZeroCurve
#'
#' Checks whether object inherits from `ZeroCurve` class
#'
#' @param x an R object
#' @return `TRUE` if `x` inherits from the `ZeroCurve` class; otherwise `FALSE`
#' @examples
#' is.ZeroCurve(build_zero_curve())
#' @export
is.ZeroCurve <- function(x) {
  inherits(x, "ZeroCurve")
}

#' @export
format.ZeroCurve <- function(x, ...) {
  paste0("<ZeroCurve> @ ", format(x$reference_date, "%e %B %Y"))
}

#' @export
print.ZeroCurve <- function(x, ...) {
  cat(format(x), "\n")
  print(tibble::as_tibble(x))
}

#' ZeroCurve attributes as a data frame
#'
#' Create a `tibble` that contains the pillar point maturities in years (using
#' the `act/365` convention) and the corresponding continuously compounded zero
#' rates.
#'
#' @param x a `ZeroCurve` object
#' @param ... other parameters that are not used by this methods
#' @return a `tibble` with two columns named `Years` and `Zeros`.
#' @seealso [tibble::tibble()]
#' @importFrom tibble as_tibble
#' @examples
#' library(tibble)
#' zc <- build_zero_curve()
#' as_tibble(zc)
#' @export
as_tibble.ZeroCurve <- function(x, ...) {
  tibble::tibble(
    Years = x$pillar_times,
    Zeros = x$pillar_zeros
  )
}


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
    natural_cubic_zeros = "Cubic"
  )
  structure(list(),
    class = c(paste0(prefix, "Interpolation"), "Interpolation"))
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
