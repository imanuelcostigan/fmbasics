
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
#' @section Interpolation schemes:
#'
#' The following interpolation schemes are supported by `ZeroCurve`:
#'
#' * `ConstantInterpolation`: constant interpolation on zero rates
#' * `LinearInterpolation`: linear interpolation on zero rates
#' * `LogDFInterpolation`: linear interpolation on log discount factors or
#'    constant on forward rates
#' * `CubicInterpolation`: natural cubic spline on zero rates
#'
#' Points outside the calibration region use constant extrapolation on zero rates.
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
    compounding = cp
  ),
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
