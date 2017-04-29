context("ZeroCurve")

test_that("fmdata_example works", {
  df <- fmdata_example("zerocurve.csv")
  expect_is(df, "data.frame")
  expect_named(df, c("start", "end", "zeros", "dfs"))
})

test_that("Interpolation checks work", {
  expect_true(is.ConstantInterpolation(ConstantInterpolation()))
  expect_true(is.LinearInterpolation(LinearInterpolation()))
  expect_true(is.LogDFInterpolation(LogDFInterpolation()))
  expect_true(is.CubicInterpolation(CubicInterpolation()))
  expect_true(is.Interpolation(CubicInterpolation()))
  expect_false(is.CubicInterpolation(ConstantInterpolation()))
})


test_that("Interpolation works", {
  # Constant
  zc <- build_zero_curve(ConstantInterpolation())
  af <- stats::approxfun(zc$pillar_times, zc$pillar_zeros, "constant")
  expect_equal(interpolate(zc, c(0, 4, 6, 50)),
    c(head(zc$pillar_zeros, 1), af(c(4, 6)), tail(zc$pillar_zeros, 1)))

  # Linear
  zc <- build_zero_curve(LinearInterpolation())
  af <- stats::approxfun(zc$pillar_times, zc$pillar_zeros, "linear")
  expect_equal(interpolate(zc, c(0, 4, 6, 50)),
    c(head(zc$pillar_zeros, 1), af(c(4, 6)), tail(zc$pillar_zeros, 1)))

  # LogDF
  zc <- build_zero_curve(LogDFInterpolation())
  af <- stats::approxfun(zc$pillar_times, -zc$pillar_times * zc$pillar_zeros, "linear")
  expect_equal(interpolate(zc, c(0, 4, 6, 50)),
    c(head(zc$pillar_zeros, 1), -af(c(4, 6)) / c(4, 6), tail(zc$pillar_zeros, 1)))

  # Natural spline
  zc <- build_zero_curve(CubicInterpolation())
  sf <- stats::splinefun(zc$pillar_times, zc$pillar_zeros, "natural")
  expect_equal(interpolate(zc, c(0, 4, 6, 50)),
    c(head(zc$pillar_zeros, 1), sf(c(4, 6)), tail(zc$pillar_zeros, 1)))

})
