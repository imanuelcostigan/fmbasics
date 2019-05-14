context("VolSurface")

testthat::test_that(desc = "vol surface creation works", {
  testthat::expect_is(build_vol_surface(), "VolSurface")
  }
)

testthat::test_that("time variance interpolation works", {
  testthat::expect_true(is.LinearTimeVarInterpolation(LinearTimeVarInterpolation()))
})

testthat::test_that("the interpolation is working as expected", {
  vs <- build_vol_surface()
  testthat::expect_equal(interpolate_vol(vs, as.Date("2023-10-10"), 96)
           , 0.26853916752886564)
  testthat::expect_equal(interpolate_vol(vs, as.Date("2020-02-29"), 150),
           0.19909034016558932)
})

