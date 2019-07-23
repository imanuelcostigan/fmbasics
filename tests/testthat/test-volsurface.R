context("VolSurface")

testthat::test_that(desc = "vol surface creation works", {
  testthat::expect_is(build_vol_surface(), "VolSurface")
  }
)

testthat::test_that("time variance interpolation works", {
  testthat::expect_true(is.LinearTimeVarInterpolation(LinearTimeVarInterpolation()))
})

testthat::test_that("the interpolation is working as expected", {
  term <-  c(as.Date("2023-10-10"), as.Date("2020-02-29"),
                    as.Date("2021-04-15"), as.Date("2022-06-10") )
  strike <- c(96, 150, 80, 90)

  vs <- build_vol_surface()
  testthat::expect_equal(interpolate(vs, list(term = term, strike = strike))
           , c(0.26853916752886564, 0.19909034016558932,
             0.25769535624031686,  0.25855784359768552 ))

})

