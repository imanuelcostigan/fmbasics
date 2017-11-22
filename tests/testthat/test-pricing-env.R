context("PricingEnv")

test_that("FXRates can be created:", {
  expect_error(build_fx_rates(), NA)
})

test_that("FXRates assertion works:", {
  expect_true(is.FXRates(build_fx_rates()))
})

test_that("ZeroCurves can be created:", {
  expect_error(build_zero_curves(), NA)
})

test_that("ZeroCurves assertion works:", {
  expect_true(is.ZeroCurves(build_zero_curves()), NA)
})

test_that("PricingEnv can be created:", {
  expect_error(build_pricing_env(), NA)
})

test_that("PricingEnv assertion works:", {
  expect_true(is.PricingEnv(build_pricing_env()))
})

test_that("pick works:", {
  pe <- build_pricing_env()
  expect_is(pick(pe, "zero_curves", "CC1_3M"), "ZeroCurve")
  expect_equal(pick(pe, "fx_rates", "AUDUSD"), 0.72335)
})