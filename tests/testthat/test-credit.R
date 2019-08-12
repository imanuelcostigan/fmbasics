library("lubridate")

test_that("Constructor works", {
  rate <- ZeroHazardRate(c(0.01, 0.02, 0.03), c(0, Inf, 0), c('act/365','act/360',
    'act/365'), specs = CDSSpec("Empty"))
  expect_equal(rate$value, c(0.01, 0.02, 0.03))
  expect_equal(rate$compounding, c(0, Inf, 0))
  expect_equal(rate$day_basis, c('act/365', 'act/360', 'act/365'))
})

test_that("as_SurvivalProbabilities method works", {
  rate <- ZeroHazardRate(0.04, 0, "act/360", specs = CDSSpec("Empty"))
  expect_equal(as_SurvivalProbabilities(rate, ymd("20100101"), ymd("20150101"))$value,
    0.831331978570109, tolerance=1e20)
  rate <- ZeroHazardRate(0.01, -1, "30/360us", specs = CDSSpec("Empty"))
  expect_equal(as_SurvivalProbabilities(rate, ymd("20100101"), ymd("20100330"))$value,
    0.997527777777778, tolerance=1e20)
})


test_that('c() works for ZeroHazard Rates', {
  rate1 <- ZeroHazardRate(0.04, 2, 'act/365', specs = CDSSpec("Empty"))
  rate2 <- ZeroHazardRate(0.035, 4, 'act/360', specs = CDSSpec("Empty"))
  rates <- ZeroHazardRate(c(0.04, 0.035), c(2, 4), c('act/365', 'act/360'), specs = CDSSpec("Empty"))
  expect_equal(c(rate1, rate2), rates)
})

test_that('Addition works for ZeroHazard Rates ',  {
  rate1 <- ZeroHazardRate(0.04, 2, 'act/365', specs = CDSSpec("Empty"))
  rate2 <- ZeroHazardRate(0.02, 2, 'act/365', specs = CDSSpec("Empty"))
  result <- ZeroHazardRate(0.06, 2, 'act/365',specs = CDSSpec("Empty"))
  expect_equal(rate1+rate2, result)
})

test_that('Comparison ops works', {
  rate1 <- ZeroHazardRate(0.04, 2, 'act/365', specs = CDSSpec("Empty"))
  rate2 <- ZeroHazardRate(0.01, 2, 'act/365', specs = CDSSpec("Empty"))
  expect_false(rate1 == rate2)
  expect_true(rate1 == rate1)
  expect_false(rate1 < rate2)
  expect_true(rate1 > rate2)
  expect_true(rate1 >= rate2)
  expect_true(rate1 >= rate1)
})

test_that('c() works as expected for Survival Probabilites', {
  sp1 <- SurvivalProbabilities(0.99, ymd(20130101), ymd(20140101), specs = CDSSpec("Empty"))
  sp2 <- SurvivalProbabilities(0.97, ymd(20130101), ymd(20150101), specs = CDSSpec("Empty"))
  result <- SurvivalProbabilities(c(0.99, 0.97), ymd(20130101, 20130101),
    ymd(20140101, 20150101), specs = CDSSpec("Empty"))
  expect_equal(c(sp1, sp2), result)
})

test_that('Division works as expected', {
  sp1 <- SurvivalProbabilities(0.99, ymd(20130101, 20130201),
    ymd(20140101, 20140201), specs = CDSSpec("Empty"))
  sp2 <- SurvivalProbabilities(0.97, ymd(20130101, 20130201),
    ymd(20150101, 20150201), specs = CDSSpec("Empty"))
  result <- SurvivalProbabilities(0.97 / 0.99, ymd(20140101, 20140201),
    ymd(20150101, 20150201), specs = CDSSpec("Empty"))
  expect_equal(sp2 /sp1, result)
})

test_that("Comparisons work", {
  sp1 <- SurvivalProbabilities(0.99, ymd(20130101), ymd(20140101), specs = CDSSpec("Empty"))
  sp2 <- SurvivalProbabilities(0.99, ymd(20130101), ymd(20150101), specs = CDSSpec("Empty"))
  sp3 <- SurvivalProbabilities(0.98, ymd(20130101), ymd(20150101), specs = CDSSpec("Empty"))
  expect_true(sp1 == sp1)
  expect_true(sp3 < sp1)
  expect_true(sp1 > sp3)
})

