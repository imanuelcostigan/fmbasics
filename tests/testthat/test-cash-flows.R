context("CashFlow")

test_that("CashFlow creation works:", {
  mcm <- MultiCurrencyMoney(1, list(AUD()))
  expect_error(CashFlow(as.Date("2017-11-15"), mcm), NA)
  expect_error(CashFlow(as.Date(c("2017-11-15", "2017-11-16")), mcm))
  mcm <- MultiCurrencyMoney(1:2, list(AUD()))
  expect_error(CashFlow(as.Date(c("2017-11-15", "2017-11-16")), mcm), NA)
})

test_that("CashFlow assertion works:", {
  expect_true(is.CashFlow(
    CashFlow(as.Date("2017-11-15"), MultiCurrencyMoney(1, list(AUD())))
  ))
})

test_that("Tibbling works:", {
  expect_is(
    as_tibble(CashFlow(as.Date("2017-11-15"), MultiCurrencyMoney(1, list(AUD())))),
    "tbl_df"
  )
})