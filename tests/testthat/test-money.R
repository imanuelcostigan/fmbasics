context("Money")

test_that("SingleCurrencyMoney created:", {
  expect_error(SingleCurrencyMoney(1, AUD()), NA)
  expect_error(SingleCurrencyMoney(1:2, AUD()), NA)
  expect_error(SingleCurrencyMoney(1, list(AUD())))
})

test_that("SingleCurrencyMoney iso is correct:", {
  expect_equal(iso(SingleCurrencyMoney(1, AUD())), "AUD")
})

test_that("SingleCurrencyMoney assertion works:", {
  expect_true(is.SingleCurrencyMoney(SingleCurrencyMoney(1, AUD())))
  expect_false(is.SingleCurrencyMoney(MultiCurrencyMoney(1, list(AUD()))))
})

test_that("MultiCurrencyMoney created:", {
  expect_error(MultiCurrencyMoney(1:2, list(AUD())), NA)
  expect_error(MultiCurrencyMoney(1:2, list(AUD(), USD())), NA)
  expect_error(MultiCurrencyMoney(1:3, list(AUD(), USD())))
  expect_error(MultiCurrencyMoney(1, AUD()))
})

test_that("MultiCurrencyMoney assertion works:", {
  expect_false(is.MultiCurrencyMoney(SingleCurrencyMoney(1, AUD())))
  expect_true(is.MultiCurrencyMoney(MultiCurrencyMoney(1, list(AUD()))))
})
