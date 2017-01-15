context('Currencies')

library("lubridate")

test_that('as.character method works', {
  expect_equal(AUD()$as.character(), "AUD")
})

context("Currency pairs")

test_that("Initialize method works", {
  audusd <- CurrencyPair$new(AUD(), USD())
  expect_equal(audusd$iso(), "AUDUSD")
  audusd <- CurrencyPair$new(AUD(), USD(),
    JointCalendar$new(list(AUSYCalendar$new(), USNYCalendar$new())))
  expect_equal(audusd$iso(), "AUDUSD")
})
test_that('iso method works', {
  audusd <- CurrencyPair$new(AUD(), USD(),
    JointCalendar$new(list(AUSYCalendar$new(), USNYCalendar$new())))
  expect_equal(audusd$iso(), 'AUDUSD')
})

test_that('is_t1 method', {
  audusd <- CurrencyPair$new(AUD(), USD(),
    JointCalendar$new(list(AUSYCalendar$new(), USNYCalendar$new())))
  expect_true(!audusd$is_t1())
  # CAD not supported. But should be tested when this is supported.
})

test_that('value_dates method', {
  audusd <- CurrencyPair$new(AUD(), USD(),
    JointCalendar$new(list(AUSYCalendar$new(), USNYCalendar$new())))
  dates <- ymd(20140416, 20140419)
  expect_equal(audusd$value_dates(dates, 'today'),
    ymd(20140416, NA))
  expect_equal(audusd$value_dates(dates, 'spot'),
    ymd(20140422, 20140423))
  expect_equal(audusd$value_dates(dates, 'tomorrow'),
    ymd(20140417, 20140422))
  expect_equal(audusd$value_dates(dates, 'spot_next'),
    ymd(20140423, 20140424))
  expect_equal(audusd$value_dates(dates, months(1)),
    ymd(20140522, 20140523))
})
