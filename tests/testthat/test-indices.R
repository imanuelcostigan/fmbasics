context('Indices')

test_that('Initialiser', {
  ibor <- AUDBBSW(months(3))
  expect_true(is(ibor, 'IborIndex'))
})

