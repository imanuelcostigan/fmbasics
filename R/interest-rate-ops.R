#' \code{InterestRate} operations
#'
#' A number of different operations can be performed on or with
#' \code{\link{InterestRate}} objects. Methods have been defined for base
#' package generic operations including arithmetic and comparison.
#'
#' The operations are:
#' \itemize{
#' \item \code{c}: concatenates a vector of \code{InterestRate} objects
#' \item \code{[}: extract parts of a \code{InterestRate} vector
#' \item \code{[<-}: replace parts of a \code{InterestRate} vector
#' \item \code{rep}: repeat a \code{InterestRate} object
#' \item \code{length}: determiens the length of a \code{InterestRate} vector
#' \item \code{+, -}: addition/subtraction of \code{InterestRate} objects. Where
#' two \code{InterestRate} objects are added/subtracted, the second is first
#' converted to have the same compounding and day basis frequency as the first.
#' Numeric values can be added/subtracted to/from an \code{InterestRate}
#' object by performing the operation directly on the \code{rate} field.
#' Arguments are recycled as necessary.
#' \item \code{*}: multiplication of \code{InterestRate} objects. Where
#' two \code{InterestRate} objects are multiplied, the second is first
#' converted to have the same compounding and day basis frequency as the first.
#' Numeric values can be multiplied to an \code{InterestRate}
#' object by performing the operation directly on the \code{rate} field.
#' Arguments are recycled as necessary.
#' \item \code{/}: division of \code{InterestRate} objects.  Where
#' two \code{InterestRate} objects are divided, the second is first
#' converted to have the same compounding and day basis frequency as the first.
#' Numeric values can divide an \code{InterestRate}
#' object by performing the operation directly on the \code{rate} field.
#' Arguments are recycled as necessary.
#' \item \code{<, >, <=, >=, ==, !=}: these operate in the standard way on the
#' \code{rate} field, and if necessary, the second \code{InterestRate} object
#' is converted to have the same compounding and day basis frequency as the
#' first.
#' }
#'
#' @name InterestRate-operators
NULL

#' @export
c.InterestRate <- function (..., recursive = FALSE) {
  dots <- list(...)
  value <- compounding <- vector("numeric")
  day_basis <- vector("character")
  for (i in seq_along(dots)) {
    value <- c(value, dots[[i]]$value)
    compounding <- c(compounding, dots[[i]]$compounding)
    day_basis <- c(day_basis, dots[[i]]$day_basis)
  }
  InterestRate(value, compounding, day_basis)
}

#' @export
`[.InterestRate` <- function (x, i, j, ..., drop = TRUE) {
  InterestRate(x$value[i], x$compounding[i], x$day_basis[i])
}

#' @export
`[<-.InterestRate` <- function (x, i, j, ..., value) {
  x$value[i] <- value$value
  x$compounding[i] <- value$compounding
  x$day_basis[i] <- value$day_basis
  x
}

#' @export
rep.InterestRate <- function (x, ...) {
  value <- rep(x$value, ...)
  compounding <- rep(x$compounding, ...)
  day_basis <- rep(x$day_basis, ...)
  InterestRate(value, compounding, day_basis)
}

#' @export
length.InterestRate <- function (x) {
  length(x$value)
}

#' @method all.equal InterestRate
#' @export
all.equal.InterestRate <- function (target, current, ...) {
  equal_rates <- all.equal(target$value, current$value)
  equal_compounding <- all.equal(target$compounding, current$compounding)
  equal_day_basis <- all.equal(target$day_basis, current$day_basis)
  msg <- NULL
  if (is.character(equal_rates))
    msg <- 'The rate fields are not equal.'
  if (is.character(equal_compounding))
    msg <- c(msg, 'The compounding fields are not equal.')
  if (is.character(equal_day_basis))
    msg <- c(msg, 'The day basis fields are not equal.')
  if (is.null(msg)) TRUE else msg
}

#####
# See zzz.R and https://github.com/wch/s3ops
####

op_ir <- function (op) {
  f <- function (e1, e2) {
    # Make sure both args are the same length
    n <- max(length(e1), length(e2))
    e1 <- rep(e1, length.out = n)
    e2 <- rep(e2, length.out = n)
    # Operate
    is_ir <- c(is.InterestRate(e1), is.InterestRate(e2))
    if (xor(is_ir[1], is_ir[2])) {
      # Only one IR. Which of e1 / e2 is IR?
      if (is_ir[1]) {
        return(InterestRate(op(e1$value, e2), e1$compounding, e1$day_basis))
      } else {
        return(f(e2, e1))
      }
    } else {
      # Both are IR (this function is called only if at least one IR found)
      # Convert second to same comp/daybasis as first.
      e2 <- as_InterestRate(e2, e1$compounding, e1$day_basis)
      return(InterestRate(op(e1$value, e2$value), e1$compounding, e1$day_basis))
    }
  }
  return (f)
}

plus_ir <- op_ir(`+`)
times_ir <- op_ir(`*`)
minus_ir <- op_ir(`-`)
div_ir <- op_ir(`/`)

compare_rate <- function(op) {
  function (e1, e2) {
    res <- vector("logical", length(e1))
    same_basis <- e1$compounding == e2$compounding & e1$day_basis == e2$day_basis
    if (any(same_basis)) {
      res[same_basis] <- op(e1$value[same_basis], e2$value[same_basis])
    }
    if (!all(same_basis)) {
      res[!same_basis] <- op(e1$value[!same_basis], as_InterestRate(e2,
        e1$compounding[!same_basis], e1$day_basis[!same_basis])$value)
    }
    return(res)
  }
}

#' @export
`==.InterestRate` <- compare_rate(`==`)

#' @export
`<.InterestRate` <- compare_rate(`<`)

#' @export
`>.InterestRate` <- compare_rate(`>`)

#' @export
`<=.InterestRate` <- function (e1, e2) {
  (e1 < e2) | (e1 == e2)
}

#' @export
`>=.InterestRate` <- function (e1, e2) {
  (e1 > e2) | (e1 == e2)
}

#' @export
`!=.InterestRate` <- function (e1, e2) {
  !(e1 == e2)
}


#' \code{DiscountFactor} operations
#'
#' A number of different operations can be performed on or with
#' \code{\link{DiscountFactor}} objects. Methods have been defined for base
#' package generic operations including arithmetic and comparison.
#'
#' The operations are:
#' \itemize{
#' \item \code{c}: concatenates a vector of \code{DiscountFactor} objects
#' \item \code{[}: extract parts of a \code{DiscountFactor} vector
#' \item \code{[<-}: replace parts of a \code{DiscountFactor} vector
#' \item \code{rep}: repeat a \code{DiscountFactor} object
#' \item \code{length}: determiens the length of a \code{DiscountFactor} vector
#' \item \code{*}: multiplication of \code{DiscountFactor} objects. The end
#' date of the first discount factor object must be equivalent to the start
#' date of the second (or vice versa). Arguments are recycled as necessary.
#' \item \code{/}: division of \code{DiscountFactor} objects. The start date
#' date of both arguments must be the same. Arguments are recycled as necessary.
#' \item \code{<, >, <=, >=, ==, !=}: these operate in the standard way on the
#' \code{discount_factor} field.
#' }
#'
#' @name DiscountFactor-operators
NULL

#' @export
c.DiscountFactor <- function (..., recursive = FALSE) {
  dots <- list(...)
  df <- d1 <- d2 <- vector("numeric", length(dots))
  for (i in seq_along(dots)) {
    df[i] <- dots[[i]]$value
    d1[i] <- dots[[i]]$start_date
    d2[i] <- dots[[i]]$end_date
  }
  DiscountFactor(df, lubridate::as_date(d1), lubridate::as_date(d2))
}

#' @export
`[.DiscountFactor` <- function (x, i, j, ..., drop = TRUE) {
  DiscountFactor(x$value[i], x$start_date[i], x$end_date[i])
}

#' @export
`[<-.DiscountFactor` <- function (x, i, j, ..., value) {
  x$value[i] <- value$value
  x$start_date[i] <- value$start_date
  x$end_date[i] <- value$end_date
  x
}

#' @export
rep.DiscountFactor <- function (x, ...) {
  discount_factor <- rep(x$value, ...)
  start_date <- rep(x$start_date, ...)
  end_date <- rep(x$end_date, ...)
  DiscountFactor(discount_factor, start_date, end_date)
}

#' @export
length.DiscountFactor <- function (x) {
  length(x$value)
}

#' @method all.equal DiscountFactor
all.equal.DiscountFactor <- function (target, current, ...) {
  equal_df <- all.equal(target$value, current$value)
  equal_d1 <- all.equal(target$start_date, current$start_date)
  equal_d2 <- all.equal(target$end_date, current$end_date)
  msg <- NULL
  if (is.character(equal_df))
    msg <- 'The discount_factor fields are not equal.'
  if (is.character(equal_d1))
    msg <- c(msg, 'The start_date fields are not equal.')
  if (is.character(equal_d2))
    msg <- c(msg, 'The end_date fields are not equal.')
  if (is.null(msg)) TRUE else msg
}

#####
# See zzz.R and https://github.com/wch/s3ops
####

times_df <- function (e1, e2) {
  n <- max(length(e1), length(e2))
  e1 <- rep(e1, length.out = n)
  e2 <- rep(e2, length.out = n)
  assertthat::assert_that(
    any(all(e2$end_date == e1$start_date),
    all(e1$end_date == e2$start_date)))
  df <- e1$value * e2$value
  # http://adv-r.had.co.nz/Performance.html#implementation-performance
  # Use of pmin/pmax suboptimal
  d1 <- e1$start_date
  d1[d1 >= e2$start_date] <- e2$start_date[d1 >= e2$start_date]
  d2 <- e1$end_date
  d2[d2 <= e2$end_date] <- e2$end_date[d2 <= e2$end_date]
  DiscountFactor(df, d1, d2)
}

div_df <- function (e1, e2) {
  n <- max(length(e1), length(e2))
  e1 <- rep(e1, length.out = n)
  e2 <- rep(e2, length.out = n)
  assertthat::assert_that(all(e1$start_date == e2$start_date))
  df <- e1$value / e2$value
  d1 <- e1$end_date
  d1[d1 >= e2$end_date] <- e2$end_date[d1 >= e2$end_date]
  d2 <- e1$end_date
  d2[d2 <= e2$end_date] <- e2$end_date[d2 <= e2$end_date]
  DiscountFactor(df, d1, d2)
}

#' @export
`==.DiscountFactor` <- function (e1, e2) {
  e1$value == e2$value
}

#' @export
`<.DiscountFactor` <- function (e1, e2) {
  e1$value < e2$value
}

`>.DiscountFactor` <- function (e1, e2) {
  e1$value > e2$value
}

#' @export
`<=.DiscountFactor` <- function (e1, e2) {
  (e1 < e2) | (e1 == e2)
}

#' @export
`>=.DiscountFactor` <- function (e1, e2) {
  (e1 > e2) | (e1 == e2)
}

#' @export
`!=.DiscountFactor` <- function (e1, e2) {
  !(e1 == e2)
}
