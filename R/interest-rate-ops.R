#' \code{InterestRate} operations
#'
#' The \code{\link{InterestRate}} class is built using the vctrs package.
#' Therefore all base vector operations can be performed on or with
#' \code{\link{InterestRate}} objects.
#'
#' Some care has to be taken when using arithmetic and comparison operators:
#' \itemize{
#' \item Arithmetic operations on \code{InterestRate} objects: Where both
#' arguments are \code{InterestRate} objects,their compounding and day
#' basis frequency must match.
#' Operations between numeric values and an \code{InterestRate} object are
#' performed directly on the \code{rate} field.
#' Arguments are recycled as necessary.
#' \item \code{<, >, <=, >=, ==, !=}: To enable comparison both vectors are
#' converted to infinite compounding and act/365 day basis. The
#' resulting \code{rate} field is used for comparison.
#' }
#'
#' @name InterestRate-operators
NULL

#' @method all.equal InterestRate
#' @export
all.equal.InterestRate <- function(target, current, ...) {
  equal_rates <-
    all.equal(field(target, "value"), field(current, "value"))
  equal_compounding <-
    all.equal(field(target, "compounding"), field(current, "compounding"))
  equal_day_basis <-
    all.equal(field(target, "day_basis"), field(current, "day_basis"))
  msg <- NULL
  if (is.character(equal_rates))
    msg <- "The rate fields are not equal."
  if (is.character(equal_compounding))
    msg <- c(msg, "The compounding fields are not equal.")
  if (is.character(equal_day_basis))
    msg <- c(msg, "The day basis fields are not equal.")
  if (is.null(msg)) TRUE else msg
}


#' @method vec_arith InterestRate
#' @export
vec_arith.InterestRate <- function(op, x, y, ...) {
  UseMethod("vec_arith.InterestRate", y)
}

#' @method vec_arith.InterestRate default
#' @export
vec_arith.InterestRate.default <- function(op, x, y, ...) {
  stop_incompatible_op(op, x, y)
}

#' @method vec_arith.InterestRate InterestRate
#' @export
vec_arith.InterestRate.InterestRate <- function(op, x, y, ...) {
  if (!all(field(x, "compounding") == field(y, "compounding"))) {
    stop_incompatible_op(details = "Compounding fields must be equal.")
  }
  if (!all(field(x, "day_basis") == field(y, "day_basis"))) {
    stop_incompatible_op(details = "Day basis fields must be equal.")
  }

  new_InterestRate(
    vec_arith_base(op, field(x, "value"), field(y, "value")),
    field(x, "compounding"),
    field(x, "day_basis")
  )
}

#' @method vec_arith.InterestRate numeric
#' @export
vec_arith.InterestRate.numeric <- function(op, x, y, ...) {
  new_InterestRate(
    vec_arith_base(op, field(x, "value"), y),
    field(x, "compounding"),
    field(x, "day_basis")
  )
}

#' @method vec_arith.numeric InterestRate
#' @export
vec_arith.numeric.InterestRate <- function(op, x, y, ...) {
  new_InterestRate(
    vec_arith_base(op, x, field(y, "value")),
    field(y, "compounding"),
    field(y, "day_basis")
  )
}

#' @export
vec_proxy_equal.InterestRate <- function(x, ...) {
  field(as_InterestRate(x, compounding = Inf, day_basis = "act/365"), "value")
}

#' @export
vec_proxy_compare.InterestRate <- function(x, ...) {
  field(as_InterestRate(x, compounding = Inf, day_basis = "act/365"), "value")
}


#' \code{DiscountFactor} operations
#'
#' As the \code{\link{DiscountFactor}} is built with the vctrs package all base
#' vector operations are available (i.e. \code{c}, \code{[}, ...).
#'
#' Additionally \code{DiscountFactor} objects may be multiplied and divided
#' under the following constraints:
#' \itemize{
#' \item \code{*}: multiplication of \code{DiscountFactor} objects. The end
#' date of the first discount factor object must be equivalent to the start
#' date of the second (or vice versa). Arguments are recycled as necessary.
#' \item \code{/}: division of \code{DiscountFactor} objects. The start date
#' date of both arguments must be the same. Arguments are recycled as necessary.
#' }
#'
#' @name DiscountFactor-operators
NULL

#' @method all.equal DiscountFactor
all.equal.DiscountFactor <- function(target, current, ...) {
  equal_df <-
    all.equal(field(target, "value"), field(current, "value"))
  equal_d1 <-
    all.equal(field(target, "start_date"), field(current, "start_date"))
  equal_d2 <-
    all.equal(field(target, "end_date"), field(current, "end_date"))
  msg <- NULL
  if (is.character(equal_df))
    msg <- "The discount_factor fields are not equal."
  if (is.character(equal_d1))
    msg <- c(msg, "The start_date fields are not equal.")
  if (is.character(equal_d2))
    msg <- c(msg, "The end_date fields are not equal.")
  if (is.null(msg)) TRUE else msg
}

#' @method vec_arith DiscountFactor
#' @export
vec_arith.DiscountFactor <- function(op, x, y, ...) {
  UseMethod("vec_arith.DiscountFactor", y)
}

#' @method vec_arith.DiscountFactor default
#' @export
vec_arith.DiscountFactor.default <- function(op, x, y, ...) {
  stop_incompatible_op(op, x, y)
}

#' @method vec_arith.DiscountFactor DiscountFactor
#' @export
vec_arith.DiscountFactor.DiscountFactor <- function(op, x, y, ...) {
  switch(
    op,
    "*" = times_df(x, y),
    "/" = div_df(x, y),
    stop_incompatible_op(op, x, y)
  )
}

times_df <- function(e1, e2) {
  c(e1, e2) %<-% vec_recycle_common(e1, e2)

  e1_start_date <- field(e1, "start_date")
  e1_end_date <- field(e1, "end_date")
  e2_start_date <- field(e2, "start_date")
  e2_end_date <- field(e2, "end_date")

  assertthat::assert_that(
    any(all(e2_end_date == e1_start_date),
    all(e1_end_date == e2_start_date))
  )
  df <- field(e1, "value") * field(e2, "value")
  # http://adv-r.had.co.nz/Performance.html#implementation-performance
  # Use of pmin/pmax suboptimal
  d1 <- e1_start_date
  d1[d1 >= e2_start_date] <- e2_start_date[d1 >= e2_start_date]
  d2 <- e1_end_date
  d2[d2 <= e2_end_date] <- e2_end_date[d2 <= e2_end_date]
  new_DiscountFactor(df, d1, d2)
}

div_df <- function(e1, e2) {
  c(e1, e2) %<-% vec_recycle_common(e1, e2)

  e1_start_date <- field(e1, "start_date")
  e1_end_date <- field(e1, "end_date")
  e2_start_date <- field(e2, "start_date")
  e2_end_date <- field(e2, "end_date")

  assertthat::assert_that(all(e1_start_date == e2_start_date))

  df <- field(e1, "value") / field(e2, "value")
  d1 <- e1_end_date
  d1[d1 >= e2_end_date] <- e2_end_date[d1 >= e2_end_date]
  d2 <- e1_end_date
  d2[d2 <= e2_end_date] <- e2_end_date[d2 <= e2_end_date]
  new_DiscountFactor(df, d1, d2)
}

#' @export
vec_proxy_compare.DiscountFactor <- function(x, ...) {
  field(x, "value")
}
