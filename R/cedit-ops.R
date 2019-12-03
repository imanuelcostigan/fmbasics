#' \code{ZeroHazardRate} operations
#'
#' A number of different operations can be performed on or with
#' \code{\link{ZeroHazardRate}} objects. Methods have been defined for base
#' package generic operations including arithmetic and comparison.
#'
#' The operations are:
#' \itemize{
#' \item \code{c}: concatenates a vector of \code{ZeroHazardRate} objects
#' \item \code{[}: extract parts of a \code{ZeroHazardRate} vector
#' \item \code{[<-}: replace parts of a \code{ZeroHazardRate} vector
#' \item \code{rep}: repeat a \code{ZeroHazardRate} object
#' \item \code{length}: determines the length of a \code{ZeroHazardRate} vector
#' \item \code{+, -}: addition/subtraction of \code{ZeroHazardRate} objects. Where
#' two \code{ZeroHazardRate} objects are added/subtracted, the second is first
#' converted to have the same compounding and day basis frequency as the first.
#' Numeric values can be added/subtracted to/from an \code{ZeroHazardRate}
#' object by performing the operation directly on the \code{rate} field.
#' Arguments are recycled as necessary.
#' \item \code{*}: multiplication of \code{ZeroHazardRate} objects. Where
#' two \code{ZeroHazardRate} objects are multiplied, the second is first
#' converted to have the same compounding and day basis frequency as the first.
#' Numeric values can be multiplied to an \code{ZeroHazardRate}
#' object by performing the operation directly on the \code{rate} field.
#' Arguments are recycled as necessary.
#' \item \code{/}: division of \code{ZeroHazardRate} objects.  Where
#' two \code{ZeroHazardRate} objects are divided, the second is first
#' converted to have the same compounding and day basis frequency as the first.
#' Numeric values can divide an \code{ZeroHazardRate}
#' object by performing the operation directly on the \code{rate} field.
#' Arguments are recycled as necessary.
#' \item \code{<, >, <=, >=, ==, !=}: these operate in the standard way on the
#' \code{rate} field, and if necessary, the second \code{ZeroHazardRate} object
#' is converted to have the same compounding and day basis frequency as the
#' first.
#' }
#'
#' @name ZeroHazardRate-operators
NULL

############################


#' @export
c.ZeroHazardRate <- function (..., recursive = FALSE) {
  dots <- list(...)
  assertthat::assert_that(is_atomic_list(dots, is.ZeroHazardRate))
  value <- compounding <- vector("numeric")
  day_basis <- vector("character")

  for (i in seq_along(dots)) {
    value <- c(value, dots[[i]]$value)
    compounding <- c(compounding, dots[[i]]$compounding)
    day_basis <- c(day_basis, dots[[i]]$day_basis)
  }

  spec_vec <- as.character(dots[[1]]$specs)
  specs_identical <- TRUE
  for(j in seq_along(dots)[-1]) {
    if(all(as.character(dots[[i]]$specs)!=spec_vec)) {
      specs_identical <- FALSE
      break
    }
    spec_vec <- as.character(dots[[i]]$specs)
  }

  if(specs_identical){
    out <- new_ZeroHazardRate(values = value, compounding = compounding,
      day_basis = day_basis, specs =  dots[[1]]$specs)
  } else {
    out <- new_ZeroHazardRate(values = value, compounding = compounding,
      day_basis = day_basis, specs =  CDSSpec("Empty"))
    warning("Warning: The output metadata is ambiguous, so removed.")
  }
  out
}


############################


#' @export
`[.ZeroHazardRate` <- function (x, i, j, ..., drop = TRUE) {
  new_ZeroHazardRate(x$value[i], x$compounding[i], x$day_basis[i], specs = x$specs[i])
}


#' @export
`[<-.ZeroHazardRate` <- function (x, i, j, ..., value) {
  x$value[i] <- value$value
  x$compounding[i] <- value$compounding
  x$day_basis[i] <- value$day_basis
  x$specs[i] <- value$specs
  x
}

#' @export
rep.ZeroHazardRate <- function (x, ...) {
  value <- rep(x$value, ...)
  compounding <- rep(x$compounding, ...)
  day_basis <- rep(x$day_basis, ...)
  specs <- rep(x$specs, ...)
  new_ZeroHazardRate(value, compounding, day_basis, specs)
}

#' @export
length.ZeroHazardRate <- function (x) {
  length(x$value)
}

#' @method all.equal ZeroHazardRate
#' @export
all.equal.ZeroHazardRate <- function (target, current, ...) {
  equal_rates <- all.equal(target$value, current$value)
  equal_compounding <- all.equal(target$compounding, current$compounding)
  equal_day_basis <- all.equal(target$day_basis, current$day_basis)
  equal_specs <- all.equal(target$specs, current$specs)
  msg <- NULL
  if (is.character(equal_rates))
    msg <- 'The rate fields are not equal.'
  if (is.character(equal_compounding))
    msg <- c(msg, 'The compounding fields are not equal.')
  if (is.character(equal_day_basis))
    msg <- c(msg, 'The day basis fields are not equal.')
  if (is.character(equal_specs))
    msg <- c(msg, 'The CDSSpec fields are not equal.')
  if (is.null(msg)) TRUE else msg
}

op_zhr <- function (op) {
  f <- function (e1, e2) {
    # Make sure both args are the same length
    assertthat::assert_that(all(length(e1) == length(e2)))

    if(all(as.character(e1$specs)!=as.character(e2$specs))) {
      specs <- CDSSpec(rank = "Empty")
      warning("Warning: The output metadata is ambiguous, so removed.")
    } else {
      specs <- e1$specs
    }
    # Operate
    is_zhr <- c(is.ZeroHazardRate(e1), is.ZeroHazardRate(e2))
    if (xor(is_zhr[1], is_zhr[2])) {
      # Only one IR. Which of e1 / e2 is IR?
      if (is_zhr[1]) {
        return(new_ZeroHazardRate(op(e1$values, e2), e1$compounding, e1$day_basis), specs)
      } else {
        return(f(e2, e1))
      }
    } else {
      # Both are ZHR (this function is called only if at least one ZHR found)
      # Convert second to same comp/daybasis as first.
      e2 <- as_ZeroHazardRate(e2, e1$compounding, e1$day_basis, specs)
      return(new_ZeroHazardRate(op(e1$values, e2$values), e1$compounding, e1$day_basis, specs))
    }
  }
  return (f)
}

plus_zhr <- op_zhr(`+`)
times_zhr <- op_zhr(`*`)
minus_zhr <- op_zhr(`-`)
div_zhr <- op_zhr(`/`)

##

compare_hazrate <- function(op) {
  function (e1, e2) {
    res <- vector("logical", length(e1))
    same_basis <- e1$compounding == e2$compounding & e1$day_basis == e2$day_basis
    if (any(same_basis)) {
      res[same_basis] <- op(e1$value[same_basis], e2$value[same_basis])
    }
    if (!all(same_basis)) {
      tmp_e2 <- as_ZeroHazardRate(e2,
        e1$compounding[!same_basis], e1$day_basis[!same_basis], specs = CDSSpec(rank = "Empty"))
      res[!same_basis] <- op(e1$value[!same_basis], tmp_e2$value)
    }
    message("Operation ignores CDSSpecs")
    return(res)
  }
}

#' @export
`==.ZeroHazardRate` <- compare_hazrate(`==`)

#' @export
`<.ZeroHazardRate` <- compare_hazrate(`<`)

#' @export
`>.ZeroHazardRate` <- compare_hazrate(`>`)

#' @export
`<=.ZeroHazardRate` <- function (e1, e2) {
  (e1 < e2) | (e1 == e2)
}

#' @export
`>=.ZeroHazardRate` <- function (e1, e2) {
  (e1 > e2) | (e1 == e2)
}

#' @export
`!=.ZeroHazardRate` <- function (e1, e2) {
  !(e1 == e2)
}

#' \code{SurvivalProbabilities} operations
#'
#' A number of different operations can be performed on or with
#' \code{\link{SurvivalProbabilities}} objects. Methods have been defined for base
#' package generic operations including arithmetic and comparison.
#'
#' The operations are:
#' \itemize{
#' \item \code{c}: concatenates a vector of \code{SurvivalProbabilities} objects
#' \item \code{[}: extract parts of a \code{SurvivalProbabilities} vector
#' \item \code{[<-}: replace parts of a \code{SurvivalProbabilities} vector
#' \item \code{rep}: repeat a \code{SurvivalProbabilities} object
#' \item \code{length}: determines the length of a \code{SurvivalProbabilities} vector
#' \item \code{*}: multiplication of \code{SurvivalProbabilities} objects. The end
#' date of the first SurvivalProbabilities object must be equivalent to the start
#' date of the second (or vice versa). Arguments are recycled as necessary.
#' \item \code{/}: division of \code{SurvivalProbabilities} objects. The start date
#' date of both arguments must be the same. Arguments are recycled as necessary.
#' \item \code{<, >, <=, >=, ==, !=}: these operate in the standard way on the
#' \code{discount_factor} field.
#' }
#'
#' @name SurvivalProbabilities-operators
NULL

#' @export
c.SurvivalProbabilities <- function (..., recursive = FALSE) {
  dots <- list(...)
  assertthat::assert_that(is_atomic_list(dots, is.SurvivalProbabilities))
  df <- d1 <- d2 <- vector("numeric", length(dots))

  for (i in seq_along(dots)) {
    df[i] <- dots[[i]]$values
    d1[i] <- dots[[i]]$start_date
    d2[i] <- dots[[i]]$end_date
  }

  spec_vec <- as.character(dots[[1]]$specs)
  specs_identical <- TRUE
  for(j in seq_along(dots)[-1]) {
    if(all(as.character(dots[[i]]$specs)!=spec_vec)) {
      specs_identical <- FALSE
      break
    }
    spec_vec <- as.character(dots[[i]]$specs)
  }

  if(specs_identical){
    out <- new_SurvivalProbabilities(df, lubridate::as_date(d1), lubridate::as_date(d2),
      specs =  dots[[1]]$specs)
  } else {
    out <- new_SurvivalProbabilities(df, lubridate::as_date(d1), lubridate::as_date(d2),
      specs =  CDSSpec("Empty"))
    warning("Warning: The output metadata is ambiguous, so removed.")
  }
  out
}

#' @export
`[.SurvivalProbabilities` <- function (x, i, j, ..., drop = TRUE) {
  new_SurvivalProbabilities(x$value[i], x$start_date[i], x$end_date[i], specs = x$specs[i])
}

#' @export
`[<-.SurvivalProbabilities` <- function (x, i, j, ..., value) {
  x$value[i] <- value$value
  x$start_date[i] <- value$start_date
  x$end_date[i] <- value$end_date
  x$specs[i] <- value$specs
  x
}

#' @export
rep.SurvivalProbabilities <- function (x, ...) {
  discount_factor <- rep(x$value, ...)
  start_date <- rep(x$start_date, ...)
  end_date <- rep(x$end_date, ...)
  new_SurvivalProbabilities(discount_factor, start_date, end_date, specs = x$specs)
}

#' @export
length.SurvivalProbabilities <- function (x) {
  length(x$value)
}

#' @method all.equal SurvivalProbabilities
all.equal.SurvivalProbabilities <- function (target, current, ...) {
  equal_df <- all.equal(target$value, current$value)
  equal_d1 <- all.equal(target$start_date, current$start_date)
  equal_d2 <- all.equal(target$end_date, current$end_date)
  equal_specs <- all.equal(target$specs, current$specs)
  msg <- NULL
  if (is.character(equal_df))
    msg <- 'The discount_factor fields are not equal.'
  if (is.character(equal_d1))
    msg <- c(msg, 'The start_date fields are not equal.')
  if (is.character(equal_d2))
    msg <- c(msg, 'The end_date fields are not equal.')
  if (is.character(equal_specs))
    msg <- c(msg, 'The CDSSpec fields are not equal.')
  if (is.null(msg)) TRUE else msg
}


#####
# See zzz.R and https://github.com/wch/s3ops
####

times_sp <- function (e1, e2) {
  assertthat::assert_that(
    any(all(e2$end_date == e1$start_date),
      all(e1$end_date == e2$start_date)),
    all(length(e1) == length(e2))
  )
  df <- e1$value * e2$value
  d1 <- e1$start_date
  d1[d1 >= e2$start_date] <- e2$start_date[d1 >= e2$start_date]
  d2 <- e1$end_date
  d2[d2 <= e2$end_date] <- e2$end_date[d2 <= e2$end_date]

  if(all(as.character(e1$specs)!=as.character(e2$specs))) {
    specs <- CDSSpec(rank = "Empty")
    warning("Warning: The output metadata is ambiguous, so removed.")
  } else {
    specs <- e1$specs
  }
  new_SurvivalProbabilities(df, d1, d2, specs)
}

div_sp <- function (e1, e2) {
  assertthat::assert_that(all(e1$start_date == e2$start_date),
    all(length(e1) == length(e2)))
  df <- e1$value / e2$value
  d1 <- e1$end_date
  d1[d1 >= e2$end_date] <- e2$end_date[d1 >= e2$end_date]
  d2 <- e1$end_date
  d2[d2 <= e2$end_date] <- e2$end_date[d2 <= e2$end_date]

  if(all(as.character(e1$specs)!=as.character(e2$specs))) {
    specs <- CDSSpec(rank = "Empty")
    warning("Warning: The output metadata is ambiguous, so removed.")
  } else {
    specs <- e1$specs
  }
  new_SurvivalProbabilities(df, d1, d2, specs)
}


#' @export
`==.SurvivalProbabilities` <- function (e1, e2) {
  e1$value == e2$value
}

#' @export
`<.SurvivalProbabilities` <- function (e1, e2) {
  e1$value < e2$value
}

`>.SurvivalProbabilities` <- function (e1, e2) {
  e1$value > e2$value
}

#' @export
`<=.SurvivalProbabilities` <- function (e1, e2) {
  (e1 < e2) | (e1 == e2)
}

#' @export
`>=.SurvivalProbabilities` <- function (e1, e2) {
  (e1 > e2) | (e1 == e2)
}

#' @export
`!=.SurvivalProbabilities` <- function (e1, e2) {
  !(e1 == e2)
}

