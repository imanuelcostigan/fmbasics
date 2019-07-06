

is.CDSCurve<- function(x)inherits(x, "CDSCurve")

is.CDSSpecs<- function(x)inherits(x, "CDSSpecs")




bootstrap_survprob <- function(cds_curve, zero_curve, ...) UseMethod("bootstrap_survprob")

#' Bootstraps Survival Probabilitie from a CDS curve
#' Using \href{https://www.rdocumentation.org/packages/credule/versions/0.1.3}{credule package.}
#'
#' @param cds_curve An object of type `CDSCurve`
#' @param zero_curve An object of type `ZeroCurve`
#' @param num_timesteps_pa It represents the number of timesteps used to perform the numerical integral
#'  required while computing the default leg value. It is shown that a monthly discretisation
#'   usually gives a good approximation (Ref. Valuation of Credit Default Swaps, Dominic O Kane and
#'   Stuart Turnbull)
#' @param accrued_premium If set to TRUE, the accrued premium will be taken into account in the calculation of the premium leg value.
#'
#' @return An object of type `SurvivalProbCurve`
#' @examples
#' zero_curve <- build_zero_curve()
#' specs <- CDSMarkitSpecs(rating = "AAA", region = "Japan", sector = "Utilities")
#' cds_curve <- CDSCurve(reference_date = zero_curve$reference_date,
#' tenors = c(1,3,5,7), spreads = c(0.0050,0.0070,0.0090,0.0110), lgd = .6,
#' premium_frequency = 4, specs = specs)
#' bootstrap_survprob(cds_curve = cds_curve, zero_curve = zero_curve)
#'
#'
bootstrap_survprob.CDSCurve <- function(cds_curve,
                                        zero_curve,
                                        num_timesteps_pa = 12,
                                        accrued_premium = TRUE)
{
  if(cds_curve$reference_date != zero_curve$reference_date)
    stop("The reference dates for CDS Curve and the Zero Curve are different", call. = FALSE)
  if(!is.ZeroCurve(zero_curve))
    stop("zero_curve must be an object of type ZeroCurve", call. = FALSE)

  sp_output <- credule::bootstrapCDS(
    yieldcurveTenor = zero_curve$pillar_times,
    yieldcurveRate = zero_curve$pillar_zeros,
    cdsTenors = cds_curve$tenors,
    cdsSpreads = cds_curve$spread,
    recoveryRate = 1 - cds_curve$lgd,
    numberPremiumPerYear = cds_curve$premium_frequency,
    numberDefaultIntervalPerYear = num_timesteps_pa,
    accruedPremium = accrued_premium
  )

  SurvivalProbCurve(
    reference_date = cds_curve$reference_date,
    tenors = cds_curve$tenors,
    probabilities = sp_output$survprob,
    specs = cds_curve$specs
  )
}


#' Bootstraps Hazard Rate from a CDS curve
#' Using \href{https://www.rdocumentation.org/packages/credule/versions/0.1.3}{credule package.}
#'
#' @param cds_curve An object of type `CDSCurve`
#' @param zero_curve An object of type `ZeroCurve`
#' @param num_timesteps_pa It represents the number of timesteps used to perform the numerical integral
#'  required while computing the default leg value. It is shown that a monthly discretisation
#'   usually gives a good approximation (Ref. Valuation of Credit Default Swaps, Dominic O Kane and
#'   Stuart Turnbull)
#' @param accrued_premium If set to TRUE, the accrued premium will be taken into account in the calculation of the premium leg value.
#'
#' @return An object of type `HazardRate`
#' @examples
#' zero_curve <- build_zero_curve()
#' specs <- CDSMarkitSpecs(rating = "AAA", region = "Japan", sector = "Utilities")
#' cds_curve <- CDSCurve(reference_date = zero_curve$reference_date,
#' tenors = c(1,3,5,7), spreads = c(0.0050,0.0070,0.0090,0.0110), lgd = .6,
#' premium_frequency = 4, specs = specs)
#' bootstrap_hazardrate(cds_curve = cds_curve, zero_curve = zero_curve)
#'
#'


bootstrap_hazardrate <- function(cds_curve, zero_curve, ...)
  UseMethod("bootstrap_hazardrate")

bootstrap_hazardrate.CDSCurve <- function(cds_curve,
                                          zero_curve,
                                          num_timesteps_pa = 12,
                                          accrued_premium = TRUE)
{
  if (cds_curve$reference_date != zero_curve$reference_date)
    stop("The reference dates for CDS Curve and the Zero Curve are different",
         call. = FALSE)
  if(!is.ZeroCurve(zero_curve))
    stop("zero_curve must be an object of type ZeroCurve", call.= FALSE)

  hr_output <- credule::bootstrapCDS(
    yieldcurveTenor = zero_curve$pillar_times,
    yieldcurveRate = zero_curve$pillar_zeros,
    cdsTenors = cds_curve$tenors,
    cdsSpreads = cds_curve$spread,
    recoveryRate = 1 - cds_curve$lgd,
    numberPremiumPerYear = cds_curve$premium_frequency,
    numberDefaultIntervalPerYear = num_timesteps_pa,
    accruedPremium = accrued_premium
  )

  HazardRate(
    reference_date = cds_curve$reference_date,
    tenors = cds_curve$tenors,
    hazard_rates = hr_output$hazrate,
    specs = cds_curve$specs
  )
}