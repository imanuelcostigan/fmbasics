format.CDSSpecs <- function(x,...){
  paste0("<Curve Specifications> \n",
         "Rank: ", x$rank, "\n")
}

print.CDSSpecs <- function(x, ...){
  cat(format(x, ...), "\n")
}

format.CDSMarkitSpecs <- function(x,...){
  paste0("<Curve Specifications: Markit CDS Sector Curve> \n",
         "Rank: SNR \n",
         "Rating: ", x$rating, "\n",
         "Region: " , x$region, "\n",
         "Sector: " , x$sector, "\n")
}

print.CDSMarkitSpecs <- function(x, ...){
  cat(format(x, ...), "\n")
}

format.CDSSingleNameSpecs <- function(x,...){
  paste0("<Curve Specifications: Single Name CDS Curve> \n",
         "Rank: ", x$rank, "\n",
         "Name: ", x$name, "\n")
}

print.CDSSingleNameSpecs <- function(x, ...){
  cat(format(x, ...), "\n")
}


format.CDSCurve <- function(x, ...){
  paste0(
    "<CDSCurve as of ", x$reference_date, "> \n",
    "Tenors: ", paste(x$tenors, collapse = " "), "\n",
    "Spreads: ", paste(x$spread, collapse = " "), "\n",
    "LGD: ", x$LGD, "\n",
    "Premium Frequency: ", x$premium_frequency, "\n",
    "--------------\n"
  )
}

print.CDSCurve <- function(x, ...){
  cat(format(x, ...), "\n")
  cat(format(x$specs,...), "\n")
}

format.SurvivalProbCurve <- function(x, ...){
  paste0(
    "<CDSCurve as of ", x$reference_date, "> \n",
    "Tenors: ", paste(x$tenors, collapse = " "), "\n",
    "Survival Probabilities: ", paste(x$probabilities, collapse = " "), "\n",
    "--------------\n"
  )
}

print.SurvivalProbCurve <- function(x, ...){
  cat(format(x, ...), "\n")
  cat(format(x$specs,...), "\n")
}

format.HazardRate <- function(x, ...){
  paste0(
    "<CDSCurve as of ", x$reference_date, "> \n",
    "Tenors: ", paste(x$tenors, collapse = " "), "\n",
    "Survival Hazard Rate: ", paste(x$hazard_rates, collapse = " "), "\n",
    "--------------\n"
  )
}

print.HazardRate <- function(x, ...){
  cat(format(x, ...), "\n")
  cat(format(x$specs,...), "\n")
}