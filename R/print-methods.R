format.CDSSpec <- function(x,...){
  paste0("<Curve Specification> \n",
         "Rank: ", x$rank, "\n")
}

print.CDSSpec <- function(x, ...){
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

format.SurvivalCurve <- function(x, ...){
  paste0(
    "<CDSCurve as of ", x$reference_date, "> \n",
    "Tenors: ", paste(x$tenors, collapse = " "), "\n",
    "Survival Probabilities: ", paste(x$probabilities, collapse = " "), "\n",
    "--------------\n"
  )
}

print.SurvivalCurve <- function(x, ...){
  cat(format(x, ...), "\n")
  cat(format(x$specs,...), "\n")
}

format.HazardCurve <- function(x, ...){
  paste0(
    "<CDSCurve as of ", x$reference_date, "> \n",
    "Tenors: ", paste(x$tenors, collapse = " "), "\n",
    "Survival Hazard Rate: ", paste(x$hazard_rates, collapse = " "), "\n",
    "--------------\n"
  )
}

print.HazardCurve <- function(x, ...){
  cat(format(x, ...), "\n")
  cat(format(x$specs,...), "\n")
}