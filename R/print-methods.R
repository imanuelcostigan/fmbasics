format.CDSSpec <- function(x,...){
  paste0("<Curve Specification> \n",
         "Rank: ", x$rank, "\n")
}

print.CDSSpec <- function(x, ...){
  cat(format(x, ...), "\n")
}

format.CDSMarkitSpec <- function(x,...){
  paste0("<Curve Specification: Markit CDS Sector Curve> \n",
         "Rank: SNR \n",
         "Rating: ", x$rating, "\n",
         "Region: " , x$region, "\n",
         "Sector: " , x$sector, "\n")
}

print.CDSMarkitSpec <- function(x, ...){
  cat(format(x, ...), "\n")
}

format.CDSSingleNameSpec <- function(x,...){
  paste0("<Curve Specification: Single Name CDS Curve> \n",
         "Rank: ", x$rank, "\n",
         "Name: ", x$name, "\n")
}

print.CDSSingleNameSpec <- function(x, ...){
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