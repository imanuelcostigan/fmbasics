.onLoad <- function (...) {
  # See: https://github.com/wch/s3ops
  registerS3method("+", "InterestRate", plus_ir)
  registerS3method("*", "InterestRate", times_ir)
  registerS3method("-", "InterestRate", minus_ir)
  registerS3method("/", "InterestRate", div_ir)
  registerS3method("*", "DiscountFactor", times_df)
  registerS3method("/", "DiscountFactor", div_df)
}

