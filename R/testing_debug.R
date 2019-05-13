

disc_curve <- data.frame(readxl::read_excel(path = "H:/Desktop/fmbasics_zc_check.xlsx",
                                            sheet = "New disc curve",range = "C3:E30",col_names = T ))
disc_curve$Start_date <- as.Date(disc_curve$Start_date, tz = "Australia/Sydney")
disc_curve$end_date <- as.Date(disc_curve$end_date, tz = "Australia/Sydney")



disc_factors <- fmbasics::DiscountFactor(disc_curve$dfs, d1 = disc_curve$Start_date, d2 = disc_curve$end_date)

zero_curve <- fmbasics::ZeroCurve(discount_factors = disc_factors,reference_date = disc_curve$Start_date[1],interpolation = LogDFInterpolation())

interpolation_dates <- data.frame(readxl::read_excel(path = "H:/Desktop/fmbasics_zc_check.xlsx",
                                                     sheet = "New disc curve",range = "K4:K19",col_names = F ))
interpolation_dates$X__1 <- as.Date(interpolation_dates$X__1, tz = "Australia/Sydney")

interpolated_dfs <- interpolate_dfs(x = zero_curve, from = disc_curve$Start_date[1], to = interpolation_dates$X__1)

interest_rates <- as_InterestRate(x = disc_factors, compounding = Inf,day_basis = "act/365")


fr_interpolation_data <- data.frame(readxl::read_excel(path = "H:/Desktop/fmbasics_zc_check.xlsx",
                                                       sheet = "New disc curve",range = "P4:Q19",col_names = F ))
fr_interpolation_data$X__1 <- as.Date(fr_interpolation_data$X__1, tz = "Australia/Sydney")
fr_interpolation_data$X__2 <- as.Date(fr_interpolation_data$X__2, tz = "Australia/Sydney")

interpolated_fwds <- interpolate_fwds(x = zero_curve,from = fr_interpolation_data$X__1, to = fr_interpolation_data$X__2)


interpolated_zeros <- fmbasics::interpolate_zeros(x = zero_curve, at = fr_interpolation_data$X__1, compounding = Inf, day_basis = "act/365"  )