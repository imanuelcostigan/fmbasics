#
#
# disc_curve <- data.frame(readxl::read_excel(path = "H:/Desktop/fmbasics_zc_check.xlsx",
#                                             sheet = "New disc curve",range = "C3:E30",col_names = T ))
# disc_curve$Start_date <- as.Date(disc_curve$Start_date, tz = "Australia/Sydney")
# disc_curve$end_date <- as.Date(disc_curve$end_date, tz = "Australia/Sydney")
#
#
#
# disc_factors <- fmbasics::DiscountFactor(disc_curve$dfs, d1 = disc_curve$Start_date, d2 = disc_curve$end_date)
#
# zero_curve <- fmbasics::ZeroCurve(discount_factors = disc_factors,reference_date = disc_curve$Start_date[1],interpolation = fmbasics::LogDFInterpolation())
#
# interpolation_dates <- data.frame(readxl::read_excel(path = "H:/Desktop/fmbasics_zc_check.xlsx",
#                                                      sheet = "New disc curve",range = "K4:K19",col_names = F ))
# interpolation_dates$X__1 <- as.Date(interpolation_dates$X__1, tz = "Australia/Sydney")
#
# interpolated_dfs <- interpolate_dfs(x = zero_curve, from = disc_curve$Start_date[1], to = interpolation_dates$X__1)
#
# interest_rates <- as_InterestRate(x = disc_factors, compounding = Inf,day_basis = "act/365")
#
#
# fr_interpolation_data <- data.frame(readxl::read_excel(path = "H:/Desktop/fmbasics_zc_check.xlsx",
#                                                        sheet = "New disc curve",range = "P4:Q19",col_names = F ))
# fr_interpolation_data$X__1 <- as.Date(fr_interpolation_data$X__1, tz = "Australia/Sydney")
# fr_interpolation_data$X__2 <- as.Date(fr_interpolation_data$X__2, tz = "Australia/Sydney")
#
# interpolated_fwds <- interpolate_fwds(x = zero_curve,from = fr_interpolation_data$X__1, to = fr_interpolation_data$X__2)
#
#
# interpolated_zeros <- fmbasics::interpolate_zeros(x = zero_curve, at = fr_interpolation_data$X__1, compounding = Inf, day_basis = "act/365"  )
#
#
#
#
# ####VolSurface tesing script
#
# reference_date <- as.Date("2019-04-26")
#
#
#
#
# src_file <-  "V:/19 Reviews/Markets/Self-Funding Instalment Warrants - SFI/02 Work Performed/SFI_validation/Validation inputs-Mx rprt/Murex reports/mxCompleteVolSurface_26042019.csv"
# vols_src <- read.csv(file = src_file, header = T )
# rio_spot <- 97.62
# vol_rio <- vols_src[which(vols_src$Code == "RIO ASX"), ]
# tenors <- lubridate::ymd(vol_rio$Date)
# strikes <- seq(from = 0.05, to = 2.55, by= 0.05)*rio_spot
# vol_quotes <- vol_rio[,-c(1:4) ]
# colnames(vol_quotes) <- strikes
# rownames(vol_quotes) <- tenors
# vol_quotes <- t(vol_quotes)
#
#
#
#
# vol_interp <- fmbasics::LinearTimeVarInterpolation()
# vol_obj <- fmbasics::VolSurface(reference_date = reference_date, vol_quotes = vol_quotes,
#                                 ticker = "RIOASX", surface_type = "strike/tenor", interpolation = vol_interp)
#
#
#
# dates_str <- c(
#   "25/07/2019",
#   "23/10/2019",
#   "21/01/2020",
#   "20/04/2020",
#   "19/07/2020",
#   "17/10/2020",
#   "15/01/2021",
#   "15/04/2021",
#   "14/07/2021",
#   "12/10/2021",
#   "10/01/2022",
#   "10/04/2022",
#   "9/07/2022",
#   "7/10/2022",
#   "5/01/2023",
#   "5/04/2023",
#   "4/07/2023",
#   "2/10/2023",
#   "31/12/2023",
#   "30/03/2024",
#   "28/06/2024",
#   "26/09/2024",
#   "25/12/2024",
#   "25/03/2025",
#   "23/06/2025")
#
# maturity <- lubridate::as_date(dates_str , format = "%d/%m/%Y", tz = "Australia/Sydney" )
# strike <- c(62,66,70,74,78,82,86,90,94,98,102,106,110,114,118, 122,126,130,134,138,142,146,150,154,158)
#
#
# interpolated_vols <- fmbasics::interpolate_vol(x = vol_obj, maturity = maturity, strike = strike)
#
# zc <- fmbasics::build_zero_curve()
#
# zc$interpolator(50)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
