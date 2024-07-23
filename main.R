# Housekeeping
source("~functions.R")
source("~specs.R")

# Run the function with the list of required packages
install_if_missing(required_packages)

if (T) {
  `%>%` <- magrittr::`%>%`
  aes <- ggplot2::aes
}

# Import raw data
gdp_data <- eurostat::get_eurostat("namq_10_gdp", time_format = "num", stringsAsFactors = TRUE) %>% 
  dplyr::mutate(geo = dplyr::if_else(geo=="EL", "GR", geo)) %>% 
  dplyr::filter(
    s_adj == "SCA", 
    unit == "CLV10_MEUR", 
    na_item == "B1GQ", 
    geo %in% emu_countries
  ) %>% 
  dplyr::mutate(
    date = zoo::as.Date(zoo::as.yearqtr(TIME_PERIOD))
  ) %>% 
  dplyr::select(geo, date, values) %>% 
  dplyr::group_by(geo) %>% 
  dplyr::mutate(
    gdp_cycle = mFilter::hpfilter(100 * log(values), freq = 1600, type = "lambda")$cycle) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(date, geo, gdp_cycle) %>% 
  tidyr::pivot_wider(names_from = geo, values_from = gdp_cycle) %>% 
  dplyr::arrange(date) %>% 
  dplyr::mutate(
    GR = dplyr::if_else(lubridate::year(date)<2001, NA, GR),
    SI = dplyr::if_else(lubridate::year(date)<2007, NA, SI),
    CY = dplyr::if_else(lubridate::year(date)<2008, NA, CY),
    MT = dplyr::if_else(lubridate::year(date)<2008, NA, MT),
    SK = dplyr::if_else(lubridate::year(date)<2009, NA, SK),
    EE = dplyr::if_else(lubridate::year(date)<2011, NA, EE),
    LV = dplyr::if_else(lubridate::year(date)<2014, NA, LV),
    LT = dplyr::if_else(lubridate::year(date)<2015, NA, LT),
    HR = dplyr::if_else(lubridate::year(date)<2023, NA, HR)
  ) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    vol_gdp = sd(dplyr::c_across(-matches("date")), na.rm = T),
    log_vol_gdp = log(sd(dplyr::c_across(-matches("date")), na.rm = T))
    ) %>%
  dplyr::ungroup() %>% 
  dplyr::arrange(date)

# Create a plot depicting GDP volatility

gdp_data %>% 
  dplyr::filter(lubridate::year(date)>=2005) %>% 
  ggplot2::ggplot(aes(x = date, y = vol_gdp)) + 
  ggplot2::geom_line(col = "darkred") + 
  ggplot2::theme_light() + 
  ggplot2::labs(x = "", y = "EMU GDP Volatility") + 
  ggplot2::geom_vline(xintercept = zoo::as.Date("2008-01-01"), linetype = "dashed") + 
  ggplot2::geom_vline(xintercept = zoo::as.Date("2012-01-01"), linetype = "dashed") + 
  ggplot2::geom_vline(xintercept = zoo::as.Date("2020-01-01"), linetype = "dashed") +
  ggplot2::geom_vline(xintercept = zoo::as.Date("2022-01-01"), linetype = "dashed") +
  ggplot2::geom_vline(xintercept = zoo::as.Date("2014-07-01"), linetype = "dashed") +
  ggplot2::geom_text(aes(x=zoo::as.Date("2008-01-01"), 
                         label="\nGreat Financial Crisis", y=3), 
                     colour="black", angle=90, size = 3.5) +
  ggplot2::geom_text(aes(x=zoo::as.Date("2012-01-01"), 
                         label="\nDebt crisis", y=3), 
                     colour="black", angle=90, size = 3.5) +
  ggplot2::geom_text(aes(x=zoo::as.Date("2020-01-01"), 
                         label="Covid-19 Crisis\n", y=3), 
                     colour="black", angle=90, size = 3.5) +
  ggplot2::geom_text(aes(x=zoo::as.Date("2022-01-01"), 
                         label="Ukraine War\n", y=3.2), 
                     colour="black", angle=90, size = 3.5) +
  ggplot2::geom_text(aes(x=zoo::as.Date("2014-07-01"), 
                         label="APP\n", y=3.2), 
                     colour="black", angle=90, size = 3.5)

# Forecasting exercise 

date_last_obs <- gdp_data$date[nrow(gdp_data)]

date_first_fcast <- date_last_obs + months(3)

gdp_data <- data.frame(date = seq(as.Date(gdp_data$date[1]), 
                                  by="quarter", 
                                  length.out = nrow(gdp_data) + fcast_horizon)) %>% 
  dplyr::left_join(gdp_data %>% dplyr::select(date, vol_gdp), by = "date") %>% 
  dplyr::mutate(
    vol_gdp_upr_95 = NA, 
    vol_gdp_lwr_95 = NA, 
    vol_gdp_upr_68 = NA, 
    vol_gdp_lwr_68 = NA)

# Generate lags 

for (mm in 1:4) {
  new_lag_name <- paste0("l", mm, "_vol_gdp")
  gdp_data <- gdp_data %>% 
    dplyr::mutate(
    !!rlang::sym(new_lag_name) := dplyr::lag(vol_gdp, n = mm)
    )
}

first_row_fcast <- which(gdp_data$date == date_first_fcast)

fcast.formula <- as.formula(paste(mod_y, paste(mod_X, collapse = " + "), sep = " ~ "))

for (jj in first_row_fcast:(first_row_fcast + fcast_horizon - 1)) {
  
  current.model <- lm(fcast.formula, gdp_data)
  current.pred <- predict(current.model, gdp_data[jj, ], 
                          interval = "confidence", level = c(.95, .8))
  
  gdp_data$vol_gdp[jj] <- current.pred[1, "fit"]
  gdp_data$vol_gdp_upr_95[jj] <- current.pred[1, "upr"]
  gdp_data$vol_gdp_lwr_95[jj] <- current.pred[1, "lwr"]
  gdp_data$vol_gdp_upr_68[jj] <- current.pred[2, "upr"]
  gdp_data$vol_gdp_lwr_68[jj] <- current.pred[2, "lwr"]
  
  
  for (mm in 1:4) {
    new_lag_name <- paste0("l", mm, "_vol_gdp")
    gdp_data <- gdp_data %>% 
      dplyr::mutate(
        !!rlang::sym(new_lag_name) := dplyr::lag(vol_gdp, n = mm)
      )
  }
  
}

gdp_data <- gdp_data %>% 
  dplyr::mutate(
    vol_gdp_upr_95 = dplyr::if_else(date == date_last_obs, vol_gdp, vol_gdp_upr_95),
    vol_gdp_lwr_95 = dplyr::if_else(date == date_last_obs, vol_gdp, vol_gdp_lwr_95),
    vol_gdp_lwr_68 = dplyr::if_else(date == date_last_obs, vol_gdp, vol_gdp_lwr_68),
    vol_gdp_upr_68 = dplyr::if_else(date == date_last_obs, vol_gdp, vol_gdp_upr_68)
  ) %>% 
  dplyr::filter(lubridate::year(date)>=2021)

# Create a plot depicting the forecast

ggplot2::ggplot() +
  ggplot2::geom_ribbon(data = gdp_data, aes(x=date, ymax=vol_gdp_upr_95, ymin=vol_gdp_lwr_95), fill="grey", alpha=.4) +
  ggplot2::geom_ribbon(data = gdp_data, aes(x=date, ymax=vol_gdp_upr_68, ymin=vol_gdp_lwr_68), fill="grey", alpha=.8) +
  ggplot2::geom_line(data = gdp_data %>% dplyr::filter(date <= date_last_obs), 
            aes(x = date, y = vol_gdp), 
            color = "darkred", linetype = "solid") +
  ggplot2::geom_line(data = gdp_data %>% dplyr::filter(date >= date_last_obs), 
            aes(x = date, y = vol_gdp), 
            color = "darkred", linetype = "dashed") +
  ggplot2::theme_light() +
  ggplot2::geom_vline(xintercept = date_last_obs, linetype = "dashed") + 
  ggplot2::labs(x = "", y = "") + 
  ggplot2::labs(x = "", y = "EMU GDP Volatility")

