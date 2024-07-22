# Housekeeping
source("~functions.R")
source("~specs.R")

# List of packages you want to check/install

# Run the function with the list of required packages
install_if_missing(required_packages)

if (T) {
  `%>%` <- dplyr::`%>%`
  aes <- ggplot2::aes
}

# Import raw data
raw_gdp_data <- read.csv(file.path(raw_data_path, "namq_10_gdp_page_linear.csv"))

# Modify raw data accordingly
gdp_data <- raw_gdp_data %>% 
  dplyr::select(
    geo, TIME_PERIOD, OBS_VALUE
    ) %>% 
  dplyr::filter(
    geo %in% emu_countries    ) %>% 
  dplyr::mutate(
    geo = dplyr::if_else(geo=="EL", "GR", geo)
  ) %>% 
  dplyr::mutate(
    date = zoo::as.Date(zoo::as.yearqtr(TIME_PERIOD, format = "%Y-Q%q"))
  ) %>% 
  #dplyr::filter(date<=as.Date("2019-04-01"), date>=as.Date("1990-01-01")) %>% 
  dplyr::group_by(geo) %>% 
  dplyr::mutate(
    gdp_cycle = mFilter::hpfilter(100 * log(OBS_VALUE), freq = 1600, type = "lambda")$cycle) %>% 
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
  ggplot2::geom_line(col = "black") + 
  ggplot2::theme_light() + 
  ggplot2::labs(x = "", y = "EMU GDP Volatility") + 
  ggplot2::geom_vline(xintercept = zoo::as.Date("2008-01-01"), linetype = "dashed") + 
  ggplot2::geom_vline(xintercept = zoo::as.Date("2012-01-01"), linetype = "dashed") + 
  ggplot2::geom_vline(xintercept = zoo::as.Date("2020-01-01"), linetype = "dashed") 

# Forecasting exercise 

date_last_obs <- gdp_data$date[nrow(gdp_data)]
date_first_fcast <- date_last_obs + months(3)

gdp_data <- data.frame(date = seq(as.Date(gdp_data$date[1]), 
                                  by="quarter", 
                                  length.out = nrow(gdp_data) + fcast_horizon)) %>% 
  dplyr::left_join(gdp_data %>% dplyr::select(date, vol_gdp), by = "date") %>% 
  dplyr::mutate(
    lag_vol_gdp = dplyr::lag(vol_gdp),
    vol_gdp_upr_95 = NA, 
    vol_gdp_lwr_95 = NA, 
    vol_gdp_upr_68 = NA, 
    vol_gdp_lwr_68 = NA, 
    d_covid = date %in% c(as.Date("2020-04-01"), as.Date("2020-07-01"), 
                          as.Date("2020-10-01")))


first_row_fcast <- which(gdp_data$date == date_first_fcast)


for (jj in first_row_fcast:(first_row_fcast + fcast_horizon - 1)) {
  
  current.model <- lm(vol_gdp ~ lag_vol_gdp, gdp_data)
  current.pred <- predict(current.model, gdp_data[jj, ], 
                          interval = "confidence", level = c(.95, .8))
  
  gdp_data$vol_gdp[jj] <- current.pred[1, "fit"]
  gdp_data$vol_gdp_upr_95[jj] <- current.pred[1, "upr"]
  gdp_data$vol_gdp_lwr_95[jj] <- current.pred[1, "lwr"]
  gdp_data$vol_gdp_upr_68[jj] <- current.pred[2, "upr"]
  gdp_data$vol_gdp_lwr_68[jj] <- current.pred[2, "lwr"]
  
  
  gdp_data <- gdp_data %>% 
    dplyr::mutate(
      lag_vol_gdp = dplyr::lag(vol_gdp)
    )
  
}


# Create a plot depicting the forecast

gdp_data %>% 
  dplyr::filter(lubridate::year(date)>=2022) %>% 
  dplyr::mutate(
    date_str = dplyr::if_else(date>=date_first_fcast, "Forecast", "Historical")
  ) %>% 
  ggplot2::ggplot(aes(x = date)) + 
  ggplot2::geom_ribbon(aes(x=date, ymax=vol_gdp_upr_95, ymin=vol_gdp_lwr_95), fill="grey", alpha=.3) +
  ggplot2::geom_ribbon(aes(x=date, ymax=vol_gdp_upr_68, ymin=vol_gdp_lwr_68), fill="grey", alpha=.8) +
  ggplot2::geom_line(aes(y = vol_gdp), col = "darkblue") + 
  ggplot2::theme_light() + 
  ggplot2::labs(x = "", y = "EMU GDP Volatility (in logs)") + 
  ggplot2::geom_vline(xintercept = zoo::as.Date("2008-01-01"), linetype = "dashed") + 
  ggplot2::geom_vline(xintercept = zoo::as.Date("2012-01-01"), linetype = "dashed") + 
  ggplot2::geom_vline(xintercept = zoo::as.Date("2020-01-01"), linetype = "dashed") +
  ggplot2::geom_vline(xintercept = as.Date("2024-04-01"), linetype = "dashed")
