
# Housekeeping
if (T) {
  `%>%` <- dplyr::`%>%`
  aes <- ggplot2::aes
}

# Import raw data
raw_gdp_data <- read.csv("~/Downloads/namq_10_gdp_page_linear.csv")

# Modify raw data accordingly
gdp_data <- raw_gdp_data %>% 
  dplyr::select(geo, TIME_PERIOD, OBS_VALUE) %>% 
  dplyr::filter(
    geo %in% c("AT", "BE", "HR", "CY", "EE", "FI", "FR", "DE", "EL", "IE", 
               "IT", "LV", "LT", "LU", "MT", "NL", "PT", "SK", "SI", "ES")
  ) %>% 
  dplyr::mutate(
    geo = dplyr::if_else(geo=="EL", "GR", geo)
  ) %>% 
  dplyr::group_by(geo) %>% 
  dplyr::mutate(
    gdp_cycle = mFilter::hpfilter(100*log(OBS_VALUE), freq = 1600)$cycle) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(
    date = zoo::as.Date(zoo::as.yearqtr(TIME_PERIOD, format = "%Y-Q%q"))
  ) %>% 
  dplyr::select(geo, date, gdp_cycle) %>% 
  dplyr::filter(lubridate::year(date)>=2000) %>% 
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
    vol_gdp = log(sd(dplyr::c_across(-matches("date")), na.rm = T))
    ) %>%
  dplyr::ungroup() 

# Create a plot depicting GDP volatility
gdp_data %>% 
  ggplot2::ggplot(aes(x = date, y = vol_gdp)) + 
  ggplot2::geom_line(col = "darkblue") + 
  ggplot2::theme_light() + 
  ggplot2::labs(x = "", y = "EMU GDP Volatility (in logs)") + 
  ggplot2::geom_vline(xintercept = zoo::as.Date("2008-01-01"), linetype = "dashed") + 
  ggplot2::geom_vline(xintercept = zoo::as.Date("2012-01-01"), linetype = "dashed") + 
  ggplot2::geom_vline(xintercept = zoo::as.Date("2020-01-01"), linetype = "dashed")

# Forecasting exercise 

gdp_data <- data.frame(date = seq(as.Date("2000-01-01"), 
                                  by="quarter", 
                                  length.out = nrow(gdp_data) + 4)) %>% 
  dplyr::left_join(gdp_data %>% dplyr::select(date, vol_gdp), by = "date") %>% 
  dplyr::mutate(lag_vol_gdp = dplyr::lag(vol_gdp))

last_obs_row <- which(gdp_data$date == "2024-01-01")


for (jj in last_obs_row:(last_obs_row + 4)) {
  
  current.model = lm(vol_gdp ~ lag_vol_gdp, gdp_data)
  gdp_data$vol_gdp[jj] = predict(current.model, gdp_data)
  
}
