rm(list=ls())
gdp_data <- read.csv("~/Downloads/namq_10_gdp_page_linear.csv")

`%>%` <- dplyr::`%>%`

clean_data <- gdp_data %>% 
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
    date = zoo::as.yearqtr(TIME_PERIOD, format = "%Y-Q%q")
  ) %>% 
  dplyr::select(geo, date, gdp_cycle) %>% 
  dplyr::filter(lubridate::year(date)>=2000) %>% 
  tidyr::pivot_wider(names_from = geo, values_from = gdp_cycle
  ) %>% 
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
  dplyr::ungroup() %>% 
  dplyr::mutate(
    lag_st = vol_gdp - dplyr::lag(vol_gdp)
  )



