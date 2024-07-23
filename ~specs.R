# List of required packages in the session
required_packages <- c("dplyr", "ggplot2", "mFilter", "zoo", 
                       "lubridate", "tidyr", "eurostat")

# Create raw data path
raw_data_path <- "data"

# Create vector of EMU countries 
emu_countries <- c("AT", "BE", "CY", "DE", "EE", "ES", "FI", 
                   "FR", "GR", "HR", "IE", "IT", "LT", "LU", 
                   "LV", "MT", "NL", "PT", "SI", "SK")

# Forecast horizon 

fcast_horizon <- 4

# Select variables for the models 

mod_y <- "vol_gdp"
mod_X <- c("l1_vol_gdp", "l2_vol_gdp", "l3_vol_gdp", "l4_vol_gdp")