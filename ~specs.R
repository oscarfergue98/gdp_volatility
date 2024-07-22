# List of required packages in the session
required_packages <- c("dplyr", "ggplot2", "mFilter", "zoo", "lubridate", "tidyr")

# Create raw data path
raw_data_path <- "data"

# Create vector of EMU countries 
emu_countries <- c("AT", "BE", "HR", "CY", "EE", "FI", "FR", "DE", "EL", "IE", 
                   "IT", "LV", "LT", "LU", "MT", "NL", "PT", "SK", "SI", "ES")

# Forecast horizon 

fcast_horizon <- 4