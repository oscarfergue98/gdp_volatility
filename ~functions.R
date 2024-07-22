# Function to check whether a package is installed or not
# If not installed, it installs the package

install_if_missing <- function(packages) {
  # Check for missing packages
  missing_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  
  # Install missing packages
  if(length(missing_packages) > 0) {
    install.packages(missing_packages)
  } else {
    message("All packages are already installed.")
  }
}