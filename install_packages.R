# Package Installation Script for Dashboard Analisis Statistik
# install_packages.R

# Function to install packages if not already installed
install_if_missing <- function(pname) {
  if (!require(pname, character.only = TRUE)) {
    install.packages(pname, dependencies = TRUE)
    library(pname, character.only = TRUE)
  }
}

# List of all packages required by the dashboard
packages_to_install <- c(
  "shiny",
  "shinydashboard",
  "DT",
  "plotly",
  "ggplot2",
  "dplyr",
  "officer",
  "car",       # Untuk VIF dan ncvTest di regresi linear, dan uji Levene
  "lmtest",    # Untuk uji Breusch-Pagan
  "gridExtra", # Untuk menggabungkan plot
  "readr",     # Untuk read_csv
  "data.table", # Untuk fread (baca data cepat)
  "moments",   # Untuk skewness dan kurtosis
  "classInt",  # Untuk Jenks Natural Breaks
  "shinycssloaders", # Untuk withSpinner function
  "sf",        # Untuk membaca dan memproses file GeoJSON
  "RColorBrewer" # Untuk palet warna peta (opsional, sudah termasuk dalam ggplot2)
)

# Install and load all packages
for (package in packages_to_install) {
  install_if_missing(package)
}
