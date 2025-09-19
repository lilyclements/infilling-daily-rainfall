library(ag5Tools)
library(terra)
library(lubridate)
library(here)
library(dplyr)

zimbabwe_stations <- readr::read_csv(here("data", "zimbabwe_stations.csv"))

# AgERA5 rainfall setup
varname <- "precipitation_flux"

# Time range
start_year <- 1979
end_year   <- 2024

# Folder for netCDF downloads
out_path <- here("data", "agera5")
if (!dir.exists(out_path)) dir.create(out_path, recursive = TRUE)

# --- DOWNLOAD DATA (yearly files) ---
for (yr in seq(start_year, end_year)) {
  message("Downloading year ", yr)
  ag5_download(
    variable  = varname,
    year      = yr,
    month     = "all",
    day       = "all",
    path      = out_path,
    version   = "2_0"
  )
}