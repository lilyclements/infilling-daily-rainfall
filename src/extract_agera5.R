library(ag5Tools)
library(terra)
library(lubridate)
library(here)
library(dplyr)

zimbabwe_stations <- readr::read_csv(here("data", "zimbabwe_stations.csv"))

# AgERA5 rainfall setup
varname <- "Precipitation-Flux"

# Time range
start_year <- 1979
end_year   <- 2024

# Folder for netCDF downloads
out_path <- here("data", "agera5")

# --- EXTRACT DATA ---

rain_df <- data.frame()  # empty container

for (i in 1:nrow(zimbabwe_stations)) {
  print(i)
  loc <- zimbabwe_stations[i, ]
  
  vals <- ag5_extract(
    coords    = c(loc$lon, loc$lat),
    dates     = c(as.Date("1979-01-01"), as.Date("2024-12-31")),
    variable  = varname,
    path      = out_path,
    version   = "2_0"
  )
  
  tmp <- data.frame(
    station = loc$station,
    date    = as.Date(names(vals)),
    agera5_rain = as.numeric(vals)
  )
  
  rain_df <- bind_rows(rain_df, tmp)
}
write.csv(rain_df, here("data", "agera5_zimbabwe.csv"), row.names = FALSE)
