library(here)
library(readr)
library(dplyr)
library(fitdistrplus)
library(lubridate)
library(ggplot2)
library(purrr)

zimbabwe <- readr::read_csv(here("data", "zimbabwe_with_sre.csv"))

zimbabwe <- zimbabwe %>%
  filter(date >= as.Date("1979-01-01") & date <= as.Date("2023-06-30")) %>%
  mutate(season = factor(season, levels = c("dry", 10:12, 1:4)))

zimbabwe_stations <- readr::read_csv(here("data", "zimbabwe_stations.csv"))

source(here("src", "methods.R"))

m_thresh <- markov_thresholds(zimbabwe, obs_col = "rain", est_col = "agera5_rain",
                              season_col = "season", station_col = "station",
                              tol = 1e-2, max_it = 20)
