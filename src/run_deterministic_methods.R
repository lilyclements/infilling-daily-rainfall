library(here)
library(readr)
library(dplyr)
library(fitdistrplus)
library(lubridate)
library(ggplot2)
library(purrr)


# Setup -------------------------------------------------------------------

zimbabwe <- readr::read_csv(here("data", "zimbabwe_with_sre.csv"))

zimbabwe <- zimbabwe %>%
  filter(date >= as.Date("1979-01-01") & date <= as.Date("2023-06-30")) %>%
  mutate(season = factor(season, levels = c("dry", 10:12, 1:4)))

zimbabwe_stations <- readr::read_csv(here("data", "zimbabwe_stations.csv"))

source(here("src", "methods.R"))

# Testing method correctness ----------------------------------------------

m_thresh <- markov_thresholds(zimbabwe, obs_col = "rain", est_col = "agera5_rain",
                              season_col = "season", station_col = "station",
                              tol = 1e-2, max_it = 20)

m_thresh$season <- factor(m_thresh$season, levels = c("dry", 10:12, 1:4))

zimbabwe_t <- zimbabwe %>% 
  left_join(m_thresh %>% dplyr::select(station, season, t0, t_w, t_d), 
            by = c("station", "season"))

zimbabwe_t <- zimbabwe_t %>%
  mutate(agera5_bc_rainday = conditional_wd(agera5_rain, t_w, t_d, t0),
         agera5_bc_rainday_lag = lag(agera5_bc_rainday))

zimbabwe_by_season <- zimbabwe_t %>% 
  group_by(station, season) %>%
  summarise(p = mean(rainday, na.rm = TRUE),
            p_bc = mean(agera5_bc_rainday, na.rm = TRUE),
            p_w = mean(rainday[rainday_lag], na.rm = TRUE),
            p_w_bc = mean(agera5_bc_rainday[agera5_bc_rainday_lag], na.rm = TRUE),
            p_d = mean(rainday[!rainday_lag], na.rm = TRUE),
            p_d_bc = mean(agera5_bc_rainday[!agera5_bc_rainday_lag], na.rm = TRUE))


# Generate BC data --------------------------------------------------------

source(here("src", "methods.R"))

zimbabwe_bc <- markov_loci(zimbabwe, obs_col = "rain", est_col = "agera5_rain", 
                 season_col = "season", station_col = "station",
                 blocks = as.Date(c("1979-01-01", "1989-01-01", "1999-01-01", 
                                    "2009-01-01", "2023-07-01")))

saveRDS(zimbabwe_bc, here("data", "BC_data", "zimbabwe_agera5_bc_det.RDS"))
