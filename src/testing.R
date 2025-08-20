library(here)
library(readr)
library(dplyr)
library(fitdistrplus)
library(lubridate)
library(ggplot2)

set.seed(12)

x <- c(0, 0, 5, 6, 2, NA, 2, 7, 0.8, 0.2, 0.9, 6, 30, 50, NA, 3, 0, 0, NA)
y <- c(0, 5, 7, 7, 1, 8,  2, 8, 1.8, 6.5, 4.1, 9, 12, 20, 3,  8, 2, 8, 25)

source(here("src", "methods.R"))

thresh(x, y)
loci(x, y)
quantile_mapping(x, y, qm_method = "gamma")
quantile_mapping(x, y, qm_method = "empirical")

rwanda <- readr::read_csv(here("data", "rwanda_daily.csv"))
rwanda_stations <- readr::read_csv(here("data", "rwanda_stations.csv"))

kigali <- rwanda %>%
  filter(station_name == "KIGALI AERO") %>%
  dplyr::select(station = station_name, date, rain = precip)

kigali_tamsat <- readr::read_csv(here("data", "tamsat3.1_kigali.csv"), 
                                 na = "-999")
kigali_tamsat <- kigali_tamsat %>%
  dplyr::mutate(station = "KIGALI AERO", date = time, tamsat_rain = rfe_filled,
                .keep = "none")

kigali <- dplyr::left_join(kigali, kigali_tamsat, 
                           by = c("station", "date"))
kigali <- kigali %>% 
  mutate(year = year(date), month = month(date), 
                            day = day(date)) %>%
  filter(year >= 1983)

kigali_month <- kigali %>% 
  group_by(year, month) %>%
  filter(!is.na(rain) & !is.na(tamsat_rain)) %>%
  summarise(n_rain = sum(rain > 0.85),
            n_rain_tamsat = sum(tamsat_rain > 0.85))

ggplot(kigali_month, aes(x = year, y = n_rain, colour = "station")) +
  geom_line() +
  geom_line(aes(y = n_rain_tamsat, colour = "TAMSAT")) +
  facet_wrap(vars(month))

dodoma <- readr::read_csv(here("data", "dodoma.csv"))

dodoma <- dodoma %>%
  dplyr::select(date, rain)

dodoma_tamsat <- readr::read_csv(here("data", "tamsat3.1_dodoma.csv"), 
                                 na = "-999")
dodoma_tamsat <- dodoma_tamsat %>%
  dplyr::mutate(date = time, tamsat_rain = rfe_filled,
                .keep = "none")
dodoma <- dplyr::left_join(dodoma, dodoma_tamsat, 
                           by = "date")
dodoma <- dodoma %>% 
  mutate(year = year(date), month = month(date), 
         day = day(date),
         rainday = rain > 0.85,
         rainday_lag = dplyr::lag(rainday, default = NA),
         season = ifelse(month %in% 5:10, "dry", as.character(month)),
         season = factor(season, levels = c("dry", 11:12, 1:4))) %>%
  group_by(season) %>%
  mutate(t_tamsat = thresh(rain, tamsat_rain),
         tamsat_rainday = tamsat_rain > t_tamsat,
         tamsat_rainday_lag = dplyr::lag(tamsat_rainday, default = NA)) %>%
  filter(year >= 1983)

dodoma_month <- dodoma %>% 
  group_by(season) %>%
  filter(!is.na(rain) & !is.na(tamsat_rain)) %>%
  summarise(n = n(),
            n_rain = sum(rainday),
            n_rain_tamsat = sum(tamsat_rainday),
            n_rain_w = sum(rainday[rainday_lag], na.rm = TRUE),
            n_rain_w_tamsat = sum(tamsat_rainday[tamsat_rainday_lag], 
                                   na.rm = TRUE),
            n_rain_d = sum(rainday[!rainday_lag], na.rm = TRUE),
            n_rain_d_tamsat = sum(tamsat_rainday[!tamsat_rainday_lag], 
                                   na.rm = TRUE))

source(here("src", "methods.R"))
res6 <- markov_thresholds(dodoma, "rain", "tamsat_rain", season_col = "season", 
                         tol = 1e-2, max_it = 20)


zimbabwe <- readr::read_csv(here("data", "zim_five_stations.csv"))
