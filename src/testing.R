library(here)
library(readr)
library(dplyr)
library(fitdistrplus)
library(lubridate)
library(ggplot2)
library(purrr)

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

zimbabwe_stations <- readr::read_csv(here("data", "zimbabwe_stations.csv"))
library(maps)
zimbabwe_map <- map_data("world", region = "Zimbabwe")
ggplot() +
  geom_polygon(
    data = zimbabwe_map,
    aes(x = long, y = lat, group = group),
    fill = "gray90", color = "black"
  ) +
  geom_point(
    data = zimbabwe_stations,
    aes(x = lon, y = lat),
    color = "red", size = 3
  ) +
  geom_text(
    data = zimbabwe_stations,
    aes(x = lon, y = lat, label = station),
    vjust = -1, size = 3
  ) +
  coord_quickmap() +
  theme_minimal() +
  labs(title = "Station Locations in Zimbabwe",
       x = "Longitude", y = "Latitude")

zimbabwe_tamsat_list <- list()
for (i in seq_along(zimbabwe_stations$station)) {
  s <- zimbabwe_stations$station[i]
  zimbabwe_tamsat_list[[i]] <- read_csv(here("data", paste0("tamsat3.1_", s, ".csv")), 
                                   na = "-999")
}
names(zimbabwe_tamsat_list) <- zimbabwe_stations$station
zimbabwe_tamsat <- bind_rows(zimbabwe_tamsat_list, .id = "station")
zimbabwe_tamsat <- zimbabwe_tamsat %>%
  dplyr::mutate(station = station, date = time, tamsat_rain = rfe_filled,
                .keep = "none")
zimbabwe <- dplyr::left_join(zimbabwe, zimbabwe_tamsat,
                             by = c("station", "date"))
rm(zimbabwe_tamsat)
rm(zimbabwe_tamsat_list)

zimbabwe <- zimbabwe %>%
  filter(date >= as.Date("1983-01-01") & date <= as.Date("2023-06-30"))

zimbabwe <- zimbabwe %>%
  mutate(year = year(date), 
         month = month(date), 
         day = day(date),
         rainday = rain > 0.85,
         rainday_lag = dplyr::lag(rainday),
         #rainday_lag = dplyr::lag(rainday, default = NA),
         #tamsat_rainday = tamsat_rain > 0.85,
         #tamsat_rainday_lag = dplyr::lag(tamsat_rainday, default = NA),
         season = ifelse(month %in% 5:9, "dry", as.character(month)),
         season = factor(season, levels = c("dry", 10:12, 1:4)))

zimbabwe_month <- zimbabwe %>% 
  group_by(station, season) %>%
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

library(tidyverse)
library(here)
source(here("src", "methods.R"))

zimbabwe %>% 
  group_by(station) %>% 
  mutate(rain_wd = rain > 0.85) %>%
  filter(tamsat_rain == 0) %>% 
  summarise(p0 = mean(rain_wd, na.rm = TRUE))

fits <- fit_rain_prob(zimbabwe, obs_col = "rain", est_col = "tamsat_rain", 
              station_col = "station")

library(broom)
library(pscl)

fits_diag <- fits %>%
  mutate(
    AIC_all = map_dbl(fit_all, AIC),
    logLik = map_dbl(fit_all, logLik),
    pseudoR2 = map_dbl(fit_all, ~pR2(.x)["McFadden"])
  )

sat_seq <- seq(0.2, 70, by = 0.2)
pd <- expand.grid(est = sat_seq, season = levels(zimbabwe$season))

preds <- fits %>%
  mutate(pred = map(fit_all, ~predict(.x, newdata = pd, type = "response"))) %>%
  unnest(pred) %>%
  mutate(est = rep(pd$est, times = nrow(fits)),
         season = rep(pd$season, times = nrow(fits))) %>%
  dplyr::select(-starts_with("fit_"))

pd <- expand.grid(est = sat_seq,
                  season = levels(zimbabwe$season),
                  obs_wd_prev = c(FALSE, TRUE))

preds_markov_obs <- fits %>%
  mutate(pred = map(fit_markov_obs, ~predict(.x, newdata = pd, type = "response"))) %>%
  unnest(pred) %>%
  mutate(
    est = rep(pd$est, times = nrow(fits)),
    season = rep(pd$season, times = nrow(fits)),
    obs_wd_prev = rep(pd$obs_wd_prev, times = nrow(fits))
  ) %>%
  dplyr::select(-starts_with("fit_"))

pd <- expand.grid(est = sat_seq,
                  season = levels(zimbabwe$season),
                  est_wd_prev = c(FALSE, TRUE))

preds_markov_est <- fits %>%
  mutate(pred = map(fit_markov_est, ~predict(.x, newdata = pd, type = "response"))) %>%
  unnest(pred) %>%
  mutate(
    est = rep(pd$est, times = nrow(fits)),
    season = rep(pd$season, times = nrow(fits)),
    est_wd_prev = rep(pd$est_wd_prev, times = nrow(fits))
  ) %>%
  dplyr::select(-starts_with("fit_"))

breaks <- c(0, 1, 5, 10, 20, 50, Inf)

obs_bins <- zimbabwe %>%
  group_by(station) %>%
  mutate(
    bin = case_when(
      tamsat_rain == 0 ~ "S=0",
      TRUE ~ as.character(cut(tamsat_rain,
                              breaks = breaks,
                              include.lowest = TRUE,
                              right = FALSE))
    )
  ) %>%
  group_by(station, season, bin) %>%
  summarise(
    sat_mid = mean(tamsat_rain, na.rm = TRUE),
    obs_prob = mean(rainday, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

obs_bins_markov <- zimbabwe %>%
  group_by(station) %>%
  filter(!is.na(rainday_lag)) %>%
  mutate(
    bin = case_when(
      tamsat_rain == 0 ~ "S=0",
      TRUE ~ as.character(cut(tamsat_rain,
                              breaks = breaks,
                              include.lowest = TRUE,
                              right = FALSE))
    )
  ) %>%
  group_by(station, season, bin, rainday_lag) %>%
  summarise(
    sat_mid = mean(tamsat_rain, na.rm = TRUE),
    obs_prob = mean(rainday, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

# Plot all curves together
ggplot(preds, aes(x = est, y = pred)) +
  # geom_point(data = obs_bins,
  #            aes(x = sat_mid, y = obs_prob)) +
  # geom_point(data = obs_bins_markov,
  #            aes(x = sat_mid, y = obs_prob,
  #                color = rainday_lag)) +
  geom_line(linewidth = 1) +
  geom_line(data = preds_markov_est,
            aes(x = est, y = pred, color = est_wd_prev),
            linewidth = 1) +
  labs(x = "Satellite rainfall (mm)",
       y = "P(rain day)",
       title = "Logistic regression fits by group") +
  facet_grid(vars(station), vars(season)) +
  theme_minimal()

for (i in 1:nrow(fits)) {
  print(fits$station[[i]])
  #print(anova(fits$fit_month_int2[[i]], test = "Chisq"))
  print(anova(fits$fit_month[[i]], fits$fit_month_int1[[i]], fits$fit_month_int2[[i]], test = "Chisq"))
  print(AIC(fits$fit_month[[i]], fits$fit_month_int1[[i]], fits$fit_month_int2[[i]]))
  print(BIC(fits$fit_month[[i]], fits$fit_month_int1[[i]], fits$fit_month_int2[[i]]))
}


m_thresh <- markov_thresholds(zimbabwe, obs_col = "rain", est_col = "tamsat_rain",
                              season_col = "season", station_col = "station",
                              tol = 1e-2, max_it = 20)

x <- markov_loci(zimbabwe, obs_col = "rain", est_col = "tamsat_rain", 
             season_col = "season", station_col = "station",
             blocks = as.Date(c("1983-01-01", "1993-01-01", "2003-01-01", 
                                "2013-01-01", "2023-07-01")))


zimbabwe %>%
  group_by(station, season, rainday_lag) %>%
  filter(!is.na(rainday_lag)) %>%
  summarise(p = mean(rainday, na.rm = TRUE)) %>%
  pivot_wider(names_from = rainday_lag, values_from = p, names_prefix = "p") %>%
  rename(pd = pFALSE, pw = pTRUE)
