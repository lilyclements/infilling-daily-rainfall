library(here)
library(readr)
library(dplyr)
library(fitdistrplus)
library(lubridate)
library(ggplot2)
library(purrr)
library(tidyr)

zimbabwe <- readr::read_csv(here("data", "zimbabwe_with_sre.csv"))

zimbabwe <- zimbabwe %>%
  filter(date >= as.Date("1979-01-01") & date <= as.Date("2023-06-30"))

zimbabwe_stations <- readr::read_csv(here("data", "zimbabwe_stations.csv"))

source(here("src", "methods.R"))

fits <- fit_rain_prob(zimbabwe, obs_col = "rain", est_col = "agera5_rain", 
                      station_col = "station")

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

ggplot(preds, aes(x = est, y = pred)) +
  geom_line(linewidth = 1) +
  geom_line(data = preds_markov_obs,
            aes(x = est, y = pred, color = obs_wd_prev),
            linewidth = 1) +
  labs(x = "Satellite rainfall (mm)",
       y = "P(rain day)",
       title = "Logistic regression fits by group") +
  facet_grid(vars(station), vars(season)) +
  theme_minimal()

