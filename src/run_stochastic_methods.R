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
  filter(date >= as.Date("1979-01-01") & date <= as.Date("2023-06-30")) %>%
  group_by(station) %>%
  mutate(agera5_rainday = agera5_rain > 0.85,
         agera5_rainday_lag = lag(agera5_rainday)) %>%
  ungroup()

zimbabwe_stations <- readr::read_csv(here("data", "zimbabwe_stations.csv"))

source(here("src", "methods.R"))

fits <- fit_rain_prob(zimbabwe, obs_col = "rain", est_col = "agera5_rain", 
                      station_col = "station")

delta_params <- fits %>% 
  dplyr::select(station, delta_params) %>% 
  unnest(delta_params)

# Next: predict values from fit, apply adjustments to predictions, calculate means to check, plot seq to visualise

zimbabwe_prob <- zimbabwe %>%
  rename(est = agera5_rain, obs_wd = rainday) %>%
  group_split(station) %>%
  map2_dfr(fits$fit_all, ~ mutate(.x, p_fit = predict(.y, newdata = .x, type = "response"))) %>%
  rename(agera5_rain = est, rainday = obs_wd) %>%
  left_join(delta_params, by = c("station", "season")) %>%
  mutate(p_fit_markov = plogis(qlogis(p_fit) + ifelse(agera5_rainday_lag, delta_w, delta_d)))

x1 <- zimbabwe_prob %>% 
  filter(!is.na(agera5_rainday_lag)) %>%
  group_by(station, season, agera5_rainday_lag, .drop = FALSE) %>%
  summarise(p = mean(p_fit_markov, na.rm = TRUE))

x2 <- zimbabwe_prob %>% 
  filter(!is.na(rainday_lag)) %>%
  group_by(station, season, rainday_lag) %>%
  summarise(p = mean(rainday, na.rm = TRUE))

x1 <- zimbabwe_prob %>% 
  filter(!is.na(agera5_rainday)) %>%
  group_by(station, season, .drop = FALSE) %>%
  summarise(p = mean(p_fit_markov, na.rm = TRUE))

x2 <- zimbabwe_prob %>% 
  filter(!is.na(rainday)) %>%
  group_by(station, season, .drop = FALSE) %>%
  summarise(p = mean(rainday, na.rm = TRUE))

View(data.frame(p_sat = x1$p, p_station = x2$p))

sat_seq <- seq(0.2, 70, by = 0.2)
pd <- expand.grid(est = sat_seq, season = levels(zimbabwe$season))

preds <- fits %>%
  mutate(pred = map(fit_all, ~predict(.x, newdata = pd, type = "response"))) %>%
  unnest(pred) %>%
  mutate(est = rep(pd$est, times = nrow(fits)),
         season = rep(pd$season, times = nrow(fits))) %>%
  dplyr::select(-c(starts_with("fit_"), delta_params)) %>%
  left_join(delta_params, by = c("station", "season")) %>%
  mutate(pred_w = plogis(qlogis(pred) + delta_w),
         pred_d = plogis(qlogis(pred) + delta_d))

ggplot(preds, aes(x = est, y = pred)) +
  geom_line(linewidth = 1) +
  geom_line(aes(y = pred_w, color = "w"),
            linewidth = 1) +
  geom_line(aes(y = pred_d, color = "d"),
            linewidth = 1) +
  labs(x = "Satellite rainfall (mm)",
       y = "P(rain day)",
       title = "Logistic regression fits by group") +
  facet_grid(vars(station), vars(season)) +
  theme_minimal()


# For plotting fit_markov_obs values
# pd <- expand.grid(est = sat_seq,
#                   season = levels(zimbabwe$season),
#                   obs_wd_prev = c(FALSE, TRUE))
# 
# preds_markov_obs <- fits %>%
#   mutate(pred = map(fit_markov_obs, ~predict(.x, newdata = pd, type = "response"))) %>%
#   unnest(pred) %>%
#   mutate(
#     est = rep(pd$est, times = nrow(fits)),
#     season = rep(pd$season, times = nrow(fits)),
#     obs_wd_prev = rep(pd$obs_wd_prev, times = nrow(fits))
#   ) %>%
#   dplyr::select(-starts_with("fit_"))
# 
# ggplot(preds, aes(x = est, y = pred)) +
#   geom_line(linewidth = 1) +
#   geom_line(data = preds_markov_obs,
#             aes(x = est, y = pred, color = obs_wd_prev),
#             linewidth = 1) +
#   labs(x = "Satellite rainfall (mm)",
#        y = "P(rain day)",
#        title = "Logistic regression fits by group") +
#   facet_grid(vars(station), vars(season)) +
#   theme_minimal()

