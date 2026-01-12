library(here)
library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(purrr)
library(verification)

# Setup -------------------------------------------------------------------

source(here("src", "helper_funs.R"))

zimbabwe_bc <- read_rds(here("data", "BC_data", "zimbabwe_agera5_bc_det.RDS"))

zimbabwe_bc_stack <- zimbabwe_bc %>%
  dplyr::select(station, date, year, month, day, season, rain, agera5_rain, est_loci:est_qm_gamma_mk) %>%
  pivot_longer(cols = c(rain, agera5_rain, est_loci:est_qm_gamma_mk), names_to = "source", values_to = "rr")

# TODO start 1 Aug instead
# 1 Aug = 214
s_doy_start <- 214

zimbabwe_bc_stack <- zimbabwe_bc_stack %>%
  group_by(station, source) %>%
  mutate(rainday = rr > 0.85,
         lag_rainday = lag(rainday),
         month = factor(month(date), levels = c(8:12, 1:7)),
         s_year = ifelse(month %in% 1:7, year - 1, year),
         doy = yday_366(date),
         s_doy = (doy - s_doy_start + 1) %% 366,
         s_doy = ifelse(s_doy == 0, 366, s_doy)
         )

zimbabwe_bc_stack$source <- factor(zimbabwe_bc_stack$source,
  levels = c("rain", "agera5_rain", "est_loci", "est_qm_gamma", "est_loci_mk", 
             "est_qm_gamma_mk", "est_qm_empirical", "est_qm_empirical_mk"),
  labels = c("Gauge", "AgERA5", "LOCI", "QM", "MC LOCI", "MC QM",
             "QM-Empirical", "MC QM-Empirical"))

# Only need to evaluate one existing BC method and one modified method
# since rain days are constructed the same way
occurrence_source <- c("Gauge", "AgERA5", "LOCI", "MC LOCI")
# Suggesting to only use the Gamma version of QM in the paper
# Don't think there's any added value including Empirical version as well
amounts_source <- c(occurrence_source, "QM", "MC QM")


# RAINFALL OCCURRENCE -----------------------------------------------------

zimbabwe_bc_stack_occ <- zimbabwe_bc_stack %>%
  filter(source %in% occurrence_source)

# Monthly climatology -----------------------------------------------------

zim_monthly_occ <- zimbabwe_bc_stack_occ %>%
  group_by(station, source, month, year) %>%
  summarise(n_rain = sum(rainday, na.rm = TRUE)) %>%
  summarise(n_rain = mean(n_rain))

ggplot(zim_monthly_occ, 
       aes(x = month, y = n_rain, colour = source, group = source)) +
  geom_point() +
  geom_line() +
  facet_wrap(vars(station))

# Question: need to present any metrics or obvious from the graph?


# Annual summaries --------------------------------------------------------

zim_annual_occ <- zimbabwe_bc_stack_occ %>%
  group_by(station, source, s_year) %>%
  summarise(n_rain = sum(rr > 0.85, na.rm = TRUE)) %>%
  ungroup()

zim_annual_dryspells <- zimbabwe_bc_stack_occ %>%
  group_by(station, source, s_year) %>%
  filter(month %in% c(10:12, 1:3)) %>%
  summarise(max_dry_spell = {
    r <- rle(!rainday)
    max(r$lengths[r$values], na.rm = TRUE)
  }) %>%
  ungroup()

zim_annual_occ <- left_join(zim_annual_occ, zim_annual_dryspells, 
                            by = c("station", "source", "s_year"))

zim_annual_occ_station <- zim_annual_occ %>% 
  filter(source == "rain") %>% 
  rename(n_rain_station = n_rain,
         max_dry_spell_station = max_dry_spell) %>% 
  dplyr::select(-source)

zim_annual_occ_wide <- zim_annual_occ %>% 
  filter(source != "rain") %>%
  left_join(zim_annual_occ_station, by = c("station", "s_year"))

zim_annual_occ_wide <- zim_annual_occ_wide %>% 
  group_by(station, source) %>%
  mutate(n_rain_diff = n_rain_station - n_rain,
         max_dry_spell_diff = max_dry_spell_station - max_dry_spell)

zim_annual_occ_metrics <- zim_annual_occ_wide %>% 
  group_by(station, source) %>%
  summarise(n_rain_me = mean(n_rain_diff, na.rm = TRUE),
            max_dry_spell_me = mean(max_dry_spell_diff, na.rm = TRUE),
            n_rain_cor = cor(n_rain, n_rain_station, use = "complete.obs"),
            max_dry_spell_cor = cor(max_dry_spell, max_dry_spell_station))

# Annual number of rain days
ggplot(zim_annual_occ, 
       aes(x = s_year, y = n_rain, colour = source)) +
  geom_line() +
  facet_wrap(vars(station))

tbl_annual_occ_nrain <- zim_annual_occ_metrics %>%
  dplyr::select(station, source, n_rain_me, n_rain_cor) %>%
  pivot_longer(cols = c(n_rain_me, n_rain_cor), names_to = "metric",
    values_to = "value") %>%
  mutate(metric = recode(metric, n_rain_me = "ME", n_rain_cor  = "cor"),
         value = round(value, 3)) %>%
  pivot_wider(names_from = c(metric, source), values_from = value, 
              names_sep = "_") %>%
  dplyr::select(station, sort(grep("^ME_", names(.), value = TRUE)),
    sort(grep("^cor_",  names(.), value = TRUE)))
tbl_annual_occ_nrain

# Annual length of longest dry spell (October to March)
# QC problem in station data at Chisumbanje in 2002 and 2009
ggplot(zim_annual_occ, 
       aes(x = s_year, y = max_dry_spell, colour = source)) +
  geom_line() +
  facet_wrap(vars(station))

tbl_annual_occ_maxdry <- zim_annual_occ_metrics %>%
  dplyr::select(station, source, max_dry_spell_me, max_dry_spell_cor) %>%
  pivot_longer(cols = c(max_dry_spell_me, max_dry_spell_cor), names_to = "metric",
               values_to = "value") %>%
  mutate(metric = recode(metric, max_dry_spell_me = "ME", max_dry_spell_cor  = "cor"),
         value = round(value, 3)) %>%
  pivot_wider(names_from = c(metric, source), values_from = value, 
              names_sep = "_") %>%
  dplyr::select(station, sort(grep("^ME_", names(.), value = TRUE)),
                sort(grep("^cor_",  names(.), value = TRUE)))
tbl_annual_occ_maxdry


# Distribution of wet/dry spells ------------------------------------------

dry_spells <- zimbabwe_bc_stack_occ %>%
  group_by(station, source, s_year) %>%
  filter(month %in% c(10:12, 1:3)) %>%
  reframe(dry_spell_length = {
    r <- rle(!rainday)
    r$lengths[r$values]
  })

wet_spells <- zimbabwe_bc_stack_occ %>%
  group_by(station, source, s_year) %>%
  filter(month %in% c(10:12, 1:3)) %>%
  reframe(wet_spell_length = {
    r <- rle(rainday)
    r$lengths[r$values]
  })

ggplot(dry_spells, aes(x = dry_spell_length, colour = source)) +
  stat_ecdf(linewidth = 1) +
  scale_x_log10() +
  facet_wrap(vars(station)) +
  labs(
    x = "Dry spell length (days)",
    y = "Empirical CDF",
    title = "Dry spell length (October to March) distribution by source and station"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank()
  )

ggplot(wet_spells, aes(x = wet_spell_length, colour = source)) +
  stat_ecdf(linewidth = 1) +
  scale_x_log10() +
  facet_wrap(vars(station)) +
  labs(
    x = "Wet spell length (days)",
    y = "Empirical CDF",
    title = "Wet spell length (October to March) distribution by source and station"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank()
  )

ks_results_dry <- dry_spells %>%
  group_by(station) %>%
  reframe(
    map_dfr(c("agera5_rain", "est_loci", "est_loci_mk"), function(src) {
      test <- ks.test(
        dry_spell_length[source == "rain"],
        dry_spell_length[source == src]
      )
      tibble(
        source = src,
        statistic = unname(test$statistic),
        p_value = test$p.value
      )
    })
  ) %>%
  ungroup()
ks_results_dry

ks_results_wet <- wet_spells %>%
  group_by(station) %>%
  reframe(
    map_dfr(c("agera5_rain", "est_loci", "est_loci_mk"), function(src) {
      test <- ks.test(
        wet_spell_length[source == "rain"],
        wet_spell_length[source == src]
      )
      tibble(
        source = src,
        statistic = unname(test$statistic),
        p_value = test$p.value
      )
    })
  ) %>%
  ungroup()
ks_results_wet


# Seasonal ----------------------------------------------------------------

# TODO include diagnostics of models in supplementary material?
#      e.g. different harmonics to justify model choice

# Markov Chain Zero Order Rainday Models

fit_zero_order_markov <- function(data) {
  # Fit logistic regression: P(rainday) ~ seasonal harmonics
  glm(rainday ~ 
        sin(2 * pi * s_doy / 366) + cos(2 * pi * s_doy / 366) +
        sin(4 * pi * s_doy / 366) + cos(4 * pi * s_doy / 366) +
        sin(6 * pi * s_doy / 366) + cos(6 * pi * s_doy / 366),
      data = data,
      family = binomial)
}

mc_models_0 <- zimbabwe_bc_stack_occ %>%
  group_by(source, station) %>%
  group_modify(~ tibble(m = list(fit_zero_order_markov(.x)))) %>%
  ungroup()

doy_df <- tibble(s_doy = 1:366)
fitted_list <- list()
for (i in seq_len(nrow(mc_models_0))) {
  
  # Extract info for this model
  src <- mc_models_0$source[i]
  stn <- mc_models_0$station[i]
  mod <- mc_models_0$m[[i]]
  
  # Predict probabilities
  preds <- predict(mod, newdata = doy_df, type = "response")
  
  # Combine into a tibble
  fitted_list[[i]] <- tibble(
    source = src,
    station = stn,
    s_doy = doy_df$s_doy,
    fitted = preds
  )
}
fitted_doy_df_0 <- bind_rows(fitted_list)

ggplot(fitted_doy_df_0, aes(x = s_doy, y = fitted, color = source)) +
  geom_line(size = 1) +
  facet_wrap(~ station, ncol = 2) +
  labs(
    title = "Predicted Seasonal Probability of Rainday",
    x = "Day of Year",
    y = "Predicted Probability",
    color = "Source"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank()
  )

rain_ref <- fitted_doy_df_0 %>%
  filter(source == "rain") %>%
  dplyr::select(station, s_doy, fitted_rain = fitted)

rmse_rainday_0 <- fitted_doy_df_0 %>%
  filter(source != "rain") %>%
  left_join(rain_ref, by = c("station", "s_doy")) %>%
  group_by(station, source) %>%
  summarise(RMSE = sqrt(mean((fitted - fitted_rain)^2, na.rm = TRUE))) %>%
  pivot_wider(names_from  = source, values_from = RMSE)
rmse_rainday_0

# Markov Chain First Order Rainday Models

fit_first_order_markov <- function(data) {
  # Fit logistic regression: P(rainday) ~ rainday lag + seasonal harmonics
  glm(rainday ~ 
        lag_rainday +
        sin(2 * pi * s_doy / 366) + cos(2 * pi * s_doy / 366) +
        sin(4 * pi * s_doy / 366) + cos(4 * pi * s_doy / 366) +
        sin(6 * pi * s_doy / 366) + cos(6 * pi * s_doy / 366),
      data = data,
      family = binomial)
}

mc_models_1 <- zimbabwe_bc_stack_occ %>%
  group_by(source, station) %>%
  group_modify(~ tibble(m = list(fit_first_order_markov(.x)))) %>%
  ungroup()

doy_df <- expand.grid(lag_rainday = c(TRUE, FALSE), s_doy = 1:366)
fitted_list <- list()
for (i in seq_len(nrow(mc_models_1))) {
  fitted_data <- doy_df
  preds <- predict(mc_models_1$m[[i]], newdata = fitted_data, type = "response")
  fitted_data$fitted <- preds
  fitted_data$source <- mc_models_1$source[i]
  fitted_data$station <- mc_models_1$station[i]
  
  fitted_list[[i]] <- fitted_data
}
fitted_doy_df_1 <- bind_rows(fitted_list)

ggplot(fitted_doy_df_1, aes(x = s_doy, y = fitted, color = source, 
                            linetype = lag_rainday)) +
  geom_line(size = 1) +
  labs(
    title = "Probability of Rainday Given Previous Day State",
    x = "Day of Year",
    y = "Predicted Probability",
    color = "Source",
    linetype = "Previous Day"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank()
  ) +
  scale_linetype_manual(
    values = c("TRUE" = "dotted", "FALSE" = "dashed"),
    breaks = c("TRUE", "FALSE"),   # ensures TRUE appears first in legend
    labels = c("Rain", "Dry")
  ) +
  facet_grid(rows = vars(lag_rainday), cols = vars(station))

rain_ref <- fitted_doy_df_1 %>%
  filter(source == "rain") %>%
  dplyr::select(station, s_doy, lag_rainday, fitted_rain = fitted)

rmse_rainday_1 <- fitted_doy_df_1 %>%
  filter(source != "rain") %>%
  left_join(rain_ref, by = c("station", "s_doy", "lag_rainday")) %>%
  group_by(station, source, lag_rainday) %>%
  summarise(RMSE = sqrt(mean((fitted - fitted_rain)^2, na.rm = TRUE)))

# Include table in supplementary material
rmse_rainday_1

ggplot(rmse_rainday_1,
       aes(x = lag_rainday, y = RMSE, fill = source)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  facet_wrap(~ station, ncol = 3, scales = "free_y") +
  labs(
    x = "Lagged rainday",
    y = "RMSE",
    fill = "Source",
    title = "RMSE by source, lagged rainday, and station"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(face = "bold")
  )


# Rainfall occurrence detection

zimbabwe_bc_stack_station_occ <- zimbabwe_bc_stack_occ %>%
  ungroup() %>%
  filter(source == "rain") %>%
  rename(rr_station = rr,
         rainday_station = rainday) %>%
  dplyr::select(station, date, rainday_station, rr_station)

zimbabwe_bc_comp_occ <- zimbabwe_bc_stack_occ %>%
  filter(source != "rain" & source %in% occurrence_source) %>%
  left_join(zimbabwe_bc_stack_station_occ, by = c("station", "date"))

zimbabwe_pod_hss_occ <- zimbabwe_bc_comp_occ %>%
  group_by(station, source) %>%
  summarise(ver = list(verify(rainday_station, rainday, frcst.type = "binary",
                              obs.type = "binary")),
            POD = map_dbl(ver, ~ .x$POD),
            FAR = map_dbl(ver, ~ .x$FAR),
            HSS = map_dbl(ver, ~ .x$HSS)) %>%
  dplyr::select(-ver) %>%
  pivot_longer(cols = c(POD, FAR, HSS), names_to = "metric", values_to = "value",
               names_ptypes = factor(levels = c("POD", "FAR", "HSS")))

ggplot(zimbabwe_pod_hss_occ,
       aes(x = metric, y = value, fill = source)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  facet_wrap(~ station, ncol = 3) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    x = "Metric",
    y = "Metric value",
    fill = "Source",
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold")
  )

# RAINFALL AMOUNTS --------------------------------------------------------

zimbabwe_bc_stack_amt <- zimbabwe_bc_stack %>%
  filter(source %in% amounts_source)

# Monthly climatology -----------------------------------------------------

zim_monthly_amt <- zimbabwe_bc_stack_amt %>%
  group_by(station, source, month, year) %>%
  summarise(n_rain = sum(rainday, na.rm = TRUE),
            t_rain = sum(rr, na.rm = TRUE),
            mean_rain = t_rain / n_rain,
            mean_rain = ifelse(is.infinite(mean_rain), NA, mean_rain),
            max_rain = max(rr, na.rm = TRUE),
            max_rain = ifelse(is.infinite(max_rain), NA, max_rain)) %>%
  summarise(t_rain = mean(t_rain, na.rm = TRUE),
            mean_rain = mean(mean_rain, na.rm = TRUE),
            max_rain = mean(max_rain, na.rm = TRUE))

ggplot(zim_monthly_amt, 
       aes(x = month, y = t_rain, colour = source, group = source)) +
  geom_point() +
  geom_line() +
  facet_wrap(vars(station))

ggplot(zim_monthly_amt, 
       aes(x = month, y = mean_rain, colour = source, group = source)) +
  geom_point() +
  geom_line() +
  facet_wrap(vars(station))

ggplot(zim_monthly_amt, 
       aes(x = month, y = max_rain, colour = source, group = source)) +
  geom_point() +
  geom_line() +
  facet_wrap(vars(station))

# Annual summaries --------------------------------------------------------

zim_annual_amt <- zimbabwe_bc_stack_amt %>%
  group_by(station, source, s_year) %>%
  summarise(n_rain = sum(rr > 0.85, na.rm = TRUE),
            t_rain = sum(rr, na.rm = TRUE),
            mean_rain = t_rain / n_rain,
            max_rain = max(rr, na.rm = TRUE)) %>%
  ungroup()

zim_annual_amt_station <- zim_annual_amt %>% 
  filter(source == "rain") %>% 
  rename(t_rain_station = t_rain, max_rain_station = max_rain,
         mean_rain_station = mean_rain) %>% 
  dplyr::select(-source)

zim_annual_amt_wide <- zim_annual_amt %>% 
  filter(source != "rain") %>%
  left_join(zim_annual_station_amt, by = c("station", "s_year"))

zim_annual_amt_wide <- zim_annual_amt_wide %>% 
  group_by(station, source) %>%
  mutate(t_rain_diff = t_rain_station - t_rain,
         max_rain_diff = max_rain_station - max_rain,
         mean_rain_diff = mean_rain_station - mean_rain)

zim_annual_amt_metrics <- zim_annual_amt_wide %>% 
  group_by(station, source) %>%
  summarise(t_rain_me = mean(t_rain_diff, na.rm = TRUE),
            max_rain_me = mean(mean_rain_diff, na.rm = TRUE),
            mean_rain_me = mean(mean_rain_diff, na.rm = TRUE),
            t_rain_cor = cor(t_rain, t_rain_station, use = "complete.obs"),
            max_rain_cor = cor(max_rain, max_rain_station, use = "complete.obs"),
            mean_rain_cor = cor(mean_rain, mean_rain_station, use = "complete.obs"))

col_scale <- scale_colour_manual(
  values = c(
    Gauge = "black",
    AgERA5 = "#E64B35",
    LOCI = "#0072B2",
    `MC LOCI` = "#0072B2",
    QM = "#E69F00",
    `MC QM` = "#E69F00"
  )
)

linetype_scale <- scale_linetype_manual(
  values = c(
    Gauge = "solid",
    AgERA5 = "solid",
    LOCI = "solid",
    `MC LOCI` = "dashed",
    QM = "solid",
    `MC QM` = "dashed"
  )
)

# Annual total rainfall
ggplot(zim_annual_amt, 
       aes(x = s_year, y = t_rain, colour = source, linetype = source)) +
  geom_line() +
  facet_wrap(vars(station)) +
  col_scale + 
  linetype_scale

tbl_annual_amt_t_rain <- zim_annual_amt_metrics %>%
  dplyr::select(station, source, t_rain_me, t_rain_cor) %>%
  pivot_longer(cols = c(t_rain_me, t_rain_cor), names_to = "metric",
               values_to = "value") %>%
  mutate(metric = recode(metric, t_rain_me = "ME", t_rain_cor  = "cor"),
         value = round(value, 3)) %>%
  pivot_wider(names_from = source, values_from = value) %>%
  arrange(metric, station)
tbl_annual_amt_t_rain

# Annual mean rainfall
ggplot(zim_annual_amt, 
       aes(x = s_year, y = mean_rain, colour = source, linetype = source)) +
  geom_line() +
  facet_wrap(vars(station)) +
  col_scale +
  linetype_scale

tbl_annual_amt_mean_rain <- zim_annual_amt_metrics %>%
  dplyr::select(station, source, mean_rain_me, mean_rain_cor) %>%
  pivot_longer(cols = c(mean_rain_me, mean_rain_cor), names_to = "metric",
               values_to = "value") %>%
  mutate(metric = recode(metric, mean_rain_me = "ME", mean_rain_cor  = "cor"),
         value = round(value, 3)) %>%
  pivot_wider(names_from = source, values_from = value) %>%
  arrange(metric, station)
tbl_annual_amt_mean_rain

# Annual max rainfall
ggplot(zim_annual_amt, 
       aes(x = s_year, y = max_rain, colour = source, linetype = source)) +
  geom_line(size = 0.6) +
  facet_wrap(vars(station)) +
  col_scale +
  linetype_scale

tbl_annual_amt_max_rain <- zim_annual_amt_metrics %>%
  dplyr::select(station, source, max_rain_me, max_rain_cor) %>%
  pivot_longer(cols = c(max_rain_me, max_rain_cor), names_to = "metric",
               values_to = "value") %>%
  mutate(metric = recode(metric, max_rain_me = "ME", max_rain_cor  = "cor"),
         value = round(value, 3)) %>%
  pivot_wider(names_from = source, values_from = value) %>%
  arrange(metric, station)
tbl_annual_amt_max_rain


# Seasonal ----------------------------------------------------------------

# Markov Chain Zero Order Rainfall Models

fit_zero_order_markov_amounts <- function(data) {
  data_rain <- data %>% filter(rainday)
  glm(rr ~ 
        sin(2 * pi * s_doy / 366) + cos(2 * pi * s_doy / 366) +
        sin(4 * pi * s_doy / 366) + cos(4 * pi * s_doy / 366),# +
        #sin(6 * pi * s_doy / 366) + cos(6 * pi * s_doy / 366),
      data = data_rain,
      family = Gamma(link = "log"))
}

mc_models_0_amounts <- zimbabwe_bc_stack_amt %>%
  group_by(source, station) %>%
  group_modify(~ tibble(m = list(fit_zero_order_markov_amounts(.x)))) %>%
  ungroup()

doy_df <- tibble(s_doy = 1:366)
fitted_list <- list()
for (i in seq_len(nrow(mc_models_0_amounts))) {
  
  src <- mc_models_0_amounts$source[i]
  stn <- mc_models_0_amounts$station[i]
  mod <- mc_models_0_amounts$m[[i]]
  
  preds <- predict(mod, newdata = doy_df, type = "response")
  
  fitted_list[[i]] <- tibble(
    source = src,
    station = stn,
    s_doy = doy_df$s_doy,
    fitted = preds
  )
}
fitted_doy_df_0_amounts <- bind_rows(fitted_list)

ggplot(fitted_doy_df_0_amounts, 
       aes(x = s_doy, y = fitted, color = source, linetype = source)) +
  geom_line(size = 1) +
  facet_wrap(vars(station)) +
  labs(
    title = "Mean Rainfall Amount on Rainy Days",
    x = "Day of Year",
    y = "Rainfall (mm)",
    color = "Source",
    linetype = "Source"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank()
  ) +
  col_scale + 
  linetype_scale


# Markov Chain First Order Rainfall Models

fit_first_order_markov_amounts <- function(data) {
  data_rain <- data %>% filter(rainday)
  glm(rr ~ 
        lag_rainday +
        sin(2 * pi * s_doy / 366) + cos(2 * pi * s_doy / 366) +
        sin(4 * pi * s_doy / 366) + cos(4 * pi * s_doy / 366),# +
        #sin(6 * pi * s_doy / 366) + cos(6 * pi * s_doy / 366),
      data = data_rain,
      family = Gamma(link = "log"))
}

mc_models_1_amounts <- zimbabwe_bc_stack_amt %>%
  group_by(source, station) %>%
  group_modify(~ tibble(m = list(fit_first_order_markov_amounts(.x)))) %>%
  ungroup()

doy_df <- expand.grid(lag_rainday = c(TRUE, FALSE), s_doy = 1:366)
fitted_list <- list()
for (i in seq_len(nrow(mc_models_1_amounts))) {
  fitted_data <- doy_df
  preds <- predict(mc_models_1_amounts$m[[i]], newdata = fitted_data, type = "response")
  fitted_data$fitted <- preds
  fitted_data$source <- mc_models_1_amounts$source[i]
  fitted_data$station <- mc_models_1_amounts$station[i]
  
  fitted_list[[i]] <- fitted_data
}
fitted_doy_df_1_amounts <- bind_rows(fitted_list)

ggplot(fitted_doy_df_1_amounts, 
       aes(x = s_doy, y = fitted, color = source, linetype = source)) +
  geom_line(size = 1) +
  facet_grid(vars(lag_rainday), vars(station)) +
  labs(
    title = "Mean Rainfall Amount on Rainy Days",
    x = "Day of Year",
    y = "Rainfall (mm)",
    color = "Source",
    linetype = "Source"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank()
  ) +
  col_scale + 
  linetype_scale


# POD and HSS for rainfall categories -------------------------------------

cat_labs <- c("Dry", "Light Rain", 
              "Moderate Rain", "Heavy Rain", 
              "Violent Rain")

zimbabwe_bc_stack <- zimbabwe_bc_stack %>%
  mutate(rain_cat = cut(rr, c(0, 0.85, 5, 20, 40, Inf), include.lowest = TRUE,
                         right = FALSE, labels = cat_labs))

zimbabwe_bc_stack_station <- zimbabwe_bc_stack %>% 
  ungroup() %>%
  filter(source == "rain") %>% 
  rename(rain_cat_station = rain_cat, rr_station = rr) %>% 
  dplyr::select(station, date, rain_cat_station, rr_station)

zimbabwe_bc_stack_wide <- zimbabwe_bc_stack %>% 
  filter(source != "rain") %>%
  left_join(zimbabwe_bc_stack_station, by = c("station", "date"))

zimbabwe_pod_hss_cats <- zimbabwe_bc_stack_wide %>%
  group_by(station, source) %>%
  filter(rain_cat_station != "Dry") %>%
  summarise(
    pod_light = sum(rain_cat == "Light Rain" & rain_cat_station == "Light Rain")/sum(rain_cat_station == "Light Rain"),
    pod_moderate = sum(rain_cat == "Moderate Rain" & rain_cat_station == "Moderate Rain")/sum(rain_cat_station == "Moderate Rain"),
    pod_heavy = sum(rain_cat == "Heavy Rain" & rain_cat_station == "Heavy Rain")/sum(rain_cat_station == "Heavy Rain"),
    pod_violent = sum(rain_cat == "Violent Rain" & rain_cat_station == "Violent Rain")/sum(rain_cat_station == "Violent Rain"),
    ver = list(verify(rain_cat_station, rain_cat, frcst.type = "cat",
                      obs.type = "cat")),
    hss = map_dbl(ver, ~ .x$hss))

zimbabwe_pod_hss_cats_format <- zimbabwe_pod_hss_cats %>%
  dplyr::select(-ver) %>%
  pivot_wider(names_from = source, values_from = hss)

zimbabwe_pod_hss_long <- zimbabwe_pod_hss_cats %>%
  dplyr::select(station, source, starts_with("pod_"), hss) %>%
  pivot_longer(
    cols = c(starts_with("pod_"), "hss"),
    names_to = "metric",
    values_to = "value"
  ) %>%
  mutate(
    metric = gsub("pod_", "", metric),
    metric = factor(metric,
                      levels = c("light", "moderate", "heavy", "violent", "hss"),
                      labels = c("Light Rain", "Moderate Rain", "Heavy Rain", "Violent Rain", "HSS"))
  )

ggplot(zimbabwe_pod_hss_long, aes(x = metric, y = value, fill = source)) +
  geom_col(position = position_dodge()) +
  facet_wrap(vars(station)) +
  labs(
    x = "Rainfall Category / Metric",
    y = "Score",
    fill = "Data Source",
    title = "POD and HSS by Rainfall Category and Station"
  ) +
  theme_bw(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1),
    strip.text = element_text(face = "bold")
  )
