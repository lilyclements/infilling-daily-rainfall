library(here)
library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)


# Setup -------------------------------------------------------------------

source(here("src", "helper_funs.R"))

zimbabwe_bc <- read_rds(here("data", "BC_data", "zimbabwe_agera5_bc_det.RDS"))

zimbabwe_bc_stack <- zimbabwe_bc %>%
  dplyr::select(station, date, year, month, day, season, rain, agera5_rain, est_loci:est_qm_gamma_mk) %>%
  pivot_longer(cols = c(rain, agera5_rain, est_loci:est_qm_gamma_mk), names_to = "source", values_to = "rr")

zimbabwe_bc_stack <- zimbabwe_bc_stack %>%
  group_by(station, source) %>%
  mutate(rainday = rr > 0.85,
         lag_rainday = lag(rainday),
         month = factor(month(date), levels = c(7:12, 1:6)),
         s_year = ifelse(month %in% 1:6, year - 1, year))


# Monthly climatology -----------------------------------------------------

zim_monthly <- zimbabwe_bc_stack %>%
  group_by(station, source, month, year) %>%
  summarise(n_rain = sum(rainday, na.rm = TRUE),
            t_rain = sum(rr, na.rm = TRUE),
            mean_rain = t_rain / n_rain,
            mean_rain = ifelse(is.infinite(mean_rain), NA, mean_rain),
  max_rain = max(rr, na.rm = TRUE),
            max_rain = ifelse(is.infinite(max_rain), NA, max_rain)) %>%
  summarise(n_rain = mean(n_rain),
            t_rain = mean(t_rain),
            mean_rain = mean(mean_rain, na.rm = TRUE),
            max_rain = mean(max_rain, na.rm = TRUE))

ggplot(zim_monthly %>% filter(source %in% c("rain", "agera5_rain", "est_loci", "est_loci_mk")), 
       aes(x = month, y = n_rain, colour = source, group = source)) +
  geom_point() +
  geom_line() +
  facet_wrap(vars(station))

ggplot(zim_monthly, 
       aes(x = month, y = t_rain, colour = source, group = source)) +
  geom_point() +
  geom_line() +
  facet_wrap(vars(station))

ggplot(zim_monthly, 
       aes(x = month, y = mean_rain, colour = source, group = source)) +
  geom_point() +
  geom_line() +
  facet_wrap(vars(station))

ggplot(zim_monthly, 
       aes(x = month, y = max_rain, colour = source, group = source)) +
  geom_point() +
  geom_line() +
  facet_wrap(vars(station))


# Annual summaries --------------------------------------------------------

zim_annual <- zimbabwe_bc_stack %>%
  group_by(station, source, s_year) %>%
  summarise(n_rain = sum(rr > 0.85, na.rm = TRUE),
            t_rain = sum(rr, na.rm = TRUE),
            mean_rain = t_rain / n_rain,
            max_rain = max(rr, na.rm = TRUE)) %>%
  ungroup()

zim_spells <-  zimbabwe_bc_stack %>%
  group_by(station, source, s_year) %>%
  filter(source %in% c("rain", "agera5_rain", "est_loci", "est_loci_mk")) %>%
  filter(month %in% c(10:12, 1:3)) %>%
  mutate(dry_spell_lns = spells(!rainday))

zim_spells_annual <- zim_spells %>%
  summarise(m_dry_spell = max(dry_spell_lns, na.rm = TRUE),
            m_dry_spell = ifelse(is.infinite(m_dry_spell), NA, m_dry_spell))

zim_annual <- left_join(zim_annual, zim_spells_annual, by = c("station", "source", "s_year"))

zim_annual_station <- zim_annual %>% 
  filter(source == "rain") %>% 
  rename(n_rain_station = n_rain, t_rain_station = t_rain, max_rain_station = max_rain,
         mean_rain_station = mean_rain, m_dry_spell_station = m_dry_spell) %>% 
  dplyr::select(-source)

zim_annual_wide <- zim_annual %>% 
  filter(source != "rain") %>%
  left_join(zim_annual_station, by = c("station", "s_year"))

zim_annual_wide <- zim_annual_wide %>% 
  group_by(station, source) %>%
  mutate(n_rain_diff = n_rain_station - n_rain,
         t_rain_diff = t_rain_station - t_rain,
         max_rain_diff = max_rain_station - max_rain,
         mean_rain_diff = mean_rain_station - mean_rain,
         m_dry_spell_diff = m_dry_spell_station - m_dry_spell)

ggplot(zim_annual, 
       aes(x = year, y = n_rain, colour = source)) +
  geom_line() +
  facet_wrap(vars(station))

ggplot(zim_annual, 
       aes(x = year, y = m_spell, colour = source)) +
  geom_line() +
  facet_wrap(vars(station))

ggplot(zim_annual_wide %>% filter(!is.na(m_dry_spell_diff)), 
       aes(x = source, y = m_dry_spell_diff, colour = source)) +
  geom_boxplot() +
  scale_x_discrete(drop = TRUE) +
  facet_wrap(vars(station))

zim_annual_metrics <- zim_annual_wide %>% 
  group_by(station, source) %>%
  summarise(n_rain_bias = mean(n_rain_diff, na.rm = TRUE),
            t_rain_bias = mean(t_rain_diff, na.rm = TRUE),
            max_rain_bias = mean(mean_rain_diff, na.rm = TRUE),
            mean_rain_bias = mean(mean_rain_diff, na.rm = TRUE),
            m_dry_spell_bias = mean(m_dry_spell_diff, na.rm = TRUE),
            n_rain_cor = cor(n_rain, n_rain_station, use = "complete.obs"),
            t_rain_cor = cor(t_rain, t_rain_station, use = "complete.obs"),
            max_rain_cor = cor(max_rain, max_rain_station, use = "complete.obs"),
            mean_rain_cor = cor(mean_rain, mean_rain_station, use = "complete.obs"),
            m_dry_spell_cor = cor(m_dry_spell, m_dry_spell_station))


# Spell distributions -----------------------------------------------------

dry_spells <- zimbabwe_bc_stack %>%
  group_by(station, source, s_year) %>%
  filter(source %in% c("rain", "agera5_rain", "est_loci", "est_loci_mk")) %>%
  filter(month %in% c(10:12, 1:3)) %>%
  reframe(dry_spell_length = rle(!rainday)$lengths[rle(!rainday)$values])

ggplot(dry_spells, aes(x = dry_spell_length, colour = source)) +
  stat_ecdf(size = 1) +
  scale_x_log10() +
  facet_wrap(vars(station)) +
  labs(
    x = "Dry spell length (days)",
    y = "Empirical CDF",
    title = "Dry spell length distribution by source and station"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank()
  )

ks_results <- dry_spells %>%
  group_by(station) %>%
  summarise(
    D_KS_agera5 = ks.test(
      dry_spell_length[source == "rain"],
      dry_spell_length[source == "agera5_rain"]
    )$statistic,
    D_KS_est_loci = ks.test(
      dry_spell_length[source == "rain"],
      dry_spell_length[source == "est_loci"]
    )$statistic,
    D_KS_est_loci_mk = ks.test(
      dry_spell_length[source == "rain"],
      dry_spell_length[source == "est_loci_mk"]
    )$statistic
  )

# Rain given rain evaluation ----------------------------------------------

zim_annual_lag <- zimbabwe_bc_stack %>%
  filter(!is.na(lag_rainday)) %>%
  group_by(station, s_year, source, lag_rainday) %>%
  summarise(n_rain = sum(rr > 0.85, na.rm = TRUE),
            t_rain = sum(rr, na.rm = TRUE)) %>%
  ungroup()

zim_annual_station <- zim_annual_lag %>% 
  filter(source == "rain") %>% 
  rename(n_rain_station = n_rain, t_rain_station = t_rain) %>% 
  dplyr::select(-source)

zim_annual_wide <- zim_annual_lag %>% 
  filter(source != "rain") %>%
  left_join(zim_annual_station, by = c("station", "s_year", "lag_rainday"))

zim_annual_metrics <- zim_annual_wide %>% 
  group_by(station, source, lag_rainday) %>%
  summarise(mad_n_rain = mean(abs(n_rain_station - n_rain)),
            mad_t_rain = mean(abs(t_rain_station - t_rain)))
