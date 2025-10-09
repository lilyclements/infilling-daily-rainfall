library(here)
library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)

zimbabwe_bc <- read_rds(here("data", "BC_data", "zimbabwe_agera5_bc_det.RDS"))

zimbabwe_bc_stack <- zimbabwe_bc %>%
  dplyr::select(station, date, year, month, day, season, rain, agera5_rain, est_loci:est_qm_gamma_mk) %>%
  pivot_longer(cols = c(rain, agera5_rain, est_loci:est_qm_gamma_mk), names_to = "source", values_to = "rr")

zimbabwe_bc_stack <- zimbabwe_bc_stack %>%
  mutate(rainday = rr > 0.85)

zim_annual <- zimbabwe_bc_stack %>%
  group_by(station, year, source) %>%
  summarise(n_rain = sum(rr > 0.85))

ggplot(zim_annual %>% filter(source %in% c("rain", "est_loci", "est_loci_mk")), 
       aes(x = year, y = n_rain, colour = source)) +
  geom_line() +
  facet_wrap(vars(station))
