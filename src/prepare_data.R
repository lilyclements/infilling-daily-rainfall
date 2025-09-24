library(here)
library(readr)
library(dplyr)


# AgERA5 ------------------------------------------------------------------

agera5_zimbabwe <- read_csv(here("data", "agera5_zimbabwe.csv"))

zimbabwe <- readr::read_csv(here("data", "zim_five_stations.csv"))

zimbabwe_stations <- readr::read_csv(here("data", "zimbabwe_stations.csv"))

zimbabwe <- dplyr::left_join(zimbabwe, agera5_zimbabwe,
                             by = c("station", "date"))

rm(agera5_zimbabwe)


# TAMSAT ------------------------------------------------------------------

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


# Overall -----------------------------------------------------------------

zimbabwe <- zimbabwe %>%
  mutate(year = year(date), 
         month = month(date), 
         day = day(date),
         rainday = rain > 0.85,
         rainday_lag = dplyr::lag(rainday),
         season = ifelse(month %in% 5:9, "dry", as.character(month)),
         season = factor(season, levels = c("dry", 10:12, 1:4)))

write.csv(zimbabwe, here("data", "zimbabwe_with_sre.csv"), row.names = FALSE)
