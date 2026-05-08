# load libraries
library(tidyverse)
library(sf)
library(akgfmaps)

# get year folder
new_year <- as.numeric(format(Sys.Date(), format = "%Y"))

# plot GOA NMFS management areas
nmfs_areas <- akgfmaps::get_nmfs_areas(set.crs = "EPSG:4326")
goa_layers <- akgfmaps::get_base_layers(select.region = "goa", set.crs = "EPSG:3338")
strata_with_coords <- goa_layers$survey.strata %>%
  st_centroid() %>%
  st_transform(crs = 4326) %>% # Transform to WGS84 (Lat/Lon)
  st_coordinates() %>%
  as.data.frame()
goa_west <- goa_layers$survey.strata %>%
  mutate(long = strata_with_coords$X) %>%
  filter(long < -156)


# plot fishery locations ----

obs_fish <- vroom::vroom(here::here(new_year, 'rsch', 'wgoa', 'data', 'fish_obs_catch.csv'))
obs_catch <- obs_fish %>% 
  tidytable::drop_na(lat, lon) %>% 
  tidytable::mutate(month = month(hday),
                    season = case_when(month <= 3 ~ 'A',
                                       month > 3 ~ 'B')) %>% 
  st_as_sf(., coords = c("lon", "lat"), crs = 4326)


ggplot() +
  geom_sf(data = nmfs_areas %>% filter(REP_AREA %in% c(610, 620)), alpha = 0, color = "black", size = 0.1) +
  geom_sf(data = goa_west, alpha = 0, color = "black", size = 0.1) +
  geom_sf(data = obs_catch %>% filter(season == 'A'), aes(geometry = geometry, color = as.factor(gear)), size = 0.5) +
  geom_sf(data = goa_layers$akland, fill = "#2c3e50", color = "white") +
  coord_sf(xlim = c(-170, -153),  # Longitude (Negative for West)
           ylim = c(52, 58),      # Latitude
           crs = "+proj=longlat +datum=WGS84") + # View in Lat/Lon for easier verification
  labs(title = "Fishery events (A season, 77%) - CONFIDENTIAL",
       x = "Longitude",
       y = "Latitude",
       color = 'Gear') +
  theme(panel.background = element_rect(fill = "aliceblue"))

ggsave(here::here(new_year, 'rsch', 'wgoa', 'figs', 'Fishery_A.png'), width = 10, height = 10, units = "in", dpi = 300)


ggplot() +
  geom_sf(data = nmfs_areas %>% filter(REP_AREA %in% c(610, 620)), alpha = 0, color = "black", size = 0.1) +
  geom_sf(data = goa_west, alpha = 0, color = "black", size = 0.1) +
  geom_sf(data = obs_catch %>% filter(season == 'B'), aes(geometry = geometry, color = as.factor(gear)), size = 0.5) +
  geom_sf(data = goa_layers$akland, fill = "#2c3e50", color = "white") +
  coord_sf(xlim = c(-170, -153),  # Longitude (Negative for West)
           ylim = c(52, 58),      # Latitude
           crs = "+proj=longlat +datum=WGS84") + # View in Lat/Lon for easier verification
  labs(title = "Fishery events (B season, 23%) - CONFIDENTIAL",
       x = "Longitude",
       y = "Latitude",
       color = 'Gear') +
  theme(panel.background = element_rect(fill = "aliceblue"))

ggsave(here::here(new_year, 'rsch', 'wgoa', 'figs', 'Fishery_B.png'), width = 10, height = 10, units = "in", dpi = 300)

