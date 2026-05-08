# load libraries
library(sf)
library(tidyverse)
library(akgfmaps)
# library(geosphere)
# library(janitor)
# library(gridExtra)
# library(grid)
# remotes::install_github("afsc-gap-products/akgfmaps", build_vignettes = TRUE)

# get year folder
new_year <- as.numeric(format(Sys.Date(), format = "%Y"))

# plot survey strata ----

# get data
nmfs_areas <- akgfmaps::get_nmfs_areas(set.crs = "EPSG:4326")
goa_layers_curr <- akgfmaps::get_base_layers(select.region = "goa", set.crs = "EPSG:3338")
goa_layers_hist <- akgfmaps::get_base_layers(select.region = "goa", design.year = 1984, set.crs = "EPSG:3338")
goa_strata_curr <- goa_layers_curr$survey.strata
goa_strata_hist <- goa_layers_hist$survey.strata
srvy_age <- vroom::vroom(here::here(new_year, 'rsch', 'wgoa', 'data', 'twl_srvy_age.csv'), delim = ',')

# Calculate centroids and keep the STRATUM ID
strata_centers_curr <- st_centroid(goa_strata_curr)
strata_centers_hist <- st_centroid(goa_strata_hist)

# filter strata west of -156
strata_with_coords_curr <- goa_strata_curr %>%
  st_centroid() %>%
  st_transform(crs = 4326) %>% # Transform to WGS84 (Lat/Lon)
  st_coordinates() %>%
  as.data.frame()
goa_west_curr <- goa_strata_curr %>%
  mutate(long = strata_with_coords_curr$X) %>%
  filter(long < -156)

strata_with_coords_hist <- goa_strata_hist %>%
  st_centroid() %>%
  st_transform(crs = 4326) %>% # Transform to WGS84 (Lat/Lon)
  st_coordinates() %>%
  as.data.frame()
goa_west_hist <- goa_strata_hist %>%
  mutate(long = strata_with_coords_hist$X) %>%
  filter(long < -156)

# plot 2025 survey with age data locations
srvy_age_25 <- srvy_age %>% 
  filter(year == 2025)

points_sf_25 <- st_as_sf(srvy_age_25, coords = c("long_mid", "lat_mid"), crs = 4326)

ggplot() +
  geom_sf(data = goa_west_curr, aes(fill = as.factor(STRATUM)), color = "white", size = 0.1) +
  geom_sf_text(data = goa_west_curr, aes(label = STRATUM)) +
  geom_sf(data = points_sf_25, size = 0.5, aes(geometry = geometry)) +
  geom_sf(data = goa_layers_curr$akland, fill = "#2c3e50", color = "white") +
  geom_sf(data = nmfs_areas %>% filter(REP_AREA %in% c(610, 620)), alpha = 0, color = "black", size = 0.1) +
  coord_sf(xlim = c(-170, -154),  # Longitude (Negative for West)
           ylim = c(52, 58),      # Latitude
           crs = "+proj=longlat +datum=WGS84") + # View in Lat/Lon for easier verification
  labs(title = "2025 survey strata (age collections)",
       x = "Longitude",
       y = "Latitude",
       fill = 'Strata') +
  theme(panel.background = element_rect(fill = "aliceblue"))

ggsave(here::here(new_year, 'rsch', 'wgoa', 'figs', 'GOA_Survey_Map_2025.png'), width = 10, height = 10, units = "in", dpi = 300)

# plot pre-2023 surveys with age data locations
srvy_age_hist <- srvy_age %>% 
  filter(year < 2025)

points_sf_hist <- st_as_sf(srvy_age_hist, coords = c("long_mid", "lat_mid"), crs = 4326)

ggplot() +
  geom_sf(data = goa_west_hist, aes(fill = as.factor(STRATUM)), color = "white", size = 0.1) +
  geom_sf_text(data = goa_west_hist, aes(label = STRATUM)) +
  geom_sf(data = points_sf_hist, size = 0.5, aes(geometry = geometry, color = factor(year))) +
  geom_sf(data = goa_layers_hist$akland, fill = "#2c3e50", color = "white") +
  geom_sf(data = nmfs_areas %>% filter(REP_AREA %in% c(610, 620)), alpha = 0, color = "black", size = 0.1) +
  coord_sf(xlim = c(-170, -154),  # Longitude (Negative for West)
           ylim = c(52, 58),      # Latitude
           crs = "+proj=longlat +datum=WGS84") + # View in Lat/Lon for easier verification
  labs(title = "Historical survey strata (age collections)",
       x = "Longitude",
       y = "Latitude",
       fill = 'Strata',
       color = 'Year collected') +
  theme(panel.background = element_rect(fill = "aliceblue"))

ggsave(here::here(new_year, 'rsch', 'wgoa', 'figs', 'GOA_Survey_Map_Hist.png'), width = 10, height = 10, units = "in", dpi = 300)
