
# plot adf&g stat areas ----
library(sf)
library(ggplot2)
library(tidyverse)
library(akgfmaps)
# remotes::install_github("afsc-gap-products/akgfmaps", build_vignettes = TRUE)


# get year folder
new_year <- as.numeric(format(Sys.Date(), format = "%Y"))



# 1. Unzip the file if you haven't already
unzip(zipfile = here::here(new_year, 'rsch', 'wgoa', 'data', 'ADFG_areas.zip'), 
      exdir = here::here(new_year, 'rsch', 'wgoa', 'data', 'ADFG_data'))

# 2. Load the shapefile
# Note: Point to the .shp file; ensure .dbf, .shx, and .prj are in the same folder
adfg_shapes <- st_read(here::here(new_year, 'rsch', 'wgoa', 'data', 'ADFG_data', 'ADFG_areas.shp'))

# 3. Transform to WGS84 (Longitude/Latitude) to use -155 and -170
adfg_wgs84 <- st_transform(adfg_shapes, crs = 4326)

# 4. Filter for the specific longitude range
# We use st_crop to "clip" the map to your window
map_subset <- st_crop(adfg_wgs84, xmin = -170, xmax = -155, ymin = 50, ymax = 58)

# 5. Plot with Area Numbers
ggplot(data = map_subset) +
  geom_sf(fill = "aliceblue", color = "darkgrey") +
  # geom_sf_text adds the STATAREA labels at the center of each polygon
  geom_sf_text(aes(label = STATAREA), size = 3, check_overlap = TRUE) +
  coord_sf(xlim = c(-170, -155)) +
  theme_minimal() +
  labs(title = "ADF&G Statistical Areas",
       x = "Longitude",
       y = "Latitude")

ggsave(here::here(new_year, 'rsch', 'wgoa', 'figs', 'ADFG_Map.png'), width = 10, height = 10, units = "in", dpi = 300)



# plot survey strata ----

# get data
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

# plot 2025 survey
srvy_age_25 <- srvy_age %>% 
  filter(year == 2025)

points_sf_25 <- st_as_sf(srvy_age_25, coords = c("long_mid", "lat_mid"), crs = 4326)

ggplot() +
  geom_sf(data = goa_west_curr, aes(fill = as.factor(STRATUM)), color = "white", size = 0.1) +
  geom_sf_text(data = goa_west_curr, aes(label = STRATUM)) +
  geom_sf(data = points_sf_25, size = 0.5, aes(geometry = geometry)) +
  geom_sf(data = goa_layers_curr$akland) +
  coord_sf(xlim = c(-170, -154),  # Longitude (Negative for West)
           ylim = c(52, 58),      # Latitude
           crs = "+proj=longlat +datum=WGS84") + # View in Lat/Lon for easier verification
  labs(title = "2025 survey strata",
       x = "Longitude",
       y = "Latitude",
       fill = 'Strata')

ggsave(here::here(new_year, 'rsch', 'wgoa', 'figs', '2025_GOA_Survey_Map.png'), width = 10, height = 10, units = "in", dpi = 300)



# plot pre-2023 surveys
srvy_age_hist <- srvy_age %>% 
  filter(year < 2025)

points_sf_hist <- st_as_sf(srvy_age_hist, coords = c("long_mid", "lat_mid"), crs = 4326)

ggplot() +
  geom_sf(data = goa_west_hist, aes(fill = as.factor(STRATUM)), color = "white", size = 0.1) +
  geom_sf_text(data = goa_west_hist, aes(label = STRATUM)) +
  geom_sf(data = points_sf_hist, size = 0.5, aes(geometry = geometry, color = year)) +
  geom_sf(data = goa_layers_hist$akland) +
  coord_sf(xlim = c(-170, -154),  # Longitude (Negative for West)
           ylim = c(52, 58),      # Latitude
           crs = "+proj=longlat +datum=WGS84") + # View in Lat/Lon for easier verification
  labs(title = "Historical survey strata",
       x = "Longitude",
       y = "Latitude",
       fill = 'Strata',
       color = 'Age sample')

ggsave(here::here(new_year, 'rsch', 'wgoa', 'figs', 'Hist_GOA_Survey_Map.png'), width = 10, height = 10, units = "in", dpi = 300)



