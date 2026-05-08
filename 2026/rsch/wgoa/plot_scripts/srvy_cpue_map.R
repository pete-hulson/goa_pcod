# load libraries
library(tidyverse)
library(sf)
library(akgfmaps)

# get year folder
new_year <- as.numeric(format(Sys.Date(), format = "%Y"))

# plot GOA NMFS management areas
nmfs_areas <- akgfmaps::get_nmfs_areas(set.crs = "EPSG:4326")
goa_layers <- akgfmaps::get_base_layers(select.region = "goa", set.crs = "EPSG:3338")

srvy_cpue <- vroom::vroom(here::here(new_year, 'rsch', 'wgoa', 'data', 'twl_srvy_cpue.csv'))

ggplot() +
  geom_sf(data = goa_layers$akland, fill = "#2c3e50", color = "white") +
  coord_sf(xlim = c(-170, -153),  # Longitude (Negative for West)
           ylim = c(52, 58),      # Latitude
           crs = "+proj=longlat +datum=WGS84") + # View in Lat/Lon for easier verification
  geom_point(data = srvy_cpue %>% filter(year >=2015), aes(x = long_mid, y = lat_mid, color = wt_cpue), size = 0.5) +
  theme_minimal() +
  labs(title = "GOA NMFS Management Areas and Survey CPUE",
       x = "Longitude",
       y = "Latitude")
