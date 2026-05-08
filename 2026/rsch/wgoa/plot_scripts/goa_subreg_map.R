# load libraries
library(tidyverse)
library(sf)
library(akgfmaps)

# get year folder
new_year <- as.numeric(format(Sys.Date(), format = "%Y"))

# plot GOA NMFS management areas
nmfs_areas <- akgfmaps::get_nmfs_areas(set.crs = "EPSG:4326")
goa_layers <- akgfmaps::get_base_layers(select.region = "goa", set.crs = "EPSG:3338")

ggplot() +
  geom_sf(data = akgfmaps::get_nmfs_areas(set.crs = "EPSG:4326") %>% filter(REP_AREA %in% c(610, 620, 630)), alpha = 0, color = "black", size = 0.1) +
  geom_sf(data = goa_layers$akland, fill = "#2c3e50", color = "white") +
  coord_sf(xlim = c(-170, -146),  # Longitude (Negative for West)
           ylim = c(50, 62),      # Latitude
           crs = "+proj=longlat +datum=WGS84") + # View in Lat/Lon for easier verification
  labs(title = "NMFS Management areas",
       x = "Longitude",
       y = "Latitude",
       color = 'Program') +
  theme(panel.background = element_rect(fill = "aliceblue"))
ggsave(here::here(new_year, 'rsch', 'wgoa', 'figs', 'NMFS_Map.png'), width = 10, height = 10, units = "in", dpi = 300)

