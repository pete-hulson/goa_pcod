# load libraries
library(sf)
library(tidyverse)

# get year folder
new_year <- as.numeric(format(Sys.Date(), format = "%Y"))

# plot adf&g stat areas ----
# 1. Unzip the file if you haven't already
if(!('ADFG_data' %in% list.files(here::here(new_year, 'rsch', 'wgoa', 'data')))){
  unzip(zipfile = here::here(new_year, 'rsch', 'wgoa', 'data', 'ADFG_areas.zip'), 
        exdir = here::here(new_year, 'rsch', 'wgoa', 'data', 'ADFG_data'))}

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
