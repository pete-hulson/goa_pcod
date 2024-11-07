# testing out mapping ----

install.packages(c("cowplot", "googleway", "ggplot2", "ggrepel", 
                   "ggspatial", "libwgeom", "sf", "rnaturalearth", "rnaturalearthdata"))




library("ggplot2")
theme_set(theme_bw())
library("sf")


library("rnaturalearth")
library("rnaturalearthdata")

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

test %>% 
  dplyr::mutate(haul_date = lubridate::date(haul_date),
                lat = (start_lat + end_lat) / 2,
                lon = (start_lon + end_lon) / 2,
                gear = case_when(gear == "POT" ~ "Pot",
                                 gear == "HAL" ~ "Longline",
                                 .default = gear)) %>% 
  tidytable::select(-start_lat, -end_lat, -start_lon, -end_lon) -> plot_data

ggplot(data = rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")) +
  geom_sf() +
  coord_sf(xlim = c(-170, -130), ylim = c(52, 62), expand = FALSE) +
  geom_point(data = plot_data, aes(x = lon, y = lat, size = weight_kg, col = weight_kg)) +
  scico::scale_color_scico(palette = 'roma', direction = -1) +
  facet_wrap(~gear, ncol = 1)
