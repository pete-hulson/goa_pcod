# testing out mapping ----

# cran pkgs
# install.packages(c("cowplot", "googleway", "ggplot2", "ggrepel", 
#                    "ggspatial", "libwgeom", "sf", "rnaturalearth", "rnaturalearthdata"))

# github pkgs
# devtools::install_github("https://github.com/afsc-gap-products/akgfmaps")

# load packages
library("akgfmaps")
library("ggplot2")
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
library("raster")
library("geodata")

# set plot stuff
theme_set(theme_bw())
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

# read in observer data
obs <- vroom::vroom(here::here(2024, 'data', 'raw', 'fish_obs_catch.csv'), delim = ',')


# plot observed catch in wgoa in spring (Jan-Mar)
obs %>% 
  dplyr::mutate(haul_date = lubridate::date(hday),
                month = lubridate::month(haul_date)) %>% 
  tidytable::select(-hday) %>% 
  tidytable::filter(year >= 2021,
                    month < 4,
                    area %in% c(610, 620, 630)) -> plot_data


# plot
lon = c(-168, -145)
lat = c(53, 62)

terr1 <- raster::getData("SRTM", lat = min(lat), lon = min(lon))
terr2 <- raster::getData("SRTM", lat = max(lat), lon = max(lon))


terr <- geodata::elevation_global(res = 0.5, path = here::here('2025', 'rsch', 'spatial'))

ggplot(data = rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")) +
  # geom_raster(aes(lon, lat))+
  # scale_fill_gradientn(colours = terrain.colors(10)) +
  geom_sf() +
  coord_sf(xlim = c(-168, -145), ylim = c(53, 62), expand = FALSE) +
  geom_point(data = plot_data, aes(x = lon, y = lat, size = weight_kg, col = weight_kg, shape = as.factor(year))) +
  scico::scale_color_scico(palette = 'roma', direction = -1) +
  labs(shape = 'year', title = "jan - mar")


# plot observed catch in wgoa outside of spring (May on)
obs %>% 
  dplyr::mutate(haul_date = lubridate::date(hday),
                month = lubridate::month(haul_date)) %>% 
  tidytable::select(-hday) %>% 
  tidytable::filter(year >= 2021,
                    month >= 4,
                    area %in% c(610, 620, 630)) -> plot_data

ggplot(data = rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")) +
  geom_sf() +
  coord_sf(xlim = c(-168, -145), ylim = c(53, 62), expand = FALSE) +
  geom_point(data = plot_data, aes(x = lon, y = lat, size = weight_kg, col = weight_kg, shape = as.factor(year))) +
  scico::scale_color_scico(palette = 'roma', direction = -1) +
  labs(shape = 'year', title = "may - dec")


# plot wgoa survey strata
goa_layers <- akgfmaps::get_base_layers(select.region = "goa",
                                        set.crs = "EPSG:3338") # Alaska Albers Equal area
nmfs_areas <- akgfmaps::get_inpfc_strata(select.region = "goa",
                                         set.crs = "EPSG:3338")
names(nmfs_areas)


wgoa_strat <-c(10, 11, 12, 13, 110, 111, 112)

nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)

nc_3857 <- sf::st_transform(nc, 3857)
ggplot() +
  geom_sf(data = nc) +
  geom_sf(data = nc_3857, colour = "red", fill = NA)



terr <- geodata::elevation_global(res = 0.5, path = here::here('2025', 'rsch', 'spatial'))
terr_df <- as.data.frame(terr, xy = TRUE)


ggplot(data = rnaturalearth::ne_countries(scale = "large", returnclass = "sf")) +
  geom_sf(colour = 'black') +
  geom_raster(data = terr_df, aes(x = x, y = y, fill = HARV_dtmCrop)) +
  # geom_sf(data = nmfs_areas) +
  # geom_sf(data = goa_layers$survey.strata[goa_layers$survey.strata$STRATUM %in% wgoa_strat,],
  #         aes(fill = as.factor(STRATUM))) +
  coord_sf(xlim = c(-175, -145), ylim = c(50, 62), expand = FALSE)
  # scico::scale_fill_scico_d(palette = 'roma') +
  # geom_sf_text(data = goa_layers$survey.strata[goa_layers$survey.strata$STRATUM %in% wgoa_strat,],
  #              mapping = aes(label = STRATUM)) +
  labs(fill = 'stratum')

# tag releases are in 11 (43% of biom in 610 in last 3 surveys), 12 (26% of biom), 13 (20% of biom)
