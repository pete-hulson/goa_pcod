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
                    month < 5,
                    area == 610) -> plot_data

ggplot(data = rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")) +
  geom_sf() +
  coord_sf(xlim = c(-168, -158), ylim = c(53, 56), expand = FALSE) +
  geom_point(data = plot_data, aes(x = lon, y = lat, size = weight_kg, col = weight_kg, shape = as.factor(year))) +
  scico::scale_color_scico(palette = 'roma', direction = -1) +
  labs(shape = 'year', title = "jan - mar")


# plot observed catch in wgoa outside of spring (May on)
obs %>% 
  dplyr::mutate(haul_date = lubridate::date(hday),
                month = lubridate::month(haul_date)) %>% 
  tidytable::select(-hday) %>% 
  tidytable::filter(year >= 2021,
                    month >= 5,
                    area == 610) -> plot_data

ggplot(data = rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")) +
  geom_sf() +
  coord_sf(xlim = c(-168, -158), ylim = c(53, 56), expand = FALSE) +
  geom_point(data = plot_data, aes(x = lon, y = lat, size = weight_kg, col = weight_kg, shape = as.factor(year))) +
  scico::scale_color_scico(palette = 'roma', direction = -1) +
  labs(shape = 'year', title = "may - dec")


# plot wgoa survey strata
goa_layers <- akgfmaps::get_base_layers(select.region = "goa",
                                        set.crs = "EPSG:3338") # Alaska Albers Equal area
names(goa_layers)


wgoa_strat <-c(10, 11, 12, 13, 110, 111, 112)


ggplot() +
  geom_sf(data = goa_layers$survey.strata[goa_layers$survey.strata$STRATUM %in% wgoa_strat,], 
          aes(fill = as.factor(STRATUM))) +
  # scico::scale_fill_scico_d(palette = 'roma') +
  geom_sf_text(data = goa_layers$survey.strata[goa_layers$survey.strata$STRATUM %in% wgoa_strat,],
               mapping = aes(label = STRATUM)) +
  labs(fill = 'stratum')

# tag releases are in 11 (43% of biom in 610 in last 3 surveys), 12 (26% of biom), 13 (20% of biom)
