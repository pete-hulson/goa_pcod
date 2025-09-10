# testing out mapping ----

library(here)
library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)

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
                    area %in% c(610, 620, 630)) %>% 
  tidytable::drop_na() -> plot_data

vroom::vroom_write(plot_data, here::here('2025', 'rsch', 'spatial', 'data', 'obs_catch.csv'), delim = ',')





# Read in maps here
west <- ne_states(c("United States of America", "Russia", "Canada"), returnclass = "sf")
west <- st_shift_longitude(west) # shift ongitude for plotting

# Read in stat areas
nmfs_areas <- read_sf(dsn = here("2025", "rsch", "spatial", "NMFS_Stat_Areas", "Sablefish_Longline_Area"), layer = "Sablefish_Longline_Area")
nmfs_areas <- nmfs_areas %>% mutate(GEN_NAME = ifelse(NAME %in% c("East Yakutat / Southeast Alaska", "West Yakutat"), "Eastern Gulf of Alaska", "A")) %>% 
  mutate(         NAME = case_when(
    NAME == "Aleutian Islands" ~ "AI",
    NAME == "Bering Sea" ~ "EBS",
    NAME == "Western Gulf of Alaska" ~ "WGOA",
    NAME == "Central Gulf of Alaska" ~ "CGOA",
    NAME == "West Yakutat" ~ "EGOA",
    NAME == "East Yakutat / Southeast Alaska" ~ "EGOA"
  ), NAME = factor(NAME, levels = c("EBS", "AI", "WGOA", "CGOA", "EGOA"))) %>% 
  group_by(NAME) %>%
  summarise(geometry = st_union(geometry))

# Coerce longline areas
nmfs_areas <- st_make_valid(nmfs_areas) # make valid so that vertices aren't duplicated
nmfs_areas <- nmfs_areas %>% st_transform(4326) # transform to crs 4326
nmfs_areas <- st_shift_longitude(nmfs_areas) # shift longitude for plotting

# get centroids of the geometry for plotting
centroids <- nmfs_areas %>%
  group_by(NAME) %>%
  summarise(geometry = st_centroid(geometry)) %>%
  ungroup()

centroids <- nmfs_areas %>%
  tidytable::summarise(geometry = st_centroid(geometry), .by = NAME) %>% 
  tidytable::filter(!NAME %in% c('AI', 'EGOA')) %>% 
  tidytable::mutate(NAME = factor(case_when(NAME == 'EBS' ~ 'EBS|NBS',
                                            .default = NAME),
                                  levels = c('EBS|NBS', 'WGOA', 'CGOA')))


colors <- unname(ggthemes::ggthemes_data[["colorblind"]][["value"]]) # get colors




plot_data %>% 
  tidytable::mutate(lon = (lon +360) %% 360) -> plot_data

test <- data.frame(plot_data %>% 
                     select(lon, lat, catch = weight_kg))

test_sf <- st_as_sf(test, coords = c("lon", "lat"), crs = 4326)

test_sf_proj <- st_transform(test_sf, 54009)

# Grid size: 5000 m (5 km)
grid_5km <- st_make_grid(test_sf_proj, cellsize = 5000, square = TRUE)

# Convert to sf
grid_5km_sf <- st_sf(grid_id = 1:length(grid_5km), geometry = grid_5km)


test_joined <- st_join(test_sf_proj, grid_5km_sf)

# Aggregate catch per grid
catch_grid <- test_joined %>%
  st_drop_geometry() %>%
  group_by(grid_id) %>%
  summarise(total_catch = sum(catch))

# Join back to grid polygons
grid_catch <- grid_5km_sf %>%
  left_join(catch_grid, by = "grid_id")





ggplot(grid_catch) +
  # geom_sf(aes(fill = total_catch), color = NA) +
  geom_sf(data = nmfs_areas, alpha = 0.25) +
  geom_sf(data = west, lwd = 0.05, color = 'black', alpha = 1) +
  coord_sf(ylim = c(51, 60), xlim = c(190, 215))  +
  scale_fill_viridis_c(option = "magma", na.value = "transparent") +
  # theme_minimal() +
  labs(fill = "Catch (tons)", title = "Fishery Catch Aggregated to 5 km Grids")














ggplot(grid_catch) +
  geom_sf(aes(fill = total_catch), color = NA) +
  geom_sf(data = nmfs_areas, alpha = 0.25) +
  geom_sf(data = west, lwd = 0.05, color = 'black', alpha = 1) +
  coord_sf(ylim = c(51, 60), xlim = c(190, 215)) + # Restrict Map Area +
  scale_fill_viridis_c(option = "magma", na.value = "transparent") +
  # theme_minimal() +
  guides(linewidth = 'none',
         size = 'none') +
  theme_bw(base_size = 24) +
  labs(x = "Longitude", y = "Latitude")=


world <- ne_countries(scale = "medium", returnclass = "sf")
fishery <- data.frame(
  lon = runif(100, -180, 180),
  lat = runif(100, -90, 90),
  catch = runif(100, 10, 500)   # tons of catch
)
ggplot(data = world) +
  geom_sf(fill = "gray95", color = "gray50") +
  stat_density_2d_filled(
    data = fishery,
    aes(x = lon, y = lat, fill = after_stat(level)),
    alpha = 0.7,
    contour_var = "ndensity"
  ) +
  scale_fill_viridis_d(option = "plasma") +
  coord_sf() +
  theme_minimal()






ggplot() +
  geom_sf(data = nmfs_areas, alpha = 0.25) +
  geom_sf(data = west, lwd = 0.05, color = 'black', alpha = 1) + # World Map
  geom_curve(data = move_df_with_coords %>% filter(rel_region != rec_region),
             aes(x = from_x, y = from_y, xend = to_x, yend = to_y, color = rec_region, linewidth = prop),
             curvature = 0.4, alpha = 0.25, lineend = 'round') + # movement arrows (from != to)
  geom_text(data = move_df_with_coords %>% filter(rel_region != rec_region),
            aes(x = to_x, y = to_y, label = paste(round(prop * 100, 1), "%", sep = ''), color = rec_region), alpha = 1, size = 6.5) + # from != to
  # geom_text(data = move_df_with_coords %>% tidytable::summarise(n = sum, .by = c(rel_region, Type)) %>% distinct(rel_region, Type, n),
  #           aes(x = 212, y = 47, label = paste0('n = ', n)), alpha = 1, size = 6.5) + # sample size
  scale_color_manual(values = colors[-c(1,5)]) +
  geom_text(data = move_df_with_coords %>% filter(rel_region == rec_region), size = 6.5, # from == to 
            aes(x = from_x, y = from_y, label = paste(round(prop * 100, 1), "%", sep = '')), alpha = 1, color = 'black', fontface = "bold") +
  facet_grid(Type ~ rel_region) +
  guides(linewidth = 'none',
         size = 'none') +
  coord_sf(ylim = c(45.2, 65), xlim = c(180, 220)) + # Restrict Map Area
  theme_bw(base_size = 24) +
  labs(x = "Longitude", y = "Latitude", color = "Recovery Region", label = "Percentage Recovered") +
  theme(legend.position = 'top',
        legend.box = 'vertical',
        legend.background = element_blank(),
        legend.spacing.y = unit(-1, 'cm'),  # Adjust spacing between items
        plot.background = element_rect(fill = "transparent", colour = NA),
        axis.text.x = element_text(angle = 90, vjust = 0.5))








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
