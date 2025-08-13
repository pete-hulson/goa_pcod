# Purpose: To plot movement estimates across sapce

library(here)
library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)

# Read in tag data
tag_dat <- readRDS(here("2025", "rsch", "spatial", "tag_out.RDS"))

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

colors <- unname(ggthemes::ggthemes_data[["colorblind"]][["value"]]) # get colors

# Plot (All Tags) ---------------------------------------------------------
all_df <- tag_dat$all %>% 
  filter(!rec_region %in% c("RUS", "NBS", "CS"),
         rel_region != "NBS") %>% 
  group_by(rel_region) %>% 
  mutate(sum = sum(n),
         prop = n / sum)

# Left join centroids to dataframe for plotting where from -> to
move_df_with_coords <- all_df %>%
  left_join(centroids, by = c("rel_region" = "NAME")) %>%
  rename(from_geometry = geometry) %>%
  left_join(centroids, by = c("rec_region" = "NAME")) %>%
  rename(to_geometry = geometry) %>%
  mutate(
    from_x = st_coordinates(st_shift_longitude(from_geometry))[, 1], # get from centroids
    from_y = st_coordinates(from_geometry)[, 2],
    to_x = st_coordinates(st_shift_longitude(to_geometry))[, 1], # get to centroids
    to_y = st_coordinates(to_geometry)[, 2]
  ) %>% 
  mutate(rel_region = factor(rel_region, levels = c("EBS", "AI", "WGOA", "CGOA")))

ggplot() +
  geom_sf(data = nmfs_areas, alpha = 0.55) +
  geom_sf(data = west, lwd = 0.05, color = 'black', alpha = 1) + # World Map
  geom_curve(data = move_df_with_coords %>% filter(rel_region != rec_region),
             aes(x = from_x, y = from_y, xend = to_x, yend = to_y, color = rel_region, size = prop),
             curvature = 0.4, alpha = 0.45) + # movement arrows (from != to)
  geom_text(data = move_df_with_coords %>% filter(rel_region != rec_region),
            aes(x = to_x, y = to_y, label = round(prop, 2), color = rec_region), alpha = 1, size = 8.5, nudge_y = 0.07) + # from != to
  scale_color_manual(values = colors[-c(1,5)]) +
  geom_text(data = move_df_with_coords %>% filter(rel_region == rec_region), size = 8.5, # from == to 
            aes(x = from_x, y = from_y, label = round(prop, 2)), alpha = 1, color = 'black', nudge_y = 0.07) +
  facet_grid(~rel_region) +
  coord_sf(ylim = c(45.2, 70.5), xlim = c(165, 230)) + # Restrict Map Area
  theme_bw(base_size = 24) +
  labs(x = "Longitude", y = "Latitude", size = 'Percentage Recovered',
       color = "Recovery Region", label = "Percentage Recovered") +
  theme(legend.position = 'top',
        legend.box = 'vertical',
        legend.background = element_blank(),
        legend.spacing.y = unit(-1, 'cm'),  # Adjust spacing between items
        plot.background = element_rect(fill = "transparent", colour = NA),
        axis.text.x = element_text(angle = 90, vjust = 0.5))

# Plot (Spawning) ---------------------------------------------------------
spawn_df <- tag_dat$spawn %>% 
  filter(!rec_region %in% c("RUS", "NBS", "CS"),
         rel_region != "NBS") %>% 
  group_by(rel_region) %>% 
  mutate(sum = sum(n),
         prop = n / sum)

# Left join centroids to dataframe for plotting where from -> to
move_df_with_coords <- spawn_df %>%
  left_join(centroids, by = c("rel_region" = "NAME")) %>%
  rename(from_geometry = geometry) %>%
  left_join(centroids, by = c("rec_region" = "NAME")) %>%
  rename(to_geometry = geometry) %>%
  mutate(
    from_x = st_coordinates(st_shift_longitude(from_geometry))[, 1], # get from centroids
    from_y = st_coordinates(from_geometry)[, 2],
    to_x = st_coordinates(st_shift_longitude(to_geometry))[, 1], # get to centroids
    to_y = st_coordinates(to_geometry)[, 2]
  ) %>% 
  mutate(rel_region = factor(rel_region, levels = c("EBS", "AI", "WGOA", "CGOA")))

ggplot() +
  geom_sf(data = nmfs_areas, alpha = 0.55) +
  geom_sf(data = west, lwd = 0.05, color = 'black', alpha = 1) + # World Map
  geom_curve(data = move_df_with_coords %>% filter(rel_region != rec_region),
             aes(x = from_x, y = from_y, xend = to_x, yend = to_y, color = rel_region, size = prop),
             curvature = 0.4, alpha = 0.45) + # movement arrows (from != to)
  geom_text(data = move_df_with_coords %>% filter(rel_region != rec_region),
            aes(x = to_x, y = to_y, label = round(prop, 2), color = rec_region), alpha = 1, size = 8.5, nudge_y = 0.07) + # from != to
  scale_color_manual(values = colors[-c(1,5)]) +
  geom_text(data = move_df_with_coords %>% filter(rel_region == rec_region), size = 8.5, # from == to 
            aes(x = from_x, y = from_y, label = round(prop, 2)), alpha = 1, color = 'black', nudge_y = 0.07) +
  facet_grid(~rel_region) +
  coord_sf(ylim = c(45.2, 70.5), xlim = c(165, 230)) + # Restrict Map Area
  theme_bw(base_size = 24) +
  labs(x = "Longitude", y = "Latitude", size = 'Percentage Recovered',
       color = "Recovery Region", label = "Percentage Recovered") +
  theme(legend.position = 'top',
        legend.box = 'vertical',
        legend.background = element_blank(),
        legend.spacing.y = unit(-1, 'cm'),  # Adjust spacing between items
        plot.background = element_rect(fill = "transparent", colour = NA),
        axis.text.x = element_text(angle = 90, vjust = 0.5))

# Plot (Not Spawning) ---------------------------------------------------------
notspawn_df <- tag_dat$not_spawn %>% 
  filter(!rec_region %in% c("RUS", "NBS", "CS"),
         rel_region != "NBS") %>% 
  group_by(rel_region) %>% 
  mutate(sum = sum(n),
         prop = n / sum)

# Left join centroids to dataframe for plotting where from -> to
move_df_with_coords <- notspawn_df %>%
  left_join(centroids, by = c("rel_region" = "NAME")) %>%
  rename(from_geometry = geometry) %>%
  left_join(centroids, by = c("rec_region" = "NAME")) %>%
  rename(to_geometry = geometry) %>%
  mutate(
    from_x = st_coordinates(st_shift_longitude(from_geometry))[, 1], # get from centroids
    from_y = st_coordinates(from_geometry)[, 2],
    to_x = st_coordinates(st_shift_longitude(to_geometry))[, 1], # get to centroids
    to_y = st_coordinates(to_geometry)[, 2]
  ) %>% 
  mutate(rel_region = factor(rel_region, levels = c("EBS", "AI", "WGOA", "CGOA")))

ggplot() +
  geom_sf(data = nmfs_areas, alpha = 0.55) +
  geom_sf(data = west, lwd = 0.05, color = 'black', alpha = 1) + # World Map
  geom_curve(data = move_df_with_coords %>% filter(rel_region != rec_region),
             aes(x = from_x, y = from_y, xend = to_x, yend = to_y, color = rel_region, size = prop),
             curvature = 0.4, alpha = 0.45) + # movement arrows (from != to)
  geom_text(data = move_df_with_coords %>% filter(rel_region != rec_region),
            aes(x = to_x, y = to_y, label = round(prop, 2), color = rec_region), alpha = 1, size = 8.5, nudge_y = 0.07) + # from != to
  scale_color_manual(values = colors[-c(1,5)]) +
  geom_text(data = move_df_with_coords %>% filter(rel_region == rec_region), size = 8.5, # from == to 
            aes(x = from_x, y = from_y, label = round(prop, 2)), alpha = 1, color = 'black', nudge_y = 0.07) +
  facet_grid(~rel_region) +
  coord_sf(ylim = c(45.2, 70.5), xlim = c(165, 230)) + # Restrict Map Area
  theme_bw(base_size = 24) +
  labs(x = "Longitude", y = "Latitude", size = 'Percentage Recovered',
       color = "Recovery Region", label = "Percentage Recovered") +
  theme(legend.position = 'top',
        legend.box = 'vertical',
        legend.background = element_blank(),
        legend.spacing.y = unit(-1, 'cm'),  # Adjust spacing between items
        plot.background = element_rect(fill = "transparent", colour = NA),
        axis.text.x = element_text(angle = 90, vjust = 0.5))
