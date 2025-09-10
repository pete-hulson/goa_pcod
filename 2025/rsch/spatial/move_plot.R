# Purpose: To plot movement estimates across sapce

library(here)
library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)

# Read in tag data
tag_dat <- readRDS(here("2025", "rsch", "spatial", 'data', "tag_out.RDS"))

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

# Plot ---------------------------------------------------------
# Plot ---------------------------------------------------------
move_df_full <- tag_dat$all %>% 
  tidytable::filter(!rec_region %in% c('RUS', 'CS', 'AI', 'EGOA'),
                    !rel_region %in% c('RUS', 'CS', 'AI', 'EGOA')) %>% 
  tidytable::mutate(rel_region = factor(case_when(rel_region %in% c('EBS', 'NBS') ~ 'EBS|NBS',
                                                  .default = rel_region), 
                                        levels = c('EBS|NBS', "WGOA", "CGOA")),
                    rec_region = factor(case_when(rec_region %in% c('EBS', 'NBS') ~ 'EBS|NBS',
                                                  .default = rec_region), 
                                        levels = c('EBS|NBS', "WGOA", "CGOA"))) %>%
  tidytable::summarise(n = sum(n), .by = c(rel_region, rec_region)) %>% 
  tidytable::mutate(sum = sum(n),
                    prop = n / sum,
                    Type = 'All', .by = rel_region) %>% 
  # combine spawning and not spawning recoveries
  bind_rows(
    tag_dat$spawn %>% 
      tidytable::filter(!rec_region %in% c('RUS', 'CS', 'AI', 'EGOA'),
                        !rel_region %in% c('RUS', 'CS', 'AI', 'EGOA')) %>% 
      tidytable::mutate(rel_region = factor(case_when(rel_region %in% c('EBS', 'NBS') ~ 'EBS|NBS',
                                                      .default = rel_region), 
                                            levels = c('EBS|NBS', "WGOA", "CGOA")),
                        rec_region = factor(case_when(rec_region %in% c('EBS', 'NBS') ~ 'EBS|NBS',
                                                      .default = rec_region), 
                                            levels = c('EBS|NBS', "WGOA", "CGOA"))) %>%
      tidytable::summarise(n = sum(n), .by = c(rel_region, rec_region)) %>% 
      tidytable::mutate(sum = sum(n),
                        prop = n / sum,
                        Type = 'Spawn', .by = rel_region),
    tag_dat$not_spawn %>% 
      tidytable::filter(!rec_region %in% c('RUS', 'CS', 'AI', 'EGOA'),
                        !rel_region %in% c('RUS', 'CS', 'AI', 'EGOA')) %>% 
      tidytable::mutate(rel_region = factor(case_when(rel_region %in% c('EBS', 'NBS') ~ 'EBS|NBS',
                                                      .default = rel_region), 
                                            levels = c('EBS|NBS', "WGOA", "CGOA")),
                        rec_region = factor(case_when(rec_region %in% c('EBS', 'NBS') ~ 'EBS|NBS',
                                                      .default = rec_region), 
                                            levels = c('EBS|NBS', "WGOA", "CGOA"))) %>%
      tidytable::summarise(n = sum(n), .by = c(rel_region, rec_region)) %>% 
      tidytable::mutate(sum = sum(n),
                        prop = n / sum,
                        Type = 'Not Spawn', .by = rel_region)
  )

vroom::vroom_write(move_df_full, here::here('2025', 'rsch', 'spatial', 'data', 'move_df_full.csv'), delim = ',')

# combine spawning and not spawning recoveries
move_df <- tag_dat$spawn %>% 
  tidytable::filter(!rec_region %in% c('RUS', 'CS', 'AI', 'EGOA'),
                    !rel_region %in% c('RUS', 'CS', 'AI', 'EGOA')) %>% 
  tidytable::mutate(rel_region = factor(case_when(rel_region %in% c('EBS', 'NBS') ~ 'EBS|NBS',
                                                  .default = rel_region), 
                                        levels = c('EBS|NBS', "WGOA", "CGOA")),
                    rec_region = factor(case_when(rec_region %in% c('EBS', 'NBS') ~ 'EBS|NBS',
                                                  .default = rec_region), 
                                        levels = c('EBS|NBS', "WGOA", "CGOA"))) %>%
  tidytable::summarise(n = sum(n), .by = c(rel_region, rec_region)) %>% 
  tidytable::mutate(sum = sum(n),
                    prop = n / sum,
                    Type = 'Jan-Apr', .by = rel_region) %>% 
  tidytable::bind_rows(tag_dat$not_spawn %>% 
                         tidytable::filter(!rec_region %in% c('RUS', 'CS', 'AI', 'EGOA'),
                                           !rel_region %in% c('RUS', 'CS', 'AI', 'EGOA')) %>% 
                         tidytable::mutate(rel_region = factor(case_when(rel_region %in% c('EBS', 'NBS') ~ 'EBS|NBS',
                                                                         .default = rel_region), 
                                                               levels = c('EBS|NBS', "WGOA", "CGOA")),
                                           rec_region = factor(case_when(rec_region %in% c('EBS', 'NBS') ~ 'EBS|NBS',
                                                                         .default = rec_region), 
                                                               levels = c('EBS|NBS', "WGOA", "CGOA"))) %>%
                         tidytable::summarise(n = sum(n), .by = c(rel_region, rec_region)) %>% 
                         tidytable::mutate(sum = sum(n),
                                           prop = n / sum,
                                           Type = 'May-Dec', .by = rel_region)) %>% 
  tidytable::mutate(Type = factor(Type, levels = c('Jan-Apr', 'May-Dec')))



# Left join centroids to dataframe for plotting where from -> to
move_df_with_coords <- move_df %>%
  tidytable::left_join(centroids, by = c("rel_region" = "NAME")) %>%
  tidytable::rename(from_geometry = geometry) %>%
  tidytable::left_join(centroids, by = c("rec_region" = "NAME")) %>%
  tidytable::rename(to_geometry = geometry) %>%
  tidytable::mutate(from_x = st_coordinates(st_shift_longitude(from_geometry))[, 1], # get from centroids
                    from_y = st_coordinates(from_geometry)[, 2],
                    to_x = st_coordinates(st_shift_longitude(to_geometry))[, 1], # get to centroids
                    to_y = st_coordinates(to_geometry)[, 2])

# move_df_with_coords$to_x[is.na(move_df_with_coords$to_x)] <- 170
# move_df_with_coords$to_y[is.na(move_df_with_coords$to_y)] <- 70

ggplot() +
  geom_sf(data = nmfs_areas, alpha = 0.25) +
  geom_sf(data = west, lwd = 0.05, color = 'black', alpha = 1) + # World Map
  geom_curve(data = move_df_with_coords %>% filter(rel_region != rec_region),
             aes(x = from_x, y = from_y, xend = to_x, yend = to_y, color = rec_region, linewidth = prop),
             curvature = 0.4, alpha = 0.25, lineend = 'round') + # movement arrows (from != to)
  geom_text(data = move_df_with_coords %>% filter(rel_region != rec_region),
            aes(x = to_x, y = to_y, label = paste(round(prop * 100, 1), "%", sep = ''), color = rec_region), alpha = 1, size = 6.5) + # from != to
  geom_text(data = move_df_with_coords %>% tidytable::summarise(n = sum, .by = c(rel_region, Type)) %>% distinct(rel_region, Type, n),
            aes(x = 212, y = 47, label = paste0('n = ', n)), alpha = 1, size = 6.5) + # sample size
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

# Save plot
ggsave(here("2025", "rsch", "spatial", 'figures', "move_plot.png"), width = 16, height = 12, dpi = 300, bg = 'transparent')
