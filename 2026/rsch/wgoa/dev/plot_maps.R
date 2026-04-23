
# load libraries
library(sf)
library(ggplot2)
library(tidyverse)
library(akgfmaps)
library(geosphere)
library(janitor)
library(gridExtra)
library(grid)
# remotes::install_github("afsc-gap-products/akgfmaps", build_vignettes = TRUE)


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



# plot survey strata ----

# get data
nmfs_areas <- akgfmaps::get_nmfs_areas(set.crs = "EPSG:4326")
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

# plot 2025 survey with age data locations
srvy_age_25 <- srvy_age %>% 
  filter(year == 2025)

points_sf_25 <- st_as_sf(srvy_age_25, coords = c("long_mid", "lat_mid"), crs = 4326)

ggplot() +
  geom_sf(data = goa_west_curr, aes(fill = as.factor(STRATUM)), color = "white", size = 0.1) +
  geom_sf_text(data = goa_west_curr, aes(label = STRATUM)) +
  geom_sf(data = points_sf_25, size = 0.5, aes(geometry = geometry)) +
  geom_sf(data = goa_layers_curr$akland, fill = "#2c3e50", color = "white") +
  geom_sf(data = nmfs_areas %>% filter(REP_AREA %in% c(610, 620)), alpha = 0, color = "black", size = 0.1) +
  coord_sf(xlim = c(-170, -154),  # Longitude (Negative for West)
           ylim = c(52, 58),      # Latitude
           crs = "+proj=longlat +datum=WGS84") + # View in Lat/Lon for easier verification
  labs(title = "2025 survey strata (age collections)",
       x = "Longitude",
       y = "Latitude",
       fill = 'Strata') +
  theme(panel.background = element_rect(fill = "aliceblue"))

ggsave(here::here(new_year, 'rsch', 'wgoa', 'figs', 'GOA_Survey_Map_2025.png'), width = 10, height = 10, units = "in", dpi = 300)


# plot pre-2023 surveys with age data locations
srvy_age_hist <- srvy_age %>% 
  filter(year < 2025)

points_sf_hist <- st_as_sf(srvy_age_hist, coords = c("long_mid", "lat_mid"), crs = 4326)

ggplot() +
  geom_sf(data = goa_west_hist, aes(fill = as.factor(STRATUM)), color = "white", size = 0.1) +
  geom_sf_text(data = goa_west_hist, aes(label = STRATUM)) +
  geom_sf(data = points_sf_hist, size = 0.5, aes(geometry = geometry, color = factor(year))) +
  geom_sf(data = goa_layers_hist$akland, fill = "#2c3e50", color = "white") +
  geom_sf(data = nmfs_areas %>% filter(REP_AREA %in% c(610, 620)), alpha = 0, color = "black", size = 0.1) +
  coord_sf(xlim = c(-170, -154),  # Longitude (Negative for West)
           ylim = c(52, 58),      # Latitude
           crs = "+proj=longlat +datum=WGS84") + # View in Lat/Lon for easier verification
  labs(title = "Historical survey strata (age collections)",
       x = "Longitude",
       y = "Latitude",
       fill = 'Strata',
       color = 'Year collected') +
  theme(panel.background = element_rect(fill = "aliceblue"))

ggsave(here::here(new_year, 'rsch', 'wgoa', 'figs', 'GOA_Survey_Map_Hist.png'), width = 10, height = 10, units = "in", dpi = 300)



# plot tag releases ----

## get tag data together ----

if('all_tags.csv' %in% list.files(here::here(new_year, 'rsch', 'wgoa', 'data'))){
  all_tags <- vroom::vroom(here::here(new_year, 'rsch', 'wgoa', 'data', 'all_tags.csv'))
} else{
  
  # data from steve/ingrid
  tag_data <- vroom::vroom(here::here(new_year, 'rsch', 'wgoa', 'data', 'ALL_LENGTH_TAGS.csv'))
  
  # data from kim
  tag_data_new <- vroom::vroom(here::here(new_year, 'rsch', 'wgoa', 'data', 'rel_rec.csv'))
  
  # format data to original data
  tag_data_new %>% 
    dplyr::rename_all(tolower) %>% 
    tidytable::mutate(rel_year = year(as.Date(rel_time_local, format = '%m/%d/%Y')),
                      rel_month = month(as.Date(rel_time_local, format = '%m/%d/%Y')),
                      rec_year = year(as.Date(rec_time_utc, format = '%m/%d/%Y')),
                      rec_month = month(as.Date(rec_time_utc, format = '%m/%d/%Y')),
                      dif_len = rel_length - rec_size,
                      growth = dif_len / dal,
                      program = 'PSAT',
                      index = .I) %>% 
    tidytable::select(index,
                      reldate = rel_time_local,
                      lat_rel = rel_latdd,
                      long_rel = rel_longdd,
                      recdate = rec_time_utc,
                      lat_rec = rec_lat,
                      long_rec = rec_lng,
                      tag_number = tag_num,
                      program,
                      rel_len = rel_length,
                      rec_len = rec_size,
                      dal,
                      rec_year,
                      rel_year,
                      rec_month,
                      rel_month,
                      dif_len,
                      growth) -> t
  
  # get release mgmnt areas
  t %>% 
    tidytable::select(index, lat_rel, long_rel) %>% 
    st_as_sf(., 
             coords = c("long_rel", "lat_rel"), 
             crs = 4326) %>% 
    st_join(., 
            nmfs_areas, 
            join = st_intersects) %>% 
    dplyr::rename_all(tolower) %>% 
    tidytable::mutate(rel_area = as.numeric(rep_area)) %>% 
    tidytable::select(index, rel_area) -> rel_area
  
  # get recovery mgmnt areas
  t %>% 
    tidytable::select(index, lat_rec, long_rec) %>% 
    tidytable::drop_na() %>% 
    st_as_sf(., 
             coords = c("long_rec", "lat_rec"), 
             crs = 4326) %>% 
    st_join(., 
            nmfs_areas, 
            join = st_intersects) %>% 
    dplyr::rename_all(tolower) %>% 
    tidytable::mutate(rec_area = as.numeric(rep_area)) %>% 
    tidytable::select(index, rec_area) -> rec_area
  
  # put it all together
  sat_tags <- t %>% 
    tidytable::left_join(rel_area) %>% 
    tidytable::left_join(rec_area) %>% 
    tidytable::mutate(rec_region = case_when(rec_area < 600 ~ 'BS',
                                             rec_area == 610 ~ 'WGOA',
                                             rec_area %in% c(620, 630) ~ 'CGOA',
                                             rec_area > 630 ~ 'EGOA',
                                             is.na(rec_area) ~ NA),
                      rel_region = case_when(rel_area < 600 ~ 'BS',
                                             rel_area == 610 ~ 'WGOA',
                                             rel_area %in% c(620, 630) ~ 'CGOA',
                                             rel_area > 630 ~ 'EGOA',
                                             is.na(rel_area) ~ NA),
                      rel_season = case_when(rel_month <= 3 ~ 'A',
                                             rel_month > 3 ~ 'B'),
                      rec_season = case_when(rec_month <= 3 ~ 'A',
                                             rec_month > 3 ~ 'B'),
                      dis_km = geosphere::distHaversine(cbind(long_rel, lat_rel), cbind(long_rec, lat_rec)) / 1000)
  
  # test if rec/rel areas are correct
  # Note: there are some differences, but it's all between areas within the ebs
  # combine tag data and search for tag duplicates and remove duplicates
  all_tags <- tag_data %>% 
    dplyr::rename_all(tolower) %>% 
    tidytable::bind_rows(sat_tags) %>% 
    tidytable::filter(n() == 1, .by = c(reldate, recdate, lat_rel, long_rel, lat_rec, long_rec, rel_len)) %>% 
    tidytable::bind_rows(tag_data %>% 
                           dplyr::rename_all(tolower) %>% 
                           tidytable::bind_rows(sat_tags) %>% 
                           tidytable::filter(n() > 1, .by = c(reldate, recdate, lat_rel, long_rel, lat_rec, long_rec, rel_len)) %>% 
                           tidytable::arrange(-tag_number) %>% 
                           tidytable::distinct(c(reldate, recdate, lat_rel, long_rel, lat_rec, long_rec, rel_len), .keep_all = TRUE))
  
  # write combined tag data
  vroom::vroom_write(all_tags, here::here(new_year, 'rsch', 'wgoa', 'data', 'all_tags.csv'), delim = ',')
}


## get movement rates ----
move_df <- all_tags %>% 
  tidytable::drop_na(rec_area) %>%
  tidytable::filter(rel_area %in% c(610, 620),
                    dal > 30,
                    rel_year > 1990) %>% 
  tidytable::mutate(rec_area = factor(case_when(rec_area < 600 ~ 517,
                                                rec_area >= 630 ~ 630,
                                                .default = rec_area)),
                    rel_area = factor(rel_area)) %>% 
  tidytable::count(rel_area, rec_area) %>% 
  tidytable::mutate(sum = sum(n),
                    prop = n / sum,
                    season = 'ALL',
                    .by = rel_area) %>% 
  tidytable::bind_rows(all_tags %>% 
                         tidytable::drop_na(rec_area) %>%
                         tidytable::filter(rel_area %in% c(610, 620),
                                           rel_season == 'A',
                                           dal > 30,
                                           rel_year > 1990) %>% 
                         tidytable::mutate(rec_area = factor(case_when(rec_area < 600 ~ 517,
                                                                       rec_area >= 630 ~ 630,
                                                                       .default = rec_area)),
                                           rel_area = factor(rel_area)) %>% 
                         tidytable::count(rel_area, rec_area) %>% 
                         tidytable::mutate(sum = sum(n),
                                           prop = n / sum,
                                           season = 'A',
                                           .by = rel_area)) %>% 
  tidytable::bind_rows(all_tags %>% 
                         tidytable::drop_na(rec_area) %>%
                         tidytable::filter(rel_area %in% c(610, 620),
                                           rel_season == 'B',
                                           dal > 30,
                                           rel_year > 1990) %>% 
                         tidytable::mutate(rec_area = factor(case_when(rec_area < 600 ~ 517,
                                                                       rec_area >= 630 ~ 630,
                                                                       .default = rec_area)),
                                           rel_area = factor(rel_area)) %>% 
                         tidytable::count(rel_area, rec_area) %>% 
                         tidytable::mutate(sum = sum(n),
                                           prop = n / sum,
                                           season = 'B',
                                           .by = rel_area))


# filter to goa releases and get centroids of the geometry for plotting
tag_points <- st_as_sf(all_tags %>% filter(rel_area %in% c(610, 620)), coords = c("long_rel", "lat_rel"), crs = 4326)
nmfs_areas_plot <- nmfs_areas %>% 
  dplyr::filter(REP_AREA %in% c(517, 610, 620, 630))
centroids <- nmfs_areas_plot %>%
  dplyr::group_by(REP_AREA) %>%
  dplyr::summarise(geometry = st_centroid(geometry)) %>%
  dplyr::ungroup()

# get 610 and 620 survey strata for plotting
goa_west_curr <- akgfmaps::get_base_layers(select.region = "goa", set.crs = "EPSG:3338")$survey.strata %>%
  dplyr::mutate(long = strata_with_coords_curr$X) %>%
  dplyr::filter(long < -156)

## helper functions ----
# function to shift text/arrows for plotting
shift_coords <- function(move_df){
  move_df %>%
    tidytable::left_join(centroids, by = c("rel_area" = "REP_AREA")) %>%
    tidytable::rename(from_geometry = geometry) %>%
    tidytable::left_join(centroids, by = c("rec_area" = "REP_AREA")) %>%
    tidytable::rename(to_geometry = geometry) %>%
    tidytable::mutate(from_x = st_coordinates(from_geometry)[, 1], # get from centroids
                      from_y = st_coordinates(from_geometry)[, 2],
                      to_x = st_coordinates(to_geometry)[, 1], # get to centroids
                      to_y = st_coordinates(to_geometry)[, 2]) %>% 
    tidytable::select(rel_area, rec_area, prop, from_x, from_y, to_x, to_y) %>% 
    tidytable::mutate(to_x = case_when(rel_area == '610' & rec_area == '610' ~ -165.3,
                                       rel_area == '610' & rec_area == '620' ~ -157,
                                       rel_area == '610' & rec_area == '630' ~ -153,
                                       rel_area == '620' & rec_area == '610' ~ -161,
                                       rel_area == '620' & rec_area == '630' ~ -153,
                                       .default = to_x),
                      to_y = case_when(rel_area == '610' & rec_area == '620' ~ 53.5,
                                       rel_area == '610' & rec_area == '630' ~ 54,
                                       rel_area == '620' & rec_area == '610' ~ 53.5,
                                       rel_area == '620' & rec_area == '630' ~ 56,
                                       rel_area == '620' & rec_area == '620' ~ 54.3,
                                       .default = to_y))
}

# create table of release/recovery locations
plot_table <- function(all_tags, rel_seas){
  tag_tbl_all <- all_tags %>% 
    tidytable::drop_na(rec_area) %>%
    tidytable::filter(dal > 30,
                      rel_year > 1990,
                      !(rel_region %in% c('BS', 'EGOA')),
                      rel_season %in% rel_seas) %>% 
    tidytable::mutate(rel_reg = case_when(rel_area == 610 ~ '610',
                                          rel_area == 620 ~ '620',
                                          rel_area == 630 ~ '630'),
                      rec_reg = case_when(rec_area < 600 ~ 'BS',
                                          rec_area == 610 ~ '610',
                                          rec_area == 620 ~ '620',
                                          rec_area == 630 ~ '630'),
                      rel_reg = factor(rel_reg, levels = c('BS', '610', '620', '630')),
                      rec_reg = factor(rec_reg, levels = c('BS', '610', '620', '630'))) %>% 
    tidytable::count(rel_reg, rec_reg) %>% 
    tidytable::pivot_wider(names_from = rec_reg, values_from = n, values_fill = 0) %>% 
    column_to_rownames(var = "rel_reg")
  table_all <- gridExtra::tableGrob(tag_tbl_all, 
                                    theme = ttheme_default(base_size = 12))
  title_rec <- grid::textGrob("Recovery region", gp = gpar(fontsize = 12, fontface = "bold"), vjust = 3.5, hjust = 0.3)
  title_rel <- grid::textGrob("Release region", gp = gpar(fontsize = 12, fontface = "italic"), rot = 90, vjust = -1)
  table_with_title <- gridExtra::arrangeGrob(table_all, 
                                             top = title_rec, 
                                             left = title_rel,
                                             padding = unit(0, "line"))
  table_with_title
}

## plot for all seasons ----
move_df_with_coords <- shift_coords(move_df %>% filter(season == 'ALL'))
table_with_title <- plot_table(all_tags, rel_seas = c('A', 'B'))

ggplot() +
  geom_sf(data = nmfs_areas %>% filter(REP_AREA %in% c(610, 620)), alpha = 0, color = "black", size = 0.1) +
  geom_sf(data = goa_west_curr, alpha = 0, color = "black", size = 0.1) +
  geom_sf(data = tag_points, aes(geometry = geometry, color = as.factor(program))) +
  geom_sf(data = goa_layers_hist$akland, fill = "#2c3e50", color = "white") +
  coord_sf(xlim = c(-170, -153),
           ylim = c(52, 58),
           crs = "+proj=longlat +datum=WGS84") +
  labs(title = "610 & 620 tag releases (Year-round)",
       x = "Longitude",
       y = "Latitude",
       color = 'Program') +
  geom_curve(data = move_df_with_coords %>% filter(rel_area != rec_area),
             aes(x = from_x, y = from_y, 
                 xend = to_x - 0.07 * (to_x - from_x), 
                 yend = to_y - 0.07 * (to_y - from_y)),
             linewidth = 1, curvature = 0.4, lineend = 'round', color = "grey",
             arrow = arrow(length = unit(0.3, "cm"), type = "closed", ends = "last")) +
  geom_text(data = move_df_with_coords %>% filter(rel_area != rec_area),
            aes(x = to_x, y = to_y, label = paste(round(prop * 100, 1), "%", sep = '')), alpha = 1, size = 6.5) +
  geom_text(data = move_df_with_coords %>% filter(rel_area == rec_area),
            aes(x = to_x, y = to_y, label = paste(round(prop * 100, 1), "%", sep = '')), alpha = 1, size = 6.5) +
  theme(panel.background = element_rect(fill = "aliceblue")) +
  annotation_custom(table_with_title, 
                    xmin = -167, xmax = -163, 
                    ymin = 56,  ymax = 58.5)

ggsave(here::here(new_year, 'rsch', 'wgoa', 'figs', 'Tag_Release_Map_All.png'), width = 10, height = 10, units = "in", dpi = 300)



## plot for A season ----
move_df_with_coords <- shift_coords(move_df %>% filter(season == 'A'))
table_with_title <- plot_table(all_tags, rel_seas = c('A'))

ggplot() +
  geom_sf(data = nmfs_areas %>% filter(REP_AREA %in% c(610, 620)), alpha = 0, color = "black", size = 0.1) +
  geom_sf(data = goa_west_curr, alpha = 0, color = "black", size = 0.1) +
  # geom_sf_text(data = goa_west_curr, aes(label = STRATUM)) +
  geom_sf(data = tag_points %>% dplyr::filter(rel_season == 'A'), aes(geometry = geometry, color = as.factor(program))) +
  geom_sf(data = goa_layers_hist$akland, fill = "#2c3e50", color = "white") +
  coord_sf(xlim = c(-170, -153),  # Longitude (Negative for West)
           ylim = c(52, 58),      # Latitude
           crs = "+proj=longlat +datum=WGS84") + # View in Lat/Lon for easier verification
  labs(title = "610 & 620 tag releases (A season)",
       x = "Longitude",
       y = "Latitude",
       color = 'Program') +
  geom_curve(data = move_df_with_coords %>% filter(rel_area != rec_area),
             aes(x = from_x, y = from_y, 
                 xend = to_x - 0.07 * (to_x - from_x), 
                 yend = to_y - 0.07 * (to_y - from_y)),
             linewidth = 1, curvature = 0.4, lineend = 'round', color = "grey",
             arrow = arrow(length = unit(0.3, "cm"), type = "closed", ends = "last")) + # movement arrows (from != to)
  geom_text(data = move_df_with_coords %>% filter(rel_area != rec_area),
            aes(x = to_x, y = to_y, label = paste(round(prop * 100, 1), "%", sep = '')), alpha = 1, size = 6.5) + # from != to
  geom_text(data = move_df_with_coords %>% filter(rel_area == rec_area),
            aes(x = to_x, y = to_y, label = paste(round(prop * 100, 1), "%", sep = '')), alpha = 1, size = 6.5) +
  theme(panel.background = element_rect(fill = "aliceblue")) +
  annotation_custom(table_with_title, 
                    xmin = -167, xmax = -163, 
                    ymin = 56,  ymax = 58.5)

ggsave(here::here(new_year, 'rsch', 'wgoa', 'figs', 'Tag_Release_Map_A.png'), width = 10, height = 10, units = "in", dpi = 300)


## plot for B season ----
move_df_with_coords <- shift_coords(move_df %>% filter(season == 'B'))
table_with_title <- plot_table(all_tags, rel_seas = c('B'))

ggplot() +
  geom_sf(data = nmfs_areas %>% filter(REP_AREA %in% c(610, 620)), alpha = 0, color = "black", size = 0.1) +
  geom_sf(data = goa_west_curr, alpha = 0, color = "black", size = 0.1) +
  # geom_sf_text(data = goa_west_curr, aes(label = STRATUM)) +
  geom_sf(data = tag_points, aes(geometry = geometry, color = as.factor(program))) +
  geom_sf(data = goa_layers_hist$akland, fill = "#2c3e50", color = "white") +
  coord_sf(xlim = c(-170, -153),  # Longitude (Negative for West)
           ylim = c(52, 58),      # Latitude
           crs = "+proj=longlat +datum=WGS84") + # View in Lat/Lon for easier verification
  labs(title = "610 & 620 tag releases (B season)",
       x = "Longitude",
       y = "Latitude",
       color = 'Program') +
  geom_curve(data = move_df_with_coords %>% filter(rel_area != rec_area),
             aes(x = from_x, y = from_y, 
                 xend = to_x - 0.07 * (to_x - from_x), 
                 yend = to_y - 0.07 * (to_y - from_y)),
             linewidth = 1, curvature = 0.4, lineend = 'round', color = "grey",
             arrow = arrow(length = unit(0.3, "cm"), type = "closed", ends = "last")) + # movement arrows (from != to)
  geom_text(data = move_df_with_coords %>% filter(rel_area != rec_area),
            aes(x = to_x, y = to_y, label = paste(round(prop * 100, 1), "%", sep = '')), alpha = 1, size = 6.5) + # from != to
  geom_text(data = move_df_with_coords %>% filter(rel_area == rec_area),
            aes(x = to_x, y = to_y, label = paste(round(prop * 100, 1), "%", sep = '')), alpha = 1, size = 6.5) +
  theme(panel.background = element_rect(fill = "aliceblue")) +
  annotation_custom(table_with_title, 
                    xmin = -167, xmax = -163, 
                    ymin = 56,  ymax = 58.5)

ggsave(here::here(new_year, 'rsch', 'wgoa', 'figs', 'Tag_Release_Map_B.png'), width = 10, height = 10, units = "in", dpi = 300)


# plot fishery locations ----

obs_fish <- vroom::vroom(here::here(new_year, 'rsch', 'wgoa', 'data', 'fish_obs_catch.csv'))

obs_catch <- obs_fish %>% 
  tidytable::drop_na(lat, lon) %>% 
  tidytable::mutate(month = month(hday),
                    season = case_when(month <= 3 ~ 'A',
                                       month > 3 ~ 'B')) %>% 
  st_as_sf(., coords = c("lon", "lat"), crs = 4326)


ggplot() +
  geom_sf(data = nmfs_areas %>% filter(REP_AREA %in% c(610, 620)), alpha = 0, color = "black", size = 0.1) +
  geom_sf(data = goa_west_curr, alpha = 0, color = "black", size = 0.1) +
  geom_sf(data = obs_catch %>% filter(season == 'A'), aes(geometry = geometry, color = as.factor(gear)), size = 0.5) +
  geom_sf(data = goa_layers_hist$akland, fill = "#2c3e50", color = "white") +
  coord_sf(xlim = c(-170, -153),  # Longitude (Negative for West)
           ylim = c(52, 58),      # Latitude
           crs = "+proj=longlat +datum=WGS84") + # View in Lat/Lon for easier verification
  labs(title = "Fishery events (A season, 77%) - CONFIDENTIAL",
       x = "Longitude",
       y = "Latitude",
       color = 'Gear') +
  theme(panel.background = element_rect(fill = "aliceblue"))

ggsave(here::here(new_year, 'rsch', 'wgoa', 'figs', 'Fishery_A.png'), width = 10, height = 10, units = "in", dpi = 300)


ggplot() +
  geom_sf(data = nmfs_areas %>% filter(REP_AREA %in% c(610, 620)), alpha = 0, color = "black", size = 0.1) +
  geom_sf(data = goa_west_curr, alpha = 0, color = "black", size = 0.1) +
  geom_sf(data = obs_catch %>% filter(season == 'B'), aes(geometry = geometry, color = as.factor(gear)), size = 0.5) +
  geom_sf(data = goa_layers_hist$akland, fill = "#2c3e50", color = "white") +
  coord_sf(xlim = c(-170, -153),  # Longitude (Negative for West)
           ylim = c(52, 58),      # Latitude
           crs = "+proj=longlat +datum=WGS84") + # View in Lat/Lon for easier verification
  labs(title = "Fishery events (B season, 23%) - CONFIDENTIAL",
       x = "Longitude",
       y = "Latitude",
       color = 'Gear') +
  theme(panel.background = element_rect(fill = "aliceblue"))

ggsave(here::here(new_year, 'rsch', 'wgoa', 'figs', 'Fishery_B.png'), width = 10, height = 10, units = "in", dpi = 300)





