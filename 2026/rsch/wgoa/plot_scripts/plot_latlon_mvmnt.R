# load libraries
library(sf)
library(tidyverse)
library(akgfmaps)
library(cowplot)
library(scales)

# get year folder
new_year <- as.numeric(format(Sys.Date(), format = "%Y"))

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

## table of wgoa tag releases by month ----
all_tags %>% 
  tidytable::filter(rel_area == 610) %>%
  tidytable::summarize(n_month = n(), .by = c(rel_month, rec_region)) %>% 
  tidytable::drop_na() %>% 
  tidytable::pivot_wider(names_from = rec_region, values_from = n_month, values_fill = 0) %>% 
  vroom::vroom_write(., here::here(new_year, 'rsch', 'wgoa', 'data', 'wgoa_tag_releases_by_month.csv'), delim = ",")

## table of ebs tag releases by month ----
all_tags %>% 
  tidytable::filter(rel_area < 600,
  rec_region != 'AI') %>%
  tidytable::summarize(n_month = n(), .by = c(rel_month, rec_region)) %>% 
  tidytable::drop_na() %>% 
  tidytable::pivot_wider(names_from = rec_region, values_from = n_month, values_fill = 0) %>% 
  vroom::vroom_write(., here::here(new_year, 'rsch', 'wgoa', 'data', 'ebs_tag_releases_by_month.csv'), delim = ",")

## map stuff ----
points_sf <- st_as_sf(all_tags %>% 
  tidytable::filter(rel_area %in% c(610, 620, 630) & rel_season == 'A' & rec_season == 'B') %>% 
  tidytable::bind_rows(all_tags %>%
    tidytable::filter(rel_region == 'BS' & rel_season == 'B' & rec_season == 'A' & rec_region != 'AI')),
   coords = c("long_rel", "lat_rel"), crs = 4326)
goa_layers <- akgfmaps::get_base_layers(select.region = "goa", set.crs = "EPSG:3338")

## helper functions to resample tags for confidence intervals ----
resample_tags <- function(all_tags){
  all_tags %>% 
    tidytable::slice_sample(prop = 1, replace = TRUE) -> resampled_tags
  resampled_tags %>% 
      tidytable::filter(rel_area %in% c(610, 620, 630) & rel_season == 'A' & rec_season == 'B') %>% 
      tidytable::mutate(rel_long_cat = floor(abs(long_rel))) %>% 
      tidytable::select(rel_long_cat, rel_region, rec_region) %>% 
      tidytable::summarise(to_region = n(), .by = c(rel_long_cat, rec_region)) %>% 
      tidytable::drop_na() %>% 
      tidytable::pivot_wider(names_from = c(rec_region), values_from = to_region, values_fill = 0) %>% 
      tidytable::pivot_longer(cols = starts_with(c('BS', 'WGOA', 'CGOA', 'EGOA')), names_to = 'rec_region', values_to = 'to_region') %>%
      tidytable::mutate(prop_to_region = to_region / sum(to_region), .by = rel_long_cat) %>%
      tidytable::select(-to_region)}

resample_tags_bs <- function(all_tags){
  all_tags %>% 
    tidytable::slice_sample(prop = 1, replace = TRUE) -> resampled_tags
  resampled_tags %>% 
    tidytable::filter(rel_region == 'BS' & rel_season == 'B' & rec_season == 'A' & rec_region != 'AI') %>% 
      tidytable::mutate(rel_lat_cat = floor(abs(lat_rel))) %>% 
      tidytable::select(rel_lat_cat, rel_region, rec_region) %>% 
      tidytable::drop_na() %>%
      tidytable::mutate(rec_region = case_when(rec_region == 'WGOA' ~ 'WGOA',
                                           rec_region == 'CGOA' ~ 'WGOA',
                                           .default = 'BS')) %>% 
       tidytable::summarise(to_region = n(), .by = c(rel_lat_cat, rec_region)) %>%
      tidytable::pivot_wider(names_from = c(rec_region), values_from = to_region, values_fill = 0) %>% 
      tidytable::pivot_longer(cols = starts_with(c('BS', 'WGOA')), names_to = 'rec_region', values_to = 'to_region') %>%
      tidytable::mutate(prop_to_region = to_region / sum(to_region), .by = rel_lat_cat) %>%
      tidytable::select(-to_region)}

## wgoa movement by release longitude ----
all_tags %>% 
  tidytable::filter(rel_area %in% c(610, 620, 630) & rel_season == 'A' & rec_season == 'B') %>% 
    tidytable::mutate(rel_long_cat = floor(abs(long_rel))) %>% 
    tidytable::select(rel_long_cat, rel_region, rec_region) %>% 
    tidytable::summarise(to_region = n(), .by = c(rel_long_cat, rec_region)) %>% 
    tidytable::drop_na() %>% 
    tidytable::pivot_wider(names_from = c(rec_region), values_from = to_region, values_fill = 0) %>% 
    tidytable::pivot_longer(cols = starts_with(c('BS', 'WGOA', 'CGOA', 'EGOA')), names_to = 'rec_region', values_to = 'to_region') %>%
    tidytable::mutate(prop_to_region = to_region / sum(to_region), .by = rel_long_cat) %>%
    tidytable::select(-to_region) %>% 
  tidytable::left_join(purrr::map(1:250, ~ resample_tags(all_tags)) %>% 
    tidytable::bind_rows(.id = 'iteration') %>% 
    tidytable::summarise(uci_prop_to_region = quantile(prop_to_region, 0.975),
                         lci_prop_to_region = quantile(prop_to_region, 0.025),
                         .by = c(rel_long_cat, rec_region))) -> long_cat_mvmnt
 
goa_plot <- ggplot(long_cat_mvmnt %>% tidytable::filter(rel_long_cat > 152 & rec_region == 'BS'), aes(x = rel_long_cat, y = prop_to_region, col = rec_region)) +
  geom_point() +
  geom_errorbar(aes(ymin = lci_prop_to_region, ymax = uci_prop_to_region), width = 0.2) +
  geom_smooth(method = 'glm', se = TRUE) +
  scale_x_reverse() +
  labs(title = "Released in GOA (A season)",
       x = "Release longitude",
       y = "Proportion recovered in BS",
      color = "Recovery region") +
  coord_cartesian(ylim = c(0, 1), xlim = c(156, 164)) +
  theme_minimal() +
  theme(legend.position = "none", panel.background = element_blank(), plot.background = element_blank(), panel.grid = element_blank())

## bs movement by release latitude ----
all_tags %>% 
  tidytable::filter(rel_region == 'BS' & rel_season == 'B' & rec_season == 'A' & rec_region != 'AI') %>% 
    tidytable::mutate(rel_lat_cat = floor(abs(lat_rel))) %>% 
    tidytable::select(rel_lat_cat, rel_region, rec_region) %>% 
    tidytable::drop_na() %>%
    tidytable::mutate(rec_region = case_when(rec_region == 'WGOA' ~ 'WGOA',
                                         rec_region == 'CGOA' ~ 'WGOA',
                                         .default = 'BS')) %>% 
     tidytable::summarise(to_region = n(), .by = c(rel_lat_cat, rec_region)) %>%
    tidytable::pivot_wider(names_from = c(rec_region), values_from = to_region, values_fill = 0) %>% 
    tidytable::pivot_longer(cols = starts_with(c('BS', 'WGOA')), names_to = 'rec_region', values_to = 'to_region') %>%
    tidytable::mutate(prop_to_region = to_region / sum(to_region), .by = rel_lat_cat) %>%
    tidytable::select(-to_region) %>% 
  tidytable::left_join(purrr::map(1:250, ~ resample_tags_bs(all_tags)) %>% 
    tidytable::bind_rows(.id = 'iteration') %>% 
    tidytable::summarise(uci_prop_to_region = quantile(prop_to_region, 0.975),
                         lci_prop_to_region = quantile(prop_to_region, 0.025),
                         .by = c(rel_lat_cat, rec_region))) -> lat_cat_mvmnt
 
bs_plot <- ggplot(lat_cat_mvmnt %>% tidytable::filter(rec_region == 'WGOA'), aes(x = rel_lat_cat, y = prop_to_region, col = rec_region)) +
  geom_point() +
  geom_errorbar(aes(ymin = lci_prop_to_region, ymax = uci_prop_to_region), width = 0.2) +
  geom_smooth(method = 'glm', se = TRUE) +
  labs(title = "Released in BS (B season)",
       x = "Release latitude",
       y = "Proportion recovered in WGOA",
      color = "Recovery region") +
  coord_flip(ylim = c(0, 0.7)) +
  theme_minimal() +
  scale_x_continuous(breaks = breaks_width(2)) +
  theme(legend.position = "none", panel.background = element_blank(), plot.background = element_blank(), panel.grid = element_blank())

## base map ----
map <- ggplot() +
  geom_sf(data = goa_layers$akland, fill = "#2c3e50", color = "white") +
  geom_sf(data = points_sf, size = 0.5, aes(geometry = geometry), color = "grey") +
  coord_sf(xlim = c(-179, -150),
           ylim = c(47, 65),
           crs = "+proj=longlat +datum=WGS84")

## combined plot ----
ggdraw(map) +
  draw_plot(
    goa_plot, 
    x = 0.47,   # Horizontal position (0 to 1)
    y = 0.05,   # Vertical position (0 to 1)
    width = 0.4, 
    height = 0.35) +
  draw_plot(
    bs_plot, 
    x = 0.15,   # Horizontal position (0 to 1)
    y = 0.42,   # Vertical position (0 to 1)
    width = 0.4, 
    height = 0.35)

ggsave(here::here(new_year, 'rsch', 'wgoa', 'figs', 'latlong_mvmnt_plot.png'), width = 12, height = 10, units = "in", dpi = 300)
