# exploratory data analysis of pcod tagging data

library(tidyverse)

# prep tag data ----

## steve data (this data came from steve in email on 1-16-25) ----
sb_data <- vroom::vroom(here::here('2025', 'rsch', 'spatial', 'raw_data_SB', 'ALL_TAG_DATA2025.csv'), delim = ',') %>% 
  dplyr::rename_all(tolower) %>% 
  tidytable::mutate(recdt = as.Date(recdt, format = '%m/%d/%Y'),
                    reldt = as.Date(reldt, format = '%m/%d/%Y'),
                    lib_time = as.numeric(recdt - reldt)) %>% 
  # filter out duplicated tags (these ones have same release location but different recovery locations)
  tidytable::filter(n() == 1, .by = c(tag_number, reldt)) %>% 
  tidytable::select(tag_number, rel_month, rel_year, reldt, rel_region, rel_lat = lat_rel, rel_lon = long_rel,
                    rec_month, rec_year, recdt, rec_region, rec_lat = lat_rec, rec_lon = long_rec, lib_time) 
## PACT data ----
# this data comes from PACTbase (date pulled is in the file name)
rel_sat <- vroom::vroom(here::here('2025', 'rsch', 'spatial', 'raw_data_PACT', 'rel_sat.csv'), delim = ',')
rec_sat <- vroom::vroom(here::here('2025', 'rsch', 'spatial', 'raw_data_PACT', 'rec_sat.csv'), delim = ',')

rel_sat %>% 
  dplyr::rename_all(tolower) %>% 
  tidytable::mutate(reldt = as.Date(rel_time_local, format = '%m/%d/%Y'),
                    rel_month = lubridate::month(reldt),
                    rel_year = lubridate::year(reldt)) %>% 
  tidytable::select(tag_number = tag_num, rel_month, rel_year, reldt, rel_region, rel_lat = rel_latdd, rel_lon = rel_longdd) %>% 
  tidytable::left_join(rec_sat %>% 
                         dplyr::rename_all(tolower) %>% 
                         tidytable::mutate(recdt = as.Date(rec_time_utc, format = '%m/%d/%Y'),
                                           rec_month = lubridate::month(recdt),
                                           rec_year = lubridate::year(recdt)) %>% 
                         tidytable::select(tag_number = tag_num, rec_month, rec_year, recdt, rec_region, rec_lat, rec_lon = rec_lng)) %>% 
  tidytable::drop_na() %>% 
  tidytable::mutate(lib_time = as.numeric(recdt - reldt)) -> pact_data
  
# remove the duplicated sat tags and keep ones from PACT data
sb_data %>%
  tidytable::bind_rows(pact_data) %>% 
  tidytable::filter(n() > 1, .by = c(tag_number, reldt)) %>% 
  pull(tag_number) %>% 
  unique() -> dup_tags

## combine tag data ----
sb_data %>% 
  tidytable::filter(!(tag_number %in% dup_tags)) %>%
  tidytable::bind_rows(pact_data) %>% 
  # filter time @ liberty > 30 days
  tidytable::filter(lib_time > 30) -> tag_data

# add subregion info when release location is 'GOA'
tag_data %>% 
  tidytable::filter(rel_region != 'GOA') %>% 
  tidytable::bind_rows(tag_data %>% 
                         tidytable::filter(rel_region == 'GOA') %>% 
                         tidytable::mutate(rel_region = case_when(rel_lon >= -170 & rel_lon <= -159 ~ 'WGOA',
                                                                  rel_lon > -159 & rel_lon <= -147 ~ 'CGOA',
                                                                  .default = 'EGOA'))) -> .tag_data
.tag_data %>% 
  tidytable::filter(rec_region != 'GOA') %>% 
  tidytable::bind_rows(.tag_data %>% 
                         tidytable::filter(rec_region == 'GOA') %>% 
                         tidytable::mutate(rec_region = case_when(rec_lon >= -170 & rec_lon <= -159 ~ 'WGOA',
                                                                  rec_lon > -159 & rec_lon <= -147 ~ 'CGOA',
                                                                  .default = 'EGOA'))) -> tag_data
# movement cases ----

# overall
tag_data %>% 
  count(rel_region, rec_region) %>% 
  tidytable::mutate(tot_n = sum(n), .by = rel_region) %>% 
  tidytable::mutate(prop_move = n / tot_n) %>% 
  tidytable::select(-tot_n) -> all

# released during spawning season
tag_data %>% 
  tidytable::filter(rel_month <= 3) %>% 
  count(rel_region, rec_region) %>% 
  tidytable::mutate(tot_n = sum(n), .by = rel_region) %>% 
  tidytable::mutate(prop_move = n / tot_n) %>% 
  tidytable::select(-tot_n) -> spawn

# released outside spawning season
tag_data %>% 
  tidytable::filter(rel_month > 3) %>%
  count(rel_region, rec_region) %>% 
  tidytable::mutate(tot_n = sum(n), .by = rel_region) %>% 
  tidytable::mutate(prop_move = n / tot_n) %>% 
  tidytable::select(-tot_n) -> not_spawn

output <- list(all = all,
               spawn = spawn,
               not_spawn = not_spawn)

saveRDS(output, here::here('2025', 'rsch', 'spatial', 'tag_out.RDS'))


