library(tidyverse)
library(afscdata)

# get connected
db = 'akfin'
conn = afscdata::connect(db)  

species = 21720
yrs = 1990
survey = 47
country = 'United States'


# get gap strata area sizes ----

# strata with area sizes
dplyr::tbl(conn, dplyr::sql('gap_products.akfin_area')) %>% 
  dplyr::rename_all(tolower) %>% 
  dplyr::filter(survey_definition_id %in% survey,
                area_type == 'STRATUM') %>% 
  dplyr::collect() -> st_area
vroom::vroom_write(st_area, here::here('st_area.csv'), delim = ',')

# subregion level with description (e.g., wgoa, etc)
dplyr::tbl(conn, dplyr::sql('gap_products.akfin_area')) %>% 
  dplyr::rename_all(tolower) %>% 
  dplyr::filter(survey_definition_id %in% survey,
                area_type == 'REGULATORY AREA') %>% 
  dplyr::collect() -> subreg

# strata within subregions
dplyr::tbl(conn, dplyr::sql('gap_products.akfin_stratum_groups')) %>% 
  dplyr::rename_all(tolower) %>% 
  dplyr::filter(survey_definition_id %in% survey) %>% 
  dplyr::collect() %>%
  tidytable::filter(area_id %in% 803:805, .by = c(stratum)) -> st_subreg

# join all to get strata with area sizes and subregion ids
st_area %>% 
  tidytable::rename(stratum = area_id) %>% 
  tidytable::left_join(st_subreg) -> strata

vroom::vroom_write(strata, here::here('strata.csv'), delim = ',')
vroom::vroom_write(subreg, here::here('subreg.csv'), delim = ',')
vroom::vroom_write(st_subreg, here::here('st_subreg.csv'), delim = ',')

strata %>% 
  tidytable::filter(design_year == 1984) %>% 
  tidytable::mutate(subreg =  case_when(area_id == 803 ~ 'CGOA',
                                        area_id == 805 ~ 'WGOA',
                                        area_id == 804 ~ 'EGOA')) %>% 
  tidytable::summarise(area = sum(area_km2), .by = c(subreg, depth_min_m, depth_max_m)) %>% 
  vroom::vroom_write(., here::here('2025', 'rsch', 'll_srv', 'data', 'gap_strata.csv'), delim = ',')

# get ll survey raw catch data ----

dplyr::tbl(conn, dplyr::sql('afsc.lls_catch_summary_with_nulls_mv')) %>% 
  dplyr::rename_all(tolower) %>% 
  dplyr::filter(species_code == species,
                year > yrs,
                stratum > 1,
                country == country,
                !(council_sablefish_management_area %in% c('Bering Sea', 'Aleutians'))) %>% 
  dplyr::select(year, 
                species_code,
                station_number, 
                vessel_number,
                hachi,
                lat = start_latitude,
                lon = end_longitude,
                catch_freq, 
                ineffective,
                rpn_filter,
                subarea = council_sablefish_management_area,
                stratum,
                stratum_description) %>% 
  dplyr::collect() %>% 
  vroom::vroom_write(., here::here('2025', 'rsch', 'll_srv', 'data', 'lls_pcod_catch.csv'), delim = ',')


