library(tidyverse)

# get connected
db = 'afsc'
conn = afscdata::connect(db)  

species = 30060
yrs = 1990
survey = 47

# get gap_products cpue
dplyr::tbl(conn, dplyr::sql('gap_products.akfin_haul')) %>% 
  dplyr::inner_join(dplyr::tbl(conn, dplyr::sql('gap_products.akfin_cruise')),
                    by = c('CRUISEJOIN')) %>% 
  dplyr::inner_join(dplyr::tbl(conn, dplyr::sql('gap_products.akfin_cpue')),
                    by = c('HAULJOIN')) %>% 
  dplyr::rename_all(tolower) %>% 
  dplyr::select(year,
                survey_definition_id,
                species_code,
                stratum,
                hauljoin,
                latitude_dd_start,
                latitude_dd_end,
                longitude_dd_start,
                longitude_dd_end,
                cpue_nokm2,
                cpue_kgkm2) %>% 
  dplyr::filter(survey_definition_id %in% survey,
                species_code %in% species,
                year >= yrs) %>% 
  dplyr::mutate(lat_mid = (latitude_dd_start + latitude_dd_end) / 2,
                long_mid = (longitude_dd_start + longitude_dd_end) / 2) %>% 
  dplyr::select(year,
                species_code,
                stratum,
                hauljoin,
                survey = survey_definition_id,
                numcpue = cpue_nokm2,
                wtcpue = cpue_kgkm2,
                lat_mid,
                long_mid) %>% 
  dplyr::collect() -> cpue

vroom::vroom_write(cpue, here::here('cpue_pop.csv'), delim = ',')





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
  vroom::vroom_write(., here::here('2025', 'rsch', 'll_srv', 'gap_strata.csv'), delim = ',')



