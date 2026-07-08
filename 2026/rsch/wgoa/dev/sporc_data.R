# script to get data together for sporc model
library(tidyverse)
library(afscdata)
library(surveyISS)
library(r4ss)
library(fs)

# params
new_year = 2026

# get external data ----

# read in ss3 files
bs_dat_ss3 <- r4ss::SS_readdat(here::here(new_year, 'rsch', 'wgoa', 'data', 'ss3_files', 'bs', 'BSPcod24_OCT_5cm_NB.dat'))
bs_rep_ss3 <- r4ss::SS_output(dir = here::here(new_year, 'rsch', 'wgoa', 'data', 'ss3_files', 'bs'),
                              verbose = FALSE,
                              printstats = FALSE)
goa_dat_ss3 <- r4ss::SS_readdat(here::here(new_year, 'rsch', 'wgoa', 'data', 'ss3_files', 'goa', 'GOAPcod2025Dec08.dat'))
goa_rep_ss3 <- r4ss::SS_output(dir = here::here(new_year, 'rsch', 'wgoa', 'data', 'ss3_files', 'goa'),
                               verbose = FALSE,
                               printstats = FALSE)




# fishery data (from steve)
fshry_dat <- readRDS(here::here(new_year, 'rsch', 'wgoa', 'data', 'PETE_DATA.rds'))

# fishery specimen data
if('fish_specimen_goa.csv' %in% list.files(here::here(new_year, 'rsch', 'wgoa', 'data', 'fshry_data'))){
  goa_spec_fsry <- vroom::vroom(new_year, 'rsch', 'wgoa', 'data', 'fshry_data', 'fish_specimen_goa.csv')
  bs_spec_fsry <- vroom::vroom(new_year, 'rsch', 'wgoa', 'data', 'fshry_data', 'fish_specimen_bs.csv')
} else{
  db = 'akfin'
  conn = afscdata::connect(db) 
  
  goa_spec_fsry <- dplyr::tbl(conn, dplyr::sql('norpac.debriefed_age')) %>% 
    dplyr::filter(SPECIES == 202) %>% 
    dplyr::rename_all(tolower) %>% 
    dplyr::select(year,
                  area = nmfs_area,
                  haul_offload_date,
                  lon = londd_end,
                  lat = latdd_end,
                  gear,
                  haul_join,
                  port_join,
                  sex = sex,
                  length,
                  weight,
                  age) %>% 
    dplyr::filter(area >= 600,
                  area <= 699,
                  area != 670) %>% 
    dplyr::mutate(haul_join = paste0('H', haul_join),
                  port_join = paste0('P', port_join),
                  season = case_when(month(haul_offload_date) <= 3 ~ 'A',
                                     month(haul_offload_date) > 3 ~ 'B')) %>% 
    dplyr::select(-haul_offload_date) %>% 
    dplyr::collect() %>% 
    dplyr::filter(!is.na(age)) %>% 
    dplyr::mutate(gear = dplyr::case_when(gear %in% c(1, 2, 3, 4) ~ 'trawl',
                                          gear == 6 ~ 'pot',
                                          gear %in% c(5, 7, 9, 10, 11, 68, 8) ~ 'longline')) %>% 
    dplyr::filter(!is.na(gear))
  
  bs_spec_fsry <- dplyr::tbl(conn, dplyr::sql('norpac.debriefed_age')) %>% 
    dplyr::filter(SPECIES == 202) %>% 
    dplyr::rename_all(tolower) %>% 
    dplyr::select(year,
                  area = nmfs_area,
                  haul_offload_date,
                  lon = londd_end,
                  lat = latdd_end,
                  gear,
                  haul_join,
                  port_join,
                  sex = sex,
                  length,
                  weight,
                  age) %>% 
    dplyr::filter(area < 600) %>% 
    dplyr::mutate(haul_join = paste0('H', haul_join),
                  port_join = paste0('P', port_join),
                  season = case_when(month(haul_offload_date) <= 3 ~ 'A',
                                     month(haul_offload_date) > 3 ~ 'B')) %>% 
    dplyr::select(-haul_offload_date) %>% 
    dplyr::collect() %>% 
    dplyr::filter(!is.na(age)) %>% 
    dplyr::mutate(gear = dplyr::case_when(gear %in% c(1, 2, 3, 4) ~ 'trawl',
                                          gear == 6 ~ 'pot',
                                          gear %in% c(5, 7, 9, 10, 11, 68, 8) ~ 'longline')) %>% 
    dplyr::filter(!is.na(gear))
  
  vroom::vroom_write(goa_spec_fsry, here::here(new_year, 'rsch', 'wgoa', 'data', 'fshry_data', 'fish_specimen_goa.csv'), delim = ",")
  vroom::vroom_write(bs_spec_fsry, here::here(new_year, 'rsch', 'wgoa', 'data', 'fshry_data', 'fish_specimen_bs.csv'), delim = ",")
  
}




# survey data
if('srvy_data' %in% list.files(here::here(new_year, 'rsch', 'wgoa', 'data'))){
  goa_srv_dat <- list(lfreq = vroom::vroom(new_year, 'rsch', 'wgoa', 'data', 'srvy_data', 'goa', 'lfreq.csv'),
                      specimen = vroom::vroom(new_year, 'rsch', 'wgoa', 'data', 'srvy_data', 'goa', 'specimen.csv'),
                      cpue = vroom::vroom(new_year, 'rsch', 'wgoa', 'data', 'srvy_data', 'goa', 'cpue.csv'),
                      strata = vroom::vroom(new_year, 'rsch', 'wgoa', 'data', 'srvy_data', 'goa', 'strata.csv'))
  bs_srv_dat <- list(lfreq = vroom::vroom(new_year, 'rsch', 'wgoa', 'data', 'srvy_data', 'nebs', 'lfreq.csv'),
                     specimen = vroom::vroom(new_year, 'rsch', 'wgoa', 'data', 'srvy_data', 'nebs', 'specimen.csv'),
                     cpue = vroom::vroom(new_year, 'rsch', 'wgoa', 'data', 'srvy_data', 'nebs', 'cpue.csv'),
                     strata = vroom::vroom(new_year, 'rsch', 'wgoa', 'data', 'srvy_data', 'nebs', 'strata.csv'))
  goa_spec_srvy <- vroom::vroom(new_year, 'rsch', 'wgoa', 'data', 'srvy_data', 'goa', 'specimen_wt.csv')
  bs_spec_srvy <- vroom::vroom(new_year, 'rsch', 'wgoa', 'data', 'srvy_data', 'nebs', 'specimen_wt.csv')
  
} else{
  # surveyISS data
  goa_srv_dat <- surveyISS::query_data(survey = 47,
                                       region = 'goa',
                                       species = 21720)
  bs_srv_dat <- surveyISS::query_data(survey = c(98, 143),
                                      region = 'nebs',
                                      species = 21720)
  old_folder <- here::here('data')
  new_folder <- here::here(new_year, 'rsch', 'wgoa', 'data', 'srvy_data')
  fs::dir_copy(path = old_folder, new_path = new_folder)
  fs::dir_delete(path = old_folder)
  
  # specimen data with weight
  db = 'akfin'
  conn = afscdata::connect(db)  
  
  goa_spec_srvy <- dplyr::tbl(conn, dplyr::sql('gap_products.akfin_haul')) %>% 
    dplyr::inner_join(dplyr::tbl(conn, dplyr::sql('gap_products.akfin_cruise')),
                      by = c('CRUISEJOIN')) %>% 
    dplyr::inner_join(dplyr::tbl(conn, dplyr::sql('gap_products.akfin_specimen')),
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
                  sex,
                  length_mm,
                  weight_g,
                  age) %>% 
    dplyr::filter(survey_definition_id %in% 47,
                  species_code %in% 21720) %>% 
    dplyr::mutate(lat_mid = (latitude_dd_start + latitude_dd_end) / 2,
                  long_mid = (longitude_dd_start + longitude_dd_end) / 2) %>% 
    dplyr::select(year, 
                  survey = survey_definition_id,
                  species_code,
                  stratum,
                  hauljoin,
                  sex,
                  length = length_mm,
                  weight = weight_g,
                  age,
                  lat_mid,
                  long_mid) %>% 
    dplyr::collect() %>% 
    tidytable::filter(age > 0) %>% 
    tidytable::mutate(length = length / 10,
                      weight = weight / 1000)
  
  bs_spec_srvy <- dplyr::tbl(conn, dplyr::sql('gap_products.akfin_haul')) %>% 
    dplyr::inner_join(dplyr::tbl(conn, dplyr::sql('gap_products.akfin_cruise')),
                      by = c('CRUISEJOIN')) %>% 
    dplyr::inner_join(dplyr::tbl(conn, dplyr::sql('gap_products.akfin_specimen')),
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
                  sex,
                  length_mm,
                  weight_g,
                  age) %>% 
    dplyr::filter(survey_definition_id %in% c(98, 143),
                  species_code %in% 21720) %>% 
    dplyr::mutate(lat_mid = (latitude_dd_start + latitude_dd_end) / 2,
                  long_mid = (longitude_dd_start + longitude_dd_end) / 2) %>% 
    dplyr::select(year, 
                  survey = survey_definition_id,
                  species_code,
                  stratum,
                  hauljoin,
                  sex,
                  length = length_mm,
                  weight = weight_g,
                  age,
                  lat_mid,
                  long_mid) %>% 
    dplyr::collect() %>% 
    tidytable::filter(age > 0) %>% 
    tidytable::mutate(length = length / 10,
                      weight = weight / 1000)
  
  vroom::vroom_write(bs_spec_srvy, here::here(new_year, 'rsch', 'wgoa', 'data', 'srvy_data', 'nebs', 'specimen_wt.csv'), delim = ",")
  vroom::vroom_write(goa_spec_srvy, here::here(new_year, 'rsch', 'wgoa', 'data', 'srvy_data', 'goa', 'specimen_wt.csv'), delim = ",")

}

# specimen data








# model dim ----
cod_data <- list(years = min(bs_dat_ss3$styr, goa_dat_ss3$styr):max(bs_dat_ss3$endyr, goa_dat_ss3$endyr),
                 ages = goa_dat_ss3$agebin_vector,
                 lens = bs_dat_ss3$lbin_vector,
                 n_pop = 3,
                 n_regions = 3,
                 n_sexes = 1,
                 n_fish_fleets = 1,
                 n_srv_fleets = 1,
                 n_seas = 2,
                 seasdur = c(0.25, 0.75),
                 spawn_seas = 1)


# bio processes ----

# weight-at-age: empirical waa for combined fishery-survey
waa <- goa_spec_srvy %>% 
  tidytable::left_join(goa_srv_dat$strata %>% 
                         tidytable::mutate(region = case_when(area_id == 805 ~ 'wgoa',
                                                              area_id < 805 ~ 'cgoa')) %>% 
                         tidytable::select(stratum, region)) %>% 
  tidytable::bind_rows(bs_spec_srvy %>% 
                         tidytable::mutate(region = 'bs')) %>% 
  tidytable::select(year, age, weight, region) %>% 
  tidytable::bind_rows(goa_spec_fsry %>% 
                         tidytable::bind_rows(bs_spec_fsry) %>% 
                         tidytable::mutate(region = case_when(area == 610 ~ 'wgoa',
                                                              area >= 620 ~ 'cgoa',
                                                              area < 600 ~ 'bs')) %>% 
                         tidytable::select(year, age, weight, region)) %>% 
  tidytable::drop_na() %>% 
  tidytable::mutate(age = case_when(age >= 10 ~ 10,
                                    .default = age)) %>% 
  tidytable::summarise(waa = mean(weight), .by = c(region, age))

# maturity-at-age
mataa <- bs_rep_ss3$endgrowth %>% 
  dplyr::rename_all(tolower) %>% 
  tidytable::select(age = real_age,
                    len_mat) %>% 
  tidytable::filter(age %in% 1:10) %>% 
  tidytable::mutate(region = 'bs') %>% 
  tidytable::select(region, age, mataa = len_mat) %>% 
  tidytable::bind_rows(goa_rep_ss3$endgrowth %>% 
                         dplyr::rename_all(tolower) %>% 
                         tidytable::select(age = real_age,
                                           len_mat) %>% 
                         tidytable::filter(age %in% 1:10) %>% 
                         tidytable::mutate(region = 'cgoa') %>% 
                         tidytable::select(region, age, mataa = len_mat)) %>% 
  tidytable::bind_rows(goa_rep_ss3$endgrowth %>% 
                         dplyr::rename_all(tolower) %>% 
                         tidytable::select(age = real_age,
                                           len_mat) %>% 
                         tidytable::filter(age %in% 1:10) %>% 
                         tidytable::mutate(region = 'wgoa') %>% 
                         tidytable::select(region, age, mataa = len_mat))


# ageing error


bs_rep_ss3$age_error_mean %>% 
  tidytable::pivot_longer(cols = c(type1, type2)) %>% 
  tidytable::mutate(mean_age = value - 0.5) %>% 
  tidytable::select(-value) %>% 
  tidytable::filter(age %in% 1:10) %>% 
  tidytable::left_join(bs_rep_ss3$age_error_sd %>% 
                         tidytable::pivot_longer(cols = c(type1, type2)) %>% 
                         tidytable::rename(sd_ae = value) %>% 
                         tidytable::filter(age %in% 1:10)) %>% 
  tidytable::left_join(expand.grid(age = 1:10, mod_age = 1:10)) %>% 
  tidytable::mutate(ae = stats::dnorm(mod_age, mean = mean_age, sd = sd_ae),
                    .by = age) %>% 
  tidytable::pivot_wider(names_from = mod_age, values_from = ae)

goa_rep_ss3$agedbase
goa_rep_ss3$age_error_sd













# get connected
db = 'akfin'
conn = afscdata::connect(db)  

new_year = 2025
twl_srvy = 47
srv_sp = 20510
area = 'goa'

# survey data ----

## trawl survey index data ----
twl_indx <- dplyr::tbl(conn, dplyr::sql('gap_products.akfin_biomass')) %>% 
  dplyr::rename_all(tolower) %>% 
  dplyr::select(year,
                survey_definition_id,
                area_id,
                species_code,
                biomass_mt,
                biomass_var,
                population_count,
                population_var) %>% 
  dplyr::filter(year <= new_year,
                survey_definition_id == twl_srvy,
                species_code == srv_sp,
                area_id %in% c(803, 804, 805, 99903)) %>% 
  dplyr::select(year,
                survey = survey_definition_id, 
                strata = area_id, 
                species_code,
                biom = biomass_mt, 
                biom_var = biomass_var,
                num = population_count,
                num_var = population_var) %>%  
  dplyr::mutate(area = case_when(strata == 803 ~ 'central',
                                 strata == 804 ~ 'eastern',
                                 strata == 805 ~ 'western',
                                 strata == 99903 ~ 'goa')) %>% 
  dplyr::collect()


lls_rpn <- afscdata::q_lls_rpn_length(year = new_year,
                                      species = srv_sp,
                                      by = 'depth',
                                      area = area,
                                      db = conn,
                                      save = FALSE)

dat <- twl_indx %>% 
  tidytable::filter(strata == 99903) %>% 
  tidytable::select(year, num) %>% 
  tidytable::mutate(index = "Trawl survey numbers") %>% 
  tidytable::bind_rows(lls_rpn %>% 
                         tidytable::select(year, length, rpn) %>% 
                         tidytable::filter(length <= 50) %>% 
                         tidytable::summarise(num = sum(rpn), .by = year) %>% 
                         tidytable::mutate(index = "LL survey numbers (<50 cm)")) %>% 
  tidytable::mutate(stnzd_index = num / mean(num), .by = index)

ggplot(dat, aes(x = year, y = stnzd_index, color = index)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  xlab("Year") +
  ylab("Standardized Index") +
  theme(legend.position = "top",
        legend.title = element_blank())
  
