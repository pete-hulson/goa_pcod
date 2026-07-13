# script to get data together for sporc model
library(tidyverse)
library(afscdata)
library(surveyISS)
library(r4ss)
library(fs)

# params
new_year = 2026

# get external data ----

## read in ss3 files ----
bs_dat_ss3 <- r4ss::SS_readdat(here::here(new_year, 'rsch', 'wgoa', 'data', 'ss3_files', 'bs', 'BSPcod24_OCT_5cm_NB.dat'))
bs_rep_ss3 <- r4ss::SS_output(dir = here::here(new_year, 'rsch', 'wgoa', 'data', 'ss3_files', 'bs'),
                              verbose = FALSE,
                              printstats = FALSE)
goa_dat_ss3 <- r4ss::SS_readdat(here::here(new_year, 'rsch', 'wgoa', 'data', 'ss3_files', 'goa', 'GOAPcod2025Dec08.dat'))
goa_rep_ss3 <- r4ss::SS_output(dir = here::here(new_year, 'rsch', 'wgoa', 'data', 'ss3_files', 'goa'),
                               verbose = FALSE,
                               printstats = FALSE)

## fishery data (from steve) ----
fshry_dat <- readRDS(here::here(new_year, 'rsch', 'wgoa', 'data', 'fshry_data', 'PETE_DATA.rds'))

## fishery specimen data ----
if('fish_specimen_goa.csv' %in% list.files(here::here(new_year, 'rsch', 'wgoa', 'data', 'fshry_data'))){
  goa_spec_fsry <- vroom::vroom(here::here(new_year, 'rsch', 'wgoa', 'data', 'fshry_data', 'fish_specimen_goa.csv'))
  bs_spec_fsry <- vroom::vroom(here::here(new_year, 'rsch', 'wgoa', 'data', 'fshry_data', 'fish_specimen_bs.csv'))
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

## survey data ----
if('srvy_data' %in% list.files(here::here(new_year, 'rsch', 'wgoa', 'data'))){
  goa_srv_dat <- list(lfreq = vroom::vroom(here::here(new_year, 'rsch', 'wgoa', 'data', 'srvy_data', 'goa', 'lfreq.csv')),
                      specimen = vroom::vroom(here::here(new_year, 'rsch', 'wgoa', 'data', 'srvy_data', 'goa', 'specimen.csv')),
                      cpue = vroom::vroom(here::here(new_year, 'rsch', 'wgoa', 'data', 'srvy_data', 'goa', 'cpue.csv')),
                      strata = vroom::vroom(here::here(new_year, 'rsch', 'wgoa', 'data', 'srvy_data', 'goa', 'strata.csv')))
  bs_srv_dat <- list(lfreq = vroom::vroom(here::here(new_year, 'rsch', 'wgoa', 'data', 'srvy_data', 'nebs', 'lfreq.csv')),
                     specimen = vroom::vroom(here::here(new_year, 'rsch', 'wgoa', 'data', 'srvy_data', 'nebs', 'specimen.csv')),
                     cpue = vroom::vroom(here::here(new_year, 'rsch', 'wgoa', 'data', 'srvy_data', 'nebs', 'cpue.csv')),
                     strata = vroom::vroom(here::here(new_year, 'rsch', 'wgoa', 'data', 'srvy_data', 'nebs', 'strata.csv')))
  goa_spec_srvy <- vroom::vroom(here::here(new_year, 'rsch', 'wgoa', 'data', 'srvy_data', 'goa', 'specimen_wt.csv'))
  bs_spec_srvy <- vroom::vroom(here::here(new_year, 'rsch', 'wgoa', 'data', 'srvy_data', 'nebs', 'specimen_wt.csv'))
  
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

## survey comps ----
if('srvy_index' %in% list.files(here::here(new_year, 'rsch', 'wgoa', 'data'))){
  goa_acomp <- vroom::vroom(here::here(new_year, 'rsch', 'wgoa', 'data', 'srvy_index', 'goa', 'cod_resampled_age_w_c_egoa.csv'))
  goa_lcomp <- vroom::vroom(here::here(new_year, 'rsch', 'wgoa', 'data', 'srvy_index', 'goa', 'cod_resampled_length_w_c_egoa.csv'))
} else{
  
  surveyISS::srvy_iss_goa_w_c_e(iters = 1,
                                lfreq_data = goa_srv_dat$lfreq,
                                specimen_data = goa_srv_dat$specimen,
                                cpue_data = goa_srv_dat$cpue,
                                strata_data = goa_srv_dat$strata,
                                yrs = 1990,
                                bin = 1,
                                boot_hauls = FALSE,
                                boot_lengths = FALSE,
                                boot_ages = FALSE,
                                al_var = FALSE,
                                al_var_ann = FALSE,
                                age_err = FALSE,
                                len_samples = NULL,
                                age_samples = NULL,
                                plus_len = NULL,
                                plus_age = NULL,
                                by_strata = FALSE,
                                global = FALSE,
                                region = "goa",
                                save_interm = TRUE,
                                save_stats = FALSE,
                                save = 'cod')
  
  old_folder <- here::here('output')
  new_folder <- here::here(new_year, 'rsch', 'wgoa', 'data', 'srvy_index')
  fs::dir_copy(path = old_folder, new_path = new_folder)
  fs::dir_delete(path = old_folder)
}

## goa survey index ----
if('index.csv' %in% list.files(here::here(new_year, 'rsch', 'wgoa', 'data', 'srvy_index', 'goa'))){
  goa_twl_indx <- vroom::vroom(here::here(new_year, 'rsch', 'wgoa', 'data', 'srvy_data', 'goa', 'index.csv'))
} else{
  # survey index data
  db = 'akfin'
  conn = afscdata::connect(db)  
  
  ## trawl survey index data ----
  goa_twl_indx <- dplyr::tbl(conn, dplyr::sql('gap_products.akfin_biomass')) %>% 
    dplyr::rename_all(tolower) %>% 
    dplyr::select(year,
                  survey_definition_id,
                  area_id,
                  species_code,
                  population_count,
                  population_var) %>% 
    dplyr::filter(year <= new_year,
                  survey_definition_id == 47,
                  species_code == 21720,
                  area_id %in% c(803, 804, 805)) %>% 
    dplyr::select(year,
                  survey = survey_definition_id, 
                  strata = area_id, 
                  species_code,
                  num = population_count,
                  num_var = population_var) %>%  
    dplyr::mutate(region = case_when(strata == 803 ~ 'cgoa',
                                     strata == 804 ~ 'cgoa',
                                     strata == 805 ~ 'wgoa')) %>% 
    dplyr::summarise(num = sum(num, na.rm = TRUE),
                     num_var = sum(num_var, na.rm = TRUE),
                     .by = c(year, region)) %>% 
    dplyr::collect()
  
  vroom::vroom_write(goa_twl_indx, here::here(new_year, 'rsch', 'wgoa', 'data', 'srvy_data', 'goa', 'index.csv'), delim = ",")
}


# model dim ----
cod_data <- list(years = 1991:max(bs_dat_ss3$endyr, goa_dat_ss3$endyr),
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

## weight-at-age ----
# empirical waa for combined fishery-survey
# waa <- goa_spec_srvy %>% 
#   tidytable::left_join(goa_srv_dat$strata %>% 
#                          tidytable::mutate(region = case_when(area_id == 805 ~ 'wgoa',
#                                                               area_id < 805 ~ 'cgoa')) %>% 
#                          tidytable::select(stratum, region)) %>% 
#   tidytable::bind_rows(bs_spec_srvy %>% 
#                          tidytable::mutate(region = 'bs')) %>% 
#   tidytable::select(year, age, weight, region) %>% 
#   tidytable::bind_rows(goa_spec_fsry %>% 
#                          tidytable::bind_rows(bs_spec_fsry) %>% 
#                          tidytable::mutate(region = case_when(area == 610 ~ 'wgoa',
#                                                               area >= 620 ~ 'cgoa',
#                                                               area < 600 ~ 'bs')) %>% 
#                          tidytable::select(year, age, weight, region)) %>% 
#   tidytable::drop_na() %>% 
#   tidytable::mutate(age = case_when(age >= 10 ~ 10,
#                                     .default = age)) %>% 
#   tidytable::summarise(waa = mean(weight), .by = c(region, age))


# using ss3 seasonal difference cludged with empirical
bs_seas_diff <- r4ss::SS_readwtatage(here::here(new_year, 'rsch', 'wgoa', 'data', 'ss3_files', 'bs', 'wtatage.ss_new')) %>% 
  tidytable::filter(year == 1977,
                    fleet %in% c(0, -1)) %>% 
  tidytable::select(-year, -seas, -sex, -bio_pattern, -birthseas, -`0`, -as.character(11:20)) %>% 
  tidytable::pivot_longer(cols = as.character(1:10)) %>% 
  tidytable::mutate(age = as.numeric(name),
                    seas = case_when(fleet == 0 ~ 'A',
                                     fleet == -1 ~ 'B')) %>% 
  tidytable::select(seas, age, waa = value) %>% 
  tidytable::pivot_wider(names_from = seas, values_from = waa) %>% 
  tidytable::mutate(diff = B - A) %>% 
  tidytable::select(age, diff)

bs_waa_b <- bs_spec_srvy %>% 
  tidytable::select(year, age, weight) %>% 
  tidytable::bind_rows(bs_spec_fsry %>% 
                         tidytable::bind_rows(bs_spec_fsry) %>% 
                         tidytable::filter(season != 'A') %>% 
                         tidytable::select(year, age, weight)) %>% 
  tidytable::drop_na() %>% 
  tidytable::mutate(age = case_when(age >= 10 ~ 10,
                                    .default = age)) %>% 
  tidytable::summarise(waa = mean(weight), .by = c(age)) %>% 
  tidytable::mutate(seas = 'B',
                    region = 'bs') %>% 
  tidytable::select(region, seas, age, waa)

bs_waa_a <- bs_waa_b %>% 
  tidytable::left_join(bs_seas_diff) %>% 
  tidytable::mutate(waa_a = waa - diff,
                    seas = 'A') %>% 
  tidytable::select(region, seas, age, waa = waa_a)

goa_seas_diff <- r4ss::SS_readwtatage(here::here(new_year, 'rsch', 'wgoa', 'data', 'ss3_files', 'goa', 'wtatage.ss_new')) %>% 
  tidytable::filter(year == 1977,
                    fleet %in% c(0, -1)) %>% 
  tidytable::select(-year, -seas, -sex, -bio_pattern, -birthseas, -`0`) %>% 
  tidytable::pivot_longer(cols = as.character(1:10)) %>% 
  tidytable::mutate(age = as.numeric(name),
                    seas = case_when(fleet == 0 ~ 'A',
                                     fleet == -1 ~ 'B')) %>% 
  tidytable::select(seas, age, waa = value) %>% 
  tidytable::pivot_wider(names_from = seas, values_from = waa) %>% 
  tidytable::mutate(diff = B - A) %>% 
  tidytable::select(age, diff)

goa_waa_b <- goa_spec_srvy %>% 
  tidytable::left_join(goa_srv_dat$strata %>% 
                         tidytable::mutate(region = case_when(area_id == 805 ~ 'wgoa',
                                                              area_id < 805 ~ 'cgoa')) %>% 
                         tidytable::select(stratum, region)) %>% 
  tidytable::select(year, age, weight, region) %>% 
  tidytable::bind_rows(goa_spec_fsry %>% 
                         tidytable::bind_rows(bs_spec_fsry) %>% 
                         tidytable::mutate(region = case_when(area == 610 ~ 'wgoa',
                                                              area >= 620 ~ 'cgoa',
                                                              area < 600 ~ 'bs')) %>% 
                         tidytable::filter(region != 'bs',
                                           season != 'A') %>% 
                         tidytable::select(year, age, weight, region)) %>% 
  tidytable::drop_na() %>% 
  tidytable::mutate(age = case_when(age >= 10 ~ 10,
                                    .default = age)) %>% 
  tidytable::summarise(waa = mean(weight), .by = c(region, age)) %>% 
  tidytable::mutate(seas = 'B') %>% 
  tidytable::select(region, seas, age, waa)

goa_waa_a <- goa_waa_b %>% 
  tidytable::left_join(goa_seas_diff) %>% 
  tidytable::mutate(waa_a = waa - diff,
                    seas = 'A') %>% 
  tidytable::select(region, seas, age, waa = waa_a)

waa <- bs_waa_a %>% 
  tidytable::bind_rows(bs_waa_b) %>% 
  tidytable::bind_rows(goa_waa_a) %>% 
  tidytable::bind_rows(goa_waa_b) %>% 
  tidytable::arrange(region, seas)

## maturity-at-age ----
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


## ageing error ----
bs_ageingerror1 <- tidytable::bind_cols(age_c = 12:0,
                                        as.data.frame(bs_rep_ss3$AAK[1,,2]),
                                        as.data.frame(bs_rep_ss3$AAK[1,,3]),
                                        as.data.frame(bs_rep_ss3$AAK[1,,4]),
                                        as.data.frame(bs_rep_ss3$AAK[1,,5]),
                                        as.data.frame(bs_rep_ss3$AAK[1,,6]),
                                        as.data.frame(bs_rep_ss3$AAK[1,,7]),
                                        as.data.frame(bs_rep_ss3$AAK[1,,8]),
                                        as.data.frame(bs_rep_ss3$AAK[1,,9]),
                                        as.data.frame(bs_rep_ss3$AAK[1,,10]),
                                        as.data.frame(bs_rep_ss3$AAK[1,,11])) %>% 
  tidytable::arrange(age_c) %>% 
  pivot_longer(cols = 2:length(colnames(.))) %>% 
  tidytable::select(-name) %>% 
  tidytable::bind_cols(., expand.grid(age_r = 1:10, age_c = 0:12) %>% tidytable::select(-age_c)) %>% 
  tidytable::mutate(age_c = case_when(age_c == 0 ~ 1,
                                      age_c >= 10 ~ 10,
                                      .default = age_c)) %>% 
  tidytable::summarise(value = sum(value), .by = c(age_c, age_r)) %>% 
  tidytable::pivot_wider(names_from = age_c, values_from = value) %>% 
  select(-age_r)

bs_ageingerror2 <- tidytable::bind_cols(age_c = 12:0,
                                        as.data.frame(bs_rep_ss3$AAK[2,,2]),
                                        as.data.frame(bs_rep_ss3$AAK[2,,3]),
                                        as.data.frame(bs_rep_ss3$AAK[2,,4]),
                                        as.data.frame(bs_rep_ss3$AAK[2,,5]),
                                        as.data.frame(bs_rep_ss3$AAK[2,,6]),
                                        as.data.frame(bs_rep_ss3$AAK[2,,7]),
                                        as.data.frame(bs_rep_ss3$AAK[2,,8]),
                                        as.data.frame(bs_rep_ss3$AAK[2,,9]),
                                        as.data.frame(bs_rep_ss3$AAK[2,,10]),
                                        as.data.frame(bs_rep_ss3$AAK[2,,11])) %>% 
  tidytable::arrange(age_c) %>% 
  pivot_longer(cols = 2:length(colnames(.))) %>% 
  tidytable::select(-name) %>% 
  tidytable::bind_cols(., expand.grid(age_r = 1:10, age_c = 0:12) %>% tidytable::select(-age_c)) %>% 
  tidytable::mutate(age_c = case_when(age_c == 0 ~ 1,
                                      age_c >= 10 ~ 10,
                                      .default = age_c)) %>% 
  tidytable::summarise(value = sum(value), .by = c(age_c, age_r)) %>% 
  tidytable::pivot_wider(names_from = age_c, values_from = value) %>% 
  select(-age_r)

goa_ageingerror1 <- tidytable::bind_cols(age_c = 10:1,
                                         as.data.frame(goa_rep_ss3$AAK[1,,2]),
                                         as.data.frame(goa_rep_ss3$AAK[1,,3]),
                                         as.data.frame(goa_rep_ss3$AAK[1,,4]),
                                         as.data.frame(goa_rep_ss3$AAK[1,,5]),
                                         as.data.frame(goa_rep_ss3$AAK[1,,6]),
                                         as.data.frame(goa_rep_ss3$AAK[1,,7]),
                                         as.data.frame(goa_rep_ss3$AAK[1,,8]),
                                         as.data.frame(goa_rep_ss3$AAK[1,,9]),
                                         as.data.frame(goa_rep_ss3$AAK[1,,10]),
                                         as.data.frame(goa_rep_ss3$AAK[1,,11])) %>% 
  tidytable::arrange(age_c) %>% 
  pivot_longer(cols = 2:length(colnames(.))) %>% 
  tidytable::select(-name) %>% 
  tidytable::bind_cols(., expand.grid(age_r = 1:10, age_c = 1:10) %>% tidytable::select(-age_c)) %>% 
  tidytable::pivot_wider(names_from = age_c, values_from = value) %>% 
  select(-age_r)

goa_ageingerror2 <- tidytable::bind_cols(age_c = 10:1,
                                         as.data.frame(goa_rep_ss3$AAK[2,,2]),
                                         as.data.frame(goa_rep_ss3$AAK[2,,3]),
                                         as.data.frame(goa_rep_ss3$AAK[2,,4]),
                                         as.data.frame(goa_rep_ss3$AAK[2,,5]),
                                         as.data.frame(goa_rep_ss3$AAK[2,,6]),
                                         as.data.frame(goa_rep_ss3$AAK[2,,7]),
                                         as.data.frame(goa_rep_ss3$AAK[2,,8]),
                                         as.data.frame(goa_rep_ss3$AAK[2,,9]),
                                         as.data.frame(goa_rep_ss3$AAK[2,,10]),
                                         as.data.frame(goa_rep_ss3$AAK[2,,11])) %>% 
  tidytable::arrange(age_c) %>% 
  pivot_longer(cols = 2:length(colnames(.))) %>% 
  tidytable::select(-name) %>% 
  tidytable::bind_cols(., expand.grid(age_r = 1:10, age_c = 1:10) %>% tidytable::select(-age_c)) %>% 
  tidytable::pivot_wider(names_from = age_c, values_from = value) %>% 
  select(-age_r)

# bs block 1
bs_ageingerror1 %>% 
  tidytable::mutate(region = 'bs',
                    time_block = 1) %>% 
  tidytable::bind_cols(age = 1:10) %>% 
  tidytable::select(region, time_block, age, as.character(1:10)) %>% 
  tidytable::bind_rows(# bs block 2
    bs_ageingerror2 %>% 
      tidytable::mutate(region = 'bs',
                        time_block = 2) %>% 
      tidytable::bind_cols(age = 1:10) %>% 
      tidytable::select(region, time_block, age, as.character(1:10))) %>% 
  tidytable::bind_rows(# goa block 1
    goa_ageingerror1 %>% 
      tidytable::mutate(region = 'wgoa',
                        time_block = 1) %>% 
      tidytable::bind_cols(age = 1:10) %>% 
      tidytable::select(region, time_block, age, as.character(1:10))) %>% 
  tidytable::bind_rows(# goa block 2
    goa_ageingerror2 %>% 
      tidytable::mutate(region = 'wgoa',
                        time_block = 2) %>% 
      tidytable::bind_cols(age = 1:10) %>% 
      tidytable::select(region, time_block, age, as.character(1:10))) %>% 
  tidytable::bind_rows(# goa block 1
    goa_ageingerror1 %>% 
      tidytable::mutate(region = 'cgoa',
                        time_block = 1) %>% 
      tidytable::bind_cols(age = 1:10) %>% 
      tidytable::select(region, time_block, age, as.character(1:10))) %>% 
  tidytable::bind_rows(# goa block 2
    goa_ageingerror2 %>% 
      tidytable::mutate(region = 'cgoa',
                        time_block = 2) %>% 
      tidytable::bind_cols(age = 1:10) %>% 
      tidytable::select(region, time_block, age, as.character(1:10))) -> ageingerror



## size-age transition ----
# for bs, use ss3 results
# for goa, use specimen data with ss3 sd and difference in seasonal growth

# bs
sizeagetrans_bs_a <- as.data.frame(bs_rep_ss3$ALK[,,1]) %>% 
  tidytable::mutate(length = as.numeric(rownames(.))) %>% 
  tidytable::arrange(length) %>% 
  pivot_longer(cols = 1:(length(colnames(.)) - 1)) %>% 
  tidytable::mutate(age = as.numeric(name)) %>% 
  tidytable::filter(age != 0,
                    age <= 10) %>% 
  tidytable::mutate(length = case_when(length <= 4.5 ~ ceiling(length / 4.5) * 4.5,
                                       length > 4.5 ~ ceiling(length / 5) * 5 - 0.5)) %>% 
  tidytable::summarise(value = sum(value), .by = c(age, length)) %>% 
  tidytable::pivot_wider(names_from = age, values_from = value) %>% 
  tidytable::mutate(region = 'bs',
                    seas = 'A') %>% 
  tidytable::select(region, seas, length, as.character(1:10))

sizeagetrans_bs_b <- as.data.frame(bs_rep_ss3$ALK[,,2]) %>% 
  tidytable::mutate(length = as.numeric(rownames(.))) %>% 
  tidytable::arrange(length) %>% 
  pivot_longer(cols = 1:(length(colnames(.)) - 1)) %>% 
  tidytable::mutate(age = as.numeric(name)) %>% 
  tidytable::filter(age != 0,
                    age <= 10) %>% 
  tidytable::mutate(length = case_when(length <= 4.5 ~ ceiling(length / 4.5) * 4.5,
                                       length > 4.5 ~ ceiling(length / 5) * 5 - 0.5)) %>% 
  tidytable::summarise(value = sum(value), .by = c(age, length)) %>% 
  tidytable::pivot_wider(names_from = age, values_from = value) %>% 
  tidytable::mutate(region = 'bs',
                    seas = 'B') %>% 
  tidytable::select(region, seas, length, as.character(1:10))

# goa
# wgoa
sizeagetrans_wgoa_a <- goa_acomp %>% 
  tidytable::filter(region == 'wgoa',
                    sex == 0) %>% 
  tidytable::mutate(age = case_when(age <= 1 ~ 1,
                                    age >= 10 ~ 10,
                                    .default = age)) %>% 
  tidytable::summarise(mean_len = mean(mean_length),
                       .by = c(region, age)) %>% 
  tidytable::left_join(as.data.frame(cbind(age = 1:10,
                                           sd_len = c(4.00851,4.88844,5.61639,6.2186,6.71679,7.12893,7.46988,7.75194,7.98528,9.10287)))) %>% 
  tidytable::left_join(as.data.frame(cbind(age = 1:10,
                                           mean_len_a = c(11.9252,25.039,37.8937,48.5281,57.3255,64.6035,70.6243,75.6052,79.7257,88.2707),
                                           mean_len_b = c(17.6378,31.7708,43.4628,53.1352,61.1369,67.7565,73.2327,77.763,81.5108,89.2829))) %>% 
                         tidytable::mutate(diff = mean_len_b - mean_len_a) %>% 
                         tidytable::select(age, diff)) %>% 
  tidytable::mutate(mean_len = mean_len - diff,
                    seas = 'A') %>% 
  tidytable::select(region, seas, age, mean_len, sd_len) %>% 
  tidytable::left_join(expand.grid(age = 1:10, length = bs_dat_ss3$lbin_vector)) %>% 
  tidytable::mutate(szage = case_when(length == 4.5 ~ pnorm(length, mean_len, sd_len),
                                      length > 4.5 ~ pnorm(length, mean_len, sd_len) - pnorm(length - 5, mean_len, sd_len))) %>% 
  tidytable::select(-mean_len, -sd_len) %>% 
  tidytable::pivot_wider(names_from = age, values_from = szage)

sizeagetrans_wgoa_b <- goa_acomp %>% 
  tidytable::filter(region == 'wgoa',
                    sex == 0) %>% 
  tidytable::mutate(age = case_when(age <= 1 ~ 1,
                                    age >= 10 ~ 10,
                                    .default = age)) %>% 
  tidytable::summarise(mean_len = mean(mean_length),
                       .by = c(region, age)) %>% 
  tidytable::left_join(as.data.frame(cbind(age = 1:10,
                                           sd_len = c(4.00851,4.88844,5.61639,6.2186,6.71679,7.12893,7.46988,7.75194,7.98528,9.10287)))) %>% 
  tidytable::mutate(seas = 'B') %>% 
  tidytable::select(region, seas, age, mean_len, sd_len) %>% 
  tidytable::left_join(expand.grid(age = 1:10, length = bs_dat_ss3$lbin_vector)) %>% 
  tidytable::mutate(szage = case_when(length == 4.5 ~ pnorm(length, mean_len, sd_len),
                                      length > 4.5 ~ pnorm(length, mean_len, sd_len) - pnorm(length - 5, mean_len, sd_len))) %>% 
  tidytable::select(-mean_len, -sd_len) %>% 
  tidytable::pivot_wider(names_from = age, values_from = szage)

# cgoa
sizeagetrans_cgoa_a <- goa_acomp %>% 
  tidytable::filter(region != 'wgoa',
                    sex == 0) %>% 
  tidytable::mutate(age = case_when(age <= 1 ~ 1,
                                    age >= 10 ~ 10,
                                    .default = age)) %>% 
  tidytable::summarise(mean_len = mean(mean_length),
                       .by = c(age)) %>% 
  tidytable::left_join(as.data.frame(cbind(age = 1:10,
                                           sd_len = c(4.00851,4.88844,5.61639,6.2186,6.71679,7.12893,7.46988,7.75194,7.98528,9.10287)))) %>% 
  tidytable::left_join(as.data.frame(cbind(age = 1:10,
                                           mean_len_a = c(11.9252,25.039,37.8937,48.5281,57.3255,64.6035,70.6243,75.6052,79.7257,88.2707),
                                           mean_len_b = c(17.6378,31.7708,43.4628,53.1352,61.1369,67.7565,73.2327,77.763,81.5108,89.2829))) %>% 
                         tidytable::mutate(diff = mean_len_b - mean_len_a) %>% 
                         tidytable::select(age, diff)) %>% 
  tidytable::mutate(mean_len = mean_len - diff,
                    seas = 'A',
                    region = 'cgoa') %>% 
  tidytable::select(region, seas, age, mean_len, sd_len) %>% 
  tidytable::left_join(expand.grid(age = 1:10, length = bs_dat_ss3$lbin_vector)) %>% 
  tidytable::mutate(szage = case_when(length == 4.5 ~ pnorm(length, mean_len, sd_len),
                                      length > 4.5 ~ pnorm(length, mean_len, sd_len) - pnorm(length - 5, mean_len, sd_len))) %>% 
  tidytable::select(-mean_len, -sd_len) %>% 
  tidytable::pivot_wider(names_from = age, values_from = szage)

sizeagetrans_cgoa_b <- goa_acomp %>% 
  tidytable::filter(region != 'wgoa',
                    sex == 0) %>% 
  tidytable::mutate(age = case_when(age <= 1 ~ 1,
                                    age >= 10 ~ 10,
                                    .default = age)) %>% 
  tidytable::summarise(mean_len = mean(mean_length),
                       .by = c(age)) %>% 
  tidytable::left_join(as.data.frame(cbind(age = 1:10,
                                           sd_len = c(4.00851,4.88844,5.61639,6.2186,6.71679,7.12893,7.46988,7.75194,7.98528,9.10287)))) %>% 
  tidytable::mutate(seas = 'A',
                    region = 'cgoa') %>% 
  tidytable::select(region, seas, age, mean_len, sd_len) %>% 
  tidytable::left_join(expand.grid(age = 1:10, length = bs_dat_ss3$lbin_vector)) %>% 
  tidytable::mutate(szage = case_when(length == 4.5 ~ pnorm(length, mean_len, sd_len),
                                      length > 4.5 ~ pnorm(length, mean_len, sd_len) - pnorm(length - 5, mean_len, sd_len))) %>% 
  tidytable::select(-mean_len, -sd_len) %>% 
  tidytable::pivot_wider(names_from = age, values_from = szage)

sizeagetrans <- sizeagetrans_bs_a %>% 
  tidytable::bind_rows(sizeagetrans_bs_b) %>% 
  tidytable::bind_rows(sizeagetrans_cgoa_a) %>% 
  tidytable::bind_rows(sizeagetrans_cgoa_b) %>% 
  tidytable::bind_rows(sizeagetrans_wgoa_a) %>% 
  tidytable::bind_rows(sizeagetrans_wgoa_b)

## concat ----

cod_data <- c(cod_data,
              list(WAA = waa,
                   MatAA = mataa,
                   AgeingError = ageingerror,
                   SizeAgeTrans = sizeagetrans))


# fishery data ----

## total catch ----
obscatch <- fshry_dat$DATA_610$CATCH %>% 
  dplyr::rename_all(tolower) %>% 
  tidytable::mutate(region = tolower(region_grp)) %>% 
  tidytable::select(region, year, seas = season, catch)

usecatch <- obscatch %>% 
  tidytable::select(region, year, seas) %>% 
  tidytable::mutate(usecatch = 1)

## age comps ----
obsfishagecomps <- fshry_dat$DATA_610$ACOMP %>% 
  dplyr::rename_all(tolower) %>% 
  tidytable::mutate(region = tolower(region_grp)) %>% 
  tidytable::select(region, year, seas = season, as.character(1:10))

usefishagecomps <- obsfishagecomps %>% 
  tidytable::select(region, year, seas) %>% 
  tidytable::mutate(use = 1)

iss_fishagecomps <- obsfishagecomps %>% 
  tidytable::select(region, year, seas) %>% 
  tidytable::mutate(iss = case_when(region == 'bs' ~ 100,
                                    region != 'bs' ~ 50))

wt_fishagecomps <- obsfishagecomps %>% 
  tidytable::select(region, year, seas) %>% 
  tidytable::mutate(wt = 1)



## length comps ----
obsfishlencomps <- fshry_dat$DATA_610$LCOMP %>% 
  dplyr::rename_all(tolower) %>% 
  tidytable::mutate(region = tolower(region_grp)) %>% 
  tidytable::select(region, year, seas = season, as.character(bs_dat_ss3$lbin_vector))

usefishlencomps <- obsfishlencomps %>% 
  tidytable::select(region, year, seas) %>% 
  tidytable::mutate(lencomp = 1) %>% 
  tidytable::left_join(obsfishagecomps %>% 
                         tidytable::select(region, year, seas) %>% 
                         tidytable::mutate(agecomp = 1)) %>% 
  tidytable::mutate(agecomp = replace_na(agecomp, 0),
                    use = case_when(lencomp == 1 & agecomp == 0 ~ 1,
                                    .default = 0)) %>% 
  tidytable::select(region, year, seas, use)

iss_fishlencomps <- obsfishlencomps %>% 
  tidytable::select(region, year, seas) %>% 
  tidytable::mutate(iss = case_when(region == 'bs' ~ 500,
                                    region != 'bs' ~ 250))

wt_fishlencomps <- usefishlencomps %>% 
  tidytable::rename(wt = use)

## concat ----
cod_data <- c(cod_data,
              list(ObsCatch = obscatch,
                   catch_units = 1,
                   UseCatch = usecatch,
                   ObsFishAgeComps = obsfishagecomps,
                   UseFishAgeComps = usefishagecomps,
                   Wt_FishAgeComps = wt_fishagecomps,
                   ISS_FishAgeComps = iss_fishagecomps,
                   ObsFishLenComps = obsfishlencomps,
                   UseFishLenComps = usefishlencomps,
                   ISS_FishLenComps = iss_fishlencomps,
                   Wt_FishLenComps = wt_fishlencomps,
                   FishAgeComps_LikeType = 0,
                   FishLenComps_LikeType = 0))



# survey data ----

## index ----
obssrvidx <- bs_dat_ss3$CPUE %>% 
  tidytable::filter(index > 0) %>% 
  tidytable::select(year, index = obs) %>% 
  tidytable::mutate(region = 'bs',
                    index = index * 1000) %>% 
  tidytable::select(year, region, index) %>% 
  tidytable::bind_rows(goa_twl_indx %>% 
                         tidytable::select(year, region, index = num) %>% 
                         tidytable::arrange(region)) %>% 
  tidytable::filter(year >= 1991)

# index se
obssrvidx_se <- bs_dat_ss3$CPUE %>% 
  tidytable::filter(index > 0) %>% 
  tidytable::mutate(se = obs * se_log * 1000,
                    region = 'bs') %>% 
  tidytable::select(year, region, se) %>% 
  tidytable::bind_rows(goa_twl_indx %>% 
                         tidytable::mutate(se = sqrt(num_var)) %>% 
                         tidytable::select(year, region, se) %>% 
                         tidytable::arrange(region)) %>% 
  tidytable::filter(year >= 1991)

usesrvidx <- obssrvidx_se %>% 
  tidytable::mutate(use = 1) %>% 
  tidytable::select(-se)

## age comps ----
obssrvagecomps <- bs_dat_ss3$agecomp %>% 
  tidytable::filter(fleet == 2) %>% 
  tidytable::select(year, paste0('a', 0:12)) %>% 
  tidytable::pivot_longer(cols = paste0('a', 0:12)) %>% 
  tidytable::mutate(age = as.numeric(stringr::str_remove(name, 'a')),
                    age = case_when(age == 0 ~ 1,
                                    age >= 10 ~ 10,
                                    .default = age),
                    region = 'bs') %>% 
  tidytable::summarise(value = sum(value), .by = c(region, year, age)) %>% 
  tidytable::pivot_wider(names_from = age, values_from = value) %>% 
  tidytable::bind_rows(goa_acomp %>% 
                         tidytable::filter(sex == 0,
                                           region != 'goa') %>% 
                         tidytable::mutate(age = case_when(age == 0 ~ 1,
                                                           age >= 10 ~ 10,
                                                           .default = age),
                                           region = case_when(region == 'egoa' ~ 'cgoa',
                                                              .default = region)) %>% 
                         tidytable::summarise(agepop = sum(agepop), .by = c(region, year, age)) %>% 
                         tidytable::mutate(value = agepop / sum(agepop), .by = c(region, year)) %>% 
                         tidytable::select(-agepop) %>% 
                         tidytable::pivot_wider(names_from = age, values_from = value) %>% 
                         tidytable::mutate(tidytable::across(as.character(1:10), ~ tidytable::coalesce(.x, 0)))) %>% 
  tidytable::filter(year >= 1991)

usesrvagecomps <- obssrvagecomps %>% 
  tidytable::select(region, year) %>% 
  tidytable::mutate(use = 1)

iss_srvagecomps <- obssrvagecomps %>% 
  tidytable::select(region, year) %>% 
  tidytable::mutate(iss = case_when(region == 'bs' ~ 100,
                                    region != 'bs' ~ 50))

wt_srvagecomps <- obssrvagecomps %>% 
  tidytable::select(region, year) %>% 
  tidytable::mutate(wt = 1)

## length comps ----
obssrvlencomps <- bs_dat_ss3$lencomp %>% 
  tidytable::filter(fleet == 2,
                    year >= 1991) %>% 
  tidytable::select(year, paste0('l', bs_dat_ss3$lbin_vector)) %>% 
  tidytable::pivot_longer(cols = paste0('l', bs_dat_ss3$lbin_vector)) %>% 
  tidytable::mutate(length = as.numeric(stringr::str_remove(name, 'l')),
                    region = 'bs') %>% 
  tidytable::select(region, year, length, value) %>% 
  tidytable::pivot_wider(names_from = length, values_from = value) %>% 
  tidytable::bind_rows(expand.grid(region = c('cgoa', 'wgoa'), year = unique(goa_lcomp$year), length = bs_dat_ss3$lbin_vector) %>% 
                         tidytable::left_join(goa_lcomp %>% 
                                                tidytable::filter(sex == 0,
                                                                  region != 'goa') %>% 
                                                tidytable::select(region, year, length, abund) %>% 
                                                tidytable::mutate(length_bin = case_when(length <= 4.5 ~ ceiling(length / 4.5) * 4.5,
                                                                                         length > 4.5 ~ ceiling((length + 1) / 5) * 5 - 0.5)) %>% 
                                                tidytable::summarise(abund = sum(abund), .by = c(region, year, length_bin)) %>% 
                                                tidytable::mutate(value = abund / sum(abund), .by = c(region, year)) %>% 
                                                tidytable::select(region, year, length = length_bin, value)) %>% 
                         tidytable::mutate(value = replace_na(value, 0)) %>% 
                         tidytable::pivot_wider(names_from = length, values_from = value)) %>% 
  tidytable::filter(year >= 1991)
  
usesrvlencomps <- obssrvlencomps %>% 
  tidytable::select(region, year) %>% 
  tidytable::mutate(lencomp = 1) %>% 
  tidytable::left_join(obssrvagecomps %>% 
                         tidytable::select(region, year) %>% 
                         tidytable::mutate(agecomp = 1)) %>% 
  tidytable::mutate(agecomp = replace_na(agecomp, 0),
                    use = case_when(lencomp == 1 & agecomp == 0 ~ 1,
                                    .default = 0)) %>% 
  tidytable::select(region, year, use)

iss_srvlencomps <- obssrvlencomps %>% 
  tidytable::select(region, year) %>% 
  tidytable::mutate(iss = case_when(region == 'bs' ~ 500,
                                    region != 'bs' ~ 250))

wt_srvlencomps <- usesrvlencomps %>% 
  tidytable::rename(wt = use)

## concat ----
cod_data <- c(cod_data,
              list(ObsSrvIdx = obssrvidx,
                   ObsSrvIdx_SE = obssrvidx_se,
                   UseSrvIdx = usesrvidx,
                   srv_idx_type = 0,
                   ObsSrvAgeComps = obssrvagecomps,
                   UseSrvAgeComps = usesrvagecomps,
                   ISS_SrvAgeComps = iss_srvagecomps,
                   Wt_SrvAgeComps = wt_srvagecomps,
                   ObsSrvLenComps = obssrvlencomps,
                   UseSrvLenComps = usesrvlencomps,
                   ISS_SrvLenComps = iss_srvlencomps,
                   Wt_SrvLenComps = wt_srvlencomps))


saveRDS(cod_data, file = here::here(new_year, 'rsch', 'wgoa', 'sporc_data', 'cod_data.rds'))


