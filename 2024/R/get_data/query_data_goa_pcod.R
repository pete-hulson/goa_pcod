#' Query data used for goa pcod
#' developed in 2024 by p. hulson
#' 
#' @param new_year current assessment year
#' @param fsh_sp species label for observer/catch data (default = 'PCOD')
#' @param fsh_sp_code species code for observer/catch data (default = 202)
#' @param fsh_subarea NPFMC subareas (default to goa subareas)
#' @param twl_srvy gap survey id number (default = 47 for goa)
#' @param srv_sp gap species code (default = 21720)
#' @param srv_area survey area (default = 'goa')
#' @param database switch for which database to pull fishery length data from (default = 'akfin')
#' 
query_goa_pcod <- function(new_year = 9999,
                           fsh_sp = "PCOD",
                           fsh_sp_code = 202,
                           fsh_subarea = c("CG","PWSI","SE","SEI","WG","WY"),
                           twl_srvy = 47,
                           srv_sp = 21720,
                           srv_area = 'goa',
                           database = 'akfin'){
  
  # get connected to akfin for the following queries
  db = 'akfin'
  conn = afscdata::connect(db)  
  
  # catch data ----
  # query catch data and write raw data to folder 
  afscdata::q_catch(year = new_year,
                    species = fsh_sp,
                    area = fsh_subarea,
                    db = conn,
                    add_fields = "akr_state_fishery_flag")
  # query ADF&G data from 1997-2002 and write raw data to folder 
  dplyr::tbl(conn, dplyr::sql('council.comprehensive_ft')) %>% 
    dplyr::rename_all(tolower) %>% 
    dplyr::select(akfin_year,
                  fmp_area,
                  adfg_i_species_code,
                  adfg_i_harvest_code,
                  fmp_gear,
                  cfec_whole_pounds) %>% 
    dplyr::filter(akfin_year >= 1997,
                  akfin_year <= 2002,
                  adfg_i_species_code == 110,
                  adfg_i_harvest_code == 80,
                  fmp_area == 'GOA') %>% 
    dplyr::summarise(catch_mt = sum(cfec_whole_pounds) / 2204.622, 
                     .by = c(adfg_i_harvest_code, fmp_area, fmp_gear, akfin_year)) -> adfg_q
  
  dplyr::collect(adfg_q) %>% 
    vroom::vroom_write(., here::here(new_year, 'data', 'raw', 'adfg_catch.csv'), delim = ",")
  capture.output(dplyr::show_query(adfg_q), 
                 file = here::here(new_year, "data", "sql", "adfg_catch_sql.txt"))
  # print message when done
  cat(crayon::green$bold("\u2713"), crayon::blue("catch query"), crayon::green$underline$bold$italic("DONE"), "\n")
  
  # trawl survey index data ----
  dplyr::tbl(conn, dplyr::sql('gap_products.akfin_biomass')) %>% 
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
                                   strata == 99903 ~ 'goa')) -> twl_q
  
  dplyr::collect(twl_q) %>% 
    vroom::vroom_write(., here::here(new_year, 'data', 'raw', 'twl_srvy_index.csv'), delim = ",")
  capture.output(dplyr::show_query(twl_q), 
                 file = here::here(new_year, "data", "sql", "twl_srvy_index_sql.txt"))
  # print message when done
  cat(crayon::green$bold("\u2713"), crayon::blue("trawl survey index query"), crayon::green$underline$bold$italic("DONE"), "\n")
  
  # longline survey index data ----
  afscdata::q_lls_rpn(year = new_year,
                      species = srv_sp,
                      area = srv_area,
                      by = 'geoarea',
                      use_historical = FALSE,
                      db = conn)
  # print message when done
  cat(crayon::green$bold("\u2713"), crayon::blue("longline survey index query"), crayon::green$underline$bold$italic("DONE"), "\n")
  
  # iphc survey index data ----
  dplyr::tbl(conn, dplyr::sql('afsc_host.fiss_rpn')) %>% 
    dplyr::rename_all(tolower) %>% 
    dplyr::filter(species %in% c('Pacific cod'),
                  fmp_sub_area %in% c("CGOA", "EY/SE", "WGOA", "WY")) %>% 
    dplyr::select(year = survey_year, 
                  fmp_sub_area, 
                  species, 
                  strata = rpn_strata, 
                  strata_rpn, 
                  boot_sd, 
                  boot_bias,
                  n_stations,
                  n_pos_catch) -> iphc_q
  
  dplyr::collect(iphc_q) %>% 
    vroom::vroom_write(., here::here(new_year, 'data', 'raw', 'iphc_srvy_index.csv'), delim = ",")
  capture.output(dplyr::show_query(iphc_q), 
                 file = here::here(new_year, "data", "sql", "iphc_srvy_index_sql.txt"))
  # print message when done
  cat(crayon::green$bold("\u2713"), crayon::blue("iphc survey index query"), crayon::green$underline$bold$italic("DONE"), "\n")
  
  # trawl survey length pop'n data ----
  dplyr::tbl(conn, dplyr::sql('gap_products.akfin_sizecomp')) %>% 
    dplyr::rename_all(tolower) %>% 
    dplyr::select(year,
                  survey_definition_id,
                  area_id,
                  species_code,
                  length_mm,
                  sex,
                  population_count) %>% 
    dplyr::filter(year <= new_year,
                  survey_definition_id == twl_srvy,
                  species_code == srv_sp,
                  area_id %in% c(99903)) %>% 
    dplyr::select(year,
                  survey = survey_definition_id, 
                  strata = area_id, 
                  species_code,
                  length = length_mm, 
                  sex,
                  num = population_count) -> twl_q
  
  dplyr::collect(twl_q) %>% 
    tidytable::filter(length > 0) %>% 
    tidytable::mutate(length = length / 10) %>% 
    vroom::vroom_write(., here::here(new_year, 'data', 'raw', 'twl_srvy_lpop.csv'), delim = ",")
  capture.output(dplyr::show_query(twl_q), 
                 file = here::here(new_year, "data", "sql", "twl_srvy_lpop_sql.txt"))
  # print message when done
  cat(crayon::green$bold("\u2713"), crayon::blue("trawl survey length pop'n query"), crayon::green$underline$bold$italic("DONE"), "\n")
  
  # longline survey length data ----
  afscdata::q_lls_rpn_length(year = new_year,
                             species = srv_sp,
                             area = srv_area,
                             db = conn)
  # print message when done
  cat(crayon::green$bold("\u2713"), crayon::blue("longline survey length rpn query"), crayon::green$underline$bold$italic("DONE"), "\n")
  
  # domestic fishery length data ----
  ## afsc ----
  if(database == 'afsc'){
    # get connected
    db = 'afsc'
    conn = afscdata::connect(db)
    # query data 
    dplyr::tbl(conn, dplyr::sql('obsint.debriefed_haul')) %>% 
      dplyr::inner_join(dplyr::tbl(conn, dplyr::sql('obsint.debriefed_spcomp')) %>% 
                          dplyr::filter(SPECIES == fsh_sp_code),
                        by = c('HAUL_JOIN')) %>% 
      dplyr::inner_join(dplyr::tbl(conn, dplyr::sql('obsint.debriefed_length')) %>% 
                          dplyr::filter(SPECIES == fsh_sp_code),
                        by = c('HAUL_JOIN')) %>% 
      dplyr::rename_all(tolower) %>% 
      dplyr::select(gear,
                    haul_join,
                    numb = extrapolated_number,
                    cruise = cruise.x,
                    permit = permit.y,
                    haul = haul.x,
                    weight = extrapolated_weight,
                    sex = sex.y,
                    length,
                    freq = frequency,
                    lon = londd_end.x,
                    lat = latdd_end.x,
                    hday = haul_date.y,
                    area = nmfs_area.x) %>% 
      dplyr::filter(area >= 600,
                    area <= 699,
                    area != 670) %>% 
      dplyr::mutate(haul_join = paste0('H', haul_join)) -> afsc_len
    
    dplyr::collect(afsc_len) %>% 
      dplyr::mutate(weight = weight / 1000,
                    year = lubridate::year(hday),
                    month = lubridate::month(hday),
                    season = dplyr::case_when(month <= 2 ~ 1,
                                              month %in% c(3, 4) ~ 2,
                                              month %in% c(5, 6, 7, 8) ~ 3,
                                              month %in% c(9, 10) ~ 4,
                                              month >= 11 ~ 5),
                    quarter = dplyr::case_when(month <= 3 ~ 1,
                                               month %in% c(4, 5, 6) ~ 2,
                                               month %in% c(7, 8, 9) ~ 3,
                                               month >= 10 ~ 4),
                    trimester = dplyr::case_when(month <= 4 ~ 1,
                                                 month %in% c(5, 6, 7, 8) ~ 2,
                                                 month >= 9 ~ 3),
                    gear = dplyr::case_when(gear %in% c(1, 2, 3, 4) ~ 'trawl',
                                            gear == 6 ~ 'pot',
                                            gear %in% c(5, 7, 9, 10, 11, 68, 8) ~ 'longline')) %>% 
      vroom::vroom_write(., here::here(new_year, 'data', 'raw', 'fish_lfreq_domestic.csv'), delim = ",")
    
    capture.output(dplyr::show_query(afsc_len), 
                   file = here::here(new_year, "data", "sql", "fsh_lfreq_domestic_afsc_sql.txt"))
  }
  ## akfin ----
  if(database == 'akfin'){
    # get connected
    db = 'akfin'
    conn = afscdata::connect(db)
    
    # query data 
    dplyr::tbl(conn, dplyr::sql('norpac.debriefed_haul_mv')) %>% 
      dplyr::inner_join(dplyr::tbl(conn, dplyr::sql('norpac.debriefed_spcomp_mv')) %>% 
                          dplyr::filter(SPECIES == fsh_sp_code),
                        by = c('HAUL_JOIN')) %>% 
      dplyr::inner_join(dplyr::tbl(conn, dplyr::sql('norpac.debriefed_length_mv')) %>% 
                          dplyr::filter(SPECIES == fsh_sp_code),
                        by = c('HAUL_JOIN')) %>% 
      dplyr::rename_all(tolower) %>% 
      dplyr::select(gear,
                    haul_join,
                    numb = extrapolated_number,
                    cruise = cruise.x,
                    permit = permit.y,
                    haul = haul.x,
                    weight = extrapolated_weight,
                    sex = sex.y,
                    length,
                    freq = frequency,
                    lon = londd_end.x,
                    lat = latdd_end.x,
                    hday = haul_date.x,
                    area = nmfs_area.x) %>% 
      dplyr::filter(area >= 600,
                    area <= 699,
                    area != 670) -> akfin_len 
    
    dplyr::collect(akfin_len) %>% 
      dplyr::mutate(haul_join = paste0('H', haul_join),
                    weight = weight / 1000,
                    year = lubridate::year(hday),
                    month = lubridate::month(hday),
                    season = dplyr::case_when(month <= 2 ~ 1,
                                              month %in% c(3, 4) ~ 2,
                                              month %in% c(5, 6, 7, 8) ~ 3,
                                              month %in% c(9, 10) ~ 4,
                                              month >= 11 ~ 5),
                    quarter = dplyr::case_when(month <= 3 ~ 1,
                                               month %in% c(4, 5, 6) ~ 2,
                                               month %in% c(7, 8, 9) ~ 3,
                                               month >= 10 ~ 4),
                    trimester = dplyr::case_when(month <= 4 ~ 1,
                                                 month %in% c(5, 6, 7, 8) ~ 2,
                                                 month >= 9 ~ 3),
                    gear = dplyr::case_when(gear %in% c(1, 2, 3, 4) ~ 'trawl',
                                            gear == 6 ~ 'pot',
                                            gear %in% c(5, 7, 9, 10, 11, 68, 8) ~ 'longline')) %>%  
      dplyr::filter(year <= new_year) %>% 
      vroom::vroom_write(., here::here(new_year, 'data', 'raw', 'fish_lfreq.csv'), delim = ",")
    
    capture.output(dplyr::show_query(akfin_len), 
                   file = here::here(new_year, "data", "sql", "fsh_lfreq_akfin_sql.txt"))
    
  }
  # print message when done
  cat(crayon::green$bold("\u2713"), crayon::blue("domestic fishery length query"), crayon::green$underline$bold$italic("DONE"), "\n")
  
  # foreign fishery length data ----
  # get connected
  db = 'afsc'
  conn = afscdata::connect(db)
  
  # query data 
  dplyr::tbl(conn, dplyr::sql('norpac.foreign_haul')) %>% 
    dplyr::inner_join(dplyr::tbl(conn, dplyr::sql('norpac.foreign_spcomp')) %>% 
                        dplyr::filter(SPECIES == fsh_sp_code),
                      by = c('HAUL_JOIN')) %>% 
    dplyr::inner_join(dplyr::tbl(conn, dplyr::sql('norpac.foreign_length')) %>% 
                        dplyr::filter(SPECIES == fsh_sp_code),
                      by = c('HAUL_JOIN')) %>% 
    dplyr::rename_all(tolower) %>% 
    dplyr::select(haul_join,
                  dt,
                  numb = species_haul_number,
                  weight = species_haul_weight,
                  length = size_group,
                  freq = frequency,
                  num_pots = number_of_pots,
                  num_skates = number_of_skates,
                  area = generic_area) %>% 
    dplyr::filter(area >= 600,
                  area <= 699,
                  area != 670) %>% 
    dplyr::mutate(haul_join = paste0('H', haul_join)) -> afsc_len
  
  dplyr::collect(afsc_len) %>% 
    dplyr::mutate(weight = weight / 1000,
                  year = lubridate::year(dt),
                  month = lubridate::month(dt),
                  season = dplyr::case_when(month <= 2 ~ 1,
                                            month %in% c(3, 4) ~ 2,
                                            month %in% c(5, 6, 7, 8) ~ 3,
                                            month %in% c(9, 10) ~ 4,
                                            month >= 11 ~ 5),
                  gear = dplyr::case_when(is.na(num_pots) & is.na(num_skates) ~ 'trawl',
                                          !is.na(num_pots) ~ 'pot',
                                          !is.na(num_skates) ~ 'longline')) %>% 
    dplyr::select(year, gear, haul_join, month, season, numb, weight, length, freq) %>% 
    vroom::vroom_write(., here::here(new_year, 'data', 'raw', 'fish_lfreq_foreign.csv'), delim = ",")
  
  capture.output(dplyr::show_query(afsc_len), 
                 file = here::here(new_year, "data", "sql", "fsh_lfreq_foreign_afsc_sql.txt"))
  # print message when done
  cat(crayon::green$bold("\u2713"), crayon::blue("foreign fishery length query"), crayon::green$underline$bold$italic("DONE"), "\n")
  
  
  # trawl survey age pop'n data ----
  # get connected
  db = 'akfin'
  conn = afscdata::connect(db)
  
  # query data
  dplyr::tbl(conn, dplyr::sql('gap_products.akfin_agecomp')) %>% 
    dplyr::rename_all(tolower) %>% 
    dplyr::select(year,
                  survey_definition_id,
                  area_id,
                  species_code,
                  age,
                  sex,
                  population_count,
                  length_mm_mean) %>% 
    dplyr::filter(year <= new_year,
                  survey_definition_id == twl_srvy,
                  species_code == srv_sp,
                  area_id %in% c(99903)) %>% 
    dplyr::select(year,
                  survey = survey_definition_id, 
                  strata = area_id, 
                  species_code,
                  age, 
                  sex,
                  num = population_count,
                  mean_len = length_mm_mean) -> twl_q
  
  dplyr::collect(twl_q) %>% 
    tidytable::filter(age > 0) %>% 
    tidytable::mutate(mean_len = mean_len / 10) %>% 
    vroom::vroom_write(., here::here(new_year, 'data', 'raw', 'twl_srvy_apop.csv'), delim = ",")
  capture.output(dplyr::show_query(twl_q), 
                 file = here::here(new_year, "data", "sql", "twl_srvy_apop_sql.txt"))
  # print message when done
  cat(crayon::green$bold("\u2713"), crayon::blue("trawl survey age pop'n query"), crayon::green$underline$bold$italic("DONE"), "\n")
  
  # domestic fishery age data ----
    # get connected
    db = 'afsc'
    conn = afscdata::connect(db)

    # query data 
    dplyr::tbl(conn, dplyr::sql('obsint.debriefed_age')) %>% 
      dplyr::filter(SPECIES == fsh_sp_code) %>% 
      dplyr::rename_all(tolower) %>% 
      dplyr::select(year,
                    area = nmfs_area,
                    haul_offload,
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
                    port_join = paste0('P', port_join)) -> afsc_age

    dplyr::collect(afsc_age) %>% 
      dplyr::filter(!is.na(age)) %>% 
      dplyr::mutate(gear = dplyr::case_when(gear %in% c(1, 2, 3, 4) ~ 'trawl',
                                            gear == 6 ~ 'pot',
                                            gear %in% c(5, 7, 9, 10, 11, 68, 8) ~ 'longline')) %>% 
      dplyr::filter(!is.na(gear)) %>% 
      vroom::vroom_write(., here::here(new_year, 'data', 'raw', 'fish_age_domestic.csv'), delim = ",")
    
    capture.output(dplyr::show_query(afsc_age), 
                   file = here::here(new_year, "data", "sql", "fsh_age_domestic_afsc_sql.txt"))
    # print message when done
    cat(crayon::green$bold("\u2713"), crayon::blue("fishery age query"), crayon::green$underline$bold$italic("DONE"), "\n")
    
    
    # trawl survey age specimen data ----
    # get connected
    db = 'akfin'
    conn = afscdata::connect(db)
    
    # query data
    dplyr::tbl(conn, dplyr::sql('gap_products.akfin_haul')) %>% 
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
      dplyr::filter(survey_definition_id %in% twl_srvy,
                    species_code %in% srv_sp) %>% 
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
                    long_mid) -> twl_q
    
    dplyr::collect(twl_q) %>% 
      tidytable::filter(age > 0) %>% 
      tidytable::mutate(length = length / 10,
                        weight = weight / 1000) %>% 
      vroom::vroom_write(., here::here(new_year, 'data', 'raw', 'twl_srvy_age.csv'), delim = ",")
    capture.output(dplyr::show_query(twl_q), 
                   file = here::here(new_year, "data", "sql", "twl_srvy_age_sql.txt"))
    # print message when done
    cat(crayon::green$bold("\u2713"), crayon::blue("trawl survey age specimen query"), crayon::green$underline$bold$italic("DONE"), "\n")
    
}