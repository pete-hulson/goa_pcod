#' Query data used for goa pcod
#' developed in 2024 by p. hulson
#' 
#' @param new_year current assessment year
#' @param fsh_sp species label for observer/catch data (default = 'PCOD')
#' @param fsh_sp_code species code for observer/catch data (default = 202)
#' @param fsh_target trip target code for catch data (default = "c")
#' @param fsh_subarea NPFMC subareas (default to goa subareas)
#' @param twl_srvy gap survey id number (default = 47 for goa)
#' @param srv_sp gap species code (default = 21720)
#' @param area assessment area (default = 'goa')
#' 
query_goa_pcod <- function(new_year = 9999,
                           fsh_sp = "PCOD",
                           fsh_sp_code = 202,
                           fsh_target = "c",
                           fsh_subarea = c("CG","PWSI","SE","SEI","WG","WY"),
                           twl_srvy = 47,
                           srv_sp = 21720,
                           area = 'goa'){
  
  # get connected
  db = 'akfin'
  conn = afscdata::connect(db)  
  
  # survey data ----
  
  ## trawl survey index data ----
  cat("\u231b", crayon::blue("working on trawl survey index query..."), "\n")
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
  
  ## trawl survey length pop'n data ----
  cat("\u231b", crayon::blue("working on trawl survey length pop'n query..."), "\n")
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
  
  ## trawl survey age pop'n data ----
  cat("\u231b", crayon::blue("working on trawl survey age pop'n query..."), "\n")
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
  
  ## trawl survey age specimen data ----
  cat("\u231b", crayon::blue("working on trawl survey age specimen query..."), "\n")
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
  
  ## trawl survey age specimen data for age bias ----
  dplyr::tbl(conn, dplyr::sql('gap_products.akfin_haul')) %>% 
    dplyr::inner_join(dplyr::tbl(conn, dplyr::sql('gap_products.akfin_cruise')),
                      by = c('CRUISEJOIN')) %>% 
    dplyr::inner_join(dplyr::tbl(conn, dplyr::sql('gap_products.akfin_specimen')),
                      by = c('HAULJOIN')) %>% 
    dplyr::rename_all(tolower) %>% 
    dplyr::select(survey_definition_id,
                  species_code,
                  cruise,
                  vessel_id,
                  haul,
                  specimen_id,
                  sex,
                  length_mm,
                  weight_g,
                  age) %>% 
    dplyr::filter(survey_definition_id %in% c(47, 98),
                  species_code %in% srv_sp,
                  cruise %in% 200301,
                  vessel_id %in% c(88, 89, 147)) %>% 
    dplyr::rename(survey = survey_definition_id,
                  specimen = specimen_id,
                  vessel = vessel_id,
                  length = length_mm,
                  weight = weight_g,
                  original_age = age) -> twl_q
  
  dplyr::collect(twl_q) %>% 
    tidytable::filter(original_age > 0) %>% 
    tidytable::mutate(length = length / 10,
                      weight = weight / 1000) %>% 
    vroom::vroom_write(., here::here(new_year, 'data', 'raw', 'twl_srvy_age_bias.csv'), delim = ",")
  capture.output(dplyr::show_query(twl_q), 
                 file = here::here(new_year, "data", "sql", "twl_srvy_age_bias_sql.txt"))
  # print message when done
  cat(crayon::green$bold("\u2713"), crayon::blue("trawl survey age specimen for age bias query"), crayon::green$underline$bold$italic("DONE"), "\n")
  
  ## trawl survey haul info query ----
  cat("\u231b", crayon::blue("working on trawl survey haul data query..."), "\n")
  dplyr::tbl(conn, dplyr::sql('gap_products.akfin_haul')) %>% 
    dplyr::inner_join(dplyr::tbl(conn, dplyr::sql('gap_products.akfin_cruise')),
                      by = c('CRUISEJOIN')) %>%
    dplyr::rename_all(tolower) %>% 
    dplyr::select(year,
                  survey_definition_id,
                  stratum,
                  hauljoin,
                  gear_temperature_c,
                  surface_temperature_c,
                  depth_m,
                  latitude_dd_end,
                  longitude_dd_end) %>% 
    dplyr::filter(survey_definition_id %in% twl_srvy) %>% 
    dplyr::select(year, 
                  survey = survey_definition_id,
                  stratum,
                  hauljoin,
                  temp = gear_temperature_c,
                  stemp = surface_temperature_c,
                  depth = depth_m,
                  lat = latitude_dd_end,
                  lon = longitude_dd_end) -> twl_q
  
  dplyr::collect(twl_q) %>% 
    vroom::vroom_write(., here::here(new_year, 'data', 'raw', 'twl_srvy_haul.csv'), delim = ",")
  capture.output(dplyr::show_query(twl_q), 
                 file = here::here(new_year, "data", "sql", "twl_srvy_haul_sql.txt"))
  # print message when done
  cat(crayon::green$bold("\u2713"), crayon::blue("trawl survey haul data query"), crayon::green$underline$bold$italic("DONE"), "\n")
  
  ## trawl survey catch query ----
  cat("\u231b", crayon::blue("working on trawl survey catch data query..."), "\n")
  dplyr::tbl(conn, dplyr::sql('gap_products.akfin_haul')) %>% 
    dplyr::inner_join(dplyr::tbl(conn, dplyr::sql('gap_products.akfin_cruise')),
                      by = c('CRUISEJOIN')) %>% 
    dplyr::inner_join(dplyr::tbl(conn, dplyr::sql('gap_products.akfin_catch')),
                      by = c('CRUISEJOIN')) %>%
    dplyr::rename_all(tolower) %>% 
    dplyr::select(year,
                  survey_definition_id,
                  stratum,
                  hauljoin.x,
                  latitude_dd_end,
                  longitude_dd_end,
                  count,
                  distance_fished_km,
                  net_width_m,
                  species_code) %>% 
    dplyr::filter(survey_definition_id %in% twl_srvy,
                  species_code %in% srv_sp) %>% 
    dplyr::mutate(cpue = count / (distance_fished_km * (net_width_m * 1000))) %>% 
    dplyr::select(year, 
                  survey = survey_definition_id,
                  stratum,
                  hauljoin = hauljoin.x,
                  lat = latitude_dd_end,
                  lon = longitude_dd_end,
                  cpue,
                  species_code) -> twl_q
  
  dplyr::collect(twl_q) %>% 
    vroom::vroom_write(., here::here(new_year, 'data', 'raw', 'twl_srvy_catch.csv'), delim = ",")
  capture.output(dplyr::show_query(twl_q), 
                 file = here::here(new_year, "data", "sql", "twl_srvy_catch_sql.txt"))
  # print message when done
  cat(crayon::green$bold("\u2713"), crayon::blue("trawl survey catch data query"), crayon::green$underline$bold$italic("DONE"), "\n")
  
  ## trawl length freq query ----
  cat("\u231b", crayon::blue("working on trawl survey length freq data query..."), "\n")
  dplyr::tbl(conn, dplyr::sql('gap_products.akfin_haul')) %>% 
    dplyr::inner_join(dplyr::tbl(conn, dplyr::sql('gap_products.akfin_cruise')),
                      by = c('CRUISEJOIN')) %>% 
    dplyr::inner_join(dplyr::tbl(conn, dplyr::sql('gap_products.akfin_length')),
                      by = c('HAULJOIN')) %>%
    dplyr::rename_all(tolower) %>% 
    dplyr::select(year,
                  survey_definition_id,
                  stratum,
                  hauljoin,
                  gear_temperature_c,
                  surface_temperature_c,
                  depth_m,
                  latitude_dd_end,
                  longitude_dd_end,
                  species_code,
                  sex,
                  length_mm,
                  frequency) %>% 
    dplyr::filter(survey_definition_id %in% twl_srvy,
                  species_code %in% srv_sp) %>% 
    dplyr::mutate(length = length_mm / 10) %>% 
    dplyr::select(year, 
                  survey = survey_definition_id,
                  stratum,
                  hauljoin,
                  temp = gear_temperature_c,
                  stemp = surface_temperature_c,
                  depth = depth_m,
                  lat = latitude_dd_end,
                  lon = longitude_dd_end,
                  species_code,
                  sex,
                  length,
                  frequency) -> twl_q
  
  dplyr::collect(twl_q) %>% 
    vroom::vroom_write(., here::here(new_year, 'data', 'raw', 'twl_srvy_lenfreq.csv'), delim = ",")
  capture.output(dplyr::show_query(twl_q), 
                 file = here::here(new_year, "data", "sql", "twl_srvy_lenfreq_sql.txt"))
  # print message when done
  cat(crayon::green$bold("\u2713"), crayon::blue("trawl survey length freq data query"), crayon::green$underline$bold$italic("DONE"), "\n")

  ## trawl survey strata data ----
  cat("\u231b", crayon::blue("working on trawl survey strata data query..."), "\n")
  # strata with area sizes
  dplyr::tbl(conn, dplyr::sql('gap_products.akfin_area')) %>% 
    dplyr::rename_all(tolower) %>% 
    dplyr::filter(survey_definition_id %in% twl_srvy,
                  area_type == 'STRATUM') %>% 
    dplyr::select(survey = survey_definition_id,
                  design_year,
                  stratum = area_id,
                  area = area_km2) %>% 
    dplyr::collect() -> st_area
  # subregion level with description (e.g., wgoa, etc)
  dplyr::tbl(conn, dplyr::sql('gap_products.akfin_area')) %>% 
    dplyr::rename_all(tolower) %>% 
    dplyr::filter(survey_definition_id %in% twl_srvy,
                  area_type == 'REGULATORY AREA') %>% 
    dplyr::select(area_id,
                  subarea_name = description,
                  design_year) %>% 
    dplyr::collect() -> subreg
  # strata within subregions
  dplyr::tbl(conn, dplyr::sql('gap_products.akfin_stratum_groups')) %>% 
    dplyr::rename_all(tolower) %>% 
    dplyr::filter(survey_definition_id %in% twl_srvy) %>% 
    dplyr::select(stratum, 
                  area_id) %>% 
    dplyr::collect() -> st_subreg
  # join all to get strata with area sizes and subregion ids
  st_area %>% 
    tidytable::left_join(st_subreg %>% 
                           tidytable::left_join(subreg) %>% 
                           tidytable::drop_na()) %>%
    tidytable::filter(design_year == max(design_year), .by = c(stratum)) %>% 
    vroom::vroom_write(., here::here(new_year, 'data', 'raw', 'twl_srvy_strata.csv'), delim = ",")
  # print message when done
  cat(crayon::green$bold("\u2713"), crayon::blue("trawl survey strata data query"), crayon::green$underline$bold$italic("DONE"), "\n")

  ## longline survey index data ----
  cat("\u231b", crayon::blue("working on longline survey index query..."), "\n")
  SimDesign::quiet(afscdata::q_lls_rpn(year = new_year,
                      species = srv_sp,
                      area = area,
                      by = 'geoarea',
                      use_historical = FALSE,
                      db = conn))
  # print message when done
  cat(crayon::green$bold("\u2713"), crayon::blue("longline survey index query"), crayon::green$underline$bold$italic("DONE"), "\n")
  
  ## longline survey length data ----
  cat("\u231b", crayon::blue("working on longline survey length rpn query..."), "\n")
  SimDesign::quiet(afscdata::q_lls_rpn_length(year = new_year,
                             species = srv_sp,
                             area = area,
                             db = conn))
  # print message when done
  cat(crayon::green$bold("\u2713"), crayon::blue("longline survey length rpn query"), crayon::green$underline$bold$italic("DONE"), "\n")

  ## iphc survey index data ----
  cat("\u231b", crayon::blue("working on iphc survey index query..."), "\n")
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

  # fishery data ----
  
  ## catch data ----
  cat("\u231b", crayon::blue("working on catch query..."), "\n")
  ### query catch data and write raw data to folder ----
  SimDesign::quiet(afscdata::q_catch(year = new_year,
                    species = fsh_sp,
                    area = fsh_subarea,
                    db = conn,
                    add_fields = c("akr_state_fishery_flag", "vessel_id")))
  ### query ADF&G data from 1997-2002 and write raw data to folder ----
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
  ### query bycatch in pcod target fishery ----
  dplyr::tbl(conn, dplyr::sql('council.comprehensive_blend_ca')) %>% 
    dplyr::rename_all(tolower) %>% 
    dplyr::select(year,                   
                  fmp_area,
                  species_group_code,
                  species_name,          
                  retained_or_discarded,            
                  trip_target_code,                 
                  trip_target_name,                      
                  weight_posted) %>% 
    dplyr::filter(fmp_area == "GOA",
                  trip_target_code == "C",
                  species_group_code != "PCOD",
                  year >= new_year - 4) %>% 
    dplyr::summarise(catch = round(sum(weight_posted), digits = 2),
                     .by = c(year, species_name, retained_or_discarded)) -> bycatch_q
  
  dplyr::collect(bycatch_q) %>% 
    vroom::vroom_write(., here::here(new_year, 'data', 'raw', 'bycatch.csv'), delim = ",")
  capture.output(dplyr::show_query(bycatch_q), 
                 file = here::here(new_year, "data", "sql", "bycatch_sql.txt"))
  # print message when done
  cat(crayon::green$bold("\u2713"), crayon::blue("catch query"), crayon::green$underline$bold$italic("DONE"), "\n")
  
  ## domestic fishery length data ----
  cat("\u231b", crayon::blue("working on domestic fishery length query..."), "\n")
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
    vroom::vroom_write(., here::here(new_year, 'data', 'raw', 'fish_lfreq_domestic.csv'), delim = ",")
  
  capture.output(dplyr::show_query(akfin_len), 
                 file = here::here(new_year, "data", "sql", "fsh_lfreq_sql.txt"))
  # print message when done
  cat(crayon::green$bold("\u2713"), crayon::blue("domestic fishery length query"), crayon::green$underline$bold$italic("DONE"), "\n")
  
  ## domestic fishery observer catch data ----
  cat("\u231b", crayon::blue("working on domestic fishery observer catch query..."), "\n")
  dplyr::tbl(conn, dplyr::sql('norpac.debriefed_haul_mv')) %>% 
    dplyr::inner_join(dplyr::tbl(conn, dplyr::sql('norpac.debriefed_spcomp_mv')) %>% 
                        dplyr::filter(SPECIES == fsh_sp_code),
                      by = c('HAUL_JOIN')) %>% 
    dplyr::rename_all(tolower) %>% 
    dplyr::select(gear = gear_type,
                  weight_kg = extrapolated_weight,
                  lon = londd_start,
                  lat = latdd_start,
                  hday = haul_date.x,
                  area = nmfs_area) %>% 
    dplyr::filter(area >= 600,
                  area <= 699,
                  area != 670) -> akfin_obsc
  
  dplyr::collect(akfin_obsc) %>% 
    dplyr::mutate(year = lubridate::year(hday),
                  gear = dplyr::case_when(gear %in% c(1, 2, 3, 4) ~ 'trawl',
                                          gear == 6 ~ 'pot',
                                          gear %in% c(5, 7, 9, 10, 11, 68, 8) ~ 'longline')) %>%  
    dplyr::filter(year <= new_year,
                  year >= 2015) %>% 
    vroom::vroom_write(., here::here(new_year, 'data', 'raw', 'fish_obs_catch.csv'), delim = ",")
  
  capture.output(dplyr::show_query(akfin_obsc), 
                 file = here::here(new_year, "data", "sql", "fsh_obs_catch_sql.txt"))
  # print message when done
  cat(crayon::green$bold("\u2713"), crayon::blue("domestic fishery observer catch query"), crayon::green$underline$bold$italic("DONE"), "\n")
  
  ## foreign fishery length data ----
  cat("\u231b", crayon::blue("working on foreign fishery length query..."), "\n")
  dplyr::tbl(conn, dplyr::sql('pre1991.foreign_haul')) %>% 
    dplyr::inner_join(dplyr::tbl(conn, dplyr::sql('pre1991.foreign_spcomp')) %>% 
                        dplyr::filter(SPECIES == fsh_sp_code),
                      by = c('HAUL_JOIN')) %>% 
    dplyr::inner_join(dplyr::tbl(conn, dplyr::sql('pre1991.foreign_length')) %>% 
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
                 file = here::here(new_year, "data", "sql", "fsh_lfreq_foreign_sql.txt"))
  # print message when done
  cat(crayon::green$bold("\u2713"), crayon::blue("foreign fishery length query"), crayon::green$underline$bold$italic("DONE"), "\n")
  
  ## domestic fishery age data ----
  cat("\u231b", crayon::blue("working on fishery age query..."), "\n")
  dplyr::tbl(conn, dplyr::sql('norpac.debriefed_age')) %>% 
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
                 file = here::here(new_year, "data", "sql", "fsh_age_domestic_sql.txt"))
  # print message when done
  cat(crayon::green$bold("\u2713"), crayon::blue("fishery age query"), crayon::green$underline$bold$italic("DONE"), "\n")

  ## pelagic trawl catch data ----
  cat("\u231b", crayon::blue("working on domestic pelagic trawl catch query..."), "\n")
  dplyr::tbl(conn, dplyr::sql('norpac.debriefed_haul_mv')) %>% 
    dplyr::filter(GEAR_TYPE == 2) %>% 
    dplyr::inner_join(dplyr::tbl(conn, dplyr::sql('norpac.debriefed_spcomp_mv')),
                      by = c('HAUL_JOIN')) %>% 
    dplyr::rename_all(tolower) %>% 
    dplyr::select(species,
                  weight_kg = extrapolated_weight,
                  hday = haul_date.x,
                  area = nmfs_area) %>% 
    dplyr::filter(area %in% c(620, 630)) -> pel_twl

  dplyr::collect(pel_twl) %>% 
    dplyr::mutate(year = lubridate::year(hday),
                  month = lubridate::month(hday)) %>% 
    dplyr::filter(month %in% seq(1,3),
                  year > 2007) %>% 
    vroom::vroom_write(., here::here(new_year, 'data', 'raw', 'pel_twl.csv'), delim = ",")
  
  capture.output(dplyr::show_query(pel_twl), 
                 file = here::here(new_year, "data", "sql", "pel_twl_sql.txt"))
  # print message when done
  cat(crayon::green$bold("\u2713"), crayon::blue("pelagic trawl query"), crayon::green$underline$bold$italic("DONE"), "\n")
  
  ## shallow water flats pcod catch ----
  cat("\u231b", crayon::blue("working on shallow water flats catch query..."), "\n")
  dplyr::tbl(conn, dplyr::sql('norpac.debriefed_haul_mv')) %>% 
    dplyr::filter(TRIP_TARGET_NAME == "Shallow Water Flatfish - GOA",
                  GEAR_TYPE == 1) %>% 
    dplyr::inner_join(dplyr::tbl(conn, dplyr::sql('norpac.debriefed_spcomp_mv')),
                      by = c('HAUL_JOIN')) %>% 
    dplyr::rename_all(tolower) %>% 
    dplyr::select(species,
                  gear = gear_type,
                  weight_kg = extrapolated_weight,
                  hday = haul_date.x,
                  area = nmfs_area) %>% 
    dplyr::filter(area > 609,
                  area < 650) -> swf_catch
  
  dplyr::collect(swf_catch) %>% 
    dplyr::mutate(year = lubridate::year(hday)) %>%  
    dplyr::filter(year > 2007) %>% 
    vroom::vroom_write(., here::here(new_year, 'data', 'raw', 'swf_catch.csv'), delim = ",")
  
  capture.output(dplyr::show_query(swf_catch), 
                 file = here::here(new_year, "data", "sql", "swf_catch_sql.txt"))
  # print message when done
  cat(crayon::green$bold("\u2713"), crayon::blue("swf catch query"), crayon::green$underline$bold$italic("DONE"), "\n")
 
  ## nontarget catch ----
  SimDesign::quiet(afscdata::q_nontarget(year = new_year,
                                         target = fsh_target,
                                         area = area,
                                         db = conn,
                                         save = TRUE))
  cat(crayon::green$bold("\u2713"), crayon::blue("nontarget catch query"), crayon::green$underline$bold$italic("DONE"), "\n")
  
  ## prohib species catch ----
  SimDesign::quiet(afscdata::q_psc(year = new_year,
                                   target = fsh_target,
                                   area = area,
                                   db = conn,
                                   save = TRUE))
  cat(crayon::green$bold("\u2713"), crayon::blue("prohib species catch query"), crayon::green$underline$bold$italic("DONE"), "\n")

  ## specs ----
  SimDesign::quiet(afscdata::q_specs(year = new_year,
                                     species = fsh_sp,
                                     area = area,
                                     db = conn,
                                     save = TRUE))
  cat(crayon::green$bold("\u2713"), crayon::blue("specs query"), crayon::green$underline$bold$italic("DONE"), "\n")
  
  ## em data ----
  dplyr::tbl(conn, dplyr::sql('akfin_marts.comprehensive_obs_em')) %>% 
    dplyr::rename_all(tolower) %>%
    dplyr::select(year,
                  target = trip_target_code,
                  species = obs_species_code,
                  em_haul_num = em_haul_number,
                  em_trip_num = em_trip_number,
                  vessel = vessel_id,
                  haul_date = retrieval_start_date,
                  gear = agency_gear_code,
                  area = fmp_area,
                  subarea = fmp_subarea,
                  start_lat = retrieval_start_latitude_dd,
                  start_lon = retrieval_start_longitude_dd,
                  end_lat = retrieval_end_latitude_dd,
                  end_lon = retrieval_end_longitude_dd,
                  weight_kg = extrapolated_weight_kg,
                  num = extrapolated_number) %>% 
    dplyr::filter(subarea %in% fsh_subarea,
                  species == fsh_sp_code,
                  year >= new_year - 5) -> em_catch

  dplyr::collect(em_catch) %>% 
    dplyr::mutate(haul_date = lubridate::date(haul_date),
                  lat = (start_lat + end_lat) / 2,
                  lon = (start_lon + end_lon) / 2,
                  gear = case_when(gear == "POT" ~ "Pot",
                                   gear == "HAL" ~ "Longline",
                                   .default = gear)) %>% 
    tidytable::select(-start_lat, -end_lat, -start_lon, -end_lon) %>% 
    vroom::vroom_write(., here::here(new_year, 'data', 'raw', 'em_catch.csv'), delim = ",")
  cat(crayon::green$bold("\u2713"), crayon::blue("em catch query"), crayon::green$underline$bold$italic("DONE"), "\n")

}
