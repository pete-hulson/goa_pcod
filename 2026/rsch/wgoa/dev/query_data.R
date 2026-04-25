library(afscdata)
library(DR4SS)

# get connected
db = 'akfin'
conn = afscdata::connect(db)  
twl_srvy = 47
srv_sp = 21720
fsh_sp_code = 202
new_year <- as.numeric(format(Sys.Date(), format = "%Y"))

# trawl age query ----
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

srvy_age <- dplyr::collect(twl_q) %>% 
  tidytable::mutate(length = length / 10,
                    weight = weight / 1000) %>% 
  vroom::vroom_write(., here::here(new_year, 'rsch', 'wgoa', 'data', 'twl_srvy_age.csv'), delim = ",")


# trawl length freq query ----
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
  vroom::vroom_write(., here::here(new_year, 'rsch', 'wgoa', 'data', 'twl_srvy_lenfreq.csv'), delim = ",")

# domestic fishery observer catch data ----
dplyr::tbl(conn, dplyr::sql('norpac.debriefed_haul_mv')) %>% 
  dplyr::inner_join(dplyr::tbl(conn, dplyr::sql('norpac.debriefed_spcomp_mv')) %>% 
                      dplyr::filter(SPECIES == fsh_sp_code),
                    by = c('HAUL_JOIN')) %>% 
  dplyr::rename_all(tolower) %>% 
  dplyr::select(gear = gear_type,
                weight_kg = extrapolated_weight,
                dur_min = duration_in_min,
                hk_pots = total_hook_pots,
                targ = haul_target_name,
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
  # dplyr::filter(year <= new_year,
  #               year >= 2015) %>% 
  vroom::vroom_write(., here::here(new_year, 'rsch', 'wgoa', 'data', 'fish_obs_catch.csv'), delim = ",")



# domestic catch query ----

catch <- DR4SS::get_blend_catch_region(conn,
                                       species = 202,
                                       region_def = list(BS = c(500:539), WGOA = c(610),CGOA = c(620:649)),
                                       season_def = list(A = c(1:3), B = c(4:12)),
                                       start_year = 1990,
                                       end_year = 2025,
                                       wgoa_cod = TRUE,
                                       wgoa_cut = -156)

vroom::vroom_write(catch, here::here(new_year, 'rsch', 'wgoa', 'data', 'dom_catch.csv'), delim = ",")




# domestic fishery length data ----
# arguments
fsh_sp_code = 202
region_def = list(BS = c(500:539), WGOA = c(610),CGOA = c(620:649))
start_year = 2000 
end_year = 2025

region_vec = unlist(region_def, use.names = FALSE)

# cat("\u231b", crayon::blue("working on domestic fishery length query..."), "\n")
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
  dplyr::filter(area %in% region_vec) -> akfin_len 

dplyr::collect(akfin_len) %>% 
  dplyr::mutate(haul_join = paste0('H', haul_join),
                weight = weight / 1000,
                year = lubridate::year(hday),
                month = lubridate::month(hday),
                gear = dplyr::case_when(gear %in% c(1, 2, 3, 4) ~ 'trawl',
                                        gear == 6 ~ 'pot',
                                        gear %in% c(5, 7, 9, 10, 11, 68, 8) ~ 'longline')) %>%  
  dplyr::filter(year <= end_year & year >= start_year) %>% 
  vroom::vroom_write(., here::here(new_year, 'rsch', 'wgoa', 'data', 'fish_lfreq_domestic.csv'), delim = ",")

capture.output(dplyr::show_query(akfin_len), 
               file = here::here(new_year, 'rsch', 'wgoa', 'data', 'sql', 'fsh_lfreq_sql.txt'))
