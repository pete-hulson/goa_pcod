#' Get fishery catch for goa pcod
#' Originally adapted/generalized from Steve Barbeaux' files for generating SS files for EBS/AI Greenland Turbot
#' Re-developed in 2024 by p. hulson to develop link to afscdata package
#' 
#' @param new_year current assessment year
#' 

get_catch_goa_pcod <- function(new_year = 9999){

  # read in data files ----
  
  fed_raw <- vroom::vroom(here::here(new_year, "data", "raw", "fsh_catch_data.csv"))
  adfg_raw <- vroom::vroom(here::here(new_year, 'data', 'raw', 'adfg_catch.csv'))
  
  # current catch ----
  
  # summarize catch data to year, subarea, gear, type (retained/discarded), and month
  # add gear description and summarize months to season
  fed_raw %>% 
    tidytable::select(year, 
                      zone = fmp_subarea,
                      species_group = species_group_code, 
                      gear = fmp_gear,
                      type = retained_or_discarded, 
                      week_end_date, 
                      weight_posted) %>% 
    tidytable::mutate(month = month(week_end_date)) %>% 
    tidytable::select(-week_end_date) %>% 
    tidytable::summarise(tons = sum(weight_posted), 
                         .by = c(year, species_group, zone, gear, type, month)) %>% 
    tidytable::mutate(gear_desc = case_when(gear %in% c("TRW", "OTH", "GLN") ~ "trawl",
                                            gear %in% c("HAL", "JIG") ~ "longline",
                                            gear == "POT" ~ "pot"),
                      season = case_when(month %in% c(1, 2) ~ 1,
                                         month %in% c(3, 4) ~ 2,
                                         month %in% c(5, 6, 7, 8) ~ 3,
                                         month %in% c(9, 10) ~ 4,
                                         month %in% c(11, 12) ~ 5)) -> catch_summ
  
  # get total catch by year and gear
  catch_summ %>% 
    tidytable::summarise(tons = sum(tons), .by = c(year, gear_desc)) %>% 
    tidytable::rename(gear = gear_desc) -> curr_catch
  
  # old catch ----
  
  # read in fixed old catch and add gear descriptions
  vroom::vroom(here::here(new_year, 'data', 'old_catch.csv')) %>% 
    dplyr::rename_all(tolower) %>% 
    tidytable::mutate(gear_desc = case_when(gear %in% c("TRAWL") ~ "trawl",
                                            gear %in% c("LONGLINE", "OTHER") ~ "longline",
                                            gear == "POT" ~ "pot")) %>% 
    tidytable::summarise(tons = sum(tons), .by = c(year, gear_desc)) %>% 
    tidytable::rename(gear = gear_desc) -> old_catch
  
  
  # adf&g catch ----
  
  # read in adf&g catch and add gear descriptions
  adfg_raw %>% 
    tidytable::rename(year = akfin_year) %>% 
    tidytable::mutate(gear = case_when(fmp_gear %in% c("JIG", "HAL") ~ "longline",
                                       fmp_gear == "POT" ~ "pot")) %>% 
    tidytable::summarise(adfg_tons = sum(catch_mt), .by = c(year, gear)) -> adfg_catch
  
  # total catch ----
  
  # bind old and current catch and get total catch by year and gear
  curr_catch %>% 
    tidytable::left_join(adfg_catch) %>% 
    tidytable::mutate(adfg_tons = replace_na(adfg_tons, 0)) %>% 
    tidytable::mutate(tot_tons = tons + adfg_tons) %>% 
    tidytable::select(year, gear, tot_tons) %>% 
    tidytable::rename(tons = tot_tons) %>% 
    tidytable::bind_rows(old_catch) %>% 
    tidytable::arrange(gear, year) -> tot_catch
  
  # put in ss3 format ----
  
  # get total catch in ss3 format
  tot_catch %>% 
    tidytable::mutate(fleet = case_when(gear == "trawl" ~ 1,
                                        gear == "longline" ~ 2,
                                        gear == "pot" ~ 3),
                      se = 0.05,
                      seas = 1) %>% 
    tidytable::rename(catch = tons) %>% 
    tidytable::select(year, seas, fleet, catch, se) %>% 
    tidytable::bind_rows(data.frame(year = c(-999, -999, -999), 
                                    seas = c(1, 1, 1), 
                                    fleet = c(1, 2, 3), 
                                    catch = c(0, 0, 0), 
                                    se = rep(0.05, 3))) %>% 
    tidytable::arrange(fleet, year) -> ss3_catch
  
  # SAFE catch tables ----
  
  # catch by gear type and jurisdiction
  # get federal catch by gear
  fed_raw %>% 
    tidytable::select(year, 
                      zone = fmp_subarea,
                      species_group = species_group_code, 
                      gear = fmp_gear,
                      type = retained_or_discarded, 
                      week_end_date, 
                      weight_posted,
                      state_flag = akr_state_fishery_flag) %>%  
    tidytable::mutate(state_flag = replace_na(state_flag, "N")) %>% 
    filter(state_flag != "Y") %>% 
    tidytable::mutate(gear_desc = case_when(gear == "TRW" ~ "fed_trawl",
                                            gear == "HAL" ~ "fed_longline",
                                            gear == "POT" ~ "fed_pot",
                                            gear %in% c("JIG", "OTH", "GLN") ~ "fed_other")) %>% 
    tidytable::summarise(catch = round(sum(weight_posted)), .by = c(year, gear_desc)) %>% 
    tidytable::pivot_wider(names_from = gear_desc, values_from = catch) %>% 
    tidytable::select(year, fed_trawl, fed_longline, fed_pot, fed_other) %>% 
    tidytable::mutate(fed_other = replace_na(fed_other, 0)) %>% 
    tidytable::mutate(fed_tot = fed_trawl + fed_longline + fed_pot + fed_other) %>% 
    # get state catch by gear
    tidytable::left_join(adfg_raw %>% 
                           tidytable::select(year = akfin_year,
                                             gear = fmp_gear,
                                             weight_posted = catch_mt) %>% 
                           tidytable::mutate(gear_desc = case_when(gear == "HAL" ~ "st_longline",
                                                                   gear == "POT" ~ "st_pot",
                                                                   gear %in% c("JIG", "OTH", "GLN") ~ "st_other")) %>% 
                           tidytable::summarise(catch = round(sum(weight_posted)), .by = c(year, gear_desc)) %>% 
                           tidytable::pivot_wider(names_from = gear_desc, values_from = catch) %>% 
                           tidytable::select(year, st_longline, st_pot, st_other) %>% 
                           tidytable::mutate(st_longline = replace_na(st_longline, 0)) %>% 
                           tidytable::mutate(st_tot = st_longline + st_pot + st_other) %>% 
                           tidytable::bind_rows(fed_raw %>% 
                                                  tidytable::select(year, 
                                                                    zone = fmp_subarea,
                                                                    species_group = species_group_code, 
                                                                    gear = fmp_gear,
                                                                    type = retained_or_discarded, 
                                                                    week_end_date, 
                                                                    weight_posted,
                                                                    state_flag = akr_state_fishery_flag) %>%  
                                                  tidytable::mutate(state_flag = replace_na(state_flag, "N")) %>% 
                                                  filter(state_flag == "Y") %>% 
                                                  tidytable::mutate(gear_desc = case_when(gear == "HAL" ~ "st_longline",
                                                                                          gear == "POT" ~ "st_pot",
                                                                                          gear %in% c("JIG", "OTH", "GLN") ~ "st_other"))  %>% 
                                                  tidytable::summarise(catch = round(sum(weight_posted)), .by = c(year, gear_desc)) %>% 
                                                  tidytable::pivot_wider(names_from = gear_desc, values_from = catch) %>% 
                                                  tidytable::select(year, st_longline, st_pot, st_other) %>% 
                                                  tidytable::mutate(st_other = replace_na(st_other, 0)) %>% 
                                                  tidytable::mutate(st_tot = st_longline + st_pot + st_other))) %>% 
    base::replace(is.na(.), 0) %>% 
    tidytable::mutate(tot = fed_tot + st_tot) -> juris_gr_tbl
  
  # Retained/discarded table
  fed_raw %>% 
    tidytable::select(year,
                      type = retained_or_discarded,
                      weight_posted) %>% 
    tidytable::summarise(catch = round(sum(weight_posted)), .by = c(year, type)) %>%  
    tidytable::pivot_wider(names_from = type, values_from = catch) %>% 
    tidytable::left_join(adfg_raw %>% 
                           tidytable::select(year = akfin_year,
                                             weight_posted = catch_mt) %>% 
                           tidytable::summarise(catch = round(sum(weight_posted)), .by = c(year))) %>% 
    base::replace(is.na(.), 0) %>% 
    tidytable::mutate(retained = R + catch) %>% 
    tidytable::rename(discarded = D) %>% 
    tidytable::select(year, discarded, retained) %>% 
    tidytable::mutate(total = discarded + retained) -> dr_tbl
  
  # Write out results
  vroom::vroom_write(juris_gr_tbl, here::here(new_year, 'output', 'juris_gr_tbl.csv'), delim = ",")
  vroom::vroom_write(dr_tbl, here::here(new_year, 'output', 'dr_tbl.csv'), delim = ",")
  
  # fcn return ----
  
  ss3_catch
  
}
