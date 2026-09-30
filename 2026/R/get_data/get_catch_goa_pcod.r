#' Get fishery catch for goa pcod
#' Originally adapted/generalized from Steve Barbeaux' files for generating SS files for EBS/AI Greenland Turbot
#' Re-developed in 2024 by p. hulson to develop link to afscdata package
#' 
#' @param new_year current assessment year
#' 

get_catch_goa_pcod <- function(new_year = 9999){

  # read in data files ----
  
  fed_raw <- vroom::vroom(here::here(new_year, "data", "raw", "fish_catch_data.csv"), 
                          progress = FALSE, 
                          show_col_types = FALSE)
  adfg_raw <- vroom::vroom(here::here(new_year, 'data', 'raw', 'adfg_catch.csv'), 
                           progress = FALSE, 
                           show_col_types = FALSE)
  specs <- vroom::vroom(here::here(new_year, 'data', 'raw', 'specs.csv'), 
                        progress = FALSE, 
                        show_col_types = FALSE)
  
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
  vroom::vroom(here::here(new_year, 'data', 'historical', 'old_catch.csv')) %>% 
    dplyr::rename_all(tolower) %>% 
    tidytable::mutate(gear_desc = case_when(gear %in% c("TRAWL") ~ "trawl",
                                            gear %in% c("LONGLINE", "OTHER") ~ "longline",
                                            gear == "POT" ~ "pot")) %>% 
    tidytable::summarise(tons = sum(tons), .by = c(year, gear_desc)) %>% 
    tidytable::rename(gear = gear_desc) -> old_catch
  
  
  # adf&g catch ----
  
  # add gear descriptions and summarise catch by gear
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
  
  # set current year catch at abc ----
  # current year catch is set at the average proportion for each gear over the previous 5 years of full catch
  tot_catch %>% 
    tidytable::filter(year < new_year) %>% 
    tidytable::bind_rows(tot_catch %>% 
                           tidytable::filter(year >= new_year - 6,
                                             year <= new_year -1) %>% 
                           tidytable::summarise(tons = mean(tons), .by = gear) %>% 
                           tidytable::mutate(prop_c = tons / sum(tons),
                                             curr_c = prop_c * as.numeric(specs %>% 
                                                                            tidytable::filter(year == new_year,
                                                                                              area_label == "GOA") %>% 
                                                                            tidytable::select(abc = acceptable_biological_catch)),
                                             year = new_year) %>% 
                           tidytable::select(year, gear, tons = curr_c)) -> tot_catch

  # put in ss3 format ----
  
  # get total catch in ss3 format
  tot_catch %>% 
    tidytable::mutate(fleet = case_when(gear == "trawl" ~ 1,
                                        gear == "longline" ~ 2,
                                        gear == "pot" ~ 3),
                      catch_se = 0.05,
                      seas = 1) %>% 
    tidytable::rename(catch = tons) %>% 
    tidytable::select(year, seas, fleet, catch, catch_se) %>% 
    tidytable::bind_rows(data.frame(year = c(-999, -999, -999), 
                                    seas = c(1, 1, 1), 
                                    fleet = c(1, 2, 3), 
                                    catch = c(0, 0, 0), 
                                    catch_se = rep(0.05, 3))) %>% 
    tidytable::arrange(fleet, year) -> ss3_catch
  
  # fcn return ----
  
  data.frame(ss3_catch)
  
}
