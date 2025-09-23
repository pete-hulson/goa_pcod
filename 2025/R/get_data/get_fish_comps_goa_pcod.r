#' Get fishery catch length composition for goa pcod pre-1991 (domestic and foreign fishery)
#' Originally adapted/generalized from Steve Barbeaux's files for generating SS files for EBS/AI Greenland Turbot
#' Re-developed in 2024 by p. hulson from GET_GOA_LENCOM2.r file
#' 
#' @param new_year current assessment year
#' @param bins specified length bins (default = NULL)
#' @param ss3_frmt whether to format comp data for ss3 data file (default = TRUE)
#' 

get_fsh_len_pre91 <- function(new_year = 9999,
                              bins = NULL,
                              ss3_frmt = TRUE){
  
  # read and prep data ----
  
  ## federal lengths ----
  fsh_len_d <- vroom::vroom(here::here(new_year, 'data', 'raw', 'fish_lfreq_domestic.csv')) %>% 
    # filter to years pre-1991 (and after 1977)
    tidytable::filter(year >= 1977,
                      year < 1991)
  
  ## foreign lengths----
  fsh_len_f <- vroom::vroom(here::here(new_year, 'data', 'raw', 'fish_lfreq_foreign.csv')) %>% 
    tidytable::filter(year >= 1977)
  
  # compute foreign length comps ----
  fsh_len_f %>% 
    # get haul level length frequency and numbers caught
    tidytable::summarise(freq = sum(freq),
                         n1 = min(numb),
                         .by = c(year, gear, haul_join, length)) %>% 
    # get total lengths and numbers by year and gear
    tidytable::left_join(fsh_len_f %>% 
                           tidytable::summarise(hfreq = sum(freq),
                                                n1 = min(numb),
                                                .by = c(year, gear, haul_join)) %>% 
                           tidytable::mutate(tn = sum(n1),
                                             .by = c(year, gear))) %>% 
    # compute weighted haul-level length comp
    tidytable::mutate(prop = ((freq / hfreq) * n1) / tn) %>% 
    # sum weighted length comps to year and gear
    tidytable::summarise(prop = sum(prop), .by = c(year, gear, length)) -> f_spcomp
  
  # compute domestic length comps ----
  fsh_len_d %>% 
    # define unique haul id
    tidytable::mutate(haul1 = paste(cruise, permit, haul, sep = "_")) %>% 
    # get haul level length frequency and numbers caught
    tidytable::summarise(freq = sum(freq),
                         n1 = min(numb),
                         .by = c(year, gear, haul1, length)) %>% 
    # get total lengths and numbers by year and gear
    tidytable::left_join(fsh_len_d %>% 
                           tidytable::mutate(haul1 = paste(cruise, permit, haul, sep = "_")) %>% 
                           tidytable::summarise(hfreq = sum(freq),
                                                n1 = min(numb),
                                                .by = c(year, gear, haul1)) %>% 
                           tidytable::mutate(tn = sum(n1),
                                             .by = c(year, gear))) %>% 
    # compute weighted haul-level length comp
    tidytable::mutate(prop = ((freq / hfreq) * n1) / tn) %>% 
    # sum weighted length comps to year and gear
    tidytable::summarise(prop = sum(prop), .by = c(year, gear, length)) -> d_spcomp
  
  # merge comps ----
  
  ## prep foreign ----
  # all combos of year, gear, length
  tidytable::expand_grid(year = sort(unique(f_spcomp$year)),
                         gear = unique(f_spcomp$gear),
                         length = seq(1,117,1)) %>% 
    # join foreign comps (and replace NAs)
    tidytable::left_join(f_spcomp %>% 
                           tidytable::mutate(length = case_when(length > 117 ~ 117, .default = length)) %>% 
                           tidytable::summarise(prop = sum(prop), .by = c(year, gear, length))) %>% 
    tidytable::mutate(prop = case_when(is.na(prop) ~ 0, .default = prop)) %>% 
    # remove year-gear combos with no length comps
    tidytable::mutate(yr_test = sum(prop), .by = c(year, gear)) %>% 
    tidytable::filter(yr_test > 0) %>% 
    tidytable::select(-yr_test) %>% 
    # both foreign and domestic lengths in trawl for 1987 and 1988, so, divide these comps by 2...
    tidytable::mutate(prop = case_when(year >= 1987 & gear == 'trawl' ~ prop / 2, .default = prop)) -> f_spcom
  
  ## prep domestic ----
  # all combos of year, gear, length
  tidytable::expand_grid(year = sort(unique(d_spcomp$year)),
                         gear = unique(d_spcomp$gear),
                         length = seq(1,117,1)) %>%
    # join domestic comps (and replace NAs)
    tidytable::left_join(d_spcomp %>% 
                           tidytable::mutate(length = case_when(length > 117 ~ 117, .default = length)) %>% 
                           tidytable::summarise(prop = sum(prop), .by = c(year, gear, length))) %>% 
    tidytable::mutate(prop = case_when(is.na(prop) ~ 0, .default = prop)) %>% 
    # remove year-gear combos with no length comps
    tidytable::mutate(yr_test = sum(prop), .by = c(year, gear)) %>% 
    tidytable::filter(yr_test > 0) %>% 
    tidytable::select(-yr_test) %>% 
    # both foreign and domestic lengths in trawl for 1987 and 1988, so, divide these comps by 2...
    tidytable::mutate(prop = case_when(year <= 1988 & gear == 'trawl' ~ prop / 2, .default = prop)) -> d_spcom
  
  ## combined comps ----
  f_spcom %>% 
    tidytable::bind_rows(d_spcom) %>% 
    tidytable::summarise(prop = sum(prop), .by = c(year, gear, length)) -> fsh_len_comp
  
  # bin length comps to desired bin width ----
  tidytable::expand_grid(year = sort(unique(fsh_len_comp$year)),
                         gear = sort(unique(fsh_len_comp$gear)),
                         length = bins) %>% 
    tidytable::left_join(fsh_len_comp %>% 
                           tidytable::left_join(get_bin(fsh_len_comp %>% 
                                                          tidytable::distinct(length), bins)) %>% 
                           tidytable::summarise(prop = sum(prop), .by = c(year, gear, new_length)) %>% 
                           tidytable::rename(length = new_length)) %>% 
    tidytable::mutate(prop = tidytable::replace_na(prop, 0)) %>% 
    # drop year/gear combos with no data
    tidytable::mutate(prop_test = sum(prop), .by = c(year, gear)) %>% 
    tidytable::filter(prop_test >  0) %>% 
    tidytable::select(-prop_test) %>% 
    # standardize length comps
    tidytable::mutate(prop_tot = sum(prop), .by = c(year, gear)) %>% 
    tidytable::mutate(lencomp = prop / prop_tot) %>% 
    tidytable::select(-prop, -prop_tot) -> fsh_lcomp
  
  
  # format for ss3 if desired ----
  if(isTRUE(ss3_frmt)){
    # hard-wired in season, etc for ss3 in ss3_args c(seas, gender, part)
    ss3_args = c(1, 0, 0)
    # get input sample size as number of hauls or 200, whichever is smaller
    fsh_len_f %>% 
      tidytable::summarise(hjn = length(unique(haul_join)), .by = c(year, gear)) %>% 
      tidytable::bind_rows(fsh_len_d %>% 
                             tidytable::summarise(hjn = length(unique(haul_join)), .by = c(year, gear))) %>%
      tidytable::summarise(nsamp = sum(hjn), .by = c(year, gear)) %>% 
      tidytable::mutate(nsamp = case_when(nsamp > 200 ~ 200, .default = nsamp)) -> nsamp
    # format data
    fsh_lcomp <- ss3_len_com_fsh(fsh_lcomp, ss3_args, nsamp)
  }
  
  fsh_lcomp
}

#' Get fishery catch length composition for goa pcod post-1991 (domestic fishery only)
#' Originally adapted/generalized from Steve Barbeaux' files for generating SS files for EBS/AI Greenland Turbot
#' Re-developed in 2024 by p. hulson from GET_LENGTH_BY_CATCH_GOA.r file
#' 
#' @param new_year current assessment year
#' @param bins specified length bins (default = NULL)
#' @param ss3_frmt whether to format comp data for ss3 data file (default = TRUE)
#' @param time time period (month/trimester) from which length comps are expanded (default = NULL)
#' 

get_fsh_len_post91 <- function(new_year = 9999,
                               bins = NULL,
                               ss3_frmt = TRUE){
  
  # expand length frequencies  ----
  lcomp_c <- expand_fsh_len(new_year)
  
  # compute comps ----
  tidytable::expand_grid(year = sort(unique(lcomp_c$year)),
                         gear = sort(unique(lcomp_c$gear)),
                         length = bins) %>% 
    # bin length comps to desired bin width
    tidytable::left_join(lcomp_c %>% 
                           tidytable::left_join(get_bin(lcomp_c %>% 
                                                          tidytable::distinct(length), bins)) %>% 
                           tidytable::summarise(prop = sum(prop), 
                                                .by = c(year, gear, new_length)) %>% 
                           tidytable::rename(length = new_length)) %>% 
    # replace 0's
    tidytable::mutate(prop = tidytable::replace_na(prop, 0)) %>% 
    # standardize length comps
    tidytable::mutate(prop_tot = sum(prop), .by = c(year, gear)) %>% 
    tidytable::mutate(lencomp = prop / prop_tot) %>% 
    tidytable::select(-prop, -prop_tot) -> fsh_lcomp
  
  # format for ss3 if desired ----
  if(isTRUE(ss3_frmt)){
    # hard-wired in season, etc for ss3 in ss3_args c(seas, gender, part)
    ss3_args = c(1, 0, 0)
    # get input sample size
    # federal data
    vroom::vroom(here::here(new_year, 'data', 'raw', 'fish_lfreq_domestic.csv'), 
                 progress = FALSE, 
                 show_col_types = FALSE) %>% 
      # filter to years post-1991
      tidytable::filter(year >= 1991) %>% 
      # unique cruise-permit-haul description
      tidytable::mutate(haul1 = paste(cruise, permit, haul, sep = "_")) %>% 
      # input sample size is number of hauls or 200, whichever is smaller (from federal data)
      tidytable::summarise(nsamp_f = length(unique(haul1)),
                           .by = c(year, gear)) -> nsamp_f
    # state data
    vroom::vroom(here::here(new_year, 'data', 'raw', 'fish_lfreq_state.csv'), 
                 progress = FALSE, 
                 show_col_types = FALSE) %>% 
      dplyr::rename_all(tolower) %>% 
      #filter to positive lengths
      tidytable::filter(length > 0) %>% 
      # define area, gear, plus length, trimester
      tidytable::mutate(area = trunc(area / 10) * 10, # truncate area to nearest 10 (e.g., 649 becomes 640)
                        gear1 = tidytable::case_when(gear == 91 ~ 'pot',
                                                     gear %in% c(5, 26, 61) ~ 'longline',
                                                     .default = 'trawl'), # define gears
                        trimester = tidytable::case_when(month %in% seq(5, 8) ~ 2,
                                                         month >= 9 ~ 3,
                                                         .default = 1)) %>% 
      tidytable::select(year, area, gear = gear1, trimester, sex, length, freq) %>% 
      tidytable::summarise(nsamp_s = round(sum(freq) / 50), .by = c(year, gear)) -> nsamp_s
    nsamp_f %>% 
      tidytable::full_join(nsamp_s) %>% 
      tidytable::mutate(nsamp_f = tidytable::replace_na(nsamp_f, 0),
                        nsamp_s = tidytable::replace_na(nsamp_s, 0)) %>% 
      tidytable::mutate(nsamp = nsamp_f + nsamp_s) %>%
      tidytable::mutate(nsamp = tidytable::case_when(nsamp > 200 ~ 200,
                                                     .default = nsamp)) %>% 
      tidytable::select(year, gear, nsamp) -> nsamp
    # format data
    fsh_lcomp <- ss3_len_com_fsh(fsh_lcomp, ss3_args, nsamp)
  }
  
  fsh_lcomp
}

#' Expand fishery length frequency for goa pcod post-1991
#' 
#' @param new_year current assessment year
#' 

expand_fsh_len <- function(new_year = 9999){
  
  ## length freq data ----
  ### federal ----
  fsh_len_f <- vroom::vroom(here::here(new_year, 'data', 'raw', 'fish_lfreq_domestic.csv'), 
                            progress = FALSE, 
                            show_col_types = FALSE) %>% 
    # filter to years post-1991
    tidytable::filter(year >= 1991) %>% 
    # unique cruise-permit-haul description
    tidytable::mutate(haul1 = paste(cruise, permit, haul, sep = "_")) %>% 
    # truncate area to nearest 10 (e.g., 649 becomes 640)
    tidytable::mutate(area = trunc(area / 10) * 10)
  
  ### state ----
  fsh_len_s <- vroom::vroom(here::here(new_year, 'data', 'raw', 'fish_lfreq_state.csv'), 
                            progress = FALSE, 
                            show_col_types = FALSE) %>% 
    dplyr::rename_all(tolower) %>% 
    #filter to positive lengths
    tidytable::filter(length > 0) %>% 
    # define area, gear, plus length, trimester
    tidytable::mutate(area = trunc(area / 10) * 10, # truncate area to nearest 10 (e.g., 649 becomes 640)
                      gear1 = tidytable::case_when(gear == 91 ~ 'pot',
                                                   gear %in% c(5, 26, 61) ~ 'longline',
                                                   .default = 'trawl'), # define gears
                      trimester = tidytable::case_when(month %in% seq(5, 8) ~ 2,
                                                       month >= 9 ~ 3,
                                                       .default = 1)) %>% 
    tidytable::select(year, area, gear = gear1, month, sex, length, freq)
  
  ## catch data ----
  vroom::vroom(here::here(new_year, 'data', 'raw', 'fish_catch_data.csv'), 
               progress = FALSE, 
               show_col_types = FALSE) %>%
    tidytable::mutate(month = lubridate::month(week_end_date),
                      trimester = tidytable::case_when(month <= 4 ~ 1,
                                                       month %in% c(5, 6, 7, 8) ~ 2,
                                                       month >= 9 ~ 3)) %>%
    # define gear type, and truncate area to nearest 10
    tidytable::mutate(gear = tidytable::case_when(fmp_gear %in% c('TRW', 'GLN', 'OTH') ~ 'trawl',
                                                  fmp_gear == 'POT' ~ 'pot',
                                                  fmp_gear %in% c('HAL', 'JIG') ~ 'longline'),
                      area = trunc(reporting_area_code / 10) * 10) %>%
    # compute proportion of annual catch by week-area-gear
    tidytable::summarise(tons = sum(weight_posted), .by = c(year, month, area, gear)) %>%
    tidytable::mutate(total = sum(tons), .by = year) %>%
    tidytable::mutate(catch_prop = tons / total) -> catch_p
  
  # compute comps: fed merged with state ----
  
  # start with federal length frequencies
  fsh_len_f %>% 
    # length frequency by month-area-gear
    tidytable::summarise(freq = sum(freq),
                         .by = c(year, month, area, gear, length)) %>% 
    # join state comps
    tidytable::bind_rows(fsh_len_s %>% 
                           # length freq obs by month-area-gear
                           tidytable::summarise(freq = sum(freq), .by = c(year, month, area, gear, length))) %>% 
    # summarise combined comps to month-area-gear
    tidytable::summarise(freq = sum(freq),
                         .by = c(year, month, area, gear, length)) %>% 
    tidytable::mutate(tfreq = sum(freq), 
                      .by = c(year, month, area, gear)) %>% 
    # join number of total fish caught per month-area-gear (using fed observer data as proxy)
    tidytable::left_join(fsh_len_f %>% 
                           tidytable::summarise(n1 = min(numb),
                                                .by = c(year, month, area, gear, haul1)) %>% 
                           tidytable::summarise(n1 = sum(n1),
                                                .by = c(year, month, area, gear))) %>% 
    # if total fish caught missing for month-area-gear, use avg for year-gear
    tidytable::mutate(mean_n1 = mean(n1, na.rm = TRUE), .by = c(year, gear)) %>% 
    tidytable::mutate(n1 = tidytable::case_when(is.na(n1) ~ mean_n1,
                                                .default = n1)) %>% 
    tidytable::select(-mean_n1) %>% 
    # if total fish caught missing for year-gear, use avg for year
    tidytable::mutate(mean_n1 = mean(n1, na.rm = TRUE), .by = c(year)) %>%
    tidytable::mutate(n1 = tidytable::case_when(is.na(n1) ~ mean_n1,
                                                .default = n1)) %>% 
    tidytable::select(-mean_n1) %>% 
    # join number of total fish sampled over the year (using fed observer data as proxy)
    tidytable::left_join(fsh_len_f %>% 
                           tidytable::summarise(n1 = min(numb),
                                                .by = c(year, month, area, gear, haul1)) %>% 
                           tidytable::summarise(n2 = sum(n1),
                                                .by = year)) %>% 
    # expand the month-area-gear length composition by numbers observed across all hauls
    tidytable::mutate(prop = ((freq / tfreq) * n1) / n2) %>%
    # join length comp with catch proportion and compute catch weighted length comp by year, month, area, and gear
    tidytable::left_join(catch_p) %>% 
    tidytable::select(year, month, area, gear, length, prop, catch_prop) %>% 
    # if total catch_prop missing for month-area-gear, use avg for year-gear
    tidytable::mutate(mean_cp = mean(catch_prop, na.rm = TRUE), .by = c(year, gear)) %>% 
    tidytable::mutate(catch_prop = tidytable::case_when(is.na(catch_prop) ~ mean_cp,
                                                        .default = catch_prop)) %>% 
    tidytable::select(-mean_cp) %>% 
    # weight length comp by proportion of catch
    tidytable::mutate(prop1 = prop * catch_prop) %>% 
    # summarise to year-gear level
    tidytable::summarise(prop = sum(prop1), 
                         .by = c(year, gear, length)) -> lcomp_c
  
  lcomp_c
}

#' Get fishery catch age composition
#' Originally adapted/generalized from Steve Barbeaux' files for generating SS files for EBS/AI Greenland Turbot
#' Re-developed in 2024 by p. hulson from FISH_AGE_COMP.r file
#' 
#' @param new_year current assessment year
#' @param st_yr start year for age data (default = 2007)
#' @param max_age max age for age comps (i.e., plus group, default = 10)
#' @param ss3_frmt whether to format comp data for ss3 data file (default = TRUE)
#' @param fit whether to fit age comps in model (default = FALSE)
#' @param time time period (month/trimester) from which length comps are expanded (default = NULL)
#' 

get_fsh_age_new <- function(new_year = 9999,
                            st_yr = 2007,
                            max_age = 10,
                            ss3_frmt = TRUE,
                            fit = FALSE){
  
  # get data ----
  ## expanded length comps ----
  fsh_len_exp <- expand_fsh_len(new_year) %>% 
    # filter to years post-2007 (as default)
    tidytable::filter(year > st_yr)
  
  ## age data ----
  fsh_age <- vroom::vroom(here::here(new_year, 'data', 'raw', 'fish_age_domestic.csv'), 
                          progress = FALSE, 
                          show_col_types = FALSE) %>% 
    # filter to years post-2007 (as default)
    tidytable::filter(year > st_yr)
  
  # compute age comps (with ALK) ----
  # get age comp for all combos of year-gear-age
  tidytable::expand_grid(year = sort(unique(fsh_age$year)),
                         gear = unique(fsh_age$gear),
                         age = seq(1, 20)) %>% 
    tidytable::left_join(fsh_age %>% 
                           tidytable::select(year, gear, length, age) %>% 
                           # compute age-length key
                           tidytable::summarise(count = .N, .by = c(year, gear, length, age)) %>% 
                           tidytable::mutate(tot = sum(count), .by = c(year, gear, length)) %>% 
                           tidytable::mutate(alk = count / tot) %>% 
                           tidytable::select(year, gear, length, age, alk) %>% 
                           # join length comps and compute expanded age comps
                           tidytable::left_join(fsh_len_exp) %>% 
                           tidytable::drop_na() %>% 
                           tidytable::mutate(acomp = alk * prop) %>% 
                           tidytable::summarise(acomp1 = sum(acomp), .by = c(year, gear, age))) %>% 
    tidytable::mutate(acomp1 = tidytable::replace_na(acomp1, 0)) %>% 
    # standardize age comps
    tidytable::mutate(tot = sum(acomp1), .by = c(year, gear)) %>% 
    tidytable::mutate(agecomp = acomp1 / tot) %>% 
    tidytable::select(year, gear, age, agecomp) %>% 
    tidytable::drop_na() -> fsh_acomp
  
  # format for ss3 if desired
  if(isTRUE(ss3_frmt)){
    # hard-wired in season, etc for ss3 in ss3_args c(seas, gender, part, ageerr, lgin_lo, lgin_hi)
    ss3_args = c(1, 0, 0, 1, -1, -1)
    # get input sample size as number of hauls + port samples or 200, whichever is smaller
    fsh_age %>% 
      tidytable::filter(haul_join != 'H') %>% 
      tidytable::summarise(nsamp = length(unique(haul_join)),
                           .by = c(year, gear)) %>% 
      tidytable::bind_rows(fsh_age %>% 
                             tidytable::filter(port_join != 'P') %>% 
                             tidytable::summarise(nsamp = length(unique(port_join)),
                                                  .by = c(year, gear))) %>% 
      tidytable::summarise(nsamp = sum(nsamp),
                           .by = c(year, gear)) %>% 
      tidytable::mutate(nsamp = case_when(nsamp > 200 ~ 200, .default = nsamp)) -> nsamp
    # format data
    fsh_acomp <- ss3_age_com_fsh(fsh_acomp, ss3_args, max_age, iss = TRUE, nsamp, fit)
  }
  
  fsh_acomp
  
}


#' Function to pull bottom trawl survey pop'n numbers for age composition
#' adapted/generalized from Steve Barbeaux' files for generating SS files for EBS/AI Greenland Turbot
#' Re-developed in 2024 by p. hulson to link to gap_products tables with dplyr::tbl()
#' 
#' @param new_year current assessment year
#' @param st_yr start year for age data (default = 2007)
#' @param max_age user defined maximum age (default = 10)
#' @param bins user-defined length bins (default = NULL)
#' @param ss3_frmt whether to format comp data for ss3 data file (default = TRUE)
#' 

get_fsh_caal <- function(new_year = 9999,
                         st_yr = 2007,
                         max_age = 10,
                         bins = NULL,
                         ss3_frmt = TRUE){
  
  # compute conditional age-at-length  ----
  fsh_age <- vroom::vroom(here::here(new_year, 'data', 'raw', 'fish_age_domestic.csv')) %>% 
    # filter to years post-2007 (as default)
    tidytable::filter(year >= st_yr)
  
  tidytable::expand_grid(year = sort(unique(fsh_age$year)),
                         gear = sort(unique(fsh_age$gear)),
                         length = bins) %>% 
    tidytable::left_join(fsh_age %>% 
                           tidytable::select(year, gear, length, age) %>% 
                           tidytable::left_join(get_bin(fsh_age %>% 
                                                          tidytable::distinct(length), bins)) %>% 
                           tidytable::select(year, gear, age, length = new_length)) %>% 
    tidytable::drop_na() %>%
    tidytable::mutate(age = tidytable::case_when(age > max_age ~ max_age,
                                                 .default = age)) %>% 
    tidytable::summarise(count = .N, .by = c(year, gear, length, age)) %>% 
    tidytable::mutate(tot = sum(count), .by = c(year, gear, length)) %>% 
    tidytable::mutate(caal = round(count / tot, digits = 5)) %>% 
    tidytable::select(year, gear, length, age, caal) -> fsh_caal
  
  # format for ss3 if desired ----
  if(isTRUE(ss3_frmt)){
    # get all combos of year-length-age with observations
    tidytable::expand_grid(year = sort(unique(fsh_age$year)),
                           gear = sort(unique(fsh_age$gear)),
                           length = bins,
                           age = seq(1, max_age)) %>% 
      tidytable::left_join(fsh_caal) %>% 
      tidytable::mutate(caal = replace_na(caal, 0)) %>% 
      tidytable::mutate(test = sum(caal), .by = c(year, gear, length)) %>% 
      tidytable::filter(test > 0) %>% 
      tidytable::select(-test) -> fsh_caal
    
    # get nsamp as sample size multiplied by 0.14
    tidytable::expand_grid(year = sort(unique(fsh_age$year)),
                           gear = sort(unique(fsh_age$gear)),
                           length = bins) %>% 
      tidytable::left_join(fsh_age %>% 
                             tidytable::select(year, gear, length, age) %>% 
                             tidytable::left_join(get_bin(fsh_age %>% 
                                                            tidytable::distinct(length), bins)) %>% 
                             tidytable::select(year, gear, age, length = new_length)) %>% 
      tidytable::drop_na() %>% 
      tidytable::summarise(count = .N, .by = c(year, gear, length)) %>% 
      tidytable::mutate(nsamp = count * 0.14) %>% 
      tidytable::mutate(fltsrv = tidytable::case_when(gear == 'trawl' ~ 1,
                                                      gear == 'longline' ~ 2,
                                                      gear == 'pot' ~ 3)) %>% 
      tidytable::select(-count, -gear) -> nsamp
    
    # hard-wired in season, etc for ss3 in ss3_args c(seas, gender, part, ageerr, lgin_lo, lgin_hi)
    ss3_args = c(1, 0, 0, 1)
    fsh_caal <- ss3_caal_fsh(data = fsh_caal,
                             ss3_args = ss3_args,
                             nsamp = nsamp)
  }
  
  fsh_caal
}
