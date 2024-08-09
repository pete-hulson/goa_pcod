#' Expand fishery length frequency for goa pcod post-1991
#' 
#' @param new_year current assessment year
#' 

expand_fsh_len <- function(new_year = 9999){
  
  # read and prep data ----
  
  ## length freq data ----
  ### federal ----
  fsh_len_f <- vroom::vroom(here::here(new_year, 'data', 'raw', 'fish_lfreq_domestic.csv')) %>% 
    # filter to years post-1991
    tidytable::filter(year >= 1991) %>% 
    # unique cruise-permit-haul description
    tidytable::mutate(haul1 = paste(cruise, permit, haul, sep = "_")) %>% 
    # truncate area to nearest 10 (e.g., 649 becomes 640)
    tidytable::mutate(area = trunc(area / 10) * 10)
  
  ### state ----
  fsh_len_s <- vroom::vroom(here::here(new_year, 'data', 'fish_lfreq_state.csv')) %>% 
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
    tidytable::select(year, area, gear = gear1, trimester, sex, length, freq)
  
  ## catch data ----
  vroom::vroom(here::here(new_year, 'data', 'raw', 'fsh_catch_data.csv')) %>%
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
    tidytable::summarise(tons = sum(weight_posted), .by = c(year, trimester, area, gear)) %>%
    tidytable::mutate(total = sum(tons), .by = year) %>%
    tidytable::mutate(catch_prop = tons / total) -> catch_p
  
  # compute comps: fed merged with state ----
  
  # start with federal length frequencies
  fsh_len_f %>% 
    # length frequency by trimester-area-gear
    tidytable::summarise(freq = sum(freq),
                         .by = c(year, trimester, area, gear, length)) %>% 
    # join state comps
    tidytable::bind_rows(fsh_len_s %>% 
                           # length freq obs by trimester-area-gear
                           tidytable::summarise(freq = sum(freq), .by = c(year, trimester, area, gear, length))) %>% 
    # summarise combined comps to trimester-area-gear
    tidytable::summarise(freq = sum(freq),
                         .by = c(year, trimester, area, gear, length)) %>% 
    tidytable::mutate(tfreq = sum(freq), 
                      .by = c(year, trimester, area, gear)) %>% 
    # join number of total fish sampled per trimester (using fed observer data as proxy)
    tidytable::left_join(fsh_len_f %>% 
                           tidytable::summarise(n1 = min(numb),
                                                .by = c(year, trimester, area, gear, haul1)) %>% 
                           tidytable::summarise(n2 = sum(n1),
                                                .by = c(year, trimester, area, gear))) %>% 
    # expand the trimester-area-gear length composition by numbers observed across all hauls
    tidytable::mutate(prop = (freq / tfreq) * n2) %>%
    # join length comp with catch proportion and compute catch weighted length comp by year, trimester, area, and gear
    tidytable::left_join(catch_p) %>% 
    # weight length comp by proportion of catch
    tidytable::mutate(prop1 = prop * catch_prop) %>% 
    tidytable::drop_na() %>% 
    # summarise to year-gear level
    tidytable::summarise(prop = sum(prop1), 
                         .by = c(year, gear, length)) -> lcomp_c

  lcomp_c
}

#' Get fishery catch length composition for goa pcod post-1991 (domestic fishery only)
#' Originally adapted/generalized from Steve Barbeaux' files for generating SS files for EBS/AI Greenland Turbot
#' Re-developed in 2024 by p. hulson from GET_LENGTH_BY_CATCH_GOA.r file
#' 
#' @param new_year current assessment year
#' @param bins specified length bins (default = NULL)
#' @param ss3_frmt whether to format comp data for ss3 data file (default = TRUE)
#' 

get_fsh_len_post91_new <- function(new_year = 9999,
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
    vroom::vroom(here::here(new_year, 'data', 'raw', 'fish_lfreq_domestic.csv')) %>% 
      # filter to years post-1991
      tidytable::filter(year >= 1991) %>% 
      # unique cruise-permit-haul description
      tidytable::mutate(haul1 = paste(cruise, permit, haul, sep = "_")) %>% 
      # input sample size is number of hauls or 200, whichever is smaller (from federal data)
      tidytable::summarise(nsamp = length(unique(haul1)),
                           .by = c(year, gear)) %>% 
      tidytable::mutate(nsamp = tidytable::case_when(nsamp > 200 ~ 200,
                                                     .default = nsamp)) -> nsamp
    # format data
    fsh_lcomp <- ss3_len_com_fsh(fsh_lcomp, ss3_args, nsamp)
  }
  
  fsh_lcomp
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
  fsh_age <- vroom::vroom(here::here(new_year, 'data', 'raw', 'fish_age_domestic.csv')) %>% 
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
