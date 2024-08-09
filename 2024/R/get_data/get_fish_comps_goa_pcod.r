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
    # drop year/gear combos with no data
    tidytable::drop_na() %>% 
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
#' @param fltr switch for whether to filter small number of length samples (default = TRUE)
#' @param bins specified length bins (default = NULL)
#' @param ss3_frmt whether to format comp data for ss3 data file (default = TRUE)
#' 

get_fsh_len_post91 <- function(new_year = 9999,
                               fltr = TRUE,
                               bins = NULL,
                               ss3_frmt = TRUE){
  
  # read and prep data ----
  
  ## length freq data ----
  ### federal ----
  fsh_len_f <- vroom::vroom(here::here(new_year, 'data', 'raw', 'fish_lfreq_domestic.csv')) %>% 
    # filter to years post-1991
    tidytable::filter(year >= 1991) %>% 
    # unique cruise-permit-haul description
    tidytable::mutate(haul1 = paste(cruise, permit, haul, sep = "_")) %>% 
    # get correct week end date
    tidytable::mutate(weekday = weekdays(hday),
                      wed = lubridate::ceiling_date(hday, "week"),
                      plus = tidytable::case_when(weekdays(hday) == "Sunday" ~ 6, 
                                                  .default = -1),
                      yr = lubridate::year(hday),
                      next_saturday = tidytable::case_when(yr >= 1993 ~ date(wed) + plus,
                                                           .default = date(wed)),
                      yr2 = lubridate::year(next_saturday),
                      wed2 = tidytable::case_when(yr != yr2 ~ date(paste0(yr, '-12-31')), 
                                                  .default = next_saturday)) %>% 
    tidytable::select(-weekday, - wed, -plus, -yr, -next_saturday, -yr2) %>% 
    tidytable::rename(wed = wed2) %>% 
    # truncate area to nearest 10 (e.g., 649 becomes 640)
    tidytable::mutate(area = trunc(area / 10) * 10)
  
  # filter to hauls with greater than 10 lengths
  if(isTRUE(fltr)){
    fsh_len_f %>% 
      # filter to >30 lengths per trimester, area, and gear
      tidytable::left_join(fsh_len_f %>% 
                             tidytable::summarise(tfreq = sum(freq), .by = c(haul_join, numb))) %>% 
      tidytable::filter(tfreq >= 10) %>% 
      tidytable::select(-tfreq) -> fsh_len_full_f
  } else{
    fsh_len_full_f <- fsh_len_f
  }
  
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
                      length = tidytable::case_when(length > 116 ~ 117,
                                                    .default = length), # set plus length to 117 cm
                      trimester = tidytable::case_when(month %in% seq(5, 8) ~ 2,
                                                       month >= 9 ~ 3,
                                                       .default = 1)) %>% 
    tidytable::select(year, area, gear = gear1, month, trimester, quarter, sex, length, freq)
  
  ## catch data ----
  vroom::vroom(here::here(new_year, 'data', 'raw', 'fsh_catch_data.csv')) %>%
    tidytable::mutate(month = lubridate::month(week_end_date),
                      trimester = tidytable::case_when(month <= 4 ~ 1,
                                                       month %in% c(5, 6, 7, 8) ~ 2,
                                                       month >= 9 ~ 3)) %>%
    # define gear type, and truncate area to nearest 10
    tidytable::mutate(wed = date(week_end_date),
                      gear = tidytable::case_when(fmp_gear %in% c('TRW', 'GLN', 'OTH') ~ 'trawl',
                                                  fmp_gear == 'POT' ~ 'pot',
                                                  fmp_gear %in% c('HAL', 'JIG') ~ 'longline'),
                      area = trunc(reporting_area_code / 10) * 10) %>%
    # compute proportion of annual catch by week-area-gear
    tidytable::summarise(tons = sum(weight_posted), .by = c(year, wed, trimester, area, gear)) %>%
    tidytable::mutate(total = sum(tons), .by = year) %>%
    tidytable::mutate(catch_prop = tons / total) -> catch_p

  # fill-in with state data ----
  
  ## federal catch weighted length comp ----
  
  # compute federal comps
  fsh_len_full_f %>% 
    # haul-level length frequency
    tidytable::summarise(freq = sum(freq),
                         .by = c(year, wed, trimester, area, haul1, gear, length)) %>% 
    # join number of total fish sampled per haul
    tidytable::left_join(fsh_len_full_f %>% 
                           tidytable::summarise(n1 = min(numb),
                                                hfreq = sum(freq),
                                                .by = c(year, wed, trimester, area, haul1, gear))) %>% 
    # join number of fish by year, week, area, and gear
    tidytable::left_join(fsh_len_full_f %>% 
                           tidytable::summarise(n1 = min(numb),
                                                hfreq = sum(freq),
                                                .by = c(year, wed, trimester, area, haul1, gear)) %>% 
                           tidytable::summarise(n2 = sum(n1),
                                                tfreq = sum(hfreq),
                                                .by = c(year, wed, trimester, area, gear))) %>% 
    # proportion of haul catch-at-length by gear-area-week observed catch
    # expand the haul length composition by haul catch, then divide by the gear-area-week total observed haul catches
    tidytable::mutate(prop = ((freq / hfreq) * n1) / n2) %>%
    # summarise to length composition by week-area-gear
    tidytable::summarise(prop = sum(prop),
                         .by = c(year, wed, trimester, area, gear, length)) %>% 
    # join length comp with catch proportion and compute catch weighted length comp by year, trimester, area, and gear
    tidytable::left_join(catch_p) %>% 
    # set plus length group to 117 cm
    tidytable::mutate(length = tidytable::case_when(length > 116 ~ 117,
                                                    .default = length)) %>% 
    # weight length comp by proportion of catch
    tidytable::mutate(prop1 = prop * catch_prop) %>% 
    tidytable::drop_na() %>% 
    tidytable::summarise(prop1 = sum(prop1), 
                         .by = c(year, trimester, area, gear, length)) -> lcomp_f

  # this was supposed to be in the original code, but wasn't actually included (tho it was written in there)
  # if(isTRUE(fltr)){
  #   lcomp_f %>% 
  #     # filter to >30 lengths per trimester, area, and gear
  #     tidytable::left_join(fsh_len_f %>% 
  #                            tidytable::summarise(tfreq = sum(freq), .by = c(year, trimester, area, gear))) %>% 
  #     tidytable::filter(tfreq >= 30) %>% 
  #     tidytable::select(-tfreq) -> lcomp_f
  # }

  # get grid of all possible combos of federal year-gear-area-trimester-length
  tidytable::expand_grid(year = sort(unique(lcomp_f$year)),
                         gear = unique(lcomp_f$gear),
                         area = unique(lcomp_f$area),
                         trimester = c(1:3),
                         length = seq(1,117,1)) %>% 
    # join weighted fish length comp
    tidytable::left_join(lcomp_f) %>% 
    tidytable::mutate(prop1 = tidytable::replace_na(prop1, 0)) %>% 
    tidytable::rename(prop = prop1) -> fsh_len_comp_f

  ## state catch weighted length comp ----
  
  # compute state comps
  fsh_len_s %>% 
    # length freq obs by trimester-area-gear
    tidytable::summarise(freq = sum(freq), .by = c(year, trimester, area, gear, length)) %>% 
    # join total length obs by trimester-area-gear
    tidytable::left_join(fsh_len_s %>% 
                           # length freq obs by trimester-area-gear
                           tidytable::summarise(total = sum(freq), .by = c(year, trimester, area, gear))) %>% 
    # compute state length comp by trimester, area, and gear
    tidytable::mutate(prop = freq / total) %>% 
    tidytable::select(-total, -freq) -> lcomp_s
  
  if(isTRUE(fltr)){
    lcomp_s %>% 
      # filter to >30 lengths per trimester, area, and gear
      tidytable::left_join(fsh_len_s %>% 
                             tidytable::summarise(sfreq = sum(freq), .by = c(year, trimester, area, gear))) %>% 
      tidytable::filter(sfreq >= 30) %>% 
      tidytable::select(-sfreq) -> lcomp_s
  }

  # get grid of all possible combos of state year-gear-area-trimester-length
  tidytable::expand_grid(year = sort(unique(fsh_len_s$year)),
                         gear = unique(fsh_len_s$gear),
                         area = unique(fsh_len_s$area),
                         trimester = c(1:3),
                         length = seq(1,117,1)) %>% 
    # join state length comps
    tidytable::left_join(lcomp_s) %>% 
    # join federal catch for trimester-area-gear
    tidytable::left_join(catch_p %>% 
                           tidytable::summarise(catch_prop = sum(catch_prop), .by = c(year, trimester, area, gear))) %>% 
    # compute catch weighted length comp
    tidytable::mutate(prop = tidytable::replace_na(prop, 0),
                      catch_prop = tidytable::replace_na(catch_prop, 0),
                      prop1 = prop * catch_prop) %>% 
    tidytable::summarise(prop = sum(prop1), .by = c(year, trimester, area, gear, length)) -> fsh_len_comp_s

  ## merge state and federal length frequencies ----
  # test which trimester-area-gear length freqs have more state than fed data
  fsh_len_full_f %>% 
    # get federal number of length obs by trimester-area-gear
    tidytable::summarise(tfreq = sum(freq), .by = c(year, trimester, area, gear)) %>% 
    # get state number of length obs by trimester-area-gear
    tidytable::full_join(fsh_len_s %>% 
                           tidytable::summarise(sfreq = sum(freq), .by = c(year, trimester, area, gear))) %>% 
    # set index for when state > federal lengths
    tidytable::mutate(tfreq = tidytable::replace_na(tfreq, 0),
                      sfreq = tidytable::replace_na(sfreq, 0),
                      state = tidytable::case_when(sfreq > tfreq ~ 1,
                                                   .default = 0)) %>% 
    # filter to state with greater than 30 lengths and fed with less
    tidytable::filter(tfreq < 30,
                      sfreq >= 30) %>% 
    tidytable::select(-sfreq, -tfreq) -> state_test 

  # join state test to fed lengths
  fsh_len_comp_f %>% 
    tidytable::left_join(state_test) %>% 
    tidytable::filter(is.na(state)) %>% 
    tidytable::left_join(fsh_len_full_f %>%
                           tidytable::summarise(nsamp = length(unique(haul_join)),
                                                .by = c(year, trimester, area, gear))) %>% 
    tidytable::mutate(state = tidytable::replace_na(state, 0),
                      nsamp = tidytable::replace_na(nsamp, 0)) %>% 
    # bind state data where fed data doesn't exist
    tidytable::bind_rows(fsh_len_comp_s %>% 
                           tidytable::left_join(state_test) %>% 
                           tidytable::filter(!is.na(state)) %>% 
                           tidytable::left_join(fsh_len_s %>% 
                                                  tidytable::summarise(tot = sum(freq), .by = c(year, trimester, area, gear)) %>% 
                                                  tidytable::drop_na() %>% 
                                                  tidytable::mutate(nsamp = round(tot / 50)) %>% 
                                                  tidytable::select(-tot))) %>% 
    tidytable::summarise(prop = sum(prop),
                         nsamp = sum(nsamp),
                         .by = c(year, gear, length)) -> fsh_len_comp
  
  # bin length comps to desired bin width ----
  tidytable::expand_grid(year = sort(unique(fsh_len_comp$year)),
                         gear = sort(unique(fsh_len_comp$gear)),
                         length = bins) %>% 
    tidytable::left_join(fsh_len_comp %>% 
                           tidytable::left_join(get_bin(fsh_len_comp %>% 
                                                          tidytable::distinct(length), bins)) %>% 
                           tidytable::summarise(prop = sum(prop), 
                                                .by = c(year, gear, new_length)) %>% 
                           tidytable::rename(length = new_length)) %>% 
    # replace NA's
    tidytable::mutate(prop = tidytable::replace_na(prop, 0)) %>% 
    # standardize length comps
    tidytable::mutate(prop_tot = sum(prop), .by = c(year, gear)) %>% 
    tidytable::mutate(lencomp = prop / prop_tot) %>% 
    tidytable::select(-prop, -prop_tot) -> fsh_lcomp
  
  
  # format for ss3 if desired ----
  if(isTRUE(ss3_frmt)){
    # hard-wired in season, etc for ss3 in ss3_args c(seas, gender, part)
    ss3_args = c(1, 0, 0)
    # get input sample size as number of hauls or 200, whichever is smaller
    nsamp <- fsh_len_comp %>% 
      tidytable::summarise(nsamp = mean(nsamp), .by = c(year, gear)) %>% 
      tidytable::mutate(nsamp = tidytable::case_when(nsamp > 200 ~ 200,
                                                     .default = nsamp))
    # format data
    fsh_lcomp <- ss3_len_com_fsh(fsh_lcomp, ss3_args, nsamp)
  }
  
  fsh_lcomp
}

#' Get fishery catch length composition for goa pcod age comp expansion
#' Originally adapted/generalized from Steve Barbeaux' files for generating SS files for EBS/AI Greenland Turbot
#' Re-developed in 2024 by p. hulson from FISH_AGE_COMP.r file (for first part that expands length comps by sex)
#' 
#' @param new_year current assessment year
#' @param fltr switch for whether to filter small number of length samples (default = TRUE)
#' @param by_sex switch for whether to compute sex-specific comps (default = TRUE)
#' 

get_fsh_len4age <- function(new_year = 9999,
                            fltr = TRUE,
                            by_sex = TRUE){
  
  # read and prep data ----
  
  ## length freq data ----
  ### federal ----
  fsh_len_f <- vroom::vroom(here::here(new_year, 'data', 'raw', 'fish_lfreq_domestic.csv')) %>% 
    # filter to years post-1991
    tidytable::filter(year >= 1991) %>% 
    # unique cruise-permit-haul description
    tidytable::mutate(haul1 = paste(cruise, permit, haul, sep = "_")) %>% 
    # get correct week end date
    tidytable::mutate(weekday = weekdays(hday),
                      wed = lubridate::ceiling_date(hday, "week"),
                      plus = tidytable::case_when(weekdays(hday) == "Sunday" ~ 6, 
                                                  .default = -1),
                      yr = lubridate::year(hday),
                      next_saturday = tidytable::case_when(yr >= 1993 ~ date(wed) + plus,
                                                           .default = date(wed)),
                      yr2 = lubridate::year(next_saturday),
                      wed2 = tidytable::case_when(yr != yr2 ~ date(paste0(yr, '-12-31')), 
                                                  .default = next_saturday)) %>% 
    tidytable::select(-weekday, - wed, -plus, -yr, -next_saturday, -yr2) %>% 
    tidytable::rename(wed = wed2) %>% 
    # truncate area to nearest 10 (e.g., 649 becomes 640)
    tidytable::mutate(area = trunc(area / 10) * 10)

  # filter to hauls with greater than 10 lengths
  if(isTRUE(fltr)){
    fsh_len_f %>% 
      # filter to >30 lengths per trimester, area, and gear
      tidytable::left_join(fsh_len_f %>% 
                             tidytable::summarise(tfreq = sum(freq), .by = c(haul_join, numb))) %>% 
      tidytable::filter(tfreq >= 10) %>% 
      tidytable::select(-tfreq) -> fsh_len_full_f
  } else{
    fsh_len_full_f <- fsh_len_f
  }
  
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
                      length = tidytable::case_when(length > 116 ~ 117,
                                                    .default = length), # set plus length to 117 cm
                      trimester = tidytable::case_when(month %in% seq(5, 8) ~ 2,
                                                       month >= 9 ~ 3,
                                                       .default = 1),
                      # this is the wrong, but historical way of assigning sex, correct way is commented out
                      sex = tidytable::case_when(sex == 9 ~ 'U',
                                                 sex == 2 ~ 'F',
                                                 .default = 'M')) %>% 
                      # sex = tidytable::case_when(sex == 1 ~ 'M',
                      #                            sex == 2 ~ 'F',
                      #                            .default = 'U')) %>% 
    tidytable::select(year, area, gear = gear1, month, trimester, quarter, sex, length, freq)
  
  ## catch data ----
  vroom::vroom(here::here(new_year, 'data', 'raw', 'fsh_catch_data.csv')) %>% 
    tidytable::mutate(month = lubridate::month(week_end_date),
                      trimester = tidytable::case_when(month <= 4 ~ 1,
                                                       month %in% c(5, 6, 7, 8) ~ 2,
                                                       month >= 9 ~ 3)) %>% 
    # define gear type, and truncate area to nearest 10
    tidytable::mutate(wed = date(week_end_date),
                      gear = tidytable::case_when(fmp_gear %in% c('TRW', 'GLN', 'OTH') ~ 'trawl',
                                                  fmp_gear == 'POT' ~ 'pot',
                                                  fmp_gear %in% c('HAL', 'JIG') ~ 'longline'),
                      area = trunc(reporting_area_code / 10) * 10) %>% 
    # compute proportion of annual catch by week-area-gear
    tidytable::summarise(tons = sum(weight_posted), .by = c(year, wed, trimester, area, gear)) %>% 
    tidytable::mutate(total = sum(tons), .by = year) %>% 
    tidytable::mutate(catch_prop = tons / total) -> catch_p

  # fill-in with state data ----
  if(isTRUE(by_sex)){
    # sex - specific comps ----
    ## federal comps ----
    fsh_len_full_f %>% 
      # haul-level length frequency
      tidytable::summarise(freq = sum(freq),
                           .by = c(year, wed, trimester, area, haul1, gear, sex, length)) %>% 
      # join number of total fish sampled per haul
      tidytable::left_join(fsh_len_full_f %>% 
                             tidytable::summarise(n1 = min(numb),
                                                  hfreq = sum(freq),
                                                  .by = c(year, wed, trimester, area, haul1, gear))) %>% 
      # join number of fish by year, week, area, and gear
      tidytable::left_join(fsh_len_full_f %>% 
                             tidytable::summarise(n1 = min(numb),
                                                  hfreq = sum(freq),
                                                  .by = c(year, wed, trimester, area, haul1, gear)) %>% 
                             tidytable::summarise(n2 = sum(n1),
                                                  tfreq = sum(hfreq),
                                                  .by = c(year, wed, trimester, area, gear))) %>% 
      # proportion of haul catch-at-length by gear-area-week observed catch
      # expand the haul length composition by haul catch, then divide by the gear-area-week total observed haul catches
      tidytable::mutate(prop = ((freq / hfreq) * n1) / n2) %>%
      # summarise to length composition by week-area-gear
      tidytable::summarise(prop = sum(prop),
                           .by = c(year, wed, trimester, area, gear, sex, length)) %>% 
      # join length comp with catch proportion and compute catch weighted length comp by year, trimester, area, and gear
      tidytable::left_join(catch_p) %>% 
      # set plus length group to 117 cm
      tidytable::mutate(length = tidytable::case_when(length > 116 ~ 117,
                                                      .default = length)) %>% 
      # weight length comp by proportion of catch
      tidytable::mutate(prop1 = prop * catch_prop) %>% 
      tidytable::drop_na() %>% 
      tidytable::summarise(prop1 = sum(prop1), 
                           .by = c(year, trimester, area, gear, sex, length)) -> lcomp_f
    
    # this was supposed to be in the original code, but wasn't actually included (tho it was written in there)
    if(isTRUE(fltr)){
      lcomp_f %>%
        # filter to >30 lengths per trimester, area, and gear
        tidytable::left_join(fsh_len_full_f %>%
                               tidytable::summarise(tfreq = sum(freq), .by = c(year, trimester, area, gear))) %>%
        tidytable::filter(tfreq >= 30) %>%
        tidytable::select(-tfreq) -> lcomp_f
    } else{
      lcomp_f <- lcomp_f
    }
    
    # get grid of all possible combos of federal year-gear-area-trimester-length
    tidytable::expand_grid(year = sort(unique(lcomp_f$year)),
                           gear = unique(lcomp_f$gear),
                           area = unique(lcomp_f$area),
                           trimester = c(1:3),
                           sex = c('M', 'F', 'U'),
                           length = seq(1,117,1)) %>% 
      # join weighted fish length comp
      tidytable::left_join(lcomp_f) %>% 
      tidytable::mutate(prop1 = tidytable::replace_na(prop1, 0)) %>% 
      tidytable::rename(prop = prop1) -> fsh_len_comp_f
    
    ## state comps ----
    fsh_len_s %>% 
      # length freq at trimester-area-gear
      tidytable::summarise(freq = sum(freq), .by = c(year, trimester, area, gear, sex, length)) %>% 
      # total lengths obs by trimester-area-gear
      tidytable::mutate(total = sum(freq), .by = c(year, trimester, area, gear)) %>% 
      # compute state length comp by trimester, area, and gear
      tidytable::mutate(prop = freq / total) %>% 
      tidytable::select(-total, -freq) -> lcomp_s
    
    # filtering data to year-trimester-area-gear with greater than 30 observations
    if(isTRUE(fltr)){
      lcomp_s %>% 
        tidytable::left_join(fsh_len_s %>% 
                               tidytable::summarise(sfreq = sum(freq), .by = c(year, trimester, area, gear)) %>% 
                               tidytable::drop_na()) %>% 
        tidytable::filter(sfreq >= 30) %>% 
        tidytable::select(-sfreq)  -> lcomp_s
    } else{
      lcomp_s <- lcomp_s
    }
    
    # get grid of all possible combos of state year-gear-area-trimester-length
    tidytable::expand_grid(year = sort(unique(fsh_len_s$year)),
                           trimester = c(1:3),
                           gear = unique(fsh_len_s$gear),
                           area = unique(fsh_len_s$area),
                           sex = c('M', 'F', 'U'),
                           length = seq(1,117,1)) %>% 
      # join state length frequencies
      tidytable::left_join(lcomp_s) %>% 
      # join federal catch for trimester-area-gear
      tidytable::left_join(catch_p %>% 
                             tidytable::summarise(catch_prop = sum(catch_prop), .by = c(year, trimester, area, gear))) %>% 
      # compute catch weighted length comp
      tidytable::mutate(prop = tidytable::replace_na(prop, 0),
                        catch_prop = tidytable::replace_na(catch_prop, 0),
                        prop1 = prop * catch_prop) %>% 
      tidytable::summarise(prop = sum(prop1), .by = c(year, trimester, area, gear, sex, length)) -> fsh_len_comp_s
    
  } else{
    # sex-combined comps ----
    ## federal comps ----
    fsh_len_full_f %>% 
      # haul-level length frequency
      tidytable::summarise(freq = sum(freq),
                           .by = c(year, wed, trimester, area, haul1, gear, length)) %>% 
      # join number of total fish sampled per haul
      tidytable::left_join(fsh_len_full_f %>% 
                             tidytable::summarise(n1 = min(numb),
                                                  hfreq = sum(freq),
                                                  .by = c(year, wed, trimester, area, haul1, gear))) %>% 
      # join number of fish by year, week, area, and gear
      tidytable::left_join(fsh_len_full_f %>% 
                             tidytable::summarise(n1 = min(numb),
                                                  hfreq = sum(freq),
                                                  .by = c(year, wed, trimester, area, haul1, gear)) %>% 
                             tidytable::summarise(n2 = sum(n1),
                                                  tfreq = sum(hfreq),
                                                  .by = c(year, wed, trimester, area, gear))) %>% 
      # proportion of haul catch-at-length by gear-area-week observed catch
      # expand the haul length composition by haul catch, then divide by the gear-area-week total observed haul catches
      tidytable::mutate(prop = ((freq / hfreq) * n1) / n2) %>%
      # summarise to length composition by week-area-gear
      tidytable::summarise(prop = sum(prop),
                           .by = c(year, wed, trimester, area, gear, length)) %>% 
      # join length comp with catch proportion and compute catch weighted length comp by year, trimester, area, and gear
      tidytable::left_join(catch_p) %>% 
      # set plus length group to 117 cm
      tidytable::mutate(length = tidytable::case_when(length > 116 ~ 117,
                                                      .default = length)) %>% 
      # weight length comp by proportion of catch
      tidytable::mutate(prop1 = prop * catch_prop) %>% 
      tidytable::drop_na() %>% 
      tidytable::summarise(prop1 = sum(prop1), 
                           .by = c(year, trimester, area, gear, length)) -> lcomp_f
    
    # this was supposed to be in the original code, but wasn't actually included (tho it was written in there)
    if(isTRUE(fltr)){
      lcomp_f %>%
        # filter to >30 lengths per trimester, area, and gear
        tidytable::left_join(fsh_len_full_f %>%
                               tidytable::summarise(tfreq = sum(freq), .by = c(year, trimester, area, gear))) %>%
        tidytable::filter(tfreq >= 30) %>%
        tidytable::select(-tfreq) -> lcomp_f
    }
    
    # get grid of all possible combos of federal year-gear-area-trimester-length
    tidytable::expand_grid(year = sort(unique(lcomp_f$year)),
                           gear = unique(lcomp_f$gear),
                           area = unique(lcomp_f$area),
                           trimester = c(1:3),
                           length = seq(1,117,1)) %>% 
      # join weighted fish length comp
      tidytable::left_join(lcomp_f) %>% 
      tidytable::mutate(prop1 = tidytable::replace_na(prop1, 0)) %>% 
      tidytable::rename(prop = prop1) -> fsh_len_comp_f
    
    ## state comps ----
    fsh_len_s %>% 
      # length freq at trimester-area-gear
      tidytable::summarise(freq = sum(freq), .by = c(year, trimester, area, gear, length)) %>% 
      # total lengths obs by trimester-area-gear
      tidytable::mutate(total = sum(freq), .by = c(year, trimester, area, gear)) %>% 
      # compute state length comp by trimester, area, and gear
      tidytable::mutate(prop = freq / total) %>% 
      tidytable::select(-total, -freq) -> lcomp_s
    
    ## filtering data to year-trimester-area-gear with greater than 30 observations
    if(isTRUE(fltr)){
      lcomp_s %>% 
        tidytable::left_join(fsh_len_s %>% 
                               tidytable::summarise(sfreq = sum(freq), .by = c(year, trimester, area, gear)) %>% 
                               tidytable::drop_na()) %>% 
        tidytable::filter(sfreq >= 30) %>% 
        tidytable::select(-sfreq)  -> lcomp_s
    }
    
    # get grid of all possible combos of state year-gear-area-trimester-length
    tidytable::expand_grid(year = sort(unique(fsh_len_s$year)),
                           trimester = c(1:3),
                           gear = unique(fsh_len_s$gear),
                           area = unique(fsh_len_s$area),
                           length = seq(1,117,1)) %>% 
      # join state length frequencies
      tidytable::left_join(lcomp_s) %>% 
      # join federal catch for trimester-area-gear
      tidytable::left_join(catch_p %>% 
                             tidytable::summarise(catch_prop = sum(catch_prop), .by = c(year, trimester, area, gear))) %>% 
      # compute catch weighted length comp
      tidytable::mutate(prop = tidytable::replace_na(prop, 0),
                        catch_prop = tidytable::replace_na(catch_prop, 0),
                        prop1 = prop * catch_prop) %>% 
      tidytable::summarise(prop = sum(prop1), .by = c(year, trimester, area, gear, length)) -> fsh_len_comp_s
    
  }

  # merge state and federal length frequencies
  # test which trimester-area-gear length freqs have more state than fed data
  fsh_len_full_f %>% 
    # get federal number of length obs by trimester-area-gear
    tidytable::summarise(tfreq = sum(freq), .by = c(year, trimester, area, gear)) %>% 
    # get state number of length obs by trimester-area-gear
    tidytable::full_join(fsh_len_s %>% 
                           tidytable::summarise(sfreq = sum(freq), .by = c(year, trimester, area, gear))) %>% 
    # set index for when state > federal lengths
    tidytable::mutate(tfreq = tidytable::replace_na(tfreq, 0),
                      sfreq = tidytable::replace_na(sfreq, 0),
                      state = tidytable::case_when(sfreq > tfreq ~ 1,
                                                   .default = 0)) %>% 
    # filter to state with greater than 30 lengths and fed with less
    tidytable::filter(tfreq < 30,
                      sfreq >= 30) %>% 
    tidytable::select(-sfreq, -tfreq) -> state_test 

  # join state test to fed lengths
  # sex-specific comps
  if(isTRUE(by_sex)){
    fsh_len_comp_f %>% 
      tidytable::left_join(state_test) %>% 
      tidytable::filter(is.na(state)) %>% 
      tidytable::mutate(state = tidytable::replace_na(state, 0)) %>% 
      # bind state data where fed data doesn't exist
      tidytable::bind_rows(fsh_len_comp_s %>% 
                             tidytable::left_join(state_test) %>% 
                             tidytable::filter(!is.na(state))) %>% 
      tidytable::summarise(prop = sum(prop),
                           .by = c(year, gear, sex, length)) -> fsh_len_comp
  } else{
    # sex-combined comps
    fsh_len_comp_f %>% 
      tidytable::left_join(state_test) %>% 
      tidytable::filter(is.na(state)) %>% 
      tidytable::mutate(state = tidytable::replace_na(state, 0)) %>% 
      # bind state data where fed data doesn't exist
      tidytable::bind_rows(fsh_len_comp_s %>% 
                             tidytable::left_join(state_test) %>% 
                             tidytable::filter(!is.na(state))) %>% 
      tidytable::summarise(prop = sum(prop),
                           .by = c(year, gear, length)) -> fsh_len_comp
  }
  
  # join nsamp correctly
  if(isTRUE(fltr)){
    fsh_len_comp %>% 
      tidytable::left_join(fsh_len_full_f %>%
                             # filter to >30 lengths per trimester, area, and gear
                             tidytable::left_join(fsh_len_full_f %>%
                                                    tidytable::summarise(tfreq = sum(freq), .by = c(year, trimester, area, gear))) %>%
                             tidytable::filter(tfreq >= 30) %>%
                             tidytable::select(-tfreq) %>% 
                             tidytable::summarise(nsamp = length(unique(haul_join)),
                                                  .by = c(year, trimester, area, gear)) %>% 
                             tidytable::bind_rows(fsh_len_s %>%
                                                    tidytable::left_join(fsh_len_s %>%
                                                                           tidytable::summarise(sfreq = sum(freq), .by = c(year, trimester, area, gear)) %>%
                                                                           tidytable::drop_na()) %>%
                                                    tidytable::filter(sfreq >= 30) %>%
                                                    tidytable::select(-sfreq) %>% 
                                                    tidytable::summarise(tot = sum(freq), .by = c(year, trimester, area, gear)) %>% 
                                                    tidytable::drop_na() %>% 
                                                    tidytable::mutate(nsamp = round(tot / 50)) %>% 
                                                    tidytable::select(-tot) %>% 
                                                    tidytable::left_join(state_test) %>% 
                                                    tidytable::filter(!is.na(state)) %>% 
                                                    tidytable::select(-state)) %>% 
                             tidytable::summarise(nsamp = sum(nsamp), .by = c(year, gear))) -> fsh_len_comp
  } else{
    fsh_len_comp %>% 
      tidytable::left_join(fsh_len_full_f %>%
                             tidytable::summarise(nsamp = length(unique(haul_join)),
                                                  .by = c(year, trimester, area, gear)) %>% 
                             tidytable::bind_rows(fsh_len_s %>%
                                                    tidytable::summarise(tot = sum(freq), .by = c(year, trimester, area, gear)) %>% 
                                                    tidytable::drop_na() %>% 
                                                    tidytable::mutate(nsamp = round(tot / 50)) %>% 
                                                    tidytable::select(-tot) %>% 
                                                    tidytable::left_join(state_test) %>% 
                                                    tidytable::filter(!is.na(state)) %>% 
                                                    tidytable::select(-state)) %>% 
                             tidytable::summarise(nsamp = sum(nsamp), .by = c(year, gear))) -> fsh_len_comp
  }

  fsh_len_comp

}

#' Get fishery catch age composition
#' Originally adapted/generalized from Steve Barbeaux' files for generating SS files for EBS/AI Greenland Turbot
#' Re-developed in 2024 by p. hulson from FISH_AGE_COMP.r file
#' 
#' @param new_year current assessment year
#' @param st_yr start year for age data (default = 2007)
#' @param max_age max age for age comps (i.e., plus group, default = 10)
#' @param fltr switch for whether to filter length samples (default = TRUE)
#' @param add_a1 option to define smallest lengths in data as age-1 (default = TRUE)
#' @param use_FSA switch for whether to use FSA package to compute age-length key or to do it manually (default = TRUE)
#' @param iters when using FSA, option to have replicates of age-length key (default = 1)
#' @param by_sex switch for whether to compute sex-specific comps (default = TRUE)
#' @param ss3_frmt whether to format comp data for ss3 data file (default = TRUE)
#' @param fit whether to fit age comps in model (default = FALSE)
#' 

get_fsh_age <- function(new_year = 9999,
                        st_yr = 2007,
                        max_age = 10,
                        fltr = TRUE,
                        add_a1 = TRUE,
                        use_FSA = TRUE,
                        iters = 1,
                        by_sex = TRUE,
                        ss3_frmt = TRUE,
                        fit = FALSE){
  
  # get data ----
  ## expanded length comps ----
  fsh_len_exp <- get_fsh_len4age(new_year,
                                 fltr,
                                 by_sex) %>% 
    tidytable::mutate(freq = prop * 10000) %>% 
    # filter to years post-2007 (as default)
    tidytable::filter(year > st_yr)
  
  ## age data ----
  fsh_age <- vroom::vroom(here::here(new_year, 'data', 'raw', 'fish_age_domestic.csv')) %>% 
    # filter to years post-2007 (as default)
    tidytable::filter(year > st_yr)
  
  # compute age comps ----
  ## using FSA package ----
  if(isTRUE(use_FSA)){
    # sex-specific comps
    if(isTRUE(by_sex)){
      # prep age and length data
      fsh_age %>% 
        tidytable::mutate(age = tidytable::case_when(age > 20 ~ 20,
                                                     .default = age)) %>% 
        tidytable::select(year, gear, sex, age, tl = length) -> adata_alk
      
      fsh_len_exp %>% 
        tidytable::select(year, gear, sex, tl = length, freq) %>% 
        tidytable::mutate(freq = floor(freq)) %>% 
        tidytable::uncount(freq) %>% 
        tidytable::mutate(age = NA) %>% 
        tidytable::select(year, gear, sex, age, tl) -> ldata_alk
      
      # define smallest lengths as age 1, or not
      if(isTRUE(add_a1)){
        adata_alk %>% 
          tidytable::bind_rows(ldata_alk %>% 
                                 tidytable::summarise(tl = min(tl), .by = c(year, gear, sex)) %>% 
                                 tidytable::bind_rows(adata_alk %>% 
                                                        tidytable::summarise(tl = min(tl) - 1, .by = c(year, gear, sex))) %>% 
                                 tidytable::mutate(age = 1)) %>% 
          FSA::lencat(x = ~tl, data = ., startcat = 5, w = 5) -> adata_alk
      } else{
        adata_alk %>% 
          FSA::lencat(x = ~tl, data = ., startcat = 5, w = 5) -> adata_alk
      }
      
      # get ages for unobserved lengths with FSA package
      combos <- fsh_age %>% 
        tidytable::select(year, gear, sex) %>% 
        dplyr::distinct() %>% 
        tidytable::filter(sex != 'U')
      
      length_age_list <- purrr::map(1:iters, ~purrr::map(1:nrow(combos), ~FSA::alkIndivAge(prop.table(table(subset(adata_alk$LCat, 
                                                                                                                   adata_alk$year == combos$year[.] & 
                                                                                                                     adata_alk$gear == combos$gear[.] & 
                                                                                                                     adata_alk$sex == combos$sex[.]),
                                                                                                            subset(adata_alk$age, 
                                                                                                                   adata_alk$year == combos$year[.] & 
                                                                                                                     adata_alk$gear == combos$gear[.] & 
                                                                                                                     adata_alk$sex == combos$sex[.])), 
                                                                                                      margin = 1), 
                                                                                           formula = ~tl, 
                                                                                           data = subset(ldata_alk, 
                                                                                                         ldata_alk$year == combos$year[.] & 
                                                                                                           ldata_alk$gear == combos$gear[.] & 
                                                                                                           ldata_alk$sex == combos$sex[.]))),
                                    .progress = list(type = "iterator", 
                                                     format = "Resampling {cli::pb_bar} {cli::pb_percent}",
                                                     clear = TRUE))
      
      
      purrr::map(1:iters, ~tidytable::map_df(length_age_list[[.]], ~as.data.frame(.x), .id = "combo")) %>% 
        tidytable::map_df(., ~as.data.frame(.x), .id = "iter") %>% 
        select(iter, year, gear, sex, age, length = tl) %>% 
        tidytable::summarise(freq = .N, .by = c(iter, year, gear, age)) %>% 
        tidytable::summarise(freq = mean(freq), .by = c(year, gear, age)) -> length_age
      
      # get age comp for all combos of year-gear-age
      tidytable::expand_grid(year = sort(unique(fsh_age$year)),
                             gear = unique(fsh_age$gear),
                             age = seq(1, 20)) %>% 
        tidytable::left_join(length_age) %>% 
        tidytable::mutate(freq = tidytable::replace_na(freq, 0)) %>% 
        tidytable::mutate(total = sum(freq), .by = c(year, gear)) %>% 
        tidytable::mutate(agecomp = freq / total) %>% 
        tidytable::select(-total, -freq) %>% 
        tidytable::drop_na() -> fsh_acomp
    } else{
      # sex-aggregated comps
      # prep age and length data
      fsh_age %>% 
        tidytable::mutate(age = tidytable::case_when(age > 20 ~ 20,
                                                     .default = age)) %>% 
        tidytable::select(year, gear, age, tl = length) -> adata_alk
      
      fsh_len_exp %>% 
        tidytable::select(year, gear, tl = length, freq) %>% 
        tidytable::mutate(freq = floor(freq)) %>% 
        tidytable::uncount(freq) %>% 
        tidytable::mutate(age = NA) %>% 
        tidytable::select(year, gear, age, tl) -> ldata_alk
      
      # define smallest lengths as age 1, or not
      if(isTRUE(add_a1)){
        adata_alk %>% 
          tidytable::bind_rows(ldata_alk %>% 
                                 tidytable::summarise(tl = min(tl), .by = c(year, gear)) %>% 
                                 tidytable::bind_rows(adata_alk %>% 
                                                        tidytable::summarise(tl = min(tl) - 1, .by = c(year, gear))) %>% 
                                 tidytable::mutate(age = 1)) %>% 
          FSA::lencat(x = ~tl, data = ., startcat = 5, w = 5) -> adata_alk
      } else{
        adata_alk %>% 
          FSA::lencat(x = ~tl, data = ., startcat = 5, w = 5) -> adata_alk
      }
      
      # get ages for unobserved lengths with FSA package
      combos <- fsh_age %>% 
        tidytable::select(year, gear) %>% 
        dplyr::distinct()
      
      length_age <- purrr::map(1:nrow(combos), ~FSA::alkIndivAge(prop.table(table(subset(adata_alk$LCat, 
                                                                                         adata_alk$year == combos$year[.] & 
                                                                                           adata_alk$gear == combos$gear[.]),
                                                                                  subset(adata_alk$age, 
                                                                                         adata_alk$year == combos$year[.] & 
                                                                                           adata_alk$gear == combos$gear[.])), 
                                                                            margin = 1), 
                                                                 formula = ~tl, 
                                                                 data = subset(ldata_alk, 
                                                                               ldata_alk$year == combos$year[.] & 
                                                                                 ldata_alk$gear == combos$gear[.]))) %>% 
        tidytable::map_df(., ~as.data.frame(.x), .id = "combo") %>% 
        select(year, gear, age, length = tl)
      
      # get age comp for all combos of year-gear-age
      tidytable::expand_grid(year = sort(unique(fsh_age$year)),
                             gear = unique(fsh_age$gear),
                             age = seq(1, 20)) %>% 
        tidytable::left_join(length_age %>% 
                               tidytable::summarise(freq = length(length), .by = c(year, gear, age))) %>% 
        tidytable::mutate(freq = tidytable::replace_na(freq, 0)) %>% 
        tidytable::mutate(total = sum(freq), .by = c(year, gear)) %>% 
        tidytable::mutate(agecomp = freq / total) %>% 
        tidytable::select(-total, -freq) %>% 
        tidytable::drop_na() -> fsh_acomp
    }
  } else{
    ## with age-length key (note: only for sex-combined comps) ----
    
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
                             tidytable::left_join(fsh_len_exp %>% 
                                                    tidytable::select(year, gear, length, prop)) %>% 
                             tidytable::drop_na() %>% 
                             tidytable::mutate(acomp = alk * prop) %>% 
                             tidytable::summarise(acomp1 = sum(acomp), .by = c(year, gear, age))) %>% 
      tidytable::mutate(acomp1 = tidytable::replace_na(acomp1, 0)) %>% 
      # standardize age comps
      tidytable::mutate(tot = sum(acomp1), .by = c(year, gear)) %>% 
      tidytable::mutate(agecomp = acomp1 / tot) %>% 
      tidytable::select(year, gear, age, agecomp) %>% 
      tidytable::drop_na() -> fsh_acomp
  }
  
  # format for ss3 if desired
  if(isTRUE(ss3_frmt)){
    # hard-wired in season, etc for ss3 in ss3_args c(seas, gender, part, ageerr, lgin_lo, lgin_hi)
    ss3_args = c(1, 0, 0, 1, -1, -1)
    # get input sample size as number of hauls or 200, whichever is smaller
    fsh_len_exp %>% 
      tidytable::summarise(nsamp = mean(nsamp), .by = c(year, gear)) %>% 
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
