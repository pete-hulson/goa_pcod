#' Function to pull bottom trawl survey pop'n numbers for length composition
#' adapted/generalized from Steve Barbeaux' files for generating SS files for EBS/AI Greenland Turbot
#' Re-developed in 2024 by p. hulson to link to gap_products tables with dplyr::tbl()
#' 
#' @param new_year current assessment year
#' @param bins user defined length bins (min/max for the length bin, default = NULL)
#' @param ss3_frmt whether to format comp data for ss3 data file (default = TRUE)
#' @param iss test for whether input sample size time-dependent (TRUE) or constant (FALSE)
#' @param bin_iss value for special case of iss based on bin size (e.g., 'bin2', default = NULL)
#' @param nsamp value or vector for input sample size (default = 100)
#' 

get_twl_srvy_lcomp <- function(new_year = 9999,
                               bins = NULL,
                               ss3_frmt = TRUE,
                               iss = FALSE,
                               bin_iss = NULL,
                               nsamp = 100){
  
  # compute comps ----
  ts_lpop <- vroom::vroom(here::here(new_year, 'data', 'raw', 'twl_srvy_lpop.csv'), 
                          progress = FALSE, 
                          show_col_types = FALSE)
  
  # bin length comps to desired bin width
  tidytable::expand_grid(year = sort(unique(ts_lpop$year)),
                         length = bins) %>% 
    tidytable::left_join(ts_lpop %>% 
                           tidytable::summarise(popn = sum(num), .by = c(year, length)) %>% 
                           tidytable::left_join(get_bin(ts_lpop %>% 
                                                          tidytable::summarise(popn = sum(num), 
                                                                               .by = c(year, length)) %>% 
                                                          distinct(length), bins)) %>% 
                           tidytable::summarise(popn = sum(popn), .by = c(year, new_length)) %>% 
                           tidytable::rename(length = new_length)) %>% 
    tidytable::mutate(popn = replace_na(popn, 0)) -> ts_lpop_bin

  # compute comps
  ts_lpop_bin %>% 
    tidytable::mutate(lencomp = popn / sum(popn), .by = year) %>% 
    tidytable::select(-popn) -> ts_lcomp
  
  # format for ss3 if desired ----
  if(isTRUE(ss3_frmt)){
    # hard-wired in season, etc for ss3 in ss3_args c(seas, fltsrv, gender, part)
    ss3_args = c(7, 4, 0, 0)
    # define iss
    if(isTRUE(iss)){
      if(is.null(bin_iss)){
        nsamp <- afscISS::get_ISS(species = 21720,
                                  region = 'goa',
                                  comp = 'length',
                                  sex_cat = 4) %>% 
          tidytable::select(year, nsamp = iss)
      } else{
        nsamp <- afscISS::get_ISS(species = 21720,
                                  region = 'goa',
                                  comp = 'length',
                                  sex_cat = 4,
                                  spec_case = bin_iss) %>% 
          tidytable::select(year, nsamp = iss)
      }
    } else{
      nsamp <- 100
    }
    # get ss3 data
    ts_lcomp <- ss3_len_com(data = ts_lcomp,
                            ss3_args,
                            iss,
                            nsamp)
  }
  ts_lcomp
}

#' Function to pull longline survey length composition
#' adapted/generalized from Steve Barbeaux' files for generating SS files
#' Re-developed in 2024 by p. hulson to link to afscdata package
#' 
#' @param new_year current assessment year
#' @param bins user defined length bins (min/max for the length bin, default = NULL)
#' @param ss3_frmt whether to format comp data for ss3 data file (default = TRUE)
#' @param iss test for whether input sample size time-dependent (TRUE) or constant (FALSE)
#' @param nsamp value or vector for input sample size (default = 100)
#' 

get_ll_srvy_lcomp <- function(new_year = 9999,
                              bins = NULL,
                              ss3_frmt = TRUE,
                              iss = FALSE,
                              nsamp = 100){
  
  # compute comps ----
  
  # read in longline survey data and filter to 1990 on
  lls_lpop <- vroom::vroom(here::here(new_year, "data", "raw", "lls_rpn_length_data.csv"), 
                           progress = FALSE, 
                           show_col_types = FALSE) %>% 
    tidytable::filter(year >= 1990)
  
  # bin length comps to desired bin width
  tidytable::expand_grid(year = sort(unique(lls_lpop$year)),
                         length = bins) %>% 
    tidytable::left_join(lls_lpop %>% 
                           tidytable::filter(length > 0) %>% 
                           tidytable::summarise(popn = sum(rpn), .by = c(year, length)) %>% 
                           tidytable::left_join(get_bin(lls_lpop %>% 
                                                          tidytable::filter(length > 0) %>% 
                                                          tidytable::summarise(popn = sum(rpn), .by = c(year, length)) %>% 
                                                          distinct(length), bins)) %>% 
                           tidytable::summarise(popn = sum(popn), .by = c(year, new_length)) %>% 
                           tidytable::rename(length = new_length)) %>% 
    tidytable::mutate(popn = replace_na(popn, 0)) -> lls_lpop_bin
  
  # compute comps
  lls_lpop_bin %>% 
    tidytable::mutate(lencomp = popn / sum(popn), .by = year) %>% 
    tidytable::select(-popn) -> lls_lcomp
  
  # format for ss3 if desired ----
  if(isTRUE(ss3_frmt)){
    # hard-wired in season, etc for ss3 in ss3_args c(seas, fltsrv, gender, part)
    ss3_args = c(7, 5, 0, 0)
    lls_lcomp <- ss3_len_com(data = lls_lcomp,
                             ss3_args = ss3_args,
                             iss = FALSE,
                             nsamp = 100)
  }
  
  lls_lcomp
}

#' Function to pull bottom trawl survey pop'n numbers for age composition
#' adapted/generalized from Steve Barbeaux' files for generating SS files for EBS/AI Greenland Turbot
#' Re-developed in 2024 by p. hulson to link to gap_products tables with dplyr::tbl()
#' 
#' @param new_year current assessment year
#' @param max_age user defined maximum age (default = 10)
#' @param ss3_frmt whether to format comp data for ss3 data file (default = TRUE)
#' @param iss test for whether input sample size time-dependent (TRUE) or constant (FALSE)
#' @param bin_iss value for special case of iss based on bin size (e.g., 'bin2', default = NULL)
#' @param nsamp value or vector for input sample size (default = 100)
#' 

get_twl_srvy_acomp <- function(new_year = 9999,
                               max_age = 10,
                               ss3_frmt = TRUE,
                               iss = FALSE,
                               bin_iss = NULL,
                               nsamp = 100){
  
  # compute comps ----
  ts_apop <- vroom::vroom(here::here(new_year, 'data', 'raw', 'twl_srvy_apop.csv'), 
                          progress = FALSE, 
                          show_col_types = FALSE)
  
  tidytable::expand_grid(year = sort(unique(ts_apop$year)),
                         age = seq(1, max_age)) %>% 
    tidytable::left_join(ts_apop %>% 
                           tidytable::mutate(age = tidytable::case_when(age >= 10 ~ 10, .default = age)) %>% 
                           tidytable::summarise(popn = sum(num), .by = c(year, age))) %>% 
    tidytable::mutate(popn = replace_na(popn, 0)) %>% 
    tidytable::mutate(agecomp = popn / sum(popn), .by = year) %>% 
    tidytable::select(-popn) -> ts_acomp
  
  # format for ss3 if desired ----
  if(isTRUE(ss3_frmt)){
    # hard-wired in season, etc for ss3 in ss3_args c(seas, fltsrv, gender, part, ageerr, lgin_lo, lgin_hi)
    ss3_args = c(7, -4, 0, 0, 1, -1, -1)
    # define iss
    if(isTRUE(iss)){
      if(is.null(bin_iss)){
        nsamp <- afscISS::get_ISS(species = 21720,
                                  region = 'goa',
                                  comp = 'age',
                                  sex_cat = 4) %>% 
          tidytable::select(year, nsamp = iss)
      } else{
        nsamp <- afscISS::get_ISS(species = 21720,
                                  region = 'goa',
                                  comp = 'age',
                                  sex_cat = 4,
                                  spec_case = bin_iss) %>% 
          tidytable::select(year, nsamp = iss)
      }
    } else{
      nsamp = 100
    }
    # get ss3 data
    ts_acomp <- ss3_age_com(data = ts_acomp,
                            ss3_args,
                            iss,
                            nsamp)
  }
  
  ts_acomp
}

#' Function to pull bottom trawl survey pop'n numbers for age composition
#' adapted/generalized from Steve Barbeaux' files for generating SS files for EBS/AI Greenland Turbot
#' Re-developed in 2024 by p. hulson to link to gap_products tables with dplyr::tbl()
#' 
#' @param new_year current assessment year
#' @param max_age user defined maximum age (default = 10)
#' @param bins user-defined length bins (default = NULL)
#' @param ss3_frmt whether to format comp data for ss3 data file (default = TRUE)
#' @param iss test for whether input sample size determined from surveyISS (TRUE) or not (FALSE)
#' @param bin_iss value for special case of iss based on bin size (e.g., 'bin2', default = NULL)
#' 

get_twl_srvy_caal <- function(new_year = 9999,
                              max_age = 10,
                              bins = NULL,
                              ss3_frmt = TRUE,
                              iss = FALSE,
                              bin_iss = NULL){
  
  # compute conditional age-at-length  ----
  ts_age <- vroom::vroom(here::here(new_year, 'data', 'raw', 'twl_srvy_age.csv'), 
                         progress = FALSE, 
                         show_col_types = FALSE)
  
  tidytable::expand_grid(year = sort(unique(ts_age$year)),
                         length = bins) %>% 
    tidytable::left_join(ts_age %>% 
                           tidytable::select(year, length, age) %>% 
                           tidytable::left_join(get_bin(ts_age %>% 
                                                          tidytable::distinct(length), bins)) %>% 
                           tidytable::select(year, age, length = new_length)) %>% 
    tidytable::drop_na() %>%
    tidytable::mutate(age = tidytable::case_when(age > max_age ~ max_age,
                                                 .default = age)) %>% 
    tidytable::summarise(count = .N, .by = c(year, length, age)) %>% 
    tidytable::mutate(tot = sum(count), .by = c(year, length)) %>% 
    tidytable::mutate(caal = round(count / tot, digits = 5)) %>% 
    tidytable::select(year, length, age, caal) -> ts_caal
  
  # format for ss3 if desired ----
  if(isTRUE(ss3_frmt)){
    # get all combos of year-length-age with observations
    tidytable::expand_grid(year = sort(unique(ts_age$year)),
                           length = bins,
                           age = seq(1, max_age)) %>% 
      tidytable::left_join(ts_caal) %>% 
      tidytable::mutate(caal = replace_na(caal, 0)) %>% 
      tidytable::mutate(test = sum(caal), .by = c(year, length)) %>% 
      tidytable::filter(test > 0) %>% 
      tidytable::select(-test) -> ts_caal
    # get iss
    if(isTRUE(iss)){
      if(is.null(bin_iss)){
        # get nsamp from surveyISS package
        afscISS::get_ISS(species = 21720,
                         region = 'goa',
                         comp = 'caal',
                         sex_cat = 4) %>% 
          tidytable::select(year, length, nsamp = iss) -> nsamp1
        
        nsamp1 %>% 
          tidytable::left_join(get_bin(nsamp1 %>% 
                                         tidytable::distinct(length), bins)) %>% 
          tidytable::select(year, length = new_length, nsamp) -> nsamp2
        # take care of plus group
        nsamp2 %>% 
          tidytable::filter(length != max(len_bins)) %>% 
          tidytable::bind_rows(nsamp2 %>% 
                                 tidytable::filter(length == max(len_bins)) %>% 
                                 tidytable::summarise(nsamp = mean(nsamp), .by = c(year, length))) -> nsamp
      } else{
        # get nsamp from surveyISS package
        afscISS::get_ISS(species = 21720,
                         region = 'goa',
                         comp = 'caal',
                         sex_cat = 4,
                         spec_case = bin_iss) %>% 
          tidytable::select(year, length, nsamp = iss) -> nsamp
      }
    } else{
      # get nsamp as sample size multiplied by 0.14
      tidytable::expand_grid(year = sort(unique(ts_age$year)),
                             length = bins) %>% 
        tidytable::bind_cols(bin = rep(seq(1, length(bins)), length(unique(ts_age$year)))) %>% 
        tidytable::left_join(ts_age %>% 
                               tidytable::select(year, length, age) %>% 
                               tidytable::left_join(get_bin(ts_age %>% 
                                                              tidytable::distinct(length), bins)) %>% 
                               tidytable::select(year, age, , length = new_length)) %>% 
        tidytable::drop_na() %>% 
        tidytable::summarise(count = .N, .by = c(year, length)) %>% 
        tidytable::mutate(nsamp = count * 0.14) %>% 
        tidytable::select(-count) -> nsamp
    }
    # hard-wired in season, etc for ss3 in ss3_args c(seas, fltsrv, gender, part, ageerr, lgin_lo, lgin_hi)
    ss3_args = c(7, 4, 0, 0, 1)
    # get ss3 data
    ts_caal <- ss3_caal(data = ts_caal,
                        ss3_args,
                        nsamp)
  }
  
  ts_caal
}
