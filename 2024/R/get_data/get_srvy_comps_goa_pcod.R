#' Function to pull bottom trawl survey pop'n numbers for length composition
#' adapted/generalized from Steve Barbeaux' files for generating SS files for EBS/AI Greenland Turbot
#' Re-developed in 2024 by p. hulson to link to gap_products tables with dplyr::tbl()
#' 
#' @param new_year current assessment year
#' @param bins user defined length bins (min/max for the length bin, default = NULL)
#' @param ss3_frmt whether to format comp data for ss3 data file (default = TRUE)
#' @param iss test for whether input sample size time-dependent (TRUE) or constant (FALSE)
#' @param nsamp value or vector for input sample size (default = 100)
#' 

get_twl_srvy_lcomp <- function(new_year = 9999,
                               bins = NULL,
                               ss3_frmt = TRUE,
                               iss = FALSE,
                               nsamp = 100){
  
  # compute comps ----
  ts_lpop <- vroom::vroom(here::here(new_year, 'data', 'raw', 'twl_srvy_lpop.csv'))
  
  # bin length comps to desired bin width
  tidytable::expand_grid(year = sort(unique(ts_lpop$year)),
                         length = bins) %>% 
    tidytable::bind_cols(bin = rep(seq(1, length(bins)), length(unique(ts_lpop$year)))) %>% 
    tidytable::left_join(ts_lpop %>% 
                           tidytable::summarise(popn = sum(num), .by = c(year, length)) %>% 
                           tidytable::left_join(get_bin(.$length, bins)) %>% 
                           tidytable::summarise(popn = sum(popn), .by = c(year, bin))) %>% 
    tidytable::mutate(popn = replace_na(popn, 0)) %>% 
    tidytable::select(-bin) -> ts_lpop_bin
  
  # compute comps
  ts_lpop_bin %>% 
    tidytable::mutate(lencomp = popn / sum(popn), .by = year) %>% 
    tidytable::select(-popn) -> ts_lcomp
  
  # format for ss3 if desired ----
  if(isTRUE(ss3_frmt)){
    # hard-wired in season, etc for ss3 in ss3_args c(seas, fltsrv, gender, part)
    ss3_args = c(7, 4, 0, 0)
    ts_lcomp <- ss3_len_com(data = ts_lcomp,
                            ss3_args = ss3_args,
                            iss = FALSE,
                            nsamp = 100)
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
  lls_lpop <- vroom::vroom(here::here(new_year, "data", "raw", "lls_rpn_length_data.csv")) %>% 
    tidytable::filter(year >= 1990)
  
  # bin length comps to desired bin width
  tidytable::expand_grid(year = sort(unique(lls_lpop$year)),
                         length = bins) %>% 
    tidytable::bind_cols(bin = rep(seq(1, length(bins)), length(unique(lls_lpop$year)))) %>% 
    tidytable::left_join(lls_lpop %>% 
                           tidytable::filter(length > 0) %>% 
                           tidytable::summarise(popn = sum(num), .by = c(year, length)) %>% 
                           tidytable::left_join(get_bin(.$length, bins)) %>% 
                           tidytable::summarise(popn = sum(popn), .by = c(year, bin))) %>% 
    tidytable::mutate(popn = replace_na(popn, 0)) %>% 
    tidytable::select(-bin) -> lls_lpop_bin
  
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
#' @param nsamp value or vector for input sample size (default = 100)
#' 

get_twl_srvy_acomp <- function(new_year = 9999,
                               max_age = 10,
                               ss3_frmt = TRUE,
                               iss = FALSE,
                               nsamp = 100){
  
  # compute comps ----
  ts_apop <- vroom::vroom(here::here(new_year, 'data', 'raw', 'twl_srvy_apop.csv'))
  
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
    ts_acomp <- ss3_age_com(data = ts_acomp,
                            ss3_args = ss3_args,
                            iss = FALSE,
                            nsamp = 100)
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
#' 

get_twl_srvy_caal <- function(new_year = 9999,
                              max_age = 10,
                              bins = NULL,
                              ss3_frmt = TRUE){
  
  # compute conditional age-at-length  ----
  ts_age <- vroom::vroom(here::here(new_year, 'data', 'raw', 'twl_srvy_age.csv'))
  
  tidytable::expand_grid(year = sort(unique(ts_age$year)),
                         length = bins) %>% 
    tidytable::bind_cols(bin = rep(seq(1, length(bins)), length(unique(ts_age$year)))) %>% 
    tidytable::left_join(ts_age %>% 
                           tidytable::select(year, length, age) %>% 
                           tidytable::left_join(get_bin(.$length, bins)) %>% 
                           tidytable::select(year, age, bin)) %>% 
    tidytable::select(-bin) %>% 
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
    
    # get nsamp as sample size multiplied by 0.14
    tidytable::expand_grid(year = sort(unique(ts_age$year)),
                           length = bins) %>% 
      tidytable::bind_cols(bin = rep(seq(1, length(bins)), length(unique(ts_age$year)))) %>% 
      tidytable::left_join(ts_age %>% 
                             tidytable::select(year, length, age) %>% 
                             tidytable::left_join(get_bin(.$length, bins)) %>% 
                             tidytable::select(year, age, bin)) %>% 
      tidytable::select(-bin) %>% 
      tidytable::drop_na() %>% 
      tidytable::summarise(count = .N, .by = c(year, length)) %>% 
      tidytable::mutate(nsamp = count * 0.14) %>% 
      tidytable::select(-count) -> nsamp
    
    # hard-wired in season, etc for ss3 in ss3_args c(seas, fltsrv, gender, part, ageerr, lgin_lo, lgin_hi)
    ss3_args = c(7, 4, 0, 0, 1)
    ts_caal <- ss3_caal(data = ts_caal,
                        ss3_args = ss3_args,
                        nsamp = nsamp)
  }
  
  ts_caal
}
