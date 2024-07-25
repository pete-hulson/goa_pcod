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
    tidytable::bind_cols(bin = rep(seq(1, length(bins)), length(unique(fsh_age$year)) * length(unique(fsh_age$gear)))) %>% 
    tidytable::left_join(fsh_age %>% 
                           tidytable::select(year, gear, length, age) %>% 
                           tidytable::left_join(get_bin(.$length, bins)) %>% 
                           tidytable::select(year, gear, age, bin)) %>% 
    tidytable::select(-bin) %>% 
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
      tidytable::bind_cols(bin = rep(seq(1, length(bins)), length(unique(fsh_age$year)) * length(unique(fsh_age$gear)))) %>% 
      tidytable::left_join(fsh_age %>% 
                             tidytable::select(year, gear, length, age) %>% 
                             tidytable::left_join(get_bin(.$length, bins)) %>% 
                             tidytable::select(year, gear, age, bin)) %>% 
      tidytable::select(-bin) %>% 
      tidytable::drop_na() %>% 
      tidytable::summarise(count = .N, .by = c(year, gear, length)) %>% 
      tidytable::mutate(nsamp = count * 0.14) %>% 
      tidytable::mutate(fltsrv = tidytable::case_when(gear == 'trawl' ~ 1,
                                                      gear == 'longline' ~ 2,
                                                      gear == 'pot' ~ 3)) %>% 
      tidytable::select(-count, -gear) -> nsamp
    
    # hard-wired in season, etc for ss3 in ss3_args c(seas, gender, part, ageerr, lgin_lo, lgin_hi)
    ss3_args = c(1, 0, 0, 1)
    fsh_caal <- ss3_caal_fsh(fsh_caal,
                             ss3_args,
                             nsamp)
  }
  
  fsh_caal
}
