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
    tidytable::mutate(caal = count / tot) %>% 
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
    ts_caal <- ss3_caal(ts_caal,
                        ss3_args,
                        nsamp)
  }
  
  ts_caal
}
