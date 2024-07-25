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
    ts_acomp <- ss3_age_com(ts_acomp,
                            ss3_args,
                            iss,
                            nsamp)
  }

  ts_acomp %>% data.table()
}
