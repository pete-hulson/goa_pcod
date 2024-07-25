#' function to get growth statistics for goa pcod
#' 
#' @param new_year current assessment year
#' @param type type of growth, either length (len) or weight (wt)
#' @param max_age user defined maximum age (default = 10)
#' @param ss3_frmt whether to format comp data for ss3 data file (default = TRUE)
#' 
get_growth <- function(new_year,
                       type = 'len',
                       max_age = 10,
                       ss3_frmt = TRUE){
  
  # get age data ----
  ts_age <- vroom::vroom(here::here(new_year, 'data', 'raw', 'twl_srvy_age.csv'))
  
  # compute mean length-at-age ----
  
  tidytable::expand_grid(year = sort(unique(ts_age$year)),
                         age = seq(1, max_age)) %>% 
    tidytable::left_join(ts_age %>% 
                           tidytable::mutate(age = tidytable::case_when(age > max_age ~ max_age,
                                                                        .default = age)) %>% 
                           tidytable::filter(!is.na(length)) %>% 
                           tidytable::summarise(mean = round(mean(length), digits = 4),
                                                nsamp = length(length),
                                                .by = c(year, age))) %>% 
    tidytable::mutate(mean = tidytable::replace_na(mean, 0),
                      nsamp = tidytable::replace_na(nsamp, 0)) -> mean_len
  
  # compute mean weight-at-age ----
  tidytable::expand_grid(year = sort(unique(ts_age$year)),
                         age = seq(1, max_age)) %>% 
    tidytable::left_join(ts_age %>% 
                           tidytable::mutate(age = tidytable::case_when(age > max_age ~ max_age,
                                                                        .default = age)) %>% 
                           tidytable::filter(!is.na(weight)) %>% 
                           tidytable::summarise(mean = round(mean(weight), digits = 4),
                                                nsamp = length(weight),
                                                .by = c(year, age))) %>% 
    tidytable::mutate(mean = tidytable::replace_na(mean, 0),
                      nsamp = tidytable::replace_na(nsamp, 0))  -> mean_wt

  # format for ss3 if desired ----
  if(isTRUE(ss3_frmt)){
    # hard-wired in season, etc for ss3 in ss3_args c(seas, fltsrv, gender, part, ageerr, ignore)
    ss3_args = c(7, -4, 0, 0, 1, 999)
    mean_len <- ss3_grwth(mean_len,
                          ss3_args)
    mean_wt <- ss3_grwth(mean_wt,
                         ss3_args)
  }
  
  if(type = 'len'){
    mean_len
  } else{
    mean_wt
  }
  
}