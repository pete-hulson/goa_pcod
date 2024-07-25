#' function to grab temperature data for longlien survey catchability link
#' 
get_lls_env <- function(new_year,
                        ss3_frmt = TRUE,
                        var = 1){
  
  # get cfsr data ----
  cfsr <- vroom::vroom(here::here(new_year, 'data', 'raw_cfsr.csv'))
  
  # compute env index ----
  cfsr %>% 
    dplyr::rename_all(tolower) %>% 
    tidytable::filter(month == 6) %>% 
    tidytable::select(year, temp = '0_20') %>% 
    tidytable::mutate(avg = as.numeric(cfsr %>% 
                                         dplyr::rename_all(tolower) %>% 
                                         tidytable::filter(month == 6,
                                                           year >= 1982,
                                                           year <= 2012) %>% 
                                         tidytable::select(year, temp = '0_20') %>% 
                                         tidytable::summarise(avg_temp = mean(temp)))) %>% 
    tidytable::mutate(index = round(temp - avg, digits = 5)) %>% 
    tidytable::select(year, index) -> env_indx
  
  # format for ss3 if desired ----
  if(isTRUE(ss3_frmt)){
    env_indx <- ss3_envlnk(env_indx,
                           var)
  }
  env_indx
}