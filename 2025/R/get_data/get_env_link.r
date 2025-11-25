#' function to grab temperature data for longline survey catchability link
#' 
get_lls_env <- function(new_year,
                        index = 'hycom',
                        ss3_frmt = TRUE,
                        var = 1){
  
  # use cfsr data ----
  if(index == 'cfsr'){
    cfsr <- vroom::vroom(here::here(new_year, 'data', 'raw_cfsr.csv'))

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
  }
  
  # use hycom-cfsr hybrid data ----
  
  if(index == 'hycom'){
    hycom <- vroom::vroom(here::here(new_year, 'data', 'raw_hycom.csv'))
    cfsr <- vroom::vroom(here::here(new_year, 'data', 'raw_cfsr.csv'))

    cfsr %>% 
      dplyr::rename_all(tolower) %>% 
      tidytable::filter(month == 6,
                        year >= 1979,
                        year <= 1994) %>% 
      tidytable::select(year, temp = '0_20') %>% 
      tidytable::mutate(avg = as.numeric(cfsr %>% 
                                           dplyr::rename_all(tolower) %>% 
                                           tidytable::filter(month == 6,
                                                             year >= 1979,
                                                             year <= 1994) %>% 
                                           tidytable::select(year, temp = '0_20') %>% 
                                           tidytable::summarise(avg_temp = mean(temp)))) %>% 
      tidytable::mutate(index = round(temp - avg, digits = 5)) %>% 
      tidytable::select(year, index) -> env_indx1
    
    hycom %>% 
      dplyr::rename_all(tolower) %>% 
      tidytable::filter(month == 5,
                        depth_cat == '150m') %>% 
      tidytable::select(year, temp = mean_monthly) %>% 
      tidytable::mutate(avg = as.numeric(hycom %>% 
                                           dplyr::rename_all(tolower) %>% 
                                           tidytable::filter(month == 5,
                                                             depth_cat == '150m',
                                                             year >= 1995,
                                                             year <= 2012) %>% 
                                           tidytable::select(year, temp = mean_monthly) %>% 
                                           tidytable::summarise(avg_temp = mean(temp)))) %>% 
      tidytable::mutate(index = round(temp - avg, digits = 5)) %>% 
      tidytable::select(year, index) -> env_indx2
    
    env_indx <- env_indx1 %>% 
      tidytable::bind_rows(env_indx2)
  }
  
  # format for ss3 if desired ----
  if(isTRUE(ss3_frmt)){
    env_indx <- ss3_envlnk(env_indx,
                           var)
  }
  env_indx
}