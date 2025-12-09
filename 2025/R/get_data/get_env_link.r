#' function to grab temperature data for longline survey catchability link
#' 
get_lls_env <- function(new_year,
                        index = 'cfsr',
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
      tidytable::select(year, index) -> env_indx1
    
    # do 2025 cloodge since cfsr not available
    hycom <- vroom::vroom(here::here(new_year, 'data', 'raw_hycom.csv'))
    
    hycom %>% 
      dplyr::rename_all(tolower) %>% 
      # use hycom 50m as representative of cfsr
      tidytable::filter(month == 5,
                        depth_cat == '50m') %>% 
      tidytable::select(year, temp = mean_monthly) %>% 
      tidytable::mutate(index = round(temp - mean(temp[which(year <= 2012)]), digits = 5)) %>% 
      tidytable::mutate(index2 = round(index - mean(index), digits = 5)) %>% 
      tidytable::filter(year == 2025) %>% 
      tidytable::mutate(index_new = round(index2 + env_indx1 %>% 
                          tidytable::summarise(mu_ind = mean(index)) %>% 
                          tidytable::pull(mu_ind), digits = 5)) %>% 
      tidytable::select(year, index = index_new) -> env_indx2
    
    env_indx <- env_indx1 %>% 
      tidytable::bind_rows(env_indx2)
    
  }
  
  # use hycom-cfsr hybrid data ----
  
  if(index == 'hycom'){
    hycom <- vroom::vroom(here::here(new_year, 'data', 'raw_hycom.csv'))
    cfsr <- vroom::vroom(here::here(new_year, 'data', 'raw_cfsr.csv'))

    cfsr %>% 
      dplyr::rename_all(tolower) %>% 
      tidytable::filter(month == 5) %>% 
      tidytable::select(year, temp = '0_20') %>% 
      tidytable::mutate(index = round(temp - mean(temp[which(year >= 1982 & year <= 2012)]), digits = 5)) %>% 
      tidytable::select(year, index) %>% 
      tidytable::filter(year <= 1994) -> env_indx1

    hycom %>% 
      dplyr::rename_all(tolower) %>% 
      tidytable::filter(month == 5,
                        depth_cat == '150m') %>% 
      tidytable::select(year, temp = mean_monthly) %>% 
      tidytable::mutate(index = round(temp - mean(temp[which(year <= 2012)]), digits = 5)) %>% 
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