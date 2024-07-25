#' utility fcns
#' 
#' function to bin length data to custom length bins 
#' @param data length data to bin (default = NULL)
#' @param bins user-defined length bins (default = NULL)
#' 
get_bin <- function(data = NULL,
                    bins = NULL){
  
  test_lens = seq(min(data), max(data))
  
  bins %>% 
    tidytable::bind_cols(less = c(bins[2:length(bins)], 1000),
                         bin = seq(1, length(bins))) %>% 
    tidytable::rename(more = '...1') -> bin_test
  bin = vector(length = length(test_lens))
  
  for(i in 1:length(test_lens)){
    bin[i] = which(test_lens[i] > bin_test$more & test_lens[i] < bin_test$less)
  }
  
  test_lens %>% 
    tidytable::bind_cols(bin) %>% 
    tidytable::rename(length = '...1',
                      bin = '...2')
}
#' function to format survey length comp data for ss3 data file
#' @param data data to format for ss3 (default = NULL)
#' @param ss3_args arguments for ss3 data file (i.e., fltsrv, gender, etc; default = NULL)
#' @param iss switch for whether iss is time-varying or constant (default = NULL)
#' @param nsamp input sample size (default = NULL)
#' 
ss3_len_com <- function(data = NULL, 
                        ss3_args = NULL, 
                        iss = NULL, 
                        nsamp = NULL){
  
  data %>% 
    tidytable::mutate(seas = ss3_args[1],
                      fltsrv = ss3_args[2],
                      gender = ss3_args[3],
                      part = ss3_args[4]) -> lcomp_part
  
  # test if input sample size constant or read-in (e.g., from surveyISS package)
  if(isTRUE(iss)){
    lcomp_part %>% 
      tidytable::left_join(nsamp) %>% 
      tidytable::pivot_wider(names_from = length, values_from = lencomp) -> ss3_lcomp
  } else{
    lcomp_part %>% 
      tidytable::mutate(nsamp = nsamp) %>% 
      tidytable::pivot_wider(names_from = length, values_from = lencomp) -> ss3_lcomp
  }
  
  ss3_lcomp
}
#' function to format fishery length comp data for ss3 data file
#' @param data data to format for ss3 (default = NULL)
#' @param ss3_args arguments for ss3 data file (i.e., fltsrv, gender, etc; default = NULL)
#' @param nsamp input sample size (default = NULL)
#' 
ss3_len_com_fsh <- function(data = NULL, 
                            ss3_args = NULL, 
                            nsamp = NULL){
  
  data %>% 
    tidytable::mutate(seas = ss3_args[1],
                      fltsrv = tidytable::case_when(gear == 'trawl' ~ 1,
                                                    gear == 'longline' ~ 2,
                                                    gear == 'pot' ~ 3),
                      gender = ss3_args[2],
                      part = ss3_args[3]) %>% 
    tidytable::left_join(nsamp) %>% 
    tidytable::select(-gear) %>% 
    tidytable::pivot_wider(names_from = length, values_from = lencomp) %>% 
    tidytable::arrange(fltsrv) -> ss3_lcomp
  
  ss3_lcomp
}
#' function to format survey age comp data for ss3 data file
#' @param data data to format for ss3 (default = NULL)
#' @param ss3_args arguments for ss3 data file (i.e., fltsrv, gender, etc; default = NULL)
#' @param iss switch for whether iss is time-varying or constant (default = NULL)
#' @param nsamp input sample size (default = NULL)
#' 
ss3_age_com <- function(data = NULL, 
                        ss3_args = NULL, 
                        iss = NULL, 
                        nsamp = NULL){
  data %>% 
    tidytable::mutate(seas = ss3_args[1],
                      fltsrv = ss3_args[2],
                      gender = ss3_args[3],
                      part = ss3_args[4],
                      ageerr = ss3_args[5],
                      lgin_lo = ss3_args[6],
                      lgin_hi = ss3_args[7]) -> acomp_part
  
  # test if input sample size constant or read-in (e.g., from surveyISS package)
  if(isTRUE(iss)){
    acomp_part %>% 
      tidytable::left_join(nsamp) %>% 
      tidytable::pivot_wider(names_from = age, values_from = agecomp) -> ss3_acomp
  } else{
    acomp_part %>% 
      tidytable::mutate(nsamp = nsamp) %>% 
      tidytable::pivot_wider(names_from = age, values_from = agecomp) -> ss3_acomp
  }
  
  ss3_acomp
}
#' function to format fishery age comp data for ss3 data file
#' @param data data to format for ss3 (default = NULL)
#' @param ss3_args arguments for ss3 data file (i.e., fltsrv, gender, etc; default = NULL)
#' @param max_age max age for data (default = 10)
#' @param iss switch for whether iss is time-varying or constant (default = NULL)
#' @param nsamp input sample size (default = NULL)
#' @param fit switch for whether fishery age data is fit (default = FALSE)
#' 
ss3_age_com_fsh <- function(data = NULL,
                            ss3_args = NULL,
                            max_age = 10, 
                            iss = NULL, 
                            nsamp = NULL,
                            fit = FALSE){
  
  data %>% 
    tidytable::mutate(seas = ss3_args[1],
                      fltsrv = tidytable::case_when(gear == 'trawl' ~ 1,
                                                    gear == 'longline' ~ 2,
                                                    gear == 'pot' ~ 3),
                      gender = ss3_args[2],
                      part = ss3_args[3],
                      ageerr = ss3_args[4],
                      lgin_lo = ss3_args[5],
                      lgin_hi = ss3_args[6],
                      age = tidytable::case_when(age > max_age ~ max_age,
                                                 .default = age)) %>% 
    tidytable::summarise(agecomp = sum(agecomp), .by = c(year, seas, fltsrv, gender, part, ageerr, lgin_lo, lgin_hi, age)) -> acomp_part
  
  # test if input sample size constant or read-in (e.g., from surveyISS package)
  if(isTRUE(iss)){
    acomp_part %>% 
      tidytable::left_join(nsamp %>% 
                             tidytable::mutate(fltsrv = tidytable::case_when(gear == 'trawl' ~ 1,
                                                                             gear == 'longline' ~ 2,
                                                                             gear == 'pot' ~ 3)) %>% 
                             tidytable::select(year, fltsrv, nsamp)) %>% 
      tidytable::pivot_wider(names_from = age, values_from = agecomp) %>% 
      tidytable::arrange(fltsrv, year) -> ss3_acomp
  } else{
    acomp_part %>% 
      tidytable::mutate(nsamp = nsamp) %>% 
      tidytable::pivot_wider(names_from = age, values_from = agecomp) -> ss3_acomp
  }
  
  if(!isTRUE(fit)){
    ss3_acomp %>% 
      tidytable::mutate(fltsrv = fltsrv * -1) -> ss3_acomp
  }
  
  ss3_acomp
}
#' function to format survey conditional age-at-length data for ss3 data file
#' @param data data to format for ss3 (default = NULL)
#' @param ss3_args arguments for ss3 data file (i.e., fltsrv, gender, etc; default = NULL)
#' @param nsamp input sample size (default = NULL)
#' 
ss3_caal <- function(data = NULL, 
                     ss3_args = NULL, 
                     nsamp = NULL){
  data %>% 
    tidytable::mutate(seas = ss3_args[1],
                      fltsrv = ss3_args[2],
                      gender = ss3_args[3],
                      part = ss3_args[4],
                      ageerr = ss3_args[5],
                      lgin_lo = length,
                      lgin_hi = length) %>% 
    tidytable::left_join(nsamp) %>% 
    tidytable::select(-length) %>% 
    tidytable::pivot_wider(names_from = age, values_from = caal)
}
#' function to format fishery conditional age-at-length data for ss3 data file
#' @param data data to format for ss3 (default = NULL)
#' @param ss3_args arguments for ss3 data file (i.e., fltsrv, gender, etc; default = NULL)
#' @param nsamp input sample size (default = NULL)
#' 
ss3_caal_fsh <- function(data = NULL,
                         ss3_args = NULL,
                         nsamp = NULL){
  data %>% 
    tidytable::mutate(seas = ss3_args[1],
                      fltsrv = tidytable::case_when(gear == 'trawl' ~ 1,
                                                    gear == 'longline' ~ 2,
                                                    gear == 'pot' ~ 3),
                      gender = ss3_args[2],
                      part = ss3_args[3],
                      ageerr = ss3_args[4],
                      lgin_lo = length,
                      lgin_hi = length) %>% 
    tidytable::left_join(nsamp) %>% 
    tidytable::select(-gear, -length) %>% 
    tidytable::pivot_wider(names_from = age, values_from = caal) %>% 
    tidytable::arrange(fltsrv, year)
}
#' function to format growth data for ss3 data file
#' @param data data to format for ss3 (default = NULL)
#' @param ss3_args arguments for ss3 data file (i.e., fltsrv, gender, etc; default = NULL)
#' 
ss3_grwth <- function(data = NULL,
                      ss3_args = NULL){
  data %>% 
    tidytable::mutate(seas = ss3_args[1],
                      fltsrv = ss3_args[2],
                      gender = ss3_args[3],
                      part = ss3_args[4],
                      ageerr = ss3_args[5],
                      ignore = ss3_args[6],
                      age = paste0('a', age)) %>% 
    tidytable::select(-nsamp) %>% 
    tidytable::pivot_wider(names_from = age, values_from = mean) %>% 
    tidytable::left_join(data %>% 
                           tidytable::mutate(age = paste0('n_a', age)) %>% 
                           tidytable::select(-mean) %>% 
                           tidytable::pivot_wider(names_from = age, values_from = nsamp))
}
