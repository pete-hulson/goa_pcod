#' utility fcns
#' function to bin length data to custom length bins 
get_bin <- function(data, bins){
  
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
ss3_len_com <- function(data, ss3_args, iss, nsamp){
  
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
ss3_len_com_fsh <- function(data, ss3_args, nsamp){
  
  data %>% 
    tidytable::mutate(seas = ss3_args[1],
                      fltsrv = case_when(gear == 'trawl' ~ 1,
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
ss3_age_com <- function(data, ss3_args, iss, nsamp){
  
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
    lcomp_part %>% 
      tidytable::mutate(nsamp = nsamp) %>% 
      tidytable::pivot_wider(names_from = age, values_from = agecomp) -> ss3_acomp
  }
  
  ss3_acomp
}