#' utility fcns

get_bin <- function(data, bins){
  bins %>% 
    tidytable::bind_cols(less = c(bins[2:length(bins)], 1000),
                         bin = seq(1, length(bins))) %>% 
    tidytable::rename(more = '...1') -> bin_test
  bin = vector(length = length(data$length))
  
  for(i in 1:length(data$length)){
    bin[i] = which(data$length[i] > bin_test$more & data$length[i] < bin_test$less)
  }
  
  tidytable::expand_grid(year = sort(unique(data$year)),
                         length = bins) %>% 
    tidytable::bind_cols(rep(seq(1, length(bins)), length(unique(data$year)))) %>% 
    tidytable::rename(bin = '...3') %>% 
    tidytable::left_join(data %>% 
                           tidytable::bind_cols(bin) %>% 
                           tidytable::rename(bin = '...4') %>% 
                           tidytable::select(-length)) %>% 
    tidytable::mutate(popn = replace_na(popn, 0)) %>% 
    tidytable::summarise(popn = sum(popn), .by = c(year, length))
}

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
