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
  data %>% 
    tidytable::bind_cols(bin) %>% 
    tidytable::rename(bin = '...4')
}

