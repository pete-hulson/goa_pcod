
library(tidyverse)

# read in data
hls <- vroom::vroom(here::here(as.numeric(format(Sys.Date(), format = "%Y")), 'emailed_data', '649_haul.csv'))
len <- vroom::vroom(here::here(as.numeric(format(Sys.Date(), format = "%Y")), 'emailed_data', '649_length.csv'))

len %>% 
  tidytable::left_join(hls) %>% 
  vroom::vroom_write(., here::here(as.numeric(format(Sys.Date(), format = "%Y")), 'emailed_data', '649_data.csv'), delim = ",")




