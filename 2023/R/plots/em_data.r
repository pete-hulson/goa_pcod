## Function to plot cumulative catch for species and subarea

get_em<-function(data_query = FALSE,
                           species = "'PCOD'",
                           FMP_AREA = "'GOA'"){

  em_data <- vroom::vroom(here::here('data', 'COMPREHENSIVE_OBS_EM.csv'))
  
  em_data %>% 
    filter(SPECIES_GROUP_CODE == "PCOD",
           YEAR >= 2021,
           FMP_AREA == "GOA") -> em_data_pcod
  
  vroom::vroom_write(em_data_pcod, here::here('data', 'em_data_pcod.csv'), delim = ",")
  
}


