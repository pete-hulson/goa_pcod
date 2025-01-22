# exploratory data analysis of pcod tagging data
library(tidyverse)


tag_data <- vroom::vroom(here::here('2025', 'rsch', 'spatial', 'ALL_TAG_DATA2025.csv'), delim = ',')



tag_data %>% 
  dplyr::rename_all(tolower) %>% 
  tidytable::mutate(recdt = as.Date(recdt, format = '%m/%d/%Y'),
                    reldt = as.Date(reldt, format = '%m/%d/%Y'),
                    lib_time = as.numeric(recdt - reldt) / 365) %>% 
  tidytable::select(tag_number, rel_month, rec_month, rel_year, rec_year, rel_region, rec_region, lib_time) -> .tag_data



# movement cases ----
# released ebs, recovered goa
.tag_data %>% 
  tidytable::filter(rel_region == 'EBS') %>% 
  tidytable::mutate(test = tidytable::case_when(rec_region == 'GOA' ~ 1,
                                                .default = 0)) %>% 
  tidytable::summarise(case = 'EBS -> GOA',
                       move = scales::percent(sum(test) / length(test), accuracy = 0.01),
                       n = length(test)) %>% 
  tidytable::bind_rows(
    # released goa, recovered ebs
    .tag_data %>% 
      tidytable::filter(rel_region == 'GOA') %>% 
      tidytable::mutate(test = tidytable::case_when(rec_region == 'EBS' ~ 1,
                                                    .default = 0)) %>% 
      tidytable::summarise(case = 'GOA -> EBS',
                           move = scales::percent(sum(test) / length(test), accuracy = 0.01),
                           n = length(test))) %>% 
  tidytable::bind_rows(
    # released ebs in winter/spawning, recovered goa during all survey months survey (with time filter > 3 months)
    .tag_data %>% 
      tidytable::filter(rel_region == 'EBS',
                        lib_time > 3/12,
                        rel_month <= 3,
                        rec_month %in% seq(5, 8)) %>% 
      tidytable::mutate(test = tidytable::case_when(rec_region == 'GOA' ~ 1,
                                                    .default = 0)) %>% 
      tidytable::summarise(case = 'EBS(w) -> GOA(s)',
                           move = scales::percent(sum(test) / length(test), accuracy = 0.01),
                           n = length(test))) %>% 
  tidytable::bind_rows(
    # released ebs in winter/spawning, recovered goa during wgoa survey months survey (with time filter > 3 months)
    .tag_data %>% 
      tidytable::filter(rel_region == 'EBS',
                        lib_time > 3/12,
                        rel_month <= 3,
                        rec_month %in% seq(5, 6)) %>% 
      tidytable::mutate(test = tidytable::case_when(rec_region == 'GOA' ~ 1,
                                                    .default = 0)) %>% 
      tidytable::summarise(case = 'EBS(w) -> GOA(wgoa-s)',
                           move = scales::percent(sum(test) / length(test), accuracy = 0.01),
                           n = length(test))) %>% 
  tidytable::bind_rows(
    # released goa in winter/spawning, recovered ebs during survey (with time filter > 3 months)
    .tag_data %>% 
      tidytable::filter(rel_region == 'GOA',
                        lib_time > 3/12,
                        rel_month <= 3,
                        rec_month %in% seq(5, 8)) %>% 
      tidytable::mutate(test = tidytable::case_when(rec_region == 'EBS' ~ 1,
                                                    .default = 0)) %>% 
      tidytable::summarise(case = 'GOA(w) -> EBS(s)',
                           move = scales::percent(sum(test) / length(test), accuracy = 0.01),
                           n = length(test)))

