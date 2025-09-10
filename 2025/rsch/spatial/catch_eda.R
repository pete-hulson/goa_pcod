# exploratory data analysis of pcod wgoa catch data

library(tidyverse)
library(vroom)
library(here)

# read in data

catch <- vroom::vroom(here::here('2025', 'data', 'raw', 'fish_catch_data.csv'))

# explore total catch data
catch %>% 
  tidytable::filter(trip_target_code == 'C',
                    reporting_area_code %in% c(610, 620, 630)) %>% 
  tidytable::select(year, trip_target_code, fmp_gear, fmp_subarea, reporting_area_code, week_end_date, weight_posted) %>% 
  tidytable::mutate(catch_month = lubridate::month(week_end_date),
                    stanza = factor(case_when(year <= 1999 ~ '90s',
                                              year >= 2000 & year <= 2009 ~ '00s',
                                              year >= 2010 & year <= 2016 ~ '10s pre-crash',
                                              year >= 2017 & year <= 2020 ~ '10s Post-crash pre-closure',
                                              year >= 2021 ~ 'Post-closure'),
                                    levels = c('90s', '00s', '10s pre-crash', '10s Post-crash pre-closure', 'Post-closure')),
                    season = factor(case_when(catch_month <= 3 ~ 'Spawning',
                                              catch_month > 3 ~ 'Non-spawning'),
                                    levels = c('Spawning', 'Non-spawning'))) %>% 
  tidytable::summarise(catch = sum(weight_posted), .by = c(fmp_gear, fmp_subarea, reporting_area_code, stanza, season)) %>% 
  vroom::vroom_write(., here::here('2025', 'rsch', 'spatial', 'data', 'catch_summ.csv'), delim = ',')


