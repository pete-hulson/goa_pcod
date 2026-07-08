# compile tagging data for sporc
library(tidyverse)

# params
new_year = 2026




# release joined with recoveries for all available data
rel_rec <- vroom::vroom(here::here(new_year, 'rsch', 'wgoa', 'data', 'all_tags.csv'))


# release data
adfg_rel <- vroom::vroom(here::here(new_year, 'rsch', 'wgoa', 'data', 'tag_dbs', 'adfg_release.csv'))
fit_rel <- vroom::vroom(here::here(new_year, 'rsch', 'wgoa', 'data', 'tag_dbs', 'fit_release.csv'))
race_rel <- vroom::vroom(here::here(new_year, 'rsch', 'wgoa', 'data', 'tag_dbs', 'race_release.csv'))

rel <- race_rel %>% 
  tidytable::select(rel_year = RlsYear,
                    rel_month = RlsMonth,
                    long_rel = RlsLong,
                    lat_rel = RlsLat,
                    rel_len = RlsLength,
                    rel_area = rlsNMFSSA) %>% 
  tidytable::mutate(program = 'RACE') %>% 
  tidytable::bind_rows(fit_rel %>% 
                         tidytable::select(rel_date = RDT,
                                           long_rel = RlsLong,
                                           lat_rel = RlsLat,
                                           rel_len = RlsLen,
                                           rel_area = RlsFedSA) %>% 
                         tidytable::drop_na() %>% 
                         tidytable::mutate(rel_year = year(as.Date(rel_date, format = '%m/%d/%Y')),
                                           rel_month = month(as.Date(rel_date, format = '%m/%d/%Y'))) %>% 
                         tidytable::select(-rel_date) %>% 
                         tidytable::mutate(program = 'FIT')) %>% 
  tidytable::bind_rows(adfg_rel %>% 
                         tidytable::select(rel_year = rlsYr,
                                           rel_date = rlsDt,
                                           long_rel = rlsLongDD,
                                           lat_rel = rlsLatDD,
                                           rel_len = rlsLen,
                                           rel_area = rlsNMFSSA) %>% 
                         tidytable::mutate(rel_month = month(as.Date(rel_date, format = '%m/%d/%Y'))) %>% 
                         tidytable::select(-rel_date) %>% 
                         tidytable::mutate(program = 'ADFG')) %>% 
  tidytable::mutate(rel_region = case_when(rel_area < 600 ~ 'BS',
                                           rel_area == 610 ~ 'WGOA',
                                           rel_area %in% c(620, 630) ~ 'CGOA',
                                           rel_area > 630 ~ 'EGOA',
                                           is.na(rel_area) ~ NA),
                    rel_season = case_when(rel_month <= 3 ~ 'A',
                                           rel_month > 3 ~ 'B'))


# put it together and write data
cod_tag_data <- list(desc = c("rel_rec = Combined release and recovery data",
                              "rel = Release data only (does not include satellite tags)"),
                     rel_rec = rel_rec,
                     rel = rel)

saveRDS(cod_tag_data, file = here::here(new_year, 'rsch', 'wgoa', 'data', 'cod_tag_data.rds'))
