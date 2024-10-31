
# format CI, PWS, NG data
vroom::vroom(here::here(new_year, "emailed_data", "adfg_fish_len", "awl_CI_NG_PWS_23_24.csv"), delim = ",") %>% 
  tidytable::left_join(vroom::vroom(here::here(new_year, "emailed_data", "adfg_fish_len", "header_CI_NG_PWS_23_24.csv"), delim = ",")) %>% 
  tidytable::mutate(area = case_when(area %in% c('CI', 'NG') ~ 630,
                                     area == 'PWS' ~ 649),
                    sex = case_when(sex == 1 ~ 1,
                                    sex == 2 ~ 2,
                                    .default = 3),
                    quarter = case_when(month %in% seq(1,3) ~ 1,
                                        month %in% seq(4,6) ~ 2,
                                        month %in% seq(7,9) ~ 3,
                                        month %in% seq(10,12) ~ 4),
                    source = 'pollak') %>% 
  tidytable::summarise(freq = .N, .by = c(year, area, gear, month, quarter, sex, length, source)) %>% 
  drop_na() %>% 
  tidytable::filter(length > 0) %>% 
  tidytable::select(year, area, gear, month, sex, length, freq, quarter, source) -> ci_pws_ng
vroom::vroom_write(ci_pws_ng, here::here(new_year, "emailed_data", "adfg_fish_len", 'lfreq_CI_NG_PWS_23_24.csv'), delim = ",")

# format NSEI data
vroom::vroom(here::here(new_year, "emailed_data", "adfg_fish_len", "raw_NSEI_22_24.csv"), delim = ",") %>% 
  tidytable::summarise(freq = .N, .by = c(year, area, gear, month, sex, length)) %>% 
  tidytable::mutate(quarter = case_when(month %in% seq(1,3) ~ 1,
                                        month %in% seq(4,6) ~ 2,
                                        month %in% seq(7,9) ~ 3,
                                        month %in% seq(10,12) ~ 4),
                    source = 'baldwin') %>% 
  drop_na() %>% 
  filter(length > 0) %>% 
  tidytable::select(year, area, gear, month, sex, length, freq, quarter, source) -> nsei
vroom::vroom_write(nsei, here::here(new_year, "emailed_data", "adfg_fish_len", 'lfreq_NSEI_22_24.csv'), delim = ",")

# format 610-630 data
vroom::vroom(here::here(new_year, "emailed_data", "adfg_fish_len", "ADFG_ALL_KLM_Cod_LF_Data.csv"), delim = ",") %>% 
  tidytable::filter(year >= 2022) %>% 
  tidytable::select(year, port, nmfs_area, gear_code, landing_date, sex, length, nfish) %>% 
  tidytable::filter(is.na(nmfs_area)) %>% 
  tidytable::mutate(area = case_when(port == "Kodiak" ~ 630,
                                     .default = 610),
                    month = month(landing_date),
                    quarter = case_when(month %in% seq(1,3) ~ 1,
                                        month %in% seq(4,6) ~ 2,
                                        month %in% seq(7,9) ~ 3,
                                        month %in% seq(10,12) ~ 4),
                    source = "elmejjati",
                    sex = case_when(sex == 1 ~ 1,
                                    sex == 2 ~ 2,
                                    .default = 3)) %>% 
  tidytable::select(year, area, gear = gear_code, month, sex, length, freq = nfish, quarter, source) %>% 
  tidytable::bind_rows(vroom::vroom(here::here(new_year, "emailed_data", "adfg_fish_len", "ADFG_ALL_KLM_Cod_LF_Data.csv"), delim = ",") %>% 
                         tidytable::filter(year >= 2022) %>% 
                         tidytable::select(year, port, nmfs_area, gear_code, landing_date, sex, length, nfish) %>% 
                         tidytable::filter(!is.na(nmfs_area)) %>% 
                         tidytable::mutate(area = case_when(nmfs_area == "610" ~ 610,
                                                            nmfs_area == "620" ~ 620,
                                                            nmfs_area == "630" ~ 630,
                                                            nmfs_area == "620/630" ~ 620,
                                                            nmfs_area == "630/620" ~ 630),
                                           month = month(landing_date),
                                           quarter = case_when(month %in% seq(1,3) ~ 1,
                                                               month %in% seq(4,6) ~ 2,
                                                               month %in% seq(7,9) ~ 3,
                                                               month %in% seq(10,12) ~ 4),
                                           source = "elmejjati",
                                           sex = case_when(sex == 1 ~ 1,
                                                           sex == 2 ~ 2,
                                                           .default = 3)) %>% 
                         tidytable::select(year, area, gear = gear_code, month, sex, length, freq = nfish, quarter, source)) %>% 
  drop_na() -> wcgoa
vroom::vroom_write(wcgoa, here::here(new_year, "emailed_data", "adfg_fish_len", 'lfreq_WCGOA_22_24.csv'), delim = ",")

# replace 2022 on in lfreq data (this file no longer here)

vroom::vroom(here::here(new_year, "data", "fish_lfreq_state.csv"), delim = ",") %>% 
  dplyr::rename_all(tolower) %>% 
  tidytable::filter(year < 2022) %>% 
  tidytable::bind_rows(ci_pws_ng) %>% 
  tidytable::bind_rows(nsei) %>% 
  tidytable::bind_rows(wcgoa) %>% 
  tidytable::summarise(freq = sum(freq), .by = c(year, source, area, gear, quarter, month, sex, length)) %>% 
  tidytable::arrange(-year) -> state_lfreq

vroom::vroom_write(state_lfreq, here::here(new_year, "data", "raw", "fish_lfreq_state.csv"), delim = ",")
