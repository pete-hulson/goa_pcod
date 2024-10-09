
# format CI, PWS, NG data
vroom::vroom(here::here(new_year, "emailed_data", "adfg_fish_len", "awl_CI_NG_PWS_23_24.csv"), delim = ",") %>% 
  tidytable::left_join(vroom::vroom(here::here(new_year, "emailed_data", "adfg_fish_len", "header_CI_NG_PWS_23_24.csv"), delim = ",")) %>% 
  dplyr::rename_all(tolower) %>% 
  tidytable::mutate(area = case_when(area %in% c('CI', 'NG') ~ 630,
                                     area == 'PWS' ~ 649)) %>% 
  tidytable::summarise(freq = .N, .by = c(year, area, gear, month, length)) %>% 
  drop_na() %>% 
  filter(length > 0) -> ci_pws_ng
vroom::vroom_write(ci_pws_ng, here::here(new_year, "emailed_data", "adfg_fish_len", 'lfreq_CI_NG_PWS_23_24.csv'), delim = ",")

# format NSEI data
vroom::vroom(here::here(new_year, "emailed_data", "adfg_fish_len", "raw_NSEI_22_24.csv"), delim = ",") %>% 
  tidytable::summarise(freq = .N, .by = c(year, area, gear, month, length)) %>% 
  drop_na() %>% 
  filter(length > 0) -> nsei
vroom::vroom_write(nsei, here::here(new_year, "emailed_data", "adfg_fish_len", 'lfreq_NSEI_22_24.csv'), delim = ",")

# format 610-630 data
vroom::vroom(here::here(new_year, "emailed_data", "adfg_fish_len", "ADFG_ALL_KLM_Cod_LF_Data.csv"), delim = ",") %>% 
  tidytable::filter(year >= 2022) %>% 
  tidytable::select(year, port, nmfs_area, gear_code, landing_date, length, nfish) %>% 
  tidytable::filter(is.na(nmfs_area)) %>% 
  tidytable::mutate(area = case_when(port == "Kodiak" ~ 630,
                                     .default = 610),
                    month = month(landing_date)) %>% 
  tidytable::select(year, area, gear = gear_code, month, length, freq = nfish) %>% 
  tidytable::bind_rows(vroom::vroom(here::here(new_year, "emailed_data", "adfg_fish_len", "ADFG_ALL_KLM_Cod_LF_Data.csv"), delim = ",") %>% 
                         tidytable::filter(year >= 2022) %>% 
                         tidytable::select(year, port, nmfs_area, gear_code, landing_date, length, nfish) %>% 
                         tidytable::filter(!is.na(nmfs_area)) %>% 
                         tidytable::mutate(area = case_when(nmfs_area == "610" ~ 610,
                                                            nmfs_area == "620" ~ 620,
                                                            nmfs_area == "630" ~ 630,
                                                            nmfs_area == "620/630" ~ 620,
                                                            nmfs_area == "630/620" ~ 630),
                                           month = month(landing_date)) %>% 
                         tidytable::select(year, area, gear = gear_code, month, length, freq = nfish)) %>% 
  drop_na() -> wcgoa
vroom::vroom_write(wcgoa, here::here(new_year, "emailed_data", "adfg_fish_len", 'lfreq_WCGOA_22_24.csv'), delim = ",")

# replace 2022 on in lfreq data

vroom::vroom(here::here(new_year, "data", "fish_lfreq_state.csv"), delim = ",") %>% 
  dplyr::rename_all(tolower) %>% 
  tidytable::summarise(freq = sum(freq), .by = c(year, area, gear, month, length)) %>% 
  tidytable::filter(year < 2022) %>% 
  tidytable::bind_rows(ci_pws_ng) %>% 
  tidytable::bind_rows(nsei) %>% 
  tidytable::bind_rows(wcgoa) %>% 
  tidytable::arrange(-year) -> state_lfreq

vroom::vroom_write(state_lfreq, here::here(new_year, "data", "raw", "fish_lfreq_state.csv"), delim = ",")
