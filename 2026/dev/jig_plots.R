# get connected
db = 'akfin'
conn = afscdata::connect(db)  

# Current assessment year
new_year <- as.numeric(format(Sys.Date(), format = "%Y"))

# query jig and pot data
dplyr::tbl(conn, dplyr::sql('akr.v_cas_account')) %>%
  dplyr::filter(ACCOUNT_NAME %in% c('CGOA Pcod Pot', 'CGOA Pcod Jig','WGOA Pcod Pot', 'WGOA Pcod Jig')) %>%
  dplyr::inner_join(dplyr::tbl(conn, dplyr::sql('akr.v_cas_txn_secondary_all')) %>% 
                      dplyr::filter(SPECIES_GROUP_CODE == 'PCOD'),
                    by = c("ID" = "ACCOUNT_ID", "QUOTA_POOL_YEAR" = "YEAR")) %>%
  dplyr::rename_all(tolower) %>%
  dplyr::filter(fmp_area_code == 'GOA') %>% 
  dplyr::select(year = quota_pool_year,
                account_name = account_name.x, 
                initial_quota_weight_allocated, 
                species_group_code,
                week_end_date,
                species_weight,
                vessel_id) %>% 
  dplyr::collect() -> catch_q


# query total catch for number of vessels
tot_catch <- vroom::vroom(here::here(new_year, 'data', 'raw', 'fish_catch_data.csv'))



# set up plot data
catch_q %>% 
  tidytable::mutate(week = lubridate::week(week_end_date)) %>% 
  tidytable::summarise(week_catch = sum(species_weight),
                       .by = c(year, account_name, initial_quota_weight_allocated, species_group_code, week)) %>% 
  tidytable::mutate(gear = case_when(stringr::str_detect(account_name, "Jig") ~ "Jig",
                                     stringr::str_detect(account_name, "Pot") ~ "Pot"),
                    area = case_when(stringr::str_detect(account_name, "CGOA") ~ "Central gulf",
                                     stringr::str_detect(account_name, "WGOA") ~ "Western gulf")) %>% 
  tidytable::select(year, gear, area, quota = initial_quota_weight_allocated, week, week_catch) -> catch.x

catch.x %>% 
  tidytable::distinct(year, gear, area, quota) %>% 
  tidytable::left_join(catch_q %>% 
                         tidytable::mutate(gear = case_when(stringr::str_detect(account_name, "Jig") ~ "Jig",
                                                            stringr::str_detect(account_name, "Pot") ~ "Pot"),
                                           area = case_when(stringr::str_detect(account_name, "CGOA") ~ "Central gulf",
                                                            stringr::str_detect(account_name, "WGOA") ~ "Western gulf")) %>% 
                         distinct(year, gear, area, vessel_id) %>% 
                         tidytable::summarise(num_vess = .N, .by = c(year, gear, area))) -> catch.y

# plot for time periods ----
# set years for time periods
pre_coll <- 2014
collapsed <- 2017

tidytable::expand_grid(year = seq(min(catch.y$year), max(catch.y$year) - 1), #c(2015:2018, seq(new_year - 2, new_year - 1)),
                       gear = c("Jig", "Pot"), 
                       area = c("Central gulf", "Western gulf"),
                       week = 1:52) %>% 
  tidytable::bind_rows(expand.grid(year = new_year, 
                                   gear = c("Jig", "Pot"), 
                                   area = c("Central gulf", "Western gulf"),
                                   week = 1:as.numeric(format(Sys.Date(), format = "%W")))) %>% 
  tidytable::left_join(catch.x %>% 
                         tidytable::select(-quota)) %>% 
  tidytable::mutate(week_catch = replace_na(week_catch, 0)) %>% 
  tidytable::mutate(cum_catch = cumsum(week_catch), .by = c(year, gear, area)) %>% 
  tidytable::left_join(catch.y) %>% 
  tidytable::mutate(prop_alloc = cum_catch / quota,
                    period = case_when(year %in% (pre_coll + 1):collapsed ~ paste0("Collapsing (", (pre_coll + 1) - 2000, "-", collapsed - 2000, ")"),
                                       year %in% (new_year - 2):new_year ~ as.character(year),
                                       .default = NA)) %>% 
  tidytable::drop_na() %>% 
  tidytable::summarise(prop_alloc = mean(prop_alloc),
                       num_vess = round(mean(num_vess), digits = 0),
                       .by = c(period, gear, area, week)) %>% 
  tidytable::mutate(period = factor(period, levels = c(paste0("Collapsing (", (pre_coll + 1) - 2000, "-", collapsed - 2000, ")"),
                                                       as.character(new_year - 2),
                                                       as.character(new_year - 1),
                                                       as.character(new_year)))) -> cumul_catch

vess_table_data <- tot_catch %>% 
  tidytable::filter(year %in% seq(min(catch.y$year), max(catch.y$year)),
                    trip_target_name == "Pacific Cod",
                    fmp_gear %in% c("POT", "JIG")) %>% 
  tidytable::distinct(year, fmp_gear, fmp_subarea, vessel_id) %>% 
  tidytable::mutate(period = case_when(year %in% (pre_coll + 1):collapsed ~ paste0("Collapsing (", (pre_coll + 1) - 2000, "-", collapsed - 2000, ")"),
                                       year %in% (new_year - 2):new_year ~ as.character(year),
                                       .default = NA),
                    gear = case_when(fmp_gear == "POT" ~ "Pot",
                                     fmp_gear == "JIG" ~ "Jig"),
                    area = case_when(fmp_subarea == "WG" ~ "Western gulf",
                                     fmp_subarea == "CG" ~ "Central gulf",
                                     .default = NA)) %>% 
  tidytable::drop_na() %>% 
  tidytable::summarise(num_vess = .N, .by = c(year, period, gear, area)) %>% 
  tidytable::summarise(num_vess = round(mean(num_vess, digits = 0)), .by = c(period, gear, area)) %>%
  tidytable::distinct(period, gear, area, num_vess) %>%
  tidytable::mutate(period = factor(period, levels = c(paste0("Collapsing (", (pre_coll + 1) - 2000, "-", collapsed - 2000, ")"),
                                                       as.character(new_year - 2),
                                                       as.character(new_year - 1),
                                                       as.character(new_year)))) %>%
  tidytable::arrange(period) %>% 
  tidytable::rename(Year = period, Vessels = num_vess) %>% 
  tidytable::nest(tb = c(Year, Vessels)) %>%
  tidytable::mutate(x = Inf,
                    y = 0) %>% 
  tidytable::mutate(y = case_when(gear == "Jig" & area == "Western gulf" ~ Inf,
                                  .default = y))

ggplot(data = cumul_catch, 
       aes(x = week, y = prop_alloc, color = factor(period))) + 
  geom_point() + 
  geom_path(aes(group = period)) +
  facet_grid(gear ~ area, scale = "free_y") +
  theme_bw(base_size = 14) +
  theme(axis.text.x = element_text(vjust = 0.5, angle = 90),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  scico::scale_color_scico_d(palette = 'roma') +
  labs(x = "Week", y = "Cummulative Proportion of Allocated Quota", color = "Year") +
  ggpp::geom_table(data = vess_table_data, aes(x = x, y = y, label = tb),
                   table.theme = ggpp::ttheme_gtminimal(base_size = 12))
  

# plot for all years ----

tidytable::expand_grid(year = seq(min(catch.y$year), max(catch.y$year) - 1),
                       gear = c("Jig", "Pot"), 
                       area = c("Central gulf", "Western gulf"),
                       week = 1:52) %>% 
  tidytable::bind_rows(expand.grid(year = new_year, 
                                   gear = c("Jig", "Pot"), 
                                   area = c("Central gulf", "Western gulf"),
                                   week = 1:as.numeric(format(Sys.Date(), format = "%W")))) %>% 
  tidytable::left_join(catch.x %>% 
                         tidytable::select(-quota)) %>% 
  tidytable::mutate(week_catch = replace_na(week_catch, 0)) %>% 
  tidytable::mutate(cum_catch = cumsum(week_catch), .by = c(year, gear, area)) %>% 
  tidytable::summarise(cum_catch = sum(cum_catch), .by = c(year, gear, area, week)) %>% 
  tidytable::left_join(catch.y %>%
                         tidytable::summarise(quota = sum(quota),
                                              num_vess = round(mean(num_vess), digits = 0), .by = c(year, gear, area))) %>% 
  tidytable::mutate(prop_alloc = cum_catch / quota) -> cumul_catch

ggplot(data = cumul_catch, 
       aes(x = week, y = prop_alloc, color = factor(year))) + 
  geom_point() + 
  geom_path(aes(group = year)) +
  facet_grid(gear ~ area, scale = "free_y") +
  theme_bw(base_size = 14) +
  theme(axis.text.x = element_text(vjust = 0.5, angle = 90)) +
  scico::scale_color_scico_d(palette = 'roma') +
  labs(x = "Week", y = "Cummulative Proportion of Allocated Quota", color = "Year")



# plot for cgoa jig----

tidytable::expand_grid(year = seq(min(catch.y$year), max(catch.y$year) - 1), #c(2015:2018, seq(new_year - 2, new_year - 1)),
                       gear = c("Jig"), 
                       area = c("Central gulf"),
                       week = 1:52) %>% 
  tidytable::bind_rows(expand.grid(year = new_year, 
                                   gear = c("Jig"), 
                                   area = c("Central gulf"),
                                   week = 1:as.numeric(format(Sys.Date(), format = "%W")))) %>% 
  tidytable::left_join(catch.x %>% 
                         tidytable::select(-quota)) %>% 
  tidytable::mutate(week_catch = replace_na(week_catch, 0)) %>% 
  tidytable::mutate(cum_catch = cumsum(week_catch), .by = c(year, gear, area)) %>%
  tidytable::summarise(cum_catch = sum(cum_catch), .by = c(year, gear, area, week)) %>% 
  tidytable::left_join(catch.y) %>% 
  tidytable::mutate(prop_alloc = cum_catch / quota) %>% 
  tidytable::drop_na() -> cumul_catch

vess_text <- cumul_catch %>% 
  tidytable::filter(week == 52) %>% 
  tidytable::bind_rows(cumul_catch %>% 
                         tidytable::filter(year == new_year & week == as.numeric(format(Sys.Date(), format = "%W")))) %>% 
  tidytable::mutate(week = 55)

ggplot(data = cumul_catch, 
       aes(x = week, y = prop_alloc, color = factor(year))) + 
  geom_point() + 
  geom_path(aes(group = year)) +
  facet_grid(gear ~ area, scale = "free_y") +
  geom_text(data = vess_text, aes(x = week, y = prop_alloc, label = year), show.legend = FALSE) +
  theme_bw(base_size = 14) +
  theme(axis.text.x = element_text(vjust = 0.5, angle = 90)) +
  scico::scale_color_scico_d(palette = 'roma') +
  ylim(0, 1) +
  labs(x = "Week", y = "Cummulative Proportion of Allocated Quota", color = "Year")





