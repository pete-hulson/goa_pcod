



# SAFE catch tables ----



if (!dir.exists(here::here(new_year, "tables"))) {
  dir.create(here::here(new_year, "tables"), recursive = TRUE)
}





# catch by gear type and jurisdiction ----

# read in data
fed_raw <- vroom::vroom(here::here(new_year, "data", "raw", "fish_catch_data.csv"))
adfg_raw <- vroom::vroom(here::here(new_year, 'data', 'raw', 'adfg_catch.csv'))


# get federal catch by gear
fed_raw %>% 
  tidytable::select(year, 
                    zone = fmp_subarea,
                    species_group = species_group_code, 
                    gear = fmp_gear,
                    type = retained_or_discarded, 
                    week_end_date, 
                    weight_posted,
                    state_flag = akr_state_fishery_flag) %>%  
  tidytable::mutate(state_flag = replace_na(state_flag, "N")) %>% 
  filter(state_flag != "Y") %>% 
  tidytable::mutate(gear_desc = case_when(gear == "TRW" ~ "fed_trawl",
                                          gear == "HAL" ~ "fed_longline",
                                          gear == "POT" ~ "fed_pot",
                                          gear %in% c("JIG", "OTH", "GLN") ~ "fed_other")) %>% 
  tidytable::summarise(catch = round(sum(weight_posted)), .by = c(year, gear_desc)) %>% 
  tidytable::pivot_wider(names_from = gear_desc, values_from = catch) %>% 
  tidytable::select(year, fed_trawl, fed_longline, fed_pot, fed_other) %>% 
  tidytable::mutate(fed_other = replace_na(fed_other, 0)) %>% 
  tidytable::mutate(fed_tot = fed_trawl + fed_longline + fed_pot + fed_other) %>% 
  # get state catch by gear
  tidytable::left_join(adfg_raw %>% 
                         tidytable::select(year = akfin_year,
                                           gear = fmp_gear,
                                           weight_posted = catch_mt) %>% 
                         tidytable::mutate(gear_desc = case_when(gear == "HAL" ~ "st_longline",
                                                                 gear == "POT" ~ "st_pot",
                                                                 gear %in% c("JIG", "OTH", "GLN") ~ "st_other")) %>% 
                         tidytable::summarise(catch = round(sum(weight_posted)), .by = c(year, gear_desc)) %>% 
                         tidytable::pivot_wider(names_from = gear_desc, values_from = catch) %>% 
                         tidytable::select(year, st_longline, st_pot, st_other) %>% 
                         tidytable::mutate(st_longline = replace_na(st_longline, 0)) %>% 
                         tidytable::mutate(st_tot = st_longline + st_pot + st_other) %>% 
                         tidytable::bind_rows(fed_raw %>% 
                                                tidytable::select(year, 
                                                                  zone = fmp_subarea,
                                                                  species_group = species_group_code, 
                                                                  gear = fmp_gear,
                                                                  type = retained_or_discarded, 
                                                                  week_end_date, 
                                                                  weight_posted,
                                                                  state_flag = akr_state_fishery_flag) %>%  
                                                tidytable::mutate(state_flag = replace_na(state_flag, "N")) %>% 
                                                filter(state_flag == "Y") %>% 
                                                tidytable::mutate(gear_desc = case_when(gear == "HAL" ~ "st_longline",
                                                                                        gear == "POT" ~ "st_pot",
                                                                                        gear %in% c("JIG", "OTH", "GLN") ~ "st_other"))  %>% 
                                                tidytable::summarise(catch = round(sum(weight_posted)), .by = c(year, gear_desc)) %>% 
                                                tidytable::pivot_wider(names_from = gear_desc, values_from = catch) %>% 
                                                tidytable::select(year, st_longline, st_pot, st_other) %>% 
                                                tidytable::mutate(st_other = replace_na(st_other, 0)) %>% 
                                                tidytable::mutate(st_tot = st_longline + st_pot + st_other))) %>% 
  base::replace(is.na(.), 0) %>% 
  tidytable::mutate(tot = fed_tot + st_tot) -> juris_gr_tbl

vroom::vroom_write(juris_gr_tbl, here::here(new_year, 'tables', 'juris_gr_tbl.csv'), delim = ",")

# catch-tac-abc-ofl-ghl ----

vroom::vroom(here::here(new_year, 'data', 'old_abc_tac.csv')) %>% 
  tidytable::bind_rows(afscdata::q_specs(year = new_year,
                                         species = "PCOD",
                                         area = "GOA",
                                         db = conn,
                                         save = FALSE) %>% 
                         tidytable::arrange(year) %>% 
                         tidytable::filter(area_label == 'GOA',
                                           year <= new_year) %>% 
                         tidytable::select(year, tac = total_allowable_catch, abc = acceptable_biological_catch, ofl = overfishing_level) %>% 
                         tidytable::mutate(ghl = abc - tac) %>% 
                         tidytable::left_join(juris_gr_tbl %>% 
                                                tidytable::select(year, catch = tot))) -> tac_abc_tbl

vroom::vroom_write(tac_abc_tbl, here::here(new_year, 'tables', 'tac_abc_tbl.csv'), delim = ",")

# retained/discarded table ----
fed_raw %>% 
  tidytable::select(year,
                    type = retained_or_discarded,
                    weight_posted) %>% 
  tidytable::summarise(catch = round(sum(weight_posted)), .by = c(year, type)) %>%  
  tidytable::pivot_wider(names_from = type, values_from = catch) %>% 
  tidytable::left_join(adfg_raw %>% 
                         tidytable::select(year = akfin_year,
                                           weight_posted = catch_mt) %>% 
                         tidytable::summarise(catch = round(sum(weight_posted)), .by = c(year))) %>% 
  base::replace(is.na(.), 0) %>% 
  tidytable::mutate(retained = R + catch) %>% 
  tidytable::rename(discarded = D) %>% 
  tidytable::select(year, discarded, retained) %>% 
  tidytable::mutate(total = discarded + retained) -> dr_tbl

vroom::vroom_write(dr_tbl, here::here(new_year, 'tables', 'dr_tbl.csv'), delim = ",")

# bycatch table ----

afscdata::q_nontarget(year = new_year,
                      target = "c",
                      area = "goa",
                      db = conn,
                      save = FALSE)

# non-target table ----




# prohib species table ----
afscdata::q_psc(year = new_year,
                target = "c",
                area = "goa",
                db = conn,
                save = FALSE) %>% 
  vroom::vroom_write(., here::here(new_year, 'tables', 'psc.csv'), delim = ",")


# cod catch in other fisheries ----

# catch by fishery
fed_raw %>% 
  tidytable::filter(year >= new_year - 4) %>% 
  tidytable::summarise(catch = round(sum(weight_posted), digits = 0), .by = c(year, trip_target_name)) %>% 
  tidytable::pivot_wider(names_from = year, values_from = catch) %>% 
  # add avg column
  tidytable::left_join(fed_raw %>% 
                         tidytable::filter(year >= new_year - 4) %>% 
                         tidytable::summarise(catch = sum(weight_posted), .by = c(year, trip_target_name)) %>% 
                         tidytable::summarise(avg = round(mean(catch), digits = 0), .by = c(trip_target_name))) %>% 
  tidytable::arrange(-avg) %>%
  tidytable::mutate(across(.cols = names(.)[2:length(names(.))], ~replace(., is.na(.), "-"))) %>% 
  # add grand total line
  tidytable::bind_rows(fed_raw %>% 
                         tidytable::filter(year >= new_year - 4) %>% 
                         tidytable::summarise(catch = round(sum(weight_posted), digits = 0), .by = c(year, trip_target_name)) %>% 
                         tidytable::summarise(catch = sum(catch), .by = c(year)) %>% 
                         tidytable::mutate(trip_target_name = 'Grand Total') %>% 
                         tidytable::pivot_wider(names_from = year, values_from = catch) %>% 
                         tidytable::left_join(fed_raw %>% 
                                                tidytable::filter(year >= new_year - 4) %>% 
                                                tidytable::summarise(catch = round(sum(weight_posted), digits = 0), .by = c(year, trip_target_name)) %>% 
                                                tidytable::summarise(catch = sum(catch), .by = c(year)) %>% 
                                                tidytable::summarise(avg = round(mean(catch), digits = 0)) %>% 
                                                tidytable::mutate(trip_target_name = 'Grand Total'))) %>% 
  # add non-pcod trip total line
  tidytable::bind_rows(fed_raw %>% 
                         tidytable::filter(year >= new_year - 4,
                                           trip_target_name != 'Pacific Cod') %>% 
                         tidytable::summarise(catch = round(sum(weight_posted), digits = 0), .by = c(year, trip_target_name)) %>% 
                         tidytable::summarise(catch = sum(catch), .by = c(year)) %>% 
                         tidytable::mutate(trip_target_name = 'Non-Pacific cod trip target total') %>% 
                         tidytable::pivot_wider(names_from = year, values_from = catch) %>% 
                         tidytable::left_join(fed_raw %>% 
                                                tidytable::filter(year >= new_year - 4,
                                                                  trip_target_name != 'Pacific Cod') %>% 
                                                tidytable::summarise(catch = round(sum(weight_posted), digits = 0), .by = c(year, trip_target_name)) %>% 
                                                tidytable::summarise(catch = sum(catch), .by = c(year)) %>% 
                                                tidytable::summarise(avg = round(mean(catch), digits = 0)) %>% 
                                                tidytable::mutate(trip_target_name = 'Non-Pacific cod trip target total'))) %>% 
  vroom::vroom_write(., here::here(new_year, 'tables', 'catch_by_fshy.csv'), delim = ",")








