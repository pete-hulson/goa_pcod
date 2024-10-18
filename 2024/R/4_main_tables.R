# script to compile tables for safe document
# developed in 2024 by p hulson


# load necessary packages ----
## cran packages ----
pkg_cran <- c("tidyverse",
              "vroom",
              "here")

# if not installed, then install
if(length(pkg_cran[which(pkg_cran %in% rownames(installed.packages()) == FALSE )]) > 0) {
  install.packages(pkg_cran[which(pkg_cran %in% rownames(installed.packages()) == FALSE)])
}

# load packages
lapply(pkg_cran, library, character.only = TRUE)

## github packages ----
pkg_git <- c("r4ss",
             "afscdata")

# if not installed, then install
if(!isTRUE("r4ss" %in% rownames(installed.packages()))) {
  devtools::install_github("r4ss/r4ss", force = TRUE)
}
if(!isTRUE("afscdata" %in% rownames(installed.packages()))) {
  devtools::install_github("afsc-assessments/afscdata", force = TRUE)
}

# load packages
lapply(pkg_git, library, character.only = TRUE)

# set up ----

# Current assessment year
new_year <- as.numeric(format(Sys.Date(), format = "%Y"))

# set up directly to plop tables into
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
bycatch <- vroom::vroom(here::here(new_year, 'data', 'raw', 'bycatch.csv'))

bycatch %>% 
  tidytable::filter(catch > 0,
                    !(species_name %in% c("sculpin, general",
                                          "sculpin, great",
                                          "sculpin, other large",
                                          "sculpin, yellow irish lord",
                                          "halibut, Pacific",
                                          "groundfish, general"))) %>% 
  tidytable::mutate(Stock = case_when(species_name %in% c("flounder, Alaska plaice",
                                                          "sole, butter",
                                                          "sole, English",
                                                          "sole, sand",
                                                          "flounder, starry",
                                                          "sole, yellowfin",
                                                          "sole, rock",
                                                          "flounder, general",
                                                          "sole, petrale") ~ "Shallow-water flatfish",
                                      species_name %in% c("sole, dover",
                                                          "turbot, Greenland",
                                                          "Kamchatka flounder") ~ "Deep-water flatfish",
                                      species_name %in% c("rockfish, canary",
                                                          "rockfish, yelloweye (red snapper)",
                                                          "rockfish, quillback",
                                                          "rockfish, copper",
                                                          "rockfish, rosethorn",
                                                          "rockfish, china",
                                                          "rockfish, tiger") ~ "Demersal shelf rockfish",
                                      species_name %in% c("rockfish, harlequin",
                                                          "rockfish, other",
                                                          "rockfish, redbanded",
                                                          "rockfish, redstripe",
                                                          "rockfish, sharpchin",
                                                          "rockfish, silvergray",
                                                          "rockfish, widow",
                                                          "rockfish, yellowtail") ~ "Other rockfish",
                                      species_name %in% c("Pacific sleeper shark",
                                                          "shark, other",
                                                          "shark, salmon",
                                                          "shark, spiny dogfish") ~ "Skarks",
                                      species_name %in% c("skate, Alaskan",
                                                          "skate, Aleutian",
                                                          "skate, big",
                                                          "skate, longnose",
                                                          "skate, other") ~ "Skates",
                                      species_name == "flounder, arrowtooth" ~ "Arrowtooth flounder",
                                      species_name == "greenling, atka mackerel" ~ "Atka mackerel",
                                      species_name == "octopus, North Pacific" ~ "Octopus",
                                      species_name == "perch, Pacific ocean" ~ "Pacific Ocean perch",
                                      species_name == "pollock, walleye" ~ "Walleye pollock",
                                      species_name == "rockfish, dusky" ~ "Dusky rockfish",
                                      species_name == "rockfish, northern" ~ "Northern rockfish",
                                      species_name == "rockfish, rougheye" ~ "Rougheye and Blackspotted rockfish",
                                      species_name == "rockfish, shortraker" ~ "Shortraker rockfish",
                                      species_name == "rockfish, thornyhead (idiots)" ~ "Thornyheads",
                                      species_name == "sablefish (blackcod)" ~ "Sablefish",
                                      species_name == "sole, flathead" ~ "Flathead sole",
                                      species_name == "sole, rex" ~ "Rex sole",
                                      .default = species_name)) %>% 
  tidytable::summarise(catch = round(sum(catch), digits = 1), .by = c(year, Stock, retained_or_discarded)) %>% 
  tidytable::bind_rows(bycatch %>% 
                         tidytable::summarise(catch = round(sum(catch), digits = 1), .by = c(year, retained_or_discarded)) %>% 
                         tidytable::mutate(Stock = "Total")) %>% 
  tidytable::pivot_wider(names_from = c(year, retained_or_discarded), values_from = catch) %>%
  tidytable::mutate(across(.cols = names(.)[2:length(names(.))], ~replace(., is.na(.), "-"))) -> bycatch_tbl

vroom::vroom_write(bycatch_tbl, here::here(new_year, 'tables', 'bycatch.csv'), delim = ",")


# non-target table ----
# note for future: will need to figure out how to filter out those not included/confidential
# get connected
db = 'akfin'
conn = afscdata::connect(db)  

afscdata::q_nontarget(year = new_year,
                      target = "c",
                      area = "goa",
                      db = conn,
                      save = FALSE) %>%
  tidytable::mutate(across(.cols = names(.)[2:length(names(.))], ~round(., digits = 2)),
                    across(.cols = names(.)[2:length(names(.))], ~replace(., is.na(.), "-"))) %>% 
  tidytable::rename("Species Group" = species) %>% 
  vroom::vroom_write(., here::here(new_year, 'tables', 'nontarget.csv'), delim = ",")


# prohib species table ----

# get connected
db = 'akfin'
conn = afscdata::connect(db)  

afscdata::q_psc(year = new_year,
                target = "c",
                area = "goa",
                db = conn,
                save = FALSE) %>% 
  vroom::vroom_write(., here::here(new_year, 'tables', 'psc.csv'), delim = ",")


# cod catch in other fisheries ----

# catch by fishery
fed_raw %>% 
  tidytable::filter(year >= new_year - 4,
                    trip_target_name != "Other Species") %>% 
  tidytable::summarise(catch = round(sum(weight_posted), digits = 0), .by = c(year, trip_target_name)) %>% 
  tidytable::pivot_wider(names_from = year, values_from = catch) %>% 
  # add avg column
  tidytable::left_join(fed_raw %>% 
                         tidytable::filter(year >= new_year - 4) %>% 
                         tidytable::summarise(catch = sum(weight_posted), .by = c(year, trip_target_name)) %>% 
                         tidytable::summarise(Average = round(mean(catch), digits = 0), .by = c(trip_target_name))) %>% 
  tidytable::arrange(-Average) %>%
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
                                                tidytable::summarise(Average = round(mean(catch), digits = 0)) %>% 
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
                                                tidytable::summarise(Average = round(mean(catch), digits = 0)) %>% 
                                                tidytable::mutate(trip_target_name = 'Non-Pacific cod trip target total'))) %>% 
  tidytable::rename("Trip Target" = trip_target_name) %>% 
  vroom::vroom_write(., here::here(new_year, 'tables', 'catch_by_fshy.csv'), delim = ",")








