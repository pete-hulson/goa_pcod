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

# recommended model name
rec_model <- "2019.1e.5cm-2024"


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
  tidytable::mutate(tot = fed_tot + st_tot) %>%
  tidytable::mutate(across(.cols = names(.)[2:length(names(.))], ~format(., big.mark = ","))) -> juris_gr_tbl

vroom::vroom_write(juris_gr_tbl, here::here(new_year, 'tables', 'juris_gr_tbl.csv'), delim = ",")

# catch-tac-abc-ofl-ghl ----
db = 'akfin'
conn = afscdata::connect(db)  

vroom::vroom(here::here(new_year, 'data', 'old_abc_tac.csv')) %>% 
  tidytable::mutate(across(.cols = names(.)[2:length(names(.))], ~format(., big.mark = ","))) %>% 
  tidytable::mutate(ofl = case_when(ofl == "     NA" ~ "-",
                                    .default = ofl),
                    ghl = case_when(ghl == "    NA" ~ "-",
                                    .default = ghl)) %>% 
  tidytable::bind_rows(afscdata::q_specs(year = new_year,
                                         species = "PCOD",
                                         area = "GOA",
                                         db = conn,
                                         save = FALSE) %>% 
                         tidytable::arrange(year) %>% 
                         tidytable::filter(area_label == 'GOA',
                                           year <= new_year) %>% 
                         tidytable::select(year, tac = total_allowable_catch, abc = acceptable_biological_catch, ofl = overfishing_level) %>% 
                         tidytable::mutate(ghl = abc - tac,
                                           across(.cols = names(.)[2:length(names(.))], ~format(., big.mark = ","))) %>% 
                         tidytable::left_join(juris_gr_tbl %>% 
                                                tidytable::select(year, catch = tot))) %>% 
  tidytable::rename(Year = year, Catch = catch, TAC = tac, ABC = abc, OFL = ofl, GHL = ghl)-> tac_abc_tbl

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
  tidytable::mutate(total = discarded + retained) %>%
  tidytable::mutate(across(.cols = names(.)[2:length(names(.))], ~format(., big.mark = ","))) %>% 
  tidytable::rename(Year = year, Discarded = discarded, Retained = retained, Total = total) -> dr_tbl

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
  tidytable::summarise(catch = format(round(sum(weight_posted), digits = 0), big.mark = ","), .by = c(year, trip_target_name)) %>% 
  tidytable::pivot_wider(names_from = year, values_from = catch) %>% 
  # add avg column
  tidytable::left_join(fed_raw %>% 
                         tidytable::filter(year >= new_year - 4) %>% 
                         tidytable::summarise(catch = sum(weight_posted), .by = c(year, trip_target_name)) %>% 
                         tidytable::summarise(Average = round(mean(catch), digits = 0), .by = c(trip_target_name))) %>% 
  tidytable::arrange(-Average) %>%
  tidytable::mutate(Average = format(Average, big.mark = ",")) %>% 
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
                                                tidytable::mutate(trip_target_name = 'Grand Total')) %>% 
                         tidytable::mutate(across(.cols = names(.)[2:length(names(.))], ~format(., big.mark = ",")))) %>% 
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
                                                tidytable::mutate(trip_target_name = 'Non-Pacific cod trip target total')) %>% 
                         tidytable::mutate(across(.cols = names(.)[2:length(names(.))], ~format(., big.mark = ",")))) %>% 
  tidytable::rename("Trip Target" = trip_target_name) %>% 
  vroom::vroom_write(., here::here(new_year, 'tables', 'catch_by_fshy.csv'), delim = ",")


# survey indices ----

bts_raw <- vroom::vroom(here::here(new_year, "data", "raw", "twl_srvy_index.csv"))
lls_raw <- vroom::vroom(here::here(new_year, 'data', 'raw', 'lls_rpn_geoarea_data.csv'))

lls_raw %>% 
  tidytable::filter(year >= 1990) %>% 
  tidytable::summarise(RPN = sum(rpn),
                       RPN_var = sum(rpn_var), .by = year) %>% 
  tidytable::mutate(RPN = paste0(format(round(RPN, digits = 0), big.mark = ","), " (", round(sqrt(RPN_var) / RPN, digits = 2),")")) %>% 
  tidytable::select(-RPN_var) %>% 
  tidytable::left_join(bts_raw %>% 
                         tidytable::filter(strata == 99903) %>% 
                         tidytable::summarise(Biomass = sum(biom),
                                              biom_var = sum(biom_var),
                                              Abundance = sum(num),
                                              num_var = sum(num_var), .by = year) %>% 
                         tidytable::mutate(Biomass = paste0(format(round(Biomass, digits = 0), big.mark = ","), " (", round(sqrt(biom_var) / Biomass, digits = 2), ")"),
                                           Abundance = paste0(format(round(Abundance / 1000, digits = 0), big.mark = ","), " (", round(sqrt(num_var) / Abundance, digits = 2), ")")) %>% 
                         tidytable::select(-biom_var, -num_var)) %>%
  tidytable::mutate(across(.cols = names(.)[2:length(names(.))], ~replace(., is.na(.), "-")))  %>% 
  tidytable::rename(Year = year, "Biomass (t)" = Biomass) %>% 
  vroom::vroom_write(., here::here(new_year, 'tables', 'surv_indx.csv'), delim = ",")


# number of parameters table ----

# read in rec model output
model_res <- r4ss::SS_output(dir = here::here(new_year, "mgmt", rec_model),
                             verbose = TRUE,
                             printstats = TRUE)

# set up helper fcns
sing_param_table <- function(par_res, pattern, par_name){
  par_res %>% 
    tidytable::filter(Phase > 0) %>% 
    tidytable::select(Label, Value) %>% 
    tidytable::slice(grep(pattern, Label, perl = TRUE)) %>% 
    tidytable::mutate(!!par_name := length(Label)) %>% 
    tidytable::distinct(!!par_name) %>% 
    tidytable::pivot_longer(names_to = 'Parameter', values_to = 'Number')
}

dub_param_table <- function(par_res, pattern1, pattern2, par_name1, par_name2){
  par_res %>% 
    tidytable::filter(Phase > 0) %>% 
    tidytable::select(Label, Value) %>% 
    tidytable::slice(grep(pattern1, Label, perl = TRUE)) %>% 
    tidytable::mutate(!!par_name1 := length(Label) - length(grep(pattern2, Label, perl = TRUE)),
                      !!par_name2 := length(grep(pattern2, Label, perl = TRUE))) %>% 
    tidytable::distinct(!!par_name1, !!par_name2) %>% 
    tidytable::pivot_longer(names_to = 'Parameter', values_to = 'Number')
}

# get table
# early init devs
sing_param_table(par_res = model_res$parameters, 
                 pattern = "Early_Init", 
                 par_name = 'Early Init Devs') %>% 
  # early rec dev
  tidytable::bind_rows(sing_param_table(par_res = model_res$parameters, 
                                        pattern = "Early_RecrDev", 
                                        par_name = 'Early Rec Dev')) %>% 
  # main rec devs
  tidytable::bind_rows(sing_param_table(par_res = model_res$parameters, 
                                        pattern = "Main_RecrDev", 
                                        par_name = 'Main Rec Dev')) %>% 
  # r_zero
  tidytable::bind_rows(sing_param_table(par_res = model_res$parameters, 
                                        pattern = "SR_LN", 
                                        par_name = 'log(mean recruitment)')) %>% 
  # 76 regime
  tidytable::bind_rows(sing_param_table(par_res = model_res$parameters, 
                                        pattern = "SR_regime", 
                                        par_name = '1976 R reg.')) %>% 
  # m and growth
  tidytable::bind_rows(dub_param_table(par_res = model_res$parameters,
                                       pattern1 = "Fem_GP_1",
                                       pattern2 = "NatM", 
                                       par_name1 = 'Growth', 
                                       par_name2 = 'Natural Mortality')) %>% 
  # catchabilities
  tidytable::bind_rows(dub_param_table(par_res = model_res$parameters,
                                       pattern1 = "LnQ",
                                       pattern2 = "LLSrv", 
                                       par_name1 = 'Survey catchability (trawl)', 
                                       par_name2 = 'Survey catchability (longline)')) %>% 
  # survey selex
  tidytable::bind_rows(dub_param_table(par_res = model_res$parameters,
                                       pattern1 = "(?=.*Size_DblN)(?=.*Srv)",
                                       pattern2 = "(?=.*Size_DblN)(?=.*LLSrv)", 
                                       par_name1 = 'Trawl survey selex', 
                                       par_name2 = 'Longline survey selex')) %>% 
  # trawl fishery selex
  tidytable::bind_rows(dub_param_table(par_res = model_res$parameters,
                                       pattern1 = "FshTrawl",
                                       pattern2 = "DEVmult", 
                                       par_name1 = 'Trawl fishery selex', 
                                       par_name2 = 'Trawl fishery selex devs')) %>% 
  # longline fishery selex
  tidytable::bind_rows(dub_param_table(par_res = model_res$parameters,
                                       pattern1 = "FshLL",
                                       pattern2 = "DEVmult", 
                                       par_name1 = 'Longline fishery selex', 
                                       par_name2 = 'Longline fishery selex devs')) %>% 
  # pot fishery selex
  tidytable::bind_rows(sing_param_table(par_res = model_res$parameters, 
                                        pattern = "FshPot", 
                                        par_name = 'Pot fishery selex')) -> param_tbl

# add total # params line
param_tbl %>% 
  tidytable::bind_rows(param_tbl %>% 
                         tidytable::summarise(Number = sum(Number)) %>% 
                         tidytable::mutate(Parameter = 'Total')) %>% 
  vroom::vroom_write(., here::here(new_year, 'tables', 'param_tbl.csv'), delim = ",")

# likelihood and derived quants table ----

model_res$likelihoods_used %>% 
  tidytable::mutate(Component = rownames(.)) %>% 
  tidytable::select(Component, Value = values) %>% 
  tidytable::mutate(Value = round(Value, digits = 2)) %>% 
  tidytable::filter(Component == "TOTAL") %>% 
  tidytable::mutate(Component = "Total negative log-likelihood") %>% 
  tidytable::bind_rows(data.frame(Component = "Survey indices", Value = "--")) %>% 
  tidytable::bind_rows(model_res$likelihoods_by_fleet %>% 
                         tidytable::filter(Label == "Surv_like") %>% 
                         tidytable::select(-Label) %>% 
                         tidytable::pivot_longer(names_to = "Component", values_to = "Value") %>% 
                         tidytable::filter(Value != 0,
                                           Component != "ALL") %>% 
                         tidytable::mutate(Value = round(Value, digits = 2)) %>% 
                         tidytable::mutate(Component = c("Bottom trawl survey index", 
                                                         "Longline survey index"))) %>% 
  tidytable::bind_rows(data.frame(Component = "Length composition", Value = "--")) %>% 
  tidytable::bind_rows(model_res$likelihoods_by_fleet %>% 
                         tidytable::filter(Label == "Length_like") %>% 
                         tidytable::select(-Label) %>% 
                         tidytable::pivot_longer(names_to = "Component", values_to = "Value") %>% 
                         tidytable::filter(Value != 0,
                                           Component != "ALL") %>% 
                         tidytable::mutate(Value = round(Value, digits = 2)) %>% 
                         tidytable::mutate(Component = c("Trawl fishery length composition", 
                                                         "Longline fishery length composition",
                                                         "Pot fishery length composition",
                                                         "Bottom trawl survey length composition",
                                                         "Longline survey length composition"))) %>% 
  tidytable::bind_rows(data.frame(Component = "Conditional age-at-length", Value = "--")) %>% 
  tidytable::bind_rows(model_res$likelihoods_by_fleet %>% 
                         tidytable::filter(Label == "Age_like") %>% 
                         tidytable::select(-Label) %>% 
                         tidytable::pivot_longer(names_to = "Component", values_to = "Value") %>% 
                         tidytable::filter(Value != 0,
                                           Component != "ALL") %>% 
                         tidytable::mutate(Value = round(Value, digits = 2)) %>% 
                         tidytable::mutate(Component = c("Trawl fishery CAAL", 
                                                         "Longline fishery CAAL",
                                                         "Pot fishery CAAL",
                                                         "Bottom trawl survey CAAL"))) %>% 
  tidytable::bind_rows(data.frame(Component = "Parameter deviations and priors", Value = "--")) %>% 
  tidytable::bind_rows(model_res$likelihoods_used %>% 
                         tidytable::mutate(Component = rownames(.)) %>% 
                         tidytable::select(Component, Value = values) %>% 
                         tidytable::mutate(Value = round(Value, digits = 2)) %>% 
                         tidytable::filter(Component %in% c("Recruitment", "InitEQ_Regime", "Parm_priors", "Parm_devs")) %>% 
                         tidytable::mutate(Component = c("Recruitment deviations",
                                                         "Initial abundance deviations",
                                                         "Parameter priors",
                                                         "Selectivity deviations"))) %>% 
  vroom::vroom_write(., here::here(new_year, 'tables', 'likes_tbl.csv'), delim = ",")

# key parameter table ----

data.frame(Name = "Key parameters", Value = "--") %>% 
  tidytable::bind_rows(model_res$parameters %>% 
                         tidytable::select(Label, Value) %>% 
                         tidytable::filter(Label %in% c("SR_LN(R0)",
                                                        "NatM_uniform_Fem_GP_1",
                                                        "NatM_uniform_Fem_GP_1_BLK4repl_2014", 
                                                        "L_at_Amin_Fem_GP_1", 
                                                        "L_at_Amax_Fem_GP_1", 
                                                        "VonBert_K_Fem_GP_1", 
                                                        "LnQ_base_Srv(4)",
                                                        "LnQ_base_LLSrv(5)",
                                                        "LnQ_base_LLSrv(5)_ENV_mult")) %>% 
                         tidytable::mutate(Name = c("Natural mortality (all years)",
                                                    "Length at age-0 (cm)",
                                                    "Length at age-10 (cm)",
                                                    "Growth rate",
                                                    "Natural mortality (2014-2016)",
                                                    "log(mean recruitment)",
                                                    "Catchability: trawl survey",
                                                    "Catchability: longline survey",
                                                    "Catchability: longline survey environmental coefficient")) %>% 
                         tidytable::mutate(Value = case_when(Name %in% c("Catchability: trawl survey", "Catchability: longline survey") ~ round(exp(Value), digits = 2),
                                                             .default = round(Value, digits = 2))) %>% 
                         tidytable::select(Name, Value) %>% 
                         tidytable::arrange(Name)) %>% 
  tidytable::bind_rows(data.frame(Name = "Key derived quantities", Value = "--")) %>% 
  tidytable::bind_rows(model_res$derived_quants %>% 
                         tidytable::select(Label, Value) %>% 
                         tidytable::filter(Label %in% c("Recr_Virgin", 
                                                        "SSB_Virgin", 
                                                        paste0("F_", new_year + 10), 
                                                        paste0("F_", new_year + 1), 
                                                        paste0("SSB_", new_year + 1), 
                                                        paste0("SSB_", new_year + 2),
                                                        "SSB_unfished")) %>% 
                         tidytable::mutate(Value = case_when(Label == "Recr_Virgin" ~ round(Value / 1000, digits = 2),
                                                             Label == "SSB_Virgin" ~ round((Value / 2) / 1000, digits = 2),
                                                             .default = round(Value, digits = 2)),
                                           unfished = .$Value[Label == "SSB_unfished"]) %>% 
                         tidytable::mutate(Value = case_when(Label %in% c(paste0("SSB_", new_year + 1), paste0("SSB_", new_year + 2)) ~ round(Value / unfished, digits = 2),
                                                             .default = Value)) %>% 
                         tidytable::filter(Label != "SSB_unfished") %>% 
                         tidytable::select(-unfished) %>% 
                         tidytable::mutate(Name = c("Unfished SSB (1,000's t)", 
                                                    paste("SSBratio", new_year + 1),
                                                    paste("SSBratio", new_year + 2),
                                                    "Virgin recruitment (mil)",
                                                    "F_ABC (sum apical F)",
                                                    "F_MSY (sum apical F)")) %>% 
                         tidytable::select(Name, Value) %>% 
                         tidytable::arrange(Name)) %>% 
  vroom::vroom_write(., here::here(new_year, 'tables', 'key_param_tbl.csv'), delim = ",")













