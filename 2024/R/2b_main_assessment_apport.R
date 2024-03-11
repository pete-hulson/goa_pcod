## Script to run 2023 GOA Pacific Cod Assessment apportionment (P. Hulson)

# Load required packages & define parameters ----

libs <- c("data.table",
          "dplyr",
          "ggplot2",
          "magrittr", 
          "r4ss",
          "adnuts",
          "afscdata",
          "rema")

if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )]) > 0) {
  install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE)])}

lapply(libs, library, character.only = TRUE)

# Current model name
Model_name_old <- "2019.1a-2023"
Model_name_new <- "2019.1b-2023"

# Current assessment year
new_SS_dat_year <- as.numeric(format(Sys.Date(), format = "%Y"))


# get data ----
afsc_species = 21720

akfin = connect()

afscdata::q_bts_biomass(year = new_SS_dat_year, 
                        area = "GOA", 
                        species = afsc_species, 
                        type = 'area', 
                        db = akfin) 


# run rema model for e/c/wgoa ----
biomass_dat <- read.csv(here::here(new_SS_dat_year,'data','raw','goa_area_bts_biomass_data.csv')) %>% # use area-specific dataframe
  tidytable::mutate(sd = sqrt(biomass_var),
                    cv = sd / area_biomass) %>%
  tidytable::select(strata = regulatory_area_name,
                    year,
                    biomass = area_biomass,
                    cv) %>%
  tidytable::filter(year >= 1990) # biomass data with dimensions strata, year, biomass, cvs (not logged)

apport_in <- rema::prepare_rema_input(model_name = paste0("TMB: GOA PCOD MULTIVAR"),
                                      biomass_dat  = bind_rows(biomass_dat))

apport_mdl <- rema::fit_rema(apport_in)

apport_out <- rema::tidy_rema(rema_model = apport_mdl)

save(apport_out, file = here::here(new_SS_dat_year, 'output', 'rema_output.rdata'))

apport_plots <- rema::plot_rema(tidy_rema = apport_out, biomass_ylab = 'Biomass (t)') # optional y-axis label

suppressWarnings(ggplot2::ggsave(apport_plots$biomass_by_strata,
                                 file = here::here(new_SS_dat_year, "plots", 'other','rema_outs.png'),
                                 width = 12, height = 7, unit = 'in', dpi = 520))


# make apportionment table ----

exec_summ <- read.csv(here::here(new_SS_dat_year,'output','mgmnt_exec_summ.csv')) %>% 
  tidytable::mutate(C_ABC = round(C_ABC, digits = 0)) %>% 
  tidytable::select(Yr, C_ABC)

apport_out$proportion_biomass_by_strata %>% 
  tidytable::filter(year == new_SS_dat_year) %>% 
  tidytable::rename("Central" = "CENTRAL GOA",
                    "Eastern" = "EASTERN GOA",
                    "Western" = "WESTERN GOA") %>% 
  tidytable::pivot_longer(., cols = c('Central', 'Eastern', 'Western'), names_to = "Region", values_to = "Apportionment") %>% 
  tidytable::select(Region, Apportionment) %>% 
  tidytable::mutate(Apportionment = round(Apportionment, digits = 3),
                    diff = 1 - sum(Apportionment)) %>%
  tidytable::mutate(apport_corr = case_when(max(diff) > 0 ~ case_when(Region == 'Western' ~ Apportionment + diff,
                                                                      Region != 'Western' ~ Apportionment),
                                            max(diff) == 0 ~ Apportionment)) %>%  # if rounding error happens, add to wgoa
  tidytable::select(Region, apport_corr) %>% 
  tidytable::rename(Apportionment = 'apport_corr') %>% 
  tidytable::mutate(ABC_yr1 = round(Apportionment * as.numeric(exec_summ %>% 
                                                                 tidytable::filter(Yr == new_SS_dat_year + 1) %>% 
                                                                 tidytable::select(C_ABC))),
                    ABC_yr2 = round(Apportionment * as.numeric(exec_summ %>% 
                                                                 tidytable::filter(Yr == new_SS_dat_year + 2) %>% 
                                                                 tidytable::select(C_ABC)))) %>% 
  tidytable::mutate(diff_y1 = as.numeric(exec_summ %>% 
                                           tidytable::filter(Yr == new_SS_dat_year + 1) %>% 
                                           tidytable::select(C_ABC)) - sum(ABC_yr1),
                    diff_y2 = as.numeric(exec_summ %>% 
                                           tidytable::filter(Yr == new_SS_dat_year + 2) %>% 
                                           tidytable::select(C_ABC)) - sum(ABC_yr2)) %>%
  tidytable::mutate(y1_corr = case_when(max(diff_y1) != 0 ~ case_when(Region == 'Western' ~ ABC_yr1 + diff_y1,
                                                                     Region != 'Western' ~ ABC_yr1),
                                        max(diff_y1) == 0 ~ ABC_yr1),
                    y2_corr = case_when(max(diff_y2) != 0 ~ case_when(Region == 'Western' ~ ABC_yr2 + diff_y2,
                                                                     Region != 'Western' ~ ABC_yr2),
                                        max(diff_y2) == 0 ~ ABC_yr2)) %>%  # if rounding error happens, add to wgoa
  tidytable::select(-c(ABC_yr1, ABC_yr2, diff_y1, diff_y2)) %>% 
  tidytable::rename(ABC_yr1 = 'y1_corr',
                    ABC_yr2 = 'y2_corr') -> abc_apport

write.csv(abc_apport, here::here(new_SS_dat_year, 'output', 'abc_apport.csv'))
