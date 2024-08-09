## Script to run 2023 GOA Pacific Cod Assessment apportionment (P. Hulson)

# Load required packages & define parameters ----
# devtools::install_github("afsc-assessments/rema", dependencies = TRUE, build_vignettes = TRUE)

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
new_year <- as.numeric(format(Sys.Date(), format = "%Y"))

# get data ----
biomass_dat <- read.csv(here::here(new_year,'data','raw','twl_srvy_index.csv')) %>%
  tidytable::filter(strata < 99900) %>% 
  tidytable::mutate(sd = sqrt(biom_var),
                    cv = sd / biom) %>%
  tidytable::select(strata = area,
                    year,
                    biomass = biom,
                    cv)

rpw_dat <- read.csv(here::here(new_year,'data','raw','lls_rpn_geoarea_data.csv')) %>%
  tidytable::summarise(rpw = sum(rpw, na.rm = TRUE),
                       sd = sqrt(sum(rpw_var, na.rm = TRUE)),
                       .by = c(year, council_management_area)) %>% 
  tidytable::mutate(cv = sd / rpw,
                    strata = tidytable::case_when(council_management_area == 'Western Gulf of Alaska' ~ 'western',
                                                  council_management_area == 'Central Gulf of Alaska' ~ 'central',
                                                  council_management_area == 'Eastern Gulf of Alaska' ~ 'eastern')) %>%
  tidytable::select(strata,
                    year,
                    cpue = rpw,
                    cv)

# run rema model for trawl survey only ----
apport_in <- rema::prepare_rema_input(model_name = paste0("pcod trawl survey"),
                                      biomass_dat = biomass_dat)
apport_mdl <- rema::fit_rema(apport_in)
apport_out <- rema::tidy_rema(rema_model = apport_mdl)

# run rema model for trawl survey and longline survey ----

## process error and scalar inv ----
### pe & q = 1 ----
apport_in1 <- rema::prepare_rema_input(model_name = paste0("pcod multi survey, pe = 1, q = 1"),
                                       multi_survey = 1,
                                       biomass_dat = biomass_dat,
                                       cpue_dat = rpw_dat,
                                       sum_cpue_index = TRUE,
                                       PE_options = list(pointer_PE_biomass = c(1, 1, 1)),
                                       q_options = list(pointer_q_cpue = c(1, 1, 1)))

apport_mdl1 <- rema::fit_rema(apport_in1)
apport_out1 <- rema::tidy_rema(rema_model = apport_mdl1)

### pe = 3, q = 1 ----
apport_in2 <- rema::prepare_rema_input(model_name = paste0("pcod multi survey, pe = 3, q = 1"),
                                       multi_survey = 1,
                                       biomass_dat = biomass_dat,
                                       cpue_dat = rpw_dat,
                                       sum_cpue_index = TRUE,
                                       q_options = list(pointer_q_cpue = c(1, 1, 1)))

apport_mdl2 <- rema::fit_rema(apport_in2)
apport_out2 <- rema::tidy_rema(rema_model = apport_mdl2)

### pe = 1, q = 3 ----
apport_in3 <- rema::prepare_rema_input(model_name = paste0("pcod multi survey, pe = 1, q = 3"),
                                       multi_survey = 1,
                                       biomass_dat = biomass_dat,
                                       cpue_dat = rpw_dat,
                                       sum_cpue_index = TRUE,
                                       PE_options = list(pointer_PE_biomass = c(1, 1, 1)))

apport_mdl3 <- rema::fit_rema(apport_in3)
apport_out3 <- rema::tidy_rema(rema_model = apport_mdl3)

### pe & q = 3 ----
apport_in4 <- rema::prepare_rema_input(model_name = paste0("pcod multi survey, pe = 3, q = 3"),
                                       multi_survey = 1,
                                       biomass_dat = biomass_dat,
                                       cpue_dat = rpw_dat,
                                       sum_cpue_index = TRUE)

apport_mdl4 <- rema::fit_rema(apport_in4)
apport_out4 <- rema::tidy_rema(rema_model = apport_mdl4)

## compare pe & q models ----
compare_peq <- rema::compare_rema_models(list(apport_mdl1, apport_mdl2, apport_mdl3, apport_mdl4),
                                     biomass_ylab = 'Biomass (t)',
                                     cpue_ylab = 'Relative Population Weights')

knitr::kable(compare_peq$aic)


## suvey extra cv cases ----
### extra ll cv (pe = 1, q = 3) ----
apport_in5 <- rema::prepare_rema_input(model_name = paste0("pcod multi survey, extra ll cv; pe1q3"),
                                       multi_survey = 1,
                                       biomass_dat = biomass_dat,
                                       cpue_dat = rpw_dat,
                                       sum_cpue_index = TRUE,
                                       PE_options = list(pointer_PE_biomass = c(1, 1, 1)),
                                       extra_cpue_cv = list(assumption = 'extra_cv'))

apport_mdl5 <- rema::fit_rema(apport_in5)
apport_out5 <- rema::tidy_rema(rema_model = apport_mdl5)

### extra twl cv (pe = 1, q = 3) ----
apport_in6 <- rema::prepare_rema_input(model_name = paste0("pcod multi survey, extra twl cv; pe1q3"),
                                       multi_survey = 1,
                                       biomass_dat = biomass_dat,
                                       cpue_dat = rpw_dat,
                                       sum_cpue_index = TRUE,
                                       PE_options = list(pointer_PE_biomass = c(1, 1, 1)),
                                       extra_biomass_cv = list(assumption = 'extra_cv'))

apport_mdl6 <- rema::fit_rema(apport_in6)
apport_out6 <- rema::tidy_rema(rema_model = apport_mdl6)

### extra twl & ll cv (pe = 1, q = 3) ----
apport_in7 <- rema::prepare_rema_input(model_name = paste0("pcod multi survey, extra twl & ll cv; pe1q3"),
                                       multi_survey = 1,
                                       biomass_dat = biomass_dat,
                                       cpue_dat = rpw_dat,
                                       sum_cpue_index = TRUE,
                                       PE_options = list(pointer_PE_biomass = c(1, 1, 1)),
                                       extra_biomass_cv = list(assumption = 'extra_cv'),
                                       extra_cpue_cv = list(assumption = 'extra_cv'))

apport_mdl7 <- rema::fit_rema(apport_in7)
apport_out7 <- rema::tidy_rema(rema_model = apport_mdl7)

### extra ll cv (pe & q = 3) ----
apport_in8 <- rema::prepare_rema_input(model_name = paste0("pcod multi survey, extra ll cv; pe3q3"),
                                       multi_survey = 1,
                                       biomass_dat = biomass_dat,
                                       cpue_dat = rpw_dat,
                                       sum_cpue_index = TRUE,
                                       extra_cpue_cv = list(assumption = 'extra_cv'))

apport_mdl8 <- rema::fit_rema(apport_in8)
apport_out8 <- rema::tidy_rema(rema_model = apport_mdl8)

### extra twl cv (pe & q = 3) ----
apport_in9 <- rema::prepare_rema_input(model_name = paste0("pcod multi survey, extra twl cv; pe3q3"),
                                       multi_survey = 1,
                                       biomass_dat = biomass_dat,
                                       cpue_dat = rpw_dat,
                                       sum_cpue_index = TRUE,
                                       extra_biomass_cv = list(assumption = 'extra_cv'))

apport_mdl9 <- rema::fit_rema(apport_in9)
apport_out9 <- rema::tidy_rema(rema_model = apport_mdl9)

### extra twl & ll cv (pe & q = 3) ----
apport_in10 <- rema::prepare_rema_input(model_name = paste0("pcod multi survey, extra twl & ll cv; pe3q3"),
                                       multi_survey = 1,
                                       biomass_dat = biomass_dat,
                                       cpue_dat = rpw_dat,
                                       sum_cpue_index = TRUE,
                                       extra_biomass_cv = list(assumption = 'extra_cv'),
                                       extra_cpue_cv = list(assumption = 'extra_cv'))

apport_mdl10 <- rema::fit_rema(apport_in10)
apport_out10 <- rema::tidy_rema(rema_model = apport_mdl10)


## compare cv models ----
compare_cv <- rema::compare_rema_models(list(apport_mdl5, apport_mdl6, apport_mdl7, apport_mdl8, apport_mdl9, apport_mdl10),
                                         biomass_ylab = 'Biomass (t)',
                                         cpue_ylab = 'Relative Population Weights')

knitr::kable(compare_cv$aic)


knitr::kable(apport_out$parameter_estimates)

knitr::kable(apport_out7$parameter_estimates)

## compare base with selected models ----

compare_base <- rema::compare_rema_models(list(apport_mdl, apport_mdl7),
                                         biomass_ylab = 'Biomass (t)',
                                         cpue_ylab = 'Relative Population Weights')

suppressWarnings(ggplot2::ggsave(compare_base$plots$total_predicted_biomass + theme(legend.position = 'top'),
                                 file = here::here(new_year, "plots", 'other','biom_compare.png'),
                                 width = 12, height = 7, unit = 'in', dpi = 520))

suppressWarnings(ggplot2::ggsave(compare_base$plots$proportion_biomass_by_strata + theme(legend.position = 'top'),
                                 file = here::here(new_year, "plots", 'other','apport_compare.png'),
                                 width = 12, height = 7, unit = 'in', dpi = 520))

base_plots <- rema::plot_rema(tidy_rema = apport_out, 
                              biomass_ylab = 'Biomass (t): Trawl only')

new_plots <- rema::plot_rema(tidy_rema = apport_out7, 
                             biomass_ylab = 'Biomass (t): Trawl and Longline',
                             cpue_ylab = 'Relative Population Weights')

cowplot::plot_grid(base_plots$biomass_by_strata + theme(legend.position = 'top'),
                   new_plots$biomass_by_strata + theme(legend.position = 'none'),
                   new_plots$cpue_by_strata + theme(legend.position = 'none'),
                   ncol = 1, rel_widths = c(0.65, 0.35))

suppressWarnings(ggplot2::ggsave(cowplot::plot_grid(base_plots$biomass_by_strata + theme(legend.position = 'top'),
                                                    new_plots$biomass_by_strata + theme(legend.position = 'none'),
                                                    new_plots$cpue_by_strata + theme(legend.position = 'none'),
                                                    ncol = 1, rel_widths = c(0.65, 0.35)),
                                 file = here::here(new_year, "plots", 'other','fit_compare.png'),
                                 width = 12, height = 7, unit = 'in', dpi = 520))


suppressWarnings(ggplot2::ggsave(cowplot::plot_grid(base_plots$proportion_biomass_by_strata + theme(legend.position = 'top'),
                                                    new_plots$proportion_biomass_by_strata + theme(legend.position = 'none'),
                                                    labels = c('Trawl only', 'Trawl and Longline'),
                                                    label_x = 0.7,
                                                    ncol = 1, rel_widths = c(0.65, 0.35)),
                                 file = here::here(new_year, "plots", 'other','apport_compare.png'),
                                 width = 12, height = 7, unit = 'in', dpi = 520))

knitr::kable(apport_out$proportion_biomass_by_strata %>% 
               tidytable::filter(year == 2023) %>% 
               tidytable::mutate(central = round(100 * central, digits = 1),
                                 western = round(100 * western, digits = 1),
                                 eastern = round(100 * eastern, digits = 1)) %>% 
               tidytable::select(model_name, year, eastern, central, western) %>% 
               bind_rows(apport_out7$proportion_biomass_by_strata %>% 
                           tidytable::filter(year == 2023) %>% 
                           tidytable::mutate(central = round(100 * central, digits = 1),
                                             western = round(100 * western, digits = 1),
                                             eastern = round(100 * eastern, digits = 1)) %>% 
                           tidytable::select(model_name, year, eastern, central, western)))





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
