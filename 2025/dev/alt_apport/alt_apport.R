library(tidyverse)
library(rema)

# run rema models ----
new_year <- as.numeric(format(Sys.Date(), format = "%Y"))

# get data
biomass_dat <- vroom::vroom(here::here(new_year,'data','raw','twl_srvy_index.csv'), 
                            delim = ",", 
                            progress = FALSE, 
                            show_col_types = FALSE) %>%
  tidytable::filter(strata < 99900) %>% 
  tidytable::mutate(sd = sqrt(biom_var),
                    cv = sd / biom) %>%
  tidytable::select(strata = area,
                    year,
                    biomass = biom,
                    cv)

load(here::here(new_year, "output", "mscen", "mgmnt_scen_rec.RData"))
curr_2yr <- mscen$Two_year

# base 2025 model
apport_in_base <- SimDesign::quiet(rema::prepare_rema_input(model_name = paste0("base pcod trawl survey"),
                                                            biomass_dat = biomass_dat))
apport_mdl_base <- SimDesign::quiet(rema::fit_rema(apport_in_base))
apport_out_base <- SimDesign::quiet(rema::tidy_rema(rema_model = apport_mdl_base))

# base 2023 model
apport_in_prev <- SimDesign::quiet(rema::prepare_rema_input(model_name = paste0("base pcod trawl survey"),
                                                            biomass_dat = biomass_dat %>% tidytable::filter(year <= 2023)))
apport_mdl_prev <- SimDesign::quiet(rema::fit_rema(apport_in_prev))
apport_out_prev <- SimDesign::quiet(rema::tidy_rema(rema_model = apport_mdl_prev))

# new model with pe prior
apport_in_pe <- SimDesign::quiet(rema::prepare_rema_input(model_name = paste0("pcod trawl survey"),
                                                       biomass_dat = biomass_dat,
                                                       PE_options = list(pointer_PE_biomass = c(1, 1, 1),
                                                                         penalty_options = "normal_prior",
                                                                         penalty_values = c(-3.15, 0.2))))
apport_mdl_pe <- SimDesign::quiet(rema::fit_rema(apport_in_pe))
apport_out_pe <- SimDesign::quiet(rema::tidy_rema(rema_model = apport_mdl_pe))

# new model with extra cv
apport_in_cv <- SimDesign::quiet(rema::prepare_rema_input(model_name = paste0("pcod trawl survey"),
                                                       biomass_dat = biomass_dat,
                                                       extra_biomass_cv = list(assumption = "extra_cv"),
                                                       PE_options = list(pointer_PE_biomass = c(1, 1, 1))))
apport_mdl_cv <- SimDesign::quiet(rema::fit_rema(apport_in_cv))
apport_out_cv <- SimDesign::quiet(rema::tidy_rema(rema_model = apport_mdl_cv))

apport_out_cv$parameter_estimates
apport_out_pe$parameter_estimates

# plot results for new model ----
apport_out_cv$biomass_by_strata %>% 
  tidytable::select(strata, year, pred, pred_lci, pred_uci, obs, obs_cv) %>% 
  tidytable::mutate(Region = case_when(strata == 'central' ~ "Central GOA",
                                       strata == 'western' ~ "Western GOA",
                                       strata == 'eastern' ~ "Eastern GOA"),
                    Region = factor(Region, levels = c("Western GOA", "Central GOA", "Eastern GOA")),
                    type = "Biomass (t)") %>% 
  tidytable::select(-strata) %>% 
  tidytable::bind_rows(apport_out_cv$proportion_biomass_by_strata %>% 
                         tidytable::pivot_longer(cols = c(central, eastern, western)) %>% 
                         tidytable::rename(Region = "name",
                                           pred = "value") %>% 
                         tidytable::select(year, Region, pred) %>% 
                         tidytable::mutate(Region = case_when(Region == 'central' ~ "Central GOA",
                                                              Region == 'western' ~ "Western GOA",
                                                              Region == 'eastern' ~ "Eastern GOA"),
                                           Region = factor(Region, levels = c("Western GOA", "Central GOA", "Eastern GOA")),
                                           type = "Apportionment")) %>% 
  tidytable::mutate(type = factor(type, levels = c("Biomass (t)", "Apportionment"))) -> apport_data

ggplot(apport_data, aes(x = year, y = pred, color = Region, fill = Region)) +
  geom_line(data = apport_data %>% tidytable::filter(type == "Biomass (t)"),
            linewidth = 1) +
  geom_ribbon(data = apport_data %>% tidytable::filter(type == "Biomass (t)"), 
              aes(ymin = pred_lci, ymax = pred_uci), alpha = 0.2) +
  geom_point(data = apport_data %>% tidytable::filter(type == "Biomass (t)"),
             aes(x = year, y = obs)) +
  geom_linerange(data = apport_data %>% tidytable::filter(type == "Biomass (t)"),
                 aes(ymin = obs - 1.96 * obs_cv * obs, ymax = obs + 1.96 * obs_cv * obs)) + 
  geom_bar(data = apport_data %>% tidytable::filter(type == "Apportionment"),
           position = "fill", stat = "identity", width = 0.5) +
  facet_wrap(~type, scales = "free_y", ncol = 1) +
  scico::scale_color_scico_d(palette = 'roma') +
  scico::scale_fill_scico_d(palette = 'roma') +
  theme_bw(base_size = 14) +
  labs(x = "Year", y = "REMA esimates") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.key.width = unit(0.25, 'cm'),
        legend.position = "top")

# save
ggsave(filename = "rec_apport.png",
       path = here::here(new_year, "dev", "alt_apport", "figs_tabs"),
       width = 7,
       height = 5,
       units = "in")


# plot comparison of results ----
apport_out_base$biomass_by_strata %>% 
  tidytable::select(strata, year, pred, pred_lci, pred_uci, obs, obs_cv) %>% 
  tidytable::mutate(Region = case_when(strata == 'central' ~ "Central GOA",
                                       strata == 'western' ~ "Western GOA",
                                       strata == 'eastern' ~ "Eastern GOA"),
                    Region = factor(Region, levels = c("Western GOA", "Central GOA", "Eastern GOA")),
                    type = "Biomass (t)") %>% 
  tidytable::select(-strata) %>% 
  tidytable::bind_rows(apport_out_base$proportion_biomass_by_strata %>% 
                         tidytable::pivot_longer(cols = c(central, eastern, western)) %>% 
                         tidytable::rename(Region = "name",
                                           pred = "value") %>% 
                         tidytable::select(year, Region, pred) %>% 
                         tidytable::mutate(Region = case_when(Region == 'central' ~ "Central GOA",
                                                              Region == 'western' ~ "Western GOA",
                                                              Region == 'eastern' ~ "Eastern GOA"),
                                           Region = factor(Region, levels = c("Western GOA", "Central GOA", "Eastern GOA")),
                                           type = "Apportionment")) %>% 
  tidytable::mutate(type = factor(type, levels = c("Biomass (t)", "Apportionment"))) -> apport_data_base


apport_data %>% 
  tidytable::mutate(Model = "Recommended REMA") %>% 
  tidytable::bind_rows(apport_data_base %>% 
                         tidytable::mutate(Model = "Base REMA")) %>% 
    tidytable::mutate(Model = factor(Model, levels = c("Recommended REMA", "Base REMA"))) -> comp_data

## subregion rema estimates ----
ggplot(comp_data %>% tidytable::filter(type == "Biomass (t)"), aes(x = year, y = pred / 1000, color = Model, fill = Model, linetype = Model)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = pred_lci / 1000, ymax = pred_uci / 1000), alpha = 0.2) +
  geom_point(aes(x = year, y = obs / 1000)) +
  geom_linerange(aes(ymin = obs / 1000 - 1.96 * obs_cv * obs / 1000, ymax = obs / 1000 + 1.96 * obs_cv * obs / 1000)) +
  facet_wrap(~Region, scales = "free_y", ncol = 1) +
  scico::scale_color_scico_d(palette = 'roma') +
  scico::scale_fill_scico_d(palette = 'roma') +
  theme_bw(base_size = 14) +
  labs(x = "Year", y = "REMA esimates (1000s mt)") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.key.width = unit(0.25, 'cm'),
        legend.position = "top")

# save
ggsave(filename = "biom_comp.png",
       path = here::here(new_year, "dev", "alt_apport", "figs_tabs"),
       width = 7,
       height = 5,
       units = "in")

## apportionment estimates ----
ggplot(comp_data %>% tidytable::filter(type == "Apportionment"), aes(x = year, y = pred, color = Region, fill = Region)) +
  geom_bar(position = "fill", stat = "identity", width = 0.5) +
  scale_y_reverse() +
  scale_x_reverse() +
  coord_flip() +
  facet_wrap(~Model, scales = "free_x", ncol = 2) +
  scico::scale_color_scico_d(palette = 'roma') +
  scico::scale_fill_scico_d(palette = 'roma') +
  theme_bw(base_size = 14) +
  labs(x = "Year", y = "REMA esimates (apportioment)") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.key.width = unit(0.25, 'cm'),
        legend.position = "top")

# save
ggsave(filename = "apport_comp.png",
       path = here::here(new_year, "dev", "alt_apport", "figs_tabs"),
       width = 6,
       height = 7,
       units = "in")

## total biomass estimates ----

rec_mdl_res <- r4ss::SS_output(dir = here::here(new_year, "mgmt", '24.0'),
                               verbose = FALSE,
                               printstats = FALSE)

rec_mdl_res$timeseries %>% 
  tidytable::select(year = Yr, value = Bio_all) %>% 
  tidytable::filter(year %in% seq(1990, new_year)) %>% 
  tidytable::rename(tot_biom = value) %>% 
  tidytable::mutate(Model = 'Assessment',
                    uci = tot_biom,
                    lci = tot_biom) %>% 
  tidytable::bind_rows(apport_out_base$total_predicted_biomass %>% 
                         tidytable::select(year, tot_biom = pred, uci = pred_uci, lci = pred_lci) %>% 
                         tidytable::mutate(Model = 'Base REMA')) %>% 
  tidytable::bind_rows(apport_out_pe$total_predicted_biomass %>% 
                         tidytable::select(year, tot_biom = pred, uci = pred_uci, lci = pred_lci) %>% 
                         tidytable::mutate(Model = 'Recommended REMA')) %>% 
  tidytable::left_join(apport_data_base %>% 
                         tidytable::select(year, obs, obs_cv) %>% 
                         tidytable::drop_na() %>% 
                         tidytable::mutate(var = (obs * obs_cv)^2) %>% 
                         tidytable::summarise(obs = sum(obs),
                                              var = sum(var),
                                              .by = year) %>% 
                         tidytable::mutate(obs_cv = sqrt(var) / obs) %>% 
                         tidytable::select(year, obs, obs_cv)) -> tot_data


ggplot(tot_data, aes(x = year, y = tot_biom / 1000, color = Model, fill = Model, linetype = Model)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = lci / 1000, ymax = uci / 1000), alpha = 0.2) +
  geom_point(aes(x = year, y = obs / 1000)) +
  geom_linerange(aes(ymin = obs / 1000 - 1.96 * obs_cv * obs / 1000, ymax = obs / 1000 + 1.96 * obs_cv * obs / 1000)) +
  # facet_wrap(~Region, scales = "free_y", ncol = 1) +
  scico::scale_color_scico_d(palette = 'roma') +
  scico::scale_fill_scico_d(palette = 'roma') +
  theme_bw(base_size = 14) +
  labs(x = "Year", y = "REMA esimates (1000s mt)") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.key.width = unit(0.25, 'cm'),
        legend.position = "top")

# save
ggsave(filename = "total_comp.png",
       path = here::here(new_year, "dev", "alt_apport", "figs_tabs"),
       width = 7,
       height = 5,
       units = "in")



# get apportionment tables ----
## recommended apportionment table ----
### pe ----
apport_out_pe$proportion_biomass_by_strata %>% 
  tidytable::filter(year == max(year)) %>% 
  tidytable::pivot_longer(., cols = c('central', 'eastern', 'western'), names_to = "region", values_to = "apport") %>% 
  tidytable::select(region, apport) %>% 
  tidytable::mutate(apport = round(apport, digits = 3),
                    diff = case_when(region == 'western' ~ 1 - sum(apport),
                                     .default = 0)) %>%
  tidytable::mutate(apport_corr = apport + diff) %>%  # if rounding error happens, add to wgoa
  tidytable::select(region, apport_corr) %>% 
  tidytable::rename(apport = 'apport_corr') %>% 
  tidytable::mutate(ABC_yr1 = round(apport * curr_2yr$C_ABC[1], digits = 0),
                    ABC_yr2 = round(apport * curr_2yr$C_ABC[2], digits = 0)) %>% 
  tidytable::mutate(diff_y1 = case_when(region == 'western' ~ round(curr_2yr$C_ABC[1], digits = 0) - sum(ABC_yr1),
                                        .default = 0),
                    diff_y2 = case_when(region == 'western' ~ round(curr_2yr$C_ABC[2], digits = 0) - sum(ABC_yr2),
                                        .default = 0)) %>%
  tidytable::mutate(y1_corr = ABC_yr1 + diff_y1,
                    y2_corr = ABC_yr2 + diff_y2) %>%  # if rounding error happens, add to wgoa
  tidytable::select(-c(ABC_yr1, ABC_yr2, diff_y1, diff_y2)) %>% 
  tidytable::rename(ABC_yr1 = 'y1_corr',
                    ABC_yr2 = 'y2_corr') -> abc_apport_pe

apport_tbl_pe <- data.table::data.table(" " = c("Recommended apportionment", 
                                             paste(new_year + 1, "ABC"), 
                                             paste(new_year + 2, "ABC")),
                                     Western = c(paste0(abc_apport_pe$apport[which(abc_apport_pe$region == 'western')] * 100, "%"),
                                                 format(abc_apport_pe$ABC_yr1[which(abc_apport_pe$region == 'western')], big.mark = ","),
                                                 format(abc_apport_pe$ABC_yr2[which(abc_apport_pe$region == 'western')], big.mark = ",")),
                                     Central = c(paste0(abc_apport_pe$apport[which(abc_apport_pe$region == 'central')] * 100, "%"),
                                                 format(abc_apport_pe$ABC_yr1[which(abc_apport_pe$region == 'central')], big.mark = ","),
                                                 format(abc_apport_pe$ABC_yr2[which(abc_apport_pe$region == 'central')], big.mark = ",")),
                                     Eastern = c(paste0(abc_apport_pe$apport[which(abc_apport_pe$region == 'eastern')] * 100, "%"),
                                                 format(abc_apport_pe$ABC_yr1[which(abc_apport_pe$region == 'eastern')], big.mark = ","),
                                                 format(abc_apport_pe$ABC_yr2[which(abc_apport_pe$region == 'eastern')], big.mark = ",")),
                                     Total = c(paste0(sum(abc_apport_pe$apport) * 100, "%"),
                                               format(sum(abc_apport_pe$ABC_yr1), big.mark = ","),
                                               format(sum(abc_apport_pe$ABC_yr2), big.mark = ",")))

### cv ----
apport_out_cv$proportion_biomass_by_strata %>% 
  tidytable::filter(year == max(year)) %>% 
  tidytable::pivot_longer(., cols = c('central', 'eastern', 'western'), names_to = "region", values_to = "apport") %>% 
  tidytable::select(region, apport) %>% 
  tidytable::mutate(apport = round(apport, digits = 3),
                    diff = case_when(region == 'western' ~ 1 - sum(apport),
                                     .default = 0)) %>%
  tidytable::mutate(apport_corr = apport + diff) %>%  # if rounding error happens, add to wgoa
  tidytable::select(region, apport_corr) %>% 
  tidytable::rename(apport = 'apport_corr') %>% 
  tidytable::mutate(ABC_yr1 = round(apport * curr_2yr$C_ABC[1], digits = 0),
                    ABC_yr2 = round(apport * curr_2yr$C_ABC[2], digits = 0)) %>% 
  tidytable::mutate(diff_y1 = case_when(region == 'western' ~ round(curr_2yr$C_ABC[1], digits = 0) - sum(ABC_yr1),
                                        .default = 0),
                    diff_y2 = case_when(region == 'western' ~ round(curr_2yr$C_ABC[2], digits = 0) - sum(ABC_yr2),
                                        .default = 0)) %>%
  tidytable::mutate(y1_corr = ABC_yr1 + diff_y1,
                    y2_corr = ABC_yr2 + diff_y2) %>%  # if rounding error happens, add to wgoa
  tidytable::select(-c(ABC_yr1, ABC_yr2, diff_y1, diff_y2)) %>% 
  tidytable::rename(ABC_yr1 = 'y1_corr',
                    ABC_yr2 = 'y2_corr') -> abc_apport_cv

apport_tbl_cv <- data.table::data.table(" " = c("Recommended apportionment", 
                                                paste(new_year + 1, "ABC"), 
                                                paste(new_year + 2, "ABC")),
                                        Western = c(paste0(abc_apport_cv$apport[which(abc_apport_cv$region == 'western')] * 100, "%"),
                                                    format(abc_apport_cv$ABC_yr1[which(abc_apport_cv$region == 'western')], big.mark = ","),
                                                    format(abc_apport_cv$ABC_yr2[which(abc_apport_cv$region == 'western')], big.mark = ",")),
                                        Central = c(paste0(abc_apport_cv$apport[which(abc_apport_cv$region == 'central')] * 100, "%"),
                                                    format(abc_apport_cv$ABC_yr1[which(abc_apport_cv$region == 'central')], big.mark = ","),
                                                    format(abc_apport_cv$ABC_yr2[which(abc_apport_cv$region == 'central')], big.mark = ",")),
                                        Eastern = c(paste0(abc_apport_cv$apport[which(abc_apport_cv$region == 'eastern')] * 100, "%"),
                                                    format(abc_apport_cv$ABC_yr1[which(abc_apport_cv$region == 'eastern')], big.mark = ","),
                                                    format(abc_apport_cv$ABC_yr2[which(abc_apport_cv$region == 'eastern')], big.mark = ",")),
                                        Total = c(paste0(sum(abc_apport_cv$apport) * 100, "%"),
                                                  format(sum(abc_apport_cv$ABC_yr1), big.mark = ","),
                                                  format(sum(abc_apport_cv$ABC_yr2), big.mark = ",")))


apport_tbl_pe
apport_tbl_cv

## base apportionment table ----
apport_out_base$proportion_biomass_by_strata %>% 
  tidytable::filter(year == max(year)) %>% 
  tidytable::pivot_longer(., cols = c('central', 'eastern', 'western'), names_to = "region", values_to = "apport") %>% 
  tidytable::select(region, apport) %>% 
  tidytable::mutate(apport = round(apport, digits = 3),
                    diff = case_when(region == 'western' ~ 1 - sum(apport),
                                     .default = 0)) %>%
  tidytable::mutate(apport_corr = apport + diff) %>%  # if rounding error happens, add to wgoa
  tidytable::select(region, apport_corr) %>% 
  tidytable::rename(apport = 'apport_corr') %>% 
  tidytable::mutate(ABC_yr1 = round(apport * curr_2yr$C_ABC[1], digits = 0),
                    ABC_yr2 = round(apport * curr_2yr$C_ABC[2], digits = 0)) %>% 
  tidytable::mutate(diff_y1 = case_when(region == 'western' ~ round(curr_2yr$C_ABC[1], digits = 0) - sum(ABC_yr1),
                                        .default = 0),
                    diff_y2 = case_when(region == 'western' ~ round(curr_2yr$C_ABC[2], digits = 0) - sum(ABC_yr2),
                                        .default = 0)) %>%
  tidytable::mutate(y1_corr = ABC_yr1 + diff_y1,
                    y2_corr = ABC_yr2 + diff_y2) %>%  # if rounding error happens, add to wgoa
  tidytable::select(-c(ABC_yr1, ABC_yr2, diff_y1, diff_y2)) %>% 
  tidytable::rename(ABC_yr1 = 'y1_corr',
                    ABC_yr2 = 'y2_corr') -> abc_apport_base

apport_tbl_base <- data.table::data.table(" " = c("Base apportionment", 
                                             paste(new_year + 1, "ABC"), 
                                             paste(new_year + 2, "ABC")),
                                     Western = c(paste0(abc_apport_base$apport[which(abc_apport_base$region == 'western')] * 100, "%"),
                                                 format(abc_apport_base$ABC_yr1[which(abc_apport_base$region == 'western')], big.mark = ","),
                                                 format(abc_apport_base$ABC_yr2[which(abc_apport_base$region == 'western')], big.mark = ",")),
                                     Central = c(paste0(abc_apport_base$apport[which(abc_apport_base$region == 'central')] * 100, "%"),
                                                 format(abc_apport_base$ABC_yr1[which(abc_apport_base$region == 'central')], big.mark = ","),
                                                 format(abc_apport_base$ABC_yr2[which(abc_apport_base$region == 'central')], big.mark = ",")),
                                     Eastern = c(paste0(abc_apport_base$apport[which(abc_apport_base$region == 'eastern')] * 100, "%"),
                                                 format(abc_apport_base$ABC_yr1[which(abc_apport_base$region == 'eastern')], big.mark = ","),
                                                 format(abc_apport_base$ABC_yr2[which(abc_apport_base$region == 'eastern')], big.mark = ",")),
                                     Total = c(paste0(sum(abc_apport_base$apport) * 100, "%"),
                                               format(sum(abc_apport_base$ABC_yr1), big.mark = ","),
                                               format(sum(abc_apport_base$ABC_yr2), big.mark = ",")))

## previous apportionment ----
apport_tbl_prev <- vroom::vroom(here::here(new_year - 1, 'output', 'safe_tables', 'intext_abc_apport.csv'), 
                                delim = ",", 
                                progress = FALSE, 
                                show_col_types = FALSE)


## compare ----
t1 <- apport_tbl_prev[1:2, ] %>% 
  tidytable::rename(` ` = ...1)
t1[1, 1] <- 'Previous apportionment'

t1 %>% 
  tidytable::bind_rows(apport_tbl_base[1:2, ]) %>% 
  tidytable::bind_rows(apport_tbl_cv[1:2, ])
