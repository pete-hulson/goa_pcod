
# run rema model with pe prior ----
apport_in <- SimDesign::quiet(rema::prepare_rema_input(model_name = paste0("pcod trawl survey"),
                                                       biomass_dat = biomass_dat,
                                                       PE_options = list(pointer_PE_biomass = c(1, 1, 1),
                                                                         penalty_options = "normal_prior",
                                                                         penalty_values = c(-3.15, 0.2))))
apport_mdl <- SimDesign::quiet(rema::fit_rema(apport_in))
apport_out <- SimDesign::quiet(rema::tidy_rema(rema_model = apport_mdl))

# plot results ----
apport_out$biomass_by_strata %>% 
  tidytable::select(strata, year, pred, pred_lci, pred_uci, obs, obs_cv) %>% 
  tidytable::mutate(Region = case_when(strata == 'central' ~ "Central GOA",
                                       strata == 'western' ~ "Western GOA",
                                       strata == 'eastern' ~ "Eastern GOA"),
                    Region = factor(Region, levels = c("Western GOA", "Central GOA", "Eastern GOA")),
                    type = "Biomass (t)") %>% 
  tidytable::select(-strata) %>% 
  tidytable::bind_rows(apport_out$proportion_biomass_by_strata %>% 
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

## apportionment table ----
apport_out$proportion_biomass_by_strata %>% 
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
                    ABC_yr2 = 'y2_corr') -> abc_apport

apport_tbl <- data.table::data.table(" " = c("Area apportionment", 
                                             paste(new_year + 1, "ABC"), 
                                             paste(new_year + 2, "ABC")),
                                     Western = c(paste0(abc_apport$apport[which(abc_apport$region == 'western')] * 100, "%"),
                                                 format(abc_apport$ABC_yr1[which(abc_apport$region == 'western')], big.mark = ","),
                                                 format(abc_apport$ABC_yr2[which(abc_apport$region == 'western')], big.mark = ",")),
                                     Central = c(paste0(abc_apport$apport[which(abc_apport$region == 'central')] * 100, "%"),
                                                 format(abc_apport$ABC_yr1[which(abc_apport$region == 'central')], big.mark = ","),
                                                 format(abc_apport$ABC_yr2[which(abc_apport$region == 'central')], big.mark = ",")),
                                     Eastern = c(paste0(abc_apport$apport[which(abc_apport$region == 'eastern')] * 100, "%"),
                                                 format(abc_apport$ABC_yr1[which(abc_apport$region == 'eastern')], big.mark = ","),
                                                 format(abc_apport$ABC_yr2[which(abc_apport$region == 'eastern')], big.mark = ",")),
                                     Total = c(paste0(sum(abc_apport$apport) * 100, "%"),
                                               format(sum(abc_apport$ABC_yr1), big.mark = ","),
                                               format(sum(abc_apport$ABC_yr2), big.mark = ",")))

apport_tbl
