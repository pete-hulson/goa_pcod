# analyses and plots for 2026 model alternatives

# load libraries
library(tidyverse)
library(data.table)

# 24.0 = accepted 2025 model
# 24.0a = caal fixes; lbin method = 3, lbin_lo and lbin_hi corrected
# 24.0b = 1 cm bins, lbin method = 3
# 24.0c = 5 cm bins length comp, 1 cm bins caal, lbin method = 3
# 24.0d = 24.0a with population bins matching data bins

# set parameters ----

# Current assessment year
new_year <- as.numeric(format(Sys.Date(), format = "%Y"))

# read model results ----
base_mdl_res <- r4ss::SS_output(dir = here::here(new_year, "dev", "model_alts", "24.0"),
                                verbose = FALSE,
                                printstats = FALSE)

mdl_res_24_0a <- r4ss::SS_output(dir = here::here(new_year, "dev", "model_alts", "24.0a"),
                               verbose = FALSE,
                               printstats = FALSE)

mdl_res_24_0b <- r4ss::SS_output(dir = here::here(new_year, "dev", "model_alts", "24.0b"),
                                 verbose = FALSE,
                                 printstats = FALSE)

mdl_res_24_0c <- r4ss::SS_output(dir = here::here(new_year, "dev", "model_alts", "24.0c"),
                                 verbose = FALSE,
                                 printstats = FALSE)

mdl_res_24_0d <- r4ss::SS_output(dir = here::here(new_year, "dev", "model_alts", "24.0d"),
                                 verbose = FALSE,
                                 printstats = FALSE)


# set up directory to plop plots into
if (!dir.exists(here::here(new_year, "dev", "model_alts", "2026_analysis_plots"))) {
  dir.create(here::here(new_year, "dev", "model_alts", "2026_analysis_plots"), recursive = TRUE)
}

# plot growth estimates ----

# set up model to compare with base
rec_mdl_res <- mdl_res_24_0a

# get growth data
vroom::vroom(here::here(new_year, 'data', 'raw', 'twl_srvy_age.csv'), 
             progress = FALSE, 
             show_col_types = FALSE) %>% 
  tidytable::select(year, age, "Length (cm)" = length, "Weight (kg)" = weight) %>% 
  tidytable::pivot_longer(cols = c("Length (cm)", "Weight (kg)")) %>% 
  tidytable::drop_na() %>% 
  tidytable::filter(age <= 10) %>% 
  tidytable::summarise(value = mean(value),
                       sd_value = sd(value), .by = c(year, age, name)) %>% 
  tidytable::bind_rows(vroom::vroom(here::here(new_year, 'data', 'raw', 'twl_srvy_age.csv'), 
                                    progress = FALSE, 
                                    show_col_types = FALSE) %>% 
                         tidytable::select(year, age, "Length (cm)" = length, "Weight (kg)" = weight) %>% 
                         tidytable::pivot_longer(cols = c("Length (cm)", "Weight (kg)")) %>% 
                         tidytable::drop_na() %>% 
                         tidytable::filter(age <= 10) %>% 
                         tidytable::summarise(value = mean(value),
                                              sd_value = sd(value), .by = c(age, name)) %>% 
                         tidytable::mutate(year = "All")) %>% 
  tidytable::left_join(rec_mdl_res$wtatage %>% 
                         tidytable::filter(fleet == -1) %>% 
                         tidytable::select(-year, -seas, -sex, -bio_pattern, -birthseas, -fleet, -'0') %>% 
                         tidytable::pivot_longer() %>% 
                         tidytable::summarise(pred = mean(value), .by = name) %>% 
                         tidytable::mutate(age = as.numeric(name),
                                           name = "Weight (kg)") %>% 
                         tidytable::bind_rows(rec_mdl_res$endgrowth[,c('Real_Age', 'Len_Mid')] %>% 
                                                tidytable::rename(age = Real_Age, pred = Len_Mid) %>% 
                                                tidytable::filter(age > 0) %>% 
                                                tidytable::mutate(name = "Length (cm)"))) %>% 
  tidytable::left_join(base_mdl_res$wtatage %>% 
                         tidytable::filter(fleet == -1) %>% 
                         tidytable::select(-year, -seas, -sex, -bio_pattern, -birthseas, -fleet, -'0') %>% 
                         tidytable::pivot_longer() %>% 
                         tidytable::summarise(pred_base = mean(value), .by = name) %>% 
                         tidytable::mutate(age = as.numeric(name),
                                           name = "Weight (kg)") %>% 
                         tidytable::bind_rows(base_mdl_res$endgrowth[,c('Real_Age', 'Len_Mid')] %>% 
                                                tidytable::rename(age = Real_Age, pred_base = Len_Mid) %>% 
                                                tidytable::filter(age > 0) %>% 
                                                tidytable::mutate(name = "Length (cm)"))) %>%
  tidytable::drop_na() -> grwth_data

# compute ssq
ssq <- grwth_data %>% 
  tidytable::filter(year == "All") %>% 
  tidytable::mutate(SSQ_base = (value - pred_base)^2,
                    SSQ_rec = (value - pred)^2) %>% 
  tidytable::summarise(SSQ_base = sum(SSQ_base),
                       SSQ_rec = sum(SSQ_rec),
                       .by = name) %>% 
  tidytable::pivot_longer(cols = c(SSQ_base, SSQ_rec), names_to = "type") %>% 
  tidytable::mutate(label = paste(type, "=", round(value, digits = 1)),
                    x = 1,
                    y = case_when(name == "Length (cm)" & type == "SSQ_base" ~ 105,
                                  name == "Length (cm)" & type == "SSQ_rec" ~ 90,
                                  name == "Weight (kg)" & type == "SSQ_base" ~ 17,
                                  name == "Weight (kg)" & type == "SSQ_rec" ~ 14)) %>% 
  tidytable::select(x, y, name, label)


# plot
ggplot(data = grwth_data %>% filter(year != "All"),
       aes(x = age, y = value, col = factor(year))) +
  geom_pointrange(data = grwth_data %>% filter(year != "All"),
                  aes(ymin = value - 1.96 * sd_value, ymax = value + 1.96 * sd_value),
                  position = position_jitter(width = 0.05),
                  linetype = 'dotted',
                  size = 0.5) +
  geom_line(aes(y = pred), col = "black", linewidth = 0.777) +
  geom_line(aes(y = pred_base), col = "red", linewidth = 0.777, linetype = "dashed") +
  geom_pointrange(data = grwth_data %>% filter(year == "All"),
                  aes(ymin = value - 1.96 * sd_value, ymax = value + 1.96 * sd_value),
                  pch = 5,
                  fatten = 3) +
  geom_text(data = data.frame(ssq), aes(x = x, y = y, label = label), 
            inherit.aes = FALSE,
            hjust = 0) +
  facet_wrap(~ name, 
             ncol = 1, 
             scales = "free_y") +
  scico::scale_color_scico_d(palette = 'roma') +
  theme_bw(base_size = 14) +
  scale_x_continuous(breaks = c(1:10), limits = c(0.5, 10.5)) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  labs(x = "Age", y = "Pacific cod size", col = "Year")

# save
ggsave(filename = "growth_ests_24_0a.png",
       path = here::here(new_year, "dev", "model_alts", "2026_analysis_plots"),
       width = 6.5,
       height = 6.5,
       units = "in")


# plot size-age ----

# set up model to compare with base
rec_mdl_res <- mdl_res_24_0a

# size-age transition matrix
size_age <- data.table(length = seq(from = 104.5, to = 0.5), rec_mdl_res$ALK[,,2]) %>% 
  tidytable::pivot_longer(cols = -length, names_to = "age", values_to = "Recommended") %>% 
  tidytable::left_join(data.table(length = seq(104.5, 0.5), base_mdl_res$ALK[,,2]) %>% 
                         tidytable::pivot_longer(cols = -length, names_to = "age", values_to = "Base")) %>% 
  tidytable::mutate(Residual = Base - Recommended) %>% 
  tidytable::pivot_longer(cols = c("Recommended", "Base", "Residual"), names_to = "type", values_to = "SzAge") %>% 
  tidytable::filter(age > 0) %>% 
  tidytable::mutate(age = factor(age, levels = seq(0, 10)))

# compare models
ggplot(size_age %>% filter(type != "Residual"), aes(x = age, y = length, fill = SzAge)) +
  geom_tile(color = "white", linewidth = 0.5) +  # Grid border lines
  scale_fill_viridis_c(option = "magma") +
  facet_wrap(~type, ncol = 1) +
  theme_bw(base_size = 14) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  labs(x = "Age", y = "Length (cm)")

# save
ggsave(filename = "size_age_24_0a.png",
       path = here::here(new_year, "dev", "model_alts", "2026_analysis_plots"),
       width = 6.5,
       height = 6.5,
       units = "in")

# residuals
ggplot(size_age %>% filter(type == "Residual") %>% mutate(type = "Residual (Base - Recommended)"),
       aes(x = age, y = length, fill = SzAge)) +
  geom_tile(color = "white", linewidth = 0.5) +
  scale_fill_distiller(palette = "RdBu", limits = c(-0.03, 0.03)) +
  facet_wrap(~type, ncol = 1) +
  theme_bw(base_size = 14) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  labs(x = "Age", y = "Length (cm)")


# save
ggsave(filename = "size_age_resid_24_0a.png",
       path = here::here(new_year, "dev", "model_alts", "2026_analysis_plots"),
       width = 6.5,
       height = 4.5,
       units = "in")


# tables of stats ----

# set up model to compare with base
rec_mdl_res <- mdl_res_24_0a

# likelihoods
likes <- base_mdl_res$likelihoods_by_fleet %>% 
  tidytable::select(Label, ALL) %>% 
  tidytable::drop_na() %>% 
  tidytable::rename("Data set" = Label, "Base" = ALL) %>% 
  tidytable::left_join(rec_mdl_res$likelihoods_by_fleet %>% 
                         tidytable::select(Label, ALL) %>% 
                         tidytable::drop_na() %>% 
                         tidytable::rename("Data set" = Label, "Recommended" = ALL)) %>% 
  tidytable::filter(`Data set` %in% c("Surv_like", "Length_like", 'Age_like')) %>% 
  tidytable::mutate(`Data set` = c("Survey Index", "Length composition", "CAAL"),
                    Base = round(Base, digits = 2),
                    Recommended = round(Recommended, digits = 2))

vroom::vroom_write(likes, here::here(new_year, "dev", "model_alts", "2026_analysis_tbls", "likes_24_0a.csv"), delim = ",")

# sample size ratio
ss_rat <- base_mdl_res$len_comp_fit_table %>% 
  tidytable::select(Fleet, Yr, Nsamp_in, effN) %>% 
  tidytable::mutate(ss_ratio = Nsamp_in / effN) %>% 
  tidytable::summarise(base_len_ss_ratio = round(mean(ss_ratio), digits = 2),
                       .by = Fleet) %>% 
  tidytable::left_join(rec_mdl_res$len_comp_fit_table %>% 
                         tidytable::select(Fleet, Yr, Nsamp_in, effN) %>% 
                         tidytable::mutate(ss_ratio = Nsamp_in / effN) %>% 
                         tidytable::summarise(rec_len_ss_ratio = round(mean(ss_ratio), digits = 2),
                                              .by = Fleet)) %>% 
  tidytable::left_join(base_mdl_res$age_comp_fit_table %>% 
                         tidytable::select(Fleet, Yr, Nsamp_in, effN) %>% 
                         tidytable::mutate(ss_ratio = Nsamp_in / effN) %>% 
                         tidytable::summarise(base_caal_ss_ratio = round(mean(ss_ratio), digits = 2),
                                              .by = Fleet)) %>% 
  tidytable::left_join(rec_mdl_res$age_comp_fit_table %>% 
                         tidytable::select(Fleet, Yr, Nsamp_in, effN) %>% 
                         tidytable::mutate(ss_ratio = Nsamp_in / effN) %>% 
                         tidytable::summarise(rec_caal_ss_ratio = round(mean(ss_ratio), digits = 2),
                                              .by = Fleet)) %>% 
  tidytable::rename("Base Length ISS/ESS" = base_len_ss_ratio,
                    "Rec Length ISS/ESS" = rec_len_ss_ratio,
                    "Base CAAL ISS/ESS" = base_caal_ss_ratio,
                    "Rec CAAL ISS/ESS" = rec_caal_ss_ratio) %>% 
  tidytable::mutate(Fleet = c("Trawl fishery", "Longline fishery", "Pot fishery", "Trawl survey", "Longline survey"))

vroom::vroom_write(ss_rat, here::here(new_year, "dev", "model_alts", "2026_analysis_tbls", "ss_rat_24_0a.csv"), delim = ",")


# plot biomass ----

# set up model to compare with base
rec_mdl_res <- mdl_res_24_0a

# get plot data
rec_mdl_res$derived_quants %>% 
  tidytable::filter(Label %in% paste0("SSB_", seq(1977, new_year + 5))) %>% 
  tidytable::select(value = Value, sd = StdDev) %>% 
  tidytable::mutate(year = seq(1977, new_year + 5),
                    value = value / 2 / 1000,
                    sd = sd / 2 / 1000,
                    type = "Spawning biomass (1,000s t)") %>% 
  tidytable::bind_rows(rec_mdl_res$timeseries %>% 
                         tidytable::select(year = Yr, value = Bio_all) %>% 
                         tidytable::mutate(value = value / 1000) %>% 
                         tidytable::filter(year %in% seq(1977, new_year + 5)) %>% 
                         tidytable::left_join(rec_mdl_res$derived_quants %>% 
                                                tidytable::filter(Label %in% paste0("SSB_", seq(1977, new_year + 5))) %>% 
                                                tidytable::select(value = Value, sd = StdDev) %>% 
                                                tidytable::mutate(year = seq(1977, new_year + 5),
                                                                  cv = sd / value) %>% 
                                                tidytable::select(-value, -sd)) %>% 
                         tidytable::mutate(sd = cv * value,
                                           type = "Total biomass (1,000s t)") %>% 
                         tidytable::select(-cv)) %>% 
  tidytable::mutate(type = factor(type, levels = c("Total biomass (1,000s t)", "Spawning biomass (1,000s t)")),
                    model = "Recommended") %>% 
  tidytable::bind_rows(base_mdl_res$derived_quants %>% 
                         tidytable::filter(Label %in% paste0("SSB_", seq(1977, new_year + 5))) %>% 
                         tidytable::select(value = Value, sd = StdDev) %>% 
                         tidytable::mutate(year = seq(1977, new_year + 5),
                                           value = value / 2 / 1000,
                                           sd = sd / 2 / 1000,
                                           type = "Spawning biomass (1,000s t)") %>% 
                         tidytable::bind_rows(base_mdl_res$timeseries %>% 
                                                tidytable::select(year = Yr, value = Bio_all) %>% 
                                                tidytable::mutate(value = value / 1000) %>% 
                                                tidytable::filter(year %in% seq(1977, new_year + 5)) %>% 
                                                tidytable::left_join(base_mdl_res$derived_quants %>% 
                                                                       tidytable::filter(Label %in% paste0("SSB_", seq(1977, new_year + 5))) %>% 
                                                                       tidytable::select(value = Value, sd = StdDev) %>% 
                                                                       tidytable::mutate(year = seq(1977, new_year + 5),
                                                                                         cv = sd / value) %>% 
                                                                       tidytable::select(-value, -sd)) %>% 
                                                tidytable::mutate(sd = cv * value,
                                                                  type = "Total biomass (1,000s t)") %>% 
                                                tidytable::select(-cv)) %>% 
                         tidytable::mutate(type = factor(type, levels = c("Total biomass (1,000s t)", "Spawning biomass (1,000s t)")),
                                           model = "Previous")) -> biom_dat




biom_text <- biom_dat %>% 
  tidytable::select(-sd) %>% 
  tidytable::mutate(type = case_when(type == "Spawning biomass (1,000s t)" ~ "ssb",
                                     type == "Total biomass (1,000s t)" ~ "tot")) %>% 
  tidytable::pivot_wider(names_from = c(type, model)) %>% 
  tidytable::mutate(tp = case_when(year <= 2026 ~ 1,
                                   year > 2026 ~ 2),
                    ssb_diff = (ssb_Recommended - ssb_Previous) / ssb_Previous,
                    tot_diff = (tot_Recommended - tot_Previous) / tot_Previous) %>% 
  tidytable::summarise(ssb_diff = mean(ssb_diff),
                       tot_diff = mean(tot_diff),
                       .by = tp) %>% 
  tidytable::pivot_longer(cols = c(ssb_diff, tot_diff)) %>% 
  tidytable::mutate(type = case_when(name == "ssb_diff" ~ "Spawning biomass (1,000s t)",
                                     name == "tot_diff" ~ "Total biomass (1,000s t)"),
                    x = case_when(tp == 1 ~ 2021,
                                  tp == 2 ~ 2028),
                    y = case_when(name == "ssb_diff" ~ 100,
                                  name == "tot_diff" ~ 400),
                    value = scales::percent(value, accuracy = 0.1))



# plot
ggplot(data = biom_dat,
       aes(x = year, y = value, shape = model)) +
  geom_point() +
  geom_line(data = biom_dat %>% filter(model == "Recommended")) +
  facet_wrap(~type, ncol = 1, scales = "free_y") +
  scico::scale_color_scico_d(palette = 'roma') +
  geom_point(data = biom_dat %>% tidytable::filter(year > new_year),
             colour = "darkgreen") +
  geom_line(data = biom_dat %>% tidytable::filter(year > new_year, model == "Recommended"),
            colour = "darkgreen",
            linetype = "dashed") +
  geom_vline(xintercept = new_year + 0.5,
             colour = "darkgreen",
             linetype = "dashed") +
  geom_text(data = data.frame(biom_text), aes(x = x, y = y, label = value), 
            inherit.aes = FALSE,
            hjust = 0) +
  theme_bw(base_size = 14) +
  labs(x = "Year", y = "Estimated quantity", shape = "Model") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.key.width = unit(0.25, 'cm'),
        legend.position = "top")


# save
ggsave(filename = "biomass_24_0a.png",
       path = here::here(new_year, "dev", "model_alts", "2026_analysis_plots"),
       width = 6.5,
       height = 6.5,
       units = "in")


# plot recruitment ----

# set up model to compare with base
rec_mdl_res <- mdl_res_24_0a


# get plot data
rec_mdl_res$parameters %>% 
  tidytable::filter(Label %in% paste0("Main_RecrDev_", seq(1978, new_year - 1))) %>% 
  tidytable::select(value = Value, sd = Parm_StDev) %>% 
  tidytable::mutate(year = seq(1978, new_year - 1),
                    type = "Recruitment deviation") %>% 
  tidytable::bind_rows(rec_mdl_res$derived_quants %>% 
                         tidytable::filter(Label %in% paste0("Recr_", seq(1978, new_year - 1))) %>% 
                         tidytable::select(value = Value, sd = StdDev) %>% 
                         tidytable::mutate(year = seq(1978, new_year - 1),
                                           value = value / 1000000,
                                           sd = sd / 1000000,
                                           type = "Age-0 Recruitment (millions)")) %>% 
  tidytable::mutate(model = "Recommended") %>% 
  tidytable::bind_rows(base_mdl_res$parameters %>% 
                         tidytable::filter(Label %in% paste0("Main_RecrDev_", seq(1978, new_year - 1))) %>% 
                         tidytable::select(value = Value, sd = Parm_StDev) %>% 
                         tidytable::mutate(year = seq(1978, new_year - 1),
                                           type = "Recruitment deviation") %>% 
                         tidytable::bind_rows(base_mdl_res$derived_quants %>% 
                                                tidytable::filter(Label %in% paste0("Recr_", seq(1978, new_year - 1))) %>% 
                                                tidytable::select(value = Value, sd = StdDev) %>% 
                                                tidytable::mutate(year = seq(1978, new_year - 1),
                                                                  value = value / 1000000,
                                                                  sd = sd / 1000000,
                                                                  type = "Age-0 Recruitment (millions)")) %>% 
                         tidytable::mutate(model = "Base")) -> rec_dat

# plot
ggplot(data = rec_dat,
                   aes(x = year, y = value, color = model, pch = model)) +
  geom_point() +
  geom_line(linetype = "dotted") +
  geom_hline(data = rec_dat %>% tidytable::filter(type == "Recruitment deviation"), aes(yintercept = 0)) +
  facet_wrap(~type, ncol = 1, scales = "free_y") +
  # geom_errorbar(aes(ymin = value - 1.96 * sd, 
  #                   ymax = value + 1.96 * sd), 
  #               linewidth = 0.777, width = 0) +
  scico::scale_color_scico_d(palette = 'roma') +
  theme_bw(base_size = 14) +
  labs(x = "Year", y = "Estimated quantity", col = "", pch = "") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.key.width = unit(0.25, 'cm'),
        legend.position = "top")



# save
ggsave(filename = "rec_24_0a.png",
       path = here::here(new_year, "dev", "model_alts", "2026_analysis_plots"),
       width = 6.5,
       height = 6.5,
       units = "in")



# forecast selex ----

# set up model to compare with base
rec_mdl_res <- mdl_res_24_0a

# get plot data
rec_mdl_res$sizeselex %>% 
  dplyr::rename_all(tolower) %>% 
  tidytable::filter(yr == new_year + 1,
                    fleet <= 3,
                    factor == "Lsel") %>% 
  tidytable::select(-factor, -yr, -sex, -label) %>% 
  tidytable::pivot_longer(cols = -fleet) %>% 
  tidytable::rename(length = name) %>% 
  tidytable::mutate(fleet = case_when(fleet == 1 ~ "Trawl",
                                      fleet == 2 ~ "Longline",
                                      fleet == 3 ~ "Pot"),
                    length = as.numeric(length),
                    type = "Recommended",
                    category = "Length") %>% 
  filter(length >= 5) %>% 
  tidytable::bind_rows(base_mdl_res$sizeselex %>% 
                         dplyr::rename_all(tolower) %>% 
                         tidytable::filter(yr == new_year + 1,
                                           fleet <= 3,
                                           factor == "Lsel") %>% 
                         tidytable::select(-factor, -yr, -sex, -label) %>% 
                         tidytable::pivot_longer(cols = -fleet) %>% 
                         tidytable::rename(length = name) %>% 
                         tidytable::mutate(fleet = case_when(fleet == 1 ~ "Trawl",
                                                             fleet == 2 ~ "Longline",
                                                             fleet == 3 ~ "Pot"),
                                           length = as.numeric(length),
                                           type = "Previous",
                                           category = "Length") %>% 
                         filter(length >= 5)) %>% 
  tidytable::bind_rows(rec_mdl_res$ageselex %>% 
                         dplyr::rename_all(tolower) %>% 
                         tidytable::filter(yr == new_year + 1,
                                           fleet <= 3,
                                           factor == "Asel2") %>% 
                         tidytable::select(-factor, -yr, -seas, -sex, - morph, -label) %>% 
                         tidytable::pivot_longer(cols = as.character(seq(0, 10))) %>% 
                         tidytable::rename(length = name) %>% 
                         tidytable::mutate(fleet = case_when(fleet == 1 ~ "Trawl",
                                                             fleet == 2 ~ "Longline",
                                                             fleet == 3 ~ "Pot"),
                                           length = as.numeric(length),
                                           type = "Recommended",
                                           category = "Age") %>% 
                         filter(length >= 1)) %>% 
  tidytable::bind_rows(base_mdl_res$ageselex %>% 
                         dplyr::rename_all(tolower) %>% 
                         tidytable::filter(yr == new_year + 1,
                                           fleet <= 3,
                                           factor == "Asel2") %>% 
                         tidytable::select(-factor, -yr, -seas, -sex, - morph, -label) %>% 
                         tidytable::pivot_longer(cols = as.character(seq(0, 10))) %>% 
                         tidytable::rename(length = name) %>% 
                         tidytable::mutate(fleet = case_when(fleet == 1 ~ "Trawl",
                                                             fleet == 2 ~ "Longline",
                                                             fleet == 3 ~ "Pot"),
                                           length = as.numeric(length),
                                           type = "Previous",
                                           category = "Age") %>% 
                         filter(length >= 1)) -> selex_dat


# plot
ggplot(data = selex_dat,
       aes(x = length, y = value, col = fleet, linetype = type)) +
  geom_line(linewidth = 0.777) +
  facet_wrap(~category, ncol = 1, scales = "free_x") +
  theme_bw(base_size = 14) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        # legend.position = "top",
        legend.key.width = unit(0.25, 'cm')) +
  labs(x = "Age / Length (cm)", y = "Forecast Selectivity", col = "Fishery", linetype = "Model") +
  scico::scale_color_scico_d(palette = 'roma')


# save
ggsave(filename = "fore_selex_24_0a.png",
       path = here::here(new_year, "dev", "model_alts", "2026_analysis_plots"),
       width = 6.5,
       height = 6.5,
       units = "in")


# ref points ----

# get results from management scen runs

if(!dir.exists(here::here(new_year, "dev", "model_alts", "mscen"))){
  scenarios <- paste0("scenario_", seq(1, 8))
  mdls <- c("24.0", "24.0a", "24.0b", "24.0c", "24.0d")
  cyr <- new_year
  syr = 1977
  sex = 2
  fleets = c(1:3)
  Scenario2 = 1
  s2_F = 0.4
  s4_F = 0.75
  
  for(i in 1:length(mdls)){
    
    # get model outputs
    mod_outs <- r4ss::SSgetoutput(dirvec = here::here(new_year, "dev", "model_alts", mdls[i], "mscen", scenarios),
                                  verbose = FALSE)
    mod_summ <- r4ss::SSsummarize(mod_outs,
                                  verbose = FALSE)
    
    # ssb
    mscen_ssb <- mod_summ$quants %>% 
      tidytable::filter(Label %in% paste0("SSB_", seq(cyr, cyr + 13))) %>% 
      tidytable::select(-Label, -replist8) %>% 
      tidytable::rename_with(~paste("Scenario", seq(1, 7)), paste0("replist", seq(1,7))) %>% 
      tidytable::select(Year = Yr, paste("Scenario", seq(1, 7))) %>% 
      tidytable::mutate(across(.cols = names(.)[2:length(names(.))], ~round(. / sex, digits = 0)),
                        across(.cols = names(.)[2:length(names(.))], ~format(., big.mark = ",")))
    
    
    # catch
    cyr_c <- format(round(as.numeric(mod_outs[[1]]$catch %>% 
                                       tidytable::filter(Yr == cyr) %>% 
                                       tidytable::summarise(catch = sum(Obs))), digits = 0), big.mark = ",")
    mscen_catch <- data.table(Year = cyr,
                              "Scenario 1" = cyr_c,
                              "Scenario 2" = cyr_c,
                              "Scenario 3" = cyr_c,
                              "Scenario 4" = cyr_c,
                              "Scenario 5" = cyr_c,
                              "Scenario 6" = cyr_c,
                              "Scenario 7" = cyr_c) %>% 
      tidytable::bind_rows(mod_summ$quants %>% 
                             tidytable::filter(Label %in% paste0("ForeCatch_", seq(cyr, cyr + 13))) %>% 
                             tidytable::select(-Label, -replist8) %>% 
                             tidytable::rename_with(~paste("Scenario", seq(1, 7)), paste0("replist", seq(1,7))) %>% 
                             tidytable::select(Year = Yr, paste("Scenario", seq(1, 7))) %>% 
                             tidytable::mutate(across(.cols = names(.)[2:length(names(.))], ~round(., digits = 0)),
                                               across(.cols = names(.)[2:length(names(.))], ~format(., big.mark = ","))))
    
    # f
    mscen_f <- mod_summ$Fvalue %>% 
      tidytable::filter(Yr %in% seq(cyr, cyr + 13)) %>% 
      tidytable::select(-Label, -replist8) %>% 
      tidytable::rename_with(~paste("Scenario", seq(1, 7)), paste0("replist", seq(1,7))) %>% 
      tidytable::select(Year = Yr, paste("Scenario", seq(1, 7))) %>% 
      tidytable::mutate(across(.cols = names(.)[2:length(names(.))], ~round(., digits = 2)),
                        across(.cols = names(.)[2:length(names(.))], ~format(., big.mark = ",")))
    
    
    # get 2-year projections for catch and F
    SB100 <- mod_summ$quants[which(mod_summ$quants$Label == "SSB_unfished"), 1] / sex
    SB40 <- mod_summ$quants[which(mod_summ$quants$Label == "SSB_SPR"), 1] / sex
    SB35 <- mod_summ$quants[which(mod_summ$quants$Label == "SSB_SPR"), 8] / sex
    F40_1 <- mod_summ$quants[which(mod_summ$quants$Label == paste0("F_", cyr + 1)), 1]
    F35_1 <- mod_summ$quants[which(mod_summ$quants$Label == paste0("F_", cyr + 1)), 6]
    catchABC_1 <- mod_summ$quants[which(mod_summ$quants$Label == paste0("ForeCatch_", cyr + 1)), 1]
    catchOFL_1 <- mod_summ$quants[which(mod_summ$quants$Label == paste0("ForeCatch_", cyr + 1)), 6]
    F40_2 <- mod_summ$quants[which(mod_summ$quants$Label == paste0("F_", cyr + 2)), 1]
    F35_2 <- mod_summ$quants[which(mod_summ$quants$Label == paste0("F_", cyr + 2)), 6]
    catchABC_2 <- mod_summ$quants[which(mod_summ$quants$Label == paste0("ForeCatch_", cyr + 2)), 1]
    catchOFL_2 <- mod_summ$quants[which(mod_summ$quants$Label == paste0("ForeCatch_", cyr + 2)), 8]
    SSB_1 <- mod_summ$quants[which(mod_summ$quants$Label == paste0("SSB_", cyr + 1)), 1] / sex
    SSB_2 <- mod_summ$quants[which(mod_summ$quants$Label == paste0("SSB_", cyr + 2)), 1] / sex
    
    Two_Year <- data.table(Yr = c((cyr + 1):(cyr + 2)),
                           SSB = c(SSB_1, SSB_2),
                           SSB_PER = c(SSB_1 / SB100, SSB_2 / SB100),
                           SB100 = rep(SB100, 2),
                           SB40 = rep(SB40, 2),
                           SB35 = rep(SB35, 2),
                           F40 = c(F40_1, F40_2),
                           F35 = c(F35_1, F35_2),
                           C_ABC = c(catchABC_1, catchABC_2),
                           C_OFL = c(catchOFL_1, catchOFL_2))
    
    # output list
    mscen <- list(mscen_ssb = mscen_ssb,
                  mscen_catch = mscen_catch,
                  mscen_f = mscen_f,
                  Two_year = Two_Year)
    
    # save results
    if (!dir.exists(here::here(new_year, "dev", "model_alts", "mscen"))) {
      dir.create(here::here(new_year, "dev", "model_alts", "mscen"), recursive = TRUE)
    }
    save(mscen, file = here::here(new_year, "dev", "model_alts", "mscen", paste0("mgmnt_scen_", mdls[i], ".RData")))
    
  }
}

# read in results
# 24.0
load(here::here(new_year, "dev", "model_alts", "mscen", "mgmnt_scen_24.0.RData"))
mscen_base <- mscen

# 24.0a
load(here::here(new_year, "dev", "model_alts", "mscen", "mgmnt_scen_24.0a.RData"))
mscen_24_0a <- mscen

# 24.0b
load(here::here(new_year, "dev", "model_alts", "mscen", "mgmnt_scen_24.0b.RData"))
mscen_24_0b <- mscen

# 24.0c
load(here::here(new_year, "dev", "model_alts", "mscen", "mgmnt_scen_24.0c.RData"))
mscen_24_0c <- mscen

# 24.0d
load(here::here(new_year, "dev", "model_alts", "mscen", "mgmnt_scen_24.0d.RData"))
mscen_24_0d <- mscen

# table of key quantities
ref_pnts <- data.table(names = c("SSB_status", "Bzero","F40", "ABC"),
                       "base" = c(round(mscen_base$Two_year$SSB_PER[1], digits = 3),
                                  round(mscen_base$Two_year$SB100[1], digits = 0),
                                  round(mscen_base$Two_year$F40[1], digits = 3),
                                  round(mscen_base$Two_year$C_ABC[1], digits = 0)),
                       "24.0a" = c(round(mscen_24_0a$Two_year$SSB_PER[1], digits = 3),
                                   round(mscen_24_0a$Two_year$SB100[1], digits = 0),
                                   round(mscen_24_0a$Two_year$F40[1], digits = 3),
                                   round(mscen_24_0a$Two_year$C_ABC[1], digits = 0)),
                       "24.0b" = c(round(mscen_24_0b$Two_year$SSB_PER[1], digits = 3),
                                   round(mscen_24_0b$Two_year$SB100[1], digits = 0),
                                   round(mscen_24_0b$Two_year$F40[1], digits = 3),
                                   round(mscen_24_0b$Two_year$C_ABC[1], digits = 0)),
                       "24.0c" = c(round(mscen_24_0c$Two_year$SSB_PER[1], digits = 3),
                                   round(mscen_24_0c$Two_year$SB100[1], digits = 0),
                                   round(mscen_24_0c$Two_year$F40[1], digits = 3),
                                   round(mscen_24_0c$Two_year$C_ABC[1], digits = 0)),
                       "24.0d" = c(round(mscen_24_0d$Two_year$SSB_PER[1], digits = 3),
                                   round(mscen_24_0d$Two_year$SB100[1], digits = 0),
                                   round(mscen_24_0d$Two_year$F40[1], digits = 3),
                                   round(mscen_24_0d$Two_year$C_ABC[1], digits = 0)))

vroom::vroom_write(ref_pnts, here::here(new_year, "dev", "model_alts", "2026_analysis_tbls", "ref_pnts.csv"), delim = ",")







