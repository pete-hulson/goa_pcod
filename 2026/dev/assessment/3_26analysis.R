
library(tidyverse)
library(data.table)

# Current assessment year
new_year <- as.numeric(format(Sys.Date(), format = "%Y"))
# recommended model name
prev_mdl <- "24.0"
rec_mdl <- "24.0a"

rec_mdl_res <- r4ss::SS_output(dir = here::here(new_year, "mgmt", rec_mdl),
                               verbose = FALSE,
                               printstats = FALSE)


prev_rec_mdl_res <- r4ss::SS_output(dir = here::here(new_year, "mgmt", prev_mdl),
                               verbose = FALSE,
                               printstats = FALSE)


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
  tidytable::left_join(prev_rec_mdl_res$wtatage %>% 
                         tidytable::filter(fleet == -1) %>% 
                         tidytable::select(-year, -seas, -sex, -bio_pattern, -birthseas, -fleet, -'0') %>% 
                         tidytable::pivot_longer() %>% 
                         tidytable::summarise(pred_prev = mean(value), .by = name) %>% 
                         tidytable::mutate(age = as.numeric(name),
                                           name = "Weight (kg)") %>% 
                         tidytable::bind_rows(prev_rec_mdl_res$endgrowth[,c('Real_Age', 'Len_Mid')] %>% 
                                                tidytable::rename(age = Real_Age, pred_prev = Len_Mid) %>% 
                                                tidytable::filter(age > 0) %>% 
                                                tidytable::mutate(name = "Length (cm)"))) %>%
  tidytable::drop_na() -> grwth_data

# plot
ggplot(data = grwth_data %>% filter(year != "All"),
                     aes(x = age, y = value, col = factor(year))) +
  geom_pointrange(data = grwth_data %>% filter(year != "All"),
                  aes(ymin = value - 1.96 * sd_value, ymax = value + 1.96 * sd_value),
                  position = position_jitter(width = 0.05),
                  linetype = 'dotted',
                  fatten = 2) +
  geom_pointrange(data = grwth_data %>% filter(year == "All"),
                  aes(ymin = value - 1.96 * sd_value, ymax = value + 1.96 * sd_value),
                  # linetype = 'dotted',
                  pch = 18,
                  fatten = 7) +
  geom_line(aes(y = pred), col = "black", linewidth = 0.777) +
  geom_line(aes(y = pred_prev), col = "red", linewidth = 0.777, linetype = "dashed") +
  facet_wrap(~ name, 
             ncol = 1, 
             scales = "free_y") +
  scico::scale_color_scico_d(palette = 'roma') +
  theme_bw(base_size = 14) +
  scale_x_continuous(breaks = c(1:10), limits = c(0.5, 10.5)) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  labs(x = "Age", y = "Pacific cod size", col = "Year")


grwth_data %>% 
  tidytable::filter(year == "All") %>% 
  tidytable::mutate(ssq_prev = (value - pred_prev)^2,
                    ssq_rec = (value - pred)^2) %>% 
  tidytable::summarise(ssq_prev = sum(ssq_prev),
                       ssq_rec = sum(ssq_rec),
                       .by = name)


# plot fit to survey


# get plot data
data.table(rec_mdl_res$cpue) %>% 
  tidytable::mutate(obs = as.numeric(Obs),
                    sd = as.numeric(SE_input) * obs,
                    pred = as.numeric(Exp)) %>% 
  tidytable::select(srv = Fleet, year = Yr, obs, pred, sd) %>% 
  tidytable::filter(srv %in% c(4, 5)) %>% 
  tidytable::mutate(name = case_when(srv == 4 ~ "AFSC trawl survey numbers (1000s)",
                                     srv == 5 ~ "AFSC longline survey RPNs")) %>% 
  tidytable::select(-srv) -> indx_dat

# plot
ggplot(data = indx_dat, aes(x = year, y = obs, col = name)) +
  geom_point() +
  geom_line(aes(y = pred), linewidth = 0.777) +
  theme_bw(base_size = 14) +
  facet_wrap(~ name, 
             ncol = 1, 
             scales = "free_y") +
  geom_errorbar(aes(ymin = obs - 1.96 * sd, ymax = obs + 1.96 * sd), width = 0) +
  scico::scale_color_scico_d(palette = 'roma') +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(vjust = 0.5, angle = 90),
        legend.position = "none") +
  labs(x = "Year", y = "Pacific cod survey index") +
  scale_x_continuous(breaks = c(min(indx_dat$year):max(indx_dat$year)), limits = c(min(indx_dat$year), max(indx_dat$year)))

# save
ggsave(filename = "srv_indx.png",
       path = here::here(new_year, "output", "safe_plots"),
       width = 6.5,
       height = 5.5,
       units = "in")


# plot biomass
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
  tidytable::bind_rows(prev_rec_mdl_res$derived_quants %>% 
                         tidytable::filter(Label %in% paste0("SSB_", seq(1977, new_year + 5))) %>% 
                         tidytable::select(value = Value, sd = StdDev) %>% 
                         tidytable::mutate(year = seq(1977, new_year + 5),
                                           value = value / 2 / 1000,
                                           sd = sd / 2 / 1000,
                                           type = "Spawning biomass (1,000s t)") %>% 
                         tidytable::bind_rows(prev_rec_mdl_res$timeseries %>% 
                                                tidytable::select(year = Yr, value = Bio_all) %>% 
                                                tidytable::mutate(value = value / 1000) %>% 
                                                tidytable::filter(year %in% seq(1977, new_year + 5)) %>% 
                                                tidytable::left_join(prev_rec_mdl_res$derived_quants %>% 
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

# plot
ggplot(data = biom_dat,
                  aes(x = year, y = value, color = type, shape = model)) +
  geom_point() +
  geom_line(data = biom_dat %>% filter(model == "Recommended")) +
  facet_wrap(~type, ncol = 1, scales = "free_y") +
  # geom_errorbar(aes(ymin = value - 1.96 * sd, 
  #                   ymax = value + 1.96 * sd), 
  #               linewidth = 0.777, width = 0) +
  scico::scale_color_scico_d(palette = 'roma') +
  geom_point(data = biom_dat %>% tidytable::filter(year > new_year),
             colour = "darkgreen") +
  geom_line(data = biom_dat %>% tidytable::filter(year > new_year, model == "Recommended"),
            colour = "darkgreen",
            linetype = "dashed") +
  # geom_errorbar(data = biom_dat %>% tidytable::filter(year > new_year),
  #               colour = "darkgreen",
  #               aes(ymin = value - 1.96 * sd, 
  #                   ymax = value + 1.96 * sd), 
  #               linewidth = 0.777, width = 0,
  #               linetype = "dashed") +
  geom_vline(xintercept = new_year + 0.5,
             colour = "darkgreen",
             linetype = "dashed") +
  theme_bw(base_size = 14) +
  labs(x = "Year", y = "Estimated quantity", col = "") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.key.width = unit(0.25, 'cm'),
        legend.position = "none")




# ref points

load(here::here(new_year, "output", "mscen", "mgmnt_scen_rec.RData"))
mscen_rec <- mscen
rec_2yr <- mscen_rec$Two_year


load(here::here(new_year, "output", "mscen", "mgmnt_scen_base.RData"))
mscen_base <- mscen
base_2yr <- mscen_base$Two_year




(base_2yr$C_ABC[1] - rec_2yr$C_ABC[1]) / base_2yr$C_ABC[1]

(base_2yr$SSB[1] - rec_2yr$SSB[1]) / base_2yr$SSB[1]




# forecast selex

# get plot data


rec_mdl_res$sizeselex %>% 
  dplyr::rename_all(tolower) %>% 
  tidytable::filter(yr == new_year + 1,
                    fleet <= 3,
                    factor == "Lsel") %>% 
  tidytable::select(-factor, -yr, -sex, -label) %>% 
  tidytable::pivot_longer(cols = as.character(seq(1, 105))) %>% 
  tidytable::rename(length = name) %>% 
  tidytable::mutate(fleet = case_when(fleet == 1 ~ "Trawl fishery",
                                      fleet == 2 ~ "Longline fishery",
                                      fleet == 3 ~ "Pot fishery"),
                    length = as.numeric(length),
                    type = "Recommended",
                    category = "Length") %>% 
  filter(length >= 5) %>% 
  tidytable::bind_rows(prev_rec_mdl_res$sizeselex %>% 
                         dplyr::rename_all(tolower) %>% 
                         tidytable::filter(yr == new_year + 1,
                                           fleet <= 3,
                                           factor == "Lsel") %>% 
                         tidytable::select(-factor, -yr, -sex, -label) %>% 
                         tidytable::pivot_longer(cols = as.character(seq(1, 105))) %>% 
                         tidytable::rename(length = name) %>% 
                         tidytable::mutate(fleet = case_when(fleet == 1 ~ "Trawl fishery",
                                                             fleet == 2 ~ "Longline fishery",
                                                             fleet == 3 ~ "Pot fishery"),
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
                         tidytable::mutate(fleet = case_when(fleet == 1 ~ "Trawl fishery",
                                                             fleet == 2 ~ "Longline fishery",
                                                             fleet == 3 ~ "Pot fishery"),
                                           length = as.numeric(length),
                                           type = "Recommended",
                                           category = "Age") %>% 
                         filter(length >= 1)) %>% 
  tidytable::bind_rows(prev_rec_mdl_res$ageselex %>% 
                         dplyr::rename_all(tolower) %>% 
                         tidytable::filter(yr == new_year + 1,
                                           fleet <= 3,
                                           factor == "Asel2") %>% 
                         tidytable::select(-factor, -yr, -seas, -sex, - morph, -label) %>% 
                         tidytable::pivot_longer(cols = as.character(seq(0, 10))) %>% 
                         tidytable::rename(length = name) %>% 
                         tidytable::mutate(fleet = case_when(fleet == 1 ~ "Trawl fishery",
                                                             fleet == 2 ~ "Longline fishery",
                                                             fleet == 3 ~ "Pot fishery"),
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
        legend.position = "top",
        legend.key.width = unit(0.25, 'cm')) +
  labs(x = "Age / Length (cm)", y = "Forecast Selectivity", col = "Fishery", linetype = "Model") +
  scico::scale_color_scico_d(palette = 'roma')




# get plot data
rec_mdl_res$ageselex %>% 
  dplyr::rename_all(tolower) %>% 
  tidytable::filter(yr == new_year,
                    fleet <= 5,
                    factor == "Asel2") %>% 
  tidytable::select(-factor, -yr, -sex, -label) %>% 
  tidytable::pivot_longer(cols = as.character(seq(0, 10))) %>% 
  tidytable::rename(age = name) %>% 
  tidytable::mutate(fleet = case_when(fleet == 1 ~ "Trawl fishery",
                                      fleet == 2 ~ "Longline fishery",
                                      fleet == 3 ~ "Pot fishery",
                                      fleet == 4 ~ "AFSC bottom trawl survey",
                                      fleet == 5 ~ "AFSC longline survey"),
                    age = as.numeric(age)) %>% 
  filter(age >= 1) -> selex_dat

# plot
ggplot(data = selex_dat,
       aes(x = age, y = value, col = fleet)) +
  geom_line(linewidth = 0.777) +
  theme_bw(base_size = 14) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "top",
        legend.key.width = unit(0.25, 'cm')) +
  labs(x = "Length (cm)", y = paste(new_year, "Selectivity"), col = "") +
  scico::scale_color_scico_d(palette = 'roma') + 
  guides(color = guide_legend(nrow = 2))


# get plot data
prev_rec_mdl_res$ageselex %>% 
  dplyr::rename_all(tolower) %>% 
  tidytable::filter(yr >= new_year - 4,
                    fleet <= 3,
                    factor == "Asel2") %>% 
  print(n=100)
  tidytable::select(-factor, -yr, -sex, -label) %>% 
  tidytable::pivot_longer(cols = as.character(seq(0, 10))) %>% 
  tidytable::rename(age = name) %>% 
  tidytable::mutate(fleet = case_when(fleet == 1 ~ "Trawl fishery",
                                      fleet == 2 ~ "Longline fishery",
                                      fleet == 3 ~ "Pot fishery",
                                      fleet == 4 ~ "AFSC bottom trawl survey",
                                      fleet == 5 ~ "AFSC longline survey"),
                    age = as.numeric(age)) %>% 
  filter(age >= 1) -> selex_dat

# plot
ggplot(data = selex_dat,
       aes(x = age, y = value, col = fleet)) +
  geom_line(linewidth = 0.777) +
  theme_bw(base_size = 14) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "top",
        legend.key.width = unit(0.25, 'cm')) +
  labs(x = "Length (cm)", y = paste(new_year, "Selectivity"), col = "") +
  scico::scale_color_scico_d(palette = 'roma') + 
  guides(color = guide_legend(nrow = 2))
























