## Script to run 2023 GOA Pacific Cod Assessment plot code

## Load required packages & define parameters ----

libs <- c("r4ss",
          "KernSmooth",
          "stringr",
          "data.table",
          "ggplot2",
          "RODBC",
          "data.table",
          "ggplot2",
          "ggrepel",
          "car",
          "dplyr",
          "tidyr",
          "magrittr",
          "nmfspalette",
          "afscdata")


pkg_cran <- c("data.table",
              "tidyverse",
              "vroom",
              "here",
              "tictoc",
              "adnuts",
              "flextable", 
              "R.utils", 
              "parallel", 
              "doParallel", 
              "foreach",
              "SimDesign")


if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )]) > 0) {
  install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE)])}

if("nmfspalette" %in% installed.packages() == FALSE){
  remotes::install_github("nmfs-fish-tools/nmfspalette")}

if("ss3diags" %in% installed.packages() == FALSE){
  remotes::install_github("jabbamodel/ss3diags", force = TRUE)}



lapply(libs, library, character.only = TRUE)

# set up ----
# recommended model name
rec_mdl <- "2019.1e.5cm-2024"
# last year's model with updated data (base model)
base_mdl <- "2019.1b-2024"
# last year's model
prev_mdl <- "2019.1b-2023"
# Current assessment year
new_year <- as.numeric(format(Sys.Date(), format = "%Y"))

# read in necessary info ----

# read in model output
rec_mdl_res <- r4ss::SS_output(dir = here::here(new_year, "mgmt", rec_mdl),
                               verbose = FALSE,
                               printstats = FALSE)
base_mdl_res <- r4ss::SS_output(dir = here::here(new_year, "mgmt", base_mdl),
                                verbose = FALSE,
                                printstats = FALSE)
prev_mdl_res <- r4ss::SS_output(dir = here::here(new_year - 1, "mgmt", prev_mdl),
                                verbose = FALSE,
                                printstats = FALSE)

# catch data
fed_raw <- vroom::vroom(here::here(new_year, "data", "raw", "fish_catch_data.csv"), 
                        progress = FALSE, 
                        show_col_types = FALSE)

# pel twl catch data
pel_twl <- vroom::vroom(here::here(new_year, "data", "raw", "pel_twl.csv"), 
                        progress = FALSE, 
                        show_col_types = FALSE)

# swf catch data
swf <- vroom::vroom(here::here(new_year, "data", "raw", "swf_catch.csv"), 
                    progress = FALSE, 
                    show_col_types = FALSE)
# env data
env_data <- vroom::vroom(here::here(new_year, 'data', 'raw_cfsr.csv'), 
                         progress = FALSE, 
                         show_col_types = FALSE)
# survey specimen data
age_data <- vroom::vroom(here::here(new_year, 'data', 'raw', 'twl_srvy_age.csv'), 
                         progress = FALSE, 
                         show_col_types = FALSE)

# retrospective results
load(here::here(new_year, "output", "retro", "retrosumm_rec.RData"))
# historical models
hist_mdls <- vroom::vroom(here::here(new_year, 'data', 'hist_mdls.csv'), 
                          progress = FALSE, 
                          show_col_types = FALSE)
# leave-one-out results
load(here::here(new_year, "output", "loo", "loo_data.RData"))
load(here::here(new_year, "output", "loo", "loo_year.RData"))





# set up directory to plop tables into
if (!dir.exists(here::here(new_year, "output", "safe_plots"))) {
  dir.create(here::here(new_year, "output", "safe_plots"), recursive = TRUE)
}






# run r4ss to plot recommended model ----

# plot recommended model
r4ss::SS_plots(rec_mdl_res,
               printfolder = "",
               dir = here::here(new_year, "output", "safe_plots", "r4ss"))

# plot catch by fleet ----
r4ss::SSplotCatch(rec_mdl_res,
                  subplots = 2,
                  fleetcols = scico::scico(3, palette = 'roma'),
                  plot = FALSE,
                  print = TRUE,
                  pheight = 3.777,
                  plotdir = here::here(new_year, "output", "safe_plots"))
invisible(file.rename(from = here::here(new_year, "output", "safe_plots", "catch2_landings_stacked.png"),
                      to = here::here(new_year, "output", "safe_plots", "catch.png")))


# plot model data ----
r4ss::SSplotData(rec_mdl_res,
                 subplots = 2,
                 fleetcol = scico::scico(5, palette = 'roma'),
                 plot = FALSE,
                 print = TRUE,
                 pheight = 6.5,
                 plotdir = here::here(new_year, "output", "safe_plots"))
invisible(file.rename(from = here::here(new_year, "output", "safe_plots", "data_plot2.png"),
                      to = here::here(new_year, "output", "safe_plots", "data.png")))


# plot vessel participation ----
tidytable::expand_grid(year = seq(new_year - 20, new_year),
                       area = c("Central gulf", "Western gulf"),
                       gear = c("Longline", "Jig", "Pot", "Trawl")) %>% 
  tidytable::left_join(fed_raw %>% 
                         tidytable::filter(trip_target_code == "C",
                                           year >= new_year - 20,
                                           fmp_subarea %in% c("CG", "WG")) %>% 
                         tidytable::summarise(num_ves = length(unique(vessel_id)), 
                                              .by = c(year, fmp_gear, fmp_subarea)) %>% 
                         tidytable::mutate(gear = case_when(fmp_gear == "HAL" ~ "Longline",
                                                            fmp_gear == "JIG" ~ "Jig",
                                                            fmp_gear == "OTH" ~ "Other",
                                                            fmp_gear == "POT" ~ "Pot",
                                                            fmp_gear == "TRW" ~ "Trawl"),
                                           area = case_when(fmp_subarea == "CG" ~ "Central gulf",
                                                            fmp_subarea == "WG" ~ "Western gulf")) %>% 
                         tidytable::select(year, area, gear, num_ves)) %>% 
  tidytable::mutate(num_ves = replace_na(num_ves, 0)) -> num_vess


vess_plot <- ggplot(num_vess, 
                    aes(x = year, y = num_ves, fill = gear)) +
  geom_area(stat = "identity") +
  scico::scale_fill_scico_d(palette = 'roma') +
  scale_x_continuous(breaks = c((new_year - 20):new_year), limits = c((new_year - 20), new_year)) +
  theme_bw(base_size = 14) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(vjust = 0.5, angle = 90),
        legend.position = "top") +
  labs(y = "Number of vessels", x = "Year", fill = "Gear Type") +
  facet_wrap( ~ area, scale = "free_y", nrow = 2)

ggsave(filename = "num_vess.png",
       path = here::here(new_year, "output", "safe_plots"),
       width = 6.5,
       height = 4.5,
       units = "in")

# plot cumulative catch ----

tidytable::expand_grid(year = seq(new_year - 4, new_year - 1),
                       gear = c("Longline", "Jig", "Pot", "Trawl"), 
                       area = c("Central gulf", "Western gulf"),
                       week = 0:52) %>% 
  tidytable::bind_rows(expand.grid(year = new_year, 
                                   gear = c("Longline", "Jig", "Pot", "Trawl"), 
                                   area = c("Central gulf", "Western gulf"),
                                   week = 0:as.numeric(format(Sys.Date(), format = "%W")))) %>% 
  tidytable::left_join(fed_raw %>% 
                         tidytable::mutate(week = as.numeric(as.character(format(as.Date(week_end_date), "%W")))) %>% 
                         tidytable::filter(year >= new_year - 4,
                                           fmp_subarea %in% c("CG", "WG")) %>% 
                         tidytable::mutate(gear = case_when(fmp_gear == "HAL" ~ "Longline",
                                                            fmp_gear == "JIG" ~ "Jig",
                                                            fmp_gear == "OTH" ~ "Other",
                                                            fmp_gear == "POT" ~ "Pot",
                                                            fmp_gear == "TRW" ~ "Trawl"),
                                           area = case_when(fmp_subarea == "CG" ~ "Central gulf",
                                                            fmp_subarea == "WG" ~ "Western gulf")) %>% 
                         tidytable::summarise(tons = sum(weight_posted), .by = c(week, gear, year, area))) %>% 
  tidytable::mutate(tons = replace_na(tons, 0)) %>% 
  tidytable::mutate(catch = cumsum(tons), .by = c(year, gear, area)) -> cumul_catch

cumul_plot <- ggplot(data = cumul_catch, 
                   aes(x = week, y = catch, color = factor(year))) + 
  geom_point() + 
  geom_path(aes(group = year)) +
  facet_grid(gear ~ area, scale = "free_y") +
  theme_bw(base_size = 14) +
  theme(axis.text.x = element_text(vjust = 0.5, angle = 90)) +
  scico::scale_color_scico_d(palette = 'roma') +
  labs(x = "Week", y = "Cummulative Catch (t)", color = "Year")

ggsave(filename = "cumul_catch.png",
       path = here::here(new_year, "output", "safe_plots"),
       width = 6.5,
       height = 6.5,
       units = "in")


# auxiliary indices ----
pel_twl %>% 
  tidytable::summarise(ncod = length(unique(weight_kg[species == 202])),
                       nhauls = length(unique(weight_kg)),
                       .by = c(year, area)) %>% 
  tidytable::mutate(pcod = ncod / nhauls) %>% 
  tidytable::summarise(pcod = mean(pcod), .by = year) %>% 
  tidytable::mutate(type = "Proportion of pelagic hauls with Pcod",
                    index = "Recruitment") %>% 
  tidytable::bind_rows(swf %>% 
                         tidytable::summarise(cod = sum(weight_kg[species == 202]),
                                              tot = sum(weight_kg),
                                              .by = year) %>% 
                         tidytable::mutate(pcod = cod / tot) %>% 
                         tidytable::mutate(type = "Proportion of Pcod catch in Shallow water flatfish",
                                           index = "Adult") %>% 
                         tidytable::select(year, pcod, type, index)) %>% 
  tidytable::bind_rows(data.table(rec_mdl_res$cpue) %>% 
                         tidytable::filter(Fleet == 7) %>% 
                         tidytable::mutate(pcod = as.numeric(Obs),
                                           type = "ADF&G trawl survey (density)",
                                           index = "Adult") %>% 
                         tidytable::bind_rows(data.table(rec_mdl_res$cpue) %>% 
                                                tidytable::filter(Fleet == 9) %>% 
                                                tidytable::mutate(pcod = as.numeric(Obs),
                                                                  type = "Age-0 beach seine survey (numbers/haul)",
                                                                  index = "Recruitment")) %>% 
                         tidytable::select(year = Yr, pcod, type, index)) %>% 
  tidytable::filter(year >= 2006) -> aux_indx_dat


aux_indx <- ggplot(aux_indx_dat, aes(year, pcod, colour = index)) +
  geom_line(linetype = 2) +
  geom_point() +
  facet_wrap(~type, ncol = 2, scale = "free_y", labeller = labeller(type = label_wrap_gen(30))) +
  scico::scale_color_scico_d(palette = 'roma') +
  labs(x = "Year", y = "Pacific cod auxiliary indices", color = "Index") +
  theme_bw(base_size = 14) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(vjust = 0.5, angle = 90),
        legend.position = "top") +
  scale_x_continuous(breaks = c(2006:new_year), limits = c(2006, new_year))

ggsave(filename = "aux_indx.png",
       path = here::here(new_year, "output", "safe_plots"),
       width = 6.5,
       height = 6.5,
       units = "in")


# fitted survey indices ----

data.table(rec_mdl_res$cpue) %>% 
  tidytable::mutate(obs = as.numeric(Obs),
                    sd = as.numeric(SE_input) * obs,
                    pred = as.numeric(Exp)) %>% 
  tidytable::select(srv = Fleet, year = Yr, obs, pred, sd) %>% 
  tidytable::filter(srv %in% c(4, 5)) %>% 
  tidytable::mutate(name = case_when(srv == 4 ~ "AFSC trawl survey numbers (1000s)",
                                     srv == 5 ~ "AFSC longline survey RPNs")) %>% 
  tidytable::select(-srv) -> indx_dat


ggplot(data = indx_dat, aes(x = year, y = obs, col = name)) +
  geom_point() +
  geom_line(aes(y = pred), size = 0.777) +
  theme_bw(base_size = 14) +
  facet_wrap(~ name, 
             ncol = 1, 
             scales = "free_y") +
  geom_errorbar(aes(ymin = obs - 1.96 * sd, ymax = obs + 1.96 * sd), width = 0.25) +
  scico::scale_color_scico_d(palette = 'roma') +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(vjust = 0.5, angle = 90),
        legend.position = "none") +
  labs(x = "Year", y = "Pacific cod survey index") +
  scale_x_continuous(breaks = c(min(indx_dat$year):max(indx_dat$year)), limits = c(min(indx_dat$year), max(indx_dat$year)))

ggsave(filename = "srv_indx.png",
       path = here::here(new_year, "output", "safe_plots"),
       width = 6.5,
       height = 5.5,
       units = "in")

# cfsr index ----
env_data %>% 
  filter(Month == 6) %>%
  select(Year, '0_20') %>% 
  rename(l20 = '0_20') %>% 
  mutate(sub = case_when(Year <= 2012 & Year >= 1982 ~ l20),
         reg_mu = mean(sub, na.rm = TRUE),
         anomaly = l20 - reg_mu) %>% 
  select(Year, anomaly) %>% 
  mutate(sign = case_when(anomaly > 0 ~ "Pos",
                          anomaly < 0 ~ "Neg")) -> temp_anom

temp_plot <- ggplot(data = temp_anom, 
                aes(x = Year, y = anomaly, fill = sign)) + 
  geom_bar(stat = "identity", width = 0.777) + 
  geom_hline(yintercept = 0) +
  scico::scale_fill_scico_d(limits = c("Pos", "Neg"), palette = 'roma') +
  theme_bw(base_size = 14) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(vjust = 0.5, angle = 90),
        legend.position = 'none') +
  labs(y = "Temperature anomaly", x = "Year")

ggsave(filename = "temp_anom.png",
       path = here::here(new_year, "output", "safe_plots"),
       width = 6.5,
       height = 4.5,
       units = "in")

# retrospectives ----

# get recommended model ssb
rec_mdl_res$derived_quants %>% 
  tidytable::select(Label, Value, StdDev) %>% 
  tidytable::slice(grep("SSB", Label, perl = TRUE)) %>% 
  tidytable::filter(!(Label %in% c("SSB_Virgin", "SSB_Initial", "SSB_unfished", "SSB_Btgt", "SSB_SPR", "SSB_MSY", "B_MSY/SSB_unfished"))) %>% 
  tidytable::mutate(Year = as.numeric(substr(Label, start = nchar(Label) - 3, stop = nchar(Label)))) %>% 
  tidytable::filter(Year <= new_year) %>% 
  tidytable::select(year = Year, Value, sd = StdDev) %>% 
  tidytable::mutate(Value = Value / 2, 
                    sd = sd / 2,
                    uci = Value + 1.96 * sd,
                    lci = Value - 1.96 * sd) %>% 
  tidytable::rename(!!paste0("a", new_year) := Value) -> rec_ssb

# get retro runs (data and model)
retrosumm_rec$SpawnBio %>% 
  tidytable::filter(Yr %in% seq(1977, new_year)) %>% 
  tidytable::mutate(replist2 = case_when(Yr >= new_year ~ NA,
                                         .default = replist2),
                    replist3 = case_when(Yr >= new_year - 1 ~ NA,
                                         .default = replist3),
                    replist4 = case_when(Yr >= new_year - 2 ~ NA,
                                         .default = replist4),
                    replist5 = case_when(Yr >= new_year - 3 ~ NA,
                                         .default = replist5),
                    replist6 = case_when(Yr >= new_year - 4 ~ NA,
                                         .default = replist6),
                    replist7 = case_when(Yr >= new_year - 5 ~ NA,
                                         .default = replist7),
                    replist8 = case_when(Yr >= new_year - 6 ~ NA,
                                         .default = replist8),
                    replist9 = case_when(Yr >= new_year - 7 ~ NA,
                                         .default = replist9),
                    replist10 = case_when(Yr >= new_year - 8 ~ NA,
                                         .default = replist10),
                    replist11 = case_when(Yr >= new_year - 9 ~ NA,
                                         .default = replist11)) %>% 
  tidytable::pivot_longer(cols = c(paste0("replist", seq(1, 11)))) %>% 
  tidytable::mutate(peel = case_when(name == "replist1" ~ "0 years",
                                     name == "replist2" ~ "-1 years",
                                     name == "replist3" ~ "-2 years",
                                     name == "replist4" ~ "-3 years",
                                     name == "replist5" ~ "-4 years",
                                     name == "replist6" ~ "-5 years",
                                     name == "replist7" ~ "-6 years",
                                     name == "replist8" ~ "-7 years",
                                     name == "replist9" ~ "-8 years",
                                     name == "replist10" ~ "-9 years",
                                     name == "replist11" ~ "-10 years"),
                    type = "Data retrospective (10 years)") %>% 
  tidytable::select(type, peel, year = Yr, ssb = value) %>% 
  tidytable::drop_na() %>% 
  tidytable::mutate(ssb = ssb / 2) %>% 
  tidytable::bind_rows(hist_mdls %>% 
                         tidytable::right_join(rec_ssb %>% 
                                                 tidytable::select(year, paste0("a", new_year))) %>% 
                         # write out appended historical models
                         vroom::vroom_write(., here::here(new_year, "output", "retro", 'hist_mdls.csv'), delim = ",") %>% 
                         tidytable::pivot_longer(cols = paste0("a", seq(2003, new_year))) %>% 
                         tidytable::mutate(peel = paste(as.numeric(substr(name, 2, 5)) - new_year, "years"),
                                           type = "Model retrospective (to 2003 assessment)") %>% 
                         tidytable::select(type, peel, year, ssb = value) %>% 
                         tidytable::drop_na()) %>% 
  tidytable::mutate(peel = factor(peel, levels = paste(seq(0, 2003 - new_year), "years"))) %>% 
  tidytable::left_join(rec_ssb %>% 
                         tidytable::select(year, uci, lci)) -> retro_dat

# plot and save
retro_plot <- ggplot(data = retro_dat, 
       aes(x = year, y = ssb, color = peel)) +
  geom_ribbon(aes(ymax = uci, ymin = lci), alpha = 0.5, color = NA) +
  geom_line(size = 0.777) +
  facet_wrap(~ type, ncol = 1, scale = "free_y") +
  scico::scale_color_scico_d(palette = 'roma') +
  labs(x = "Year", y = "Spawning biomass (t)", color = "Retrospective peel") +
  theme_bw(base_size = 14) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(vjust = 0.5, angle = 90),
        legend.position = "none")

ggsave(filename = "retro.png",
       path = here::here(new_year, "output", "safe_plots"),
       width = 6.5,
       height = 6.5,
       units = "in")


## leave-one-out ----

loo_data
ggsave(filename = "loo_data.png",
       path = here::here(new_year, "output", "safe_plots"),
       width = 6.5,
       height = 6.5,
       units = "in")

loo_year[[2]]
ggsave(filename = "loo_year.png",
       path = here::here(new_year, "output", "safe_plots"),
       width = 6.5,
       height = 6.5,
       units = "in")

# aggregated length comp fit ----


r4ss::SSplotComps(replist = rec_mdl_res,
                  subplots = 21,
                  kind = "LEN",
                  fleetnames = c("Trawl Fishery", "Longline Fishery", "Pot Fishery", "AFSC Bottom Trawl Survey", "AFSC Longline Survey"),
                  showsampsize = FALSE,
                  showeffN = FALSE,
                  linescol = scico::scico(1, palette='roma'),
                  # plot = FALSE,
                  print = TRUE,
                  pheight = 6.5,
                  plotdir = here::here(new_year, "output", "safe_plots"))
invisible(file.rename(from = here::here(new_year, "output", "safe_plots", "comp_lenfit__aggregated_across_time.png"),
                      to = here::here(new_year, "output", "safe_plots", "agg_lencomp.png")))



# growth fit plots ----

# get growth parameters
t0 = as.numeric(rec_mdl_res$Growth_Parameters$A_a_L0)
Linf = as.numeric(rec_mdl_res$Growth_Parameters$Linf)
k = as.numeric(rec_mdl_res$Growth_Parameters$K)

# get growth obs and pred
age_data %>% 
  tidytable::select(year, age, "Length (cm)" = length, "Weight (kg)" = weight) %>% 
  tidytable::pivot_longer(cols = c("Length (cm)", "Weight (kg)")) %>% 
  tidytable::drop_na() %>% 
  tidytable::filter(age <= 10) %>% 
  tidytable::summarise(value = mean(value),
                       sd_value = sd(value), .by = c(year, age, name)) %>% 
  tidytable::left_join(rec_mdl_res$wtatage %>% 
                         tidytable::filter(fleet == -1) %>% 
                         tidytable::select(-year, -seas, -sex, -bio_pattern, -birthseas, -fleet, -'0') %>% 
                         tidytable::pivot_longer() %>% 
                         tidytable::summarise(pred = mean(value), .by = name) %>% 
                         tidytable::mutate(age = as.numeric(name),
                                           name = "Weight (kg)") %>% 
                         tidytable::bind_rows(data.table(age = seq(1, 10),
                                                         name = "Length (cm)",
                                                         pred = Linf * (1 - exp(-k * ((seq(1, 10) + 0.5) - t0)))))) -> grwth_data

grwth_plot <- ggplot(data = grwth_data,
       aes(x = age, y = value, col = factor(year))) +
  geom_pointrange(aes(ymin = value - 1.96 * sd_value, ymax = value + 1.96 * sd_value),
                  position = position_jitter(width = 0.3),
                  linetype = 'dotted') +
  geom_line(aes(y = pred), col = "black", size = 0.777) +
  facet_wrap(~ name, 
             ncol = 1, 
             scales = "free_y") +
  scico::scale_color_scico_d(palette = 'roma') +
  theme_bw(base_size = 14) +
  scale_x_continuous(breaks = c(1:10), limits = c(0.5, 10.5)) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  labs(x = "Age", y = "Pacific cod growth", col = "Year")

ggsave(filename = "grwth_fit.png",
       path = here::here(new_year, "output", "safe_plots"),
       width = 6.5,
       height = 6.5,
       units = "in")



# plot profiles ----
# m profile
load(here::here(new_year, "output", "profile", "m_prof.RData"))
r4ss::SSplotProfile(summ_prof,
                    profile.string = "NatM_uniform_Fem_GP_1",
                    profile.label = "Base M",
                    exact = TRUE,
                    pch = "")


profilevec <- list(rec = seq(12.2, 13.8, by = 0.2),
                   m = seq(0.41, 0.57, by = 0.02),
                   m14 = seq(0.73, 0.89, by = 0.02),
                   q_twl = seq(0.17, 0.33, by = 0.02),
                   q_ll = seq(0.07, 0.23, by = 0.02),
                   q_ll_env = seq(0.84, 1, by = 0.02))


get_profile_data <- function(data, profilevec){
  data %>% 
    tidytable::filter(Label %in% c("TOTAL",
                                   "Survey",
                                   "Length_comp",
                                   "Age_comp",
                                   "Recruitment",
                                   "InitEQ_Regime",
                                   "Parm_priors",
                                   "Parm_devs")) %>% 
    tidytable::pivot_longer(paste0("replist", seq(1, 9))) %>% 
    tidytable::mutate(par_val = case_when(name == "replist1" ~ profilevec[1],
                                          name == "replist2" ~ profilevec[2],
                                          name == "replist3" ~ profilevec[3],
                                          name == "replist4" ~ profilevec[4],
                                          name == "replist5" ~ profilevec[5],
                                          name == "replist6" ~ profilevec[6],
                                          name == "replist7" ~ profilevec[7],
                                          name == "replist8" ~ profilevec[8],
                                          name == "replist9" ~ profilevec[9]),
                      like = case_when(Label == "TOTAL" ~ "Total",
                                       Label == "Survey" ~ "Index data",
                                       Label == "Length_comp" ~ "Length comps",
                                       Label == "Recruitment" ~ "Recruitment devs",
                                       Label == "Age_comp" ~ "CAAL",
                                       Label == "InitEQ_Regime" ~ "Initial equil rec",
                                       Label == "Parm_priors" ~ "Priors",
                                       Label == "Parm_devs" ~ "Parameter devs")) %>% 
    tidytable::select(-name, -Label)
}

# m
load(here::here(new_year, "output", "profile", "m_prof.RData"))
get_profile_data(summ_prof$likelihoods, profilevec$m) %>% 
  tidytable::mutate(par_name = "Base natural mortality") -> m_profile

# r
load(here::here(new_year, "output", "profile", "r_prof.RData"))
get_profile_data(summ_prof$likelihoods, profilevec$r) %>% 
  tidytable::mutate(par_name = "Log(Mean Recruitment)") -> r_profile

# m 14-16
load(here::here(new_year, "output", "profile", "m14_prof.RData"))
get_profile_data(summ_prof$likelihoods, profilevec$m14) %>% 
  tidytable::mutate(par_name = "Natural mortality 2014-2016") -> m14_profile

# q trawl
load(here::here(new_year, "output", "profile", "q_twl_prof.RData"))
get_profile_data(summ_prof$likelihoods, profilevec$q_twl) %>% 
  tidytable::mutate(par_val = exp(par_val)) %>% 
  tidytable::mutate(par_name = "Bottom trawl survey catchability") -> q_twl_profile

# q ll
load(here::here(new_year, "output", "profile", "q_ll_prof.RData"))
get_profile_data(summ_prof$likelihoods, profilevec$q_ll) %>% 
  tidytable::mutate(par_val = exp(par_val)) %>% 
  tidytable::mutate(par_name = "Longline survey catchability") -> q_ll_profile

# q ll env
load(here::here(new_year, "output", "profile", "q_ll_env_prof.RData"))
get_profile_data(summ_prof$likelihoods, profilevec$q_ll_env) %>% 
  tidytable::mutate(par_name = "Longline survey environmental link") -> q_ll_env_profile

m_profile %>% 
  tidytable::bind_rows(r_profile) %>% 
  tidytable::bind_rows(m14_profile) %>% 
  tidytable::bind_rows(q_twl_profile) %>% 
  tidytable::bind_rows(q_ll_profile) %>% 
  tidytable::bind_rows(q_ll_env_profile) %>% 
  tidytable::mutate(min_like = min(value), .by = c(like, par_name)) %>% 
  tidytable::mutate(value = value - min_like,
                    like = factor(like, levels = c("Total", 
                                                   "Index data",
                                                   "Length comps",
                                                   "CAAL", 
                                                   "Recruitment devs",
                                                   "Initial equil rec",
                                                   "Parameter devs",
                                                   "Priors"))) %>% 
  tidytable::select(-min_like) -> profile_data


ggplot(data = profile_data,
       aes(x = par_val, y = value, col = like)) +
  geom_line(size = 0.777) +
  facet_wrap(~par_name, 
             scales = "free",
             ncol = 2, 
             labeller = labeller(par_name = label_wrap_gen(20))) +
  scico::scale_color_scico_d(palette = 'roma') +
  theme_bw(base_size = 14) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "top",
        legend.key.width = unit(0.25, 'cm')) +
  labs(x = "Parameter profile value", y = "Change in negative log-likelihood", col = "")

ggsave(filename = "profile_plot.png",
       path = here::here(new_year, "output", "safe_plots"),
       width = 6.5,
       height = 6.5,
       units = "in")




# r profile
load(here::here(new_year, "output", "profile", "r_prof.RData"))
r4ss::SSplotProfile(summ_prof,
                    profile.string = "SR_LN(R0)",
                    profile.label = "Log(Mean Recruitment)",
                    exact = TRUE,
                    col = scico::scico(9, palette='roma'),
                    pch = "")


# m 14-16 profile
load(here::here(new_year, "output", "profile", "m14_prof.RData"))
r4ss::SSplotProfile(summ_prof,
                    profile.string = "NatM_uniform_Fem_GP_1_BLK4repl_2014",
                    profile.label = "M 2014-2016",
                    exact = TRUE,
                    col = scico::scico(9, palette='roma'),
                    pch = "")

# trawl q profile
load(here::here(new_year, "output", "profile", "q_twl_prof.RData"))
r4ss::SSplotProfile(summ_prof,
                    profile.string = "LnQ_base_Srv(4)",
                    profile.label = "Log(q_twl)",
                    exact = TRUE)














# Define plot function
multiplot <- function(..., plotlist = NULL, cols) {
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # Make the panel
  plotCols = cols                          # Number of columns of plots
  plotRows = ceiling(numPlots/plotCols) # Number of rows needed, calculated from # of cols
  
  # Set up the page
  grid::grid.newpage()
  grid::pushViewport(grid::viewport(layout = grid::grid.layout(plotRows, plotCols)))
  vplayout <- function(x, y)
    grid::viewport(layout.pos.row = x, layout.pos.col = y)
  
  # Make each plot, in the correct location
  for (i in 1:numPlots) {
    curRow = ceiling(i/plotCols)
    curCol = (i-1) %% plotCols + 1
    print(plots[[i]], vp = vplayout(curRow, curCol ))
  }
}


## Model comparisons (for appendix) ----

# read model outputs
#previous accepted model with new data
model_dir_old <- here::here(new_year, "mgmt", Model_name_old)
model_run_old <- r4ss::SS_output(dir = model_dir_old,
                                 verbose = TRUE,
                                 printstats = TRUE)

# recommended model
model_dir_new <- here::here(new_year, "mgmt", Model_name_new)
model_run_new <- r4ss::SS_output(dir = model_dir_new,
                                 verbose = TRUE,
                                 printstats = TRUE)

#previous accepted model
model_dir_prev <- here::here(new_year, "mgmt", Model_name_prev)
model_run_prev <- r4ss::SS_output(dir = model_dir_prev,
                                  verbose = TRUE,
                                  printstats = TRUE)

# plot comparisons
#prev accepted with new data vs recommended model
model_comp <- r4ss::SSsummarize(list(model_run_old, model_run_new))
r4ss::SSplotComparisons(model_comp,
                        legendlabels = c(Model_name_old, Model_name_new),
                        print = TRUE,
                        plotdir = here::here(new_year, "plots", "comp_apndx") )

#prev accepted vs recommended model
model_comp_prev <- r4ss::SSsummarize(list(model_run_prev, model_run_new))
r4ss::SSplotComparisons(model_comp_prev,
                        legendlabels = c(Model_name_prev, Model_name_new),
                        print = TRUE,
                        plotdir = here::here(new_year, "plots", "comp_prev") )



## Plot base model ----
# note: before running this you need to delete the folder, otherwise r4ss will error out

load(here::here(new_year, "output", "model_run.RData"))

r4ss::SS_plots(model_run_new,
               printfolder = "",
               dir = here::here(new_year, "plots", "r4ss"))



# plot ssb and stuff ----
# Get ssb and index fit for spreadsheets with figures
model_run_new$timeseries$Yr %>% 
  bind_cols(model_run_new$timeseries$SpawnBio / 2) %>% 
  rename(year = ...1, ssb = ...2) %>% 
  filter(year >= 1977,
         year <= new_year + 1) -> ssb_pred

model_run_new$timeseries$Yr %>% 
  bind_cols(model_run_new$timeseries$Bio_all) %>% 
  rename(year = ...1, biom = ...2) %>% 
  filter(year >= 1984,
         year <= new_year + 1) -> tot_trwl_pred




# plot profiles ----
# m profile
load(here::here(new_year, "output", "profile", "m_prof.RData"))
r4ss::SSplotProfile(summ_prof,
                    profile.string = "NatM_uniform_Fem_GP_1",
                    profile.label = "Base M",
                    exact = TRUE)


# r profile
load(here::here(new_year, "output", "profile", "r_prof.RData"))
r4ss::SSplotProfile(summ_prof,
                    profile.string = "SR_LN(R0)",
                    profile.label = "Log(Mean Recruitment)",
                    exact = TRUE)


# m 14-16 profile
load(here::here(new_year, "output", "profile", "m14_prof.RData"))
r4ss::SSplotProfile(summ_prof,
                    profile.string = "NatM_uniform_Fem_GP_1_BLK4repl_2014",
                    profile.label = "M 2014-2016",
                    exact = TRUE)

# trawl q profile
load(here::here(new_year, "output", "profile", "q_twl_prof.RData"))
r4ss::SSplotProfile(summ_prof,
                    profile.string = "LnQ_base_Srv(4)",
                    profile.label = "Log(q_twl)",
                    exact = TRUE)


# Plot retrospective analysis ----

load(here::here(new_year, "output", "retroSummary.RData"))

# make plots comparing the retrospective models
endyrvec <- retroSummary[["endyrs"]] + 0:-10

# r4ss::SSplotComparisons(retroSummary,
#                         endyrvec = endyrvec,
#                         legendlabels = paste("Data", 0:-10, "years"),
#                         print = TRUE,
#                         plotdir = here::here("plots", "retro"))

rho_output_ss3diags <- ss3diags::SSplotRetro(retroSummary,
                                             subplots = c("SSB"),
                                             endyrvec = endyrvec,
                                             legendlabels = paste("Data", 0:-10, "years"),
                                             print = TRUE,
                                             plotdir = here::here(new_year, "plots", "other"),
                                             pwidth = 8.5,
                                             pheight = 4.5)

## plot year-class retrospective - put into function at some point
yc_retro <- vroom::vroom(here::here(new_year, 'output', 'yc_retro.csv'))

yc_retro %>% 
  group_by(asmnt_yr) %>% 
  mutate(avg_00_05 = mean(c(yc_2000, yc_2001, yc_2002, yc_2003, yc_2004, yc_2005), na.rm = TRUE),
         avg_06_11 = mean(c(yc_2006, yc_2007, yc_2008, yc_2009, yc_2010, yc_2011), na.rm = TRUE)) %>% 
  select(asmnt_yr, avg_00_05, avg_06_11, yc_2012, yc_2013, yc_2014, yc_2015, yc_2016, yc_2017, yc_2018) %>% 
  pivot_longer(c("avg_00_05", "avg_06_11", paste0("yc_", seq(2012, 2018))), names_to = 'yearclass', values_to = 'Recruitment') -> plot_dat

plot_dat %>% 
  filter(asmnt_yr == 2023) %>%
  mutate(yearclass = case_when(yearclass == 'avg_00_05' ~ '00-05',
                               yearclass == 'avg_06_11' ~ '06-11',
                               yearclass == 'yc_2012' ~ '2012',
                               yearclass == 'yc_2013' ~ '2013',
                               yearclass == 'yc_2014' ~ '2014',
                               yearclass == 'yc_2015' ~ '2015',
                               yearclass == 'yc_2016' ~ '2016',
                               yearclass == 'yc_2017' ~ '2017',
                               yearclass == 'yc_2018' ~ '2018')) -> lab_dat
ggplot(data = plot_dat, 
       aes(x = asmnt_yr, y = Recruitment, color = factor(yearclass))) + 
  geom_point(size = 3) + 
  geom_path(aes(group = yearclass)) +
  scale_color_nmfs("waves", name = "") +
  scale_fill_nmfs("waves", name = "") +
  theme_bw(base_size = 18) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        axis.text.x = element_text(vjust = 0.5, angle = 90)) +
  labs(y = "Recruitment (billions)", x = "Assessment year") +
  facet_wrap(~yearclass, 
             scales = "free_y",
             strip.position = "top") +
  scale_x_continuous(limits = c(2012.5, 2023.5), breaks = seq(2013, 2023, by = 1))

dev.print(png, file = here::here(new_year, "plots", "other", "yc_retro_grid.png"), width = 700, height = 700)
dev.off()

ggplot(data = plot_dat, 
       aes(x = asmnt_yr, y = Recruitment, color = factor(yearclass), fill = yearclass)) + 
  geom_point(size = 3) + 
  geom_path(aes(group = yearclass)) +
  scale_color_nmfs("waves", name = "") +
  scale_fill_nmfs("waves", name = "") +
  theme_bw(base_size = 18) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        axis.text.x = element_text(vjust = 0.5, angle = 90)) +
  labs(y = "Recruitment (billions)", x = "Assessment year") +
  geom_label_repel(data = lab_dat,
                   aes(x = asmnt_yr, y = Recruitment, label = yearclass),
                   color = "black",
                   label.padding = unit(0.3, "lines"),
                   nudge_x = 0.5,
                   nudge_y = 0.1) +
  scale_x_continuous(limits = c(2012.5, 2023.5), breaks = seq(2013, 2023, by = 1))

dev.print(png, file = here::here(new_year, "plots", "other", "yc_retro.png"), width = 700, height = 400)
dev.off()


## Plot phase-plane ----

load(here::here(new_year, "output", "mgmnt_scen.RData"))
load(here::here(new_year, "output", "model_run.RData"))
source(here::here(new_year, "R", "plots", "phase_plane_figure.r"))

Fabc = mscen$Tables$F$scenario_1[16]
Fmsy = mscen$Tables$F$scenario_7[16]
SSB0 = mscen$Two_year$SB100[1]
SSBproj1 = mscen$Two_year$SSB[1]
SSBproj2 = mscen$Two_year$SSB[2]
Fproj1 = mscen$Two_year$F40[1]
Fproj2 = mscen$Two_year$F40[2]
BoverBmsy = model_run_new$timeseries$SpawnBio[3:((new_year - 1977) + 5)] / 2 / (SSB0 * 0.35)  ## SSB w/ 2-year projection
FoverFmsy = model_run_new$sprseries$F_report[1:((new_year - 1977) + 3)] / Fmsy  

plot.phase.plane(SSB0 = SSB0,
                 Fabc = Fabc,
                 Fmsy = Fmsy,
                 BoverBmsy = BoverBmsy, 
                 FoverFmsy = FoverFmsy,
                 xlim = c(0, 5),
                 ylim = c(0, 1.2),
                 header = "Pacific cod 2023 Model 19.1b",
                 eyr = new_year + 2)

dev.print(png, file = here::here(new_year, "plots", "other", "phase_plane.png"), width = 700, height = 700)
dev.off()


## Plot index time series ----

source(here::here(new_year, "R", "plots", "index_figures.r"))

# Get data file name
ss_datname <- list.files(here::here(new_year, "output"), pattern = "GOAPcod")


# Plot indices
index_plots <- plot_indices(styr = 1990, 
                            endyr = new_year, 
                            ss_datname = ss_datname)

index_plots[[1]]
dev.print(png, file = here::here(new_year, "plots", "other", "fitted_indices.png"), width = 700, height = 700)
dev.off()

index_plots[[2]]
dev.print(png, file = here::here(new_year, "plots", "other", "nonfitted_indices.png"), width = 700, height = 700)
dev.off()

index_plots[[3]]
dev.print(png, file = here::here(new_year, "plots", "other", "age0_index.png"), width = 700, height = 400)
dev.off()


## Plot Leave-One-Out analysis results ----

load(here::here(new_year, "output", "loo", "loo_data.RData"))

# Plot parameters from Leave one out
loo_data[[2]]
dev.print(png, file = here::here(new_year, "plots", "other", "LOO.png"), width = 700, height = 700)
dev.off()


load(here::here(new_year, "output", "LOO_add_data.RData"))

# Plot parameters from Leave one out
LOO_add_data[[1]]
dev.print(png, file = here::here(new_year, "plots", "other", "LOO_add_data.png"), width = 700, height = 700)
dev.off()


## Plot Cumulative catch ----

source(here::here(new_year, "R", "plots", "cumulative_catch_plots.r"))

# Get cumulative catch plots
cumul_plots <- plot_cumulative(data_query = data_query,
                               species = "PCOD",
                               fmp_area = "GOA",
                               cyr = new_year)

cumul_plots[[1]]
dev.print(png, file = here::here(new_year, "plots", "other", "cummC_CG.png"), width = 700, height = 400)
dev.off()

cumul_plots[[2]]
dev.print(png, file = here::here(new_year, "plots", "other", "cummC_WG.png"), width = 700, height = 400)
dev.off()



## Plot number of vessels ----

source(here::here(new_year, "R", "plots", "num_vess.r"))

## number of vessels
num_vess <- num_fish_vess(CYR = new_year,
                          data_query = data_query)

num_vess
dev.print(png, file = here::here(new_year, "plots", "other", "num_vess.png"), width = 700, height = 400)
dev.off()


## Plot PCod bycatch in pollock and swf fisheries ----

source(here::here(new_year, "R", "plots", "cod_bycatch_plots.r"))

# Pollock plots
pol_plots <- pollock_bycatch(CYR = new_year,
                             data_query = data_query)

multiplot(pol_plots[[1]], pol_plots[[2]], cols = 1)
dev.print(png, file = here::here(new_year, "plots", "other", "poll_bycatch.png"), width = 700, height = 700)
dev.off()

# SWF plots (BEWARE: this takes a super-duper long time to run)
swf_plot <- swf_bycatch(CYR = new_year, 
                        data_query = data_query)

swf_plot
dev.print(png, file = here::here(new_year, "plots", "other", "swf_bycatch.png"), width = 700, height = 400)
dev.off()



## Plot environmental indices ----

env_data <- vroom::vroom(here::here(new_year, 'data', 'raw_cfsr.csv'))

source(here::here(new_year, "R", "plots", "env_indices.r"))

env_ind <- plot_env_ind(env_data)

multiplot(env_ind[[1]], env_ind[[2]], cols = 1)
dev.print(png, file = here::here(new_year, "plots", "other", "Env_indx.png"), width = 1024, height = 1000)
dev.off()


## Plot MCMC ----

load(here::here(new_year, "output", "mcmc", "mcmc.RData"))
source(here::here(new_year, "R", "plots", "mcmcplots.r"))

mcmc_dir <- here::here(new_year, "mgmt", Model_name_new, "MCMC")

mcmc_plots <- plot_mcmc(mcmc_dir, new_year)

# Save plot
multiplot(mcmc_plots[[1]], mcmc_plots[[2]], cols = 1)
dev.print(png, file = here::here(new_year, "plots", "other", "SSB_Rec.png"), width = 1024, height = 1000)
dev.off()


# plot mcmc for key parameters ----

parameters <- c('NatM_uniform_Fem_GP_1', 
                'NatM_uniform_Fem_GP_1_BLK4repl_2014', 
                'L_at_Amin_Fem_GP_1', 
                'L_at_Amax_Fem_GP_1', 
                'VonBert_K_Fem_GP_1',
                "SR_LN(R0)",
                "LnQ_base_Srv(4)",
                'LnQ_base_LLSrv(5)',
                'LnQ_base_LLSrv(5)_ENV_mult')

mle_value <- model_run_new$estimated_non_dev_parameters$Value[rownames(model_run_new$estimated_non_dev_parameters) %in% parameters]

data.frame(cbind(rownames(model_run_new$estimated_non_dev_parameters)[rownames(model_run_new$estimated_non_dev_parameters) %in% parameters],
                 model_run_new$estimated_non_dev_parameters$Value[rownames(model_run_new$estimated_non_dev_parameters) %in% parameters])) %>% 
  tidytable::rename(parameter = 'X1',
                    mle_value = 'X2') %>% 
  tidytable::mutate(mle_value = as.numeric(mle_value),
                    parameter = case_when(parameter == 'NatM_uniform_Fem_GP_1' ~ 'M_base',
                                          parameter == 'NatM_uniform_Fem_GP_1_BLK4repl_2014' ~ 'M_14_16',
                                          parameter == 'L_at_Amin_Fem_GP_1' ~ 'Lmin',
                                          parameter == 'L_at_Amax_Fem_GP_1' ~ 'Linf',
                                          parameter == 'VonBert_K_Fem_GP_1' ~ 'k',
                                          parameter == 'SR_LN(R0)' ~ 'R0',
                                          parameter == 'LnQ_base_Srv(4)' ~ 'q_BTS',
                                          parameter == 'LnQ_base_LLSrv(5)' ~ 'q_LL',
                                          parameter == 'LnQ_base_LLSrv(5)_ENV_mult' ~ 'q_LL_env'),
                    mle_value = case_when(parameter %in% c('q_BTS', 'q_LL') ~ exp(mle_value),
                                          !(parameter %in% c('q_BTS', 'q_LL')) ~ mle_value)) -> mle

mcmc %>% 
  tidytable::select(NatM_uniform_Fem_GP_1, 
                    NatM_uniform_Fem_GP_1_BLK4repl_2014, 
                    L_at_Amin_Fem_GP_1, 
                    L_at_Amax_Fem_GP_1, 
                    VonBert_K_Fem_GP_1,
                    `SR_LN(R0)`,
                    `LnQ_base_Srv(4)`,
                    `LnQ_base_LLSrv(5)`,
                    `LnQ_base_LLSrv(5)_ENV_mult`) %>% 
  tidytable::rename(M_base = 'NatM_uniform_Fem_GP_1',
                    M_14_16 = 'NatM_uniform_Fem_GP_1_BLK4repl_2014',
                    Lmin = 'L_at_Amin_Fem_GP_1',
                    Linf = 'L_at_Amax_Fem_GP_1',
                    k = 'VonBert_K_Fem_GP_1',
                    R0 = `SR_LN(R0)`,
                    q_BTS = `LnQ_base_Srv(4)`,
                    q_LL = `LnQ_base_LLSrv(5)`,
                    q_LL_env = `LnQ_base_LLSrv(5)_ENV_mult`) %>% 
  tidytable::mutate(q_BTS = exp(q_BTS),
                    q_LL = exp(q_LL)) %>% 
  tidytable::pivot_longer(names_to = 'parameter') %>% 
  tidytable::left_join(mle) -> mcmc_plot_dat

ggplot(mcmc_plot_dat, aes(x = value, y = after_stat(density))) +
  geom_histogram(bins = 15, position = "identity", alpha = 0.5, fill = "gold", color = "red") +
  geom_vline(aes(xintercept = mle_value), color = "black", linewidth = 2) +
  facet_wrap( ~ parameter,
              scales = 'free') +
  theme_bw() +
  theme_bw(base_size = 24) +
  labs(y = "Density", x = "Parameter value")

# Save plot
dev.print(png, file = here::here(new_year, "plots", "other", "param_mcmc.png"), width = 1024, height = 1000)
dev.off()

# plot mcmc adnuts stuff ----

load(here::here(new_year, "output", "mcmc", "mcmc_adnut.RData"))

par = c('MGparm[1]', 'MGparm[3]', 'MGparm[4]', 'MGparm[15]', 'SR_parm[1]', 'Q_parm[1]', 'Q_parm[2]', 'Q_parm[9]')

mcmc_adnut


mcmc_adnut$samples

names(mcmc_adnut)

adnuts::pairs_admb(mcmc_adnut, pars = par)

mcmc_adnut2 <- mcmc_adnut

dimnames(mcmc_adnut2$samples)[[3]][1] <- "M_base"
dimnames(mcmc_adnut2$samples)[[3]][3] <- "Linf"
dimnames(mcmc_adnut2$samples)[[3]][4] <- "k"
dimnames(mcmc_adnut2$samples)[[3]][7] <- "M_14_16"
dimnames(mcmc_adnut2$samples)[[3]][8] <- "R0"
dimnames(mcmc_adnut2$samples)[[3]][68] <- "ln(q_BTS)"
dimnames(mcmc_adnut2$samples)[[3]][69] <- "ln(q_LL)"
dimnames(mcmc_adnut2$samples)[[3]][70] <- "q_LL_env"

par = c('M_base', 'Linf', 'k', 'M_14_16', 'R0', 'ln(q_BTS)', 'ln(q_LL)', 'q_LL_env')

adnuts::pairs_admb(mcmc_adnut2, pars = par, label.cex = 1)
dev.print(png, file = here::here(new_year, "plots", "other", "param_mcmc_nuts.png"), width = 1024, height = 1000)
dev.off()


## plot length sampling rates diags ----

load(here::here(new_year, "output", "lencomp.RData"))

lencomp %>% 
  dplyr::rename_all(tolower) %>% 
  tidytable::mutate(haul1 = as.character(paste(cruise, permit, haul, sep = "_")),
                    area = trunc(area/10)*10,
                    gear = case_when(gear == 0 ~ "TRAWL",
                                     gear == 2 ~ "POT",
                                     gear == 3 ~ "LONGLINE")) %>% 
  tidytable::summarise(len_ss = sum(freq), .by = c(year, gear)) %>%  
  tidytable::left_join(vroom::vroom(here::here(new_year, 'data', 'raw', 'catch.csv')) %>% 
                         dplyr::rename_all(tolower) %>%
                         tidytable::select(year, gear, tons) %>% 
                         tidytable::filter(gear %in% c('HAL', 'POT', 'TRW')) %>% 
                         tidytable::mutate(gear = case_when(gear == 'TRW' ~ "TRAWL",
                                                            gear == 'POT' ~ "POT",
                                                            gear == 'HAL' ~ "LONGLINE")) %>% 
                         tidytable::summarise(catch = sum(tons), .by = c(year, gear))) %>% 
  tidytable::filter(!is.na(catch)) %>% 
  tidytable::mutate(len_tot = sum(len_ss),
                    catch_tot = sum(catch), .by = c(year)) %>% 
  tidytable::mutate(p_len = len_ss / len_tot,
                    p_c = catch / catch_tot) %>% 
  tidytable::select(year, gear, p_len, p_c) %>% 
  tidytable::pivot_longer(cols = c(p_len, p_c), names_to = 'data', values_to = 'proportion') %>% 
  tidytable::mutate(data = case_when(data == 'p_len' ~ "Length frequency",
                                     data == 'p_c' ~ "Catch")) %>% 
  tidytable::filter(year != 2020) -> plot_data


ggplot(data = plot_data, 
       aes(x = year, y = proportion, fill = gear)) + 
  geom_bar(position="fill", stat="identity", width=0.5) + 
  # scale_x_continuous(breaks = c(2010:CYR), limits = c(2009.5, CYR + 0.5)) +
  scale_fill_nmfs("waves", name = "") +
  facet_wrap( ~ data) +
  theme_bw(base_size = 24) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(vjust = 0.5, angle = 90)) +
  labs(y = "Proportion by gear type", x = "Year") +
  coord_flip()

dev.print(png, file = here::here(new_year, "plots", "other", "sampling_rates.png"), width = 1000, height = 1000)
dev.off()

## plot subregion length sampling rates diags ----

load(here::here(new_year, "output", "lencomp.RData"))

lencomp %>% 
  dplyr::rename_all(tolower) %>% 
  tidytable::mutate(haul1 = as.character(paste(cruise, permit, haul, sep = "_")),
                    area = trunc(area/10)*10,
                    gear = case_when(gear == 0 ~ "TRAWL",
                                     gear == 2 ~ "POT",
                                     gear == 3 ~ "LONGLINE")) %>% 
  tidytable::select(year, gear, area, length, freq) %>% 
  tidytable::mutate(subarea = case_when(area %in% c(640, 650) ~ "SE",
                                        area %in% c(620, 630) ~ "CG",
                                        area == 610 ~ "WG")) %>% 
  tidytable::select(year, gear, subarea, length, freq) %>% 
  tidytable::summarise(len_ss = sum(freq), .by = c(year, gear, subarea)) %>% 
  tidytable::filter(year >= 2015) %>% 
  tidytable::left_join(vroom::vroom(here::here(new_year, 'data', 'raw', 'catch.csv')) %>% 
                         dplyr::rename_all(tolower) %>%
                         tidytable::select(year, gear, tons, zone) %>% 
                         tidytable::filter(gear %in% c('HAL', 'POT', 'TRW')) %>% 
                         tidytable::mutate(gear = case_when(gear == 'TRW' ~ "TRAWL",
                                                            gear == 'POT' ~ "POT",
                                                            gear == 'HAL' ~ "LONGLINE"),
                                           subarea = case_when(zone %in% c("SE", "WY", "SEI") ~ "SE",
                                                               zone %in% c("CG", "PWSI") ~ "CG",
                                                               zone == "WG" ~ "WG")) %>% 
                         tidytable::summarise(catch = sum(tons), .by = c(year, gear, subarea)) %>% 
                         tidytable::filter(year >= 2015)) %>% 
  tidytable::mutate(len_tot = sum(len_ss),
                    catch_tot = sum(catch), .by = c(year)) %>% 
  tidytable::mutate(p_len = len_ss / len_tot,
                    p_c = catch / catch_tot) %>% 
  tidytable::select(year, gear, subarea, p_len, p_c) %>% 
  tidytable::pivot_longer(cols = c(p_len, p_c), names_to = 'data', values_to = 'proportion') %>% 
  tidytable::mutate(data = case_when(data == 'p_len' ~ "Length frequency",
                                     data == 'p_c' ~ "Catch")) %>% 
  tidytable::filter(year != 2020,
                    subarea != "SE") -> plot_data

ggplot(data = plot_data, 
       aes(x = year, y = proportion, fill = gear)) + 
  geom_bar(position="fill", stat="identity", width=0.5) + 
  # scale_x_continuous(breaks = c(2010:CYR), limits = c(2009.5, CYR + 0.5)) +
  scale_fill_nmfs("waves", name = "") +
  facet_grid(subarea ~ data) +
  theme_bw(base_size = 24) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(vjust = 0.5, angle = 90)) +
  labs(y = "Proportion by gear type", x = "Year") +
  coord_flip()

dev.print(png, file = here::here(new_year, "plots", "other", "sampling_rates_subarea.png"), width = 1000, height = 1000)
dev.off()





## plot observer sampling effort ----

vroom::vroom(here::here(new_year, 'data', 'raw', 'akfin_raw_obscatch.csv')) %>% 
  dplyr::rename_all(tolower) %>% 
  tidytable::select(year, `gear description`, `extrapolated weight (kg)`)  %>% 
  tidytable::rename(gear = `gear description`,
                    obs_catch = `extrapolated weight (kg)`) %>% 
  tidytable::filter(gear != "JIG") %>% 
  tidytable::mutate(gear = case_when(gear %in% c("PELAGIC", "NON PELAGIC") ~ "Trawl",
                                     gear == "POT OR TRAP" ~ "Pot",
                                     gear == "LONGLINER" ~ "Longline")) %>%
  tidytable::summarise(obs_catch = sum(obs_catch) * 0.001, .by = c(year, gear)) %>% 
  tidytable::filter(!is.na(gear)) %>% 
  tidytable::left_join(vroom::vroom(here::here(new_year, 'data', 'raw', 'catch.csv')) %>% 
                         dplyr::rename_all(tolower) %>%
                         tidytable::select(year, gear, tons) %>% 
                         tidytable::filter(gear %in% c('HAL', 'POT', 'TRW')) %>% 
                         tidytable::mutate(gear = case_when(gear == 'TRW' ~ "Trawl",
                                                            gear == 'POT' ~ "Pot",
                                                            gear == 'HAL' ~ "Longline")) %>% 
                         tidytable::summarise(catch = sum(tons), .by = c(year, gear))) %>% 
  tidytable::left_join(vroom::vroom(here::here(new_year, 'data', 'raw', 'akfin_raw_length.csv')) %>% 
                         dplyr::rename_all(tolower) %>% 
                         tidytable::select(year, `gear description`, `haul join`, frequency)  %>% 
                         tidytable::rename(gear = `gear description`,
                                           haul_join = `haul join`) %>% 
                         tidytable::filter(gear != "JIG") %>% 
                         tidytable::mutate(gear = case_when(gear %in% c("PELAGIC", "NON PELAGIC") ~ "Trawl",
                                                            gear == "POT OR TRAP" ~ "Pot",
                                                            gear == "LONGLINER" ~ "Longline")) %>% 
                         tidytable::summarise(len_ss = sum(frequency), .by = c(year, gear, haul_join)) %>% 
                         tidytable::filter(!is.na(haul_join),
                                           !is.na(gear)) %>% 
                         tidytable::left_join(vroom::vroom(here::here(new_year, 'data', 'raw', 'akfin_raw_obscatch.csv')) %>% 
                                                dplyr::rename_all(tolower) %>% 
                                                tidytable::select(year, `gear description`, `haul join`, `extrapolated weight (kg)`)  %>% 
                                                tidytable::rename(gear = `gear description`,
                                                                  obs_catch = `extrapolated weight (kg)`,
                                                                  haul_join = `haul join`) %>% 
                                                tidytable::filter(gear != "JIG") %>% 
                                                tidytable::mutate(gear = case_when(gear %in% c("PELAGIC", "NON PELAGIC") ~ "Trawl",
                                                                                   gear == "POT OR TRAP" ~ "Pot",
                                                                                   gear == "LONGLINER" ~ "Longline"))) %>% 
                         tidytable::summarise(len_ss = sum(len_ss),
                                              obs_c_len = sum(obs_catch, na.rm = TRUE) * 0.001, .by = c(year, gear))) %>% 
  tidytable::mutate(samp_eff = obs_catch / catch,
                    samp_eff_len = obs_c_len / catch) %>% 
  tidytable::mutate(samp_eff_tot = sum(samp_eff),
                    samp_eff_len_tot = sum(samp_eff_len),
                    catch_tot = sum(catch), .by = c(year)) %>% 
  tidytable::mutate(rel_samp_eff = samp_eff / samp_eff_tot,
                    rel_samp_eff_len = samp_eff_len / samp_eff_len_tot,
                    rel_catch = catch / catch_tot) %>% 
  tidytable::select(year, gear, rel_samp_eff, rel_samp_eff_len, rel_catch) %>% 
  tidytable::pivot_longer(cols = c(rel_samp_eff, rel_samp_eff_len, rel_catch), names_to = 'data', values_to = 'proportion') %>% 
  tidytable::mutate(data = case_when(data == 'rel_samp_eff' ~ "Observed Catch",
                                     data == 'rel_samp_eff_len' ~ "Observed Catch Sampled for Length Frequency",
                                     data == 'rel_catch' ~ "Catch")) %>% 
  tidytable::filter(year != 2020) -> plot_data

ggplot(data = plot_data, 
       aes(x = year, y = proportion, fill = gear)) + 
  geom_bar(position="fill", stat="identity", width=0.5) + 
  # scale_x_continuous(breaks = c(2010:CYR), limits = c(2009.5, CYR + 0.5)) +
  scale_fill_nmfs("waves", name = "") +
  facet_wrap( ~ data,
              labeller = label_wrap_gen(width = 21)) +
  theme_bw(base_size = 24) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(vjust = 0.5, angle = 90)) +
  labs(y = "Relative proportion by gear type", x = "Year") +
  coord_flip()

dev.print(png, file = here::here(new_year, "plots", "other", "sampling_rates_all.png"), width = 1000, height = 1000)
dev.off()




## plot regional observer sampling effort ----

vroom::vroom(here::here(new_year, 'data', 'raw', 'akfin_raw_obscatch.csv')) %>% 
  dplyr::rename_all(tolower) %>% 
  tidytable::select(year, `gear description`, `extrapolated weight (kg)`,`fmp subarea`)  %>% 
  tidytable::rename(gear = `gear description`,
                    obs_catch = `extrapolated weight (kg)`,
                    subregion = `fmp subarea`) %>% 
  tidytable::filter(gear != "JIG",
                    subregion %in% c("CG", "WG")) %>% 
  tidytable::mutate(gear = case_when(gear %in% c("PELAGIC", "NON PELAGIC") ~ "Trawl",
                                     gear == "POT OR TRAP" ~ "Pot",
                                     gear == "LONGLINER" ~ "Longline")) %>%
  tidytable::summarise(obs_catch = sum(obs_catch) * 0.001, .by = c(year, gear, subregion)) %>% 
  tidytable::filter(!is.na(gear)) %>% 
  tidytable::left_join(vroom::vroom(here::here(new_year, 'data', 'raw', 'catch.csv')) %>% 
                         dplyr::rename_all(tolower) %>%
                         tidytable::select(year, gear, tons, zone) %>% 
                         tidytable::rename(subregion = zone) %>% 
                         tidytable::filter(gear %in% c('HAL', 'POT', 'TRW'),
                                           subregion %in% c("CG", "WG")) %>% 
                         tidytable::mutate(gear = case_when(gear == 'TRW' ~ "Trawl",
                                                            gear == 'POT' ~ "Pot",
                                                            gear == 'HAL' ~ "Longline")) %>% 
                         tidytable::summarise(catch = sum(tons), .by = c(year, gear, subregion))) %>% 
  tidytable::left_join(vroom::vroom(here::here(new_year, 'data', 'raw', 'akfin_raw_length.csv')) %>% 
                         dplyr::rename_all(tolower) %>% 
                         tidytable::select(year, `gear description`, `haul join`, frequency, `fmp subarea`)  %>% 
                         tidytable::rename(gear = `gear description`,
                                           haul_join = `haul join`,
                                           subregion = `fmp subarea`) %>% 
                         tidytable::filter(gear != "JIG",
                                           subregion %in% c("CG", "WG")) %>% 
                         tidytable::mutate(gear = case_when(gear %in% c("PELAGIC", "NON PELAGIC") ~ "Trawl",
                                                            gear == "POT OR TRAP" ~ "Pot",
                                                            gear == "LONGLINER" ~ "Longline")) %>% 
                         tidytable::summarise(len_ss = sum(frequency), .by = c(year, gear, haul_join, subregion)) %>% 
                         tidytable::filter(!is.na(haul_join),
                                           !is.na(gear)) %>% 
                         tidytable::left_join(vroom::vroom(here::here(new_year, 'data', 'raw', 'akfin_raw_obscatch.csv')) %>% 
                                                dplyr::rename_all(tolower) %>% 
                                                tidytable::select(year, `gear description`, `haul join`, `extrapolated weight (kg)`,`fmp subarea`)  %>% 
                                                tidytable::rename(gear = `gear description`,
                                                                  obs_catch = `extrapolated weight (kg)`,
                                                                  haul_join = `haul join`,
                                                                  subregion = `fmp subarea`) %>% 
                                                tidytable::filter(gear != "JIG",
                                                                  subregion %in% c("CG", "WG")) %>% 
                                                tidytable::mutate(gear = case_when(gear %in% c("PELAGIC", "NON PELAGIC") ~ "Trawl",
                                                                                   gear == "POT OR TRAP" ~ "Pot",
                                                                                   gear == "LONGLINER" ~ "Longline"))) %>% 
                         tidytable::summarise(len_ss = sum(len_ss),
                                              obs_c_len = sum(obs_catch, na.rm = TRUE) * 0.001, .by = c(year, gear, subregion))) %>% 
  tidytable::mutate(samp_eff = obs_catch / catch,
                    samp_eff_len = obs_c_len / catch) %>% 
  tidytable::mutate(samp_eff_tot = sum(samp_eff),
                    samp_eff_len_tot = sum(samp_eff_len),
                    catch_tot = sum(catch), .by = c(year, subregion)) %>% 
  tidytable::mutate(rel_samp_eff = samp_eff / samp_eff_tot,
                    rel_samp_eff_len = samp_eff_len / samp_eff_len_tot,
                    rel_catch = catch / catch_tot) %>% 
  tidytable::select(year, gear, subregion, rel_samp_eff, rel_samp_eff_len, rel_catch) %>% 
  tidytable::pivot_longer(cols = c(rel_samp_eff, rel_samp_eff_len, rel_catch), names_to = 'data', values_to = 'proportion') %>% 
  tidytable::mutate(data = case_when(data == 'rel_samp_eff' ~ "Observed Catch",
                                     data == 'rel_samp_eff_len' ~ "Observed Catch Sampled for Length Frequency",
                                     data == 'rel_catch' ~ "Catch")) %>% 
  tidytable::filter(year != 2020,
                    year >= 2015) -> plot_data

ggplot(data = plot_data, 
       aes(x = year, y = proportion, fill = gear)) + 
  geom_bar(position="fill", stat="identity", width=0.5) + 
  # scale_x_continuous(breaks = c(2010:CYR), limits = c(2009.5, CYR + 0.5)) +
  scale_fill_nmfs("waves", name = "") +
  facet_grid(subregion ~ data,
              labeller = label_wrap_gen(width = 21)) +
  theme_bw(base_size = 24) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(vjust = 0.5, angle = 90)) +
  labs(y = "Relative proportion by gear type", x = "Year") +
  coord_flip()

dev.print(png, file = here::here(new_year, "plots", "other", "sampling_rates_subregion.png"), width = 1000, height = 1000)
dev.off()



# plot length-weight relationship ----

vroom::vroom(here::here(new_year, 'data', 'raw', 'afsc_meanage.csv')) %>% 
  dplyr::rename_all(tolower) %>% 
  tidytable::select(year, length, weight) %>% 
  tidytable::filter(!is.na(weight)) %>% 
  tidytable::filter(!is.na(length)) %>% 
  tidytable::mutate(length = length / 10,
                    weight = weight / 1000) -> lw_dat


lw_fit <- function(par, data){
  sum((data$weight - par[1] * data$length ^ par[2]) ^ 2)
}


direct_fit <- optim(c(1E-05, 3), lw_fit, data = lw_dat)

lw_fit(c(3.37845E-06, 3.2804959), lw_dat)

lin_fit <- lm(log(weight) ~ log(length), data = lw_dat)

lw_dat %>% 
  tidytable::filter(year <= 2015) %>% 
  tidytable::summarise(weight = mean(weight), .by = c(length)) %>% 
  tidytable::mutate(range = "pre-2015") %>% 
  tidytable::bind_rows(lw_dat %>% 
                         tidytable::filter(year > 2015) %>% 
                         tidytable::summarise(weight = mean(weight), .by = c(length)) %>% 
                         tidytable::mutate(range = "post-2015")) -> plot_dat_mu


ggplot(data = plot_dat_mu, 
       aes(x = length, y = weight, color = range)) + 
  geom_point(size = 2) +
  theme_bw(base_size = 24) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

dev.print(png, file = here::here(new_year, "plots", "other", "length_weight.png"), width = 700, height = 500)
dev.off()






ggplot(data = plot_dat, 
       aes(x = length, y = weight, color = range)) + 
  geom_point() +
  geom_line(aes(x = length, y = pred_old), color = "red", linewidth = 1.5) + 
  geom_line(aes(x = length, y = pred_new), color = "navyblue", linewidth = 1.5)
  
ggplot(data = plot_dat, 
       aes(x = log(length), y = log(weight), color = year)) + 
  geom_point() +
  geom_line(aes(x = log(length), y = log(5.63096e-06) + 3.1306 * log(length)), color = "red") + 
  geom_line(aes(x = log(length), y = fit$coefficients[1] + fit$coefficients[2] * log(length)))

exp(fit$coefficients[1])


# plot osa residuals ----

remotes::install_github("fishfollower/compResidual/compResidual", force = TRUE)
library(compResidual)

model_run_new <- r4ss::SS_output(dir = model_dir_new,
                                 verbose = TRUE,
                                 printstats = TRUE)



# plot ll survey sensitivities ----


dat <- r4ss::SS_readdat_3.30(here::here(new_year, "output", "GOAPcod2023Oct16.dat"), verbose = TRUE)

dat$CPUE %>% 
  tidytable::filter(index == 5) %>% 
  tidytable::mutate(uci_obs = obs + 1.96 * se_log * obs,
                    lci_obs = obs - 1.96 * se_log * obs) %>% 
  tidytable::select(year, obs, uci, lci) -> ll_obs


vroom::vroom(here::here(new_year, 'output', 'llq_cov_pred_ll.csv')) %>% 
  tidytable::filter(model %in% c('base', 'no_cov')) %>% 
  tidytable::pivot_wider(names_from = model, values_from = values) %>% 
  tidytable::left_join(vroom::vroom(here::here(new_year, 'output', 'llq_cov_pred_ll.csv')) %>% 
                         tidytable::filter(!(model %in% c('base', 'no_cov'))) %>% 
                         tidytable::summarise(mean_rand = mean(values),
                                              uci_rand = max(values),
                                              lci_rand = min(values), .by = year)) %>% 
  tidytable::left_join(dat$CPUE %>% 
                         tidytable::filter(index == 5) %>% 
                         tidytable::mutate(uci_obs = obs + 1.96 * se_log * obs,
                                           lci_obs = obs - 1.96 * se_log * obs) %>% 
                         tidytable::select(year, obs, uci_obs, lci_obs)) %>% 
  tidytable::left_join(vroom::vroom(here::here(new_year, 'output', 'llq_cov_pred_ll.csv')) %>% 
                         tidytable::filter(model %in% c('rand37', 'rand21')) %>% 
                         tidytable::pivot_wider(names_from = model, values_from = values)) -> plot_dat


colors <- c("No covariate" = "orange", "2019.1b" = "blue", "Random" = "darkgrey")


ggplot(plot_dat, aes(x = year, y = obs, ymin = lci_obs, ymax = uci_obs)) +
  geom_ribbon(aes(ymin = lci_rand, ymax = uci_rand), fill = "grey", alpha = 0.2) +
  geom_line(aes(x = year, y = rand21, color = "Random"), linewidth = 1) +
  geom_line(aes(x = year, y = rand37, color = "Random"), linewidth = 1) + 
  geom_point() +
  geom_linerange() +
  geom_line(aes(x = year, y = no_cov, color = "No covariate"), linewidth = 1) + 
  geom_line(aes(x = year, y = base, color = "2019.1b"), linewidth = 1) +
  theme_bw(base_size = 24) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "right") +
  labs(x = "Year",
       y = "LL Survey RPN",
       color = "Catchability") +
  scale_color_manual(values = colors)

dev.print(png, file = here::here(new_year, "plots", "other", "ll_srv_fits.png"), width = 1000, height = 500)


# plot apportionment ----

load(file = here::here(new_year, 'output', 'rema_output.rdata'))

cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

apport_out$biomass_by_strata %>% 
  tidytable::select(strata, year, pred, pred_lci, pred_uci, obs, obs_cv) %>% 
  tidytable::mutate(Region = case_when(strata == 'CENTRAL GOA' ~ "CGOA",
                                       strata == 'WESTERN GOA' ~ "WGOA",
                                       strata == 'EASTERN GOA' ~ "EGOA"),
                    Region = factor(Region, levels = c("WGOA", "CGOA", "EGOA")),
                    species = "Pacific cod") -> plot_dat1

apport_out$proportion_biomass_by_strata %>% 
  tidytable::pivot_longer(cols = c(`CENTRAL GOA`, `EASTERN GOA`, `WESTERN GOA`)) %>% 
  tidytable::rename(strata = "name",
                    Apportionment = "value") %>% 
  tidytable::select(year, strata, Apportionment) %>% 
  tidytable::mutate(Region = case_when(strata == 'CENTRAL GOA' ~ "CGOA",
                                       strata == 'WESTERN GOA' ~ "WGOA",
                                       strata == 'EASTERN GOA' ~ "EGOA"),
                    Region = factor(Region, levels = c("WGOA", "CGOA", "EGOA")),
                    species = "Pacific cod") -> plot_dat2


ggplot(plot_dat1, aes(x = year, y = pred)) +
  scale_fill_manual(values = cbp1) +
  scale_color_manual(values = cbp1) +
  geom_line(aes(color = Region), linewidth = 1) +
  geom_ribbon(aes(ymin = pred_lci, ymax = pred_uci, fill = Region), alpha = 0.2) +
  geom_point(aes(x = year, y = obs, color = Region)) +
  geom_linerange(aes(ymin = obs - 1.96 * obs_cv * obs, ymax = obs + 1.96 * obs_cv * obs, color = Region)) +
  theme_bw(base_size = 21) +
  theme(axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank()) +
  labs(y = "Biomass (t)") -> p1


ggplot(data = plot_dat2, 
       aes(x = year, y = Apportionment, fill = Region)) + 
  geom_bar(position="fill", stat="identity", width=0.5) +
  scale_fill_manual(values = cbp1) +
  theme_bw(base_size = 21) +
  labs(x = "Year") -> p2


ggarrange(p1, p2,
          ncol = 1, nrow = 2)

dev.print(png, file = here::here(new_year, "plots", "other", "new_apportionment.png"), width = 1000, height = 1000)
