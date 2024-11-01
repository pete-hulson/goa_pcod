## Script to run 2023 GOA Pacific Cod Assessment plot code

# Load required packages & define parameters ----

## cran packages ----
pkg_cran <- c("data.table",
              "tidyverse",
              "vroom",
              "here",
              "adnuts")


# if not installed, then install
if(length(pkg_cran[which(pkg_cran %in% rownames(installed.packages()) == FALSE )]) > 0) {
  install.packages(pkg_cran[which(pkg_cran %in% rownames(installed.packages()) == FALSE)])
}

## github packages ----
pkg_git <- c("afscOSA",
             "compResidual",
             "r4ss")

# if not installed, then install
if(!isTRUE("afscOSA" %in% rownames(installed.packages()))){
  devtools::install_github("noaa-afsc/afscOSA", dependencies = TRUE, force = TRUE)
}
if(!isTRUE("compResidual" %in% rownames(installed.packages()))){
  TMB:::install.contrib("https://github.com/vtrijoulet/OSA_multivariate_dists/archive/main.zip")
  devtools::install_github("fishfollower/compResidual/compResidual", force = TRUE)
}
if(!isTRUE("r4ss" %in% rownames(installed.packages()))) {
  devtools::install_github("r4ss/r4ss", force = TRUE)
}

# set up ----
# recommended model name
rec_mdl <- "24.0"
# last year's model with updated data (base model)
base_mdl <- "19.1b"
# last year's model
prev_mdl <- "2019.1b-2023"
# Current assessment year
new_year <- as.numeric(format(Sys.Date(), format = "%Y"))

# read in necessary info ----

## model output ----
rec_mdl_res <- r4ss::SS_output(dir = here::here(new_year, "mgmt", rec_mdl),
                               verbose = FALSE,
                               printstats = FALSE)
base_mdl_res <- r4ss::SS_output(dir = here::here(new_year, "mgmt", base_mdl),
                                verbose = FALSE,
                                printstats = FALSE)
prev_mdl_res <- r4ss::SS_output(dir = here::here(new_year - 1, "mgmt", prev_mdl),
                                verbose = FALSE,
                                printstats = FALSE)
res_19_1c <- r4ss::SS_output(dir = here::here(new_year, "mgmt", "19.1c"),
                             verbose = FALSE,
                             printstats = FALSE)
res_19_1d <- r4ss::SS_output(dir = here::here(new_year, "mgmt", "19.1d"),
                             verbose = FALSE,
                             printstats = FALSE)
res_19_1e <- r4ss::SS_output(dir = here::here(new_year, "mgmt", "19.1e"),
                             verbose = FALSE,
                             printstats = FALSE)
## catch data ----
fed_raw <- vroom::vroom(here::here(new_year, "data", "raw", "fish_catch_data.csv"), 
                        progress = FALSE, 
                        show_col_types = FALSE)
## pelagic twl catch data ----
pel_twl <- vroom::vroom(here::here(new_year, "data", "raw", "pel_twl.csv"), 
                        progress = FALSE, 
                        show_col_types = FALSE)

## swf catch data ----
swf <- vroom::vroom(here::here(new_year, "data", "raw", "swf_catch.csv"), 
                    progress = FALSE, 
                    show_col_types = FALSE)
## env data ----
env_data <- vroom::vroom(here::here(new_year, 'data', 'raw_cfsr.csv'), 
                         progress = FALSE, 
                         show_col_types = FALSE)
## survey specimen data ----
age_data <- vroom::vroom(here::here(new_year, 'data', 'raw', 'twl_srvy_age.csv'), 
                         progress = FALSE, 
                         show_col_types = FALSE)
## data for temp plot ----
catch <- vroom::vroom(here::here(new_year, 'data', 'raw', 'twl_srvy_catch.csv'), delim = ',', 
                      progress = FALSE, 
                      show_col_types = FALSE) %>% 
  tidytable::select(-lat, -lon, -survey, -species_code)
haul <- vroom::vroom(here::here(new_year, 'data', 'raw', 'twl_srvy_haul.csv'), delim = ',', 
                     progress = FALSE, 
                     show_col_types = FALSE) %>% 
  tidytable::select(-lat, -lon, -survey, -stemp)
lfreq <- vroom::vroom(here::here(new_year, 'data', 'raw', 'twl_srvy_lenfreq.csv'), delim = ',', 
                      progress = FALSE, 
                      show_col_types = FALSE) %>% 
  tidytable::select(-lat, -lon, - sex, -survey, -species_code, -stemp) %>% 
  tidytable::summarise(freq = sum(frequency), .by = c(year, stratum, hauljoin, temp, depth, length))
strata <- vroom::vroom(here::here(new_year, 'data', 'raw', 'twl_srvy_strata.csv'), delim = ',', 
                       progress = FALSE, 
                       show_col_types = FALSE) %>% 
  tidytable::mutate(Subarea = case_when(subarea_name == 'WESTERN GOA - INPFC' ~ 'Western',
                                        subarea_name == 'CENTRAL GOA - INPFC' ~ 'Central',
                                        subarea_name == 'EASTERN GOA - INPFC' ~ 'Eastern',
                                        subarea_name == 'WESTERN GOA - NMFS' ~ 'Western',
                                        subarea_name == 'CENTRAL GOA - NMFS' ~ 'Central',
                                        subarea_name == 'EASTERN GOA - NMFS' ~ 'Eastern')) %>% 
  tidytable::select(-area_id, -design_year, -subarea_name)
## ageing bias data ----
vroom::vroom(here::here(new_year, 'data', 'raw', 'twl_srvy_age_bias.csv'), delim = ',', 
             progress = FALSE, 
             show_col_types = FALSE) %>% 
  tidytable::select(vessel, haul, specimen, sex, length, original_age) %>% 
  tidytable::full_join(vroom::vroom(here::here(new_year, 'data', 'ageing_error', 'reread_2017.csv'), delim = ',', 
                                    progress = FALSE, 
                                    show_col_types = FALSE) %>% 
                         tidytable::select(vessel, haul, specimen, sex, length, age) %>% 
                         tidytable::mutate(length = length / 10) %>% 
                         tidytable::rename(reread_age = age)) %>% 
  tidytable::filter(!is.na(original_age),
                    !is.na(reread_age)) %>% 
  tidytable::summarise(num_reread = .N,
                       .by = c(reread_age, original_age)) %>% 
  tidytable::arrange(original_age) -> reread

bias_res <- vroom::vroom(here::here(new_year, 'output', 'ageing_error', 'agebias_res', 'Pcod SS3_format_Reader1.csv'), delim = ',', 
                         progress = FALSE, 
                         show_col_types = FALSE)
## ageing error data ----
vroom::vroom(here::here(new_year, 'data', 'ageing_error', 'reader_tester.csv'), delim = ',',
             progress = FALSE, 
             show_col_types = FALSE) %>% 
  dplyr::rename_all(tolower) %>% 
  tidytable::filter(species == 202,
                    region != 'AI') %>% 
  tidytable::mutate(year = as.numeric(substr(date_read, start = nchar(date_read) - 3, stop = nchar(date_read)))) %>% 
  tidytable::select(region, year, read_age, test_age) %>% 
  filter(year >= 2000) %>%  
  tidytable::summarise(n = .N, .by = c(read_age, test_age)) -> read_test

ae_res <- vroom::vroom(here::here(new_year, 'output', 'ageing_error', 'agerr_res', 'Pcod SS3_format_Reader1.csv'), delim = ',', 
                         progress = FALSE, 
                         show_col_types = FALSE)
## ll survey q sensitivities ----
load(here::here(new_year, "output", "llq", "llq_res.RData"))
## retrospective results ----
load(here::here(new_year, "output", "retro", "retrosumm.RData"))
## historical models ----
hist_mdls <- vroom::vroom(here::here(new_year, 'data', 'hist_mdls.csv'), 
                          progress = FALSE, 
                          show_col_types = FALSE)
## leave-one-out results ----
load(here::here(new_year, "output", "loo", "loo.RData"))
## add-one-in results ----
load(here::here(new_year, "output", "aoi", "aoi.RData"))
## management scenarios ----
load(here::here(new_year, "output", "mscen", "mgmnt_scen_rec.RData"))
## mcmc ----
load(here::here(new_year, "output", "mcmc", "mcmc_adnut.RData"))
load(here::here(new_year, "output", "mcmc", "mcmc_eval.RData"))
## apportionment ----
load(file = here::here(new_year, 'output', "apport", 'apport.rdata'))





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


# ssb and rec comparison ----
prev_mdl_res$timeseries %>% 
  tidytable::mutate(SpawnBio = SpawnBio / 2) %>% 
  tidytable::select(year = Yr, "19.1b-23" = SpawnBio) %>% 
  tidytable::filter(year >= 1977,
                    year <= new_year - 1) %>% 
  tidytable::right_join(base_mdl_res$timeseries %>% 
                          tidytable::mutate(SpawnBio = SpawnBio / 2) %>% 
                          tidytable::select(year = Yr, "19.1b" = SpawnBio) %>% 
                          tidytable::filter(year >= 1977,
                                            year <= new_year)) %>% 
  tidytable::right_join(res_19_1c$timeseries %>% 
                          tidytable::mutate(SpawnBio = SpawnBio / 2) %>% 
                          tidytable::select(year = Yr, "19.1c" = SpawnBio) %>% 
                          tidytable::filter(year >= 1977,
                                            year <= new_year)) %>% 
  tidytable::right_join(res_19_1d$timeseries %>% 
                          tidytable::mutate(SpawnBio = SpawnBio / 2) %>% 
                          tidytable::select(year = Yr, "19.1d" = SpawnBio) %>% 
                          tidytable::filter(year >= 1977,
                                            year <= new_year)) %>% 
  tidytable::right_join(res_19_1e$timeseries %>% 
                          tidytable::mutate(SpawnBio = SpawnBio / 2) %>% 
                          tidytable::select(year = Yr, "19.1e" = SpawnBio) %>% 
                          tidytable::filter(year >= 1977,
                                            year <= new_year)) %>% 
  tidytable::right_join(rec_mdl_res$timeseries %>% 
                          tidytable::mutate(SpawnBio = SpawnBio / 2) %>% 
                          tidytable::select(year = Yr, "24.0" = SpawnBio) %>% 
                          tidytable::filter(year >= 1977,
                                            year <= new_year)) %>% 
  tidytable::pivot_longer(cols = c("19.1b-23",
                                   "19.1b", 
                                   "19.1c",
                                   "19.1d",
                                   "19.1e", 
                                   "24.0")) %>% 
  tidytable::mutate(type = "Spawning biomass (t)") %>% 
  tidytable::bind_rows(prev_mdl_res$timeseries %>% 
                         tidytable::select(year = Yr, "19.1b-23" = Recruit_0) %>% 
                         tidytable::filter(year >= 1977,
                                           year <= new_year - 1) %>% 
                         tidytable::right_join(base_mdl_res$timeseries %>% 
                                                 tidytable::select(year = Yr, "19.1b" = Recruit_0) %>% 
                                                 tidytable::filter(year >= 1977,
                                                                   year <= new_year)) %>% 
                         tidytable::right_join(res_19_1c$timeseries %>% 
                                                 tidytable::select(year = Yr, "19.1c" = Recruit_0) %>% 
                                                 tidytable::filter(year >= 1977,
                                                                   year <= new_year)) %>% 
                         tidytable::right_join(res_19_1d$timeseries %>% 
                                                 tidytable::select(year = Yr, "19.1d" = Recruit_0) %>% 
                                                 tidytable::filter(year >= 1977,
                                                                   year <= new_year)) %>% 
                         tidytable::right_join(res_19_1e$timeseries %>% 
                                                 tidytable::select(year = Yr, "19.1e" = Recruit_0) %>% 
                                                 tidytable::filter(year >= 1977,
                                                                   year <= new_year)) %>% 
                         tidytable::right_join(rec_mdl_res$timeseries %>% 
                                                 tidytable::select(year = Yr, "24.0" = Recruit_0) %>% 
                                                 tidytable::filter(year >= 1977,
                                                                   year <= new_year)) %>% 
                         tidytable::pivot_longer(cols = c("19.1b-23",
                                                          "19.1b", 
                                                          "19.1c",
                                                          "19.1d",
                                                          "19.1e", 
                                                          "24.0")) %>% 
                         tidytable::mutate(type = "Age-0 recruitment (1000s)")) %>% 
  tidytable::mutate(name = factor(name, levels = c("19.1b-23",
                                                   "19.1b", 
                                                   "19.1c",
                                                   "19.1d",
                                                   "19.1e", 
                                                   "24.0"))) -> ssb_rec_comp




ssb_rec_comp_plot <- ggplot(data = ssb_rec_comp,
       aes(x = year, y = value, col = name)) +
  geom_point() +
  geom_line() +
  facet_wrap(~type, scales = "free_y", ncol = 1) +
  theme_bw(base_size = 14) +
  scico::scale_color_scico_d(palette = 'roma') +
  labs(x = "Year", y = "Model estimate", color = "Model") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "top")

ggsave(filename = "ssb_rec_comp.png",
       path = here::here(new_year, "output", "safe_plots"),
       width = 6.5,
       height = 7,
       units = "in")


# key parameters comparison ----

prev_mdl_res$parameters %>% 
  tidytable::select(par = Label, value = Value, sd = Parm_StDev) %>% 
  tidytable::filter(par %in% c("NatM_uniform_Fem_GP_1",
                               "L_at_Amax_Fem_GP_1",
                               "NatM_uniform_Fem_GP_1_BLK4repl_2014",
                               "SR_LN(R0)",
                               "LnQ_base_Srv(4)",
                               "LnQ_base_LLSrv(5)")) %>% 
  tidytable::mutate(par = case_when(par == "NatM_uniform_Fem_GP_1" ~ "M",
                                    par == "L_at_Amax_Fem_GP_1" ~ "Linf",
                                    par == "NatM_uniform_Fem_GP_1_BLK4repl_2014" ~ "M (2014-2016)",
                                    par == "SR_LN(R0)" ~ "R0",
                                    par == "LnQ_base_Srv(4)" ~ "q (Trawl survey)",
                                    par == "LnQ_base_LLSrv(5)" ~ "q (Longline survey)"),
                    value = case_when(par %in% c("q (Trawl survey)", "q (Longline survey)") ~ exp(value),
                                      .default = value),
                    model = "19.1b-23") %>% 
  tidytable::bind_rows(base_mdl_res$parameters %>% 
                         tidytable::select(par = Label, value = Value, sd = Parm_StDev) %>% 
                         tidytable::filter(par %in% c("NatM_uniform_Fem_GP_1",
                                                      "L_at_Amax_Fem_GP_1",
                                                      "NatM_uniform_Fem_GP_1_BLK4repl_2014",
                                                      "SR_LN(R0)",
                                                      "LnQ_base_Srv(4)",
                                                      "LnQ_base_LLSrv(5)")) %>% 
                         tidytable::mutate(par = case_when(par == "NatM_uniform_Fem_GP_1" ~ "M",
                                                           par == "L_at_Amax_Fem_GP_1" ~ "Linf",
                                                           par == "NatM_uniform_Fem_GP_1_BLK4repl_2014" ~ "M (2014-2016)",
                                                           par == "SR_LN(R0)" ~ "R0",
                                                           par == "LnQ_base_Srv(4)" ~ "q (Trawl survey)",
                                                           par == "LnQ_base_LLSrv(5)" ~ "q (Longline survey)"),
                                           value = case_when(par %in% c("q (Trawl survey)", "q (Longline survey)") ~ exp(value),
                                                             .default = value),
                                           model = "19.1b")) %>% 
  tidytable::bind_rows(res_19_1c$parameters %>% 
                         tidytable::select(par = Label, value = Value, sd = Parm_StDev) %>% 
                         tidytable::filter(par %in% c("NatM_uniform_Fem_GP_1",
                                                      "L_at_Amax_Fem_GP_1",
                                                      "NatM_uniform_Fem_GP_1_BLK4repl_2014",
                                                      "SR_LN(R0)",
                                                      "LnQ_base_Srv(4)",
                                                      "LnQ_base_LLSrv(5)")) %>% 
                         tidytable::mutate(par = case_when(par == "NatM_uniform_Fem_GP_1" ~ "M",
                                                           par == "L_at_Amax_Fem_GP_1" ~ "Linf",
                                                           par == "NatM_uniform_Fem_GP_1_BLK4repl_2014" ~ "M (2014-2016)",
                                                           par == "SR_LN(R0)" ~ "R0",
                                                           par == "LnQ_base_Srv(4)" ~ "q (Trawl survey)",
                                                           par == "LnQ_base_LLSrv(5)" ~ "q (Longline survey)"),
                                           value = case_when(par %in% c("q (Trawl survey)", "q (Longline survey)") ~ exp(value),
                                                             .default = value),
                                           model = "19.1c")) %>% 
  tidytable::bind_rows(res_19_1d$parameters %>% 
                         tidytable::select(par = Label, value = Value, sd = Parm_StDev) %>% 
                         tidytable::filter(par %in% c("NatM_uniform_Fem_GP_1",
                                                      "L_at_Amax_Fem_GP_1",
                                                      "NatM_uniform_Fem_GP_1_BLK4repl_2014",
                                                      "SR_LN(R0)",
                                                      "LnQ_base_Srv(4)",
                                                      "LnQ_base_LLSrv(5)")) %>% 
                         tidytable::mutate(par = case_when(par == "NatM_uniform_Fem_GP_1" ~ "M",
                                                           par == "L_at_Amax_Fem_GP_1" ~ "Linf",
                                                           par == "NatM_uniform_Fem_GP_1_BLK4repl_2014" ~ "M (2014-2016)",
                                                           par == "SR_LN(R0)" ~ "R0",
                                                           par == "LnQ_base_Srv(4)" ~ "q (Trawl survey)",
                                                           par == "LnQ_base_LLSrv(5)" ~ "q (Longline survey)"),
                                           value = case_when(par %in% c("q (Trawl survey)", "q (Longline survey)") ~ exp(value),
                                                             .default = value),
                                           model = "19.1d")) %>% 
  tidytable::bind_rows(res_19_1e$parameters %>% 
                         tidytable::select(par = Label, value = Value, sd = Parm_StDev) %>% 
                         tidytable::filter(par %in% c("NatM_uniform_Fem_GP_1",
                                                      "L_at_Amax_Fem_GP_1",
                                                      "NatM_uniform_Fem_GP_1_BLK4repl_2014",
                                                      "SR_LN(R0)",
                                                      "LnQ_base_Srv(4)",
                                                      "LnQ_base_LLSrv(5)")) %>% 
                         tidytable::mutate(par = case_when(par == "NatM_uniform_Fem_GP_1" ~ "M",
                                                           par == "L_at_Amax_Fem_GP_1" ~ "Linf",
                                                           par == "NatM_uniform_Fem_GP_1_BLK4repl_2014" ~ "M (2014-2016)",
                                                           par == "SR_LN(R0)" ~ "R0",
                                                           par == "LnQ_base_Srv(4)" ~ "q (Trawl survey)",
                                                           par == "LnQ_base_LLSrv(5)" ~ "q (Longline survey)"),
                                           value = case_when(par %in% c("q (Trawl survey)", "q (Longline survey)") ~ exp(value),
                                                             .default = value),
                                           model = "19.1e")) %>% 
  tidytable::bind_rows(rec_mdl_res$parameters %>% 
                         tidytable::select(par = Label, value = Value, sd = Parm_StDev) %>% 
                         tidytable::filter(par %in% c("NatM_uniform_Fem_GP_1",
                                                      "L_at_Amax_Fem_GP_1",
                                                      "NatM_uniform_Fem_GP_1_BLK4repl_2014",
                                                      "SR_LN(R0)",
                                                      "LnQ_base_Srv(4)",
                                                      "LnQ_base_LLSrv(5)")) %>% 
                         tidytable::mutate(par = case_when(par == "NatM_uniform_Fem_GP_1" ~ "M",
                                                           par == "L_at_Amax_Fem_GP_1" ~ "Linf",
                                                           par == "NatM_uniform_Fem_GP_1_BLK4repl_2014" ~ "M (2014-2016)",
                                                           par == "SR_LN(R0)" ~ "R0",
                                                           par == "LnQ_base_Srv(4)" ~ "q (Trawl survey)",
                                                           par == "LnQ_base_LLSrv(5)" ~ "q (Longline survey)"),
                                           value = case_when(par %in% c("q (Trawl survey)", "q (Longline survey)") ~ exp(value),
                                                             .default = value),
                                           model = "24.0")) %>% 
  tidytable::mutate(model = factor(model, levels = c("19.1b-23",
                                                   "19.1b", 
                                                   "19.1c",
                                                   "19.1d",
                                                   "19.1e", 
                                                   "24.0"))) -> par_dat

par_comp <- ggplot(data = par_dat, 
       aes(x = model, y = value, col = model)) +
  geom_point() +
  geom_errorbar(aes(ymin = value - 1.96 * sd, ymax = value + 1.96 * sd), width = 0) +
  facet_wrap(~par, scales = "free_y", ncol = 2, axis.labels = "all_y")+
  theme_bw(base_size = 14)+
  scico::scale_color_scico_d(palette = 'roma') +
  labs(x = "Model", y = "Model estimate", color = "") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(vjust = 0.5, angle = 90))

ggsave(filename = "par_comp.png",
       path = here::here(new_year, "output", "safe_plots"),
       width = 6.5,
       height = 7,
       units = "in")



# curr year selex comparison ----

rec_mdl_res$sizeselex %>% 
  dplyr::rename_all(tolower) %>% 
  tidytable::filter(yr == new_year,
                    fleet <= 5,
                    factor == "Lsel") %>% 
  tidytable::select(-factor, -yr, -sex, -label) %>% 
  tidytable::pivot_longer(cols = as.character(seq(1, 105))) %>% 
  tidytable::rename(length = name) %>% 
  tidytable::mutate(fleet = case_when(fleet == 1 ~ "Trawl fishery",
                                      fleet == 2 ~ "Longline fishery",
                                      fleet == 3 ~ "Pot fishery",
                                      fleet == 4 ~ "AFSC bottom trawl survey",
                                      fleet == 5 ~ "AFSC longline survey"),
                    length = as.numeric(length),
                    model = "24.0") %>% 
  filter(length >= 5) %>% 
  tidytable::bind_rows(res_19_1e$sizeselex %>% 
                         dplyr::rename_all(tolower) %>% 
                         tidytable::filter(yr == new_year,
                                           fleet <= 5,
                                           factor == "Lsel") %>% 
                         tidytable::select(-factor, -yr, -sex, -label) %>% 
                         tidytable::pivot_longer(cols = as.character(seq(1, 105))) %>% 
                         tidytable::rename(length = name) %>% 
                         tidytable::mutate(fleet = case_when(fleet == 1 ~ "Trawl fishery",
                                                             fleet == 2 ~ "Longline fishery",
                                                             fleet == 3 ~ "Pot fishery",
                                                             fleet == 4 ~ "AFSC bottom trawl survey",
                                                             fleet == 5 ~ "AFSC longline survey"),
                                           length = as.numeric(length),
                                           model = "19.1e") %>% 
                         filter(length >= 5)) -> selex_comp_dat

selex_comp <- ggplot(data = selex_comp_dat,
                     aes(x = length, y = value, col = fleet)) +
  geom_line(size = 0.777) +
  theme_bw(base_size = 14) +
  facet_wrap(~model, ncol= 1) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "top",
        legend.key.width = unit(0.25, 'cm')) +
  labs(x = "Length (cm)", y = paste(new_year, "Selectivity"), col = "") +
  scico::scale_color_scico_d(palette = 'roma') + 
  guides(color = guide_legend(nrow = 2))

ggsave(filename = "curr_selex_comp.png",
       path = here::here(new_year, "output", "safe_plots"),
       width = 6.5,
       height = 7,
       units = "in")





## likelihood comparison ----

get_likes <- function(mdl_res){
  mdl_res$likelihoods_used %>% 
    tidytable::mutate(Component = rownames(.)) %>% 
    tidytable::select(Component, Value = values) %>% 
    tidytable::mutate(Value = round(Value, digits = 2)) %>% 
    tidytable::filter(Component == "TOTAL") %>% 
    tidytable::mutate(Component = "Total negative log-likelihood") %>% 
    tidytable::bind_rows(data.frame(Component = "Survey indices", 
                                    Value = round(mdl_res$likelihoods_used$values[which(rownames(mdl_res$likelihoods_used) == 'Survey')], digits = 2))) %>% 
    tidytable::bind_rows(mdl_res$likelihoods_by_fleet %>% 
                           tidytable::filter(Label == "Surv_like") %>% 
                           tidytable::select(-Label) %>% 
                           tidytable::pivot_longer(names_to = "Component", values_to = "Value") %>% 
                           tidytable::filter(Value != 0,
                                             Component != "ALL") %>% 
                           tidytable::mutate(Value = round(Value, digits = 2)) %>% 
                           tidytable::mutate(Component = c("Bottom trawl survey index", 
                                                           "Longline survey index"))) %>% 
    tidytable::bind_rows(data.frame(Component = "Length composition", 
                                    Value = round(mdl_res$likelihoods_used$values[which(rownames(mdl_res$likelihoods_used) == 'Length_comp')], digits = 2))) %>% 
    tidytable::bind_rows(mdl_res$likelihoods_by_fleet %>% 
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
    tidytable::bind_rows(data.frame(Component = "Conditional age-at-length", 
                                    Value = round(mdl_res$likelihoods_used$values[which(rownames(mdl_res$likelihoods_used) == 'Age_comp')], digits = 2))) %>% 
    tidytable::bind_rows(mdl_res$likelihoods_by_fleet %>% 
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
    tidytable::bind_rows(data.frame(Component = "Parameter deviations and priors", 
                                    Value = round(mdl_res$likelihoods_used$values[which(rownames(mdl_res$likelihoods_used) == 'Recruitment')] +
                                                    mdl_res$likelihoods_used$values[which(rownames(mdl_res$likelihoods_used) == 'InitEQ_Regime')] +
                                                    mdl_res$likelihoods_used$values[which(rownames(mdl_res$likelihoods_used) == 'Parm_priors')] +
                                                    mdl_res$likelihoods_used$values[which(rownames(mdl_res$likelihoods_used) == 'Parm_devs')], digits = 2))) %>% 
    tidytable::bind_rows(mdl_res$likelihoods_used %>% 
                           tidytable::mutate(Component = rownames(.)) %>% 
                           tidytable::select(Component, Value = values) %>% 
                           tidytable::mutate(Value = round(Value, digits = 2)) %>% 
                           tidytable::filter(Component %in% c("Recruitment", "InitEQ_Regime", "Parm_priors", "Parm_devs")) %>% 
                           tidytable::mutate(Component = c("Recruitment deviations",
                                                           "Initial Regime (InitEQ_Regime)",
                                                           "Parameter priors",
                                                           "Selectivity deviations"))) %>% 
    tidytable::bind_rows(mdl_res$len_comp_fit_table %>% 
                           tidytable::select(Fleet, Yr, Nsamp_in, effN) %>% 
                           tidytable::mutate(ratio_ess = Nsamp_in / effN) %>% 
                           tidytable::summarise(Value = round(mean(ratio_ess), digits = 2)) %>% 
                           tidytable::mutate(Component = "Length composition mean ISS/ESS")) %>% 
    tidytable::bind_rows(mdl_res$len_comp_fit_table %>% 
                           tidytable::select(Fleet, Yr, Nsamp_in, effN) %>% 
                           tidytable::mutate(ratio_ess = Nsamp_in / effN) %>% 
                           tidytable::summarise(Value = round(mean(ratio_ess), digits = 2), .by = Fleet) %>% 
                           tidytable::mutate(Component = case_when(Fleet == 1 ~ "Trawl fishery",
                                                                   Fleet == 2 ~ "Longline fishery",
                                                                   Fleet == 3 ~ "Pot fishery",
                                                                   Fleet == 4 ~ "Bottom trawl survey",
                                                                   Fleet == 5 ~ "Longline survey")) %>% 
                           tidytable::select(Component, Value)) %>% 
    tidytable::bind_rows(mdl_res$age_comp_fit_table %>% 
                           tidytable::select(Fleet, Yr, Nsamp_in, effN) %>% 
                           tidytable::mutate(ratio_ess = Nsamp_in / effN) %>% 
                           tidytable::summarise(Value = round(mean(ratio_ess), digits = 2)) %>% 
                           tidytable::mutate(Component = "Conditional age-at-length mean ISS/ESS")) %>% 
    tidytable::bind_rows(mdl_res$age_comp_fit_table %>% 
                           tidytable::select(Fleet, Yr, Nsamp_in, effN) %>% 
                           tidytable::mutate(ratio_ess = Nsamp_in / effN) %>% 
                           tidytable::summarise(Value = round(mean(ratio_ess), digits = 2), .by = Fleet) %>% 
                           tidytable::mutate(Component = case_when(Fleet == 1 ~ "Trawl fishery",
                                                                   Fleet == 2 ~ "Longline fishery",
                                                                   Fleet == 3 ~ "Pot fishery",
                                                                   Fleet == 4 ~ "Bottom trawl survey")) %>% 
                           tidytable::select(Component, Value))
}


like_comp <- get_likes(prev_mdl_res) %>% 
  tidytable::rename("19.1b-23" = Value) %>% 
  tidytable::bind_cols(get_likes(base_mdl_res) %>% 
                         tidytable::select(-Component) %>% 
                         tidytable::rename("19.1b" = Value)) %>% 
  tidytable::bind_cols(get_likes(res_19_1c) %>% 
                         tidytable::select(-Component) %>% 
                         tidytable::rename("19.1c" = Value)) %>% 
  tidytable::bind_cols(get_likes(res_19_1d) %>% 
                         tidytable::select(-Component) %>% 
                         tidytable::rename("19.1d" = Value)) %>% 
  tidytable::bind_cols(get_likes(res_19_1e) %>% 
                         tidytable::select(-Component) %>% 
                         tidytable::rename("19.1e" = Value)) %>% 
  tidytable::bind_cols(get_likes(rec_mdl_res) %>% 
                         tidytable::select(-Component) %>% 
                         tidytable::rename("24.0" = Value))

vroom::vroom_write(like_comp,
                   here::here(new_year, 'output', 'safe_tables', 'like_comp.csv'),
                   delim = ",")


# mean length & age ----

rec_mdl_res$len_comp_fit_table %>% 
  tidytable::select(year = Yr, fleet = Fleet, mean = All_obs_mean) %>% 
  tidytable::mutate(fleet = case_when(fleet == 1 ~ "Trawl fishery",
                                      fleet == 2 ~ "Longline fishery",
                                      fleet == 3 ~ "Pot fishery",
                                      fleet == 4 ~ "Bottom trawl survey",
                                      fleet == 5 ~ "Longline survey")) %>% 
  tidytable::left_join(vroom::vroom(here::here(new_year, "data", "raw", "fish_lfreq_domestic.csv")) %>% 
                         tidytable::select(gear, year, length, freq) %>% 
                         tidytable::uncount(freq) %>% 
                         tidytable::bind_rows(vroom::vroom(here::here(new_year, "data", "raw", "fish_lfreq_foreign.csv")) %>% 
                                                tidytable::select(gear, year, length, freq) %>% 
                                                tidytable::uncount(freq)) %>% 
                         tidytable::summarise(uci = quantile(length, probs = 0.975),
                                              lci = quantile(length, probs = 0.025), .by = c(year, gear)) %>% 
                         tidytable::mutate(fleet = case_when(gear == "trawl" ~ "Trawl fishery",
                                                             gear == "longline" ~ "Longline fishery",
                                                             gear == "pot" ~ "Pot fishery")) %>% 
                         tidytable::select(year, fleet, uci, lci) %>% 
                         tidytable::bind_rows(vroom::vroom(here::here(new_year, "data", "raw", "twl_srvy_lenfreq.csv")) %>% 
                                                tidytable::select(year, length, frequency) %>% 
                                                tidytable::uncount(frequency) %>% 
                                                tidytable::mutate(fleet = "Bottom trawl survey") %>% 
                                                tidytable::bind_rows(vroom::vroom(here::here(new_year, "data", "raw", "lls_rpn_length_data.csv")) %>% 
                                                                       tidytable::select(year, length, rpn) %>% 
                                                                       tidytable::mutate(fleet = "Longline survey",
                                                                                         rpn = round(rpn / 10, digits = 0)) %>% 
                                                                       tidytable::uncount(rpn)) %>% 
                                                tidytable::summarise(uci = quantile(length, probs = 0.975),
                                                                     lci = quantile(length, probs = 0.025), .by = c(year, fleet)))) %>% 
  tidytable::drop_na() %>% 
  tidytable::mutate(fleet = factor(fleet, levels = c("Trawl fishery", 
                                                     "Longline fishery",
                                                     "Pot fishery",
                                                     "Bottom trawl survey",
                                                     "Longline survey"))) -> mean_dat

mean_len <- ggplot(data = mean_dat, 
         aes(x = year, y = mean, col = fleet)) +
    geom_point(size = 2) +
    geom_pointrange(aes(ymin = lci, ymax = uci),
                    position = position_jitter(width = 0.3)) +
    geom_line(linewidth = 0.777, linetype = "dotted") +
    facet_wrap(~fleet, ncol = 1) +
    theme_bw(base_size = 14) +
    scico::scale_color_scico_d(palette = 'roma') +
    labs(x = "Year", y = "Mean length (cm)", color = "Fleet/Survey") +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          legend.position = "none")

 ggsave(filename = "mean_len.png",
        path = here::here(new_year, "output", "safe_plots"),
        width = 6.5,
        height = 7,
        units = "in")
  
  
  

# plot model data ----
r4ss::SSplotData(rec_mdl_res,
                 subplots = 2,
                 fleetcol = scico::scico(5, palette = 'roma'),
                 plot = FALSE,
                 print = TRUE,
                 pheight = 7,
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
       height = 7,
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
       height = 7,
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
retro_res$retrosumm_rec$SpawnBio %>% 
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
       height = 7,
       units = "in")


# leave-one-out ----

loo_res$pars %>% 
  tidytable::filter(Label %in% c("NatM_uniform_Fem_GP_1",
                                 "SR_LN(R0)",
                                 "LnQ_base_Srv(4)")) %>% 
  tidytable::select(-Yr, -recdev) %>% 
  tidytable::rename_with(~as.character(cyr + years), paste0("replist", seq(1,length(years)))) %>% 
  tidytable::pivot_longer(cols = as.character(cyr + years)) %>% 
  tidytable::bind_rows(loo_res$quants %>% 
                         tidytable::filter(Label %in% c(paste0("ForeCatch_", cyr + 1),
                                                        "annF_Btgt",
                                                        paste0("SSB_", cyr + 1))) %>% 
                         tidytable::select(-Yr) %>% 
                         tidytable::rename_with(~as.character(cyr + years), paste0("replist", seq(1,length(years)))) %>% 
                         tidytable::pivot_longer(cols = as.character(cyr + years))) %>% 
  tidytable::left_join(loo_res$parsSD %>% 
                         tidytable::filter(Label %in% c("NatM_uniform_Fem_GP_1",
                                                        "SR_LN(R0)",
                                                        "LnQ_base_Srv(4)")) %>% 
                         tidytable::rename_with(~as.character(cyr + years), paste0("replist", seq(1,length(years)))) %>% 
                         tidytable::pivot_longer(cols = as.character(cyr + years)) %>% 
                         tidytable::bind_rows(loo_res$quantsSD %>% 
                                                tidytable::filter(Label %in% c(paste0("ForeCatch_", cyr + 1),
                                                                               "annF_Btgt",
                                                                               paste0("SSB_", cyr + 1))) %>% 
                                                tidytable::select(-Yr) %>% 
                                                tidytable::rename_with(~as.character(cyr + years), paste0("replist", seq(1,length(years)))) %>% 
                                                tidytable::pivot_longer(cols = as.character(cyr + years))) %>% 
                         tidytable::rename(sd = value)) %>% 
  tidytable::mutate(year = as.numeric(name)) %>% 
  tidytable::left_join(rec_mdl_res$parameters %>% 
                         tidytable::select(Label, Value) %>% 
                         tidytable::filter(Label %in% c("NatM_uniform_Fem_GP_1",
                                                        "SR_LN(R0)",
                                                        "LnQ_base_Srv(4)")) %>% 
                         tidytable::rename(base = Value) %>% 
                         tidytable::bind_rows(rec_mdl_res$derived_quants %>% 
                                                tidytable::select(Label, Value) %>% 
                                                tidytable::filter(Label %in% c(paste0("ForeCatch_", cyr + 1),
                                                                               "annF_Btgt",
                                                                               paste0("SSB_", cyr + 1))) %>% 
                                                tidytable::rename(base = Value))) %>% 
  tidytable::mutate(name = case_when(Label == "NatM_uniform_Fem_GP_1" ~ "Base Natural Mortality",
                                     Label == "SR_LN(R0)" ~ "log(Mean Recruitment)",
                                     Label == "LnQ_base_Srv(4)" ~ "Bottom trawl survey catchability",
                                     Label == paste0("ForeCatch_", cyr + 1) ~ "One-year forecasted ABC",
                                     Label == "annF_Btgt" ~ "F40%",
                                     Label == paste0("SSB_", cyr + 1) ~ "One-year forecasted Spawning Biomass"),
                    value = case_when(Label == "LnQ_base_Srv(4)" ~ exp(value),
                                      .default = value),
                    sd = case_when(Label == "LnQ_base_Srv(4)" ~ sd* exp(value),
                                   .default = sd),
                    base = case_when(Label == "LnQ_base_Srv(4)" ~ exp(base),
                                     .default = base)) -> loo_plot_dat

loo_plot <- ggplot(data = loo_plot_dat,
                   aes(x = year, y = value, col = year)) +
  geom_errorbar(aes(ymin = value - 1.96 * sd, ymax = value + 1.96 * sd), width = 0.25) +
  geom_point(size = 3) +
  geom_hline(aes(yintercept = base), linewidth = 1.25, linetype = 2, color = "black") +
  theme_bw(base_size = 14) +
  facet_wrap( ~ name, 
              scales = "free_y", 
              ncol = 2, 
              labeller = labeller(name = label_wrap_gen(20))) +
  scico::scale_color_scico(palette = 'roma') +
  labs(x = 'Year', y = 'Parameter value') +
  scale_x_continuous(limits = c(min(loo_plot_dat$year) - 0.5, max(loo_plot_dat$year) + 0.5), 
                     breaks = seq(min(loo_plot_dat$year), max(loo_plot_dat$year), by = 1)) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(vjust = 0.5, angle = 90),
        legend.position = "none")

ggsave(filename = "loo.png",
       path = here::here(new_year, "output", "safe_plots"),
       width = 6.5,
       height = 6.5,
       units = "in")

# add-one-in ----

aoi_res$pars %>% 
  tidytable::filter(Label %in% c("NatM_uniform_Fem_GP_1",
                                 "SR_LN(R0)",
                                 "LnQ_base_Srv(4)")) %>% 
  tidytable::select(-Yr, -recdev) %>% 
  tidytable::rename_with(~aoi_mdls[2:length(aoi_mdls)], paste0("replist", seq(1,length(aoi_mdls[2:length(aoi_mdls)])))) %>% 
  tidytable::pivot_longer(cols = as.character(aoi_mdls[2:length(aoi_mdls)])) %>% 
  tidytable::bind_rows(aoi_res$quants %>% 
                         tidytable::filter(Label %in% c(paste0("ForeCatch_", cyr + 1),
                                                        "annF_Btgt",
                                                        paste0("SSB_", cyr + 1))) %>% 
                         tidytable::select(-Yr) %>% 
                         tidytable::rename_with(~aoi_mdls[2:length(aoi_mdls)], paste0("replist", seq(1,length(aoi_mdls[2:length(aoi_mdls)])))) %>% 
                         tidytable::pivot_longer(cols = as.character(aoi_mdls[2:length(aoi_mdls)]))) %>% 
  tidytable::left_join(aoi_res$parsSD %>% 
                         tidytable::filter(Label %in% c("NatM_uniform_Fem_GP_1",
                                                        "SR_LN(R0)",
                                                        "LnQ_base_Srv(4)")) %>% 
                         tidytable::rename_with(~aoi_mdls[2:length(aoi_mdls)], paste0("replist", seq(1,length(aoi_mdls[2:length(aoi_mdls)])))) %>% 
                         tidytable::pivot_longer(cols = as.character(aoi_mdls[2:length(aoi_mdls)])) %>% 
                         tidytable::bind_rows(aoi_res$quantsSD %>% 
                                                tidytable::filter(Label %in% c(paste0("ForeCatch_", cyr + 1),
                                                                               "annF_Btgt",
                                                                               paste0("SSB_", cyr + 1))) %>% 
                                                tidytable::select(-Yr) %>% 
                                                tidytable::rename_with(~aoi_mdls[2:length(aoi_mdls)], paste0("replist", seq(1,length(aoi_mdls[2:length(aoi_mdls)])))) %>% 
                                                tidytable::pivot_longer(cols = as.character(aoi_mdls[2:length(aoi_mdls)]))) %>% 
                         tidytable::rename(sd = value)) %>% 
  tidytable::left_join(base_res$parameters %>% 
                         tidytable::select(Label, Value) %>% 
                         tidytable::filter(Label %in% c("NatM_uniform_Fem_GP_1",
                                                        "SR_LN(R0)",
                                                        "LnQ_base_Srv(4)")) %>% 
                         tidytable::rename(base = Value) %>% 
                         tidytable::bind_rows(base_res$derived_quants %>% 
                                                tidytable::select(Label, Value) %>% 
                                                tidytable::filter(Label %in% c(paste0("ForeCatch_", cyr + 1),
                                                                               "annF_Btgt",
                                                                               paste0("SSB_", cyr + 1))) %>% 
                                                tidytable::rename(base = Value))) %>% 
  tidytable::mutate(par = case_when(Label == "NatM_uniform_Fem_GP_1" ~ "Base Natural Mortality",
                                    Label == "SR_LN(R0)" ~ "log(Mean Recruitment)",
                                    Label == "LnQ_base_Srv(4)" ~ "Bottom trawl survey catchability",
                                    Label == paste0("ForeCatch_", cyr + 1) ~ "One-year forecasted ABC",
                                    Label == "annF_Btgt" ~ "F40%",
                                    Label == paste0("SSB_", cyr + 1) ~ "One-year forecasted Spawning Biomass"),
                    value = case_when(Label == "LnQ_base_Srv(4)" ~ exp(value),
                                      .default = value),
                    sd = case_when(Label == "LnQ_base_Srv(4)" ~ sd* exp(value),
                                   .default = sd),
                    base = case_when(Label == "LnQ_base_Srv(4)" ~ exp(base),
                                     .default = base)) -> aoi_plot_dat

# plot aoi analysis
aoi_plot <- ggplot(data = aoi_plot_dat,
                   aes(x = name, y = value, col = name)) +
  geom_errorbar(aes(ymin = value - 1.96 * sd, ymax = value + 1.96 * sd), width = 0.25) +
  geom_point(size = 3) +
  geom_hline(aes(yintercept = base), linewidth = 1.25, linetype = 2, color = "black") +
  theme_bw(base_size = 14) +
  facet_wrap( ~ par, 
              scales = "free_y", 
              ncol = 2, 
              labeller = labeller(par = label_wrap_gen(20))) +
  scico::scale_color_scico_d(palette = 'roma') +
  labs(x = 'Dataset', y = 'Parameter value') +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(vjust = 0.5, angle = 90),
        legend.position = "none")

ggsave(filename = "aoi.png",
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
                  pheight = 7,
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
  labs(x = "Age", y = "Pacific cod size", col = "Year")

ggsave(filename = "grwth_fit.png",
       path = here::here(new_year, "output", "safe_plots"),
       width = 6.5,
       height = 7,
       units = "in")


# length-weight growth plots ----

# summarise weight data (mean & sd) by year and length
summ_data <- vroom::vroom(here::here(new_year, 'data', 'raw', 'twl_srvy_age.csv')) %>% 
  tidytable::select(year, length, weight) %>% 
  tidytable::bind_rows(vroom::vroom(here::here(new_year, 'data', 'raw', 'beachseine_lw.csv'))) %>% 
  tidytable::drop_na() %>% 
  tidytable::summarise(wt = mean(weight, na.rm = TRUE),
                       n = .N,
                       .by = c(year, length))

# define optimizing function
ests <- function(pars, summ_data){
  summ_data %>% 
    tidytable::mutate(est_wt = pars[1] * length ^ pars[2],
                      rss = n * (wt - est_wt) ^ 2) %>% 
    tidytable::summarise(obj = sum(rss))
}

# fit model
fit <- stats::optim(par = c(3e-06, 3), 
                    fn = ests,
                    summ_data = summ_data)

wtlen <- ggplot(data = summ_data,
       aes(x = length, y = wt, color = year)) +
  geom_point() +
  geom_line(aes(x = length, y = fit$par[1] * length ^ fit$par[2]),
            color = "darkgreen", size = 1) +
  scico::scale_color_scico(palette = 'roma') +
  theme_bw(base_size = 14) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  labs(x = "Length (cm)", y = "Weight (kg)", col = "Year")

ggsave(filename = "wtlen.png",
       path = here::here(new_year, "output", "safe_plots"),
       width = 6.5,
       height = 4.5,
       units = "in")



# plot profiles ----

profilevec <- list(rec = seq(12.2, 13.8, by = 0.2),
                   m = seq(0.41, 0.57, by = 0.02),
                   m14 = seq(0.73, 0.89, by = 0.02),
                   q_twl = seq(0.17, 0.33, by = 0.02),
                   q_ll = seq(0.07, 0.23, by = 0.02),
                   q_ll_env = seq(0.76, 1.08, by = 0.04))


get_profile_data <- function(data, profilevec){
  data %>% 
    tidytable::filter(Label %in% c("TOTAL",
                                   "Survey",
                                   "Length_comp",
                                   "Age_comp")) %>% 
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
                                       Label == "Age_comp" ~ "CAAL")) %>% 
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
                                                   "CAAL"))) %>% 
  tidytable::select(-min_like) -> profile_data


profile_plot <- ggplot(data = profile_data,
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
       height = 7,
       units = "in")


# temp vs depth ----

## set up plot data function ----
plot_dat <- function(subregion, catch, haul, lfreq, strata){
  # weight temperature by area to get mean
  haul %>% 
    tidytable::filter(!is.na(temp)) %>% 
    tidytable::left_join(strata %>% 
                           tidytable::filter(Subarea %in% subregion)) %>% 
    tidytable::filter(!is.na(area)) %>% 
    tidytable::summarise(mtemp = mean(temp),
                         n = .N,
                         .by = c(year, stratum, area, Subarea)) %>% 
    tidytable::mutate(tot_area = sum(area), .by = c(year)) %>% 
    tidytable::mutate(prop_area = area / tot_area) %>% 
    tidytable::summarise(mtemp = weighted.mean(mtemp, prop_area),
                         sd_temp = sqrt(modi::weighted.var(mtemp, prop_area)),
                         .by = c(year, Subarea)) -> temp
  
  # weight depth by cpue to get mean
  catch %>% 
    tidytable::left_join(haul) %>% 
    tidytable::filter(!is.na(cpue),
                      !is.na(depth)) %>% 
    tidytable::left_join(lfreq) %>% 
    tidytable::filter(!is.na(length)) %>% 
    tidytable::mutate(bin = case_when(length <= 60 ~ 1,
                                      length %in% seq(61, 80) ~ 2,
                                      length >= 80 ~ 3)) %>% 
    tidytable::summarise(freq = sum(freq), .by = c(year, stratum, hauljoin, cpue, temp, depth, bin)) %>% 
    tidytable::left_join(strata %>% 
                           tidytable::filter(Subarea %in% subregion)) %>% 
    tidytable::filter(!is.na(area)) %>% 
    tidytable::summarise(mdepth = weighted.mean(depth, cpue),
                         sd_depth = sqrt(modi::weighted.var(depth, cpue)),
                         n = length(unique(hauljoin)),
                         .by = c(year, Subarea, bin)) %>% 
    tidytable::filter(n >= 10) -> depth
  
  # join mean depth and mean temp
  depth %>% 
    tidytable::left_join(temp) %>% 
    mutate(xtemp = mean(mtemp),
           sdxtemp =sd(mtemp), 
           .by = c(bin)) %>% 
    tidytable::mutate(label = case_when(bin == 1 ~ '0-60 cm',
                                        bin == 2 ~ '61-80 cm',
                                        bin == 3 ~ '81-plus cm'),
                      regi = case_when(mtemp <= (xtemp - 0.66 * sdxtemp) ~ 'Cold',
                                       mtemp >= (xtemp + 0.66 * sdxtemp) ~ 'Warm',
                                       .default = 'Med')) -> dat
  dat
}

## set up plot function ----
plot_temdep <- function(dat){
  
  dat_text <- data.frame(
    text = c("0.15%", "", "",
             "2.67%", "", "",
             "7.77%", "", "",
             "22.3%", "", ""),
    label = c('0-60 cm', '61-80 cm', '81-plus cm',
              '0-60 cm', '61-80 cm', '81-plus cm',
              '0-60 cm', '61-80 cm', '81-plus cm',
              '0-60 cm', '61-80 cm', '81-plus cm'),
    x = 2.75,
    y = c(rep(-50, 3),
          rep(-125, 3),
          rep(-175, 3),
          rep(-225, 3)),
    Subarea = 'Central',
    regi = 'Med')
  
  ggplot(data = dat, aes(x = mtemp, y = -1 * mdepth, group = label, shape = Subarea, color = regi, fill = regi)) +
    geom_point() +
    geom_hline(yintercept = c(-100, -150, -200)) +
    facet_wrap(~ label) +
    scale_colour_manual(name = "Shelf temp.", values = c("blue","darkgrey","red")) +
    scale_fill_manual(name = "Shelf temp.", values = c("blue","darkgrey","red")) +
    geom_pointrange(data = dat, aes(y = -1 * mdepth, x = mtemp,
                                    ymax = -1 * (mdepth + sd_depth), 
                                    ymin = -1 * (mdepth - sd_depth),
                                    color = regi),
                    linewidth = 0.25) +
    geom_errorbarh(data = dat, aes(xmax = mtemp - sd_temp, 
                                   xmin = mtemp + sd_temp,
                                   color = regi),
                   linewidth = 0.25) +
    theme_test(base_size = 15) +
    theme(legend.position = 'top',
          legend.box = "vertical") +
    xlab(expression("Temperature ("* degree * C *")")) +
    ylab("Depth (m)") +
    geom_point(data = dat %>% 
                 tidytable::summarise(mtemp = mean(mtemp),
                                      mdepth = mean(mdepth),
                                      .by = c(bin, label, regi)),
               aes(x = mtemp, y = -1 * mdepth), 
               size = 3, shape = 23, color = "black", stroke = 1.5) +
    xlim(2, 8) + 
    geom_text(data = dat_text, mapping = aes(x = x, y = y, label = text), fontface = 'bold', color = "black")
}

# plot
dat <- plot_dat(subregion = c('Western', 'Central', 'Eastern'), catch, haul, lfreq, strata)
plot <- plot_temdep(dat)

ggsave(filename = "temp_depth.png",
       path = here::here(new_year, "output", "safe_plots"),
       width = 6.5,
       height = 7,
       units = "in")




# osa resids ----

# function to run osa for ss3
run_osa_ss3 <- function(mdl_res, flt, sx, fleet_name, comp_name){

  # length bins
  lens = unique(mdl_res$lendbase$Bin)
  
  # input sample sizes and years (vector)
  data.frame(mdl_res$lendbase[,c(1,6,13,16,22)]) %>% 
    dplyr::rename_all(tolower) %>% 
    tidytable::filter(bin == min(bin)) %>% 
    tidytable::filter(fleet == flt,
                      sex %in% sx) %>% 
    tidytable::select(yr, effn) %>% 
    as.vector(.) -> N
  
  # observed values (matrix)
  data.frame(mdl_res$lendbase[,c(1,6,13,16:18)]) %>% 
    dplyr::rename_all(tolower) %>% 
    tidytable::pivot_longer(cols = c(obs, exp)) %>% 
    tidytable::filter(fleet == flt,
                      sex %in% sx,
                      name == "obs") %>% 
    tidytable::pivot_wider(names_from = bin, values_from = value) %>% 
    tidytable::select(-yr, -fleet, -sex, -name) %>% 
    as.matrix(.) -> obs
  
  # expected values (matrix)
  data.frame(mdl_res$lendbase[,c(1,6,13,16:18)]) %>% 
    dplyr::rename_all(tolower) %>% 
    tidytable::pivot_longer(cols = c(obs, exp)) %>% 
    tidytable::filter(fleet == flt,
                      sex %in% sx,
                      name == "exp") %>% 
    tidytable::pivot_wider(names_from = bin, values_from = value) %>% 
    tidytable::select(-yr, -fleet, -sex, -name) %>% 
    as.matrix(.) -> exp
  
  
  out <- afscOSA::run_osa(fleet = fleet_name, index_label = comp_name,
                          obs = obs, exp = exp, N = N$effn, index = lens, years = N$yr)
  
  out
}

# plot fishery osa
fsh_flt = seq(1, 3)
fsh_fleet_name = c("Trawl fishery", "Longline fishery", "Pot fishery")
comp_name = "Length"

osa_out_fsh <- purrr::map(1:length(fsh_flt), ~run_osa_ss3(rec_mdl_res, 
                                                          fsh_flt[.], 
                                                          sx = 1, 
                                                          fleet_name = fsh_fleet_name[.], 
                                                          comp_name = comp_name))

fsh_osa <- afscOSA::plot_osa(osa_out_fsh,
                             outpath = here::here(new_year, "output", "safe_plots")) 

fsh_osa_new <- cowplot::plot_grid(fsh_osa$bubble +
                                    theme_bw(base_size = 12) +
                                    theme(legend.position = "top",
                                          legend.key.width = unit(0.1, 'cm')),
                                  fsh_osa$qq +
                                    theme_bw(base_size = 12),
                                  fsh_osa$aggcomp +
                                    theme_bw(base_size = 12),
                                  nrow = 3, 
                                  rel_heights = c(4,3,3))

ggsave(filename = "fsh_osa.png",
       path = here::here(new_year, "output", "safe_plots"),
       width = 6.5,
       height = 7,
       units = "in")


# plot survey osa
srv_flt = seq(4, 5)
srv_fleet_name = c("AFSC bottom trawl survey", "AFSC longline survey")
comp_name = "Length"

osa_out_srv <- purrr::map(1:length(srv_flt), ~run_osa_ss3(rec_mdl_res, 
                                                          srv_flt[.], 
                                                          sx = 1, 
                                                          fleet_name = srv_fleet_name[.], 
                                                          comp_name = comp_name))


srv_osa <- afscOSA::plot_osa(osa_out_srv,
                             outpath = here::here(new_year, "output", "safe_plots")) 

srv_osa_new <- cowplot::plot_grid(srv_osa$bubble +
                                    theme_bw(base_size = 12) +
                                    theme(legend.position = "top",
                                          legend.key.width = unit(0.1, 'cm')),
                                  srv_osa$qq +
                                    theme_bw(base_size = 12),
                                  srv_osa$aggcomp +
                                    theme_bw(base_size = 12),
                                  nrow = 3, 
                                  rel_heights = c(4,3,3))

ggsave(filename = "srv_osa.png",
       path = here::here(new_year, "output", "safe_plots"),
       width = 6.5,
       height = 7,
       units = "in")

# delete osa standard plot
invisible(file.remove(here::here(new_year, "output", "safe_plots", "osa_length_diagnostics.png")))




names(rec_mdl_res)

rec_mdl_res$len



# pearson resids ----


r4ss::SSplotComps(replist = rec_mdl_res,
                  subplots = 24,
                  plot = TRUE,
                  fleets = seq(1, 5),
                  fleetnames = c("Trawl fishery", "Longline fishery", "Pot fishery", "AFSC bottom trawl survey", "AFSC longline survey"),
                  print = TRUE,
                  pheight = 6.5,
                  plotdir = here::here(new_year, "output", "safe_plots"))

invisible(file.rename(from = here::here(new_year, "output", "safe_plots", "comp_lenfit__multi-fleet_comparison.png"),
                      to = here::here(new_year, "output", "safe_plots", "all_pearson.png")))





# curr year selex ----

rec_mdl_res$sizeselex %>% 
  dplyr::rename_all(tolower) %>% 
  tidytable::filter(yr == new_year,
                    fleet <= 5,
                    factor == "Lsel") %>% 
  tidytable::select(-factor, -yr, -sex, -label) %>% 
  tidytable::pivot_longer(cols = as.character(seq(1, 105))) %>% 
  tidytable::rename(length = name) %>% 
  tidytable::mutate(fleet = case_when(fleet == 1 ~ "Trawl fishery",
                                      fleet == 2 ~ "Longline fishery",
                                      fleet == 3 ~ "Pot fishery",
                                      fleet == 4 ~ "AFSC bottom trawl survey",
                                      fleet == 5 ~ "AFSC longline survey"),
                    length = as.numeric(length)) %>% 
  filter(length >= 5) -> selex_dat

selex_plot <- ggplot(data = selex_dat,
       aes(x = length, y = value, col = fleet)) +
  geom_line(size = 0.777) +
  theme_bw(base_size = 14) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "top",
        legend.key.width = unit(0.25, 'cm')) +
  labs(x = "Length (cm)", y = paste(new_year, "Selectivity"), col = "") +
  scico::scale_color_scico_d(palette = 'roma') + 
  guides(color = guide_legend(nrow = 2))

ggsave(filename = "curr_selex.png",
       path = here::here(new_year, "output", "safe_plots"),
       width = 6.5,
       height = 4.5,
       units = "in")





# time-dep selex ----

rec_mdl_res$sizeselex %>% 
  dplyr::rename_all(tolower) %>% 
  tidytable::filter(fleet %in% seq(1, 4),
                    factor == "Lsel",
                    yr <= new_year) %>% 
  tidytable::select(-factor, -sex, -label) %>% 
  tidytable::pivot_longer(cols = as.character(seq(1, 105))) %>% 
  tidytable::rename(length = name) %>% 
  tidytable::mutate(fleet = case_when(fleet == 1 ~ "Trawl fishery",
                                      fleet == 2 ~ "Longline fishery",
                                      fleet == 3 ~ "Pot fishery",
                                      fleet == 4 ~ "AFSC bottom trawl survey",
                                      fleet == 5 ~ "AFSC longline survey"),
                    length = as.numeric(length),
                    value = round(value, digits = 2)) %>% 
  filter(length >= 5) -> td_sel_data

td_sel <- ggplot(data = td_sel_data,
       aes(x = length, y = yr, z = value)) +
  geom_contour(aes(colour = after_stat(level))) +
  scico::scale_color_scico(palette = 'roma') +
  theme_bw(base_size = 14) +
  facet_wrap(~fleet, ncol = 1) +
  labs(x = "Length (cm)", y = "Selectivity", col = "") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.key.width = unit(0.25, 'cm'))

ggsave(filename = "td_selex.png",
       path = here::here(new_year, "output", "safe_plots"),
       width = 6.5,
       height = 7,
       units = "in")



# f ----

rec_mdl_res$derived_quants %>% 
  tidytable::select(Label, Value, StdDev) %>% 
  tidytable::slice(grep("F_", Label, perl = TRUE)) %>% 
  tidytable::filter(!(Label %in% c("annF_Btgt", "annF_SPR", "annF_MSY"))) %>% 
  tidytable::mutate(year = as.numeric(substr(Label, start = nchar(Label) - 3, stop = nchar(Label)))) %>% 
  tidytable::filter(year <= new_year) %>% 
  tidytable::mutate(f = Value,
                    sd_f = StdDev,
                    type = "Apical Fishing Mortality",
                    fleet = "Combined") %>% 
  tidytable::select(-Label, -Value, -StdDev) %>% 
  tidytable::bind_rows(rec_mdl_res$timeseries %>% 
                         tidytable::select(Yr, 'F:_1', 'F:_2', 'F:_3') %>% 
                         tidytable::filter(Yr %in% seq(1977, new_year)) %>% 
                         tidytable::rename(Trawl = 'F:_1',
                                           Longline = 'F:_2',
                                           Pot = 'F:_3') %>% 
                         tidytable::pivot_longer(cols = c(Trawl, Longline, Pot)) %>% 
                         tidytable::rename(year = Yr, fleet = name, f = value) %>% 
                         tidytable::mutate(sd_f = 0,
                                           type = "Continuous Fishing Mortality")) -> f_data


f_plot <- ggplot(data = f_data,
       aes(x = year, y = f, col = fleet)) +
  geom_line(size = 0.777) +
  geom_point() +
  facet_wrap(~type, ncol = 1, scales = "free_y") +
  theme_bw(base_size = 14) +
  geom_errorbar(aes(ymin = f + 1.96 * sd_f, ymax = f - 1.96 * sd_f), size = 0.777, width = 0) +
  labs(x = "Year", y = "Fishing Mortality", col = "Fleet") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.key.width = unit(0.25, 'cm'),
        legend.position = "top") +
  scico::scale_color_scico_d(palette = 'roma')

ggsave(filename = "f.png",
       path = here::here(new_year, "output", "safe_plots"),
       width = 6.5,
       height = 7,
       units = "in")


# phase-plane ----

Fabc = as.numeric(mscen$mscen_f$`Scenario 1`[length(mscen$mscen_f$F$`Scenario 1`)])
Fmsy = mscen$mscen_f$`Scenario 7`[length(mscen$mscen_f$F$`Scenario 7`)]
B35 = mscen$Two_year$SB35[1]
B0 = mscen$Two_year$SB100[1]
  
rec_mdl_res$derived_quants %>% 
  tidytable::select(Label, Value, StdDev) %>% 
  tidytable::slice(grep("F_", Label, perl = TRUE)) %>% 
  tidytable::filter(!(Label %in% c("annF_Btgt", "annF_SPR", "annF_MSY"))) %>% 
  tidytable::mutate(year = as.numeric(substr(Label, start = nchar(Label) - 3, stop = nchar(Label)))) %>% 
  tidytable::filter(year <= new_year + 2) %>% 
  tidytable::mutate(f = Value,
                    sd_f = StdDev) %>% 
  tidytable::select(year, f, sd_f) %>% 
  tidytable::left_join(rec_mdl_res$derived_quants %>% 
                         tidytable::select(Label, Value, StdDev) %>% 
                         tidytable::slice(grep("SSB", Label, perl = TRUE)) %>% 
                         tidytable::filter(!(Label %in% c("SSB_Virgin", "SSB_Initial", "SSB_unfished", "SSB_Btgt", "SSB_SPR", "SSB_MSY", "B_MSY/SSB_unfished"))) %>% 
                         tidytable::mutate(year = as.numeric(substr(Label, start = nchar(Label) - 3, stop = nchar(Label)))) %>% 
                         tidytable::filter(year <= new_year + 2) %>% 
                         tidytable::mutate(ssb = Value / 2,
                                           sd_ssb = StdDev / 2) %>%
                         tidytable::select(year, ssb, sd_ssb)) %>% 
  tidytable::mutate(f_rat = f / Fmsy,
                    ssb_rat = ssb / B35,
                    sd_f_rat = sd_f / Fmsy,
                    sd_ssb_rat = sd_ssb / B35) -> phs_pln_data

ggplot(data = phs_pln_data,
       aes(x = ssb_rat, y = f_rat, col = year)) +
  geom_path(size = 0.777) +
  geom_point(size = 7, col = "white") +
  coord_cartesian(xlim = c(0, 5), ylim = c(0, 1.1), expand = FALSE) +
  geom_text(aes(x = ssb_rat, y = f_rat, label = substr(year, 3, 4))) +
  theme_bw(base_size = 14) +
  scico::scale_color_scico(palette = 'roma') +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none") +
  geom_vline(xintercept = 1, linetype = "dashed") +
  geom_vline(xintercept = 0.2 / 0.35, linetype = "dashed", color = "brown", linewidth = 1) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_segment(x = 0, y = 0, xend = 1.2, yend = 1, color = "red", linewidth = 1) +
  geom_segment(x = 1.2, y = 1, xend = 5, yend = 1, color = "red", linewidth = 1) +
  geom_segment(x = (0.05 * B0) / (B0 * 0.40), 
               y = 0, 
               xend = (0.05 * B0) / (B0 * 0.40), 
               yend = Fabc / Fmsy*(((B0 * 0.05) / (B0 * 0.40)) - 0.05) / (1 - 0.05),
               linewidth = 1,
               color = "black") +
  geom_segment(x = (0.05 * B0) / (B0 * 0.40), 
               y = Fabc / Fmsy*(((B0 * 0.05) / (B0 * 0.40)) - 0.05) / (1 - 0.05), 
               xend = 0.4 / 0.35, 
               yend = Fabc / Fmsy,
               linewidth = 1,
               color = "black") +
  geom_segment(x = 0.4 / 0.35, 
               y = Fabc / Fmsy, 
               xend = 5, 
               yend = Fabc / Fmsy,
               linewidth = 1,
               color = "black") +
  geom_label(x = 4, y = 1, label = "OFL Definition", color = "red") +
  geom_label(x = 4, y = Fabc / Fmsy, label = "ABC Control Rule", color = "black") +
  labs(x = expression(italic(B) / italic(B)["35%"]), 
       y = expression(italic(F) / italic(F)["35%"]), col = "")


ggsave(filename = "phase_plane.png",
       path = here::here(new_year, "output", "safe_plots"),
       width = 6.5,
       height = 6.5,
       units = "in")


# mcmc adnuts pairs ----

load(here::here(new_year, "output", "mcmc", "mcmc_adnut.RData"))

dimnames(mcmc_adnut$samples)[[3]][1] <- "M"
dimnames(mcmc_adnut$samples)[[3]][3] <- "Linf"
dimnames(mcmc_adnut$samples)[[3]][7] <- "M[14-16]"
dimnames(mcmc_adnut$samples)[[3]][8] <- "R0"
dimnames(mcmc_adnut$samples)[[3]][68] <- "ln(q_BTS)"
dimnames(mcmc_adnut$samples)[[3]][69] <- "ln(q_LL)"

adnuts::pairs_admb(mcmc_adnut, 
                   pars = c('M', 'M[14-16]', 'R0', 'ln(q_BTS)', 'ln(q_LL)'),
                   label.cex = 1) +
  theme(axis.text = 1)

dev.print(png, file = here::here(new_year, "output", "safe_plots", "mcmc_pairs.png"), 
          width = 1024, height = 1000)

# mcmc histogram for key parameters ----

parameters <- c('NatM_uniform_Fem_GP_1', 
                'NatM_uniform_Fem_GP_1_BLK4repl_2014',  
                'L_at_Amax_Fem_GP_1', 
                "SR_LN(R0)",
                "LnQ_base_Srv(4)",
                'LnQ_base_LLSrv(5)')

mle_value <- rec_mdl_res$estimated_non_dev_parameters$Value[rownames(rec_mdl_res$estimated_non_dev_parameters) %in% parameters]

data.frame(cbind(rownames(rec_mdl_res$estimated_non_dev_parameters)[rownames(rec_mdl_res$estimated_non_dev_parameters) %in% parameters],
                 rec_mdl_res$estimated_non_dev_parameters$Value[rownames(rec_mdl_res$estimated_non_dev_parameters) %in% parameters])) %>% 
  tidytable::rename(parameter = 'X1',
                    mle_value = 'X2') %>% 
  tidytable::mutate(mle_value = as.numeric(mle_value),
                    parameter = case_when(parameter == 'NatM_uniform_Fem_GP_1' ~ 'M',
                                          parameter == 'NatM_uniform_Fem_GP_1_BLK4repl_2014' ~ 'M (2014-2016)',
                                          parameter == 'L_at_Amax_Fem_GP_1' ~ 'Linf',
                                          parameter == 'SR_LN(R0)' ~ 'R0',
                                          parameter == 'LnQ_base_Srv(4)' ~ 'q (Trawl survey)',
                                          parameter == 'LnQ_base_LLSrv(5)' ~ 'q (Longline survey)'),
                    mle_value = case_when(parameter %in% c('q (Trawl survey)', 'q (Longline survey)') ~ exp(mle_value),
                                          !(parameter %in% c('q (Trawl survey)', 'q (Longline survey)')) ~ mle_value)) -> mle

mcmc_eval %>% 
  tidytable::select(NatM_uniform_Fem_GP_1, 
                    NatM_uniform_Fem_GP_1_BLK4repl_2014,  
                    L_at_Amax_Fem_GP_1, 
                    `SR_LN(R0)`,
                    `LnQ_base_Srv(4)`,
                    `LnQ_base_LLSrv(5)`) %>% 
  tidytable::rename(M = 'NatM_uniform_Fem_GP_1',
                    'M (2014-2016)' = 'NatM_uniform_Fem_GP_1_BLK4repl_2014',
                    Linf = 'L_at_Amax_Fem_GP_1',
                    R0 = `SR_LN(R0)`,
                    'q (Trawl survey)' = `LnQ_base_Srv(4)`,
                    'q (Longline survey)' = `LnQ_base_LLSrv(5)`) %>% 
  tidytable::mutate(`q (Trawl survey)` = exp(`q (Trawl survey)`),
                    `q (Longline survey)` = exp(`q (Longline survey)`)) %>% 
  tidytable::pivot_longer(names_to = 'parameter') %>% 
  tidytable::left_join(mle) -> mcmc_plot_dat

mcmc_hist <- ggplot(mcmc_plot_dat, aes(x = value, y = after_stat(density), fill = value)) +
  geom_histogram(bins = 77, position = "identity", fill = scico::scico(1, palette = 'roma'), color = "black", alpha = 0.777) +
  geom_vline(aes(xintercept = mle_value), color = scico::scico(2, palette = 'roma')[2], linewidth = 2) +
  facet_wrap( ~ parameter,
              scales = 'free',
              ncol = 2) +
  theme_bw(base_size = 14) +
  labs(y = "Density", x = "Parameter value") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none")

ggsave(filename = "mcmc_hist.png",
       path = here::here(new_year, "output", "safe_plots"),
       width = 6.5,
       height = 7,
       units = "in")




# tot bio & ssb ----


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
  tidytable::mutate(type = factor(type, levels = c("Total biomass (1,000s t)", "Spawning biomass (1,000s t)"))) -> biom_dat

tot_ssb <- ggplot(data = biom_dat,
       aes(x = year, y = value, color = type)) +
  geom_point() +
  geom_line() +
  facet_wrap(~type, ncol = 1, scales = "free_y") +
  geom_errorbar(aes(ymin = value - 1.96 * sd, 
                    ymax = value + 1.96 * sd), 
                size = 0.777, width = 0) +
  scico::scale_color_scico_d(palette = 'roma') +
  geom_point(data = biom_dat %>% tidytable::filter(year > new_year),
             colour = "darkgreen") +
  geom_line(data = biom_dat %>% tidytable::filter(year > new_year),
            colour = "darkgreen",
            linetype = "dashed") +
  geom_errorbar(data = biom_dat %>% tidytable::filter(year > new_year),
                colour = "darkgreen",
                aes(ymin = value - 1.96 * sd, 
                    ymax = value + 1.96 * sd), 
                size = 0.777, width = 0,
                linetype = "dashed") +
  geom_vline(xintercept = new_year + 0.5,
             colour = "darkgreen",
             linetype = "dashed") +
  theme_bw(base_size = 14) +
  labs(x = "Year", y = "Estimated quantity", col = "") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.key.width = unit(0.25, 'cm'),
        legend.position = "none")

ggsave(filename = "tot_ssb.png",
       path = here::here(new_year, "output", "safe_plots"),
       width = 6.5,
       height = 7,
       units = "in")


# rec & devs ----


rec_mdl_res$parameters %>% 
  tidytable::filter(Label %in% paste0("Main_RecrDev_", seq(1978, new_year))) %>% 
  tidytable::select(value = Value, sd = Parm_StDev) %>% 
  tidytable::mutate(year = seq(1978, new_year),
                    type = "Recruitment deviation") %>% 
  tidytable::bind_rows(rec_mdl_res$derived_quants %>% 
                         tidytable::filter(Label %in% paste0("Recr_", seq(1978, new_year))) %>% 
                         tidytable::select(value = Value, sd = StdDev) %>% 
                         tidytable::mutate(year = seq(1978, new_year),
                                           value = value / 1000000,
                                           sd = sd / 1000000,
                                           type = "Age-0 Recruitment (millions)")) -> rec_dat


rec_plot <- ggplot(data = rec_dat,
       aes(x = year, y = value, color = type)) +
  geom_point() +
  geom_line(linetype = "dotted") +
  geom_hline(data = rec_dat %>% tidytable::filter(type == "Recruitment deviation"), aes(yintercept = 0)) +
  facet_wrap(~type, ncol = 1, scales = "free_y") +
  geom_errorbar(aes(ymin = value - 1.96 * sd, 
                    ymax = value + 1.96 * sd), 
                size = 0.777, width = 0) +
  scico::scale_color_scico_d(palette = 'roma') +
  theme_bw(base_size = 14) +
  labs(x = "Year", y = "Estimated quantity", col = "") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.key.width = unit(0.25, 'cm'),
        legend.position = "none")


ggsave(filename = "rec.png",
       path = here::here(new_year, "output", "safe_plots"),
       width = 6.5,
       height = 7,
       units = "in")


# ssb and rec w/ mcmc intervals ----



mcmc_eval %>% 
  tidytable::select(grep(colnames(mcmc_eval), pattern = "SSB")) %>% 
  tidytable::pivot_longer() %>% 
  tidytable::filter(!(name %in% c("SSB_Virgin", "SSB_Initial", "SSB_unfished", "SSB_Btgt", "SSB_SPR", "SSB_MSY", "B_MSY/SSB_unfished"))) %>% 
  tidytable::mutate(year = as.numeric(substr(name, 5, nchar(name)))) %>% 
  tidytable::filter(year <= new_year + 5) %>% 
  tidytable::summarise(uci = quantile(value / 2, probs = 0.975) / 1000,
                       lci = quantile(value / 2, probs = 0.025) / 1000,
                       .by = year) %>% 
  tidytable::left_join(rec_mdl_res$derived_quants %>% 
                         tidytable::select(Label, Value, StdDev) %>% 
                         tidytable::slice(grep("SSB", Label, perl = TRUE)) %>% 
                         tidytable::filter(!(Label %in% c("SSB_Virgin", "SSB_Initial", "SSB_unfished", "SSB_Btgt", "SSB_SPR", "SSB_MSY", "B_MSY/SSB_unfished"))) %>% 
                         tidytable::mutate(year = as.numeric(substr(Label, start = nchar(Label) - 3, stop = nchar(Label))),
                                           ssb = (Value / 2) / 1000) %>% 
                         tidytable::select(year, value = ssb)) %>% 
  tidytable::mutate(name = "Spawning biomass (1,000s t)")
    
    
    
mcmc_eval %>% 
  tidytable::select(grep(colnames(mcmc_eval), pattern = "SSB")) %>% 
  tidytable::pivot_longer() %>% 
  tidytable::filter(!(name %in% c("SSB_Virgin", "SSB_Initial", "SSB_unfished", "SSB_Btgt", "SSB_SPR", "SSB_MSY", "B_MSY/SSB_unfished"))) %>% 
  tidytable::mutate(year = as.numeric(substr(name, 5, nchar(name)))) %>% 
  tidytable::filter(year <= new_year + 5) %>% 
  tidytable::summarise(uci = quantile(value / 2, probs = 0.975) / 1000,
                       lci = quantile(value / 2, probs = 0.025) / 1000,
                       .by = year) %>% 
  tidytable::left_join(rec_mdl_res$derived_quants %>% 
                         tidytable::select(Label, Value, StdDev) %>% 
                         tidytable::slice(grep("SSB", Label, perl = TRUE)) %>% 
                         tidytable::filter(!(Label %in% c("SSB_Virgin", "SSB_Initial", "SSB_unfished", "SSB_Btgt", "SSB_SPR", "SSB_MSY", "B_MSY/SSB_unfished"))) %>% 
                         tidytable::mutate(year = as.numeric(substr(Label, start = nchar(Label) - 3, stop = nchar(Label))),
                                           ssb = (Value / 2) / 1000) %>% 
                         tidytable::select(year, value = ssb)) %>% 
  tidytable::mutate(name = "Spawning biomass (1,000s t)")
    
    
    
    
    
    
    
  tidytable::bind_rows(mcmc_eval %>% 
                         tidytable::select(grep(colnames(mcmc_eval), pattern = "Recr_")) %>% 
                         tidytable::pivot_longer() %>% 
                         tidytable::filter(!(name %in% c("Recr_Virgin", "Recr_Initial", "Recr_unfished"))) %>% 
                         tidytable::mutate(year = as.numeric(substr(name, 6, nchar(name)))) %>% 
                         tidytable::summarise(uci = quantile(value, probs = 0.975) / 1000000,
                                              lci = quantile(value, probs = 0.025) / 1000000,
                                              .by = year) %>% 
                         tidytable::left_join(rec_mdl_res$derived_quants %>% 
                                                tidytable::select(Label, Value, StdDev) %>% 
                                                tidytable::slice(grep("Recr_", Label, perl = TRUE)) %>% 
                                                tidytable::filter(!(Label %in% c("Recr_Virgin", "Recr_Initial", "Recr_unfished"))) %>% 
                                                tidytable::mutate(year = as.numeric(substr(Label, start = nchar(Label) - 3, stop = nchar(Label))),
                                                                  rec = Value / 1000000) %>% 
                                                tidytable::select(year, value = rec)) %>% 
                         tidytable::mutate(name = "Age-0 abundance (millions)")) %>% 
  tidytable::mutate(name = factor(name, levels = c("Spawning biomass (1,000s t)", "Age-0 abundance (millions)"))) -> mcmc_ssbrec


ssb_rec <- ggplot(data = mcmc_ssbrec,
       aes(x = year, y = value, color = name)) +
  geom_point() +
  geom_line() +
  geom_hline(data = mcmc_ssbrec %>% tidytable::filter(name == "Spawning biomass (1,000s t)"),
               aes(yintercept = (B0 / 1000) * 0.2), linetype = "dashed") +
  facet_wrap(~name, ncol = 1, scales = "free_y") +
  geom_errorbar(aes(ymin = lci, ymax = uci), size = 0.777, width = 0) +
  scico::scale_color_scico_d(palette = 'roma') +
  theme_bw(base_size = 14) +
  labs(x = "Year", y = "Derived quantity", col = "") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.key.width = unit(0.25, 'cm'),
        legend.position = "none")

ggsave(filename = "ssb_rec.png",
       path = here::here(new_year, "output", "safe_plots"),
       width = 6.5,
       height = 7,
       units = "in")



# apportionment ----

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


apport <- ggplot(apport_data, 
       aes(x = year, y = pred, color = Region, fill = Region)) +
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

ggsave(filename = "apport.png",
       path = here::here(new_year, "output", "safe_plots"),
       width = 6.5,
       height = 7,
       units = "in")



# ll survey sensitivities ----

llq_res$pred_ll %>% 
  tidytable::filter(model %in% c('base', 'no_cov')) %>% 
  tidytable::pivot_wider(names_from = model, values_from = values) %>% 
  tidytable::left_join(llq_res$pred_ll %>% 
                         tidytable::filter(!(model %in% c('base', 'no_cov'))) %>% 
                         tidytable::summarise(mean_rand = mean(values),
                                              uci_rand = max(values),
                                              lci_rand = min(values), .by = year)) %>% 
  tidytable::left_join(rec_mdl_res$cpue %>% 
                         dplyr::rename_all(tolower) %>% 
                         tidytable::filter(fleet == 5) %>% 
                         tidytable::mutate(uci_obs = obs + 1.96 * se * obs,
                                           lci_obs = obs - 1.96 * se * obs) %>% 
                         tidytable::select(year = yr, obs, uci_obs, lci_obs)) %>% 
  tidytable::left_join(llq_res$pred_ll %>% 
                         tidytable::filter(model %in% c('rand37', 'rand21')) %>% 
                         tidytable::pivot_wider(names_from = model, values_from = values)) -> llq_dat


colors <- c("No covariate" = scico::scico(3, palette = 'roma')[1], 
            "Recommended" = scico::scico(3, palette = 'roma')[3], 
            "Random" = "darkgrey")


llq <- ggplot(data = llq_dat, 
       aes(x = year, y = obs, ymin = lci_obs, ymax = uci_obs)) +
  geom_ribbon(aes(ymin = lci_rand, ymax = uci_rand), fill = "grey", alpha = 0.5) +
  geom_line(aes(x = year, y = rand21, color = "Random"), linewidth = 1) +
  geom_line(aes(x = year, y = rand37, color = "Random"), linewidth = 1) + 
  geom_point() +
  geom_linerange() +
  geom_line(aes(x = year, y = no_cov, color = "No covariate"), linewidth = 1) + 
  geom_line(aes(x = year, y = base, color = "Recommended"), linewidth = 1) +
  theme_bw(base_size = 14) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "top") +
  labs(x = "Year",
       y = "LL Survey RPN",
       color = "Catchability") +
  scale_color_manual(values = colors)

ggsave(filename = "llq.png",
       path = here::here(new_year, "output", "safe_plots"),
       width = 6.5,
       height = 4.5,
       units = "in")



# ageing error plots ----

age_bias <- ggplot(reread, aes(x = reread_age, y = original_age, col = num_reread, size = num_reread)) +
  geom_segment(x = 1, 
                   y = bias_res$`Age 1`[5] - 0.5, 
                   xend = 10, 
                   yend = bias_res$`Age 10`[5] - 0.5, size = 0.777, color = "darkgreen") +
  geom_abline(slope = 1, color = "grey", linewidth = 1.25) +
  geom_point(alpha = 0.5) +
  geom_text(aes(x = reread_age, y = original_age, label = num_reread, size = 2), color = "black") +
  scale_size(name = "Number re-aged", range = c(3, 21)) +
  scico::scale_color_scico(palette = 'roma') +
  theme_bw(base_size = 14) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none") +
  labs(x = "'True' age (2018 read)", y = "Pre-2007 age") +
  scale_x_continuous(breaks = seq(1, max(reread$reread_age)), labels = seq(1, max(reread$reread_age))) +
  scale_y_continuous(breaks = seq(1, max(reread$original_age)), labels = seq(1, max(reread$original_age)))

age_err <- ggplot(read_test, aes(x = read_age, y = test_age, col = n, size = n)) +
  geom_segment(x = 1,
               y = 1 + 1.96 * ae_res$`Age 0`[3] * 1,
               xend = 14,
               yend = max(read_test$read_age) + 1.96 * ae_res$`Age 0`[3] * max(read_test$read_age), 
               color = "darkgreen", size = 0.777) +
  geom_segment(x = 1,
               y = 1 - 1.96 * ae_res$`Age 0`[3] * 1,
               xend = 14,
               yend = max(read_test$read_age) - 1.96 * ae_res$`Age 0`[3] * max(read_test$read_age), 
               color = "darkgreen", size = 0.777) +
  geom_abline(slope = 1, color = "grey", linewidth = 1.25) +
  geom_point(alpha = 0.5) +
  geom_text(aes(x = read_age, y = test_age, label = n, size = 2), color = "black") +
  scale_size(name = "Number re-aged", range = c(3, 21)) +
  scico::scale_color_scico(palette = 'roma') +
  theme_bw(base_size = 14) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none") +
  labs(x = "Original age", y = "Tester age") +
  scale_x_continuous(breaks = seq(1, max(read_test$read_age)), labels = seq(1, max(read_test$read_age))) +
  scale_y_continuous(breaks = seq(1, max(read_test$test_age)), labels = seq(1, max(read_test$test_age)))

ageing <- cowplot::plot_grid(age_err,
                             age_bias,
                             nrow = 2)



ggsave(filename = "agerr.png",
       path = here::here(new_year, "output", "safe_plots"),
       width = 6.5,
       height = 6.5,
       units = "in")

