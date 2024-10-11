## Script to run 2024 GOA Pacific Cod Assessment (P. Hulson)


# load necessary packages ----
## cran packages ----
pkg_cran <- c("data.table",
              "tidyverse",
              "vroom",
              "here",
              "tictoc",
              "adnuts")

# if not installed, then install
if(length(pkg_cran[which(pkg_cran %in% rownames(installed.packages()) == FALSE )]) > 0) {
  install.packages(pkg_cran[which(pkg_cran %in% rownames(installed.packages()) == FALSE)])
}

# load packages
lapply(pkg_cran, library, character.only = TRUE)

## github packages ----
pkg_git <- c("r4ss")

# if not installed, then install
if(!isTRUE("r4ss" %in% rownames(installed.packages()))) {
  devtools::install_github("r4ss/r4ss", force = TRUE)
}

# load packages
lapply(pkg_git, library, character.only = TRUE)


# define run parameters/load fcns ----

# day data pulled
dat_day <- "Oct10"

# 2023 accepted model
base_mdl <- "2019.1b-2023"

# current assessment year
new_year <- as.numeric(format(Sys.Date(), format = "%Y"))

# source functions
source_files <- list.files(here::here(new_year, "R", "assessment"), "*.r$")
purrr::map(here::here(new_year, "R", "assessment", source_files), source)
source(here::here(new_year, "R", "utils.r"))


# run models ----

## 2019.1b: updated base model ----
base_mdl_update <- "2019.1b-2024"

# copy ss input files
if(!file.exists(here::here(new_year, 'mgmt', base_mdl_update, 'ss3.exe'))){
  start_ss_fldr(from = here::here(new_year - 1, 'mgmt', base_mdl),
                to = here::here(new_year, 'mgmt', base_mdl_update))
}


# update files
update_ss3_files(new_year, 
                 folder = 'mgmt',
                 mdl = base_mdl_update, 
                 dat_filename = paste0("GOAPcod2024", dat_day, "_old.dat"),
                 ctl_in = "Model19_1b.ctl",
                 ctl_out = "Model19_1b.ctl")

# run model
run_ss3_model(new_year, 
              folder = 'mgmt',
              mdl = base_mdl_update,
              ctl_filename = "Model19_1b.ctl")

## 2019.1c: new base model ----
# includes:
# - corrected ll survey sd
# - corrected ll survey length comp bins
# - corrected ll survey timing
# - fishery iss set at number of hauls that data is used from
# - plus length bin set at 104 cm
# - correct season for twl survey caal
# - turn off recr forecase phase
# - fic rec forecast option
# - update l-w

new_base <- "2019.1c-2024"

# copy ss input files
if(!file.exists(here::here(new_year, 'mgmt', new_base, 'ss3.exe'))){
  start_ss_fldr(from = here::here(new_year, 'mgmt', base_mdl_update),
                to = here::here(new_year, 'mgmt', new_base))
}

# update files
update_ss3_files(new_year, 
                 folder = 'mgmt',
                 mdl = new_base, 
                 dat_filename = paste0("GOAPcod2024", dat_day, "_1c.dat"),
                 ctl_in = "Model19_1c.ctl",
                 ctl_out = "Model19_1c.ctl")

# run model
run_ss3_model(new_year, 
              folder = 'mgmt',
              mdl = new_base,
              ctl_filename = "Model19_1c.ctl")

## 2019.1d: update ageing error ----
new_base_ae <- "2019.1d-2024"

# copy ss input files
if(!file.exists(here::here(new_year, 'mgmt', new_base_ae, 'ss3.exe'))){
  start_ss_fldr(from = here::here(new_year, 'mgmt', base_mdl_update),
                to = here::here(new_year, 'mgmt', new_base_ae))
}

# update files
update_ss3_files(new_year, 
                 folder = 'mgmt',
                 mdl = new_base_ae, 
                 dat_filename = paste0("GOAPcod2024", dat_day, "_1d.dat"),
                 ctl_in = "Model19_1d.ctl",
                 ctl_out = "Model19_1d.ctl")

# run model
run_ss3_model(new_year, 
              folder = 'mgmt',
              mdl = new_base_ae,
              ctl_filename = "Model19_1d.ctl")

## 2019.1e: new fishery length comps ----
new_base_lcomp <- "2019.1e-2024"

# copy ss input files
if(!file.exists(here::here(new_year, 'mgmt', new_base_lcomp, 'ss3.exe'))){
  start_ss_fldr(from = here::here(new_year, 'mgmt', new_base),
                to = here::here(new_year, 'mgmt', new_base_lcomp))
}

# update files
update_ss3_files(new_year, 
                 folder = 'mgmt',
                 mdl = new_base_lcomp, 
                 dat_filename = paste0("GOAPcod2024", dat_day, "_1e.dat"),
                 ctl_in = "Model19_1d.ctl",
                 ctl_out = "Model19_1e.ctl")

# run model
run_ss3_model(new_year, 
              folder = 'mgmt',
              mdl = new_base_lcomp,
              ctl_filename = "Model19_1e.ctl")


## 2019.1e.5: new fishery length comps at 5 cm bin ----
new_base_lcomp_bin5 <- "2019.1e.5cm-2024"

## copy ss input files ----
if(!file.exists(here::here(new_year, 'mgmt', new_base_lcomp_bin5, 'ss3.exe'))){
  start_ss_fldr(from = here::here(new_year, 'mgmt', new_base_lcomp),
                to = here::here(new_year, 'mgmt', new_base_lcomp_bin5))
}

## update files ----
update_ss3_files(new_year, 
                 folder = 'mgmt',
                 mdl = new_base_lcomp_bin5, 
                 dat_filename = paste0("GOAPcod2024", dat_day, "_1e_5cm.dat"),
                 ctl_in = "Model19_1d.ctl",
                 ctl_out = "Model19_1e.ctl")

## run model ----
run_ss3_model(new_year, 
              folder = 'mgmt',
              mdl = new_base_lcomp_bin5,
              ctl_filename = "Model19_1e.ctl")


# run management scenarios ----

## 2019.1b ----
base_mdl_update_mscen <- run_ss3_mgmnt_scen(dir = here::here(new_year, "mgmt", base_mdl_update),
                                            cyr = new_year,
                                            do_fig = FALSE)

## 2019.1e.5cm ----
new_base_lcomp_bin5_mscen <- run_ss3_mgmnt_scen(dir = here::here(new_year, "mgmt", new_base_lcomp_bin5),
                                                cyr = new_year,
                                                do_fig = FALSE)

## save results ----
if (!dir.exists(here::here(new_year, "output", "mscen"))) {
  dir.create(here::here(new_year, "output", "mscen"), recursive = TRUE)
}
save(base_mdl_update_mscen, file = here::here(new_year, "output", "mscen", "mgmnt_scen_19_1b.RData"))
save(new_base_lcomp_bin5_mscen, file = here::here(new_year, "output", "mscen", "mgmnt_scen_24.RData"))
write.csv(new_base_lcomp_bin5_mscen$Tables, here::here(new_year, "output", "mscen", "mgmnt_scen_table.csv"))
write.csv(new_base_lcomp_bin5_mscen$Two_year, here::here(new_year, "output", "mscen", "mgmnt_exec_summ_24.csv"))
write.csv(base_mdl_update_mscen$Two_year, here::here(new_year, "output", "mscen", "mgmnt_exec_summ_19_1b.csv"))


# run retrospective analysis ----

# define how many retro years you want to go back
ret_yr <- 1 # For testing
# ret_yr <- 10 # For full


## 2019.1b ----
r4ss::retro(dir = here::here(new_year, "mgmt", base_mdl_update),
            years = 0:-ret_yr)
# load the retrospective models
retro_1b <- r4ss::SSgetoutput(dirvec = here::here(new_year, "mgmt", base_mdl_update, "retrospectives", paste("retro", 0:-ret_yr, sep = "")))
# summarize the model results
retrosumm_1b <- r4ss::SSsummarize(retro_1b)

## 2019.1e.5cm ----
# Run retrospective
r4ss::retro(dir = here::here(new_year, "mgmt", new_base_lcomp_bin5),
            years = 0:-ret_yr)
# load the retrospective models
retro_1e5cm <- r4ss::SSgetoutput(dirvec = here::here(new_year, "mgmt", new_base_lcomp_bin5, "retrospectives", paste("retro", 0:-ret_yr, sep = "")))
# summarize the model results
retrosumm_1e5cm <- r4ss::SSsummarize(retro_1e5cm)

## save results ----
if (!dir.exists(here::here(new_year, "output", "retro"))) {
  dir.create(here::here(new_year, "output", "retro"), recursive = TRUE)
}
save(retrosumm_1b, file = here::here(new_year, "output", "retro", "retrosumm_1b.RData"))
save(retrosumm_1e5cm, file = here::here(new_year, "output", "retro", "retrosumm_1e5cm.RData"))


# run leave-one-out Analysis ----

## across time ----
# Define how many LOO years you want to go back
# loo_yr <- 1 # For testing
loo_yr <- 10 # For full

datname <- list.files(here::here(new_year, "mgmt", new_base_lcomp_bin5), pattern = "GOAPcod")

LOO <- SS_doLOO(Model_name = new_base_lcomp_bin5,
                newsubdir = "LeaveOneOut", 
                years = 0:-loo_yr,
                datafilename = datname,
                CYR = new_year,
                run_models = TRUE)

# Save output
save(LOO, file = here::here(new_year, "output", "LOO.RData"))
write.csv(LOO[[1]], here::here(new_year, "output", "LOO_table.csv"))

## by new data ----
# Run for newly added data - note that this is a hardwired function, where folders and data files must be manually changed to remove recent data
LOO_add_data <- SS_doLOO_cyr(Model_name = Model_name_new,
                             newsubdir = "LeaveOneOut",
                             CYR = new_year,
                             run_models = TRUE)

# Save output
save(LOO_add_data, file = here::here(new_year, "output", "LOO_add_data.RData"))


# Run Jitter ----
# note, in 2023 assessment couldn't get r4ss::jitter to run, but, could get the functions to run
# so just sourcing functions and will come back to this

source(here::here(new_year, "R", "assessment", "jitter.r"))

# Define how many jitters you want to do
Njitter <- 5 # For testing
# Njitter <- 50 # For full

# define a new directory
if (!file.exists(here::here(new_year, "mgmt", Model_name_new, "jitter"))) 
  dir.create(here::here(new_year, "mgmt", Model_name_new, "jitter"))

jitter_dir <- here::here(new_year, "mgmt", Model_name_new, "jitter")

# copy over the stock synthesis model files to the new directory
r4ss::copy_SS_inputs(dir.old = here::here(new_year, "mgmt", Model_name_new), 
                     dir.new = here::here(new_year, "mgmt", Model_name_new, "jitter"),
                     copy_par = TRUE,
                     copy_exe = TRUE,
                     overwrite = TRUE)

# run the jitters
jitter_loglike <- jitter(dir = jitter_dir,
                         Njitter = Njitter,
                         jitter_fraction = 0.05,
                         init_values_src = 1,
                         exe = "ss3",
                         printlikes = TRUE,
                         verbose = FALSE)

# Save output
write.csv(jitter_loglike, here::here(new_year, "output", "jitter_table.csv"))

















# Current model name
Model_name_old <- "2019.1a-2023"
Model_name_new <- "2019.1b-2023"

# Current assessment year
new_year <- as.numeric(format(Sys.Date(), format = "%Y"))


# Run models and management scenarios ----

# set up forecast file
forecast <- r4ss::SS_readforecast(file = here::here(new_year, "mgmt", Model_name_new, 'forecast.ss'))

forecast$Bmark_years <- c(1977, new_year - 2, 1977, new_year - 2, 1977, new_year - 1, 1977, new_year - 2, 1977, new_year - 2)

forecast$Fcast_years <- c(2000, new_year - 2, new_year - 5, new_year - 1, 1977, new_year - 2)

r4ss::SS_writeforecast(mylist = forecast,
                      dir = here::here(new_year, "mgmt", Model_name_old),
                      overwrite = TRUE)

r4ss::SS_writeforecast(mylist = forecast,
                       dir = here::here(new_year, "mgmt", Model_name_new),
                       overwrite = TRUE)


# Run previous accepted model (first run with init vals, then set to par for subsequent runs)

# set up starter file
old_starter <- r4ss::SS_readstarter(file = here::here(new_year, "mgmt", Model_name_old, 'starter.ss'))

old_starter$init_values_src = 0

r4ss::SS_writestarter(mylist = old_starter,
                      dir = here::here(new_year, "mgmt", Model_name_old),
                      overwrite = TRUE)

r4ss::run(dir = here::here(new_year, "mgmt", Model_name_old),
          skipfinished = FALSE,
          show_in_console = TRUE)

old_starter$init_values_src = 1
  
r4ss::SS_writestarter(mylist = old_starter,
                      dir = here::here(new_year, "mgmt", Model_name_old),
                      overwrite = TRUE)

model_run_old <- r4ss::SS_output(dir = here::here(new_year, "mgmt", Model_name_old),
                                 verbose = TRUE,
                                 printstats = FALSE)

# Run recommended model (first run with init vals, then set to par for subsequent runs)
new_starter <- r4ss::SS_readstarter(file = here::here(new_year, "mgmt", Model_name_new, 'starter.ss'))

new_starter$init_values_src = 0

r4ss::SS_writestarter(mylist = new_starter,
                      dir = here::here(new_year, "mgmt", Model_name_new),
                      overwrite = TRUE)

r4ss::run(dir = here::here(new_year, "mgmt", Model_name_new),
          skipfinished = FALSE,
          show_in_console = TRUE)

new_starter$init_values_src = 1

r4ss::SS_writestarter(mylist = new_starter,
                      dir = here::here(new_year, "mgmt", Model_name_new),
                      overwrite = TRUE)

model_run_new <- r4ss::SS_output(dir = here::here(new_year, "mgmt", Model_name_new),
                                 verbose = TRUE,
                                 printstats = FALSE)

# Run management scenarios

source(here::here(new_year, "R", "assessment", "run_mngmnt_scenarios.r"))

mscen_old <- Do_AK_Scenarios(Model_name = Model_name_old,
                             Model_dir = here::here(new_year, "mgmt", Model_name_old),
                             CYR = new_year,
                             SYR = 1977,
                             FCASTY = 15,
                             FLEETS = c(1:3),
                             do_fig = FALSE,
                             SEXES = 1)

mscen <- Do_AK_Scenarios(Model_name = Model_name_new,
                         Model_dir = here::here(new_year, "mgmt", Model_name_new),
                         CYR = new_year,
                         SYR = 1977,
                         FCASTY = 15,
                         FLEETS = c(1:3),
                         do_fig = TRUE,
                         SEXES = 1)

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

# Save output
save(model_run_new, file = here::here(new_year, "output", "model_run.RData"))
write.csv(ssb_pred, here::here(new_year, "output", "ssb_pred.csv"))
write.csv(tot_trwl_pred, here::here(new_year, "output", "tot_trwl_pred.csv"))

save(mscen, file = here::here(new_year, "output", "mgmnt_scen.RData"))
write.csv(mscen$Tables, here::here(new_year, "output", "mgmnt_scen_table.csv"))
write.csv(mscen$Two_year, here::here(new_year, "output", "mgmnt_exec_summ.csv"))
write.csv(mscen_old$Two_year, here::here(new_year, "output", "mgmnt_exec_summ_old.csv"))


# Run retrospective analysis ----

# Define how many retro years you want to go back
# ret_yr <- 1 # For testing
ret_yr <- 10 # For full

# Run retrospective
r4ss::retro(dir = here::here(new_year, "mgmt", Model_name_new),
            years = 0:-ret_yr)

# load the retrospective models
retroModels <- r4ss::SSgetoutput(dirvec = file.path(here::here(new_year, "mgmt", Model_name_new),
                                                    "retrospectives",
                                                    paste("retro", 0:-ret_yr, sep = "")))

# summarize the model results
retroSummary <- r4ss::SSsummarize(retroModels)

# Run ss3diags on retro results
endyrvec <- retroSummary[["endyrs"]] + 0:-ret_yr
rho_output_ss3diags <- ss3diags::SSplotRetro(retroSummary,
                                             subplots = c("SSB"),
                                             endyrvec = endyrvec,
                                             legendlabels = paste("Data", 0:-ret_yr, "years"),
                                             print = TRUE,
                                             plotdir = here::here(new_year, "plots", "other"),
                                             pwidth = 8.5,
                                             pheight = 4.5)

# Save output
save(retroSummary, file = here::here(new_year, "output", "retroSummary.RData"))
write.csv(retroSummary$SpawnBio, here::here(new_year, "output", "retro_SSB.csv"))
write.csv(retroSummary$recruits, here::here(new_year, "output", "retro_Rec.csv"))
write.csv(rho_output_ss3diags, here::here(new_year, "output", "retro_Rho_ss3diags.csv"))

## All the r4ss retrospective stuff kinda sux but keeping it here to maybe use in future
# r4ss::SSplotComparisons(retroSummary,
#                         endyrvec = endyrvec,
#                         legendlabels = paste("Data", 0:-10, "years"),
#                         print = TRUE,
#                         plotdir = here::here("plots", "retro"))
# # calculate Mohn's rho
# rho_output_r4ss <- r4ss::SSmohnsrho(summaryoutput = retroSummary,
#                                endyrvec = endyrvec,
#                                startyr = retroSummary[["endyrs"]] - 10,
#                                verbose = FALSE)
#write.csv(rho_output_r4ss, here::here("output", "retro_Rho_r4ss.csv"))


# Run Leave-One-Out Analysis ----

# Define how many LOO years you want to go back
# loo_yr <- 1 # For testing
loo_yr <- 10 # For full

ss_datname <- list.files(here::here(new_year, "mgmt", Model_name_new), pattern = "GOAPcod")

source(here::here(new_year, "R", "assessment", "LeaveOneOut.r"))

LOO <- SS_doLOO(Model_name = Model_name_new,
                newsubdir = "LeaveOneOut", 
                years = 0:-loo_yr,
                datafilename = ss_datname,
                CYR = new_year,
                run_models = TRUE)

# Save output
save(LOO, file = here::here(new_year, "output", "LOO.RData"))
write.csv(LOO[[1]], here::here(new_year, "output", "LOO_table.csv"))

# Run for newly added data - note that this is a hardwired function, where folders and data files must be manually changed to remove recent data
LOO_add_data <- SS_doLOO_cyr(Model_name = Model_name_new,
                             newsubdir = "LeaveOneOut",
                             CYR = new_year,
                             run_models = TRUE)

# Save output
save(LOO_add_data, file = here::here(new_year, "output", "LOO_add_data.RData"))


# Run Jitter ----
# note, in 2023 assessment couldn't get r4ss::jitter to run, but, could get the functions to run
# so just sourcing functions and will come back to this

source(here::here(new_year, "R", "assessment", "jitter.r"))

# Define how many jitters you want to do
Njitter <- 5 # For testing
# Njitter <- 50 # For full

# define a new directory
if (!file.exists(here::here(new_year, "mgmt", Model_name_new, "jitter"))) 
  dir.create(here::here(new_year, "mgmt", Model_name_new, "jitter"))

jitter_dir <- here::here(new_year, "mgmt", Model_name_new, "jitter")

# copy over the stock synthesis model files to the new directory
r4ss::copy_SS_inputs(dir.old = here::here(new_year, "mgmt", Model_name_new), 
                     dir.new = here::here(new_year, "mgmt", Model_name_new, "jitter"),
                     copy_par = TRUE,
                     copy_exe = TRUE,
                     overwrite = TRUE)

# run the jitters
jitter_loglike <- jitter(dir = jitter_dir,
                         Njitter = Njitter,
                         jitter_fraction = 0.05,
                         init_values_src = 1,
                         exe = "ss3",
                         printlikes = TRUE,
                         verbose = FALSE)

# Save output
write.csv(jitter_loglike, here::here(new_year, "output", "jitter_table.csv"))
