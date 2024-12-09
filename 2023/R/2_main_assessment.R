## Script to run 2023 GOA Pacific Cod Assessment (P. Hulson)

# Load required packages & define parameters ----

libs <- c("data.table",
          "dplyr",
          "ggplot2",
          "magrittr", 
          "r4ss",
          "adnuts")

if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )]) > 0) {
  install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE)])}

lapply(libs, library, character.only = TRUE)

# Current model name
Model_name_old <- "2019.1a-2023"
Model_name_new <- "2019.1b-2023"

# Current assessment year
new_SS_dat_year <- as.numeric(format(Sys.Date(), format = "%Y"))


# Run models and management scenarios ----

# set up forecast file
forecast <- r4ss::SS_readforecast(file = here::here(new_SS_dat_year, "mgmt", Model_name_new, 'forecast.ss'))

forecast$Bmark_years <- c(1977, new_SS_dat_year - 2, 1977, new_SS_dat_year - 2, 1977, new_SS_dat_year - 1, 1977, new_SS_dat_year - 2, 1977, new_SS_dat_year - 2)

forecast$Fcast_years <- c(2000, new_SS_dat_year - 2, new_SS_dat_year - 5, new_SS_dat_year - 1, 1977, new_SS_dat_year - 2)

r4ss::SS_writeforecast(mylist = forecast,
                      dir = here::here(new_SS_dat_year, "mgmt", Model_name_old),
                      overwrite = TRUE)

r4ss::SS_writeforecast(mylist = forecast,
                       dir = here::here(new_SS_dat_year, "mgmt", Model_name_new),
                       overwrite = TRUE)


# Run previous accepted model (first run with init vals, then set to par for subsequent runs)

# set up starter file
old_starter <- r4ss::SS_readstarter(file = here::here(new_SS_dat_year, "mgmt", Model_name_old, 'starter.ss'))

old_starter$init_values_src = 0

r4ss::SS_writestarter(mylist = old_starter,
                      dir = here::here(new_SS_dat_year, "mgmt", Model_name_old),
                      overwrite = TRUE)

r4ss::run(dir = here::here(new_SS_dat_year, "mgmt", Model_name_old),
          skipfinished = FALSE,
          show_in_console = TRUE)

old_starter$init_values_src = 1
  
r4ss::SS_writestarter(mylist = old_starter,
                      dir = here::here(new_SS_dat_year, "mgmt", Model_name_old),
                      overwrite = TRUE)

model_run_old <- r4ss::SS_output(dir = here::here(new_SS_dat_year, "mgmt", Model_name_old),
                                 verbose = TRUE,
                                 printstats = FALSE)

# Run recommended model (first run with init vals, then set to par for subsequent runs)
new_starter <- r4ss::SS_readstarter(file = here::here(new_SS_dat_year, "mgmt", Model_name_new, 'starter.ss'))

new_starter$init_values_src = 0

r4ss::SS_writestarter(mylist = new_starter,
                      dir = here::here(new_SS_dat_year, "mgmt", Model_name_new),
                      overwrite = TRUE)

r4ss::run(dir = here::here(new_SS_dat_year, "mgmt", Model_name_new),
          skipfinished = FALSE,
          show_in_console = TRUE)

new_starter$init_values_src = 1

r4ss::SS_writestarter(mylist = new_starter,
                      dir = here::here(new_SS_dat_year, "mgmt", Model_name_new),
                      overwrite = TRUE)

model_run_new <- r4ss::SS_output(dir = here::here(new_SS_dat_year, "mgmt", Model_name_new),
                                 verbose = TRUE,
                                 printstats = FALSE)

# Run management scenarios

source(here::here(new_SS_dat_year, "R", "assessment", "run_mngmnt_scenarios.r"))

mscen_old <- Do_AK_Scenarios(Model_name = Model_name_old,
                             Model_dir = here::here(new_SS_dat_year, "mgmt", Model_name_old),
                             CYR = new_SS_dat_year,
                             SYR = 1977,
                             FCASTY = 15,
                             FLEETS = c(1:3),
                             do_fig = FALSE,
                             SEXES = 1)

mscen <- Do_AK_Scenarios(Model_name = Model_name_new,
                         Model_dir = here::here(new_SS_dat_year, "mgmt", Model_name_new),
                         CYR = new_SS_dat_year,
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
         year <= new_SS_dat_year + 1) -> ssb_pred

model_run_new$timeseries$Yr %>% 
  bind_cols(model_run_new$timeseries$Bio_all) %>% 
  rename(year = ...1, biom = ...2) %>% 
  filter(year >= 1984,
         year <= new_SS_dat_year + 1) -> tot_trwl_pred

# Save output
save(model_run_new, file = here::here(new_SS_dat_year, "output", "model_run.RData"))
write.csv(ssb_pred, here::here(new_SS_dat_year, "output", "ssb_pred.csv"))
write.csv(tot_trwl_pred, here::here(new_SS_dat_year, "output", "tot_trwl_pred.csv"))

save(mscen, file = here::here(new_SS_dat_year, "output", "mgmnt_scen.RData"))
write.csv(mscen$Tables, here::here(new_SS_dat_year, "output", "mgmnt_scen_table.csv"))
write.csv(mscen$Two_year, here::here(new_SS_dat_year, "output", "mgmnt_exec_summ.csv"))
write.csv(mscen_old$Two_year, here::here(new_SS_dat_year, "output", "mgmnt_exec_summ_old.csv"))


# Run retrospective analysis ----

# Define how many retro years you want to go back
# ret_yr <- 1 # For testing
ret_yr <- 10 # For full

# Run retrospective
r4ss::retro(dir = here::here(new_SS_dat_year, "mgmt", Model_name_new),
            years = 0:-ret_yr)

# load the retrospective models
retroModels <- r4ss::SSgetoutput(dirvec = file.path(here::here(new_SS_dat_year, "mgmt", Model_name_new),
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
                                             plotdir = here::here(new_SS_dat_year, "plots", "other"),
                                             pwidth = 8.5,
                                             pheight = 4.5)

# Save output
save(retroSummary, file = here::here(new_SS_dat_year, "output", "retroSummary.RData"))
write.csv(retroSummary$SpawnBio, here::here(new_SS_dat_year, "output", "retro_SSB.csv"))
write.csv(retroSummary$recruits, here::here(new_SS_dat_year, "output", "retro_Rec.csv"))
write.csv(rho_output_ss3diags, here::here(new_SS_dat_year, "output", "retro_Rho_ss3diags.csv"))

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

ss_datname <- list.files(here::here(new_SS_dat_year, "mgmt", Model_name_new), pattern = "GOAPcod")

source(here::here(new_SS_dat_year, "R", "assessment", "LeaveOneOut.r"))

LOO <- SS_doLOO(Model_name = Model_name_new,
                newsubdir = "LeaveOneOut", 
                years = 0:-loo_yr,
                datafilename = ss_datname,
                CYR = new_SS_dat_year,
                run_models = TRUE)

# Save output
save(LOO, file = here::here(new_SS_dat_year, "output", "LOO.RData"))
write.csv(LOO[[1]], here::here(new_SS_dat_year, "output", "LOO_table.csv"))

# Run for newly added data - note that this is a hardwired function, where folders and data files must be manually changed to remove recent data
LOO_add_data <- SS_doLOO_cyr(Model_name = Model_name_new,
                             newsubdir = "LeaveOneOut",
                             CYR = new_SS_dat_year,
                             run_models = TRUE)

# Save output
save(LOO_add_data, file = here::here(new_SS_dat_year, "output", "LOO_add_data.RData"))


# Run Jitter ----
# note, in 2023 assessment couldn't get r4ss::jitter to run, but, could get the functions to run
# so just sourcing functions and will come back to this

source(here::here(new_SS_dat_year, "R", "assessment", "jitter.r"))

# Define how many jitters you want to do
Njitter <- 5 # For testing
# Njitter <- 50 # For full

# define a new directory
if (!file.exists(here::here(new_SS_dat_year, "mgmt", Model_name_new, "jitter"))) 
  dir.create(here::here(new_SS_dat_year, "mgmt", Model_name_new, "jitter"))

jitter_dir <- here::here(new_SS_dat_year, "mgmt", Model_name_new, "jitter")

# copy over the stock synthesis model files to the new directory
r4ss::copy_SS_inputs(dir.old = here::here(new_SS_dat_year, "mgmt", Model_name_new), 
                     dir.new = here::here(new_SS_dat_year, "mgmt", Model_name_new, "jitter"),
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
write.csv(jitter_loglike, here::here(new_SS_dat_year, "output", "jitter_table.csv"))
