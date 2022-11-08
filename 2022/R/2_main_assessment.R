## Script to run 2022 GOA Pacific Cod Assessment (P. Hulson)

#######################################################################################
######## Load required packages & define parameters

libs <- c("data.table",
          "dplyr",
          "ggplot2",
          "magrittr", 
          "r4ss")

if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )]) > 0) {
  install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE)])}

lapply(libs, library, character.only = TRUE)

# Current model name
Model_name_old <- "Model19.1 (22)"
Model_name_new <- "Model19.1a (22) - wADFG"

# Current assessment year
new_SS_dat_year <- as.numeric(format(Sys.Date(), format = "%Y"))

#######################################################################################
######## Run models

# Old model
model_dir_old <- here::here(new_SS_dat_year, "Stock_Synthesis_files", Model_name_old)

r4ss::run(dir = model_dir_old,
          skipfinished = FALSE,
          show_in_console = TRUE)

# read the model output and print diagnostic messages
model_run_old <- r4ss::SS_output(dir = model_dir_old,
                    verbose = TRUE,
                    printstats = TRUE)

# Recommended model
model_dir_new <- here::here(new_SS_dat_year, "Stock_Synthesis_files", Model_name_new)

r4ss::run(dir = model_dir_new,
          skipfinished = FALSE,
          show_in_console = TRUE)

# read the model output and print diagnostic messages
model_run_new <- r4ss::SS_output(dir = model_dir_new,
                                 verbose = TRUE,
                                 printstats = TRUE)

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

#######################################################################################
######## Run retrospective analysis

# Define how many retro years you want to go back
# ret_yr <- 1 # For testing
ret_yr <- 10 # For full

# Run retrospective
r4ss::retro(dir = model_dir_new,
            oldsubdir = "",
            newsubdir = "retrospectives",
            years = 0:-ret_yr)

# load the retrospective models
retroModels <- r4ss::SSgetoutput(dirvec = file.path(
  model_dir_new, "retrospectives",
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


#######################################################################################
######## Run management scenarios

source(here::here(new_SS_dat_year, "R", "assessment", "run_mngmnt_scenarios.r"))

# Run management scenarios function
mscen <- Do_AK_Scenarios(Model_name = Model_name_new,
                         CYR = new_SS_dat_year,
                         SYR = 1977,
                         FCASTY = 15,
                         FLEETS = c(1:3),
                         do_fig = FALSE,
                         SEXES = 1)

# Save output
save(mscen, file = here::here(new_SS_dat_year, "output", "mgmnt_scen.RData"))
write.csv(mscen$Tables, here::here(new_SS_dat_year, "output", "mgmnt_scen_table.csv"))
write.csv(mscen$Two_year, here::here(new_SS_dat_year, "output", "mgmnt_exec_summ.csv"))


#######################################################################################
######## Run Leave-One-Out Analysis

# Define how many LOO years you want to go back
# loo_yr <- 1 # For testing
loo_yr <- 10 # For full

ss_datname <- list.files(here::here(new_SS_dat_year, "Stock_synthesis_files", Model_name_new), pattern = "GOAPcod")

source(here::here(new_SS_dat_year, "R", "assessment", "LeaveOneOut.r"))

LOO <- SS_doLOO(Model_name = Model_name_new,
                newsubdir = "LeaveOneOut", 
                years = 0:-loo_yr,
                datafilename = ss_datname,
                CYR = new_SS_dat_year)

# Save output
save(LOO, file = here::here(new_SS_dat_year, "output", "LOO.RData"))
write.csv(LOO[[1]], here::here(new_SS_dat_year, "output", "LOO_table.csv"))

# Run for newly added data
LOO_add_data <- SS_doLOO_cyr(Model_name = Model_name_new,
                newsubdir = "LeaveOneOut",
                CYR = new_SS_dat_year)

# Save output
save(LOO_add_data, file = here::here(new_SS_dat_year, "output", "LOO_add_data.RData"))



#######################################################################################
######## Run Jitter Test

# Define how many jitters you want to do
# Njitter <- 2 # For testing
Njitter <- 50 # For full

# define a new directory
if (!file.exists(here::here(new_SS_dat_year, "Stock_Synthesis_files", Model_name_new, "jitter"))) 
  dir.create(here::here(new_SS_dat_year, "Stock_Synthesis_files", Model_name_new, "jitter"))

jitter_dir <- here::here(new_SS_dat_year, "Stock_Synthesis_files", Model_name_new, "jitter")

# copy over the stock synthesis model files to the new directory
r4ss::copy_SS_inputs(dir.old = here::here(new_SS_dat_year, "Stock_Synthesis_files", Model_name_new), 
                     dir.new = here::here(new_SS_dat_year, "Stock_Synthesis_files", Model_name_new, "jitter"),
                     overwrite = TRUE)
base::file.copy(from = here::here(new_SS_dat_year, "Stock_Synthesis_files", Model_name_new, "ss.exe"),
                to = here::here(new_SS_dat_year, "Stock_Synthesis_files", Model_name_new, "jitter", "ss.exe"),
                overwrite = TRUE)
base::file.copy(from = here::here(new_SS_dat_year, "Stock_Synthesis_files", Model_name_new, "ss.par"),
                to = here::here(new_SS_dat_year, "Stock_Synthesis_files", Model_name_new, "jitter", "ss.par"),
                overwrite = TRUE)

# run the jitters
jitter_loglike <- r4ss::jitter(dir = jitter_dir,
                               Njitter = Njitter,
                               jitter_fraction = 0.05)

# Save output
write.csv(jitter_loglike, here::here(new_SS_dat_year, "output", "jitter_table.csv"))


#######################################################################################
######## Run MCMC

# Define burnin and length of chain
# starter$MCMCburn <- 100 # For testing
# chain <- 1000 # For testing
# save <- 2 # For testing
starter$MCMCburn <- 10000 # For full
chain <- 1000000 # For full
save <- 2000 # For full

mcmc_dir <- here::here(new_SS_dat_year, "Stock_Synthesis_files", Model_name_new, "MCMC")

# Write SS files in MCMC subfolder
r4ss::copy_SS_inputs(dir.old = model_dir_new, 
                     dir.new = mcmc_dir,
                     overwrite = TRUE)
base::file.copy(from = here::here(new_SS_dat_year, "Stock_Synthesis_files", Model_name_new, "ss.exe"),
                to = here::here(new_SS_dat_year, "Stock_Synthesis_files", Model_name_new, "MCMC", "ss.exe"),
                overwrite = TRUE)
base::file.copy(from = here::here(new_SS_dat_year, "Stock_Synthesis_files", Model_name_new, "ss.par"),
                to = here::here(new_SS_dat_year, "Stock_Synthesis_files", Model_name_new, "MCMC", "ss.par"),
                overwrite = TRUE)

# Run MCMC
starter <- r4ss::SS_readstarter(file = here::here(new_SS_dat_year, "Stock_Synthesis_files", Model_name_new, "MCMC", "starter.ss"))

r4ss::SS_writestarter(starter, 
                      dir = here::here(new_SS_dat_year, "Stock_Synthesis_files", Model_name_new, "MCMC"), 
                      file = "starter.ss",
                      overwrite = TRUE)

r4ss::run(dir = here::here(new_SS_dat_year, "Stock_Synthesis_files", Model_name_new, "MCMC"), 
          extras = paste0("-mcmc ", chain," -mcsave ", save),
          skipfinished = FALSE,
          show_in_console = TRUE)

r4ss::run(dir = here::here(new_SS_dat_year, "Stock_Synthesis_files", Model_name_new, "MCMC"), 
          extras = "-mceval",
          skipfinished = FALSE,
          show_in_console = TRUE)

# Read output
mcmc <- r4ss::SSgetMCMC(here::here(new_SS_dat_year, "Stock_Synthesis_files", Model_name_new, "MCMC"))

# Save output
save(mcmc, file = here::here(new_SS_dat_year, "output", "mcmc.RData"))


