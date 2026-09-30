## Script to run alternative projections for GOA Pacific Cod Assessment (P. Hulson)

## Load required packages & define parameters ----

libs <- c("data.table",
          "dplyr",
          "ggplot2",
          "magrittr", 
          "r4ss")

if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )]) > 0) {
  install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE)])}

lapply(libs, library, character.only = TRUE)

# Model names
Model_name_14 <- "2019.1b-2014on"
Model_name_sigr <- "2019.1b-est_sigr"
Model_name_m <- "2019.1b-M17on"
Model_name_age <- "2019.1b-fit_age"

# Current assessment year
new_SS_dat_year <- as.numeric(format(Sys.Date(), format = "%Y"))

## Run models ----

# 2014 projection
model_dir_14 <- here::here(new_SS_dat_year, "mgmt", "alt_proj", Model_name_14)

r4ss::run(dir = model_dir_14,
          skipfinished = FALSE,
          show_in_console = TRUE)

model_run_14 <- r4ss::SS_output(dir = model_dir_14,
                                verbose = TRUE,
                                printstats = TRUE)

r4ss::SS_plots(model_run_14,
               printfolder = "",
               dir = paste0(model_dir_14, "/plots"))

# M separate 2017 on
model_dir_m <- here::here(new_SS_dat_year, "mgmt", "alt_proj", Model_name_m)

r4ss::run(dir = model_dir_m,
          skipfinished = FALSE,
          show_in_console = TRUE)

model_run_m <- r4ss::SS_output(dir = model_dir_m,
                               verbose = TRUE,
                               printstats = TRUE)

r4ss::SS_plots(model_run_m,
               printfolder = "",
               dir = paste0(model_dir_m, "/plots"))

# estimate sigr
model_dir_sigr <- here::here(new_SS_dat_year, "mgmt", "alt_proj", Model_name_sigr)

r4ss::run(dir = model_dir_sigr,
          skipfinished = FALSE,
          show_in_console = TRUE)

model_run_sigr <- r4ss::SS_output(dir = model_dir_sigr,
                                  verbose = TRUE,
                                  printstats = TRUE)

r4ss::SS_plots(model_run_sigr,
               printfolder = "",
               dir = paste0(model_dir_sigr, "/plots"))

# fit age
model_dir_age <- here::here(new_SS_dat_year, "mgmt", "alt_proj", Model_name_age)

model_run_age <- r4ss::SS_output(dir = model_dir_age,
                                  verbose = TRUE,
                                  printstats = TRUE)

r4ss::SS_plots(model_run_age,
               printfolder = "",
               dir = paste0(model_dir_age, "/plots"))



## Run management scenarios ----

source(here::here(new_SS_dat_year, "R", "assessment", "run_mngmnt_scenarios.r"))

# 2014 on in projection
mscen14 <- Do_AK_Scenarios(Model_name = Model_name_14,
                           Model_dir = model_dir_14,
                           CYR = new_SS_dat_year,
                           SYR = 1977,
                           FCASTY = 15,
                           FLEETS = c(1:3),
                           do_fig = TRUE,
                           SEXES = 1)

write.csv(mscen14$Tables, here::here(new_SS_dat_year, "output", "mgmnt_scen_table_14on.csv"))
write.csv(mscen14$Two_year, here::here(new_SS_dat_year, "output", "mgmnt_exec_summ_14on.csv"))

# separate m post 2017
mscenM <- Do_AK_Scenarios(Model_name = Model_name_m,
                          Model_dir = model_dir_m,
                          CYR = new_SS_dat_year,
                          SYR = 1977,
                          FCASTY = 15,
                          FLEETS = c(1:3),
                          do_fig = TRUE,
                          SEXES = 1)

write.csv(mscenM$Tables, here::here(new_SS_dat_year, "output", "mgmnt_scen_table_M17on.csv"))
write.csv(mscenM$Two_year, here::here(new_SS_dat_year, "output", "mgmnt_exec_summ_M17on.csv"))

# estimate sigr
mscensigr <- Do_AK_Scenarios(Model_name = Model_name_sigr,
                             Model_dir = model_dir_sigr,
                             CYR = new_SS_dat_year,
                             SYR = 1977,
                             FCASTY = 15,
                             FLEETS = c(1:3),
                             do_fig = TRUE,
                             SEXES = 1)

write.csv(mscensigr$Tables, here::here(new_SS_dat_year, "output", "mgmnt_scen_table_sigr.csv"))
write.csv(mscensigr$Two_year, here::here(new_SS_dat_year, "output", "mgmnt_exec_summ_sigr.csv"))

# plot comparison between base and sigr

Model_name_new <- "2019.1b-2023"

model_dir_new <- here::here(new_SS_dat_year, "mgmt", Model_name_new)

model_run_new <- r4ss::SS_output(dir = model_dir_new,
                                 verbose = TRUE,
                                 printstats = TRUE)

model_comp <- r4ss::SSsummarize(list(model_run_new, model_run_sigr))


r4ss::SSplotComparisons(model_comp,
                        legendlabels = c(Model_name_new, Model_name_sigr),
                        print = TRUE,
                        plotdir = here::here(new_SS_dat_year, "mgmt", "alt_proj", "comp_proj_plots") )


