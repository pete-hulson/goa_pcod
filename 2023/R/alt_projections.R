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

# Current assessment year
new_SS_dat_year <- as.numeric(format(Sys.Date(), format = "%Y"))

## Run models ----

# 2014 projection
model_dir_14 <- here::here(new_SS_dat_year, "mgmt", "alt_proj", Model_name_14)

r4ss::run(dir = model_dir_14,
          skipfinished = FALSE,
          show_in_console = TRUE)

# read the model output and print diagnostic messages
model_run_14 <- r4ss::SS_output(dir = model_dir_14,
                                 verbose = TRUE,
                                 printstats = TRUE)

## Run management scenarios ----

source(here::here(new_SS_dat_year, "R", "assessment", "run_mngmnt_scenarios.r"))

mscen14 <- Do_AK_Scenarios(Model_name = Model_name_14,
                           Model_dir = model_dir_14,
                           CYR = new_SS_dat_year,
                           SYR = 1977,
                           FCASTY = 15,
                           FLEETS = c(1:3),
                           do_fig = TRUE,
                           SEXES = 1)

# Save output
write.csv(mscen14$Tables, here::here(new_SS_dat_year, "output", "mgmnt_scen_table_14on.csv"))
write.csv(mscen14$Two_year, here::here(new_SS_dat_year, "output", "mgmnt_exec_summ_14on.csv"))
