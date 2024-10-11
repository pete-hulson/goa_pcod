## Script to run 2024 GOA Pacific Cod Assessment (P. Hulson)


# user-defined function arguments ----

## ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))<
## ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))<

# day data pulled
dat_day <- "Oct10"

# 2023 accepted model
base_mdl <- "2019.1b-2023"

# full run for retro/jitter/mcmc/etc
full_run = FALSE

## ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))<
## ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))<


# load necessary packages ----
## cran packages ----
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
              "foreach")

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
# - fix rec forecast option
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
if(isTRUE(full_run)){
  ret_yr <- 10
} else{ret_yr <- 1}

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


# run leave-one-out analysis ----

## across time ----

# define how many loo years you want to go back
if(isTRUE(full_run)){
  loo_yr <- 10
} else{loo_yr <- 1}

loo_year <- year_loo(dir = here::here(new_year, "mgmt", new_base_lcomp_bin5),
                     years = 0:-loo_yr,
                     cyr = new_year)

## by new data ----
loo_data <- data_loo(dir = here::here(new_year, "mgmt", new_base_lcomp_bin5),
                     cyr = new_year)

## save results ----
if (!dir.exists(here::here(new_year, "output", "loo"))) {
  dir.create(here::here(new_year, "output", "loo"), recursive = TRUE)
}
save(loo_year, file = here::here(new_year, "output", "loo", "loo_year.RData"))
save(loo_data, file = here::here(new_year, "output", "loo", "loo_data.RData"))
write.csv(loo_year[[1]], here::here(new_year, "output", "loo", "loo_year_table.csv"))


# run jitter ----

# define how many jitters you want to go back
if(isTRUE(full_run)){
  Njitter <- 50
} else{Njitter <- 5}

## set up jitter model ----
# define a new directory
if (!file.exists(here::here(new_year, "mgmt", new_base_lcomp_bin5, "jitter"))) 
  dir.create(here::here(new_year, "mgmt", new_base_lcomp_bin5, "jitter"))
# copy ss3 files
r4ss::copy_SS_inputs(dir.old = here::here(new_year, "mgmt", new_base_lcomp_bin5), 
                     dir.new = here::here(new_year, "mgmt", new_base_lcomp_bin5, "jitter"),
                     copy_par = TRUE,
                     copy_exe = TRUE,
                     overwrite = TRUE)

## run the jitters ----
jitter_loglike <- r4ss::jitter(dir = here::here(new_year, "mgmt", new_base_lcomp_bin5, "jitter"),
                               Njitter = Njitter,
                               jitter_fraction = 0.05,
                               init_values_src = 1,
                               exe = "ss3",
                               printlikes = TRUE,
                               verbose = FALSE)

## save results ----
write.csv(jitter_loglike, here::here(new_year, "output", "jitter_table.csv"))










