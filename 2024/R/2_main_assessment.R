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

# start timer
tictoc::tic()

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

# end timer
mdl_time <- tictoc::toc(quiet = TRUE)

# run management scenarios ----

#start timer
tictoc::tic()

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

mscen_time <- tictoc::toc(quiet = TRUE)

# run retrospective analysis ----

# start timer
tictoc::tic()

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

# end timer
retro_time <- tictoc::toc(quiet = TRUE)

# run leave-one-out analysis ----

## across time ----

# start timer
tictoc::tic()

# define how many loo years you want to go back
if(isTRUE(full_run)){
  loo_yr <- 10
} else{loo_yr <- 1}

loo_year <- year_loo(dir = here::here(new_year, "mgmt", new_base_lcomp_bin5),
                     years = 0:-loo_yr,
                     cyr = new_year)

# end timer
loo_yr_time <- tictoc::toc(quiet = TRUE)

## by new data ----

# start timer
tictoc::tic()

loo_data <- data_loo(dir = here::here(new_year, "mgmt", new_base_lcomp_bin5),
                     cyr = new_year)

## save results ----
if (!dir.exists(here::here(new_year, "output", "loo"))) {
  dir.create(here::here(new_year, "output", "loo"), recursive = TRUE)
}
save(loo_year, file = here::here(new_year, "output", "loo", "loo_year.RData"))
save(loo_data, file = here::here(new_year, "output", "loo", "loo_data.RData"))
write.csv(loo_year[[1]], here::here(new_year, "output", "loo", "loo_year_table.csv"))

# end timer
loo_dat_time <- tictoc::toc(quiet = TRUE)


# run jitter ----

# start timer
tictoc::tic()

# define how many jitters you want to go back
if(isTRUE(full_run)){
  Njitter <- 50
} else{Njitter <- 5}

## set up jitter model ----
# set up folder
if (!file.exists(here::here(new_year, "mgmt", new_base_lcomp_bin5, "jitter"))) {
  dir.create(here::here(new_year, "mgmt", new_base_lcomp_bin5, "jitter"), recursive = TRUE)
}

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
if (!dir.exists(here::here(new_year, "output", "jitter"))) {
  dir.create(here::here(new_year, "output", "jitter"), recursive = TRUE)
}
write.csv(jitter_loglike, here::here(new_year, "output", "jitter", "jitter_table.csv"))

# end timer
jitter_time <- tictoc::toc(quiet = TRUE)

# run mcmc ----

# start timer
tictoc::tic()

## set up ----

# define number of iterations
if(isTRUE(full_run)){
  iter <- 5000000
  thin <- 2000
  warmup <- 250
} else{
  iter <-500
  thin <- 1
  warmup <- 100
}

# set up folder
if (!file.exists(here::here(new_year, "mgmt", new_base_lcomp_bin5, "mcmc"))) {
  dir.create(here::here(new_year, "mgmt", new_base_lcomp_bin5, "mcmc"), recursive = TRUE)
}

# copy ss3 files
r4ss::copy_SS_inputs(dir.old = here::here(new_year, "mgmt", new_base_lcomp_bin5), 
                     dir.new = here::here(new_year, "mgmt", new_base_lcomp_bin5, "mcmc"),
                     copy_par = TRUE,
                     copy_exe = TRUE,
                     overwrite = TRUE)

# read starter file
starter <- r4ss::SS_readstarter(here::here(new_year, "mgmt", new_base_lcomp_bin5, "mcmc", "starter.ss"))
# change init vals source
starter$init_values_src <- 0
# write modified starter file
r4ss::SS_writestarter(starter, 
                      dir = here::here(new_year, "mgmt", new_base_lcomp_bin5, "mcmc"), 
                      overwrite = TRUE)


## run sample_nuts ----
mcmc_nut <- adnuts::sample_nuts(model = 'ss3',
                                path = here::here(new_year, "mgmt", new_base_lcomp_bin5, "mcmc"),
                                iter = iter,
                                chains = 3,
                                warmup = warmup,
                                thin = thin,
                                mceval = TRUE,
                                control = list(metric = 'mle'),
                                skip_optimization = FALSE)


## save results ----
if (!dir.exists(here::here(new_year, "output", "mcmc"))) {
  dir.create(here::here(new_year, "output", "mcmc"), recursive = TRUE)
}
save(mcmc_nut, file = here::here(new_year, "output", "mcmc", "mcmc_nut.RData"))

# end timer
mcmc_time <- tictoc::toc(quiet = TRUE)



# run profiles ----

## mean recruitment ----
if(isTRUE(full_run)){
  profilevec = seq(8:18)
} else{
  profilevec <- c(12, 13, 14)
}

r_prof <- run_profile(dir = here::here(new_year, "mgmt", new_base_lcomp_bin5),
                      mod_ctl = "Model19_1e.ctl",
                      folder = "r",
                      profilevec = profilevec,
                      linenum = 85)

## base m ----
if(isTRUE(full_run)){
  param_vec = seq(8:18)
} else{
  param_vec <- c(0.4, 0.5, 0.6)
}

m_prof <- run_profile(dir = here::here(new_year, "mgmt", new_base_lcomp_bin5),
                      mod_ctl = "Model19_1e.ctl",
                      folder = "m",
                      profilevec = profilevec,
                      linenum = 57)

## m 2014-2016 ----
if(isTRUE(full_run)){
  param_vec = seq(8:18)
} else{
  param_vec <- c(0.7, 0.8, 0.9)
}

m14_prof <- run_profile(dir = here::here(new_year, "mgmt", new_base_lcomp_bin5),
                        mod_ctl = "Model19_1e.ctl",
                        folder = "m14",
                        profilevec = profilevec,
                        linenum = 73)

## save results ----
if (!dir.exists(here::here(new_year, "output", "profile"))) {
  dir.create(here::here(new_year, "output", "profile"), recursive = TRUE)
}
save(r_prof, file = here::here(new_year, "output", "profile", "r_prof.RData"))
save(m_prof, file = here::here(new_year, "output", "profile", "m_prof.RData"))
save(m14_prof, file = here::here(new_year, "output", "profile", "m14_prof.RData"))























if(iters < iters_full){
  end <- tictoc::toc(quiet = TRUE)
  runtime <- round((((as.numeric(strsplit(end$callback_msg, split = " ")[[1]][1]) / iters) * iters_full) / 60) / 60, digits = 1)
  cat("Full run of", crayon::green$bold(iters_full), "iterations will take", crayon::red$bold$underline$italic(runtime), "hours", "\u2693","\n")
} else{
  cat("All", crayon::green$bold$underline$italic('Done'), "\u2693","\n")
}











# Current assessment year
new_SS_dat_year <- as.numeric(format(Sys.Date(), format = "%Y"))

# Set which method of MCMC to use
# mcmc_meth <- 'base'
mcmc_meth <- 'nuts'

# Set whether testing or doing full run
mcmc_run <- 'test'
# mcmc_run <- 'full'

# Run base MCMC ----
if(mcmc_meth == 'base'){
  
  # Write SS files in MCMC subfolder
  mcmc_dir <- here::here(new_SS_dat_year, "mgmt", Model_name_new, "MCMC")
  r4ss::copy_SS_inputs(dir.old = here::here(new_SS_dat_year, "mgmt", Model_name_new), 
                       dir.new = mcmc_dir,
                       copy_par = TRUE,
                       copy_exe = TRUE,
                       overwrite = TRUE)
  
  # Read starter file
  starter <- r4ss::SS_readstarter(file = here::here(new_SS_dat_year, "mgmt", Model_name_new, "MCMC", "starter.ss"))
  
  # Define burnin and length of chain
  if(mcmc_run == 'test'){
    starter$MCMCburn <- 100
    chain <- 1000
    save <- 2
    st_time <- Sys.time()
  }
  if(mcmc_run == 'full'){
    starter$MCMCburn <- 100
    chain <- 1000000
    save <- 2000
  }
  
  # Run MCMC 
  r4ss::SS_writestarter(starter,
                        dir = mcmc_dir,
                        file = "starter.ss",
                        overwrite = TRUE)
  
  r4ss::run(dir = mcmc_dir,
            extras = paste0("-mcmc ", chain," -mcsave ", save),
            skipfinished = FALSE,
            show_in_console = TRUE)
  
  r4ss::run(dir = mcmc_dir,
            extras = "-mceval",
            skipfinished = FALSE,
            show_in_console = TRUE)
  
  # Read output
  mcmc <- r4ss::SSgetMCMC(mcmc_dir)
  
  # Save output
  save(mcmc, file = here::here(new_SS_dat_year, "output", "mcmc.RData"))
  
  if(mcmc_run == 'test'){
    end_time <- Sys.time()
    (end_time - st_time) * 1000 / 60
  }
}

# Run adnuts MCMC ----
if(mcmc_meth == 'nuts'){
  
  # Write SS files in MCMC subfolder and run model
  # mcmc_dir <- here::here(new_SS_dat_year, "mgmt", Model_name_new, "MCMC_nuts")
  # r4ss::copy_SS_inputs(dir.old = here::here(new_SS_dat_year, "mgmt", Model_name_new), 
  #                      dir.new = mcmc_dir,
  #                      copy_par = TRUE,
  #                      copy_exe = TRUE,
  #                      overwrite = TRUE)
  # r4ss::run(dir = mcmc_dir,
  #           skipfinished = FALSE,
  #           show_in_console = TRUE)
  
  # Define number of iterations
  if(mcmc_run == 'test'){
    iter <-5000
    thin <- 2
    st_time <- Sys.time()
  }
  if(mcmc_run == 'full'){
    iter <- 5000000
    thin <- 2000
  }
  
  # Run MCMC
  mcmc_nut <- adnuts::sample_rwm(model = 'ss3',
                                 path = mcmc_dir,
                                 iter = iter,
                                 thin = thin,
                                 skip_optimization = TRUE,
                                 mceval = TRUE,
                                 chains = 7)
  
  # Save output
  save(mcmc_nut, file = here::here(new_SS_dat_year, "output", "mcmc_nut.RData"))
  
  if(mcmc_run == 'test'){
    end_time <- Sys.time()
    (end_time - st_time) * 1000 / 60
  }
  
}











