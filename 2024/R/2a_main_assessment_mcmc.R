## Script to run 2024 GOA Pacific Cod Assessment (P. Hulson)


# user-defined function arguments ----

## ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))<
## ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))<

# 2024 recommended model
new_base_lcomp_bin5 <- "2019.1e.5cm-2024"

# full run for retro/jitter/mcmc/etc
full_run = FALSE

# directory to work from
dir <- "C:/AA - PH Stuff/Asmnts/goa_pcod"

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
source_files <- list.files(paste0(dir, "/", new_year, "/R", "/assessment"), "*.r$")
purrr::map(paste0(dir, "/", new_year, "/R", "/assessment/", source_files), source)
source(paste0(dir, "/", new_year, "/R", "/utils.r"))


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
  iter <-5000
  thin <- 1
  warmup <- 250
}

# set up folder
if (!file.exists(paste0(dir, "/", new_year, "/mgmt/", new_base_lcomp_bin5, "/mcmc"))) {
  dir.create(paste0(dir, "/", new_year, "/mgmt/", new_base_lcomp_bin5, "/mcmc"), recursive = TRUE)
}

# set up folder for mcmc
  R.utils::copyDirectory(paste0(dir, "/", new_year, "/mgmt/", new_base_lcomp_bin5), paste0(dir, "/", new_year, "/mgmt/", new_base_lcomp_bin5, "/mcmc"), recursive = FALSE)

# read starter file
starter <- r4ss::SS_readstarter(paste0(dir, "/", new_year, "/mgmt/", new_base_lcomp_bin5, "/mcmc", "/starter.ss"))
# change init vals source
starter$init_values_src <- 0
# write modified starter file
r4ss::SS_writestarter(starter, 
                      dir = paste0(dir, "/", new_year, "/mgmt/", new_base_lcomp_bin5, "/mcmc"), 
                      overwrite = TRUE)


## run sample_nuts ----
mcmc_nut <- adnuts::sample_nuts(model = 'ss3',
                                path = paste0(dir, "/", new_year, "/mgmt/", new_base_lcomp_bin5, "/mcmc"),
                                iter = iter,
                                chains = 7,
                                warmup = warmup,
                                thin = thin,
                                mceval = TRUE,
                                control = list(metric = 'mle'),
                                skip_optimization = FALSE)


## save results ----
# if (!dir.exists(here::here(new_year, "output", "mcmc"))) {
#   dir.create(here::here(new_year, "output", "mcmc"), recursive = TRUE)
# }
# save(mcmc_nut, file = here::here(new_year, "output", "mcmc", "mcmc_nut.RData"))

# end timer
mcmc_time <- tictoc::toc()

5000*24*7

# num of sec for sample_rwm
3773.65

































# 
# 
# ## Script to run 2023 GOA Pacific Cod Assessment (P. Hulson)
# 
# # Load required packages & define parameters ----
# 
# libs <- c("data.table",
#           "dplyr",
#           "ggplot2",
#           "magrittr", 
#           "r4ss",
#           "adnuts")
# 
# if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )]) > 0) {
#   install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE)])}
# 
# lapply(libs, library, character.only = TRUE)
# 
# # Current model name
# Model_name_old <- "2019.1a-2023"
# Model_name_new <- "2019.1b-2023"
# 
# # Current assessment year
# new_SS_dat_year <- as.numeric(format(Sys.Date(), format = "%Y"))
# 
# # Set which method of MCMC to use
# # mcmc_meth <- 'base'
# mcmc_meth <- 'nuts'
# 
# # Set whether testing or doing full run
# mcmc_run <- 'test'
# # mcmc_run <- 'full'
# 
# # Run base MCMC ----
# if(mcmc_meth == 'base'){
# 
#   # Write SS files in MCMC subfolder
#   mcmc_dir <- here::here(new_SS_dat_year, "mgmt", Model_name_new, "MCMC")
#   r4ss::copy_SS_inputs(dir.old = here::here(new_SS_dat_year, "mgmt", Model_name_new), 
#                        dir.new = mcmc_dir,
#                        copy_par = TRUE,
#                        copy_exe = TRUE,
#                        overwrite = TRUE)
#   
#   # Read starter file
#   starter <- r4ss::SS_readstarter(file = here::here(new_SS_dat_year, "mgmt", Model_name_new, "MCMC", "starter.ss"))
#   
#   # Define burnin and length of chain
#   if(mcmc_run == 'test'){
#     starter$MCMCburn <- 100
#     chain <- 1000
#     save <- 2
#     st_time <- Sys.time()
#   }
#   if(mcmc_run == 'full'){
#     starter$MCMCburn <- 100
#     chain <- 1000000
#     save <- 2000
#   }
# 
#   # Run MCMC 
#   r4ss::SS_writestarter(starter,
#                         dir = mcmc_dir,
#                         file = "starter.ss",
#                         overwrite = TRUE)
#   
#   r4ss::run(dir = mcmc_dir,
#             extras = paste0("-mcmc ", chain," -mcsave ", save),
#             skipfinished = FALSE,
#             show_in_console = TRUE)
#   
#   r4ss::run(dir = mcmc_dir,
#             extras = "-mceval",
#             skipfinished = FALSE,
#             show_in_console = TRUE)
#   
#   # Read output
#   mcmc <- r4ss::SSgetMCMC(mcmc_dir)
#   
#   # Save output
#   save(mcmc, file = here::here(new_SS_dat_year, "output", "mcmc.RData"))
#   
#   if(mcmc_run == 'test'){
#     end_time <- Sys.time()
#     (end_time - st_time) * 1000 / 60
#   }
# }
# 
# # Run adnuts MCMC ----
# if(mcmc_meth == 'nuts'){
# 
#   # Write SS files in MCMC subfolder and run model
#   # mcmc_dir <- here::here(new_SS_dat_year, "mgmt", Model_name_new, "MCMC_nuts")
#   # r4ss::copy_SS_inputs(dir.old = here::here(new_SS_dat_year, "mgmt", Model_name_new), 
#   #                      dir.new = mcmc_dir,
#   #                      copy_par = TRUE,
#   #                      copy_exe = TRUE,
#   #                      overwrite = TRUE)
#   # r4ss::run(dir = mcmc_dir,
#   #           skipfinished = FALSE,
#   #           show_in_console = TRUE)
#   
#   # Define number of iterations
#   if(mcmc_run == 'test'){
#     iter <-5000
#     thin <- 2
#     st_time <- Sys.time()
#   }
#   if(mcmc_run == 'full'){
#     iter <- 5000000
#     thin <- 2000
#   }
# 
#   # Run MCMC
#   mcmc_nut <- adnuts::sample_rwm(model = 'ss3',
#                                  path = mcmc_dir,
#                                  iter = iter,
#                                  thin = thin,
#                                  skip_optimization = TRUE,
#                                  mceval = TRUE,
#                                  chains = 7)
#   
#   # Save output
#   save(mcmc_nut, file = here::here(new_SS_dat_year, "output", "mcmc_nut.RData"))
#   
#   if(mcmc_run == 'test'){
#     end_time <- Sys.time()
#     (end_time - st_time) * 1000 / 60
#   }
#   
# }
# 
# 
# 
# 
# 
