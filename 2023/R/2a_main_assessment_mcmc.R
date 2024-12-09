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





