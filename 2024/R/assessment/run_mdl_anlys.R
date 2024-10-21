#' function to run model analyses
#' developed in 2024 by p hulson
#' 
#' @param new_year current assessment year (default = NULL)
#' @param base_mdl previous year final accepted model (default = NULL)
#' @param rec_mdl author recommended model (default = NULL)
#' @param full_run boolean, whether a full analysis is to be conducted (default = NULL)
#' @param rec_ctl name recommended model ctl file (default = NULL)
#' @param run_mcmc boolean, whether to run mcmc or not (default = FALSE)
#' 
run_mdl_anlys <- function(new_year = NULL,
                          base_mdl = NULL,
                          rec_mdl = NULL,
                          full_run = NULL,
                          rec_ctl = NULL,
                          run_mcmc = FALSE){
  
  # load functions ----
  source_files <- c(list.files(here::here(new_year, "R", "assessment"), pattern = "*.r$"),
                    list.files(here::here(new_year, "R", "assessment"), pattern = "*.R$"))
  purrr::map(1:length(source_files), ~ source(here::here(new_year, "R", "assessment", source_files[.]), local = TRUE))
  source(here::here(new_year, "R", "utils.R"), local = TRUE)
  
  # run management scenarios ----
  cat("\u231b", crayon::blue("working on management scenarios..."), "\n")
  tictoc::tic()
  
  ## base model ----
  base_mdl_mscen <- run_ss3_mgmnt_scen(dir = here::here(new_year, "mgmt", base_mdl),
                                       cyr = new_year,
                                       do_fig = FALSE)
  
  ## recommended model ----
  rec_mdl_mscen <- run_ss3_mgmnt_scen(dir = here::here(new_year, "mgmt", rec_mdl),
                                      cyr = new_year,
                                      do_fig = FALSE)
  
  ## save results ----
  if (!dir.exists(here::here(new_year, "output", "mscen"))) {
    dir.create(here::here(new_year, "output", "mscen"), recursive = TRUE)
  }
  save(base_mdl_mscen, file = here::here(new_year, "output", "mscen", "mgmnt_scen_base.RData"))
  save(rec_mdl_mscen, file = here::here(new_year, "output", "mscen", "mgmnt_scen_rec.RData"))
  write.csv(rec_mdl_mscen$Tables, here::here(new_year, "output", "mscen", "mgmnt_scen_table.csv"))
  write.csv(rec_mdl_mscen$Two_year, here::here(new_year, "output", "mscen", "mgmnt_exec_summ_rec.csv"))
  write.csv(base_mdl_mscen$Two_year, here::here(new_year, "output", "mscen", "mgmnt_exec_summ_base.csv"))
  
  # print message when done
  cat(crayon::green$bold("\u2713"), crayon::blue("management scenarios"), crayon::green$underline$bold$italic("DONE"), "\n")
  mscen_time <- tictoc::toc(quiet = TRUE)
  
  # run retrospective analysis ----
  cat("\u231b", crayon::blue("working on retrospective analysis..."), "\n")
  tictoc::tic()
  
  # define how many retro years you want to go back
  if(isTRUE(full_run)){
    ret_yr <- 10
  } else{ret_yr <- 1}
  
  ## run retro in parallel ----
  # list models
  mdls = c(base_mdl, rec_mdl)
  # set up index
  indx = seq(1, length(mdls))
  
  # Get the number of available cores
  num_cores <- parallel::detectCores()
  if(num_cores > length(mdls)) num_cores = length(mdls)
  # Set the number of cores to be used for parallel computing
  doParallel::registerDoParallel(cores = num_cores)
  
  foreach::foreach(i = indx) %dopar% {
    
    r4ss::retro(dir = here::here(new_year, "mgmt", mdls[i]),
                years = 0:-ret_yr)
    
  }
  
  # Stop parallel computing
  doParallel::stopImplicitCluster()
  
  # load the retrospective models
  retro_base <- r4ss::SSgetoutput(dirvec = here::here(new_year, "mgmt", base_mdl, "retrospectives", paste("retro", 0:-ret_yr, sep = "")),
                                  verbose = FALSE)
  retro_rec <- r4ss::SSgetoutput(dirvec = here::here(new_year, "mgmt", rec_mdl, "retrospectives", paste("retro", 0:-ret_yr, sep = "")),
                                 verbose = FALSE)
  # summarize the model results
  retrosumm_rec <- r4ss::SSsummarize(retro_rec,
                                     verbose = FALSE)
  retrosumm_base <- r4ss::SSsummarize(retro_base,
                                      verbose = FALSE)
  
  ## save results ----
  if(isTRUE(full_run)){
    if (!dir.exists(here::here(new_year, "output", "retro"))) {
      dir.create(here::here(new_year, "output", "retro"), recursive = TRUE)
    }
    save(retrosumm_base, file = here::here(new_year, "output", "retro", "retrosumm_base.RData"))
    save(retrosumm_rec, file = here::here(new_year, "output", "retro", "retrosumm_rec.RData"))
  }
  
  # print message when done
  cat(crayon::green$bold("\u2713"), crayon::blue("retrospective analysis"), crayon::green$underline$bold$italic("DONE"), "\n")
  retro_time <- tictoc::toc(quiet = TRUE)
  
  # run leave-one-out analysis ----
  cat("\u231b", crayon::blue("working on leave-one-out analysis..."), "\n")
  
  ## across time ----
  tictoc::tic()
  
  # define how many loo years you want to go back
  if(isTRUE(full_run)){
    loo_yr <- 10
  } else{loo_yr <- 1}
  
  loo_year <- year_loo(dir = here::here(new_year, "mgmt", rec_mdl),
                       years = 0:-loo_yr,
                       cyr = new_year)
  
  # end timer
  loo_yr_time <- tictoc::toc(quiet = TRUE)
  
  ## by new data ----
  tictoc::tic()
  
  loo_data <- data_loo(dir = here::here(new_year, "mgmt", rec_mdl),
                       cyr = new_year)
  
  ## save results ----
  if(isTRUE(full_run)){
    if (!dir.exists(here::here(new_year, "output", "loo"))) {
      dir.create(here::here(new_year, "output", "loo"), recursive = TRUE)
    }
    save(loo_year, file = here::here(new_year, "output", "loo", "loo_year.RData"))
    save(loo_data, file = here::here(new_year, "output", "loo", "loo_data.RData"))
    write.csv(loo_year[[1]], here::here(new_year, "output", "loo", "loo_year_table.csv"))
  }
  
  # end timer
  loo_dat_time <- tictoc::toc(quiet = TRUE)
  
  # print message when done
  cat(crayon::green$bold("\u2713"), crayon::blue("leave-one-out analysis"), crayon::green$underline$bold$italic("DONE"), "\n")
  
  # run jitter ----
  cat("\u231b", crayon::blue("working on jitter analysis..."), "\n")
  tictoc::tic()
  
  # define how many jitters you want to go back
  if(isTRUE(full_run)){
    Njitter <- 50
  } else{Njitter <- 5}
  
  ## set up jitter model ----
  # set up folder
  if (!file.exists(here::here(new_year, "mgmt", rec_mdl, "jitter"))) {
    dir.create(here::here(new_year, "mgmt", rec_mdl, "jitter"), recursive = TRUE)
  }
  
  # copy ss3 files
  r4ss::copy_SS_inputs(dir.old = here::here(new_year, "mgmt", rec_mdl), 
                       dir.new = here::here(new_year, "mgmt", rec_mdl, "jitter"),
                       copy_par = TRUE,
                       copy_exe = TRUE,
                       overwrite = TRUE,
                       verbose = FALSE)
  
  ## run the jitters ----
  jitter_loglike <- r4ss::jitter(dir = here::here(new_year, "mgmt", rec_mdl, "jitter"),
                                 Njitter = Njitter,
                                 jitter_fraction = 0.05,
                                 init_values_src = 1,
                                 exe = "ss3",
                                 printlikes = FALSE,
                                 verbose = FALSE)
  
  ## save results ----
  if(isTRUE(full_run)){
    if (!dir.exists(here::here(new_year, "output", "jitter"))) {
      dir.create(here::here(new_year, "output", "jitter"), recursive = TRUE)
    }
    write.csv(jitter_loglike, here::here(new_year, "output", "jitter", "jitter_table.csv"))
  }
  
  # print message when done
  cat(crayon::green$bold("\u2713"), crayon::blue("jitter analysis"), crayon::green$underline$bold$italic("DONE"), "\n")
  jitter_time <- tictoc::toc(quiet = TRUE)
  
  # run profiles ----
  cat("\u231b", crayon::blue("working on profile analysis..."), "\n")
  tictoc::tic()
  
  ## define parameter specs for profiling ----
  # names for parameters to profile over (used as folder names)
  params <- c("r", "m", "m14", "q_twl", "q_ll", "q_ll_env")
  # define line numbers for params in ctl
  linenums = c(84, 56, 72, 136, 137, 146)
  # define parameter value vector
  if(isTRUE(full_run)){
    profilevec <- list(rec = seq(12.2, 13.8, by = 0.2),
                       m = seq(0.41, 0.57, by = 0.02),
                       m14 = seq(0.73, 0.89, by = 0.02),
                       q_twl = seq(0.17, 0.33, by = 0.02),
                       q_ll = seq(0.07, 0.23, by = 0.02),
                       q_ll_env = seq(0.84, 1, by = 0.02))
  } else{
    profilevec <- list(rec = c(12, 13, 14),
                       m = c(0.4, 0.5, 0.6),
                       m14 = c(0.7, 0.8, 0.9),
                       q_twl = c(0.2, 0.25, 0.3),
                       q_ll = c(0.1, 0.15, 0.2),
                       q_ll_env = c(0.8, 0.9, 1))
  }
  
  ##run profiles in parallel ----
  run_profile(mdl_dir = here::here(new_year, "mgmt", rec_mdl),
              res_dir = here::here(new_year, "output", "profile"),
              params = params,
              profilevec = profilevec,
              linenum = linenums,
              mod_ctl = rec_ctl,
              full_run = full_run)
  
  # print message when done
  cat(crayon::green$bold("\u2713"), crayon::blue("profile analysis"), crayon::green$underline$bold$italic("DONE"), "\n")
  prof_time <- tictoc::toc(quiet = TRUE)
  
  # run ll q analysis ----
  cat("\u231b", crayon::blue("working on ll q analysis..."), "\n")
  tictoc::tic()
  
  ## run model cases ----
  llq_res <- llq(dir = here::here(new_year, "mgmt", rec_mdl),
                 ctl_filename = rec_ctl,
                 full_run = full_run)
  
  ## save results ----
  if(isTRUE(full_run)){
    if (!dir.exists(here::here(new_year, "output", "llq"))) {
      dir.create(here::here(new_year, "output", "llq"), recursive = TRUE)
    }
    save(llq_res, file = here::here(new_year, "output", "llq", "llq_res.RData"))
  }
  
  # print message when done
  cat(crayon::green$bold("\u2713"), crayon::blue("ll q analysis"), crayon::green$underline$bold$italic("DONE"), "\n")
  llq_time <- tictoc::toc(quiet = TRUE)
  
  
  # run mcmc ----
  if(isTRUE(run_mcmc)){
    cat("\u231b", crayon::blue("working on mcmcs..."), "\n")
    
    ## define number of iterations ----
    if(isTRUE(full_run)){
      iter <- 350000
      thin <- 50
      warmup <- 250
    } else{
      iter <-5000
      thin <- 5
      warmup <- 250
    }
    
    ## set up model ----
    
    # set up folder
    if (!file.exists(here::here(new_year, "mgmt", rec_mdl, "mcmc"))) {
      dir.create(here::here(new_year, "mgmt", rec_mdl, "mcmc"), recursive = TRUE)
    }
    # copy model files
    R.utils::copyDirectory(here::here(new_year, "mgmt", rec_mdl), 
                           here::here(new_year, "mgmt", rec_mdl, "mcmc"), recursive = FALSE)
    
    # read starter file
    starter <- r4ss::SS_readstarter(here::here(new_year, "mgmt", rec_mdl, "mcmc", "starter.ss"),
                                    verbose = FALSE)
    # change init vals source
    starter$init_values_src <- 0
    # write modified starter file
    r4ss::SS_writestarter(starter, 
                          dir = here::here(new_year, "mgmt", rec_mdl, "mcmc"), 
                          overwrite = TRUE,
                          verbose = FALSE)
    
    ## run adnuts ----
    
    # start timer
    tictoc::tic()
    
    mcmc_adnut <- adnuts::sample_rwm(model = 'ss3',
                                     path = here::here(new_year, "mgmt", rec_mdl, "mcmc"),
                                     iter = iter,
                                     chains = 7,
                                     warmup = warmup,
                                     thin = thin,
                                     mceval = FALSE,
                                     control = list(metric = 'mle'),
                                     skip_optimization = FALSE,
                                     verbose = FALSE)
    
    # end timer
    mcmc_time <- tictoc::toc(quiet = TRUE)
    
    ## run mceval ----
    
    # start timer
    tictoc::tic()
    
    r4ss::run(dir = here::here(new_year, "mgmt", rec_mdl, "mcmc"),
              extras = "-mceval",
              skipfinished = FALSE,
              show_in_console = FALSE,
              verbose = FALSE)
    
    # end timer
    eval_time <- tictoc::toc(quiet = TRUE)
    
    ## save results ----
    if(isTRUE(full_run)){
      if (!dir.exists(here::here(new_year, "output", "mcmc"))) {
        dir.create(here::here(new_year, "output", "mcmc"), recursive = TRUE)
      }
      # Read output
      mcmc_eval <- r4ss::SSgetMCMC(here::here(new_year, "mgmt", rec_mdl, "mcmc"),
                                   verbose = FALSE)
      save(mcmc_adnut, file = here::here(new_year, "output", "mcmc", "mcmc_adnut.RData"))
      save(mcmc_eval, file = here::here(new_year, "output", "mcmc", "mcmc_eval.RData"))
    }
    cat(crayon::green$bold("\u2713"), crayon::blue("mcmc"), crayon::green$underline$bold$italic("DONE"), "\n")
  }

  # compute full run time ----
  tot_time <- round(((as.numeric(strsplit(mscen_time$callback_msg, split = " ")[[1]][1])) / 60) / 60 +
                      ((as.numeric(strsplit(retro_time$callback_msg, split = " ")[[1]][1])) / 60) / 60 +
                      ((as.numeric(strsplit(loo_yr_time$callback_msg, split = " ")[[1]][1])) / 60) / 60 +
                      ((as.numeric(strsplit(loo_dat_time$callback_msg, split = " ")[[1]][1])) / 60) / 60 +
                      ((as.numeric(strsplit(jitter_time$callback_msg, split = " ")[[1]][1])) / 60) / 60 +
                      ((as.numeric(strsplit(prof_time$callback_msg, split = " ")[[1]][1])) / 60) / 60 +
                      ((as.numeric(strsplit(llq_time$callback_msg, split = " ")[[1]][1])) / 60) / 60 +
                      ((as.numeric(strsplit(mcmc_time$callback_msg, split = " ")[[1]][1])) / 60) / 60 +
                      ((as.numeric(strsplit(eval_time$callback_msg, split = " ")[[1]][1])) / 60) / 60, digits = 1)
  if(!isTRUE(full_run)){
    # total test time
    cat("Test time took", crayon::red$bold$underline$italic(tot_time), "hours", "\u2693","\n")
    # estimated run time
    run_time <- round(((as.numeric(strsplit(mscen_time$callback_msg, split = " ")[[1]][1])) / 60) / 60 +
                       ((as.numeric(strsplit(retro_time$callback_msg, split = " ")[[1]][1]) / 2 * 11) / 60) / 60 +
                       ((as.numeric(strsplit(loo_yr_time$callback_msg, split = " ")[[1]][1]) / 2 * 11) / 60) / 60 +
                       ((as.numeric(strsplit(loo_dat_time$callback_msg, split = " ")[[1]][1])) / 60) / 60 +
                       ((as.numeric(strsplit(jitter_time$callback_msg, split = " ")[[1]][1]) * 10) / 60) / 60 +
                       ((as.numeric(strsplit(prof_time$callback_msg, split = " ")[[1]][1]) * 3) / 60) / 60 +
                       ((as.numeric(strsplit(llq_time$callback_msg, split = " ")[[1]][1]) * 10) / 60) / 60 +
                        ((as.numeric(strsplit(mcmc_time$callback_msg, split = " ")[[1]][1]) * 70) / 60) / 60 +
                        ((as.numeric(strsplit(eval_time$callback_msg, split = " ")[[1]][1]) * 70) / 60) / 60, digits = 1)
    cat("Full run will take", crayon::red$bold$underline$italic(run_time), "hours", "\u2693","\n")
  } else{
    cat("All", crayon::green$bold$underline$italic('Done'), "in", tot_time, "hours", "\u2693","\n")
  }
}