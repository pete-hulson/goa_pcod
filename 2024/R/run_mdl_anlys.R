#' function to run model analyses
#' developed in 2024 by p hulson
#' 
#' @param new_year current assessment year (default = NULL)
#' @param base_mdl previous year final accepted model (default = NULL)
#' @param rec_mdl author recommended model (default = NULL)
#' @param full_run boolean, whether a full analysis is to be conducted (default = NULL)
#' @param rec_ctl name recommended model ctl file (default = NULL)
#' @param run_mcmcz boolean, whether to run mcmc or not (default = FALSE)
#' 
run_mdl_anlys <- function(new_year = NULL,
                          base_mdl = NULL,
                          rec_mdl = NULL,
                          full_run = NULL,
                          rec_ctl = NULL,
                          run_mcmcz = FALSE){
  
  # load functions ----
  source_files <- c(list.files(here::here(new_year, "R"), pattern = "*.r$"),
                    list.files(here::here(new_year, "R"), pattern = "*.R$"))
  purrr::map(1:length(source_files), ~ source(here::here(new_year, "R", source_files[.]), local = TRUE))

  # run management scenarios ----
  cat("\u231b", crayon::blue("working on management scenarios..."), "\n")
  tictoc::tic()
  
  # base model
  run_ss3_mgmnt_scen(dir = here::here(new_year, "mgmt", base_mdl),
                     cyr = new_year,
                     output_name = "mgmnt_scen_base")
  
  # recommended model
  run_ss3_mgmnt_scen(dir = here::here(new_year, "mgmt", rec_mdl),
                     cyr = new_year,
                     output_name = "mgmnt_scen_rec")
  
  # print message when done
  cat(crayon::green$bold("\u2713"), crayon::blue("management scenarios"), crayon::green$underline$bold$italic("DONE"), "\n")
  mscen_time <- tictoc::toc(quiet = TRUE)
  
  # run F at prev year ofl ----
  cat("\u231b", crayon::blue("working on f with previous year catch @ ofl..."), "\n")
  tictoc::tic()
  
  run_fofl_prev(new_year = new_year,
                rec_mdl = rec_mdl)
  
  # print message when done
  cat(crayon::green$bold("\u2713"), crayon::blue("f with previous year catch @ ofl"), crayon::green$underline$bold$italic("DONE"), "\n")
  fofl_time <- tictoc::toc(quiet = TRUE)
  
  
  # run retrospective analysis ----
  cat("\u231b", crayon::blue("working on retrospective analysis..."), "\n")
  tictoc::tic()
  
  run_retro(new_year = new_year,
            full_run = full_run,
            base_mdl = base_mdl,
            rec_mdl = rec_mdl)
  
  # print message when done
  cat(crayon::green$bold("\u2713"), crayon::blue("retrospective analysis"), crayon::green$underline$bold$italic("DONE"), "\n")
  retro_time <- tictoc::toc(quiet = TRUE)
  
  # run leave-one-out analysis ----
  cat("\u231b", crayon::blue("working on leave-one-out analysis..."), "\n")
  tictoc::tic()
  
  run_loo(full_run = full_run,
          dir = here::here(new_year, "mgmt", rec_mdl),
          cyr = new_year)
  
  # print message when done
  cat(crayon::green$bold("\u2713"), crayon::blue("leave-one-out analysis"), crayon::green$underline$bold$italic("DONE"), "\n")
  loo_time <- tictoc::toc(quiet = TRUE)
  
  # run add-one-in analysis ----
  cat("\u231b", crayon::blue("working on add-one-in analysis..."), "\n")
  tictoc::tic()
  
  run_aoi(dir = here::here(new_year, "mgmt", rec_mdl),
          cyr = new_year)
  
  
  # print message when done
  cat(crayon::green$bold("\u2713"), crayon::blue("add-one-in analysis"), crayon::green$underline$bold$italic("DONE"), "\n")
  aoi_time <- tictoc::toc(quiet = TRUE)
  
  # run jitter ----
  cat("\u231b", crayon::blue("working on jitter analysis..."), "\n")
  tictoc::tic()
  
  run_jitter(full_run = full_run,
             new_year = new_year,
             rec_mdl = rec_mdl)
  
  # print message when done
  cat(crayon::green$bold("\u2713"), crayon::blue("jitter analysis"), crayon::green$underline$bold$italic("DONE"), "\n")
  jitter_time <- tictoc::toc(quiet = TRUE)
  
  # run profiles ----
  cat("\u231b", crayon::blue("working on profile analysis..."), "\n")
  tictoc::tic()
  
  # define parameter specs for profiling
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
                       q_ll_env = seq(0.76, 1.08, by = 0.04))
  } else{
    profilevec <- list(rec = c(12, 13, 14),
                       m = c(0.4, 0.5, 0.6),
                       m14 = c(0.7, 0.8, 0.9),
                       q_twl = c(0.2, 0.25, 0.3),
                       q_ll = c(0.1, 0.15, 0.2),
                       q_ll_env = c(0.8, 0.9, 1))
  }
  
  # run profiles in parallel
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
  
  run_llq(dir = here::here(new_year, "mgmt", rec_mdl),
          ctl_filename = rec_ctl,
          full_run = full_run)
  
  # print message when done
  cat(crayon::green$bold("\u2713"), crayon::blue("ll q analysis"), crayon::green$underline$bold$italic("DONE"), "\n")
  llq_time <- tictoc::toc(quiet = TRUE)
  
  # run apportionment ----
  cat("\u231b", crayon::blue("working on apportionment..."), "\n")
  tictoc::tic()
  
  run_apport(new_year = new_year)
  
  # print message when done
  cat(crayon::green$bold("\u2713"), crayon::blue("apportionment..."), crayon::green$underline$bold$italic("DONE"), "\n")
  apport_time <- tictoc::toc(quiet = TRUE)

  # run mcmc ----
  if(isTRUE(run_mcmcz)){
    cat("\u231b", crayon::blue("working on mcmcs..."), "\n")
    
    mcmc_time <- run_mcmc(full_run = full_run,
                          new_year = new_year,
                          rec_mdl = rec_mdl)
    
    # print message when done
    cat(crayon::green$bold("\u2713"), crayon::blue("mcmc"), crayon::green$underline$bold$italic("DONE"), "\n")
  }
  
  # compute full run time ----
  tot_time <- round(((as.numeric(strsplit(mscen_time$callback_msg, split = " ")[[1]][1])) / 60) / 60 +
                      ((as.numeric(strsplit(retro_time$callback_msg, split = " ")[[1]][1])) / 60) / 60 +
                      ((as.numeric(strsplit(loo_time$callback_msg, split = " ")[[1]][1])) / 60) / 60 +
                      ((as.numeric(strsplit(aoi_time$callback_msg, split = " ")[[1]][1])) / 60) / 60 +
                      ((as.numeric(strsplit(jitter_time$callback_msg, split = " ")[[1]][1])) / 60) / 60 +
                      ((as.numeric(strsplit(prof_time$callback_msg, split = " ")[[1]][1])) / 60) / 60 +
                      ((as.numeric(strsplit(llq_time$callback_msg, split = " ")[[1]][1])) / 60) / 60 +
                      ((as.numeric(strsplit(mcmc_time$nuts_time$callback_msg, split = " ")[[1]][1])) / 60) / 60 +
                      ((as.numeric(strsplit(mcmc_time$eval_time$callback_msg, split = " ")[[1]][1])) / 60) / 60, digits = 1)
  if(!isTRUE(full_run)){
    # total test time
    cat("Test time took", crayon::red$bold$underline$italic(tot_time), "hours", "\u2693","\n")
    # estimated run time
    run_time <- round(((as.numeric(strsplit(mscen_time$callback_msg, split = " ")[[1]][1])) / 60) / 60 +
                        ((as.numeric(strsplit(retro_time$callback_msg, split = " ")[[1]][1]) / 2 * 11) / 60) / 60 +
                        ((as.numeric(strsplit(loo_time$callback_msg, split = " ")[[1]][1]) / 2 * 11) / 60) / 60 +
                        ((as.numeric(strsplit(aoi_time$callback_msg, split = " ")[[1]][1])) / 60) / 60 +
                        ((as.numeric(strsplit(jitter_time$callback_msg, split = " ")[[1]][1]) * 10) / 60) / 60 +
                        ((as.numeric(strsplit(prof_time$callback_msg, split = " ")[[1]][1]) * 3) / 60) / 60 +
                        ((as.numeric(strsplit(llq_time$callback_msg, split = " ")[[1]][1]) * 10) / 60) / 60 +
                        ((as.numeric(strsplit(mcmc_time$nuts_time$callback_msg, split = " ")[[1]][1]) * 70) / 60) / 60 +
                        ((as.numeric(strsplit(mcmc_time$eval_time$callback_msg, split = " ")[[1]][1])) / 60) / 60, digits = 1)
    cat("Full run will take", crayon::red$bold$underline$italic(run_time), "hours", "\u2693","\n")
  } else{
    cat("All", crayon::green$bold$underline$italic('Done'), "in", tot_time, "hours", "\u2693","\n")
  }
}