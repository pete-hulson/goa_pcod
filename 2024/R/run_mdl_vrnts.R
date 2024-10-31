#' function to run model variants tested
#' developed in 2024 by p hulson
#' 
#' @param mdls vector of model variant names (default = NULL)
#' @param new_year current assessment year (default = NULL)
#' @param base_mdl previous year final accepted model (default = NULL)
#' @param dats vector of names of dat files (default = NULL)
#' @param ctls vector of names of ctl files (default = NULL)
#' 
run_mdl_vrnts <- function(mdls = NULL,
                          new_year = NULL,
                          base_mdl = NULL,
                          dats = NULL,
                          ctls = NULL){
  
  
  # start timer ----
  cat("\u231b", crayon::blue("working on model variant runs..."), "\n")
  tictoc::tic()
  
  # source functions ----
  source(here::here(new_year, "R", "utils.R"), local = TRUE)
  
  # get needed stuff ----
  # set up an index vector defining which data is being left-out
  indx = seq(1, length(mdls))
  # Get the number of available cores
  num_cores <- parallel::detectCores()
  if(num_cores > length(mdls)) num_cores = length(mdls)
  # Set the number of cores to be used for parallel computing
  doParallel::registerDoParallel(cores = num_cores)
  
  # run model variants ----
  foreach::foreach(i = indx) %dopar% {
    
    # copy ss input files
    if(!file.exists(here::here(new_year, 'mgmt', mdls[i], 'ss3.exe'))){
      start_ss_fldr(from = here::here(new_year - 1, 'mgmt', base_mdl),
                    to = here::here(new_year, 'mgmt', mdls[i]))
    }
    
    # update files
    update_ss3_files(new_year, 
                     folder = 'mgmt',
                     mdl = mdls[i], 
                     dat_filename = dats[i],
                     ctl_filename = ctls[i])
    
    # run model
    run_ss3_model(new_year, 
                  folder = 'mgmt',
                  mdl = mdls[i],
                  ctl_filename = ctls[i])
    
  }
  
  # Stop parallel computing
  doParallel::stopImplicitCluster()
  
  # end timer
  cat(crayon::green$bold("\u2713"), crayon::blue("model variant runs"), crayon::green$underline$bold$italic("DONE"), "\n")
  mdl_time <- tictoc::toc(quiet = TRUE)
  cat("Model variant runs took", crayon::red$bold$underline$italic(round(((as.numeric(strsplit(mdl_time$callback_msg, split = " ")[[1]][1])) / 60), digits = 1)), "minutes", "\u2693","\n")
  
}