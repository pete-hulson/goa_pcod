#' function to run parameter profiles
#' developed in 2024 by p hulson
#' 
#' @param mdl_dir is the model directory (default = NULL)
#' @param res_dir is the results directory (default = NULL)
#' @param params parameters for which to perform profile (default = NULL)
#' @param profilevec vector of parameter values over which to profile (default = NULL)
#' @param linenum line number for parameter in ctl file (default = NULL)
#' @param mod_ctl model ctl name (default = NULL)
#' @param full_run boolean, if full_run = TRUE then write results (default = FALSE)
#' 
run_profile <- function(mdl_dir = NULL,
                        res_dir = NULL,
                        params = NULL,
                        profilevec = NULL,
                        linenum = NULL,
                        mod_ctl = NULL,
                        full_run = FALSE){

  ## get needed stuff ----
  # get model executable name
  exe_name <- ss3_exename(mdl_dir)
  # set up an index vector defining which year is being left-out
  idx = seq(1, length(params))
  
  ## run profiles ----
  # Get the number of available cores
  num_cores <- parallel::detectCores()
  if(num_cores > length(params)) num_cores = length(params)
  # Set the number of cores to be used for parallel computing
  doParallel::registerDoParallel(cores = num_cores)
  # run in parallel
  foreach::foreach(i = idx) %dopar% {

    # set up folder for profile
    R.utils::copyDirectory(mdl_dir, here::here(mdl_dir, "profile", params[i]), recursive = FALSE)

    # read starter file
    starter <- r4ss::SS_readstarter(here::here(mdl_dir, "profile", params[i], "starter.ss"))
    # change init vals source
    starter$init_values_src <- 0
    # write modified starter file
    r4ss::SS_writestarter(starter, 
                          dir = here::here(mdl_dir, "profile", params[i]), 
                          overwrite = TRUE)

    # run profile
    r4ss::profile(dir = here::here(mdl_dir, "profile", params[i]),
                  oldctlfile = mod_ctl,
                  newctlfile = mod_ctl,
                  linenum = linenum[i],
                  profilevec = profilevec[[i]])

    ## write results ----
    if(isTRUE(full_run)){
      # create results folder
      if (!dir.exists(res_dir)) {
        dir.create(res_dir, recursive = TRUE)
      }
      # get results
      res_prof <- r4ss::SSgetoutput(dirvec = here(mdl_dir, "profile", params[i]),
                                    keyvec = 1:length(profilevec[[i]]))
      summ_prof <- r4ss::SSsummarize(res_prof)
      # write results
      save(summ_prof, file = here::here(res_dir, paste0(params[i], "_prof.RData")))
    }
  }
  
  # Stop parallel computing
  doParallel::stopImplicitCluster()

}
