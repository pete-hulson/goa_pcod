#' function to run parameter profiles
#' developed in 2024 by p hulson
#' 
#' @param dir is the model directory (default = NULL)
#' @param mod_ctl model ctl name (default = NULL)
#' @param folder folder in which to perform profile (default = NULL)
#' @param profilevec vector of parameter values over which to profile (default = NULL)
#' @param linenum line number for parameter in ctl file (default = NULL)
#' 
run_profile <- function(dir = NULL,
                        mod_ctl = NULL,
                        folder = NULL,
                        profilevec = NULL,
                        linenum = NULL){
  
  # set up folder for profile
  R.utils::copyDirectory(dir, here::here(dir, "profile", folder), recursive = FALSE)
  
  
  # read starter file
  starter <- r4ss::SS_readstarter(here::here(dir, "profile", folder, "starter.ss"))
  # change init vals source
  starter$init_values_src <- 0
  # write modified starter file
  r4ss::SS_writestarter(starter, 
                        dir = here::here(dir, "profile", folder), 
                        overwrite = TRUE)
  
  
  # get length of profile vector
  Nprofile <- length(profilevec)
  
  # run profile
  r_prof <- r4ss::profile(dir = here::here(dir, "profile", folder),
                          oldctlfile = mod_ctl,
                          newctlfile = mod_ctl,
                          linenum = linenum,
                          profilevec = profilevec)
  
  
  # read the output files (with names like Report1.sso, Report2.sso, etc.)
  profile_res <- r4ss::SSgetoutput(dirvec = here::here(dir, "profile", folder), 
                                   keyvec = 1:Nprofile)
  
  # summarize output
  profilesummary <- r4ss::SSsummarize(profile_res)
  
  profilesummary
  
}
