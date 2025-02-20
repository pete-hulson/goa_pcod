#' utility fcns
#' 
#' 
#' function to get systems ss3 exe name
#' @param supported_os supported operting systems (default = c("Windows", "Darwin"))
#' @param executables list of possible ss3 exe names by operating system (default = list("Windows" = c("ss.exe", "ss3.exe"), "Darwin" = c("ss_osx")))
#' 
ss3_exename <- function(dir,
                        supported_os = c("Windows", "Darwin"), 
                        executables = list("Windows" = c("ss.exe", "ss3.exe"), "Darwin" = c("ss_osx"))){
  
  # Get the operating system information
  os_info <- Sys.info()

  # figure out the ss3 executable name to run
  # list the possible exes
  executables <- list("Windows" = c("ss.exe", "ss3.exe"), "Darwin" = c("ss_osx"))
  # set the exe name
  if (os_info["sysname"] %in% names(executables)) {
    # Get the list of executables for the detected operating system
    os_executables <- executables[[os_info["sysname"]]]
    # Check if any of the executables exist
    existing_executables <- os_executables[file.exists(here::here(dir, os_executables))]
    if (length(existing_executables) > 0) {
      # set the name of the executable
      exe_name = existing_executables[1]
    } else {
      print("No executable found for the operating system.")
      stop()
    }
  }
  
  exe_name

}

#' function to run ss3 model with recruitment bias ramp adjustment
#' @param asmnt_yr year of assessment (default = NULL)
#' @param folder root foloder containing models (default = NULL)
#' @param mdl name of model to run (default = NULL)
#' @param ctl_filename name of ctl file in which to adjust recruitment ramp parameters (default = NULL)
#' @param iters the number of iters to run for rec ramp (default = 2)
#' 
run_ss3_model <- function(asmnt_yr = NULL, 
                          folder = NULL,
                          mdl = NULL, 
                          ctl_filename = NULL,
                          iters = 2){

  # load fcns
  source(here::here(asmnt_yr, "R", "utils.R"), local = TRUE)
  
  # set init values in starter file to 0
  mdl_starter <- r4ss::SS_readstarter(file = here::here(asmnt_yr, folder, mdl, "starter.ss"))
  mdl_starter$init_values_src = 0
  r4ss::SS_writestarter(mdl_starter, 
                        dir = here::here(asmnt_yr, folder, mdl),
                        overwrite = TRUE)
  
  # run model
  r4ss::run(dir = here::here(asmnt_yr, folder, mdl),
            skipfinished = FALSE,
            show_in_console = FALSE,
            verbose = FALSE)
  
  # now, iterate model iters times to settle on recruitment bias ramp
  purrr::map(1:iters, ~get_recr_ramp(asmnt_yr, folder, mdl, ctl_filename))
  
  # set init vals to use par file in starter file for further runs
  mdl_starter <- r4ss::SS_readstarter(file = here::here(asmnt_yr, folder, mdl, "starter.ss"))
  mdl_starter$init_values_src = 1
  r4ss::SS_writestarter(mdl_starter, 
                        dir = here::here(asmnt_yr, folder, mdl),
                        overwrite = TRUE)
}

# function to get recruitment ramp
#' @param asmnt_yr year of assessment (default = NULL)
#' @param folder root foloder containing models (default = NULL)
#' @param mdl name of model to run (default = NULL)
#' @param ctl_filename name of ctl file  to update (default = NULL)
#' 
get_recr_ramp <- function(asmnt_yr, folder, mdl, ctl_filename){
  # update recruitment bias ramp ests in ctl file
  mdl_res <- r4ss::SS_output(dir = here::here(asmnt_yr, folder, mdl),
                             verbose = FALSE,
                             printstats = FALSE)
  rec_ramp <- r4ss::SS_fitbiasramp(mdl_res,
                                   plot = FALSE)
  ctl <- r4ss::SS_readctl_3.30(here::here(asmnt_yr, folder, mdl, ctl_filename))
  ctl$last_early_yr_nobias_adj <- rec_ramp$newbias$par[1]
  ctl$first_yr_fullbias_adj <- rec_ramp$newbias$par[2]
  ctl$last_yr_fullbias_adj <- rec_ramp$newbias$par[3]
  ctl$first_recent_yr_nobias_adj <- rec_ramp$newbias$par[4]
  ctl$max_bias_adj <- rec_ramp$newbias$par[5]
  r4ss::SS_writectl_3.30(ctllist = ctl,
                         outfile = here::here(asmnt_yr, folder, mdl, ctl_filename),
                         overwrite = TRUE)
  # run model
  r4ss::run(dir = here::here(asmnt_yr, folder, mdl),
            skipfinished = FALSE,
            show_in_console = FALSE,
            verbose = FALSE)
}

#' function to update ss3 model dat, ctl, and starter files
#' @param asmnt_yr year of assessment (default = NULL)
#' @param folder root foloder containing models (default = NULL)
#' @param mdl name of model to run (default = NULL)
#' @param dat_filename name of dat file  to update (default = NULL)
#' @param ctl_filename name of ctl file in 'output' folder that has been updated (default = NULL)
#' 
update_ss3_files <- function(asmnt_yr = NULL, 
                             folder = NULL,
                             mdl = NULL, 
                             dat_filename = NULL,
                             ctl_filename = NULL){
  
  # update dat file
  # Remove previous dat files
  if(length(list.files(here::here(asmnt_yr, folder, mdl), pattern = "GOAPcod")) > 0) {
    file.remove(here::here(asmnt_yr, folder, mdl, list.files(here::here(asmnt_yr, folder, mdl), pattern = "GOAPcod")))
  }
  # copy new data file
  file.copy(here::here(asmnt_yr, 'output', 'mdl_input', dat_filename),
            here::here(asmnt_yr, folder, mdl, dat_filename))
  
  # update ctl file
  # Remove previous ctl files
  if(length(list.files(here::here(asmnt_yr, folder, mdl), pattern = ".ctl")) > 0) {
    file.remove(here::here(asmnt_yr, folder, mdl, list.files(here::here(asmnt_yr, folder, mdl), pattern = ".ctl")))
  }
  # copy new ctl file
  file.copy(here::here(asmnt_yr, 'output', 'mdl_input', ctl_filename),
            here::here(asmnt_yr, folder, mdl, ctl_filename))
  
  # set up starter file
  old_starter <- r4ss::SS_readstarter(file = here::here(asmnt_yr, folder, mdl, 'starter.ss'))
  old_starter$datfile <- dat_filename
  old_starter$ctlfile <- ctl_filename
  old_starter$init_values_src = 0
  r4ss::SS_writestarter(mylist = old_starter,
                        dir = here::here(asmnt_yr, folder, mdl),
                        overwrite = TRUE)
  
  # set up forecast file
  forecast <- r4ss::SS_readforecast(file = here::here(asmnt_yr, folder, mdl, 'forecast.ss'))
  # set generic blocks for benchmark/forecast years so won't need to update in the future
  forecast$Bmark_years <- c(-999, -2, -999, -2, -999, -1, -999, -2, -999, -2)
  forecast$Fcast_years <- c(2000, -2, -5, -1, -999, -2)
  # change forecast recruitment option
  forecast$fcast_rec_option <- 4
  r4ss::SS_writeforecast(mylist = forecast,
                         dir = here::here(asmnt_yr, folder, mdl),
                         overwrite = TRUE)
  
}

#' function to set up folder with ss3 model files and exe
#' @param from folder containing ss3 files that are to be copied (default = NULL)
#' @param to destination folder for ss3 files (default = NULL)
#' 
start_ss_fldr <- function(from, to){
  
  # set up folder
  if(!dir.exists(to)){
    dir.create(to, recursive = TRUE)
  }
  # get model input files
  r4ss::copy_SS_inputs(dir.old = from, 
                       dir.new = to,
                       overwrite = TRUE,
                       verbose = FALSE)
  # get data_echo file so you can make changes to ctl file
  file.copy(paste0(from, '/data_echo.ss_new'),
            paste0(to, '/data_echo.ss_new'))
  # get exe
  invisible(r4ss::get_ss3_exe(dir = to))
  
}
#' function to get weight-length parameters (from survey data)
#' @param new_year year of assessment, to get data file (default = NULL)
#' 
wt_len <- function(new_year = NULL){
  
  # summarise weight data (mean & sd) by year and length
  summ_data <- vroom::vroom(here::here(new_year, 'data', 'raw', 'twl_srvy_age.csv')) %>% 
    tidytable::select(year, length, weight) %>% 
    tidytable::bind_rows(vroom::vroom(here::here(new_year, 'data', 'raw', 'beachseine_lw.csv'))) %>% 
    tidytable::drop_na() %>% 
    tidytable::summarise(wt = mean(weight, na.rm = TRUE),
                         n = .N,
                         .by = c(year, length))

  # define optimizing function
  ests <- function(pars, summ_data){
    summ_data %>% 
    tidytable::mutate(est_wt = pars[1] * length ^ pars[2],
                      rss = n * (wt - est_wt) ^ 2) %>% 
    tidytable::summarise(obj = sum(rss))
  }

  # fit model
  fit <- stats::optim(par = c(3e-06, 3), 
                      fn = ests,
                      summ_data = summ_data)

  # return weight-length parms
  fit$par
  
}

#' function to bin length data to custom length bins 
#' @param data length data to bin (default = NULL)
#' @param bins user-defined length bins (default = NULL)
#' 
get_bin <- function(data = NULL,
                    bins = NULL){
  
  # custom length bins, convention follows ss3 binning
  # set up bin bounds
  tidytable::tidytable(lwr = c(0, bins)) %>% 
    tidytable::mutate(label = tidytable::case_when(lwr != 0 ~ lwr,
                                                   lwr == 0 ~ bins[1])) -> bin_bnds
  # determine which bin length is in
  data %>% 
    tidytable::mutate(new_length = bin_bnds$label[max(which(bin_bnds$lwr < length))], 
                      .by = c(length))

}

#' function to format survey length comp data for ss3 data file
#' @param data data to format for ss3 (default = NULL)
#' @param ss3_args arguments for ss3 data file (i.e., fltsrv, gender, etc; default = NULL)
#' @param iss switch for whether iss is time-varying or constant (default = NULL)
#' @param nsamp input sample size (default = NULL)
#' 
ss3_len_com <- function(data = NULL, 
                        ss3_args = NULL, 
                        iss = NULL, 
                        nsamp = NULL){
  
  data %>% 
    tidytable::mutate(seas = ss3_args[1],
                      fltsrv = ss3_args[2],
                      gender = ss3_args[3],
                      part = ss3_args[4]) -> lcomp_part
  
  # switch if input sample size time-dependent (e.g., from surveyISS package) or constant
  if(isTRUE(iss)){
    lcomp_part %>% 
      tidytable::left_join(nsamp) %>% 
      tidytable::mutate(length = paste0('l', length),
                        lencomp = round(lencomp, digits = 5)) %>% 
      tidytable::pivot_wider(names_from = length, values_from = lencomp) -> ss3_lcomp
  } else{
    lcomp_part %>% 
      tidytable::mutate(nsamp = nsamp,
                        length = paste0('l', length),
                        lencomp = round(lencomp, digits = 5)) %>% 
      tidytable::pivot_wider(names_from = length, values_from = lencomp) -> ss3_lcomp
  }
  
  data.frame(ss3_lcomp)
}

#' function to format fishery length comp data for ss3 data file
#' @param data data to format for ss3 (default = NULL)
#' @param ss3_args arguments for ss3 data file (i.e., fltsrv, gender, etc; default = NULL)
#' @param nsamp input sample size (default = NULL)
#' 
ss3_len_com_fsh <- function(data = NULL, 
                            ss3_args = NULL, 
                            nsamp = NULL){
  
  data %>% 
    tidytable::mutate(seas = ss3_args[1],
                      fltsrv = tidytable::case_when(gear == 'trawl' ~ 1,
                                                    gear == 'longline' ~ 2,
                                                    gear == 'pot' ~ 3),
                      gender = ss3_args[2],
                      part = ss3_args[3]) %>% 
    tidytable::left_join(nsamp) %>% 
    tidytable::select(-gear) %>% 
    tidytable::mutate(length = paste0('l', length),
                      lencomp = round(lencomp, digits = 5)) %>% 
    tidytable::pivot_wider(names_from = length, values_from = lencomp) %>% 
    tidytable::arrange(fltsrv) -> ss3_lcomp
  
  data.frame(ss3_lcomp)
}

#' function to format survey age comp data for ss3 data file
#' @param data data to format for ss3 (default = NULL)
#' @param ss3_args arguments for ss3 data file (i.e., fltsrv, gender, etc; default = NULL)
#' @param iss switch for whether iss is time-varying or constant (default = NULL)
#' @param nsamp input sample size (default = NULL)
#' 
ss3_age_com <- function(data = NULL, 
                        ss3_args = NULL, 
                        iss = NULL, 
                        nsamp = NULL){
  data %>% 
    tidytable::mutate(seas = ss3_args[1],
                      fltsrv = ss3_args[2],
                      gender = ss3_args[3],
                      part = ss3_args[4],
                      ageerr = ss3_args[5],
                      lgin_lo = ss3_args[6],
                      lgin_hi = ss3_args[7]) -> acomp_part
  
  # switch if input sample size time-dependent (e.g., from surveyISS package) or constant
  if(isTRUE(iss)){
    acomp_part %>% 
      tidytable::left_join(nsamp) %>% 
      tidytable::mutate(age = paste0('a', age),
                        agecomp = round(agecomp, digits = 5)) %>% 
      tidytable::pivot_wider(names_from = age, values_from = agecomp) -> ss3_acomp
  } else{
    acomp_part %>% 
      tidytable::mutate(nsamp = nsamp,
                        age = paste0('a', age),
                        agecomp = round(agecomp, digits = 5)) %>% 
      tidytable::pivot_wider(names_from = age, values_from = agecomp) -> ss3_acomp
  }
  
  data.frame(ss3_acomp)
}

#' function to format fishery age comp data for ss3 data file
#' @param data data to format for ss3 (default = NULL)
#' @param ss3_args arguments for ss3 data file (i.e., fltsrv, gender, etc; default = NULL)
#' @param max_age max age for data (default = 10)
#' @param iss switch for whether iss is time-varying or constant (default = NULL)
#' @param nsamp input sample size (default = NULL)
#' @param fit switch for whether fishery age data is fit (default = FALSE)
#' 
ss3_age_com_fsh <- function(data = NULL,
                            ss3_args = NULL,
                            max_age = 10, 
                            iss = NULL, 
                            nsamp = NULL,
                            fit = FALSE){
  
  data %>% 
    tidytable::mutate(seas = ss3_args[1],
                      fltsrv = tidytable::case_when(gear == 'trawl' ~ 1,
                                                    gear == 'longline' ~ 2,
                                                    gear == 'pot' ~ 3),
                      gender = ss3_args[2],
                      part = ss3_args[3],
                      ageerr = ss3_args[4],
                      lgin_lo = ss3_args[5],
                      lgin_hi = ss3_args[6],
                      age = tidytable::case_when(age > max_age ~ max_age,
                                                 .default = age)) %>% 
    tidytable::summarise(agecomp = sum(agecomp), .by = c(year, seas, fltsrv, gender, part, ageerr, lgin_lo, lgin_hi, age)) -> acomp_part
  
  # test if input sample size constant or read-in (e.g., from surveyISS package)
  if(isTRUE(iss)){
    acomp_part %>% 
      tidytable::left_join(nsamp %>% 
                             tidytable::mutate(fltsrv = tidytable::case_when(gear == 'trawl' ~ 1,
                                                                             gear == 'longline' ~ 2,
                                                                             gear == 'pot' ~ 3)) %>% 
                             tidytable::select(year, fltsrv, nsamp)) %>% 
      tidytable::mutate(age = paste0('a', age),
                        agecomp = round(agecomp, digits = 5)) %>% 
      tidytable::pivot_wider(names_from = age, values_from = agecomp) %>% 
      tidytable::arrange(fltsrv, year) -> ss3_acomp
  } else{
    acomp_part %>% 
      tidytable::mutate(nsamp = nsamp,
                        age = paste0('a', age),
                        agecomp = round(agecomp, digits = 5)) %>% 
      tidytable::pivot_wider(names_from = age, values_from = agecomp) -> ss3_acomp
  }
  
  if(!isTRUE(fit)){
    ss3_acomp %>% 
      tidytable::mutate(fltsrv = fltsrv * -1) -> ss3_acomp
  }
  
  data.frame(ss3_acomp)
}

#' function to format survey conditional age-at-length data for ss3 data file
#' @param data data to format for ss3 (default = NULL)
#' @param ss3_args arguments for ss3 data file (i.e., fltsrv, gender, etc; default = NULL)
#' @param nsamp input sample size (default = NULL)
#' 
ss3_caal <- function(data = NULL, 
                     ss3_args = NULL, 
                     nsamp = NULL){
  data.frame(data %>% 
               tidytable::mutate(seas = ss3_args[1],
                                 fltsrv = ss3_args[2],
                                 gender = ss3_args[3],
                                 part = ss3_args[4],
                                 ageerr = ss3_args[5],
                                 lgin_lo = length,
                                 lgin_hi = length) %>% 
               tidytable::left_join(nsamp) %>% 
               tidytable::select(-length) %>% 
               tidytable::mutate(age = paste0('a', age),
                                 caal = round(caal, digits = 5)) %>% 
               tidytable::pivot_wider(names_from = age, values_from = caal))
}

#' function to format fishery conditional age-at-length data for ss3 data file
#' @param data data to format for ss3 (default = NULL)
#' @param ss3_args arguments for ss3 data file (i.e., fltsrv, gender, etc; default = NULL)
#' @param nsamp input sample size (default = NULL)
#' 
ss3_caal_fsh <- function(data = NULL,
                         ss3_args = NULL,
                         nsamp = NULL){
  data.frame(data %>% 
               tidytable::mutate(seas = ss3_args[1],
                                 fltsrv = tidytable::case_when(gear == 'trawl' ~ 1,
                                                               gear == 'longline' ~ 2,
                                                               gear == 'pot' ~ 3),
                                 gender = ss3_args[2],
                                 part = ss3_args[3],
                                 ageerr = ss3_args[4],
                                 lgin_lo = length,
                                 lgin_hi = length) %>% 
               tidytable::left_join(nsamp) %>% 
               tidytable::select(-gear, -length) %>% 
               tidytable::mutate(age = paste0('a', age),
                                 caal = round(caal, digits = 5)) %>% 
               tidytable::pivot_wider(names_from = age, values_from = caal) %>% 
               tidytable::arrange(fltsrv, year))
}

#' function to format growth data for ss3 data file
#' @param data data to format for ss3 (default = NULL)
#' @param ss3_args arguments for ss3 data file (i.e., fltsrv, gender, etc; default = NULL)
#' 
ss3_grwth <- function(data = NULL,
                      ss3_args = NULL){
  data.frame(data %>% 
               tidytable::mutate(seas = ss3_args[1],
                                 fltsrv = ss3_args[2],
                                 gender = ss3_args[3],
                                 part = ss3_args[4],
                                 ageerr = ss3_args[5],
                                 ignore = ss3_args[6],
                                 age = paste0('a', age)) %>% 
               tidytable::select(-nsamp) %>% 
               tidytable::pivot_wider(names_from = age, values_from = mean) %>% 
               tidytable::left_join(data %>% 
                                      tidytable::mutate(age = paste0('n_a', age)) %>% 
                                      tidytable::select(-mean) %>% 
                                      tidytable::pivot_wider(names_from = age, values_from = nsamp)))
}

#' function to format environmental link data for ss3 data file
#' @param data data to format for ss3 (default = NULL)
#' @param var variable column for ss3 data file (default = NULL)
#' 
ss3_envlnk <- function(data = NULL,
                       var = NULL){
  data.frame(data %>% 
               tidytable::mutate(variable = var) %>% 
               tidytable::select(year, variable, index))
}
