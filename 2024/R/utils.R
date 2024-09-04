#' utility fcns
#' 
#' 
#' #' function to make 2024 changes to ctl file
#' @param asmnt_yr year of assessment (default = NULL)
#' @param folder root foloder containing models (default = NULL)
#' @param mdl name of model to run (default = NULL)
#' @param ctl_filename name of ctl file in which to adjust recruitment ramp parameters (default = NULL)
#' @param iters the number of iters to run for rec ramp (default = 2)
#' 
ctl_2024 <- function(asmnt_yr = NULL, 
                     folder = NULL,
                     mdl = NULL, 
                     ctl_filename = NULL){
  # read ctl file
  ctl <- r4ss::SS_readctl_3.30(here::here(asmnt_yr, folder, mdl, ctl_filename))
  
  # turn off forecast rec phase
  ctl$Fcast_recr_phase = -1

  # add prior to L_at_Amin (with CV = 5%)
  ctl$MG_parms$PR_SD[which(rownames(ctl$MG_parms) == 'L_at_Amin_Fem_GP_1')] = 0.3
  ctl$MG_parms$PR_type[which(rownames(ctl$MG_parms) == 'L_at_Amin_Fem_GP_1')] = 6
  ctl$MG_parms$PHASE[which(rownames(ctl$MG_parms) == 'L_at_Amin_Fem_GP_1')] = 1

  # add prior to descend_sd for twl survey
  ctl$size_selex_parms$PRIOR[which(rownames(ctl$size_selex_parms) == 'SizeSel_P_4_Srv(4)')] = 4
  ctl$size_selex_parms$PR_SD[which(rownames(ctl$size_selex_parms) == 'SizeSel_P_4_Srv(4)')] = 0.2
  ctl$size_selex_parms$PR_type[which(rownames(ctl$size_selex_parms) == 'SizeSel_P_4_Srv(4)')] = 0
  
  # fix trawl survey start_logit param
  ctl$size_selex_parms$INIT[which(rownames(ctl$size_selex_parms) == 'SizeSel_P_5_Srv(4)')] = -1012.5
  ctl$size_selex_parms$PHASE[which(rownames(ctl$size_selex_parms) == 'SizeSel_P_5_Srv(4)')] = -2
  ctl$size_selex_parms$Block[which(rownames(ctl$size_selex_parms) == 'SizeSel_P_5_Srv(4)')] = 0
  ctl$size_selex_parms$Block_Fxn[which(rownames(ctl$size_selex_parms) == 'SizeSel_P_5_Srv(4)')] = 0
  ctl$size_selex_parms_tv = ctl$size_selex_parms_tv[-which(rownames(ctl$size_selex_parms_tv) == 'SizeSel_P_5_Srv(4)_BLK1repl_1996'),]
  ctl$size_selex_parms_tv = ctl$size_selex_parms_tv[-which(rownames(ctl$size_selex_parms_tv) == 'SizeSel_P_5_Srv(4)_BLK1repl_2006'),]
  
  # write new ctl file
  r4ss::SS_writectl_3.30(ctllist = ctl,
                         outfile = here::here(asmnt_yr, folder, mdl, ctl_filename),
                         overwrite = TRUE)
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

  # set init values in starter file to 0
  mdl_starter <- r4ss::SS_readstarter(file = here::here(asmnt_yr, folder, mdl, "starter.ss"))
  mdl_starter$init_values_src = 0
  r4ss::SS_writestarter(mdl_starter, 
                        dir = here::here(asmnt_yr, folder, mdl),
                        overwrite = TRUE)
  
  # run model
  r4ss::run(dir = here::here(asmnt_yr, folder, mdl),
            skipfinished = FALSE,
            show_in_console = TRUE)
  
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
  mdl_res <- r4ss::SS_output(dir = here::here(asmnt_yr, folder, mdl))
  rec_ramp <- r4ss::SS_fitbiasramp(mdl_res)
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
            show_in_console = TRUE)
}
#' function to update ss3 model dat, ctl, and starter files
#' @param asmnt_yr year of assessment (default = NULL)
#' @param folder root foloder containing models (default = NULL)
#' @param mdl name of model to run (default = NULL)
#' @param dat_filename name of dat file  to update (default = NULL)
#' @param ctl_in name of ctl file in 'output' folder that has been updated (default = NULL)
#' @param ctl_out name of ctl file to be writtin in model folder (default = NULL)
#' 
update_ss3_files <- function(asmnt_yr = NULL, 
                             folder = NULL,
                             mdl = NULL, 
                             dat_filename = NULL,
                             ctl_in = NULL,
                             ctl_out){
  
  # update dat file
  # Remove previous dat files
  if(length(list.files(here::here(asmnt_yr, folder, mdl), pattern = "GOAPcod")) > 0) {
    file.remove(here::here(asmnt_yr, folder, mdl, list.files(here::here(asmnt_yr, folder, mdl), pattern = "GOAPcod")))
  }
  # copy new data file
  file.copy(here::here(asmnt_yr, 'output', dat_filename),
            here::here(asmnt_yr, folder, mdl, dat_filename))
  
  # update ctl file
  # Remove previous ctl files
  if(length(list.files(here::here(asmnt_yr, folder, mdl), pattern = ".ctl")) > 0) {
    file.remove(here::here(asmnt_yr, folder, mdl, list.files(here::here(asmnt_yr, folder, mdl), pattern = ".ctl")))
  }
  # copy new ctl file
  file.copy(here::here(asmnt_yr, 'output', ctl_in),
            here::here(asmnt_yr, folder, mdl, ctl_out))
  
  # set up starter file
  old_starter <- r4ss::SS_readstarter(file = here::here(asmnt_yr, folder, mdl, 'starter.ss'))
  old_starter$datfile <- dat_filename
  old_starter$ctlfile <- ctl_out
  old_starter$init_values_src = 0
  r4ss::SS_writestarter(mylist = old_starter,
                        dir = here::here(asmnt_yr, folder, mdl),
                        overwrite = TRUE)
  
}

#' function to set up folder with ss3 model files and exe
#' @param from folder containing ss3 files that are to be copied (default = NULL)
#' @param to destination folder for ss3 files (default = NULL)
#' 
start_ss_fldr <- function(from, to){
  # get model input files
  r4ss::copy_SS_inputs(dir.old = from, 
                       dir.new = to,
                       overwrite = TRUE)
  # get data_echo file so you can make changes to ctl file
  file.copy(paste0(from, '/data_echo.ss_new'),
            paste0(to, '/data_echo.ss_new'))
  # get exe
  r4ss::get_ss3_exe(dir = to)
}
#' function to get weight-length parameters (from survey data)
#' @param new_year year of assessment, to get data file (default = NULL)
#' 
wt_len <- function(new_year = NULL){
  
  # summarise weight data (mean & sd) by year and length
  summ_data <- vroom::vroom(here::here(new_year, 'data', 'raw', 'twl_srvy_age.csv')) %>% 
    tidytable::drop_na() %>% 
    tidytable::summarise(wt = mean(weight, na.rm = TRUE),
                         wt_sd = sd(weight, na.rm = TRUE),
                         n = .N,
                         .by = c(year, length)) %>% 
    tidytable::drop_na() %>% 
    tidytable::filter(wt_sd > 0)
  
  # define optimizing function
  ests <- function(pars, summ_data){
    summ_data %>% 
    tidytable::mutate(est_wt = pars[1] * length ^ pars[2],
                      rss = (wt - est_wt) ^ 2 / wt_sd) %>% 
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
