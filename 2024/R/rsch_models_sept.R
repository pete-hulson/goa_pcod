# Script to run 2023 GOA Pacific Cod Assessment analyses (P. Hulson)

# Load required packages & define parameters ----

# load necessary pkgs
libs <- c("data.table",
          "dplyr",
          "ggplot2",
          "magrittr", 
          "r4ss")

if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )]) > 0) {
  install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE)])}

lapply(libs, library, character.only = TRUE)

# assessment year
asmnt_yr <- as.numeric(format(Sys.Date(), format = "%Y"))

# source functions
# source_files <- list.files(here::here(new_dat_year, "R", "get_data"), "*.r$")
# purrr::map(here::here(new_dat_year, "R", "get_data", source_files), source)
source(here::here(asmnt_yr, "R", "utils.r"))

# run models? if not just get results
run_mdl = FALSE
run_retro = FALSE
# ret_yr <- 2 # For testing
ret_yr <- 10 # For full



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# base model (2023 model 2019.1b) ----
# read results from base model
base_mdl <- "2019.1b-2023-new_lw" # 2023 accepted model

base_res_23 <- r4ss::SS_output(dir = here::here(asmnt_yr - 1, 'mgmt', base_mdl),
                            verbose = TRUE,
                            printstats = TRUE)

# retro_base <- r4ss::SSsummarize(r4ss::SSgetoutput(dirvec = file.path(
#   here::here(asmnt_yr, 'rsch', base_mdl), "retrospectives",
#   paste("retro", 0:-ret_yr, sep = ""))))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# updated base model (2024 model 2019.1b) ----
base_mdl_update <- "2019.1b-2024"

## copy ss input files ----
start_ss_fldr(from = here::here(asmnt_yr - 1, 'mgmt', base_mdl),
              to = here::here(asmnt_yr, 'rsch', base_mdl_update))

# set up forecast file (with generic blocks for years so won't need to update in the future)
forecast <- r4ss::SS_readforecast(file = here::here(asmnt_yr, 'rsch', base_mdl_update, 'forecast.ss'))
forecast$Bmark_years <- c(-999, -2, -999, -2, -999, -1, -999, -2, -999, -2)
forecast$Fcast_years <- c(2000, -2, -5, -1, -999, -2)
r4ss::SS_writeforecast(mylist = forecast,
                       dir = here::here(asmnt_yr, 'rsch', base_mdl_update),
                       overwrite = TRUE)

## update files ----
update_ss3_files(asmnt_yr, 
                 folder = 'rsch',
                 mdl = "2019.1b-2024", 
                 dat_filename = "GOAPcod2024Aug13_old.dat",
                 ctl_in = "Model19_1b.ctl",
                 ctl_out = "Model19_1b.ctl")

## run model ----
run_ss3_model(asmnt_yr, 
              folder = 'rsch',
              mdl = base_mdl_update,
              ctl_filename = "Model19_1b.ctl")


# read the model output
update_base_res <- r4ss::SS_output(dir = here::here(asmnt_yr, 'rsch', base_mdl_update))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# new base model ----
new_base <- "2019.1c-2024"

## copy ss input files ----
start_ss_fldr(from = here::here(asmnt_yr, 'rsch', base_mdl_update),
              to = here::here(asmnt_yr, 'rsch', new_base))

## update files ----
update_ss3_files(asmnt_yr, 
                 folder = 'rsch',
                 mdl = "2019.1c-2024", 
                 dat_filename = "GOAPcod2024Aug13.dat",
                 ctl_in = "Model19_1b.ctl",
                 ctl_out = "Model19_1c.ctl")

## run model ----
run_ss3_model(asmnt_yr, 
              folder = 'rsch',
              mdl = new_base,
              ctl_filename = "Model19_1c.ctl")


# read the model output
new_base_res <- r4ss::SS_output(dir = here::here(asmnt_yr, 'rsch', new_base))


























































# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# minsamplesize corr (model 2019.1b) ----

# copy over the stock synthesis model files to the new directory for model 2019.1b
# start_ss_fldr(from = here::here(asmnt_yr, 'rsch', base_mdl),
#               to = here::here(asmnt_yr, 'rsch', new_base))

if(isTRUE(run_mdl)){
  
  # correct minsamplesize in dat file
  dat <- r4ss::SS_readdat(here::here(asmnt_yr, 'rsch', base_mdl, list.files(here::here(asmnt_yr, 'rsch', base_mdl), pattern = '.dat')))
  
  dat$age_info$minsamplesize <- 0.01
  
  r4ss::SS_writedat(datlist = dat, 
                    outfile = here::here(asmnt_yr, 'rsch', new_base, list.files(here::here(asmnt_yr, 'rsch', new_base), pattern = '.dat')),
                    overwrite = TRUE)
  
  # run model
  run_ss_model(asmnt_yr, new_base)
}

# read the model output and print diagnostic messages
new_base_res <- r4ss::SS_output(dir = here::here(asmnt_yr, 'rsch', new_base),
                                verbose = TRUE,
                                printstats = TRUE)

# run retro
if(isTRUE(run_retro)){
  r4ss::SS_doRetro(masterdir = here::here(asmnt_yr, 'rsch', new_base),
                   oldsubdir = "",
                   newsubdir = "retrospectives",
                   years = 0:-ret_yr)
}

# get retro results
retro_new_base <- r4ss::SSsummarize(r4ss::SSgetoutput(dirvec = file.path(
  here::here(asmnt_yr, 'rsch', new_base), "retrospectives",
  paste("retro", 0:-ret_yr, sep = ""))))

vroom::vroom_write(retro_new_base$likelihoods, here::here(asmnt_yr, 'rsch', 'output', 'retro_new_base_likes.csv'), delim = ",")
base::save(retro_new_base, file = here::here(asmnt_yr, 'rsch', 'output', 'retro_new_base.RData'))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ll survey w/o env link (2019.1c) ----

# copy over the stock synthesis model files to the new directory for model 2019.1c
# start_ss_fldr(from = here::here(asmnt_yr, 'rsch', new_base),
#               to = here::here(asmnt_yr, 'rsch', new_base_noll))

# run model
if(isTRUE(run_mdl)){
  run_ss_model(asmnt_yr, mdl = new_base_noll)
}

# read the model output and print diagnostic messages
new_base_noll_res <- r4ss::SS_output(dir = here::here(asmnt_yr, 'rsch', new_base_noll),
                                     verbose = TRUE,
                                     printstats = TRUE)

if(isTRUE(run_retro)){
  r4ss::SS_doRetro(masterdir = here::here(asmnt_yr, 'rsch', new_base_noll),
                   oldsubdir = "",
                   newsubdir = "retrospectives",
                   years = 0:-ret_yr)
}

# get retro results
retro_new_base_noll <- r4ss::SSsummarize(r4ss::SSgetoutput(dirvec = file.path(
  here::here(asmnt_yr, 'rsch', new_base_noll), "retrospectives",
  paste("retro", 0:-ret_yr, sep = ""))))

vroom::vroom_write(retro_new_base_noll$likelihoods, here::here(asmnt_yr, 'rsch', 'output', 'retro_new_base_noll_likes.csv'), delim = ",")
base::save(retro_new_base_noll, file = here::here(asmnt_yr, 'rsch', 'output', 'retro_new_base_noll.RData'))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# llq new env link model (model 2019.1d) ----

# copy over the stock synthesis model files to the new directory for model 2019.1b
# start_ss_fldr(from = here::here(asmnt_yr, 'rsch', new_base),
#               to = here::here(asmnt_yr, 'rsch', new_base_llq))

if(isTRUE(run_mdl)){
  # read in env data
  cfsr <- vroom::vroom(here::here(asmnt_yr, 'data', 'raw_cfsr.csv')) %>% 
    dplyr::rename_with(., tolower) %>% 
    tidytable::rename(l0_20 = '0_20',
                      l20_40 = '20_40',
                      l40_60 = '40_60',
                      l60_80 = '60_80',
                      l80plus = '80plus')
  
  # read in ss data
  ss_dat <- r4ss::SS_readdat(here::here(asmnt_yr, 'rsch', new_base_llq, list.files(here::here(asmnt_yr, 'rsch', new_base_llq), pattern = '.dat')))
  
  cfsr %>% 
    tidytable::pivot_longer(cols = c(l0_20, l20_40, l40_60, l60_80, l80plus)) %>% 
    tidytable::filter(month == 3 & name == 'l40_60') %>% 
    tidytable::mutate(Value = value - as.numeric(cfsr %>% 
                                                   tidytable::pivot_longer(cols = c(l0_20, l20_40, l40_60, l60_80, l80plus)) %>% 
                                                   tidytable::filter(month == 3 & name == 'l40_60' & year %in% 1982:2012) %>% 
                                                   tidytable::summarise(mean(value))),
                      Variable = 1) %>% 
    tidytable::rename(Yr = year) %>% 
    tidytable::select(Yr, Variable, Value) -> new_env1
  
  new_env1 %>% 
    tidytable::bind_rows(ss_dat$envdat %>% 
                           tidytable::filter(Variable != 1)) -> new_env
  
  ss_dat$envdat <- as.data.frame(new_env)
  r4ss::SS_writedat(datlist = ss_dat, 
                    outfile = here::here(asmnt_yr, 'rsch', new_base_llq, list.files(here::here(asmnt_yr, 'rsch', new_base_llq), pattern = '.dat')),
                    overwrite = TRUE)
  
  # run model
  run_ss_model(asmnt_yr, mdl = new_mdl2)
  
}


# read the model output and print diagnostic messages
new_base_llq_res <- r4ss::SS_output(dir = here::here(asmnt_yr, 'rsch', new_base_llq),
                                    verbose = TRUE,
                                    printstats = TRUE)


# run retro
if(isTRUE(run_retro)){
  r4ss::SS_doRetro(masterdir = here::here(asmnt_yr, 'rsch', new_base_llq),
                   oldsubdir = "",
                   newsubdir = "retrospectives",
                   years = 0:-ret_yr)
}

# get retro results
retro_new_base_llq <- r4ss::SSsummarize(r4ss::SSgetoutput(dirvec = file.path(
  here::here(asmnt_yr, 'rsch', new_base_llq), "retrospectives",
  paste("retro", 0:-ret_yr, sep = ""))))

vroom::vroom_write(retro_new_base_llq$likelihoods, here::here(asmnt_yr, 'rsch', 'output', 'retro_new_base_llq_likes.csv'), delim = ",")
base::save(retro_new_base_llq, file = here::here(asmnt_yr, 'rsch', 'output', 'retro_new_base_llq.RData'))


# run management scenarios function
source(here::here(asmnt_yr, "R", "assessment", "run_mngmnt_scenarios.r"))
mscen_new_base_llq <- Do_AK_Scenarios(Model_name = new_base_llq,
                                      Model_dir = here::here(asmnt_yr, 'rsch', new_base_llq),
                                      CYR = asmnt_yr,
                                      SYR = 1977,
                                      FCASTY = 15,
                                      FLEETS = c(1:3),
                                      do_fig = TRUE,
                                      SEXES = 1)

# save output
write.csv(mscen_new_base_llq$Two_year, here::here(asmnt_yr, 'rsch', 'output', 'mgmnt_exec_summ_new_base_llq.csv'))




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# growth env link model (model 2023.1) ----

# copy over the stock synthesis model files to the new directory for model 2019.1b
# start_ss_fldr(from = here::here(asmnt_yr, 'rsch', new_base),
#               to = here::here(asmnt_yr, 'rsch', new_mdl2))

if(isTRUE(run_mdl)){
  # read in env data
  cfsr <- vroom::vroom(here::here(asmnt_yr, 'data', 'raw_cfsr.csv')) %>% 
    dplyr::rename_with(., tolower) %>% 
    tidytable::rename(l0_20 = '0_20',
                      l20_40 = '20_40',
                      l40_60 = '40_60',
                      l60_80 = '60_80',
                      l80plus = '80plus')
  
  # read in ss data
  ss_dat <- r4ss::SS_readdat(here::here(asmnt_yr, 'rsch', new_mdl1, list.files(here::here(asmnt_yr, 'rsch', new_mdl1), pattern = '.dat')))
  
  # kappa env link
  cfsr %>% 
    tidytable::pivot_longer(cols = c(l0_20, l20_40, l40_60, l60_80, l80plus)) %>% 
    tidytable::filter(month == 1 & name == 'l60_80') %>% 
    tidytable::mutate(Value = value - as.numeric(cfsr %>% 
                                                   tidytable::pivot_longer(cols = c(l0_20, l20_40, l40_60, l60_80, l80plus)) %>% 
                                                   tidytable::filter(month == 1 & name == 'l60_80' & year %in% 1982:2012) %>% 
                                                   tidytable::summarise(mean(value))),
                      Variable = 2) %>% 
    tidytable::rename(Yr = year) %>% 
    tidytable::select(Yr, Variable, Value) -> kappa_env
  
  # linf env link
  cfsr %>% 
    tidytable::pivot_longer(cols = c(l0_20, l20_40, l40_60, l60_80, l80plus)) %>% 
    tidytable::filter(month == 1 & name == 'l20_40') %>% 
    tidytable::mutate(Value = value - as.numeric(cfsr %>% 
                                                   tidytable::pivot_longer(cols = c(l0_20, l20_40, l40_60, l60_80, l80plus)) %>% 
                                                   tidytable::filter(month == 1 & name == 'l20_40' & year %in% 1982:2012) %>% 
                                                   tidytable::summarise(mean(value))),
                      Variable = 3) %>% 
    tidytable::rename(Yr = year) %>% 
    tidytable::select(Yr, Variable, Value) -> linf_env
  
  # lzero env link
  tidytable::bind_cols(c(1977, 1978), c(4, 4), c(1, 1)) %>% 
    tidytable::rename(Yr = '...1',
                      Variable = '...2',
                      Value = '...3') %>% 
    tidytable::bind_rows(cfsr %>% 
                           tidytable::pivot_longer(cols = c(l0_20, l20_40, l40_60, l60_80, l80plus)) %>% 
                           tidytable::filter(month == 10 & name == 'l0_20') %>% 
                           tidytable::mutate(index = value - as.numeric(cfsr %>% 
                                                                          tidytable::pivot_longer(cols = c(l0_20, l20_40, l40_60, l60_80, l80plus)) %>% 
                                                                          tidytable::filter(month == 10 & name == 'l0_20' & year %in% 1982:2012) %>% 
                                                                          tidytable::summarise(mean(value)))) %>% 
                           tidytable::mutate(Value = growth_L0(index, as.numeric(cfsr %>% 
                                                                                   tidytable::pivot_longer(cols = c(l0_20, l20_40, l40_60, l60_80, l80plus)) %>% 
                                                                                   tidytable::filter(month == 10 & name == 'l0_20' & year %in% 1982:2012) %>% 
                                                                                   tidytable::summarise(mean(value)))),
                                             Variable = 4) %>% 
                           tidytable::rename(Yr = year) %>% 
                           tidytable::select(Yr, Variable, Value)) -> lzero_env
  
  #put it all together
  ss_dat$envdat %>% 
    tidytable::filter(Variable == 1) %>% 
    tidytable::bind_rows(kappa_env) %>%
    tidytable::bind_rows(linf_env) %>%
    tidytable::bind_rows(lzero_env) -> new_env
  
  # write ss datafile
  ss_dat$envdat <- as.data.frame(new_env)
  r4ss::SS_writedat(datlist = ss_dat, 
                    outfile = here::here(asmnt_yr, 'rsch', new_mdl1, 'all_best', list.files(here::here(asmnt_yr, 'rsch', new_mdl1), pattern = '.dat')),
                    overwrite = TRUE)
  
  # add env link to ctl file - something doesn't work here, change it manually
  
  # ctl <- r4ss::SS_readctl(here::here(asmnt_yr, 'rsch', base_mdl, list.files(here::here(asmnt_yr, 'rsch', base_mdl), pattern = 'ctl')),
  #                         datlist = here::here(asmnt_yr, 'rsch', base_mdl, list.files(here::here(asmnt_yr, 'rsch', base_mdl), pattern = 'dat')[2]))
  # 
  # ctl$MG_parms$`env_var&link`[2:3] = 101
  # 
  # r4ss::SS_writectl(ctllist = ctl,
  #                   outfile = here::here(asmnt_yr, 'rsch', new_base, list.files(here::here(asmnt_yr, 'rsch', new_mdl1), pattern = 'ctl')),
  #                   overwrite = TRUE)
  
  # run model
  run_ss_model(asmnt_yr, mdl = new_mdl1)
}

# read the model output and print diagnostic messages
mdl1_res <- r4ss::SS_output(dir = here::here(asmnt_yr, 'rsch', new_mdl1),
                            verbose = TRUE,
                            printstats = TRUE)

# run retro
if(isTRUE(run_retro)){
  r4ss::SS_doRetro(masterdir = here::here(asmnt_yr, 'rsch', new_mdl1),
                   oldsubdir = "",
                   newsubdir = "retrospectives",
                   years = 0:-ret_yr)
}

# get retro results
retro_mdl1 <- r4ss::SSsummarize(r4ss::SSgetoutput(dirvec = file.path(
  here::here(asmnt_yr, 'rsch', new_mdl1), "retrospectives",
  paste("retro", 0:-ret_yr, sep = ""))))

vroom::vroom_write(retro_mdl1$likelihoods, here::here(asmnt_yr, 'rsch', 'output', 'retro_mdl1_likes.csv'), delim = ",")
base::save(retro_mdl1, file = here::here(asmnt_yr, 'rsch', 'output', 'retro_mdl1.RData'))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# growth and llq env link model (model 2023.2) ----

# copy over the stock synthesis model files to the new directory for model 2019.1b
# start_ss_fldr(from = here::here(asmnt_yr, 'rsch', new_mdl1),
#               to = here::here(asmnt_yr, 'rsch', new_mdl2))

if(isTRUE(run_mdl)){
  # read in env data
  cfsr <- vroom::vroom(here::here(asmnt_yr, 'data', 'raw_cfsr.csv')) %>% 
    dplyr::rename_with(., tolower) %>% 
    tidytable::rename(l0_20 = '0_20',
                      l20_40 = '20_40',
                      l40_60 = '40_60',
                      l60_80 = '60_80',
                      l80plus = '80plus')
  
  # read in ss data
  ss_dat <- r4ss::SS_readdat(here::here(asmnt_yr, 'rsch', new_mdl2, list.files(here::here(asmnt_yr, 'rsch', new_mdl2), pattern = '.dat')))
  
  cfsr %>% 
    tidytable::pivot_longer(cols = c(l0_20, l20_40, l40_60, l60_80, l80plus)) %>% 
    tidytable::filter(month == 3 & name == 'l40_60') %>% 
    tidytable::mutate(Value = value - as.numeric(cfsr %>% 
                                                   tidytable::pivot_longer(cols = c(l0_20, l20_40, l40_60, l60_80, l80plus)) %>% 
                                                   tidytable::filter(month == 3 & name == 'l40_60' & year %in% 1982:2012) %>% 
                                                   tidytable::summarise(mean(value))),
                      Variable = 1) %>% 
    tidytable::rename(Yr = year) %>% 
    tidytable::select(Yr, Variable, Value) -> new_env1
  
  new_env1 %>% 
    tidytable::bind_rows(ss_dat$envdat %>% 
                           tidytable::filter(Variable != 1)) -> new_env
  
  ss_dat$envdat <- as.data.frame(new_env)
  r4ss::SS_writedat(datlist = ss_dat, 
                    outfile = here::here(asmnt_yr, 'rsch', new_mdl2, list.files(here::here(asmnt_yr, 'rsch', new_mdl2), pattern = '.dat')),
                    overwrite = TRUE)
  
  # run model
  run_ss_model(asmnt_yr, mdl = new_mdl2)
  
}


# read the model output and print diagnostic messages
mdl2_res <- r4ss::SS_output(dir = here::here(asmnt_yr, 'rsch', new_mdl2),
                            verbose = TRUE,
                            printstats = TRUE)


# run retro
if(isTRUE(run_retro)){
  r4ss::SS_doRetro(masterdir = here::here(asmnt_yr, 'rsch', new_mdl2),
                   oldsubdir = "",
                   newsubdir = "retrospectives",
                   years = 0:-ret_yr)
}

# get retro results
retro_mdl2 <- r4ss::SSsummarize(r4ss::SSgetoutput(dirvec = file.path(
  here::here(asmnt_yr, 'rsch', new_mdl2), "retrospectives",
  paste("retro", 0:-ret_yr, sep = ""))))

vroom::vroom_write(retro_mdl2$likelihoods, here::here(asmnt_yr, 'rsch', 'output', 'retro_mdl2_likes.csv'), delim = ",")
base::save(retro_mdl2, file = here::here(asmnt_yr, 'rsch', 'output', 'retro_mdl2.RData'))




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# plots, results and stuff ----


base_mdl <- "2019.1a-2022" # 2022 accepted model
new_base <- "2019.1b-2022" # 2022 model with minsamplesize correction
new_base_noll <- "2019.1c-2022" # 2022 model with no llq env link
new_mdl1 <- "2023.1-2022" # 2022 model with env growth link
new_mdl2 <- "2023.2-2022" # 2022 model with env growth link and pref llq env link


# 2019.1a
r4ss::SS_plots(base_res,
               printfolder = "",
               dir = here::here(asmnt_yr, 'rsch', base_mdl, "plots"))

# 2019.1b
r4ss::SS_plots(new_base_res,
               printfolder = "",
               dir = here::here(asmnt_yr, 'rsch', new_base, "plots"))

# 2019.1c
r4ss::SS_plots(new_base_noll,
               printfolder = "",
               dir = here::here(asmnt_yr, 'rsch', new_base_noll, "plots"))

# 2023.1
r4ss::SS_plots(mdl1_res,
               printfolder = "",
               dir = here::here(asmnt_yr, 'rsch', new_mdl1, "plots"))

# 2023.2
r4ss::SS_plots(mdl2_res,
               printfolder = "",
               dir = here::here(asmnt_yr, 'rsch', new_mdl2, "plots"))

# plot 2019.1a vs 2019.1b for new base
base_summ <- r4ss::SSsummarize(list(base_res, new_base_res))

r4ss::SSplotComparisons(base_summ,
                        print = TRUE,
                        legendlabels = c(base_mdl, new_base),
                        plotdir = here::here(asmnt_yr, 'rsch', 'output', 'plots', 'new_base'))

vroom::vroom_write(base_summ$likelihoods, here::here(asmnt_yr, 'rsch', 'output', 'plots', 'new_base', 'base_summ_likes.csv'), delim = ",")
vroom::vroom_write(base_summ$likelihoods_by_fleet, here::here(asmnt_yr, 'rsch', 'output', 'plots', 'new_base', 'base_summ_likes_by_fleet.csv'), delim = ",")

# plot 2019.1a vs 2019.1b vs 2019.1d
llq_summ <- r4ss::SSsummarize(list(base_res, new_base_res, new_base_llq_res))

r4ss::SSplotComparisons(llq_summ,
                        print = TRUE,
                        pch = "",
                        legendlabels = c(base_mdl, new_base, new_base_llq),
                        plotdir = here::here(asmnt_yr, 'rsch', 'output', 'plots', 'llq'))

vroom::vroom_write(llq_summ$likelihoods, here::here(asmnt_yr, 'rsch', 'output', 'plots', 'llq', 'llq_summ_likes.csv'), delim = ",")
vroom::vroom_write(llq_summ$likelihoods_by_fleet, here::here(asmnt_yr, 'rsch', 'output', 'plots', 'llq', 'llq_summ_likes_by_fleet.csv'), delim = ",")

# plot 2019.1b vs 2019.1c
llq_noel_summ <- r4ss::SSsummarize(list(new_base_res, new_base_noll_res))

r4ss::SSplotComparisons(llq_noel_summ,
                        print = TRUE,
                        pch = "",
                        legendlabels = c(new_base, new_base_noll),
                        plotdir = here::here(asmnt_yr, 'rsch', 'output', 'plots', 'llq_no_el'))

rstats_new_base <- r4ss::SSmohnsrho(retro_new_base)
rstats_new_base_noll <- r4ss::SSmohnsrho(retro_new_base_noll)
rstats_new_base_llq <- r4ss::SSmohnsrho(retro_new_base_llq)

rstats_new_base$AFSC_Hurtado_SSB
rstats_new_base_noll$AFSC_Hurtado_SSB
rstats_new_base_llq$AFSC_Hurtado_SSB


# make statistics table for 2019.1b vs 2019.1c vs 2019.1d
llq_summ$likelihoods %>% 
  tidytable::filter(Label == 'TOTAL') %>%
  tidytable::select(-Label) %>% 
  tidytable::pivot_longer(names_to = 'model', values_to = 'tot_like') %>% 
  tidytable::mutate(model = c(new_base, new_base_noll, new_base_llq)) %>% 
  tidytable::bind_cols(dplyr::as_tibble(llq_summ$npars)) %>% 
  tidytable::rename(npars = 'value') %>% 
  tidytable::mutate(aic = 2 * npars + 2* tot_like) %>% 
  vroom::vroom_write(., here::here(asmnt_yr, 'rsch', 'output', 'plots', 'llq', 'aic_tbl.csv'), delim = ",")
















# plot 2019.1b vs 2023.1
grwth_summ <- r4ss::SSsummarize(list(new_base_res, mdl1_res))

r4ss::SSplotComparisons(grwth_summ,
                        print = TRUE,
                        pch = "",
                        legendlabels = c(new_base, new_mdl1),
                        plotdir = here::here(asmnt_yr, 'rsch', 'output', 'plots', 'grwth'))

vroom::vroom_write(grwth_summ$likelihoods, here::here(asmnt_yr, 'rsch', 'output', 'plots', 'grwth', 'grwth_summ_likes.csv'), delim = ",")
vroom::vroom_write(grwth_summ$likelihoods_by_fleet, here::here(asmnt_yr, 'rsch', 'output', 'plots', 'grwth', 'grwth_summ_likes_by_fleet.csv'), delim = ",")


# make statistics table for 2019.1b vs 2023.1
grwth_summ$likelihoods %>% 
  tidytable::filter(Label == 'TOTAL') %>%
  tidytable::select(-Label) %>% 
  tidytable::pivot_longer(names_to = 'model', values_to = 'tot_like') %>% 
  tidytable::mutate(model = c(new_base, new_mdl1)) %>% 
  tidytable::bind_cols(dplyr::as_tibble(grwth_summ$npars)) %>% 
  tidytable::rename(npars = 'value') %>% 
  tidytable::mutate(aic = 2 * npars + 2* tot_like) %>% 
  vroom::vroom_write(., here::here(asmnt_yr, 'rsch', 'output', 'plots', 'grwth', 'aic_tbl.csv'), delim = ",")

# do retro comp for 2019.1b vs 2023.1

retro_new_base$likelihoods %>% 
  tidytable::filter(Label == 'TOTAL') %>% 
  tidytable::mutate(model = new_base) %>% 
  tidytable::bind_rows(retro_mdl1$likelihoods %>% 
                         tidytable::filter(Label == 'TOTAL') %>% 
                         tidytable::mutate(model = new_mdl1)) %>% 
  vroom::vroom_write(., here::here(asmnt_yr, 'rsch', 'output', 'plots', 'grwth', 'retro_like_tbl.csv'), delim = ",")

r4ss::SSmohnsrho(retro_new_base)
r4ss::SSmohnsrho(retro_mdl1)







# plot 2019.1b vs 2019.1c vs 2023.2
llq_summ <- r4ss::SSsummarize(list(new_base_res, new_base_noll_res, mdl2_res))

r4ss::SSplotComparisons(llq_summ,
                        print = TRUE,
                        legendlabels = c(new_base, new_base_noll, new_mdl2),
                        plotdir = here::here(asmnt_yr, 'rsch', 'output', 'plots', 'llq'))















# do retro comp for  2019.1b vs 2019.1c vs 2023.2

retro_new_base$likelihoods %>% 
  tidytable::filter(Label == 'TOTAL') %>% 
  tidytable::mutate(model = new_base) %>% 
  tidytable::bind_rows(retro_mdl1$likelihoods %>% 
                         tidytable::filter(Label == 'TOTAL') %>% 
                         tidytable::mutate(model = new_mdl1)) %>% 
  vroom::vroom_write(., here::here(asmnt_yr, 'rsch', 'output', 'plots', 'grwth', 'retro_like_tbl.csv'), delim = ",")






retro_new_base$pars %>% 
  tidytable::select(Label, replist2) %>% 
  tidytable::filter(replist2 != 0 & is.na(replist2) == FALSE) %>% 
  tidytable::mutate(strs = as.character(format(replist2, scientific = F))) %>% 
  print(n = 230)


get_p <- function(retro){
  retro_new_base$parsSD %>% 
    tidytable::select(retro) %>% 
    tidytable::rename(pars = retro) %>% 
    tidytable::filter(is.na(pars) == FALSE & pars != 1 & pars != 0.44) %>% 
    tidytable::summarise(p = length(pars))
}

get_p('replist1')
get_p('replist2')

retro = 'replist1'




countDecimalPlaces <- function(x) {
  if ((x %% 1) != 0) {
    strs <- strsplit(as.character(format(x, scientific = F)), "\\.")
    n <- nchar(strs[[1]][2])
  } else {
    n <- 0
  }
  return(n) 
}


ss3diags::SSplotModelcomp(all_summ)



# rho retro
r4ss::SSmohnsrho(retro_base)
r4ss::SSmohnsrho(retro_mdl1)
r4ss::SSmohnsrho(retro_mdl2)



all_summ <- r4ss::SSsummarize(list(base_res, mdl1_res, mdl2_best_res))

names(all_summ)

all_summ$SpawnBio

all_summ$likelihoods %>% 
  tidytable::filter(Label == 'TOTAL') %>%
  tidytable::select(-Label) %>% 
  tidytable::pivot_longer(names_to = 'model', values_to = 'tot_like') %>% 
  tidytable::mutate(model = c(base_mdl, new_base, new_mdl2)) %>% 
  tidytable::bind_cols(dplyr::as_tibble(all_summ$npars)) %>% 
  tidytable::rename(npars = 'value') %>% 
  tidytable::mutate(aic = 2 * npars + 2* tot_like)


r4ss::SSplotComparisons(all_summ)

ss3diags::SSplotModelcomp(all_summ)





all_res <- r4ss::SSgetoutput(dirvec = c(here::here(asmnt_yr, 'rsch', base_mdl), here::here(asmnt_yr, 'rsch', new_base)))


base_res$likelihoods_by_fleet
mdl1_res$likelihoods_by_fleet


base_res$likelihoods_used
mdl1_res$likelihoods_used




dat$MeanSize_at_Age_obs

list.files(here::here(asmnt_yr, 'rsch', new_base), pattern = 'dat')

