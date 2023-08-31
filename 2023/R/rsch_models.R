# Script to run 2023 GOA Pacific Cod Assessment analyses (P. Hulson)

# Load required packages & define parameters ----

libs <- c("data.table",
          "dplyr",
          "ggplot2",
          "magrittr", 
          "r4ss")

if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )]) > 0) {
  install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE)])}

lapply(libs, library, character.only = TRUE)

# model names
base_mdl <- "2019.1a-2022" # 2022 accepted model
new_mdl1 <- "2019.1b-2022" # 2022 model with minsamplesize correction
new_mdl2 <- "2023.1-2022" # 2022 model with env growth link

# assessment year
asmnt_yr <- as.numeric(format(Sys.Date(), format = "%Y"))

# helper fcns ----
start_ss_fldr <- function(from, to){
  r4ss::copy_SS_inputs(dir.old = from, 
                       dir.new = to,
                       overwrite = TRUE)
  base::file.copy(from = paste0(from, "/ss.exe"),
                  to = paste0(to, "/ss.exe"),
                  overwrite = TRUE)
}

growth_L0 <- function(data, T){
  exp(0.2494 + 0.3216 * (T + data) - 0.0069 * (T + data) ^ 2 - 0.0004 * (T + data) ^ 3) / exp(0.2494 + 0.3216*(T)-0.0069 * (T) ^ 2 - 0.0004 * (T) ^ 3)
}

run_ss_model <- function(asmnt_yr, mdl){
  # if par file doesn't exist then run without initial conditions, otherwise, use par file
  if(base::file.exists(here::here(asmnt_yr, 'rsch', mdl, "ss.par"))){
    mdl_starter <- r4ss::SS_readstarter(file = here::here(asmnt_yr, 'rsch', mdl, "starter.ss"))
    mdl_starter$init_values_src = 1
    r4ss::SS_writestarter(mdl_starter, 
                          dir = here::here(asmnt_yr, 'rsch', mdl),
                          overwrite = TRUE)
    
    r4ss::run_SS_models(dirvec = here::here(asmnt_yr, 'rsch', mdl),
                        skipfinished = FALSE,
                        intern = TRUE)
  } else{
    mdl_starter <- r4ss::SS_readstarter(file = here::here(asmnt_yr, 'rsch', mdl, "starter.ss"))
    mdl_starter$init_values_src = 0
    r4ss::SS_writestarter(mdl_starter, 
                          dir = here::here(asmnt_yr, 'rsch', mdl),
                          overwrite = TRUE)
    
    r4ss::run_SS_models(dirvec = here::here(asmnt_yr, 'rsch', mdl),
                        skipfinished = FALSE,
                        intern = TRUE)
    
    mdl_starter$init_values_src = 1
    r4ss::SS_writestarter(mdl_starter, 
                          dir = here::here(asmnt_yr, 'rsch', mdl),
                          overwrite = TRUE)
  }
}

# base model (model 2019.1a) ----
# read results from base model
base_res <- r4ss::SS_output(dir = here::here(asmnt_yr, 'rsch', base_mdl),
                            verbose = TRUE,
                            printstats = TRUE)


# minsamplesize corr (model 2019.1b) ----

# copy over the stock synthesis model files to the new directory for model 2019.1b
start_ss_fldr(from = here::here(asmnt_yr, 'rsch', base_mdl),
              to = here::here(asmnt_yr, 'rsch', new_mdl1))

# correct minsamplesize in dat file
dat <- r4ss::SS_readdat(here::here(asmnt_yr, 'rsch', base_mdl, list.files(here::here(asmnt_yr, 'rsch', base_mdl), pattern = '.dat')))

dat$age_info$minsamplesize <- 0.01

r4ss::SS_writedat(datlist = dat, 
                  outfile = here::here(asmnt_yr, 'rsch', new_mdl1, list.files(here::here(asmnt_yr, 'rsch', new_mdl1), pattern = '.dat')),
                  overwrite = TRUE)

# run model
run_ss_model(asmnt_yr, new_mdl1)

# read the model output and print diagnostic messages
mdl1_res <- r4ss::SS_output(dir = here::here(asmnt_yr, 'rsch', new_mdl1),
                            verbose = TRUE,
                            printstats = TRUE)


# growth models (model 2023.1) ----

# copy over the stock synthesis model files to the new directory for model 2019.1b
# start_ss_fldr(from = here::here(asmnt_yr, 'rsch', new_mdl2),
#               to = here::here(asmnt_yr, 'rsch', new_mdl2, "all_best"))
# start_ss_fldr(from = here::here(asmnt_yr, 'rsch', new_mdl2),
#               to = here::here(asmnt_yr, 'rsch', new_mdl2, "all_seas"))


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


# run model for best month

# kappa
cfsr %>% 
  tidytable::pivot_longer(cols = c(l0_20, l20_40, l40_60, l60_80, l80plus)) %>% 
  tidytable::filter(month == 1 & name == 'l0_20') %>% 
  tidytable::mutate(Value = value - as.numeric(cfsr %>% 
                                                 tidytable::pivot_longer(cols = c(l0_20, l20_40, l40_60, l60_80, l80plus)) %>% 
                                                 tidytable::filter(month == 1 & name == 'l0_20' & year %in% 1982:2012) %>% 
                                                 tidytable::summarise(mean(value))),
                    Variable = 1) %>% 
  tidytable::rename(Yr = year) %>% 
  tidytable::select(Yr, Variable, Value) -> new_env1

# linf
cfsr %>% 
  tidytable::pivot_longer(cols = c(l0_20, l20_40, l40_60, l60_80, l80plus)) %>% 
  tidytable::filter(month == 1 & name == 'l20_40') %>% 
  tidytable::mutate(Value = value - as.numeric(cfsr %>% 
                                                 tidytable::pivot_longer(cols = c(l0_20, l20_40, l40_60, l60_80, l80plus)) %>% 
                                                 tidytable::filter(month == 1 & name == 'l20_40' & year %in% 1982:2012) %>% 
                                                 tidytable::summarise(mean(value))),
                    Variable = 2) %>% 
  tidytable::rename(Yr = year) %>% 
  tidytable::select(Yr, Variable, Value) -> new_env2

# lzero
tidytable::bind_cols(c(1977, 1978), c(3, 3), c(1, 1)) %>% 
  tidytable::rename(Yr = '...1',
                    Variable = '...2',
                    Value = '...3') %>% 
  tidytable::bind_rows(cfsr %>% 
                         tidytable::pivot_longer(cols = c(l0_20, l20_40, l40_60, l60_80, l80plus)) %>% 
                         tidytable::filter(month == 6 & name == 'l0_20') %>% 
                         tidytable::mutate(index = value - as.numeric(cfsr %>% 
                                                                        tidytable::pivot_longer(cols = c(l0_20, l20_40, l40_60, l60_80, l80plus)) %>% 
                                                                        tidytable::filter(month == 6 & name == 'l0_20' & year %in% 1982:2012) %>% 
                                                                        tidytable::summarise(mean(value)))) %>% 
                         tidytable::mutate(Value = growth_L0(index, as.numeric(cfsr %>% 
                                                                                 tidytable::pivot_longer(cols = c(l0_20, l20_40, l40_60, l60_80, l80plus)) %>% 
                                                                                 tidytable::filter(month == 6 & name == 'l0_20' & year %in% 1982:2012) %>% 
                                                                                 tidytable::summarise(mean(value)))),
                                           Variable = 3) %>% 
                         tidytable::rename(Yr = year) %>% 
                         tidytable::select(Yr, Variable, Value)) -> new_env3

#put it all together
new_env1 %>%
  tidytable::bind_rows(new_env2) %>%
  tidytable::bind_rows(new_env3) -> new_env

# write ss datafile
ss_dat$envdat <- as.data.frame(new_env)
r4ss::SS_writedat(datlist = ss_dat, 
                  outfile = here::here(asmnt_yr, 'rsch', new_mdl2, 'all_best', list.files(here::here(asmnt_yr, 'rsch', new_mdl2, 'all_best'), pattern = '.dat')),
                  overwrite = TRUE)

# run model
run_ss_model(asmnt_yr, mdl = paste0(new_mdl2, "/all_best"))

# read the model output and print diagnostic messages
mdl2_best_res <- r4ss::SS_output(dir = here::here(asmnt_yr, 'rsch', new_mdl2, "all_best"),
                                 verbose = TRUE,
                                 printstats = TRUE)





# run model for best season

# linf and kappa
cfsr %>% 
  tidytable::pivot_longer(cols = c(l0_20, l20_40, l40_60, l60_80, l80plus)) %>% 
  tidytable::filter(month %in% 1:3 & name == 'l20_40') %>% 
  tidytable::summarise(value = mean(value), .by = c(year)) %>% 
  tidytable::mutate(Value = value - as.numeric(cfsr %>% 
                                                 tidytable::pivot_longer(cols = c(l0_20, l20_40, l40_60, l60_80, l80plus)) %>% 
                                                 tidytable::filter(month %in% 1:3 & name == 'l20_40') %>% 
                                                 tidytable::summarise(value = mean(value), .by = c(year)) %>% 
                                                 tidytable::summarise(mean(value))),
                    Variable = 1) %>% 
  tidytable::rename(Yr = year) %>% 
  tidytable::select(Yr, Variable, Value) -> new_env1

# lzero
tidytable::bind_cols(c(1977, 1978), c(2, 2), c(1, 1)) %>% 
  tidytable::rename(Yr = '...1',
                    Variable = '...2',
                    Value = '...3') %>% 
  tidytable::bind_rows(cfsr %>% 
                         tidytable::pivot_longer(cols = c(l0_20, l20_40, l40_60, l60_80, l80plus)) %>% 
                         tidytable::filter(month == 6 & name == 'l0_20') %>% 
                         tidytable::mutate(index = value - as.numeric(cfsr %>% 
                                                                        tidytable::pivot_longer(cols = c(l0_20, l20_40, l40_60, l60_80, l80plus)) %>% 
                                                                        tidytable::filter(month == 6 & name == 'l0_20' & year %in% 1982:2012) %>% 
                                                                        tidytable::summarise(mean(value)))) %>% 
                         tidytable::mutate(Value = growth_L0(index, as.numeric(cfsr %>% 
                                                                                 tidytable::pivot_longer(cols = c(l0_20, l20_40, l40_60, l60_80, l80plus)) %>% 
                                                                                 tidytable::filter(month == 6 & name == 'l0_20' & year %in% 1982:2012) %>% 
                                                                                 tidytable::summarise(mean(value)))),
                                           Variable = 2) %>% 
                         tidytable::rename(Yr = year) %>% 
                         tidytable::select(Yr, Variable, Value)) -> new_env2

#put it all together
new_env1 %>%
  tidytable::bind_rows(new_env2) -> new_env

# write ss datafile
ss_dat$envdat <- as.data.frame(new_env)
r4ss::SS_writedat(datlist = ss_dat, 
                  outfile = here::here(asmnt_yr, 'rsch', new_mdl2, 'all_seas', list.files(here::here(asmnt_yr, 'rsch', new_mdl2, 'all_seas'), pattern = '.dat')),
                  overwrite = TRUE)

# run model
run_ss_model(asmnt_yr, mdl = paste0(new_mdl2, "/all_seas"))

# read the model output and print diagnostic messages
mdl2_seas_res <- r4ss::SS_output(dir = here::here(asmnt_yr, 'rsch', new_mdl2, "all_seas"),
                                 verbose = TRUE,
                                 printstats = TRUE)






# copy over the stock synthesis model files to the new directory for model 2019.1b
start_ss_fldr(from = here::here(asmnt_yr, 'rsch', base_mdl),
              to = here::here(asmnt_yr, 'rsch', new_mdl2))

# correct minsamplesize in dat file
dat <- r4ss::SS_readdat(here::here(asmnt_yr, 'rsch', base_mdl, list.files(here::here(asmnt_yr, 'rsch', base_mdl), pattern = '.dat')))

dat$age_info$minsamplesize <- 0.01

# add env timeseries to dat file

tempheat <- vroom::vroom(here::here(asmnt_yr, 'data', 'TEMPANDHEAT.csv')) %>% 
  dplyr::rename_with(., tolower)

tempheat %>% 
  tidytable::filter(yr %in% 1982:2012) %>% 
  tidytable::summarise(mu_temp = mean(june_temp)) -> mu_temp

# Based on larval growth by temp from Laurel et al. 2016 getting growth L0 index
tidytable::bind_cols(c(1977, 1978), c(1, 1), c(0, 0)) %>% 
  tidytable::rename(Yr = '...1',
                    Variable = '...2',
                    Value = '...3') %>% 
  tidytable::bind_rows(dat$envdat) %>% 
  tidytable::bind_rows(tidytable::bind_cols(c(1977, 1978), c(1, 1)) %>% 
                         tidytable::bind_rows(tidytable::bind_cols(tempheat$yr, growth_L0(tempheat$temp, as.numeric(mu_temp)))) %>% 
                         tidytable::rename(Yr = '...1',
                                           Value = '...2') %>% 
                         tidytable::mutate(Variable = 2)) -> new_env

dat$envdat <- as.data.frame(new_env)

r4ss::SS_writedat(datlist = dat, 
                  outfile = here::here(asmnt_yr, 'rsch', new_mdl2, list.files(here::here(asmnt_yr, 'rsch', new_mdl2), pattern = '.dat')),
                  overwrite = TRUE)


# add env link to ctl file - something doesn't work here, change it manually

# ctl <- r4ss::SS_readctl(here::here(asmnt_yr, 'rsch', base_mdl, list.files(here::here(asmnt_yr, 'rsch', base_mdl), pattern = 'ctl')),
#                         datlist = here::here(asmnt_yr, 'rsch', base_mdl, list.files(here::here(asmnt_yr, 'rsch', base_mdl), pattern = 'dat')[2]))
# 
# ctl$MG_parms$`env_var&link`[2:3] = 101
# 
# r4ss::SS_writectl(ctllist = ctl,
#                   outfile = here::here(asmnt_yr, 'rsch', new_mdl1, list.files(here::here(asmnt_yr, 'rsch', new_mdl2), pattern = 'ctl')),
#                   overwrite = TRUE)


# run model
mdl2_starter <- r4ss::SS_readstarter(file = here::here(asmnt_yr, 'rsch', new_mdl2, "starter.ss"))

mdl2_starter$init_values_src = 0
r4ss::SS_writestarter(mdl2_starter, 
                      dir = here::here(asmnt_yr, 'rsch', new_mdl2),
                      overwrite = TRUE)

r4ss::run_SS_models(dirvec = here::here(asmnt_yr, 'rsch', new_mdl2),
                    skipfinished = FALSE,
                    intern = TRUE)

mdl2_starter$init_values_src = 1
r4ss::SS_writestarter(mdl2_starter, 
                      dir = here::here(asmnt_yr, 'rsch', new_mdl2),
                      overwrite = TRUE)

# read the model output and print diagnostic messages
mdl2_res <- r4ss::SS_output(dir = here::here(asmnt_yr, 'rsch', new_mdl2),
                            verbose = TRUE,
                            printstats = TRUE)



# plots results and stuff ----


all_summ <- r4ss::SSsummarize(list(base_res, mdl1_res, mdl2_res))

names(all_summ)

all_summ$SpawnBio

all_summ$likelihoods

all_summ$npars


r4ss::SSplotComparisons(all_summ)

ss3diags::SSplotModelcomp(all_summ)





all_res <- r4ss::SSgetoutput(dirvec = c(here::here(asmnt_yr, 'rsch', base_mdl), here::here(asmnt_yr, 'rsch', new_mdl1)))


base_res$likelihoods_by_fleet
mdl1_res$likelihoods_by_fleet


base_res$likelihoods_used
mdl1_res$likelihoods_used




dat$MeanSize_at_Age_obs

list.files(here::here(asmnt_yr, 'rsch', new_mdl1), pattern = 'dat')

