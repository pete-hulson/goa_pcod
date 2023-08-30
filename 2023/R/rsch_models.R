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
ass_yr <- as.numeric(format(Sys.Date(), format = "%Y"))
  
# helper fcns ----
growth_L0 <- function(data, T){
  exp(0.2494 + 0.3216 * (T + data) - 0.0069 * (T + data) ^ 2 - 0.0004 * (T + data) ^ 3) / exp(0.2494 + 0.3216*(T)-0.0069 * (T) ^ 2 - 0.0004 * (T) ^ 3)
}

start_ss_fldr <- function(from, to){
  r4ss::copy_SS_inputs(dir.old = from, 
                       dir.new = to,
                       overwrite = TRUE)
  base::file.copy(from = paste0(from, "/ss.exe"),
                  to = paste0(to, "/ss.exe"),
                  overwrite = TRUE)
}

run_env_mdl <- function(env_data, ss_dat, param, mo, indx){
  
  env_data %>% 
    tidytable::pivot_longer(cols = c(l0_20, l20_40, l40_60, l60_80, l80plus)) %>% 
    tidytable::filter(month == mo & name == indx) %>% 
    tidytable::mutate(Value = value - as.numeric(env_data %>% 
                                                   tidytable::pivot_longer(cols = c(l0_20, l20_40, l40_60, l60_80, l80plus)) %>% 
                                                   tidytable::filter(month == mo & name == indx & year %in% 1982:2012) %>% 
                                                   tidytable::summarise(mean(value))),
                      Variable = 1) %>% 
    tidytable::rename(Yr = year) %>% 
    tidytable::select(Yr, Variable, Value) -> new_env1
  
  new_env1 %>%
    tidytable::bind_rows(ss_dat$envdat %>% 
                           tidytable::filter(Variable != 1)) -> new_env
  
  ss_dat$envdat <- as.data.frame(new_env)
  
  r4ss::SS_writedat(datlist = ss_dat, 
                    outfile = here::here(ass_yr, 'rsch', new_mdl2, param, list.files(here::here(ass_yr, 'rsch', new_mdl2, param), pattern = '.dat')),
                    overwrite = TRUE)
  
  # run model
  r4ss::run_SS_models(dirvec = here::here(ass_yr, 'rsch', new_mdl2, param),
                      skipfinished = FALSE,
                      intern = TRUE)
  
  # read the model output and print diagnostic messages
  res <- r4ss::SS_output(dir = here::here(ass_yr, 'rsch', new_mdl2, param),
                         verbose = TRUE,
                         printstats = TRUE)
  
  res}

# run/develop/read results from models ----

# base model ----
# read results from base model

base_res <- r4ss::SS_output(dir = here::here(ass_yr, 'rsch', base_mdl),
                            verbose = TRUE,
                            printstats = TRUE)

# minsamplesize corr (model 2019.1b) ----

# copy over the stock synthesis model files to the new directory for model 2019.1b
start_ss_fldr(from = here::here(ass_yr, 'rsch', base_mdl),
              to = here::here(ass_yr, 'rsch', new_mdl1))

# correct minsamplesize in dat file
dat <- r4ss::SS_readdat(here::here(ass_yr, 'rsch', base_mdl, list.files(here::here(ass_yr, 'rsch', base_mdl), pattern = '.dat')))

dat$age_info$minsamplesize <- 0.01

r4ss::SS_writedat(datlist = dat, 
                  outfile = here::here(ass_yr, 'rsch', new_mdl1, list.files(here::here(ass_yr, 'rsch', new_mdl1), pattern = '.dat')),
                  overwrite = TRUE)

# run model (not using par)
mdl1_starter <- r4ss::SS_readstarter(file = here::here(ass_yr, 'rsch', new_mdl1, "starter.ss"))

mdl1_starter$init_values_src = 0
r4ss::SS_writestarter(mdl1_starter, 
                      dir = here::here(ass_yr, 'rsch', new_mdl1),
                      overwrite = TRUE)

r4ss::run_SS_models(dirvec = here::here(ass_yr, 'rsch', new_mdl1),
                    skipfinished = FALSE,
                    intern = TRUE)

mdl1_starter$init_values_src = 1
r4ss::SS_writestarter(mdl1_starter, 
                      dir = here::here(ass_yr, 'rsch', new_mdl1),
                      overwrite = TRUE)

# read the model output and print diagnostic messages
mdl1_res <- r4ss::SS_output(dir = here::here(ass_yr, 'rsch', new_mdl1),
                            verbose = TRUE,
                            printstats = TRUE)


# growth model (model 2023.1) ----

# copy over the stock synthesis model files to the new directory for model 2019.1b
start_ss_fldr(from = here::here(ass_yr, 'rsch', base_mdl),
              to = here::here(ass_yr, 'rsch', new_mdl2))

# correct minsamplesize in dat file
dat <- r4ss::SS_readdat(here::here(ass_yr, 'rsch', base_mdl, list.files(here::here(ass_yr, 'rsch', base_mdl), pattern = '.dat')))

dat$age_info$minsamplesize <- 0.01

# add env timeseries to dat file

tempheat <- vroom::vroom(here::here(ass_yr, 'data', 'TEMPANDHEAT.csv')) %>% 
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
                  outfile = here::here(ass_yr, 'rsch', new_mdl2, list.files(here::here(ass_yr, 'rsch', new_mdl2), pattern = '.dat')),
                  overwrite = TRUE)


# add env link to ctl file - something doesn't work here, change it manually

# ctl <- r4ss::SS_readctl(here::here(ass_yr, 'rsch', base_mdl, list.files(here::here(ass_yr, 'rsch', base_mdl), pattern = 'ctl')),
#                         datlist = here::here(ass_yr, 'rsch', base_mdl, list.files(here::here(ass_yr, 'rsch', base_mdl), pattern = 'dat')[2]))
# 
# ctl$MG_parms$`env_var&link`[2:3] = 101
# 
# r4ss::SS_writectl(ctllist = ctl,
#                   outfile = here::here(ass_yr, 'rsch', new_mdl1, list.files(here::here(ass_yr, 'rsch', new_mdl2), pattern = 'ctl')),
#                   overwrite = TRUE)


# run model
mdl2_starter <- r4ss::SS_readstarter(file = here::here(ass_yr, 'rsch', new_mdl2, "starter.ss"))

mdl2_starter$init_values_src = 0
r4ss::SS_writestarter(mdl2_starter, 
                      dir = here::here(ass_yr, 'rsch', new_mdl2),
                      overwrite = TRUE)

r4ss::run_SS_models(dirvec = here::here(ass_yr, 'rsch', new_mdl2),
                    skipfinished = FALSE,
                    intern = TRUE)

mdl2_starter$init_values_src = 1
r4ss::SS_writestarter(mdl2_starter, 
                      dir = here::here(ass_yr, 'rsch', new_mdl2),
                      overwrite = TRUE)

# read the model output and print diagnostic messages
mdl2_res <- r4ss::SS_output(dir = here::here(ass_yr, 'rsch', new_mdl2),
                            verbose = TRUE,
                            printstats = TRUE)


# run envir ts analysis ----

# copy over the ss model files to the new directory for eval models
# start_ss_fldr(from = here::here(ass_yr, 'rsch', new_mdl2),
#               to = here::here(ass_yr, 'rsch', new_mdl2, "linf"))
# start_ss_fldr(from = here::here(ass_yr, 'rsch', new_mdl2),
#               to = here::here(ass_yr, 'rsch', new_mdl2, "kappa"))

# read in env data
cfsr <- vroom::vroom(here::here(ass_yr, 'data', 'raw_cfsr.csv')) %>% 
  dplyr::rename_with(., tolower) %>% 
  tidytable::rename(l0_20 = '0_20',
                    l20_40 = '20_40',
                    l40_60 = '40_60',
                    l60_80 = '60_80',
                    l80plus = '80plus')

# read in ss data
dat <- r4ss::SS_readdat(here::here(ass_yr, 'rsch', new_mdl2, list.files(here::here(ass_yr, 'rsch', new_mdl2), pattern = '.dat')))

# run model scenarios
test <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "linf", mo = 1, indx = "l0_20")


linf_1_20 <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "linf", mo = 1, indx = "l0_20")
linf_2_20 <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "linf", mo = 2, indx = "l0_20")
linf_3_20 <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "linf", mo = 3, indx = "l0_20")
linf_4_20 <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "linf", mo = 4, indx = "l0_20")
linf_5_20 <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "linf", mo = 5, indx = "l0_20")
linf_6_20 <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "linf", mo = 6, indx = "l0_20")
linf_7_20 <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "linf", mo = 7, indx = "l0_20")
linf_8_20 <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "linf", mo = 8, indx = "l0_20")
linf_9_20 <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "linf", mo = 9, indx = "l0_20")
linf_10_20 <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "linf", mo = 10, indx = "l0_20")
linf_11_20 <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "linf", mo = 11, indx = "l0_20")
linf_12_20 <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "linf", mo = 12, indx = "l0_20")

linf_1_40 <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "linf", mo = 1, indx = "l20_40")
linf_2_40 <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "linf", mo = 2, indx = "l20_40")
linf_3_40 <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "linf", mo = 3, indx = "l20_40")
linf_4_40 <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "linf", mo = 4, indx = "l20_40")
linf_5_40 <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "linf", mo = 5, indx = "l20_40")
linf_6_40 <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "linf", mo = 6, indx = "l20_40")
linf_7_40 <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "linf", mo = 7, indx = "l20_40")
linf_8_40 <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "linf", mo = 8, indx = "l20_40")
linf_9_40 <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "linf", mo = 9, indx = "l20_40")
linf_10_40 <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "linf", mo = 10, indx = "l20_40")
linf_11_40 <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "linf", mo = 11, indx = "l20_40")
linf_12_40 <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "linf", mo = 12, indx = "l20_40")

linf_1_60 <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "linf", mo = 1, indx = "l40_60")
linf_2_60 <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "linf", mo = 2, indx = "l40_60")
linf_3_60 <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "linf", mo = 3, indx = "l40_60")
linf_4_60 <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "linf", mo = 4, indx = "l40_60")
linf_5_60 <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "linf", mo = 5, indx = "l40_60")
linf_6_60 <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "linf", mo = 6, indx = "l40_60")
linf_7_60 <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "linf", mo = 7, indx = "l40_60")
linf_8_60 <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "linf", mo = 8, indx = "l40_60")
linf_9_60 <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "linf", mo = 9, indx = "l40_60")
linf_10_60 <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "linf", mo = 10, indx = "l40_60")
linf_11_60 <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "linf", mo = 11, indx = "l40_60")
linf_12_60 <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "linf", mo = 12, indx = "l40_60")

linf_1_80 <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "linf", mo = 1, indx = "l60_80")
linf_2_80 <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "linf", mo = 2, indx = "l60_80")
linf_3_80 <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "linf", mo = 3, indx = "l60_80")
linf_4_80 <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "linf", mo = 4, indx = "l60_80")
linf_5_80 <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "linf", mo = 5, indx = "l60_80")
linf_6_80 <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "linf", mo = 6, indx = "l60_80")
linf_7_80 <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "linf", mo = 7, indx = "l60_80")
linf_8_80 <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "linf", mo = 8, indx = "l60_80")
linf_9_80 <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "linf", mo = 9, indx = "l60_80")
linf_10_80 <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "linf", mo = 10, indx = "l60_80")
linf_11_80 <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "linf", mo = 11, indx = "l60_80")
linf_12_80 <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "linf", mo = 12, indx = "l60_80")

linf_1_80p <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "linf", mo = 1, indx = "l80plus")
linf_2_80p <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "linf", mo = 2, indx = "l80plus")
linf_3_80p <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "linf", mo = 3, indx = "l80plus")
linf_4_80p <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "linf", mo = 4, indx = "l80plus")
linf_5_80p <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "linf", mo = 5, indx = "l80plus")
linf_6_80p <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "linf", mo = 6, indx = "l80plus")
linf_7_80p <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "linf", mo = 7, indx = "l80plus")
linf_8_80p <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "linf", mo = 8, indx = "l80plus")
linf_9_80p <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "linf", mo = 9, indx = "l80plus")
linf_10_80p <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "linf", mo = 10, indx = "l80plus")
linf_11_80p <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "linf", mo = 11, indx = "l80plus")
linf_12_80p <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "linf", mo = 12, indx = "l80plus")


kappa_1_20 <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 1, indx = "l0_20")
kappa_2_20 <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 2, indx = "l0_20")
kappa_3_20 <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 3, indx = "l0_20")
kappa_4_20 <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 4, indx = "l0_20")
kappa_5_20 <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 5, indx = "l0_20")
kappa_6_20 <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 6, indx = "l0_20")
kappa_7_20 <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 7, indx = "l0_20")
kappa_8_20 <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 8, indx = "l0_20")
kappa_9_20 <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 9, indx = "l0_20")
kappa_10_20 <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 10, indx = "l0_20")
kappa_11_20 <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 11, indx = "l0_20")
kappa_12_20 <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 12, indx = "l0_20")

kappa_1_40 <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 1, indx = "l20_40")
kappa_2_40 <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 2, indx = "l20_40")
kappa_3_40 <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 3, indx = "l20_40")
kappa_4_40 <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 4, indx = "l20_40")
kappa_5_40 <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 5, indx = "l20_40")
kappa_6_40 <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 6, indx = "l20_40")
kappa_7_40 <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 7, indx = "l20_40")
kappa_8_40 <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 8, indx = "l20_40")
kappa_9_40 <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 9, indx = "l20_40")
kappa_10_40 <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 10, indx = "l20_40")
kappa_11_40 <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 11, indx = "l20_40")
kappa_12_40 <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 12, indx = "l20_40")

kappa_1_60 <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 1, indx = "l40_60")
kappa_2_60 <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 2, indx = "l40_60")
kappa_3_60 <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 3, indx = "l40_60")
kappa_4_60 <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 4, indx = "l40_60")
kappa_5_60 <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 5, indx = "l40_60")
kappa_6_60 <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 6, indx = "l40_60")
kappa_7_60 <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 7, indx = "l40_60")
kappa_8_60 <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 8, indx = "l40_60")
kappa_9_60 <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 9, indx = "l40_60")
kappa_10_60 <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 10, indx = "l40_60")
kappa_11_60 <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 11, indx = "l40_60")
kappa_12_60 <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 12, indx = "l40_60")

kappa_1_80 <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 1, indx = "l60_80")
kappa_2_80 <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 2, indx = "l60_80")
kappa_3_80 <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 3, indx = "l60_80")
kappa_4_80 <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 4, indx = "l60_80")
kappa_5_80 <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 5, indx = "l60_80")
kappa_6_80 <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 6, indx = "l60_80")
kappa_7_80 <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 7, indx = "l60_80")
kappa_8_80 <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 8, indx = "l60_80")
kappa_9_80 <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 9, indx = "l60_80")
kappa_10_80 <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 10, indx = "l60_80")
kappa_11_80 <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 11, indx = "l60_80")
kappa_12_80 <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 12, indx = "l60_80")

kappa_1_80p <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 1, indx = "l80plus")
kappa_2_80p <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 2, indx = "l80plus")
kappa_3_80p <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 3, indx = "l80plus")
kappa_4_80p <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 4, indx = "l80plus")
kappa_5_80p <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 5, indx = "l80plus")
kappa_6_80p <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 6, indx = "l80plus")
kappa_7_80p <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 7, indx = "l80plus")
kappa_8_80p <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 8, indx = "l80plus")
kappa_9_80p <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 9, indx = "l80plus")
kappa_10_80p <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 10, indx = "l80plus")
kappa_11_80p <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 11, indx = "l80plus")
kappa_12_80p <- run_env_mdl(env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 12, indx = "l80plus")











# plots results and stuff ----


all_summ <- r4ss::SSsummarize(list(base_res, mdl1_res, mdl2_res))

names(all_summ)

all_summ$SpawnBio

all_summ$likelihoods

all_summ$npars


r4ss::SSplotComparisons(all_summ)

ss3diags::SSplotModelcomp(all_summ)





all_res <- r4ss::SSgetoutput(dirvec = c(here::here(ass_yr, 'rsch', base_mdl), here::here(ass_yr, 'rsch', new_mdl1)))


base_res$likelihoods_by_fleet
mdl1_res$likelihoods_by_fleet


base_res$likelihoods_used
mdl1_res$likelihoods_used




dat$MeanSize_at_Age_obs

list.files(here::here(ass_yr, 'rsch', new_mdl1), pattern = 'dat')

