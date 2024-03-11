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
new_base_llq <- "2019.1d-2022" # 2022 model with new llq env link
new_mdl1 <- "2023.1-2022" # 2022 model with env growth link
new_mdl2 <- "2023.2-2022" # 2022 model with env growth link and pref llq env link


# assessment year
asmnt_yr <- as.numeric(format(Sys.Date(), format = "%Y"))

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

run_env_mdl <- function(mdl, env_data, ss_dat, param, mo, indx){
  
  env_data %>% 
    tidytable::pivot_longer(cols = c(l0_20, l20_40, l40_60, l60_80, l80plus)) %>% 
    tidytable::filter(month == mo & name == indx) %>% 
    tidytable::mutate(Value = value - as.numeric(env_data %>% 
                                                   tidytable::pivot_longer(cols = c(l0_20, l20_40, l40_60, l60_80, l80plus)) %>% 
                                                   tidytable::filter(month == mo & name == indx & year %in% 1982:2012) %>% 
                                                   tidytable::summarise(mean(value))),
                      Variable = 2) %>% 
    tidytable::rename(Yr = year) %>% 
    tidytable::select(Yr, Variable, Value) -> new_env1
  
  ss_dat$envdat %>% 
    tidytable::filter(Variable == 1) %>% 
    tidytable::bind_rows(new_env1) -> new_env
  
  ss_dat$envdat <- as.data.frame(new_env)
  
  r4ss::SS_writedat(datlist = ss_dat, 
                    outfile = here::here(asmnt_yr, 'rsch', mdl, param, list.files(here::here(asmnt_yr, 'rsch', mdl, param), pattern = '.dat')),
                    overwrite = TRUE)
  
  # run model
  r4ss::run_SS_models(dirvec = here::here(asmnt_yr, 'rsch', mdl, param),
                      skipfinished = FALSE,
                      intern = TRUE)
  
  # read the model output and print diagnostic messages
  res <- r4ss::SS_output(dir = here::here(asmnt_yr, 'rsch', mdl, param),
                         verbose = TRUE,
                         printstats = TRUE)
  
  res}

run_lzero_mdl <- function(mdl, env_data, ss_dat, param, mo){
  
  tidytable::bind_cols(c(1977, 1978), c(2, 2), c(1, 1)) %>% 
    tidytable::rename(Yr = '...1',
                      Variable = '...2',
                      Value = '...3') %>% 
    tidytable::bind_rows(env_data %>% 
                           tidytable::pivot_longer(cols = c(l0_20, l20_40, l40_60, l60_80, l80plus)) %>% 
                           tidytable::filter(month == mo & name == 'l0_20') %>% 
                           tidytable::mutate(index = value - as.numeric(env_data %>% 
                                                                          tidytable::pivot_longer(cols = c(l0_20, l20_40, l40_60, l60_80, l80plus)) %>% 
                                                                          tidytable::filter(month == mo & name == 'l0_20' & year %in% 1982:2012) %>% 
                                                                          tidytable::summarise(mean(value)))) %>% 
                           tidytable::mutate(Value = growth_L0(index, as.numeric(env_data %>% 
                                                                                   tidytable::pivot_longer(cols = c(l0_20, l20_40, l40_60, l60_80, l80plus)) %>% 
                                                                                   tidytable::filter(month == mo & name == 'l0_20' & year %in% 1982:2012) %>% 
                                                                                   tidytable::summarise(mean(value)))),
                                             Variable = 2) %>% 
                           tidytable::rename(Yr = year) %>% 
                           tidytable::select(Yr, Variable, Value)) -> new_env1
  
  
  ss_dat$envdat %>% 
    tidytable::filter(Variable == 1) %>%
    tidytable::bind_rows(new_env1) -> new_env
  
  ss_dat$envdat <- as.data.frame(new_env)
  
  r4ss::SS_writedat(datlist = ss_dat, 
                    outfile = here::here(asmnt_yr, 'rsch', mdl, param, list.files(here::here(asmnt_yr, 'rsch', mdl, param), pattern = '.dat')),
                    overwrite = TRUE)
  
  # run model
  r4ss::run_SS_models(dirvec = here::here(asmnt_yr, 'rsch', mdl, param),
                      skipfinished = FALSE,
                      intern = TRUE)
  
  # read the model output and print diagnostic messages
  res <- r4ss::SS_output(dir = here::here(asmnt_yr, 'rsch', mdl, param),
                         verbose = TRUE,
                         printstats = TRUE)
  
  res}

run_llq_mdl <- function(mdl, env_data, ss_dat, param, mo, indx){
  
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
                    outfile = here::here(asmnt_yr, 'rsch', mdl, param, list.files(here::here(asmnt_yr, 'rsch', mdl, param), pattern = '.dat')),
                    overwrite = TRUE)
  
  # run model
  r4ss::run_SS_models(dirvec = here::here(asmnt_yr, 'rsch', mdl, param),
                      skipfinished = FALSE,
                      intern = TRUE)
  
  # read the model output and print diagnostic messages
  res <- r4ss::SS_output(dir = here::here(asmnt_yr, 'rsch', mdl, param),
                         verbose = TRUE,
                         printstats = TRUE)
  
  res}


# run envir ts analysis ----

# copy over the ss model files to the new directory for eval models
# start_ss_fldr(from = here::here(asmnt_yr, 'rsch', new_mdl2),
#               to = here::here(asmnt_yr, 'rsch', new_mdl2, "linf"))
# start_ss_fldr(from = here::here(asmnt_yr, 'rsch', new_mdl2),
#               to = here::here(asmnt_yr, 'rsch', new_mdl2, "kappa"))
# start_ss_fldr(from = here::here(asmnt_yr, 'rsch', new_mdl2),
#               to = here::here(asmnt_yr, 'rsch', new_mdl2, "lzero"))
# start_ss_fldr(from = here::here(asmnt_yr, 'rsch', new_mdl2),
#               to = here::here(asmnt_yr, 'rsch', new_mdl3, "llq"))

# read in env data
cfsr <- vroom::vroom(here::here(asmnt_yr, 'data', 'raw_cfsr.csv')) %>% 
  dplyr::rename_with(., tolower) %>% 
  tidytable::rename(l0_20 = '0_20',
                    l20_40 = '20_40',
                    l40_60 = '40_60',
                    l60_80 = '60_80',
                    l80plus = '80plus')

# read in ss data
dat <- r4ss::SS_readdat(here::here(asmnt_yr, 'rsch', new_mdl1, list.files(here::here(asmnt_yr, 'rsch', new_mdl1), pattern = '.dat')))

# linf ----
linf_1_20 <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "linf", mo = 1, indx = "l0_20")
linf_2_20 <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "linf", mo = 2, indx = "l0_20")
linf_3_20 <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "linf", mo = 3, indx = "l0_20")
linf_4_20 <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "linf", mo = 4, indx = "l0_20")
linf_5_20 <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "linf", mo = 5, indx = "l0_20")
linf_6_20 <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "linf", mo = 6, indx = "l0_20")
linf_7_20 <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "linf", mo = 7, indx = "l0_20")
linf_8_20 <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "linf", mo = 8, indx = "l0_20")
linf_9_20 <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "linf", mo = 9, indx = "l0_20")
linf_10_20 <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "linf", mo = 10, indx = "l0_20")
linf_11_20 <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "linf", mo = 11, indx = "l0_20")
linf_12_20 <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "linf", mo = 12, indx = "l0_20")

linf_1_40 <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "linf", mo = 1, indx = "l20_40")
linf_2_40 <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "linf", mo = 2, indx = "l20_40")
linf_3_40 <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "linf", mo = 3, indx = "l20_40")
linf_4_40 <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "linf", mo = 4, indx = "l20_40")
linf_5_40 <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "linf", mo = 5, indx = "l20_40")
linf_6_40 <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "linf", mo = 6, indx = "l20_40")
linf_7_40 <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "linf", mo = 7, indx = "l20_40")
linf_8_40 <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "linf", mo = 8, indx = "l20_40")
linf_9_40 <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "linf", mo = 9, indx = "l20_40")
linf_10_40 <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "linf", mo = 10, indx = "l20_40")
linf_11_40 <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "linf", mo = 11, indx = "l20_40")
linf_12_40 <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "linf", mo = 12, indx = "l20_40")

linf_1_60 <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "linf", mo = 1, indx = "l40_60")
linf_2_60 <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "linf", mo = 2, indx = "l40_60")
linf_3_60 <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "linf", mo = 3, indx = "l40_60")
linf_4_60 <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "linf", mo = 4, indx = "l40_60")
linf_5_60 <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "linf", mo = 5, indx = "l40_60")
linf_6_60 <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "linf", mo = 6, indx = "l40_60")
linf_7_60 <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "linf", mo = 7, indx = "l40_60")
linf_8_60 <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "linf", mo = 8, indx = "l40_60")
linf_9_60 <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "linf", mo = 9, indx = "l40_60")
linf_10_60 <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "linf", mo = 10, indx = "l40_60")
linf_11_60 <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "linf", mo = 11, indx = "l40_60")
linf_12_60 <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "linf", mo = 12, indx = "l40_60")

linf_1_80 <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "linf", mo = 1, indx = "l60_80")
linf_2_80 <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "linf", mo = 2, indx = "l60_80")
linf_3_80 <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "linf", mo = 3, indx = "l60_80")
linf_4_80 <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "linf", mo = 4, indx = "l60_80")
linf_5_80 <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "linf", mo = 5, indx = "l60_80")
linf_6_80 <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "linf", mo = 6, indx = "l60_80")
linf_7_80 <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "linf", mo = 7, indx = "l60_80")
linf_8_80 <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "linf", mo = 8, indx = "l60_80")
linf_9_80 <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "linf", mo = 9, indx = "l60_80")
linf_10_80 <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "linf", mo = 10, indx = "l60_80")
linf_11_80 <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "linf", mo = 11, indx = "l60_80")
linf_12_80 <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "linf", mo = 12, indx = "l60_80")

linf_1_80p <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "linf", mo = 1, indx = "l80plus")
linf_2_80p <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "linf", mo = 2, indx = "l80plus")
linf_3_80p <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "linf", mo = 3, indx = "l80plus")
linf_4_80p <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "linf", mo = 4, indx = "l80plus")
linf_5_80p <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "linf", mo = 5, indx = "l80plus")
linf_6_80p <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "linf", mo = 6, indx = "l80plus")
linf_7_80p <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "linf", mo = 7, indx = "l80plus")
linf_8_80p <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "linf", mo = 8, indx = "l80plus")
linf_9_80p <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "linf", mo = 9, indx = "l80plus")
linf_10_80p <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "linf", mo = 10, indx = "l80plus")
linf_11_80p <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "linf", mo = 11, indx = "l80plus")
linf_12_80p <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "linf", mo = 12, indx = "l80plus")

linf_summ <- r4ss::SSsummarize(list(mdl1_res, linf_1_20,linf_2_20, linf_3_20, linf_4_20, linf_5_20, linf_6_20, linf_7_20, linf_8_20, linf_9_20, linf_10_20, linf_11_20, linf_12_20,
                                    linf_1_40,linf_2_40, linf_3_40, linf_4_40, linf_5_40, linf_6_40, linf_7_40, linf_8_40, linf_9_40, linf_10_40, linf_11_40, linf_12_40,
                                    linf_1_60,linf_2_60, linf_3_60, linf_4_60, linf_5_60, linf_6_60, linf_7_60, linf_8_60, linf_9_60, linf_10_60, linf_11_60, linf_12_60,
                                    linf_1_80,linf_2_80, linf_3_80, linf_4_80, linf_5_80, linf_6_80, linf_7_80, linf_8_80, linf_9_80, linf_10_80, linf_11_80, linf_12_80,
                                    linf_1_80p,linf_2_80p, linf_3_80p, linf_4_80p, linf_5_80p, linf_6_80p, linf_7_80p, linf_8_80p, linf_9_80p, linf_10_80p, linf_11_80p, linf_12_80p))

vroom::vroom_write(linf_summ$likelihoods, here::here(asmnt_yr, 'rsch', 'output', 'linf_likes.csv'), delim = ",")
base::save(linf_summ, file = here::here(asmnt_yr, 'rsch', 'output', 'linf.RData'))

# kappa ----
kappa_1_20 <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 1, indx = "l0_20")
kappa_2_20 <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 2, indx = "l0_20")
kappa_3_20 <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 3, indx = "l0_20")
kappa_4_20 <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 4, indx = "l0_20")
kappa_5_20 <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 5, indx = "l0_20")
kappa_6_20 <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 6, indx = "l0_20")
kappa_7_20 <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 7, indx = "l0_20")
kappa_8_20 <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 8, indx = "l0_20")
kappa_9_20 <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 9, indx = "l0_20")
kappa_10_20 <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 10, indx = "l0_20")
kappa_11_20 <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 11, indx = "l0_20")
kappa_12_20 <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 12, indx = "l0_20")

kappa_1_40 <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 1, indx = "l20_40")
kappa_2_40 <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 2, indx = "l20_40")
kappa_3_40 <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 3, indx = "l20_40")
kappa_4_40 <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 4, indx = "l20_40")
kappa_5_40 <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 5, indx = "l20_40")
kappa_6_40 <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 6, indx = "l20_40")
kappa_7_40 <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 7, indx = "l20_40")
kappa_8_40 <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 8, indx = "l20_40")
kappa_9_40 <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 9, indx = "l20_40")
kappa_10_40 <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 10, indx = "l20_40")
kappa_11_40 <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 11, indx = "l20_40")
kappa_12_40 <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 12, indx = "l20_40")

kappa_1_60 <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 1, indx = "l40_60")
kappa_2_60 <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 2, indx = "l40_60")
kappa_3_60 <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 3, indx = "l40_60")
kappa_4_60 <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 4, indx = "l40_60")
kappa_5_60 <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 5, indx = "l40_60")
kappa_6_60 <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 6, indx = "l40_60")
kappa_7_60 <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 7, indx = "l40_60")
kappa_8_60 <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 8, indx = "l40_60")
kappa_9_60 <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 9, indx = "l40_60")
kappa_10_60 <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 10, indx = "l40_60")
kappa_11_60 <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 11, indx = "l40_60")
kappa_12_60 <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 12, indx = "l40_60")

kappa_1_80 <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 1, indx = "l60_80")
kappa_2_80 <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 2, indx = "l60_80")
kappa_3_80 <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 3, indx = "l60_80")
kappa_4_80 <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 4, indx = "l60_80")
kappa_5_80 <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 5, indx = "l60_80")
kappa_6_80 <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 6, indx = "l60_80")
kappa_7_80 <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 7, indx = "l60_80")
kappa_8_80 <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 8, indx = "l60_80")
kappa_9_80 <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 9, indx = "l60_80")
kappa_10_80 <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 10, indx = "l60_80")
kappa_11_80 <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 11, indx = "l60_80")
kappa_12_80 <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 12, indx = "l60_80")

kappa_1_80p <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 1, indx = "l80plus")
kappa_2_80p <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 2, indx = "l80plus")
kappa_3_80p <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 3, indx = "l80plus")
kappa_4_80p <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 4, indx = "l80plus")
kappa_5_80p <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 5, indx = "l80plus")
kappa_6_80p <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 6, indx = "l80plus")
kappa_7_80p <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 7, indx = "l80plus")
kappa_8_80p <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 8, indx = "l80plus")
kappa_9_80p <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 9, indx = "l80plus")
kappa_10_80p <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 10, indx = "l80plus")
kappa_11_80p <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 11, indx = "l80plus")
kappa_12_80p <- run_env_mdl(new_mdl1, env_dat = cfsr, ss_dat = dat, param = "kappa", mo = 12, indx = "l80plus")

kappa_summ <- r4ss::SSsummarize(list(mdl1_res, kappa_1_20,kappa_2_20, kappa_3_20, kappa_4_20, kappa_5_20, kappa_6_20, kappa_7_20, kappa_8_20, kappa_9_20, kappa_10_20, kappa_11_20, kappa_12_20,
                                     kappa_1_40,kappa_2_40, kappa_3_40, kappa_4_40, kappa_5_40, kappa_6_40, kappa_7_40, kappa_8_40, kappa_9_40, kappa_10_40, kappa_11_40, kappa_12_40,
                                     kappa_1_60,kappa_2_60, kappa_3_60, kappa_4_60, kappa_5_60, kappa_6_60, kappa_7_60, kappa_8_60, kappa_9_60, kappa_10_60, kappa_11_60, kappa_12_60,
                                     kappa_1_80,kappa_2_80, kappa_3_80, kappa_4_80, kappa_5_80, kappa_6_80, kappa_7_80, kappa_8_80, kappa_9_80, kappa_10_80, kappa_11_80, kappa_12_80,
                                     kappa_1_80p,kappa_2_80p, kappa_3_80p, kappa_4_80p, kappa_5_80p, kappa_6_80p, kappa_7_80p, kappa_8_80p, kappa_9_80p, kappa_10_80p, kappa_11_80p, kappa_12_80p))

vroom::vroom_write(kappa_summ$likelihoods, here::here(asmnt_yr, 'rsch', 'output', 'kappa_likes.csv'), delim = ",")
base::save(kappa_summ, file = here::here(asmnt_yr, 'rsch', 'output', 'kappa.RData'))

# lzero ----

lzero_1 <- run_lzero_mdl(new_mdl1, env_data = cfsr, ss_dat = dat, param = "lzero", mo = 1)
lzero_2 <- run_lzero_mdl(new_mdl1, env_data = cfsr, ss_dat = dat, param = "lzero", mo = 2)
lzero_3 <- run_lzero_mdl(new_mdl1, env_data = cfsr, ss_dat = dat, param = "lzero", mo = 3)
lzero_4 <- run_lzero_mdl(new_mdl1, env_data = cfsr, ss_dat = dat, param = "lzero", mo = 4)
lzero_5 <- run_lzero_mdl(new_mdl1, env_data = cfsr, ss_dat = dat, param = "lzero", mo = 5)
lzero_6 <- run_lzero_mdl(new_mdl1, env_data = cfsr, ss_dat = dat, param = "lzero", mo = 6)
lzero_7 <- run_lzero_mdl(new_mdl1, env_data = cfsr, ss_dat = dat, param = "lzero", mo = 7)
lzero_8 <- run_lzero_mdl(new_mdl1, env_data = cfsr, ss_dat = dat, param = "lzero", mo = 8)
lzero_9 <- run_lzero_mdl(new_mdl1, env_data = cfsr, ss_dat = dat, param = "lzero", mo = 9)
lzero_10 <- run_lzero_mdl(new_mdl1, env_data = cfsr, ss_dat = dat, param = "lzero", mo = 10)
lzero_11 <- run_lzero_mdl(new_mdl1, env_data = cfsr, ss_dat = dat, param = "lzero", mo = 11)
lzero_12 <- run_lzero_mdl(new_mdl1, env_data = cfsr, ss_dat = dat, param = "lzero", mo = 12)

lzero_summ <- r4ss::SSsummarize(list(mdl1_res, lzero_1,lzero_2, lzero_3, lzero_4, lzero_5, lzero_6, lzero_7, lzero_8, lzero_9, lzero_10, lzero_11, lzero_12))

vroom::vroom_write(lzero_summ$likelihoods, here::here(asmnt_yr, 'rsch', 'output', 'lzero_likes.csv'), delim = ",")
base::save(lzero_summ, file = here::here(asmnt_yr, 'rsch', 'output', 'lzero.RData'))

# ll survey q ----

# read in ss data
dat <- r4ss::SS_readdat(here::here(asmnt_yr, 'rsch', new_mdl2, list.files(here::here(asmnt_yr, 'rsch', new_mdl2), pattern = '.dat')))

# llq w/ env link for growth ----
llq_1_20 <- run_llq_mdl(new_mdl2, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 1, indx = "l0_20")
llq_2_20 <- run_llq_mdl(new_mdl2, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 2, indx = "l0_20")
llq_3_20 <- run_llq_mdl(new_mdl2, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 3, indx = "l0_20")
llq_4_20 <- run_llq_mdl(new_mdl2, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 4, indx = "l0_20")
llq_5_20 <- run_llq_mdl(new_mdl2, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 5, indx = "l0_20")
llq_6_20 <- run_llq_mdl(new_mdl2, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 6, indx = "l0_20")
llq_7_20 <- run_llq_mdl(new_mdl2, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 7, indx = "l0_20")
llq_8_20 <- run_llq_mdl(new_mdl2, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 8, indx = "l0_20")
llq_9_20 <- run_llq_mdl(new_mdl2, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 9, indx = "l0_20")
llq_10_20 <- run_llq_mdl(new_mdl2, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 10, indx = "l0_20")
llq_11_20 <- run_llq_mdl(new_mdl2, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 11, indx = "l0_20")
llq_12_20 <- run_llq_mdl(new_mdl2, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 12, indx = "l0_20")

llq_1_40 <- run_llq_mdl(new_mdl2, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 1, indx = "l20_40")
llq_2_40 <- run_llq_mdl(new_mdl2, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 2, indx = "l20_40")
llq_3_40 <- run_llq_mdl(new_mdl2, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 3, indx = "l20_40")
llq_4_40 <- run_llq_mdl(new_mdl2, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 4, indx = "l20_40")
llq_5_40 <- run_llq_mdl(new_mdl2, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 5, indx = "l20_40")
llq_6_40 <- run_llq_mdl(new_mdl2, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 6, indx = "l20_40")
llq_7_40 <- run_llq_mdl(new_mdl2, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 7, indx = "l20_40")
llq_8_40 <- run_llq_mdl(new_mdl2, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 8, indx = "l20_40")
llq_9_40 <- run_llq_mdl(new_mdl2, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 9, indx = "l20_40")
llq_10_40 <- run_llq_mdl(new_mdl2, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 10, indx = "l20_40")
llq_11_40 <- run_llq_mdl(new_mdl2, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 11, indx = "l20_40")
llq_12_40 <- run_llq_mdl(new_mdl2, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 12, indx = "l20_40")

llq_1_60 <- run_llq_mdl(new_mdl2, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 1, indx = "l40_60")
llq_2_60 <- run_llq_mdl(new_mdl2, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 2, indx = "l40_60")
llq_3_60 <- run_llq_mdl(new_mdl2, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 3, indx = "l40_60")
llq_4_60 <- run_llq_mdl(new_mdl2, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 4, indx = "l40_60")
llq_5_60 <- run_llq_mdl(new_mdl2, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 5, indx = "l40_60")
llq_6_60 <- run_llq_mdl(new_mdl2, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 6, indx = "l40_60")
llq_7_60 <- run_llq_mdl(new_mdl2, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 7, indx = "l40_60")
llq_8_60 <- run_llq_mdl(new_mdl2, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 8, indx = "l40_60")
llq_9_60 <- run_llq_mdl(new_mdl2, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 9, indx = "l40_60")
llq_10_60 <- run_llq_mdl(new_mdl2, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 10, indx = "l40_60")
llq_11_60 <- run_llq_mdl(new_mdl2, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 11, indx = "l40_60")
llq_12_60 <- run_llq_mdl(new_mdl2, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 12, indx = "l40_60")

llq_1_80 <- run_llq_mdl(new_mdl2, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 1, indx = "l60_80")
llq_2_80 <- run_llq_mdl(new_mdl2, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 2, indx = "l60_80")
llq_3_80 <- run_llq_mdl(new_mdl2, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 3, indx = "l60_80")
llq_4_80 <- run_llq_mdl(new_mdl2, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 4, indx = "l60_80")
llq_5_80 <- run_llq_mdl(new_mdl2, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 5, indx = "l60_80")
llq_6_80 <- run_llq_mdl(new_mdl2, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 6, indx = "l60_80")
llq_7_80 <- run_llq_mdl(new_mdl2, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 7, indx = "l60_80")
llq_8_80 <- run_llq_mdl(new_mdl2, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 8, indx = "l60_80")
llq_9_80 <- run_llq_mdl(new_mdl2, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 9, indx = "l60_80")
llq_10_80 <- run_llq_mdl(new_mdl2, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 10, indx = "l60_80")
llq_11_80 <- run_llq_mdl(new_mdl2, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 11, indx = "l60_80")
llq_12_80 <- run_llq_mdl(new_mdl2, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 12, indx = "l60_80")

llq_1_80p <- run_llq_mdl(new_mdl2, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 1, indx = "l80plus")
llq_2_80p <- run_llq_mdl(new_mdl2, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 2, indx = "l80plus")
llq_3_80p <- run_llq_mdl(new_mdl2, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 3, indx = "l80plus")
llq_4_80p <- run_llq_mdl(new_mdl2, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 4, indx = "l80plus")
llq_5_80p <- run_llq_mdl(new_mdl2, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 5, indx = "l80plus")
llq_6_80p <- run_llq_mdl(new_mdl2, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 6, indx = "l80plus")
llq_7_80p <- run_llq_mdl(new_mdl2, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 7, indx = "l80plus")
llq_8_80p <- run_llq_mdl(new_mdl2, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 8, indx = "l80plus")
llq_9_80p <- run_llq_mdl(new_mdl2, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 9, indx = "l80plus")
llq_10_80p <- run_llq_mdl(new_mdl2, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 10, indx = "l80plus")
llq_11_80p <- run_llq_mdl(new_mdl2, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 11, indx = "l80plus")
llq_12_80p <- run_llq_mdl(new_mdl2, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 12, indx = "l80plus")

llq_summ <- r4ss::SSsummarize(list(mdl1_res, llq_1_20,llq_2_20, llq_3_20, llq_4_20, llq_5_20, llq_6_20, llq_7_20, llq_8_20, llq_9_20, llq_10_20, llq_11_20, llq_12_20,
                                     llq_1_40,llq_2_40, llq_3_40, llq_4_40, llq_5_40, llq_6_40, llq_7_40, llq_8_40, llq_9_40, llq_10_40, llq_11_40, llq_12_40,
                                     llq_1_60,llq_2_60, llq_3_60, llq_4_60, llq_5_60, llq_6_60, llq_7_60, llq_8_60, llq_9_60, llq_10_60, llq_11_60, llq_12_60,
                                     llq_1_80,llq_2_80, llq_3_80, llq_4_80, llq_5_80, llq_6_80, llq_7_80, llq_8_80, llq_9_80, llq_10_80, llq_11_80, llq_12_80,
                                     llq_1_80p,llq_2_80p, llq_3_80p, llq_4_80p, llq_5_80p, llq_6_80p, llq_7_80p, llq_8_80p, llq_9_80p, llq_10_80p, llq_11_80p, llq_12_80p))

vroom::vroom_write(llq_summ$likelihoods, here::here(asmnt_yr, 'rsch', 'output', 'llq_likes.csv'), delim = ",")
base::save(llq_summ, file = here::here(asmnt_yr, 'rsch', 'output', 'llq.RData'))

# llq for base ----

# read in ss data
dat <- r4ss::SS_readdat(here::here(asmnt_yr, 'rsch', new_base_llq, list.files(here::here(asmnt_yr, 'rsch', new_base_llq), pattern = '.dat')))

llq_1_20 <- run_llq_mdl(new_base_llq, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 1, indx = "l0_20")
llq_2_20 <- run_llq_mdl(new_base_llq, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 2, indx = "l0_20")
llq_3_20 <- run_llq_mdl(new_base_llq, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 3, indx = "l0_20")
llq_4_20 <- run_llq_mdl(new_base_llq, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 4, indx = "l0_20")
llq_5_20 <- run_llq_mdl(new_base_llq, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 5, indx = "l0_20")
llq_6_20 <- run_llq_mdl(new_base_llq, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 6, indx = "l0_20")
llq_7_20 <- run_llq_mdl(new_base_llq, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 7, indx = "l0_20")
llq_8_20 <- run_llq_mdl(new_base_llq, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 8, indx = "l0_20")
llq_9_20 <- run_llq_mdl(new_base_llq, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 9, indx = "l0_20")
llq_10_20 <- run_llq_mdl(new_base_llq, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 10, indx = "l0_20")
llq_11_20 <- run_llq_mdl(new_base_llq, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 11, indx = "l0_20")
llq_12_20 <- run_llq_mdl(new_base_llq, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 12, indx = "l0_20")

llq_1_40 <- run_llq_mdl(new_base_llq, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 1, indx = "l20_40")
llq_2_40 <- run_llq_mdl(new_base_llq, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 2, indx = "l20_40")
llq_3_40 <- run_llq_mdl(new_base_llq, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 3, indx = "l20_40")
llq_4_40 <- run_llq_mdl(new_base_llq, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 4, indx = "l20_40")
llq_5_40 <- run_llq_mdl(new_base_llq, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 5, indx = "l20_40")
llq_6_40 <- run_llq_mdl(new_base_llq, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 6, indx = "l20_40")
llq_7_40 <- run_llq_mdl(new_base_llq, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 7, indx = "l20_40")
llq_8_40 <- run_llq_mdl(new_base_llq, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 8, indx = "l20_40")
llq_9_40 <- run_llq_mdl(new_base_llq, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 9, indx = "l20_40")
llq_10_40 <- run_llq_mdl(new_base_llq, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 10, indx = "l20_40")
llq_11_40 <- run_llq_mdl(new_base_llq, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 11, indx = "l20_40")
llq_12_40 <- run_llq_mdl(new_base_llq, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 12, indx = "l20_40")

llq_1_60 <- run_llq_mdl(new_base_llq, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 1, indx = "l40_60")
llq_2_60 <- run_llq_mdl(new_base_llq, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 2, indx = "l40_60")
llq_3_60 <- run_llq_mdl(new_base_llq, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 3, indx = "l40_60")
llq_4_60 <- run_llq_mdl(new_base_llq, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 4, indx = "l40_60")
llq_5_60 <- run_llq_mdl(new_base_llq, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 5, indx = "l40_60")
llq_6_60 <- run_llq_mdl(new_base_llq, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 6, indx = "l40_60")
llq_7_60 <- run_llq_mdl(new_base_llq, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 7, indx = "l40_60")
llq_8_60 <- run_llq_mdl(new_base_llq, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 8, indx = "l40_60")
llq_9_60 <- run_llq_mdl(new_base_llq, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 9, indx = "l40_60")
llq_10_60 <- run_llq_mdl(new_base_llq, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 10, indx = "l40_60")
llq_11_60 <- run_llq_mdl(new_base_llq, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 11, indx = "l40_60")
llq_12_60 <- run_llq_mdl(new_base_llq, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 12, indx = "l40_60")

llq_1_80 <- run_llq_mdl(new_base_llq, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 1, indx = "l60_80")
llq_2_80 <- run_llq_mdl(new_base_llq, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 2, indx = "l60_80")
llq_3_80 <- run_llq_mdl(new_base_llq, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 3, indx = "l60_80")
llq_4_80 <- run_llq_mdl(new_base_llq, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 4, indx = "l60_80")
llq_5_80 <- run_llq_mdl(new_base_llq, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 5, indx = "l60_80")
llq_6_80 <- run_llq_mdl(new_base_llq, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 6, indx = "l60_80")
llq_7_80 <- run_llq_mdl(new_base_llq, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 7, indx = "l60_80")
llq_8_80 <- run_llq_mdl(new_base_llq, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 8, indx = "l60_80")
llq_9_80 <- run_llq_mdl(new_base_llq, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 9, indx = "l60_80")
llq_10_80 <- run_llq_mdl(new_base_llq, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 10, indx = "l60_80")
llq_11_80 <- run_llq_mdl(new_base_llq, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 11, indx = "l60_80")
llq_12_80 <- run_llq_mdl(new_base_llq, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 12, indx = "l60_80")

llq_1_80p <- run_llq_mdl(new_base_llq, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 1, indx = "l80plus")
llq_2_80p <- run_llq_mdl(new_base_llq, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 2, indx = "l80plus")
llq_3_80p <- run_llq_mdl(new_base_llq, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 3, indx = "l80plus")
llq_4_80p <- run_llq_mdl(new_base_llq, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 4, indx = "l80plus")
llq_5_80p <- run_llq_mdl(new_base_llq, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 5, indx = "l80plus")
llq_6_80p <- run_llq_mdl(new_base_llq, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 6, indx = "l80plus")
llq_7_80p <- run_llq_mdl(new_base_llq, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 7, indx = "l80plus")
llq_8_80p <- run_llq_mdl(new_base_llq, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 8, indx = "l80plus")
llq_9_80p <- run_llq_mdl(new_base_llq, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 9, indx = "l80plus")
llq_10_80p <- run_llq_mdl(new_base_llq, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 10, indx = "l80plus")
llq_11_80p <- run_llq_mdl(new_base_llq, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 11, indx = "l80plus")
llq_12_80p <- run_llq_mdl(new_base_llq, env_dat = cfsr, ss_dat = dat, param = "llq", mo = 12, indx = "l80plus")

llq_summ <- r4ss::SSsummarize(list(mdl1_res, llq_1_20,llq_2_20, llq_3_20, llq_4_20, llq_5_20, llq_6_20, llq_7_20, llq_8_20, llq_9_20, llq_10_20, llq_11_20, llq_12_20,
                                   llq_1_40,llq_2_40, llq_3_40, llq_4_40, llq_5_40, llq_6_40, llq_7_40, llq_8_40, llq_9_40, llq_10_40, llq_11_40, llq_12_40,
                                   llq_1_60,llq_2_60, llq_3_60, llq_4_60, llq_5_60, llq_6_60, llq_7_60, llq_8_60, llq_9_60, llq_10_60, llq_11_60, llq_12_60,
                                   llq_1_80,llq_2_80, llq_3_80, llq_4_80, llq_5_80, llq_6_80, llq_7_80, llq_8_80, llq_9_80, llq_10_80, llq_11_80, llq_12_80,
                                   llq_1_80p,llq_2_80p, llq_3_80p, llq_4_80p, llq_5_80p, llq_6_80p, llq_7_80p, llq_8_80p, llq_9_80p, llq_10_80p, llq_11_80p, llq_12_80p))

vroom::vroom_write(llq_summ$likelihoods, here::here(asmnt_yr, 'rsch', 'output', 'llq_base_likes.csv'), delim = ",")
base::save(llq_summ, file = here::here(asmnt_yr, 'rsch', 'output', 'llq_base.RData'))


