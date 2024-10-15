## Script to run 2024 GOA Pacific Cod Assessment (P. Hulson)


# user-defined function arguments ----

## ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))<
## ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))<

# 2024 recommended model
new_base_lcomp_bin5 <- "2019.1e.5cm-2024"

# full run for retro/jitter/mcmc/etc
full_run = FALSE

## ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))<
## ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))<


# load necessary packages ----
## cran packages ----
pkg_cran <- c("data.table",
              "tidyverse",
              "vroom",
              "here",
              "tictoc",
              "adnuts",
              "flextable", 
              "R.utils", 
              "parallel", 
              "doParallel", 
              "foreach")

# if not installed, then install
if(length(pkg_cran[which(pkg_cran %in% rownames(installed.packages()) == FALSE )]) > 0) {
  install.packages(pkg_cran[which(pkg_cran %in% rownames(installed.packages()) == FALSE)])
}

# load packages
lapply(pkg_cran, library, character.only = TRUE)

## github packages ----
pkg_git <- c("r4ss")

# if not installed, then install
if(!isTRUE("r4ss" %in% rownames(installed.packages()))) {
  devtools::install_github("r4ss/r4ss", force = TRUE)
}

# load packages
lapply(pkg_git, library, character.only = TRUE)


# define run parameters ----

# current assessment year
new_year <- as.numeric(format(Sys.Date(), format = "%Y"))

# define number of iterations
if(isTRUE(full_run)){
  iter <- 250000
  thin <- 1750
  warmup <- 250
} else{
  iter <-5000
  thin <- 5
  warmup <- 250
}

# set up model ----

# set up folder
if (!file.exists(here::here(new_year, "mgmt", new_base_lcomp_bin5, "mcmc"))) {
  dir.create(here::here(new_year, "mgmt", new_base_lcomp_bin5, "mcmc"), recursive = TRUE)
}
# copy model files
R.utils::copyDirectory(here::here(new_year, "mgmt", new_base_lcomp_bin5), 
                       here::here(new_year, "mgmt", new_base_lcomp_bin5, "mcmc"), recursive = FALSE)

# read starter file
starter <- r4ss::SS_readstarter(here::here(new_year, "mgmt", new_base_lcomp_bin5, "mcmc", "starter.ss"))
# change init vals source
starter$init_values_src <- 0
# write modified starter file
r4ss::SS_writestarter(starter, 
                      dir = here::here(new_year, "mgmt", new_base_lcomp_bin5, "mcmc"), 
                      overwrite = TRUE)


# run adnuts ----

# start timer
tictoc::tic()

mcmc_adnut <- adnuts::sample_rwm(model = 'ss3',
                                 path = here::here(new_year, "mgmt", new_base_lcomp_bin5, "mcmc"),
                                 iter = iter,
                                 chains = 7,
                                 warmup = warmup,
                                 thin = thin,
                                 mceval = FALSE,
                                 control = list(metric = 'mle'),
                                 skip_optimization = FALSE)

# end timer
mcmc_time <- tictoc::toc(quiet = TRUE)


# run mceval ----

# start timer
tictoc::tic()

r4ss::run(dir = here::here(new_year, "mgmt", new_base_lcomp_bin5, "mcmc"),
          extras = "-mceval",
          skipfinished = FALSE,
          show_in_console = TRUE)

# end timer
eval_time <- tictoc::toc(quiet = TRUE)

# Read output
mcmc_eval <- r4ss::SSgetMCMC(here::here(new_year, "mgmt", new_base_lcomp_bin5, "mcmc"))


# save results ----
if(isTRUE(full_run)){
  if (!dir.exists(here::here(new_year, "output", "mcmc"))) {
    dir.create(here::here(new_year, "output", "mcmc"), recursive = TRUE)
  }
  save(mcmc_adnut, file = here::here(new_year, "output", "mcmc", "mcmc_adnut.RData"))
  save(mcmc_eval, file = here::here(new_year, "output", "mcmc", "mcmc_eval.RData"))
}


# compute full run time ----
if(!isTRUE(full_run)){
  # total test time
  test_time <- round(((as.numeric(strsplit(mcmc_time$callback_msg, split = " ")[[1]][1])) / 60) +
                       ((as.numeric(strsplit(eval_time$callback_msg, split = " ")[[1]][1])) / 60), digits = 1)
  cat("Test time took", crayon::red$bold$underline$italic(test_time), "minutes", "\u2693","\n")
  # estimated run time
  runtime <- round(((as.numeric(strsplit(mcmc_time$callback_msg, split = " ")[[1]][1]) * 50) / 60) / 60 +
                     ((as.numeric(strsplit(eval_time$callback_msg, split = " ")[[1]][1]) * 50) / 60) / 60, digits = 1)
  cat("Full run will take", crayon::red$bold$underline$italic(runtime), "hours", "\u2693","\n")
} else{
  cat("All", crayon::green$bold$underline$italic('Done'), "\u2693","\n")
}


