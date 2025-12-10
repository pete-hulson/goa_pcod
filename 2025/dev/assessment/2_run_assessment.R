## Script to run 2025 GOA Pacific Cod Assessment (P. Hulson)


# user-defined function arguments ----

## ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))<
## ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))<

# day data pulled
dat_day <- "Dec08"

# 2024 accepted model
base_mdl <- "24.0"

# full run for retro/jitter/mcmc/etc
full_run = FALSE

# run mcmc?
run_mcmcz = FALSE

# remove previous run files?
remove_files = TRUE

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
              "parallelly",
              "foreach",
              "SimDesign")

# if not installed, then install
if(length(pkg_cran[which(pkg_cran %in% rownames(installed.packages()) == FALSE )]) > 0) {
  install.packages(pkg_cran[which(pkg_cran %in% rownames(installed.packages()) == FALSE)])
}

# load packages
lapply(pkg_cran, library, character.only = TRUE)

## github packages ----
pkg_git <- c("r4ss",
             "rema")

# if not installed, then install
if(length(pkg_git[which(pkg_git %in% rownames(installed.packages()) == FALSE )]) > 0) {
  devtools::install_github("r4ss/r4ss", force = TRUE)
  devtools::install_github("afsc-assessments/rema", dependencies = TRUE, build_vignettes = TRUE)
}

# load packages
lapply(pkg_git, library, character.only = TRUE)


# define run parameters/load fcns ----

## current assessment year ----
new_year <- as.numeric(format(Sys.Date(), format = "%Y"))

## load functions ----
source_files <- c(list.files(here::here(new_year, "R"), pattern = "*.r$"),
                  list.files(here::here(new_year, "R"), pattern = "*.R$"))
purrr::map(here::here(new_year, "R", source_files), source)

## remove previous runs and results ----
if(isTRUE(remove_files)){
  unlink(here::here(new_year, "mgmt"), recursive = TRUE, force = TRUE)
  unlink(here::here(new_year, "output", "mscen"), recursive = TRUE, force = TRUE)
  unlink(here::here(new_year, "output", "retro"), recursive = TRUE, force = TRUE)
  unlink(here::here(new_year, "output", "loo"), recursive = TRUE, force = TRUE)
  unlink(here::here(new_year, "output", "aoi"), recursive = TRUE, force = TRUE)
  unlink(here::here(new_year, "output", "jitter"), recursive = TRUE, force = TRUE)
  unlink(here::here(new_year, "output", "profile"), recursive = TRUE, force = TRUE)
  unlink(here::here(new_year, "output", "llq"), recursive = TRUE, force = TRUE)
  unlink(here::here(new_year, "output", "apport"), recursive = TRUE, force = TRUE)
  unlink(here::here(new_year, "output", "mcmc"), recursive = TRUE, force = TRUE)
  unlink(here::here(new_year, "output", "fofl_prev"), recursive = TRUE, force = TRUE)
}



# set up model folder ----

## copy ss files from last year's accepted assessment ----
start_ss_fldr(from = here::here(new_year - 1, 'mgmt', base_mdl), 
              to = here::here(new_year, 'mgmt', base_mdl),
              update_exe = TRUE,
              ver_exe = 'v3.30.22.1')

## update dat/ctl/etc files ----
update_ss3_files(asmnt_yr = new_year, 
                 folder = 'mgmt',
                 mdl = base_mdl, 
                 dat_filename = paste0('GOAPcod2025', dat_day, '.dat'),
                 ctl_filename = 'Model24_0.ctl')


# run model ----
run_ss3_model(asmnt_yr = new_year, 
              folder = 'mgmt',
              mdl = base_mdl, 
              ctl_filename = 'Model24_0.ctl',
              iters = 2)

# run model analyses ----
run_mdl_anlys(new_year = new_year,
              base_mdl = base_mdl,
              rec_mdl = base_mdl,
              full_run = full_run,
              rec_ctl = 'Model24_0.ctl',
              run_mcmcz = run_mcmcz)




