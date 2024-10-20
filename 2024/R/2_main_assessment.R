## Script to run 2024 GOA Pacific Cod Assessment (P. Hulson)


# user-defined function arguments ----

## ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))<
## ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))<

# day data pulled
dat_day <- "Oct17"

# 2023 accepted model
base_mdl <- "2019.1b-2023"

# full run for retro/jitter/mcmc/etc
full_run = FALSE

# run mcmc?
run_mcmc = TRUE

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


# define run parameters/load fcns ----

## current assessment year ----
new_year <- as.numeric(format(Sys.Date(), format = "%Y"))

## load functions ----
source_files <- c(list.files(here::here(new_year, "R", "assessment"), pattern = "*.r$"),
                  list.files(here::here(new_year, "R", "assessment"), pattern = "*.R$"))
purrr::map(here::here(new_year, "R", "assessment", source_files), source)
source(here::here(new_year, "R", "utils.R"))

## remove previous runs and results ----
unlink(here::here(new_year, "mgmt"), recursive = TRUE)
unlink(here::here(new_year, "output", "mscen"), recursive = TRUE)
unlink(here::here(new_year, "output", "retro"), recursive = TRUE)
unlink(here::here(new_year, "output", "loo"), recursive = TRUE)
unlink(here::here(new_year, "output", "jitter"), recursive = TRUE)
unlink(here::here(new_year, "output", "profile"), recursive = TRUE)
unlink(here::here(new_year, "output", "llq"), recursive = TRUE)

# run model variants ----

## define model variant names (model, dat, & ctl name) ----
### 2019.1b: updated base model ----
base_mdl_update <- "2019.1b-2024"
base_mdl_update_dat <- paste0("GOAPcod2024", dat_day, "_old.dat")
base_mdl_update_ctl <- "Model19_1b.ctl"
### 2019.1c: new base model ----
# includes:
# - corrected ll survey sd
# - corrected ll survey length comp bins
# - corrected ll survey timing
# - fishery iss set at number of hauls that data is used from
# - plus length bin set at 104 cm
# - correct season for twl survey caal
# - turn off recr forecase phase
# - fix rec forecast option
# - update l-w
new_base <- "2019.1c-2024"
new_base_dat <- paste0("GOAPcod2024", dat_day, "_1c.dat")
new_base_ctl <- "Model19_1c.ctl"
### 2019.1d: update ageing error ----
new_base_ae <- "2019.1d-2024"
new_base_ae_dat <- paste0("GOAPcod2024", dat_day, "_1d.dat")
new_base_ae_ctl <- "Model19_1d.ctl"
### 2019.1e: new fishery length comps ----
new_base_lcomp <- "2019.1e-2024"
new_base_lcomp_dat <- paste0("GOAPcod2024", dat_day, "_1e.dat")
new_base_lcomp_ctl <- "Model19_1e.ctl"
### 2019.1e.5: new fishery length comps at 5 cm bin ----
new_base_lcomp_bin5 <- "2019.1e.5cm-2024"
new_base_lcomp_bin5_dat <- paste0("GOAPcod2024", dat_day, "_1e_5cm.dat")
new_base_lcomp_bin5_ctl <- "Model19_1e.ctl"

# lists of model variants
mdls = c(base_mdl_update, new_base, new_base_ae, new_base_lcomp, new_base_lcomp_bin5)
dats = c(base_mdl_update_dat, new_base_dat, new_base_ae_dat, new_base_lcomp_dat, new_base_lcomp_bin5_dat)
ctls = c(base_mdl_update_ctl, new_base_ctl, new_base_ae_ctl, new_base_lcomp_ctl, new_base_lcomp_bin5_ctl)

## run model variants in parallel ----
run_mdl_vrnts(mdls = mdls,
              new_year = new_year,
              base_mdl = base_mdl,
              dats = dats,
              ctls = ctls)

# run model analyses ----
run_mdl_anlys(new_year = new_year,
              base_mdl = base_mdl_update,
              rec_mdl = new_base_lcomp_bin5,
              full_run = full_run,
              rec_ctl = new_base_lcomp_bin5_ctl,
              run_mcmc = run_mcmc)




