# script to test what's going on with 2023 fishery caal

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




# set up model w/o 2023 fishery caal ----

new_base_lcomp_bin5 <- "2019.1e.5cm-2024"
test_mdl <- "test_caal"


# copy ss input files
if(!file.exists(here::here(new_year, 'mgmt', test_mdl, 'ss3.exe'))){
  start_ss_fldr(from = here::here(new_year, 'mgmt', new_base_lcomp_bin5),
                to = here::here(new_year, 'mgmt', test_mdl))
}

# read in datafile

dat_filename <- "GOAPcod2024Oct17_1e_5cm.dat"

data <- r4ss::SS_readdat_3.30(here::here(new_year, 'mgmt', new_base_lcomp_bin5, dat_filename))

# data$agecomp[which(data$agecomp$fleet == 1 & data$agecomp$year == 2023),]$fleet = -1
data$agecomp[which(data$agecomp$fleet == 2 & data$agecomp$year == 2023),]$fleet = -2
data$agecomp[which(data$agecomp$fleet == 3 & data$agecomp$year == 2023),]$fleet = -3

r4ss::SS_writedat_3.30(data, here::here(new_year, 'mgmt', test_mdl, dat_filename), overwrite = TRUE)


# run model
r4ss::run(dir = here::here(new_year, 'mgmt', test_mdl),
          skipfinished = FALSE,
          show_in_console = TRUE,
          verbose = FALSE)


test <- r4ss::SSgetoutput(dirvec = c(here::here(new_year, 'mgmt', new_base_lcomp_bin5), here::here(new_year, 'mgmt', test_mdl)))

test_summ <- r4ss::SSsummarize(test)

names(test_summ)

test$replist1$

