# script to compile tables for safe document
# developed in 2024 by p hulson

# load necessary packages ----
## cran packages ----
pkg_cran <- c("tidyverse",
              "vroom",
              "here",
              "scales",
              "SimDesign",
              "data.table")

# if not installed, then install
if(length(pkg_cran[which(pkg_cran %in% rownames(installed.packages()) == FALSE )]) > 0) {
  install.packages(pkg_cran[which(pkg_cran %in% rownames(installed.packages()) == FALSE)])
}

# load packages
lapply(pkg_cran, library, character.only = TRUE)

## github packages ----
pkg_git <- c("r4ss",
             "afscdata")

# if not installed, then install
if(!isTRUE("r4ss" %in% rownames(installed.packages()))) {
  devtools::install_github("r4ss/r4ss", force = TRUE)
}
if(!isTRUE("afscdata" %in% rownames(installed.packages()))) {
  devtools::install_github("afsc-assessments/afscdata", force = TRUE)
}

# load packages
lapply(pkg_git, library, character.only = TRUE)

# set up ----
# Current assessment year
new_year <- as.numeric(format(Sys.Date(), format = "%Y"))
# recommended model name
rec_mdl <- "24.0"
# last year's model
prev_mdl <- "2019.1b-2023"

# load functions ----
source_files <- c(list.files(here::here(new_year, "R"), pattern = "*.r$"),
                  list.files(here::here(new_year, "R"), pattern = "*.R$"))
purrr::map(here::here(new_year, "R", source_files), source)

# remove previous tables and plots ----
unlink(here::here(new_year, "output", "safe_tables"), recursive = TRUE)
# unlink(here::here(new_year, "output", "safe_plots"), recursive = TRUE)

# get tables for safe ----
safe_tbls(new_year = new_year,
          rec_mdl = rec_mdl,
          prev_mdl = prev_mdl)
  
  
  
  
  
  
  