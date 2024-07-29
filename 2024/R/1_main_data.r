## Get SS# data file for GOA Pacific cod
## adapted/generalized from Steve Barbeaux' files for generating SS files for EBS/AI Greenland Turbot ZTA, 2021-10-07, R version 4.05.01 64 bit
## Altered in 2022 by Pete Hulson
## Completely re-developed in 2024 by Pete Hulson
## Sections denoted with ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< need to be updated at the start of each assessment cycle

# install packages (if not installed) ----

# afscdata
# devtools::install_github("afsc-assessments/afscdata", force = TRUE)

# r4ss
# devtools::install_github("r4ss/r4ss", force = TRUE)

# load necessary libraries ----
libs <- c("r4ss",
          "data.table",
          "FSA",
          "lubridate",
          "afscdata",
          "tidyverse",
          "vroom",
          "here")

if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )]) > 0) {
  install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE)])
}

lapply(libs, library, character.only = TRUE)

# user-defined function arguments ----

## ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))<

# previous ss3 dat filename
old_dat_filename <- "GOAPcod2023Oct16.dat"

# run data queries? TRUE if first time running this script, or if data needs to be updated, FALSE for every run thereafter
query = FALSE

# run glm model for adf&g survey index? TRUE if first time running this script, FALSE for every run thereafter
run_glm <- FALSE

## ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))<

# automated function arguments ----

## data file specs ----

# current year ss3 dat filename
new_dat_filename <- paste0("GOAPcod", 
                              format(Sys.Date(), format = "%Y%b%d"),
                              ".dat")

# Current assessment year
new_dat_year <- as.numeric(format(Sys.Date(), format = "%Y"))

# format data for ss3 dat file
ss3_frmt = TRUE

## fishery data arguments ----

# the fmp region for this stock
area = 'goa'

# the fishery sub-areas
fsh_subarea = c("CG","PWSI","SE","SEI","WG","WY")

# catch data species label
fsh_sp = "PCOD"

# observer species code
fsh_sp_code = 202

# year in which to start the fishery age comp data
fsh_age_st_yr = 2007

# filter out small number of lenth observations
fltr = TRUE

## survey data arguments ----

# region of trawl sruvey
twl_srvy = 47

# survey species code
srv_sp = 21720

# type of survey index (numbers/biomass)
indx = 'num'

## comp data arguments ----

# length bins to use for length comp data
bin_width <- 1
min_size <- 0.5
max_size <- 116.5  # less than 1% of the fish in each year are 105 cm or larger (max less than 0.6%)
len_bins <- seq(min_size, max_size, bin_width)

# maximum age
max_age <- 10

# set up needed folders ----

# Make folders for output and plots
if (!file.exists(here::here(new_dat_year, "data", "raw"))){
  dir.create(here::here(new_dat_year, "data", "raw"))
}
if (!file.exists(here::here(new_dat_year, "data", "sql"))){
  dir.create(here::here(new_dat_year, "data", "sql"))
}
if (!file.exists(here::here(new_dat_year, "output"))){
  dir.create(here::here(new_dat_year, "output"))
}
if (!file.exists(here::here(new_dat_year, "plots"))){
  dir.create(here::here(new_dat_year, "plots", "assessment"), recursive = TRUE)
  dir.create(here::here(new_dat_year, "plots", "nonSS"), recursive = TRUE)
}

# Remove previous dat files from output folder
if (file.exists(here::here(new_dat_year, "output")) & length(list.files(here::here(new_dat_year, "output"), pattern = "GOAPcod")) > 0) {
  file.remove(here::here(new_dat_year, "output", list.files(here::here(new_dat_year, "output"), pattern = "GOAPcod")))
}

# source functions ----
source_files <- list.files(here::here(new_dat_year, "R", "get_data"), "*.r$")
purrr::map(here::here(new_dat_year, "R", "get_data", source_files), source)
source(here::here(new_dat_year, "R", "utils.r"))

# get ss data file ----

# read in previous assessment ss3 datafile
old_data <- r4ss::SS_readdat_3.30(here::here(new_dat_year, "data", old_dat_filename))

# get new ss3 dat
new_data <- get_data_goa_pcod(new_data = old_data,
                              new_file = new_dat_filename,
                              new_year = new_dat_year,
                              query = query,
                              fsh_sp = fsh_sp,
                              fsh_sp_code = fsh_sp_code,
                              fsh_subarea = fsh_subarea,
                              fsh_age_st_yr = fsh_age_st_yr,
                              twl_srvy = twl_srvy,
                              srv_sp = srv_sp,
                              area = area,
                              indx = indx,
                              run_glm = run_glm,
                              len_bins = len_bins,
                              fltr = fltr,
                              ss3_frmt = ss3_frmt,
                              max_age = max_age)


# Write out data script
r4ss::SS_writedat_3.30(new_data,
                       here::here(new_dat_year, "output", new_dat_filename), overwrite = TRUE)

