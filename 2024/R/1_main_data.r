## adapted/generalized from Steve Barbeaux' files for
## generating SS files for EBS/AI Greenland Turbot
## ZTA, 2021-10-07, R version 4.05.01 64 bit
## GOA Pacific cod
## Altered in 2022 by Pete Hulson
## Re-developed in 2024 by Pete Hulson
## Sections denoted with ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< need to be updated at the start of each assessment cycle

# load afscdata package
devtools::install_github("afsc-assessments/afscdata", force = TRUE)


# Load libraries
libs <- c("r4ss",
          "RODBC",
          "DBI",
          "dplyr",
          "data.table",
          "FSA",
          "lubridate",
          "tidyr",
          "afscdata",
          "purrr",
          "tidyverse",
          "tidytable",
          "vroom",
          "here")

if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )]) > 0) {
  install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE)])
}

lapply(libs, library, character.only = TRUE)

## DEFINE ALL CONSTANTS FOR THIS RUN

## ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))<

# previous SS DAT filename, if it exists
old_dat_filename <- "GOAPcod2022Oct25_wADFG.dat"

# SS DAT filename
new_dat_filename <- paste0("GOAPcod", 
                              format(Sys.Date(), format = "%Y%b%d"),
                              ".dat")

# Update IPHC & ADF&G survey indices? TRUE if first time running this script, FALSE for every run thereafter
update_adfg_iphc <- FALSE

## ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))<

# Current assessment year
new_dat_year <- as.numeric(format(Sys.Date(), format = "%Y"))

# is this a new SS DAT file
is_new_SS_DAT_file <- FALSE

# the FMP area for this stock
sp_area = "GOA"

# the GOA FMP sub-areas in the COUNCIL.COMPREHENSIVE_BLEND_CA database table
fsh_sp_area = c("CG","PWSI","SE","SEI","WG","WY")

# species label for AKFIN
fsh_sp_label = "PCOD"

# the fishery species code(s) for this stock/these stocks
fsh_sp_str = "202"

# year in which to start the fishery data
fsh_start_yr <- 1977

# the survey species code(s) for this stock/these stocks
srv_sp_str <- 21720

# year in which to start the bottom trawl survey data
srv_start_yr <- 1984

# length bins to use for fsh and srv length comp data
bin_width <- 1
min_size <- 0.5
max_size <- 116.5  # less than 1% of the fish in each year are 105 cm or larger (max less than 0.6%)
len_bins <- seq(min_size, max_size, bin_width)

# maximum age
max_age <- 10

## Get all the alternative data that isn't in AKFIN or AFSC databases
OLD_SEAS_GEAR_CATCH <- vroom::vroom(here::here(new_SS_dat_year, 'data', 'OLD_SEAS_GEAR_CATCH.csv'))
Larval_indices <- vroom::vroom(here::here(new_SS_dat_year, 'data', 'Larval_indices.csv'))
ALL_STATE_LENGTHS <- vroom::vroom(here::here(new_SS_dat_year, 'data', 'ALL_STATE_LENGTHS.csv'))
TEMPHW <- vroom::vroom(here::here(new_SS_dat_year, 'data', 'TEMPANDHEAT.csv'))
if(update_adfg_iphc == TRUE){
  ADFG_IPHC <- vroom::vroom(here::here(new_SS_dat_year, 'data', 'ADFG_IPHC.csv'))
}else{ADFG_IPHC <- vroom::vroom(here::here(new_SS_dat_year, 'data', 'ADFG_IPHC_updated.csv'))}

# Make folders for output and plots
if (!file.exists(here::here(new_SS_dat_year, "output"))){
  dir.create(here::here(new_SS_dat_year, "output"))
}
if (!file.exists(here::here(new_SS_dat_year, "plots"))){
  dir.create(here::here(new_SS_dat_year, "plots", "assessment"), recursive = TRUE)
  dir.create(here::here(new_SS_dat_year, "plots", "nonSS"), recursive = TRUE)
}

# Remove previous dat files from output folder
if (file.exists(here::here(new_SS_dat_year, "output")) & length(list.files(here::here(new_SS_dat_year, "output"), pattern = "GOAPcod")) > 0) {
  file.remove(here::here(new_SS_dat_year, "output", list.files(here::here(new_SS_dat_year, "output"), pattern = "GOAPcod")))
}

## Get all the functions for pulling GOA Pcod data


# source_files <- list.files(here::here(new_dat_year, "R", "get_data"), "*.r$")
# purrr::map(here::here(new_dat_year, "R", "get_data", source_files), source)
source(here::here(new_dat_year, "R", "utils.R"))
source(here::here(new_dat_year, "R", "get_data", "get_catch_goa_pcod.r"))
source(here::here(new_dat_year, "R", "get_data", "get_srvy_indices_goa_pcod.r"))
source(here::here(new_dat_year, "R", "get_data", "get_srvy_len_goa_pcod.r"))


source(here::here(new_SS_dat_year, "R", "data", "BIN_LEN_DATA.r"))
source(here::here(new_SS_dat_year, "R", "data", "cond_length_age_corFISH.r"))
source(here::here(new_SS_dat_year, "R", "data", "conditional_Length_AGE_cor.r"))
source(here::here(new_SS_dat_year, "R", "data", "find_ALF.r"))
source(here::here(new_SS_dat_year, "R", "data", "FISH_AGE_COMP.r"))
source(here::here(new_SS_dat_year, "R", "data", "FORMAT_AGE_MEANS1.r"))
source(here::here(new_SS_dat_year, "R", "data", "GET_DOM_AGE.r"))
source(here::here(new_SS_dat_year, "R", "data", "GET_GOA_ACOMP1.r"))
source(here::here(new_dat_year, "R", "get_data", "GET_GOA_BIOM.r"))
source(here::here(new_SS_dat_year, "R", "data", "GET_GOA_LCOMP1.r"))
source(here::here(new_SS_dat_year, "R", "data", "GET_GOA_LENCOM2.r"))
source(here::here(new_SS_dat_year, "R", "data", "GET_GOA_LL_RPN.r"))
source(here::here(new_SS_dat_year, "R", "data", "GET_LENGTH_BY_CATCH_GOA.R"))
source(here::here(new_SS_dat_year, "R", "data", "GET_SURV_AGE_cor.r"))

## Open up data base connections
db <- vroom::vroom(here::here(new_SS_dat_year, "database_specs.csv"))
afsc_user = db$username[db$database == "AFSC"]
afsc_pass = db$password[db$database == "AFSC"]
akfin_user = db$username[db$database == "AKFIN"]
akfin_pass = db$password[db$database == "AKFIN"]

AFSC = odbcConnect("AFSC", 
                   afsc_user, 
                   afsc_pass, 
                   believeNRows=FALSE)
CHINA = odbcConnect("AKFIN", 
                    akfin_user, 
                    akfin_pass, 
                    believeNRows=FALSE)

## Get all data for data file
source(here::here(new_SS_dat_year, "R", "data", "SBSS_GET_ALL_DATA_GOA_PCOD_cor.r"))

if (!is_new_SS_DAT_file){
  old_data <- r4ss::SS_readdat_3.30(here::here(new_SS_dat_year, "data", old_SS_dat_filename))
  new_data <- old_data}else{print(" Warning:  Need to enter old SS data file name")}

# # for testing functions
# new_data = new_data
# new_file = new_SS_dat_filename
# new_year = new_SS_dat_year
# inc_ADFG = TRUE
# sndz_lc = FALSE
# catch_table = TRUE
# AUXFCOMP = 3

# get new data file
new_data <- SBSS_GET_ALL_DATA(new_data = new_data,
                              new_file = new_SS_dat_filename,
                              new_year = new_SS_dat_year,
                              sp_area = sp_area,
                              fsh_sp_label = fsh_sp_label,
                              fsh_sp_area = fsh_sp_area,
                              fsh_sp_str = fsh_sp_str,
                              fsh_start_yr = fsh_start_yr,
                              srv_sp_str = srv_sp_str,
                              srv_start_yr = srv_start_yr,
                              len_bins = len_bins,
                              max_age = max_age,
                              is_new_SS_DAT_file = is_new_SS_DAT_file,
                              update_adfg_iphc = update_adfg_iphc,
                              inc_ADFG = TRUE,
                              sndz_lc = FALSE,
                              catch_table = TRUE,
                              AUXFCOMP = 3)

# Write out data script
r4ss::SS_writedat_3.30(new_data,
                       here::here(new_SS_dat_year, "output", new_SS_dat_filename), overwrite = TRUE)

# # test that the new file is readable
# test_dat <- r4ss::SS_readdat_3.30(here::here("output", new_SS_dat_filename), verbose = TRUE)





