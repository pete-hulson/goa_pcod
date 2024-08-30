## Get SS# data file for GOA Pacific cod
## adapted/generalized from Steve Barbeaux' files for generating SS files for EBS/AI Greenland Turbot ZTA, 2021-10-07, R version 4.05.01 64 bit
## Altered in 2022 by Pete Hulson
## Completely re-developed in 2024 by Pete Hulson
## Sections denoted with ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< need to be updated at the start of each assessment cycle

# install packages (if not installed) ----

# devtools::install_github("afsc-assessments/afscdata", force = TRUE)
# devtools::install_github("afsc-assessments/afscISS", force = TRUE)
# devtools::install_github("r4ss/r4ss", force = TRUE)

# load necessary libraries ----
libs <- c("r4ss",
          "data.table",
          "FSA",
          "lubridate",
          "afscdata",
          "tidyverse",
          "vroom",
          "here",
          "afscISS",
          "tictoc")

if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )]) > 0) {
  install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE)])
}

lapply(libs, library, character.only = TRUE)

tictoc::tic()

# user-defined function arguments ----

## ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))<

# previous ss3 dat filename
# note: need to copy in the following files into the 'data' folder:
# 1. accepted model dat file from previous assessment
# 2. accepted model ctl file from previous assessment
# 3. data_echo.ss_new file from accepted model
old_dat_filename <- "GOAPcod2023Oct16.dat"
old_ctl_filename <- "Model19_1b.ctl"

# run data queries? TRUE if first time running this script, or if data needs to be updated, FALSE for every run thereafter
query = TRUE

# run glm model for adf&g survey index? TRUE if first time running this script, FALSE for every run thereafter
run_glm = FALSE

# do you want to remove previous dat files
remove_dat = TRUE

## ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))<

# automated function arguments ----

## data file specs ----
# current year ss3 dat filename
new_dat_filename <- paste0("GOAPcod", 
                              format(Sys.Date(), format = "%Y%b%d"),
                              ".dat")

# Current assessment year
new_dat_year <- as.numeric(format(Sys.Date(), format = "%Y"))

## comp data arguments ----
# length bins to use for length comp data
bin_width <- 1
min_size <- 0.5
max_size <- 104.5  # less than 1% of the fish in each year are 105 cm or larger (max less than 0.6%)
len_bins <- seq(min_size, max_size, bin_width)
len_bins2 <- seq(min_size, max_size, 2)
len_bins5 <- c(4.5, 9.5, 14.5, 19.5, 24.5, 29.5, 34.5, 39.5, 44.5, 49.5, 54.5, 59.5, 64.5, 69.5, 74.5, 79.5, 84.5, 89.5, 94.5, 99.5, 104.5)
len_bins0 <- seq(min_size, 116.5, bin_width)

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
if(isTRUE(remove_dat)){
  if(file.exists(here::here(new_dat_year, "output")) & length(setdiff(list.files(here::here(new_dat_year, "output"), pattern = "GOAPcod"), list.files(here::here(new_dat_year, "output"), pattern = "_old"))) > 0) {
    file.remove(here::here(new_dat_year, "output", setdiff(list.files(here::here(new_dat_year, "output"), pattern = "GOAPcod"), list.files(here::here(new_dat_year, "output"), pattern = "_old"))))
  }
}

# source functions ----
source_files <- list.files(here::here(new_dat_year, "R", "get_data"), "*.r$")
purrr::map(here::here(new_dat_year, "R", "get_data", source_files), source)
source(here::here(new_dat_year, "R", "utils.r"))

# get ss3 data file ----

# read in previous assessment ss3 datafile
old_data <- r4ss::SS_readdat_3.30(here::here(new_dat_year, "data", old_dat_filename))

# fix survey timing
old_data$fleetinfo = old_data$fleetinfo %>% 
  dplyr::mutate(surveytiming = dplyr::case_when(surveytiming == 1007 ~ 1,
                                                .default = surveytiming))
old_data$surveytiming[old_data$surveytiming == 1007] = 1

## file with all c series changes ----
new_data <- get_data_goa_pcod(new_data = old_data,
                              new_file = new_dat_filename,
                              new_year = new_dat_year,
                              query = query,
                              run_glm = run_glm,
                              len_bins = len_bins)
r4ss::SS_writedat_3.30(new_data,
                       here::here(new_dat_year, "output", new_dat_filename), overwrite = TRUE)

tictoc::toc()

## data file to investigate c series ----
new_data <- get_data_goa_pcod(new_data = old_data,
                              new_file = new_dat_filename,
                              new_year = new_dat_year,
                              query = FALSE,
                              run_glm = run_glm,
                              len_bins = len_bins0)
r4ss::SS_writedat_3.30(new_data,
                       here::here(new_dat_year, "output", paste0(substr(new_dat_filename, start = 1, stop = (nchar(new_dat_filename) - 4)), "_new.dat")), overwrite = TRUE)

## with spline ageing error ----
new_data <- get_data_goa_pcod(new_data = old_data,
                              new_file = new_dat_filename,
                              new_year = new_dat_year,
                              query = FALSE,
                              run_glm = run_glm,
                              len_bins = len_bins,
                              update_ae = TRUE)
r4ss::SS_writedat_3.30(new_data,
                       here::here(new_dat_year, "output",
                                  paste0(substr(new_dat_filename, start = 1, stop = (nchar(new_dat_filename) - 4)), "_ae.dat")), overwrite = TRUE)

## with new fish len comps ----
new_data <- get_data_goa_pcod(new_data = old_data,
                              new_file = new_dat_filename,
                              new_year = new_dat_year,
                              query = FALSE,
                              run_glm = run_glm,
                              len_bins = len_bins,
                              new_lcomp = TRUE)
r4ss::SS_writedat_3.30(new_data,
                       here::here(new_dat_year, "output", 
                                  paste0(substr(new_dat_filename, start = 1, stop = (nchar(new_dat_filename) - 4)), "_lcomp.dat")), overwrite = TRUE)

## new len comps at 2 cm bins ----
new_data <- get_data_goa_pcod(new_data = old_data,
                              new_file = new_dat_filename,
                              new_year = new_dat_year,
                              query = FALSE,
                              run_glm = run_glm,
                              len_bins = len_bins2,
                              new_lcomp = TRUE)
r4ss::SS_writedat_3.30(new_data,
                       here::here(new_dat_year, "output",
                                  paste0(substr(new_dat_filename, start = 1, stop = (nchar(new_dat_filename) - 4)), "_lcomp_bin2.dat")), overwrite = TRUE)

## new len comps at 5 cm bins ----
new_data <- get_data_goa_pcod(new_data = old_data,
                              new_file = new_dat_filename,
                              new_year = new_dat_year,
                              query = FALSE,
                              run_glm = run_glm,
                              len_bins = len_bins5,
                              new_lcomp = TRUE)
r4ss::SS_writedat_3.30(new_data,
                       here::here(new_dat_year, "output",
                                  paste0(substr(new_dat_filename, start = 1, stop = (nchar(new_dat_filename) - 4)), "_lcomp_bin5.dat")), overwrite = TRUE)

# get ss3 ctl file ----

# read in previous assessment ss3 ctl
old_ctl <- r4ss::SS_readctl_3.30(here::here(new_dat_year, "data", old_ctl_filename))

## reset params this one time ----
# reset F ballpark to 0
old_ctl$F_ballpark <- 0

# reset q params for surveys not fit to 0
old_ctl$Q_parms[which(rownames(old_ctl$Q_parms) == "LnQ_base_SPAWN(8)"), 3] <- 0
old_ctl$Q_parms[which(rownames(old_ctl$Q_parms) == "Q_power_SPAWN(8)"), 3] <- 0
old_ctl$Q_parms[which(rownames(old_ctl$Q_parms) == "LnQ_base_Seine(9)"), 3] <- 0
old_ctl$Q_parms[which(rownames(old_ctl$Q_parms) == "Q_power_Seine(9)"), 3] <- 0

# reset selex patterns for surveys not fit to 0
old_ctl$size_selex_types[which(rownames(old_ctl$size_selex_types) == "IPHCLL"), 1] <- 0
old_ctl$size_selex_types[which(rownames(old_ctl$size_selex_types) == "IPHCLL"), 4] <- 0
old_ctl$size_selex_types[which(rownames(old_ctl$size_selex_types) == "ADFG"), 1] <- 0
old_ctl$size_selex_types[which(rownames(old_ctl$size_selex_types) == "ADFG"), 4] <- 0
old_ctl$age_selex_types[, 1] <- 0
old_ctl$age_selex_types[, 4] <- 0
old_ctl$size_selex_parms$HI[which(old_ctl$size_selex_parms$HI > 100)] = 100

## reset params annually ----
# reset end year in block designs
old_ctl$Block_Design[[1]][length(old_ctl$Block_Design[[1]])] <- new_dat_year
old_ctl$Block_Design[[2]][length(old_ctl$Block_Design[[2]])] <- new_dat_year
old_ctl$Block_Design[[3]][length(old_ctl$Block_Design[[3]])] <- new_dat_year

# reset end year for recr_devs
old_ctl$MainRdevYrLast <- new_dat_year - 2

# update weight-length parameters
wtlen <- wt_len(new_dat_year)
old_ctl$MG_parms[which(rownames(old_ctl$MG_parms) == "Wtlen_1_Fem_GP_1"), 3] <- wtlen[1]
old_ctl$MG_parms[which(rownames(old_ctl$MG_parms) == "Wtlen_2_Fem_GP_1"), 3] <- wtlen[2]

# write base model ctl
r4ss::SS_writectl_3.30(ctllist = old_ctl,
                       outfile = here::here(new_dat_year, "output", old_ctl_filename),
                       overwrite = TRUE)

# update ageing error parameters
ae <- vroom::vroom(here::here(new_dat_year, 'data', 'ageing_error', 'ResultsGOA_Linear', 'Pcod SS3_format_Reader1.csv')) %>% 
  tidytable::filter(...1 == 'SD') %>% 
  tidytable::pivot_longer() %>% 
  tidytable::select(value)

old_ctl$MG_parms[which(rownames(old_ctl$MG_parms) == "AgeKeyParm1"), 3] <- 1
old_ctl$MG_parms[which(rownames(old_ctl$MG_parms) == "AgeKeyParm5"), 3] <- as.numeric(ae[2])
old_ctl$MG_parms[which(rownames(old_ctl$MG_parms) == "AgeKeyParm6"), 3] <- as.numeric(ae[length(ae$value)])

# write base model ctl with updated ageing error
r4ss::SS_writectl_3.30(ctllist = old_ctl,
                       outfile = here::here(new_dat_year, "output", "updated_ae.ctl"),
                       overwrite = TRUE)
