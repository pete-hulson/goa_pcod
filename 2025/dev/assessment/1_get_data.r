## Get SS3 data file for GOA Pacific cod
## adapted/generalized from Steve Barbeaux' files for generating SS files for EBS/AI Greenland Turbot ZTA, 2021-10-07, R version 4.05.01 64 bit
## Altered in 2022 by Pete Hulson
## Completely re-developed in 2024 by Pete Hulson
## Sections denoted with ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< need to be updated at the start of each assessment cycle

# user-defined function arguments ----

## ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))<
## ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))<

# previous ss3 dat filename
# note: need to copy in the following files into the 'data' folder:
# 1. accepted model dat file from previous assessment
# 2. accepted model ctl file from previous assessment
# 3. data_echo.ss_new file from accepted model
# 4. data/historical folder
# 5. delta_glm_1-7-2.get file to get adf&g trawl survey index
# 6. other_indices.csv that includes age-0 beach seine indices (updated by hand)
# 7. fish_lfreq_state.csv in the data/raw folder (updated by hand)
# 8. data/ageing_error folder (note to updated reader-tester data on occasion)
old_dat_filename <- "GOAPcod2024Oct17.dat"
old_ctl_filename <- "Model24_0.ctl"

# run data queries? TRUE if first time running this script, or if data needs to be updated, FALSE for every run thereafter
query = TRUE

# run glm model for adf&g survey index? TRUE if first time running this script, FALSE for every run thereafter
run_glm = FALSE

# do you want to remove previous dat files
remove_dat = TRUE

## ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))<
## ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))< ~~~~ <*)))<

# load necessary packages ----
## cran packages ----
pkg_cran <- c("data.table",
              "FSA",
              "lubridate",
              "tidyverse",
              "vroom",
              "here",
              "tictoc",
              "SimDesign")

# if not installed, then install
if(length(pkg_cran[which(pkg_cran %in% rownames(installed.packages()) == FALSE )]) > 0) {
  install.packages(pkg_cran[which(pkg_cran %in% rownames(installed.packages()) == FALSE)])
}

# load packages
lapply(pkg_cran, library, character.only = TRUE)

## github packages ----
pkg_git <- c("r4ss",
             "afscdata",
             "afscISS",
             "AgeingError")

# if not installed, then install
if(!isTRUE("r4ss" %in% rownames(installed.packages()))) {
  devtools::install_github("r4ss/r4ss", force = TRUE)
}
if(!isTRUE("afscdata" %in% rownames(installed.packages()))) {
  devtools::install_github("afsc-assessments/afscdata", force = TRUE)
}
if(!isTRUE("afscISS" %in% rownames(installed.packages()))) {
  devtools::install_github("afsc-assessments/afscISS", force = TRUE)
}
if(!isTRUE("AgeingError" %in% rownames(installed.packages()))) {
  devtools::install_github("pfmc-assessments/AgeingError", force = TRUE)
}

# load packages
lapply(pkg_git, library, character.only = TRUE)

# automated function arguments ----

## data file specs ----
# current year ss3 dat filename
new_dat_filename <- paste0("GOAPcod", 
                              format(Sys.Date(), format = "%Y%b%d"),
                              ".dat")

# Current assessment year
new_year <- as.numeric(format(Sys.Date(), format = "%Y"))

## comp data arguments ----
# length bins to use for length comp data
len_bins <- c(4.5, 9.5, 14.5, 19.5, 24.5, 29.5, 34.5, 39.5, 44.5, 49.5, 54.5, 59.5, 64.5, 69.5, 74.5, 79.5, 84.5, 89.5, 94.5, 99.5, 104.5)

# set up needed folders ----

# Make folders for data queries, raw data, and model input files
if (!file.exists(here::here(new_year, "data", "raw"))){
  dir.create(here::here(new_year, "data", "raw"))
}
if (!file.exists(here::here(new_year, "data", "output"))){
  dir.create(here::here(new_year, "data", "output"))
}
if (!file.exists(here::here(new_year, "data", "sql"))){
  dir.create(here::here(new_year, "data", "sql"))
}
if (!file.exists(here::here(new_year, "output", "mdl_input"))){
  dir.create(here::here(new_year, "output", "mdl_input"))
}
if (!file.exists(here::here(new_year, "output", "ageing_error"))){
  dir.create(here::here(new_year, "output", "ageing_error"))
}

# Remove previous dat files from output folder
if(isTRUE(remove_dat)){
  if(file.exists(here::here(new_year, "output", "mdl_input")) & length(setdiff(list.files(here::here(new_year, "output", "mdl_input"), pattern = "GOAPcod"), list.files(here::here(new_year, "output", "mdl_input"), pattern = "_old"))) > 0) {
    file.remove(here::here(new_year, "output", "mdl_input", setdiff(list.files(here::here(new_year, "output", "mdl_input"), pattern = "GOAPcod"), list.files(here::here(new_year, "output", "mdl_input"), pattern = "_old"))))
  }
  if(file.exists(here::here(new_year, "output", "mdl_input")) & length(list.files(here::here(new_year, "output", "mdl_input"), pattern = ".ctl")) > 0) {
    file.remove(here::here(new_year, "output", "mdl_input", list.files(here::here(new_year, "output", "mdl_input"), pattern = ".ctl")))
  }
}

# source functions ----
source_files <- list.files(here::here(new_year, "R", "get_data"), "*.r$")
purrr::map(here::here(new_year, "R", "get_data", source_files), source)
source(here::here(new_year, "R", "utils.r"))

# get ss3 data files ----

# read in previous assessment ss3 datafile
old_data <- r4ss::SS_readdat_3.30(here::here(new_year, "data", old_dat_filename))

## get data file ----
new_data <- get_data_goa_pcod(new_data = old_data,
                              new_file = new_dat_filename,
                              new_year = new_year,
                              query = query,
                              run_glm = run_glm,
                              len_bins = len_bins)
r4ss::SS_writedat_3.30(new_data,
                       here::here(new_year, "output", "mdl_input",
                                  paste0(substr(new_dat_filename, start = 1, stop = (nchar(new_dat_filename) - 4)), ".dat")), overwrite = TRUE)


# get ss3 ctl files ----

# read in previous assessment ss3 ctl
old_ctl <- r4ss::SS_readctl_3.30(here::here(new_year, "data", old_ctl_filename))

## reset params this one time ----

# reset q params for surveys not fit to 0
old_ctl$Q_parms[which(rownames(old_ctl$Q_parms) == "LnQ_base_SPAWN(8)"), 3] <- 0
old_ctl$Q_parms[which(rownames(old_ctl$Q_parms) == "Q_power_SPAWN(8)"), 3] <- 0
old_ctl$Q_parms[which(rownames(old_ctl$Q_parms) == "LnQ_base_Seine(9)"), 3] <- 0
old_ctl$Q_parms[which(rownames(old_ctl$Q_parms) == "Q_power_Seine(9)"), 3] <- 0

# reset selex patterns for surveys not fit to 0
old_ctl$size_selex_types$Pattern[which(rownames(old_ctl$size_selex_types) == "IPHCLL")] <- 0
old_ctl$size_selex_types$Special[which(rownames(old_ctl$size_selex_types) == "IPHCLL")] <- 0
old_ctl$size_selex_types$Pattern[which(rownames(old_ctl$size_selex_types) == "ADFG")] <- 0
old_ctl$size_selex_types$Special[which(rownames(old_ctl$size_selex_types) == "ADFG")] <- 0
old_ctl$age_selex_types$Pattern[which(rownames(old_ctl$age_selex_types) == "IPHCLL")] <- 0
old_ctl$age_selex_types$Pattern[which(rownames(old_ctl$age_selex_types) == "ADFG")] <- 0

# set max bound for selex params to 100 cm
old_ctl$size_selex_parms$HI[which(old_ctl$size_selex_parms$HI > 100)] = 100

## reset params annually ----
# reset end year in block designs
old_ctl$Block_Design[[1]][length(old_ctl$Block_Design[[1]])] <- new_year
old_ctl$Block_Design[[2]][length(old_ctl$Block_Design[[2]])] <- new_year
old_ctl$Block_Design[[3]][length(old_ctl$Block_Design[[3]])] <- new_year

# reset end year for recr_devs
# note: when fcast_rec_option = 0 in forecast file, then use the following:
# old_ctl$MainRdevYrLast <- new_year - 2
# note: when fcast_rec_option = 4 in forecast file, then use the following:
old_ctl$MainRdevYrLast <- new_year

## write model 2019.1b ctl ----
r4ss::SS_writectl_3.30(ctllist = old_ctl,
                       outfile = here::here(new_year, "output", "mdl_input", old_ctl_filename),
                       overwrite = TRUE)


## write model 2019.1c ctl ----
# turn off forecast rec phase
old_ctl$Fcast_recr_phase = -1
# update weight-length parameters
wtlen <- wt_len(new_year)
old_ctl$MG_parms[which(rownames(old_ctl$MG_parms) == "Wtlen_1_Fem_GP_1"), 3] <- wtlen[1]
old_ctl$MG_parms[which(rownames(old_ctl$MG_parms) == "Wtlen_2_Fem_GP_1"), 3] <- wtlen[2]
# write ctl
r4ss::SS_writectl_3.30(ctllist = old_ctl,
                       outfile = here::here(new_year, "output", "mdl_input", "Model19_1c.ctl"),
                       overwrite = TRUE)

## write model 2019.1d and 2019.1e ctls ----
# remove ageing error sds
old_ctl$MG_parms <- old_ctl$MG_parms[-which(rownames(old_ctl$MG_parms) == "AgeKeyParm1"),]
old_ctl$MG_parms <- old_ctl$MG_parms[-which(rownames(old_ctl$MG_parms) == "AgeKeyParm2"),]
old_ctl$MG_parms <- old_ctl$MG_parms[-which(rownames(old_ctl$MG_parms) == "AgeKeyParm3"),]
old_ctl$MG_parms <- old_ctl$MG_parms[-which(rownames(old_ctl$MG_parms) == "AgeKeyParm4"),]
old_ctl$MG_parms <- old_ctl$MG_parms[-which(rownames(old_ctl$MG_parms) == "AgeKeyParm5"),]
old_ctl$MG_parms <- old_ctl$MG_parms[-which(rownames(old_ctl$MG_parms) == "AgeKeyParm6"),]
old_ctl$MG_parms <- old_ctl$MG_parms[-which(rownames(old_ctl$MG_parms) == "AgeKeyParm7"),]
# remove bias
old_ctl$MG_parms_tv <- old_ctl$MG_parms_tv[-which(rownames(old_ctl$MG_parms_tv) == "AgeKeyParm2_BLK6repl_1976"),]
old_ctl$MG_parms_tv <- old_ctl$MG_parms_tv[-which(rownames(old_ctl$MG_parms_tv) == "AgeKeyParm3_BLK6repl_1976"),]
# write ctl
r4ss::SS_writectl_3.30(ctllist = old_ctl,
                       outfile = here::here(new_year, "output", "mdl_input", "Model19_1d.ctl"),
                       overwrite = TRUE)
r4ss::SS_writectl_3.30(ctllist = old_ctl,
                       outfile = here::here(new_year, "output", "mdl_input", "Model19_1e.ctl"),
                       overwrite = TRUE)
r4ss::SS_writectl_3.30(ctllist = old_ctl,
                       outfile = here::here(new_year, "output", "mdl_input", "Model24_0.ctl"),
                       overwrite = TRUE)






