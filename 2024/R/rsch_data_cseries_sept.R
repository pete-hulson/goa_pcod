# Script to run 2023 GOA Pacific Cod Assessment analyses (P. Hulson)

# Load required packages & define parameters ----

# load necessary pkgs
libs <- c("data.table",
          "dplyr",
          "ggplot2",
          "magrittr", 
          "r4ss")

if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )]) > 0) {
  install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE)])}

lapply(libs, library, character.only = TRUE)

# assessment year
asmnt_yr <- as.numeric(format(Sys.Date(), format = "%Y"))

# day data pulled
dat_day <- "Sep02"

# base model from which to get files
base_mdl_update <- "2019.1b-2024"

# source functions
source_files <- list.files(here::here(asmnt_yr, "R", "assessment"), "*.r$")
purrr::map(here::here(asmnt_yr, "R", "assessment", source_files), source)
source(here::here(asmnt_yr, "R", "utils.r"))

# create c series folder
dir.create(here::here(asmnt_yr, 'rsch', 'cseries'), recursive = TRUE)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2019.1c.1 ----
# includes:
# .1 corrected ll survey sd

new_base.1 <- "2019.1c.1-2024"

## copy ss input files ----
if(!file.exists(here::here(asmnt_yr, 'rsch', 'cseries', new_base.1, 'ss3.exe'))){
  start_ss_fldr(from = here::here(asmnt_yr, 'rsch', base_mdl_update),
                to = here::here(asmnt_yr, 'rsch', 'cseries', new_base.1))
}

## update files ----
update_ss3_files(asmnt_yr, 
                 folder = 'rsch/cseries',
                 mdl = new_base.1, 
                 dat_filename = paste0("GOAPcod2024", dat_day, "_old.dat"),
                 ctl_in = "Model19_1b.ctl",
                 ctl_out = "Model19_1c.ctl")

## correct ll survey sd ----
dat_orig <- r4ss::SS_readdat_3.30(here::here(asmnt_yr, 'rsch', 'cseries', new_base.1, paste0("GOAPcod2024", dat_day, "_old.dat")))
dat <- r4ss::SS_readdat_3.30(here::here(asmnt_yr, 'output', paste0("GOAPcod2024", dat_day, "_new.dat")))
dat_orig$CPUE = dat$CPUE
r4ss::SS_writedat_3.30(dat_orig,
                       here::here(asmnt_yr, 'rsch', 'cseries', new_base.1, paste0("GOAPcod2024", dat_day, "_old.dat")), overwrite = TRUE)

## run model ----
run_ss3_model(asmnt_yr, 
              folder = 'rsch/cseries',
              mdl = new_base.1,
              ctl_filename = "Model19_1c.ctl")

## get and plot model output ----
# get output
new_base.1_res <- r4ss::SS_output(dir = here::here(asmnt_yr, 'rsch', 'cseries', new_base.1))
# if exists, delete plot folder
if(file.exists(here::here(asmnt_yr, 'rsch', 'cseries', new_base.1, 'plots'))){
  unlink(here::here(asmnt_yr, 'rsch', 'cseries', new_base.1, 'plots'), recursive = TRUE)
}
# plot results
r4ss::SS_plots(new_base.1_res,
               printfolder = "",
               dir = here::here(asmnt_yr, 'rsch', 'cseries', new_base.1, "plots"))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2019.1c.2 ----
# includes:
# .2 corrected ll survey length comp bins

new_base.2 <- "2019.1c.2-2024"

## copy ss input files ----
if(!file.exists(here::here(asmnt_yr, 'rsch', 'cseries', new_base.2, 'ss3.exe'))){
  start_ss_fldr(from = here::here(asmnt_yr, 'rsch', base_mdl_update),
                to = here::here(asmnt_yr, 'rsch', 'cseries', new_base.2))
}

## update files ----
update_ss3_files(asmnt_yr, 
                 folder = 'rsch/cseries',
                 mdl = new_base.2, 
                 dat_filename = paste0("GOAPcod2024", dat_day, "_old.dat"),
                 ctl_in = "Model19_1b.ctl",
                 ctl_out = "Model19_1c.ctl")

## correct ll survey length comp bins ----
dat_orig <- r4ss::SS_readdat_3.30(here::here(asmnt_yr, 'rsch', 'cseries', new_base.2, paste0("GOAPcod2024", dat_day, "_old.dat")))
dat <- r4ss::SS_readdat_3.30(here::here(asmnt_yr, 'output', paste0("GOAPcod2024", dat_day, "_new.dat")))
dat_orig$lencomp[dat_orig$lencomp$fleet == 5, 7:length(colnames(dat_orig$lencomp))] = dat$lencomp[dat$lencomp$fleet == 5, 7:length(colnames(dat$lencomp))]
r4ss::SS_writedat_3.30(dat_orig,
                       here::here(asmnt_yr, 'rsch', 'cseries', new_base.2, paste0("GOAPcod2024", dat_day, "_old.dat")), overwrite = TRUE)

## run model ----
run_ss3_model(asmnt_yr, 
              folder = 'rsch/cseries',
              mdl = new_base.2,
              ctl_filename = "Model19_1c.ctl")

## get and plot model output ----
# get output
new_base.2_res <- r4ss::SS_output(dir = here::here(asmnt_yr, 'rsch', 'cseries', new_base.2))
# if exists, delete plot folder
if(file.exists(here::here(asmnt_yr, 'rsch', 'cseries', new_base.2, 'plots'))){
  unlink(here::here(asmnt_yr, 'rsch', 'cseries', new_base.2, 'plots'), recursive = TRUE)
}
# plot results
r4ss::SS_plots(new_base.2_res,
               printfolder = "",
               dir = here::here(asmnt_yr, 'rsch', 'cseries', new_base.2, "plots"))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2019.1c.3 ----
# includes:
# .3 corrected ll survey timing

new_base.3 <- "2019.1c.3-2024"

## copy ss input files ----
if(!file.exists(here::here(asmnt_yr, 'rsch', 'cseries', new_base.3, 'ss3.exe'))){
  start_ss_fldr(from = here::here(asmnt_yr, 'rsch', base_mdl_update),
                to = here::here(asmnt_yr, 'rsch', 'cseries', new_base.3))
}

## update files ----
update_ss3_files(asmnt_yr, 
                 folder = 'rsch/cseries',
                 mdl = new_base.3, 
                 dat_filename = paste0("GOAPcod2024", dat_day, "_old.dat"),
                 ctl_in = "Model19_1b.ctl",
                 ctl_out = "Model19_1c.ctl")

## correct ll survey length comp timing ----
dat_orig <- r4ss::SS_readdat_3.30(here::here(asmnt_yr, 'rsch', 'cseries', new_base.3, paste0("GOAPcod2024", dat_day, "_old.dat")))
dat <- r4ss::SS_readdat_3.30(here::here(asmnt_yr, 'output', paste0("GOAPcod2024", dat_day, "_new.dat")))
dat_orig$lencomp[dat_orig$lencomp$fleet == 5, 'month'] = dat$lencomp[dat$lencomp$fleet == 5, 'month']
r4ss::SS_writedat_3.30(dat_orig,
                       here::here(asmnt_yr, 'rsch', 'cseries', new_base.3, paste0("GOAPcod2024", dat_day, "_old.dat")), overwrite = TRUE)

## run model ----
run_ss3_model(asmnt_yr, 
              folder = 'rsch/cseries',
              mdl = new_base.3,
              ctl_filename = "Model19_1c.ctl")

## get and plot model output ----
# get output
new_base.3_res <- r4ss::SS_output(dir = here::here(asmnt_yr, 'rsch', 'cseries', new_base.3))
# if exists, delete plot folder
if(file.exists(here::here(asmnt_yr, 'rsch', 'cseries', new_base.3, 'plots'))){
  unlink(here::here(asmnt_yr, 'rsch', 'cseries', new_base.3, 'plots'), recursive = TRUE)
}
# plot results
r4ss::SS_plots(new_base.3_res,
               printfolder = "",
               dir = here::here(asmnt_yr, 'rsch', 'cseries', new_base.3, "plots"))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2019.1c.4 ----
# includes:
# .4 fishery iss set at number of hauls that data is used from

new_base.4 <- "2019.1c.4-2024"

## copy ss input files ----
if(!file.exists(here::here(asmnt_yr, 'rsch', 'cseries', new_base.4, 'ss3.exe'))){
  start_ss_fldr(from = here::here(asmnt_yr, 'rsch', base_mdl_update),
                to = here::here(asmnt_yr, 'rsch', 'cseries', new_base.4))
}

## update files ----
update_ss3_files(asmnt_yr, 
                 folder = 'rsch/cseries',
                 mdl = new_base.4, 
                 dat_filename = paste0("GOAPcod2024", dat_day, "_old.dat"),
                 ctl_in = "Model19_1b.ctl",
                 ctl_out = "Model19_1c.ctl")

## change fishery iss ----
dat_orig <- r4ss::SS_readdat_3.30(here::here(asmnt_yr, 'rsch', 'cseries', new_base.4, paste0("GOAPcod2024", dat_day, "_old.dat")))
dat <- r4ss::SS_readdat_3.30(here::here(asmnt_yr, 'output', paste0("GOAPcod2024", dat_day, "_new.dat")))
dat_orig$lencomp[dat_orig$lencomp$fleet %in% c(1, 2, 3), 'Nsamp'] = dat$lencomp[dat$lencomp$fleet %in% c(1, 2, 3), 'Nsamp']
r4ss::SS_writedat_3.30(dat_orig,
                       here::here(asmnt_yr, 'rsch', 'cseries', new_base.4, paste0("GOAPcod2024", dat_day, "_old.dat")), overwrite = TRUE)

## run model ----
run_ss3_model(asmnt_yr, 
              folder = 'rsch/cseries',
              mdl = new_base.4,
              ctl_filename = "Model19_1c.ctl")

## get and plot model output ----
# get output
new_base.4_res <- r4ss::SS_output(dir = here::here(asmnt_yr, 'rsch', 'cseries', new_base.4))
# if exists, delete plot folder
if(file.exists(here::here(asmnt_yr, 'rsch', 'cseries', new_base.4, 'plots'))){
  unlink(here::here(asmnt_yr, 'rsch', 'cseries', new_base.4, 'plots'), recursive = TRUE)
}
# plot results
r4ss::SS_plots(new_base.4_res,
               printfolder = "",
               dir = here::here(asmnt_yr, 'rsch', 'cseries', new_base.4, "plots"))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2019.1c.5 ----
# includes:
# .5 plus length bin set at 104 cm

new_base.5 <- "2019.1c.5-2024"

## copy ss input files ----
if(!file.exists(here::here(asmnt_yr, 'rsch', 'cseries', new_base.5, 'ss3.exe'))){
  start_ss_fldr(from = here::here(asmnt_yr, 'rsch', base_mdl_update),
                to = here::here(asmnt_yr, 'rsch', 'cseries', new_base.5))
}

## update files ----
update_ss3_files(asmnt_yr, 
                 folder = 'rsch/cseries',
                 mdl = new_base.5, 
                 dat_filename = paste0("GOAPcod2024", dat_day, "_old.dat"),
                 ctl_in = "Model19_1b.ctl",
                 ctl_out = "Model19_1c.ctl")

## change plus length bin ----
dat_orig <- r4ss::SS_readdat_3.30(here::here(asmnt_yr, 'rsch', 'cseries', new_base.5, paste0("GOAPcod2024", dat_day, "_old.dat")))
dat <- r4ss::SS_readdat_3.30(here::here(asmnt_yr, 'output', paste0("GOAPcod2024", dat_day, ".dat")))
dat_orig$maximum_size = dat$maximum_size
dat_orig$N_lbins = dat$N_lbins
dat_orig$lbin_vector = dat$lbin_vector
dat_orig$lencomp[,length(colnames(dat$lencomp))] = rowSums(dat_orig$lencomp[,((length(colnames(dat$lencomp)) + 1):length(colnames(dat_orig$lencomp)))])
dat_orig$lencomp = dat_orig$lencomp[,-((length(colnames(dat$lencomp)) + 1):length(colnames(dat_orig$lencomp)))]
dat_orig$agecomp = dat_orig$agecomp[dat_orig$agecomp$Lbin_lo < dat$maximum_size,] %>% 
  dplyr::bind_rows(dat_orig$agecomp[dat_orig$agecomp$Lbin_lo >= dat$maximum_size,] %>% 
                     dplyr::summarise(Lbin_lo = dat$maximum_size,
                                      Lbin_hi = dat$maximum_size,
                                      Nsamp = mean(Nsamp),
                                      a1 = mean(a1),
                                      a2 = mean(a2),
                                      a3 = mean(a3),
                                      a4 = mean(a4),
                                      a5 = mean(a5),
                                      a6 = mean(a6),
                                      a7 = mean(a7),
                                      a8 = mean(a8),
                                      a9 = mean(a9),
                                      a10 = mean(a10),
                                      .by = c(year, month, fleet, sex, part, ageerr)))
r4ss::SS_writedat_3.30(dat_orig,
                       here::here(asmnt_yr, 'rsch', 'cseries', new_base.5, paste0("GOAPcod2024", dat_day, "_old.dat")), overwrite = TRUE)

## run model ----
run_ss3_model(asmnt_yr, 
              folder = 'rsch/cseries',
              mdl = new_base.5,
              ctl_filename = "Model19_1c.ctl")

## get and plot model output ----
# get output
new_base.5_res <- r4ss::SS_output(dir = here::here(asmnt_yr, 'rsch', 'cseries', new_base.5))
# if exists, delete plot folder
if(file.exists(here::here(asmnt_yr, 'rsch', 'cseries', new_base.5, 'plots'))){
  unlink(here::here(asmnt_yr, 'rsch', 'cseries', new_base.5, 'plots'), recursive = TRUE)
}
# plot results
r4ss::SS_plots(new_base.5_res,
               printfolder = "",
               dir = here::here(asmnt_yr, 'rsch', 'cseries', new_base.5, "plots"))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2019.1c.6 ----
# includes:
# .6 correct season for twl survey caal

new_base.6 <- "2019.1c.6-2024"

## copy ss input files ----
if(!file.exists(here::here(asmnt_yr, 'rsch', 'cseries', new_base.6, 'ss3.exe'))){
  start_ss_fldr(from = here::here(asmnt_yr, 'rsch', base_mdl_update),
                to = here::here(asmnt_yr, 'rsch', 'cseries', new_base.6))
}

## update files ----
update_ss3_files(asmnt_yr, 
                 folder = 'rsch/cseries',
                 mdl = new_base.6, 
                 dat_filename = paste0("GOAPcod2024", dat_day, "_old.dat"),
                 ctl_in = "Model19_1b.ctl",
                 ctl_out = "Model19_1c.ctl")

## change plus length bin ----
dat_orig <- r4ss::SS_readdat_3.30(here::here(asmnt_yr, 'rsch', 'cseries', new_base.6, paste0("GOAPcod2024", dat_day, "_old.dat")))
dat_orig$agecomp[dat_orig$agecomp$fleet == 4, 'month'] = 7
r4ss::SS_writedat_3.30(dat_orig,
                       here::here(asmnt_yr, 'rsch', 'cseries', new_base.6, paste0("GOAPcod2024", dat_day, "_old.dat")), overwrite = TRUE)

## run model ----
run_ss3_model(asmnt_yr, 
              folder = 'rsch/cseries',
              mdl = new_base.6,
              ctl_filename = "Model19_1c.ctl")

## get and plot model output ----
# get output
new_base.6_res <- r4ss::SS_output(dir = here::here(asmnt_yr, 'rsch', 'cseries', new_base.6))
# if exists, delete plot folder
if(file.exists(here::here(asmnt_yr, 'rsch', 'cseries', new_base.6, 'plots'))){
  unlink(here::here(asmnt_yr, 'rsch', 'cseries', new_base.6, 'plots'), recursive = TRUE)
}
# plot results
r4ss::SS_plots(new_base.6_res,
               printfolder = "",
               dir = here::here(asmnt_yr, 'rsch', 'cseries', new_base.6, "plots"))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2019.1c.7 ----
# includes:
# .7 turn off recr forecast phase

new_base.7 <- "2019.1c.7-2024"

## copy ss input files ----
if(!file.exists(here::here(asmnt_yr, 'rsch', 'cseries', new_base.7, 'ss3.exe'))){
  start_ss_fldr(from = here::here(asmnt_yr, 'rsch', base_mdl_update),
                to = here::here(asmnt_yr, 'rsch', 'cseries', new_base.7))
}

## update files ----
update_ss3_files(asmnt_yr, 
                 folder = 'rsch/cseries',
                 mdl = new_base.7, 
                 dat_filename = paste0("GOAPcod2024", dat_day, "_old.dat"),
                 ctl_in = "Model19_1b.ctl",
                 ctl_out = "Model19_1c.ctl")

## turn off recr forecase phase ----
ctl <- r4ss::SS_readctl_3.30(here::here(asmnt_yr, 'rsch', 'cseries', new_base.7, "Model19_1c.ctl"))
ctl$Fcast_recr_phase = -1
r4ss::SS_writectl_3.30(ctllist = ctl,
                       outfile = here::here(asmnt_yr, 'rsch', 'cseries', new_base.7, "Model19_1c.ctl"),
                       overwrite = TRUE)

## run model ----
run_ss3_model(asmnt_yr, 
              folder = 'rsch/cseries',
              mdl = new_base.7,
              ctl_filename = "Model19_1c.ctl")

## get and plot model output ----
# get output
new_base.7_res <- r4ss::SS_output(dir = here::here(asmnt_yr, 'rsch', 'cseries', new_base.7))
# if exists, delete plot folder
if(file.exists(here::here(asmnt_yr, 'rsch', 'cseries', new_base.7, 'plots'))){
  unlink(here::here(asmnt_yr, 'rsch', 'cseries', new_base.7, 'plots'), recursive = TRUE)
}
# plot results
r4ss::SS_plots(new_base.7_res,
               printfolder = "",
               dir = here::here(asmnt_yr, 'rsch', 'cseries', new_base.7, "plots"))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2019.1c.8 ----
# includes:
# .8 add prior to Lmin

new_base.8 <- "2019.1c.8-2024"

## copy ss input files ----
if(!file.exists(here::here(asmnt_yr, 'rsch', 'cseries', new_base.8, 'ss3.exe'))){
  start_ss_fldr(from = here::here(asmnt_yr, 'rsch', base_mdl_update),
                to = here::here(asmnt_yr, 'rsch', 'cseries', new_base.8))
}

## update files ----
update_ss3_files(asmnt_yr, 
                 folder = 'rsch/cseries',
                 mdl = new_base.8, 
                 dat_filename = paste0("GOAPcod2024", dat_day, "_old.dat"),
                 ctl_in = "Model19_1b.ctl",
                 ctl_out = "Model19_1c.ctl")

## add prior to Lmin ----
ctl <- r4ss::SS_readctl_3.30(here::here(asmnt_yr, 'rsch', 'cseries', new_base.8, "Model19_1c.ctl"))
ctl$MG_parms$PR_SD[which(rownames(ctl$MG_parms) == 'L_at_Amin_Fem_GP_1')] = 0.3
ctl$MG_parms$PR_type[which(rownames(ctl$MG_parms) == 'L_at_Amin_Fem_GP_1')] = 6
ctl$MG_parms$PHASE[which(rownames(ctl$MG_parms) == 'L_at_Amin_Fem_GP_1')] = 1
r4ss::SS_writectl_3.30(ctllist = ctl,
                       outfile = here::here(asmnt_yr, 'rsch', 'cseries', new_base.8, "Model19_1c.ctl"),
                       overwrite = TRUE)

## run model ----
run_ss3_model(asmnt_yr, 
              folder = 'rsch/cseries',
              mdl = new_base.8,
              ctl_filename = "Model19_1c.ctl")

## get and plot model output ----
# get output
new_base.8_res <- r4ss::SS_output(dir = here::here(asmnt_yr, 'rsch', 'cseries', new_base.8))
# if exists, delete plot folder
if(file.exists(here::here(asmnt_yr, 'rsch', 'cseries', new_base.8, 'plots'))){
  unlink(here::here(asmnt_yr, 'rsch', 'cseries', new_base.8, 'plots'), recursive = TRUE)
}
# plot results
r4ss::SS_plots(new_base.8_res,
               printfolder = "",
               dir = here::here(asmnt_yr, 'rsch', 'cseries', new_base.8, "plots"))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2019.1c.9 ----
# includes:
# .9 turn off start_logit param for survey selex

new_base.9 <- "2019.1c.9-2024"

## copy ss input files ----
if(!file.exists(here::here(asmnt_yr, 'rsch', 'cseries', new_base.9, 'ss3.exe'))){
  start_ss_fldr(from = here::here(asmnt_yr, 'rsch', base_mdl_update),
                to = here::here(asmnt_yr, 'rsch', 'cseries', new_base.9))
}

## update files ----
update_ss3_files(asmnt_yr, 
                 folder = 'rsch/cseries',
                 mdl = new_base.9, 
                 dat_filename = paste0("GOAPcod2024", dat_day, "_old.dat"),
                 ctl_in = "Model19_1b.ctl",
                 ctl_out = "Model19_1c.ctl")

## turn off recr forecase phase ----
ctl <- r4ss::SS_readctl_3.30(here::here(asmnt_yr, 'rsch', 'cseries', new_base.9, "Model19_1c.ctl"))
ctl$size_selex_parms$INIT[which(rownames(ctl$size_selex_parms) == 'SizeSel_P_5_Srv(4)')] = -1007.5
ctl$size_selex_parms$PHASE[which(rownames(ctl$size_selex_parms) == 'SizeSel_P_5_Srv(4)')] = -2
ctl$size_selex_parms$Block[which(rownames(ctl$size_selex_parms) == 'SizeSel_P_5_Srv(4)')] = 0
ctl$size_selex_parms$Block_Fxn[which(rownames(ctl$size_selex_parms) == 'SizeSel_P_5_Srv(4)')] = 0
ctl$size_selex_parms_tv = ctl$size_selex_parms_tv[-which(rownames(ctl$size_selex_parms_tv) == 'SizeSel_P_5_Srv(4)_BLK1repl_1996'),]
ctl$size_selex_parms_tv = ctl$size_selex_parms_tv[-which(rownames(ctl$size_selex_parms_tv) == 'SizeSel_P_5_Srv(4)_BLK1repl_2006'),]
r4ss::SS_writectl_3.30(ctllist = ctl,
                       outfile = here::here(asmnt_yr, 'rsch', 'cseries', new_base.9, "Model19_1c.ctl"),
                       overwrite = TRUE)

## run model ----
run_ss3_model(asmnt_yr, 
              folder = 'rsch/cseries',
              mdl = new_base.9,
              ctl_filename = "Model19_1c.ctl")

## get and plot model output ----
# get output
new_base.9_res <- r4ss::SS_output(dir = here::here(asmnt_yr, 'rsch', 'cseries', new_base.9))
# if exists, delete plot folder
if(file.exists(here::here(asmnt_yr, 'rsch', 'cseries', new_base.9, 'plots'))){
  unlink(here::here(asmnt_yr, 'rsch', 'cseries', new_base.9, 'plots'), recursive = TRUE)
}
# plot results
r4ss::SS_plots(new_base.9_res,
               printfolder = "",
               dir = here::here(asmnt_yr, 'rsch', 'cseries', new_base.9, "plots"))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2019.1b: updated base model ----
base_mdl <- "2019.1b-2023" # 2023 accepted model
# includes updated GAP tables and length-weight relationship
base_mdl_update <- "2019.1b-2024"

# only run model if it doesn't exist
if(!file.exists(here::here(asmnt_yr, 'rsch', base_mdl_update))){
  ## copy ss input files ----
  if(!file.exists(here::here(asmnt_yr, 'rsch', base_mdl_update, 'ss3.exe'))){
    start_ss_fldr(from = here::here(asmnt_yr - 1, 'mgmt', base_mdl),
                  to = here::here(asmnt_yr, 'rsch', base_mdl_update))
  }
  
  # set up forecast file (with generic blocks for years so won't need to update in the future)
  forecast <- r4ss::SS_readforecast(file = here::here(asmnt_yr, 'rsch', base_mdl_update, 'forecast.ss'))
  forecast$Bmark_years <- c(-999, -2, -999, -2, -999, -1, -999, -2, -999, -2)
  forecast$Fcast_years <- c(2000, -2, -5, -1, -999, -2)
  r4ss::SS_writeforecast(mylist = forecast,
                         dir = here::here(asmnt_yr, 'rsch', base_mdl_update),
                         overwrite = TRUE)
  
  ## update files ----
  update_ss3_files(asmnt_yr, 
                   folder = 'rsch',
                   mdl = base_mdl_update, 
                   dat_filename = paste0("GOAPcod2024", dat_day, "_old.dat"),
                   ctl_in = "Model19_1b.ctl",
                   ctl_out = "Model19_1b.ctl")
  
  ## run model ----
  run_ss3_model(asmnt_yr, 
                folder = 'rsch',
                mdl = base_mdl_update,
                ctl_filename = "Model19_1b.ctl")
  
  
  ## get and plot model output ----
  # get output
  update_base_res <- r4ss::SS_output(dir = here::here(asmnt_yr, 'rsch', base_mdl_update))
  # if exists, delete plot folder
  if(file.exists(here::here(asmnt_yr, 'rsch', base_mdl_update, 'plots'))){
    unlink(here::here(asmnt_yr, 'rsch', base_mdl_update, 'plots'), recursive = TRUE)
  }
  # plot results
  r4ss::SS_plots(update_base_res,
                 printfolder = "",
                 dir = here::here(asmnt_yr, 'rsch', base_mdl_update, "plots"))
} else{
  update_base_res <- r4ss::SS_output(dir = here::here(asmnt_yr, 'rsch', base_mdl_update))
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2019.1c: new base model ----
# includes:
# .1 corrected ll survey sd
# .2 corrected ll survey length comp bins
# .3 corrected ll survey timing
# .4 fishery iss set at number of hauls that data is used from
# .5 plus length bin set at 104 cm
# .6 correct season for twl survey caal
# .7 turn off recr forecase phase
# .8 add prior to Lmin
# .9 turn off start_logit param for survey selex

new_base <- "2019.1c-2024"

# only run model if it doesn't exist
if(!file.exists(here::here(asmnt_yr, 'rsch', new_base))){
  ## copy ss input files ----
  if(!file.exists(here::here(asmnt_yr, 'rsch', new_base, 'ss3.exe'))){
    start_ss_fldr(from = here::here(asmnt_yr, 'rsch', base_mdl_update),
                  to = here::here(asmnt_yr, 'rsch', new_base))
  }
  
  ## update files ----
  update_ss3_files(asmnt_yr, 
                   folder = 'rsch',
                   mdl = new_base, 
                   dat_filename = paste0("GOAPcod2024", dat_day, ".dat"),
                   ctl_in = "Model19_1b.ctl",
                   ctl_out = "Model19_1c.ctl")
  
  ## make 2024 changes to ctl file ----
  ctl_2024(asmnt_yr, 
           folder = 'rsch',
           mdl = new_base, 
           ctl_filename = 'Model19_1c.ctl')
  
  ## run model ----
  run_ss3_model(asmnt_yr, 
                folder = 'rsch',
                mdl = new_base,
                ctl_filename = "Model19_1c.ctl")
  
  ## get and plot model output ----
  # get output
  new_base_res <- r4ss::SS_output(dir = here::here(asmnt_yr, 'rsch', new_base))
  # if exists, delete plot folder
  if(file.exists(here::here(asmnt_yr, 'rsch', new_base, 'plots'))){
    unlink(here::here(asmnt_yr, 'rsch', new_base, 'plots'), recursive = TRUE)
  }
  # plot results
  r4ss::SS_plots(new_base_res,
                 printfolder = "",
                 dir = here::here(asmnt_yr, 'rsch', new_base, "plots"))
} else{new_base_res <- r4ss::SS_output(dir = here::here(asmnt_yr, 'rsch', new_base))}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# plot comparisons ----
if (!file.exists(here::here(asmnt_yr, 'rsch', 'output', 'compare', 'data_plots_cseries'))){
  dir.create(here::here(asmnt_yr, 'rsch', 'output', 'compare', 'data_plots_cseries'), recursive = TRUE)
}


## 2019.1c model series ----
### all combined ----
data_summ_cseries <- r4ss::SSsummarize(list(update_base_res, 
                                            new_base_res, 
                                            new_base.1_res,
                                            new_base.2_res,
                                            new_base.3_res,
                                            new_base.4_res,
                                            new_base.5_res,
                                            new_base.6_res,
                                            new_base.7_res,
                                            new_base.8_res,
                                            new_base.9_res))
r4ss::SSplotComparisons(data_summ_cseries,
                        print = TRUE,
                        legendlabels = c(base_mdl_update, 
                                         new_base, 
                                         new_base.1,
                                         new_base.2,
                                         new_base.3,
                                         new_base.4,
                                         new_base.5,
                                         new_base.6,
                                         new_base.7,
                                         new_base.8,
                                         new_base.9),
                        plotdir = here::here(asmnt_yr, 'rsch', 'output', 'compare', 'data_plots_cseries'))

### one at a time ----
# 2019.1c
r4ss::SSplotComparisons(data_summ_cseries, subplots = 1, 
                        print = TRUE,
                        models = c(1,2),
                        legendlabels = c(base_mdl_update, 
                                         '2019.1c'),
                        plotdir = here::here(asmnt_yr, 'rsch', 'output', 'compare', 'data_plots_cseries'),
                        filenameprefix = 'c')

# 2019.1c.1
r4ss::SSplotComparisons(data_summ_cseries, subplots = 1, 
                        print = TRUE,
                        models = c(1,3),
                        legendlabels = c(base_mdl_update, 
                                         '2019.1c.1-LLSrvSD'),
                        plotdir = here::here(asmnt_yr, 'rsch', 'output', 'compare', 'data_plots_cseries'),
                        filenameprefix = 'c1')
# 2019.1c.2
r4ss::SSplotComparisons(data_summ_cseries, subplots = 1, 
                        print = TRUE,
                        models = c(1,4),
                        legendlabels = c(base_mdl_update, 
                                         '2019.1c.2-LLSrvLCompBin'),
                        plotdir = here::here(asmnt_yr, 'rsch', 'output', 'compare', 'data_plots_cseries'),
                        filenameprefix = 'c2')
# 2019.1c.3
r4ss::SSplotComparisons(data_summ_cseries, subplots = 1, 
                        print = TRUE,
                        models = c(1,5),
                        legendlabels = c(base_mdl_update, 
                                         '2019.1c.3-LLSrvLCompMonth'),
                        plotdir = here::here(asmnt_yr, 'rsch', 'output', 'compare', 'data_plots_cseries'),
                        filenameprefix = 'c3')
# 2019.1c.4
r4ss::SSplotComparisons(data_summ_cseries, subplots = 1, 
                        print = TRUE,
                        models = c(1,6),
                        legendlabels = c(base_mdl_update, 
                                         '2019.1c.4-FishISS'),
                        plotdir = here::here(asmnt_yr, 'rsch', 'output', 'compare', 'data_plots_cseries'),
                        filenameprefix = 'c4')
# 2019.1c.5
r4ss::SSplotComparisons(data_summ_cseries, subplots = 1, 
                        print = TRUE,
                        models = c(1,7),
                        legendlabels = c(base_mdl_update, 
                                         '2019.1c.5-104cm'),
                        plotdir = here::here(asmnt_yr, 'rsch', 'output', 'compare', 'data_plots_cseries'),
                        filenameprefix = 'c5')
# 2019.1c.6
r4ss::SSplotComparisons(data_summ_cseries, subplots = 1, 
                        print = TRUE,
                        models = c(1,8),
                        legendlabels = c(base_mdl_update, 
                                         '2019.1c.6-TwlSrvCAAL'),
                        plotdir = here::here(asmnt_yr, 'rsch', 'output', 'compare', 'data_plots_cseries'),
                        filenameprefix = 'c6')
# 2019.1c.7
r4ss::SSplotComparisons(data_summ_cseries, subplots = 1, 
                        print = TRUE,
                        models = c(1,9),
                        legendlabels = c(base_mdl_update, 
                                         '2019.1c.7-ForeRec'),
                        plotdir = here::here(asmnt_yr, 'rsch', 'output', 'compare', 'data_plots_cseries'),
                        filenameprefix = 'c7')
# 2019.1c.8
r4ss::SSplotComparisons(data_summ_cseries, subplots = 1, 
                        print = TRUE,
                        models = c(1,10),
                        legendlabels = c(base_mdl_update, 
                                         '2019.1c.8-LatAmin'),
                        plotdir = here::here(asmnt_yr, 'rsch', 'output', 'compare', 'data_plots_cseries'),
                        filenameprefix = 'c8')
# 2019.1c.9
r4ss::SSplotComparisons(data_summ_cseries, subplots = 1, 
                        print = TRUE,
                        models = c(1,11),
                        legendlabels = c(base_mdl_update, 
                                         '2019.1c.9-start_logit'),
                        plotdir = here::here(asmnt_yr, 'rsch', 'output', 'compare', 'data_plots_cseries'),
                        filenameprefix = 'c9')

## likes & abc ----
vroom::vroom_write(data_summ_cseries$likelihoods %>% 
                     tidytable::rename('2019.1b-24' = model1,
                                       '2019.1c' = model2,
                                       '2019.1c.1' = model3,
                                       '2019.1c.2' = model4,
                                       '2019.1c.3' = model5,
                                       '2019.1c.4' = model6,
                                       '2019.1c.5' = model7,
                                       '2019.1c.6' = model8,
                                       '2019.1c.7' = model9,
                                       '2019.1c.8' = model10,
                                       '2019.1c.9' = model11), 
                   here::here(asmnt_yr, 'rsch', 'output', 'compare', 'data_summ_likes_cseries.csv'), delim = ",")
vroom::vroom_write(data_summ_cseries$likelihoods_by_fleet %>% 
                     tidytable::mutate(model = case_when(model == 1 ~ '2019.1b-24',
                                                         model == 2 ~ '2019.1c',
                                                         model == 3 ~ '2019.1c.1',
                                                         model == 4 ~ '2019.1c.2',
                                                         model == 5 ~ '2019.1c.3',
                                                         model == 6 ~ '2019.1c.4',
                                                         model == 7 ~ '2019.1c.5',
                                                         model == 8 ~ '2019.1c.6',
                                                         model == 9 ~ '2019.1c.7',
                                                         model == 10 ~ '2019.1c.8',
                                                         model == 11 ~ '2019.1c.9')), 
                   here::here(asmnt_yr, 'rsch', 'output', 'compare', 'data_summ_likes_by_fleet_cseries.csv'), delim = ",")

abc_comp <- data.frame(model = c(base_mdl_update, 
                                 new_base, 
                                 new_base.1,
                                 new_base.2,
                                 new_base.3,
                                 new_base.4,
                                 new_base.5,
                                 new_base.6,
                                 new_base.7,
                                 new_base.8,
                                 new_base.9),
                       abc = c(as.numeric(update_base_res$derived_quants %>% 
                                            filter(Label == 'ForeCatch_2025') %>% 
                                            select(Value)),
                               as.numeric(new_base_res$derived_quants %>% 
                                            filter(Label == 'ForeCatch_2025') %>% 
                                            select(Value)),
                               as.numeric(new_base.1_res$derived_quants %>% 
                                            filter(Label == 'ForeCatch_2025') %>% 
                                            select(Value)),
                               as.numeric(new_base.2_res$derived_quants %>% 
                                            filter(Label == 'ForeCatch_2025') %>% 
                                            select(Value)),
                               as.numeric(new_base.3_res$derived_quants %>% 
                                            filter(Label == 'ForeCatch_2025') %>% 
                                            select(Value)),
                               as.numeric(new_base.4_res$derived_quants %>% 
                                            filter(Label == 'ForeCatch_2025') %>% 
                                            select(Value)),
                               as.numeric(new_base.5_res$derived_quants %>% 
                                            filter(Label == 'ForeCatch_2025') %>% 
                                            select(Value)),
                               as.numeric(new_base.6_res$derived_quants %>% 
                                            filter(Label == 'ForeCatch_2025') %>% 
                                            select(Value)),
                               as.numeric(new_base.7_res$derived_quants %>% 
                                            filter(Label == 'ForeCatch_2025') %>% 
                                            select(Value)),
                               as.numeric(new_base.8_res$derived_quants %>% 
                                            filter(Label == 'ForeCatch_2025') %>% 
                                            select(Value)),
                               as.numeric(new_base.9_res$derived_quants %>% 
                                            filter(Label == 'ForeCatch_2025') %>% 
                                            select(Value))))
vroom::vroom_write(abc_comp, here::here(asmnt_yr, 'rsch', 'output', 'compare', 'data_abc_comp_cseries.csv'), delim = ",")

