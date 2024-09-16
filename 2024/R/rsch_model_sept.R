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
dat_day <- "Sep07"

# source functions
source_files <- list.files(here::here(asmnt_yr, "R", "assessment"), "*.r$")
purrr::map(here::here(asmnt_yr, "R", "assessment", source_files), source)
source(here::here(asmnt_yr, "R", "utils.r"))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2019.1b-2023: 2023 base model ----
base_mdl <- "2019.1b-2023" # 2023 accepted model

base_res_23 <- r4ss::SS_output(dir = here::here(asmnt_yr - 1, 'mgmt', base_mdl),
                               verbose = TRUE,
                               printstats = TRUE)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2019.1b: updated base model ----
base_mdl_update <- "2019.1b-2024"

update_base_res <- r4ss::SS_output(dir = here::here(asmnt_yr, 'rsch', base_mdl_update))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2019.1e.5cm: new base model ----
new_base <- "2019.1e.5cm-2024"

new_base_res <- r4ss::SS_output(dir = here::here(asmnt_yr, 'rsch', new_base))

## run management scens ----
new_base_mscen <- Do_AK_TIER_3_Scenarios(DIR = here::here(asmnt_yr, 'rsch', new_base),
                                         CYR = asmnt_yr,
                                         FLEETS = c(1:3),
                                         do_fig = FALSE,
                                         do_mark = FALSE)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# time-invariant/logistic trawl survey selex ----
twlsel <- "twl_sel"
copy_mdl <- new_base

## copy ss input files ----
if(!file.exists(here::here(asmnt_yr, 'rsch/extra_models', twlsel, 'ss3.exe'))){
  start_ss_fldr(from = here::here(asmnt_yr, 'rsch', copy_mdl),
                to = here::here(asmnt_yr, 'rsch/extra_models', twlsel))
}

## update files ----
update_ss3_files(asmnt_yr, 
                 folder = 'rsch/extra_models',
                 mdl = twlsel, 
                 dat_filename = paste0("GOAPcod2024", dat_day, "_lcomp_bin5.dat"),
                 ctl_in = "updated_ae.ctl",
                 ctl_out = "test_model.ctl")

## make 2024 changes to ctl file ----
ctl_2024(asmnt_yr, 
         folder = 'rsch/extra_models',
         mdl = twlsel, 
         ctl_filename = 'test_model.ctl')

## change selex in ctl ----
# read ctl file
ctl <- r4ss::SS_readctl_3.30(here::here(asmnt_yr, 'rsch/extra_models', twlsel, 'test_model.ctl'))
# set blocks to 0
ctl$size_selex_parms$Block[seq(which(rownames(ctl$size_selex_parms) == 'SizeSel_P_1_Srv(4)'),
                               which(rownames(ctl$size_selex_parms) == 'SizeSel_P_6_Srv(4)'))] <- 0
ctl$size_selex_parms$Block_Fxn[seq(which(rownames(ctl$size_selex_parms) == 'SizeSel_P_1_Srv(4)'),
                                   which(rownames(ctl$size_selex_parms) == 'SizeSel_P_6_Srv(4)'))] <- 0
# remove from tv section
ctl$size_selex_parms_tv <- ctl$size_selex_parms_tv[-seq(which(rownames(ctl$size_selex_parms_tv) == 'SizeSel_P_1_Srv(4)_BLK1repl_1996'),
                                                        length(rownames(ctl$size_selex_parms_tv))),]
# freeze twl srv to logistic
ctl$size_selex_parms$INIT[which(rownames(ctl$size_selex_parms) == 'SizeSel_P_6_Srv(4)')] <- 10
ctl$size_selex_parms$PHASE[which(rownames(ctl$size_selex_parms) == 'SizeSel_P_6_Srv(4)')] <- -2
# remove prior on descend_se
ctl$size_selex_parms$PR_SD[which(rownames(ctl$size_selex_parms) == 'SizeSel_P_4_Srv(4)')] = 0
ctl$size_selex_parms$PR_type[which(rownames(ctl$size_selex_parms) == 'SizeSel_P_4_Srv(4)')] = 0
# write new ctl file
r4ss::SS_writectl_3.30(ctllist = ctl,
                       outfile = here::here(asmnt_yr, 'rsch/extra_models', twlsel, 'test_model.ctl'),
                       overwrite = TRUE)

## run model ----
run_ss3_model(asmnt_yr, 
              folder = 'rsch/extra_models',
              mdl = twlsel,
              ctl_filename = "test_model.ctl")

## get and plot model output ----
# get output
twlsel_res <- r4ss::SS_output(dir = here::here(asmnt_yr, 'rsch/extra_models', twlsel))
# if exists, delete plot folder
if(file.exists(here::here(asmnt_yr, 'rsch/extra_models', twlsel, 'plots'))){
  unlink(here::here(asmnt_yr, 'rsch/extra_models', twlsel, 'plots'), recursive = TRUE)
}
# plot results
r4ss::SS_plots(twlsel_res,
               printfolder = "",
               dir = here::here(asmnt_yr, 'rsch/extra_models', twlsel, "plots"))

## run management scens ----
twlsel_mscen <- Do_AK_TIER_3_Scenarios(DIR = here::here(asmnt_yr, 'rsch/extra_models', twlsel),
                                       CYR = asmnt_yr,
                                       FLEETS = c(1:3),
                                       do_fig = FALSE,
                                       do_mark = FALSE)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# logistic ll survey selex ----
llsel <- "ll_sel"
copy_mdl <- new_base

## copy ss input files ----
if(!file.exists(here::here(asmnt_yr, 'rsch/extra_models', llsel, 'ss3.exe'))){
  start_ss_fldr(from = here::here(asmnt_yr, 'rsch', copy_mdl),
                to = here::here(asmnt_yr, 'rsch/extra_models', llsel))
}

## update files ----
update_ss3_files(asmnt_yr, 
                 folder = 'rsch/extra_models',
                 mdl = llsel, 
                 dat_filename = paste0("GOAPcod2024", dat_day, "_lcomp_bin5.dat"),
                 ctl_in = "updated_ae.ctl",
                 ctl_out = "test_model.ctl")

## make 2024 changes to ctl file ----
ctl_2024(asmnt_yr, 
         folder = 'rsch/extra_models',
         mdl = llsel, 
         ctl_filename = 'test_model.ctl')

## change selex in ctl ----
# read ctl file
ctl <- r4ss::SS_readctl_3.30(here::here(asmnt_yr, 'rsch/extra_models', llsel, 'test_model.ctl'))
# freeze ll srv to logistic
ctl$size_selex_parms$INIT[which(rownames(ctl$size_selex_parms) == 'SizeSel_P_6_LLSrv(5)')] <- 10
ctl$size_selex_parms$PHASE[which(rownames(ctl$size_selex_parms) == 'SizeSel_P_6_LLSrv(5)')] <- -2
# write new ctl file
r4ss::SS_writectl_3.30(ctllist = ctl,
                       outfile = here::here(asmnt_yr, 'rsch/extra_models', llsel, 'test_model.ctl'),
                       overwrite = TRUE)

## run model ----
run_ss3_model(asmnt_yr, 
              folder = 'rsch/extra_models',
              mdl = llsel,
              ctl_filename = "test_model.ctl")

## get and plot model output ----
# get output
llsel_res <- r4ss::SS_output(dir = here::here(asmnt_yr, 'rsch/extra_models', llsel))
# if exists, delete plot folder
if(file.exists(here::here(asmnt_yr, 'rsch/extra_models', llsel, 'plots'))){
  unlink(here::here(asmnt_yr, 'rsch/extra_models', llsel, 'plots'), recursive = TRUE)
}
# plot results
r4ss::SS_plots(llsel_res,
               printfolder = "",
               dir = here::here(asmnt_yr, 'rsch/extra_models', llsel, "plots"))

## run management scens ----
llsel_mscen <- Do_AK_TIER_3_Scenarios(DIR = here::here(asmnt_yr, 'rsch/extra_models', llsel),
                                       CYR = asmnt_yr,
                                       FLEETS = c(1:3),
                                       do_fig = FALSE,
                                       do_mark = FALSE)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ll survey selex env param 1 ----
llsel <- "ll_sel_p1"
copy_mdl <- new_base

## copy ss input files ----
if(!file.exists(here::here(asmnt_yr, 'rsch/extra_models', llsel, 'ss3.exe'))){
  start_ss_fldr(from = here::here(asmnt_yr, 'rsch', copy_mdl),
                to = here::here(asmnt_yr, 'rsch/extra_models', llsel))
}

## update files ----
update_ss3_files(asmnt_yr, 
                 folder = 'rsch/extra_models',
                 mdl = llsel, 
                 dat_filename = paste0("GOAPcod2024", dat_day, "_lcomp_bin5.dat"),
                 ctl_in = "updated_ae.ctl",
                 ctl_out = "test_model.ctl")

## make 2024 changes to ctl file ----
ctl_2024(asmnt_yr, 
         folder = 'rsch/extra_models',
         mdl = llsel, 
         ctl_filename = 'test_model.ctl')

## change selex in ctl ----
# read ctl file
ctl <- r4ss::SS_readctl_3.30(here::here(asmnt_yr, 'rsch/extra_models', llsel, 'test_model.ctl'))
# remove env link from q
ctl$Q_parms$`env_var&link`[which(rownames(ctl$Q_parms) == 'LnQ_base_LLSrv(5)')] = 0
ctl$Q_parms_tv = NULL
# add env link to selex param
ctl$size_selex_parms$`env_var&link`[which(rownames(ctl$size_selex_parms) == 'SizeSel_P_1_LLSrv(5)')] = 101
ctl$size_selex_parms_tv = ctl$size_selex_parms_tv %>% 
  dplyr::bind_rows(data.frame(LO = -10, 
                              HI = 10, 
                              INIT = 0, 
                              PRIOR = 0, 
                              PR_SD = 99, 
                              PR_type = 0, 
                              PHASE = 5))
rownames(ctl$size_selex_parms_tv)[which(rownames(ctl$size_selex_parms_tv) == '...58')] <- "SizeSel_LLSrv(5)_ENV_add"
# write new ctl file
r4ss::SS_writectl_3.30(ctllist = ctl,
                       outfile = here::here(asmnt_yr, 'rsch/extra_models', llsel, 'test_model.ctl'),
                       overwrite = TRUE)

## run model ----
run_ss3_model(asmnt_yr, 
              folder = 'rsch/extra_models',
              mdl = llsel,
              ctl_filename = "test_model.ctl")

## get and plot model output ----
# get output
llsel_p1_res <- r4ss::SS_output(dir = here::here(asmnt_yr, 'rsch/extra_models', llsel))
# if exists, delete plot folder
if(file.exists(here::here(asmnt_yr, 'rsch/extra_models', llsel, 'plots'))){
  unlink(here::here(asmnt_yr, 'rsch/extra_models', llsel, 'plots'), recursive = TRUE)
}
# plot results
r4ss::SS_plots(llsel_p1_res,
               printfolder = "",
               dir = here::here(asmnt_yr, 'rsch/extra_models', llsel, "plots"))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ll survey selex env param 1 ----
llsel <- "ll_sel_p2"
copy_mdl <- new_base

## copy ss input files ----
if(!file.exists(here::here(asmnt_yr, 'rsch/extra_models', llsel, 'ss3.exe'))){
  start_ss_fldr(from = here::here(asmnt_yr, 'rsch', copy_mdl),
                to = here::here(asmnt_yr, 'rsch/extra_models', llsel))
}

## update files ----
update_ss3_files(asmnt_yr, 
                 folder = 'rsch/extra_models',
                 mdl = llsel, 
                 dat_filename = paste0("GOAPcod2024", dat_day, "_lcomp_bin5.dat"),
                 ctl_in = "updated_ae.ctl",
                 ctl_out = "test_model.ctl")

## make 2024 changes to ctl file ----
ctl_2024(asmnt_yr, 
         folder = 'rsch/extra_models',
         mdl = llsel, 
         ctl_filename = 'test_model.ctl')

## change selex in ctl ----
# read ctl file
ctl <- r4ss::SS_readctl_3.30(here::here(asmnt_yr, 'rsch/extra_models', llsel, 'test_model.ctl'))
# remove env link from q
ctl$Q_parms$`env_var&link`[which(rownames(ctl$Q_parms) == 'LnQ_base_LLSrv(5)')] = 0
ctl$Q_parms_tv = NULL
# add env link to selex param
ctl$size_selex_parms$`env_var&link`[which(rownames(ctl$size_selex_parms) == 'SizeSel_P_2_LLSrv(5)')] = 101
ctl$size_selex_parms_tv = ctl$size_selex_parms_tv %>% 
  dplyr::bind_rows(data.frame(LO = -10, 
                              HI = 10, 
                              INIT = 0, 
                              PRIOR = 0, 
                              PR_SD = 99, 
                              PR_type = 0, 
                              PHASE = 5))
rownames(ctl$size_selex_parms_tv)[which(rownames(ctl$size_selex_parms_tv) == '...58')] <- "SizeSel_LLSrv(5)_ENV_add"
# write new ctl file
r4ss::SS_writectl_3.30(ctllist = ctl,
                       outfile = here::here(asmnt_yr, 'rsch/extra_models', llsel, 'test_model.ctl'),
                       overwrite = TRUE)

## run model ----
run_ss3_model(asmnt_yr, 
              folder = 'rsch/extra_models',
              mdl = llsel,
              ctl_filename = "test_model.ctl")

## get and plot model output ----
# get output
llsel_p2_res <- r4ss::SS_output(dir = here::here(asmnt_yr, 'rsch/extra_models', llsel))
# if exists, delete plot folder
if(file.exists(here::here(asmnt_yr, 'rsch/extra_models', llsel, 'plots'))){
  unlink(here::here(asmnt_yr, 'rsch/extra_models', llsel, 'plots'), recursive = TRUE)
}
# plot results
r4ss::SS_plots(llsel_p2_res,
               printfolder = "",
               dir = here::here(asmnt_yr, 'rsch/extra_models', llsel, "plots"))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ll survey selex env param 3 ----
llsel <- "ll_sel_p3"
copy_mdl <- new_base

## copy ss input files ----
if(!file.exists(here::here(asmnt_yr, 'rsch/extra_models', llsel, 'ss3.exe'))){
  start_ss_fldr(from = here::here(asmnt_yr, 'rsch', copy_mdl),
                to = here::here(asmnt_yr, 'rsch/extra_models', llsel))
}

## update files ----
update_ss3_files(asmnt_yr, 
                 folder = 'rsch/extra_models',
                 mdl = llsel, 
                 dat_filename = paste0("GOAPcod2024", dat_day, "_lcomp_bin5.dat"),
                 ctl_in = "updated_ae.ctl",
                 ctl_out = "test_model.ctl")

## make 2024 changes to ctl file ----
ctl_2024(asmnt_yr, 
         folder = 'rsch/extra_models',
         mdl = llsel, 
         ctl_filename = 'test_model.ctl')

## change selex in ctl ----
# read ctl file
ctl <- r4ss::SS_readctl_3.30(here::here(asmnt_yr, 'rsch/extra_models', llsel, 'test_model.ctl'))
# remove env link from q
ctl$Q_parms$`env_var&link`[which(rownames(ctl$Q_parms) == 'LnQ_base_LLSrv(5)')] = 0
ctl$Q_parms_tv = NULL
# add env link to selex param
ctl$size_selex_parms$`env_var&link`[which(rownames(ctl$size_selex_parms) == 'SizeSel_P_3_LLSrv(5)')] = 101
ctl$size_selex_parms_tv = ctl$size_selex_parms_tv %>% 
  dplyr::bind_rows(data.frame(LO = -10, 
                              HI = 10, 
                              INIT = 0, 
                              PRIOR = 0, 
                              PR_SD = 99, 
                              PR_type = 0, 
                              PHASE = 5))
rownames(ctl$size_selex_parms_tv)[which(rownames(ctl$size_selex_parms_tv) == '...58')] <- "SizeSel_LLSrv(5)_ENV_add"
# write new ctl file
r4ss::SS_writectl_3.30(ctllist = ctl,
                       outfile = here::here(asmnt_yr, 'rsch/extra_models', llsel, 'test_model.ctl'),
                       overwrite = TRUE)

## run model ----
run_ss3_model(asmnt_yr, 
              folder = 'rsch/extra_models',
              mdl = llsel,
              ctl_filename = "test_model.ctl")

## get and plot model output ----
# get output
llsel_p3_res <- r4ss::SS_output(dir = here::here(asmnt_yr, 'rsch/extra_models', llsel))
# if exists, delete plot folder
if(file.exists(here::here(asmnt_yr, 'rsch/extra_models', llsel, 'plots'))){
  unlink(here::here(asmnt_yr, 'rsch/extra_models', llsel, 'plots'), recursive = TRUE)
}
# plot results
r4ss::SS_plots(llsel_p3_res,
               printfolder = "",
               dir = here::here(asmnt_yr, 'rsch/extra_models', llsel, "plots"))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ll survey selex env param 4 ----
llsel <- "ll_sel_p4"
copy_mdl <- new_base

## copy ss input files ----
if(!file.exists(here::here(asmnt_yr, 'rsch/extra_models', llsel, 'ss3.exe'))){
  start_ss_fldr(from = here::here(asmnt_yr, 'rsch', copy_mdl),
                to = here::here(asmnt_yr, 'rsch/extra_models', llsel))
}

## update files ----
update_ss3_files(asmnt_yr, 
                 folder = 'rsch/extra_models',
                 mdl = llsel, 
                 dat_filename = paste0("GOAPcod2024", dat_day, "_lcomp_bin5.dat"),
                 ctl_in = "updated_ae.ctl",
                 ctl_out = "test_model.ctl")

## make 2024 changes to ctl file ----
ctl_2024(asmnt_yr, 
         folder = 'rsch/extra_models',
         mdl = llsel, 
         ctl_filename = 'test_model.ctl')

## change selex in ctl ----
# read ctl file
ctl <- r4ss::SS_readctl_3.30(here::here(asmnt_yr, 'rsch/extra_models', llsel, 'test_model.ctl'))
# remove env link from q
ctl$Q_parms$`env_var&link`[which(rownames(ctl$Q_parms) == 'LnQ_base_LLSrv(5)')] = 0
ctl$Q_parms_tv = NULL
# add env link to selex param
ctl$size_selex_parms$`env_var&link`[which(rownames(ctl$size_selex_parms) == 'SizeSel_P_4_LLSrv(5)')] = 101
ctl$size_selex_parms_tv = ctl$size_selex_parms_tv %>% 
  dplyr::bind_rows(data.frame(LO = -10, 
                              HI = 10, 
                              INIT = 0, 
                              PRIOR = 0, 
                              PR_SD = 99, 
                              PR_type = 0, 
                              PHASE = 5))
rownames(ctl$size_selex_parms_tv)[which(rownames(ctl$size_selex_parms_tv) == '...58')] <- "SizeSel_LLSrv(5)_ENV_add"
# write new ctl file
r4ss::SS_writectl_3.30(ctllist = ctl,
                       outfile = here::here(asmnt_yr, 'rsch/extra_models', llsel, 'test_model.ctl'),
                       overwrite = TRUE)

## run model ----
run_ss3_model(asmnt_yr, 
              folder = 'rsch/extra_models',
              mdl = llsel,
              ctl_filename = "test_model.ctl")

## get and plot model output ----
# get output
llsel_p4_res <- r4ss::SS_output(dir = here::here(asmnt_yr, 'rsch/extra_models', llsel))
# if exists, delete plot folder
if(file.exists(here::here(asmnt_yr, 'rsch/extra_models', llsel, 'plots'))){
  unlink(here::here(asmnt_yr, 'rsch/extra_models', llsel, 'plots'), recursive = TRUE)
}
# plot results
r4ss::SS_plots(llsel_p4_res,
               printfolder = "",
               dir = here::here(asmnt_yr, 'rsch/extra_models', llsel, "plots"))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ll survey selex env param 6 ----
llsel <- "ll_sel_p6"
copy_mdl <- new_base

## copy ss input files ----
if(!file.exists(here::here(asmnt_yr, 'rsch/extra_models', llsel, 'ss3.exe'))){
  start_ss_fldr(from = here::here(asmnt_yr, 'rsch', copy_mdl),
                to = here::here(asmnt_yr, 'rsch/extra_models', llsel))
}

## update files ----
update_ss3_files(asmnt_yr, 
                 folder = 'rsch/extra_models',
                 mdl = llsel, 
                 dat_filename = paste0("GOAPcod2024", dat_day, "_lcomp_bin5.dat"),
                 ctl_in = "updated_ae.ctl",
                 ctl_out = "test_model.ctl")

## make 2024 changes to ctl file ----
ctl_2024(asmnt_yr, 
         folder = 'rsch/extra_models',
         mdl = llsel, 
         ctl_filename = 'test_model.ctl')

## change selex in ctl ----
# read ctl file
ctl <- r4ss::SS_readctl_3.30(here::here(asmnt_yr, 'rsch/extra_models', llsel, 'test_model.ctl'))
# remove env link from q
ctl$Q_parms$`env_var&link`[which(rownames(ctl$Q_parms) == 'LnQ_base_LLSrv(5)')] = 0
ctl$Q_parms_tv = NULL
# add env link to selex param
ctl$size_selex_parms$`env_var&link`[which(rownames(ctl$size_selex_parms) == 'SizeSel_P_6_LLSrv(5)')] = 101
ctl$size_selex_parms_tv = ctl$size_selex_parms_tv %>% 
  dplyr::bind_rows(data.frame(LO = -10, 
                              HI = 10, 
                              INIT = 0, 
                              PRIOR = 0, 
                              PR_SD = 99, 
                              PR_type = 0, 
                              PHASE = 5))
rownames(ctl$size_selex_parms_tv)[which(rownames(ctl$size_selex_parms_tv) == '...58')] <- "SizeSel_LLSrv(5)_ENV_add"
# write new ctl file
r4ss::SS_writectl_3.30(ctllist = ctl,
                       outfile = here::here(asmnt_yr, 'rsch/extra_models', llsel, 'test_model.ctl'),
                       overwrite = TRUE)

## run model ----
run_ss3_model(asmnt_yr, 
              folder = 'rsch/extra_models',
              mdl = llsel,
              ctl_filename = "test_model.ctl")

## get and plot model output ----
# get output
llsel_p6_res <- r4ss::SS_output(dir = here::here(asmnt_yr, 'rsch/extra_models', llsel))
# if exists, delete plot folder
if(file.exists(here::here(asmnt_yr, 'rsch/extra_models', llsel, 'plots'))){
  unlink(here::here(asmnt_yr, 'rsch/extra_models', llsel, 'plots'), recursive = TRUE)
}
# plot results
r4ss::SS_plots(llsel_p6_res,
               printfolder = "",
               dir = here::here(asmnt_yr, 'rsch/extra_models', llsel, "plots"))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ll survey selex env combined ----
llsel <- "ll_sel_comb"
copy_mdl <- new_base

## copy ss input files ----
if(!file.exists(here::here(asmnt_yr, 'rsch/extra_models', llsel, 'ss3.exe'))){
  start_ss_fldr(from = here::here(asmnt_yr, 'rsch', copy_mdl),
                to = here::here(asmnt_yr, 'rsch/extra_models', llsel))
}

## update files ----
update_ss3_files(asmnt_yr, 
                 folder = 'rsch/extra_models',
                 mdl = llsel, 
                 dat_filename = paste0("GOAPcod2024", dat_day, "_lcomp_bin5.dat"),
                 ctl_in = "updated_ae.ctl",
                 ctl_out = "test_model.ctl")

## make 2024 changes to ctl file ----
ctl_2024(asmnt_yr, 
         folder = 'rsch/extra_models',
         mdl = llsel, 
         ctl_filename = 'test_model.ctl')

## change selex in ctl ----
# read ctl file
ctl <- r4ss::SS_readctl_3.30(here::here(asmnt_yr, 'rsch/extra_models', llsel, 'test_model.ctl'))
# add env link to selex param
ctl$size_selex_parms$`env_var&link`[which(rownames(ctl$size_selex_parms) == 'SizeSel_P_1_LLSrv(5)')] = 101
ctl$size_selex_parms$`env_var&link`[which(rownames(ctl$size_selex_parms) == 'SizeSel_P_3_LLSrv(5)')] = 101
ctl$size_selex_parms_tv = ctl$size_selex_parms_tv %>% 
  dplyr::bind_rows(data.frame(LO = -10, 
                              HI = 10, 
                              INIT = 0, 
                              PRIOR = 0, 
                              PR_SD = 99, 
                              PR_type = 0, 
                              PHASE = 5)) %>% 
  dplyr::bind_rows(data.frame(LO = -10, 
                              HI = 10, 
                              INIT = 0, 
                              PRIOR = 0, 
                              PR_SD = 99, 
                              PR_type = 0, 
                              PHASE = 5))
rownames(ctl$size_selex_parms_tv)[which(rownames(ctl$size_selex_parms_tv) == '...58')] <- "SizeSel_P1_LLSrv(5)_ENV_add"
rownames(ctl$size_selex_parms_tv)[which(rownames(ctl$size_selex_parms_tv) == '...59')] <- "SizeSel_P2_LLSrv(5)_ENV_add"

# write new ctl file
r4ss::SS_writectl_3.30(ctllist = ctl,
                       outfile = here::here(asmnt_yr, 'rsch/extra_models', llsel, 'test_model.ctl'),
                       overwrite = TRUE)

## run model ----
run_ss3_model(asmnt_yr, 
              folder = 'rsch/extra_models',
              mdl = llsel,
              ctl_filename = "test_model.ctl")

## get and plot model output ----
# get output
llsel_comb_res <- r4ss::SS_output(dir = here::here(asmnt_yr, 'rsch/extra_models', llsel))
# if exists, delete plot folder
if(file.exists(here::here(asmnt_yr, 'rsch/extra_models', llsel, 'plots'))){
  unlink(here::here(asmnt_yr, 'rsch/extra_models', llsel, 'plots'), recursive = TRUE)
}
# plot results
r4ss::SS_plots(llsel_comb_res,
               printfolder = "",
               dir = here::here(asmnt_yr, 'rsch/extra_models', llsel, "plots"))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# plot model comparisons ----
if (!file.exists(here::here(asmnt_yr, 'rsch', 'output', 'compare', 'model_plots'))){
  dir.create(here::here(asmnt_yr, 'rsch', 'output', 'compare', 'model_plots'), recursive = TRUE)
}

## all base with asymp twl survey selex & ll survey selex ----
mdl_summ <- r4ss::SSsummarize(list(base_res_23, 
                                   update_base_res, 
                                   new_base_res, 
                                   twlsel_res, 
                                   llsel_res))
r4ss::SSplotComparisons(mdl_summ,
                        print = TRUE,
                        legendlabels = c(base_mdl, 
                                         base_mdl_update, 
                                         new_base,
                                         "Twl selex", 
                                         "LL selex"),
                        plotdir = here::here(asmnt_yr, 'rsch', 'output', 'compare', 'model_plots'))

## likes & abc ----
abc_comp <- data.frame(model = c(base_mdl, 
                                 base_mdl_update, 
                                 new_base,
                                 twlsel, 
                                 llsel),
                       abc = c(as.numeric(base_res_23$derived_quants %>% 
                                            filter(Label == 'ForeCatch_2024') %>% 
                                            select(Value)),
                               as.numeric(update_base_res$derived_quants %>% 
                                            filter(Label == 'ForeCatch_2025') %>% 
                                            select(Value)),
                               as.numeric(new_base_res$derived_quants %>% 
                                            filter(Label == 'ForeCatch_2025') %>% 
                                            select(Value)),
                               as.numeric(twlsel_res$derived_quants %>% 
                                            filter(Label == 'ForeCatch_2025') %>% 
                                            select(Value)),
                               as.numeric(llsel_res$derived_quants %>% 
                                            filter(Label == 'ForeCatch_2025') %>% 
                                            select(Value))))

vroom::vroom_write(abc_comp, here::here(asmnt_yr, 'rsch', 'output', 'compare', 'model_abc_comp.csv'), delim = ",")

vroom::vroom_write(mdl_summ$likelihoods %>% 
                     tidytable::rename('2019.1b-23' = model1,
                                       '2019.1b-24' = model2,
                                       '2019.1e.5cm-2024' = model3,
                                       'twlsel' = model4,
                                       'llsel' = model5), 
                   here::here(asmnt_yr, 'rsch', 'output', 'compare', 'model_summ_likes.csv'), delim = ",")


vroom::vroom_write(new_base_mscen$Two_year %>% 
                     tidytable::mutate(model = new_base) %>% 
                     tidytable::bind_rows(twlsel_mscen$Two_year %>% 
                                            tidytable::mutate(model = 'twlsel')) %>% 
                     tidytable::bind_rows(llsel_mscen$Two_year %>% 
                                            tidytable::mutate(model = 'llsel')),
                   here::here(asmnt_yr, 'rsch', 'output', 'compare', 'model_2yr.csv'), delim = ",")

## compare longline survey models ----

mdl_ll <- r4ss::SSsummarize(list(new_base_res,
                                 llsel_p1_res,
                                 llsel_p2_res,
                                 llsel_p3_res,
                                 llsel_p4_res,
                                 llsel_p6_res,
                                 llsel_comb_res))

vroom::vroom_write(mdl_ll$likelihoods %>% 
                     tidytable::rename('2019.1e.5cm-2024' = model1,
                                       'peak' = model2,
                                       'top_logit' = model3,
                                       'ascend_se' = model4,
                                       'descend_se' = model5,
                                       'start_logit' = model6,
                                       'combined' = model7), 
                   here::here(asmnt_yr, 'rsch', 'output', 'compare', 'model_ll_likes.csv'), delim = ",")

r4ss::SSplotComparisons(mdl_ll,
                        print = TRUE,
                        legendlabels = c(new_base,
                                         'peak',
                                         'top_logit',
                                         'ascend_se',
                                         'descend_se',
                                         'start_logit',
                                         'combined'),
                        plotdir = here::here(asmnt_yr, 'rsch', 'output', 'compare', 'model_plots'),
                        filenameprefix = 'llsrv_')

mdl_ll$npars
