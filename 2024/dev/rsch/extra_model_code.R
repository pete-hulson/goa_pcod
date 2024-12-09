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

# # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# # 2024.2: logistic & time-invariant survey selex ----
# new_base_selex <- "2024.2-2024"
# copy_mdl <- new_base
# 
# ## copy ss input files ----
# if(!file.exists(here::here(asmnt_yr, 'rsch', new_base_selex, 'ss3.exe'))){
#   start_ss_fldr(from = here::here(asmnt_yr, 'rsch', copy_mdl),
#                 to = here::here(asmnt_yr, 'rsch', new_base_selex))
# }
# 
# ## update files ----
# update_ss3_files(asmnt_yr, 
#                  folder = 'rsch',
#                  mdl = new_base_selex, 
#                  dat_filename = paste0("GOAPcod2024", dat_day, "_lcomp_bin5.dat"),
#                  ctl_in = "updated_ae.ctl",
#                  ctl_out = "Model24_2.ctl")
# 
# ## make 2024 changes to ctl file ----
# ctl_2024(asmnt_yr, 
#          folder = 'rsch',
#          mdl = new_base_selex, 
#          ctl_filename = 'Model24_2.ctl')
# 
# ## change selex in ctl ----
# # read ctl file
# ctl <- r4ss::SS_readctl_3.30(here::here(asmnt_yr, 'rsch', new_base_selex, 'Model24_2.ctl'))
# # fix maturity
# # ctl$MG_parms$INIT[which(rownames(ctl$MG_parms) == 'Mat50%_Fem_GP_1')] = 57.3
# # freeze ll srv to logistic
# ctl$size_selex_parms$INIT[which(rownames(ctl$size_selex_parms) == 'SizeSel_P_6_LLSrv(5)')] <- 10
# ctl$size_selex_parms$PHASE[which(rownames(ctl$size_selex_parms) == 'SizeSel_P_6_LLSrv(5)')] <- -2
# # set blocks to 0
# ctl$size_selex_parms$Block[seq(which(rownames(ctl$size_selex_parms) == 'SizeSel_P_1_Srv(4)'),
#                                which(rownames(ctl$size_selex_parms) == 'SizeSel_P_6_Srv(4)'))] <- 0
# ctl$size_selex_parms$Block_Fxn[seq(which(rownames(ctl$size_selex_parms) == 'SizeSel_P_1_Srv(4)'),
#                                    which(rownames(ctl$size_selex_parms) == 'SizeSel_P_6_Srv(4)'))] <- 0
# # remove from tv section
# ctl$size_selex_parms_tv <- ctl$size_selex_parms_tv[-seq(which(rownames(ctl$size_selex_parms_tv) == 'SizeSel_P_1_Srv(4)_BLK1repl_1996'),
#                                                         length(rownames(ctl$size_selex_parms_tv))),]
# # freeze twl srv to logistic
# ctl$size_selex_parms$INIT[which(rownames(ctl$size_selex_parms) == 'SizeSel_P_6_Srv(4)')] <- 10
# ctl$size_selex_parms$PHASE[which(rownames(ctl$size_selex_parms) == 'SizeSel_P_6_Srv(4)')] <- -2
# # write new ctl file
# r4ss::SS_writectl_3.30(ctllist = ctl,
#                        outfile = here::here(asmnt_yr, 'rsch', new_base_selex, 'Model24_2.ctl'),
#                        overwrite = TRUE)
# 
# ## run model ----
# run_ss3_model(asmnt_yr, 
#               folder = 'rsch',
#               mdl = new_base_selex,
#               ctl_filename = "Model24_2.ctl")
# 
# ## get and plot model output ----
# # get output
# new_base_selex_res <- r4ss::SS_output(dir = here::here(asmnt_yr, 'rsch', new_base_selex))
# # if exists, delete plot folder
# if(file.exists(here::here(asmnt_yr, 'rsch', new_base_selex, 'plots'))){
#   unlink(here::here(asmnt_yr, 'rsch', new_base_selex, 'plots'), recursive = TRUE)
# }
# # plot results
# r4ss::SS_plots(new_base_selex_res,
#                printfolder = "",
#                dir = here::here(asmnt_yr, 'rsch', new_base_selex, "plots"))
# 
# # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# # 2024.3: logistic twl surv & prior on ll surv selex ----
# new_base_selex2 <- "2024.3-2024"
# copy_mdl <- new_base
# 
# ## copy ss input files ----
# if(!file.exists(here::here(asmnt_yr, 'rsch', new_base_selex2, 'ss3.exe'))){
#   start_ss_fldr(from = here::here(asmnt_yr, 'rsch', copy_mdl),
#                 to = here::here(asmnt_yr, 'rsch', new_base_selex2))
# }
# 
# ## update files ----
# update_ss3_files(asmnt_yr, 
#                  folder = 'rsch',
#                  mdl = new_base_selex2, 
#                  dat_filename = paste0("GOAPcod2024", dat_day, "_lcomp_bin5.dat"),
#                  ctl_in = "updated_ae.ctl",
#                  ctl_out = "Model24_3.ctl")
# 
# ## make 2024 changes to ctl file ----
# ctl_2024(asmnt_yr, 
#          folder = 'rsch',
#          mdl = new_base_selex2, 
#          ctl_filename = 'Model24_3.ctl')
# 
# ## change selex in ctl ----
# # read ctl file
# ctl <- r4ss::SS_readctl_3.30(here::here(asmnt_yr, 'rsch', new_base_selex2, 'Model24_3.ctl'))
# # fix maturity
# # ctl$MG_parms$INIT[which(rownames(ctl$MG_parms) == 'Mat50%_Fem_GP_1')] = 57.3
# # penalize ll srv
# ctl$size_selex_parms$LO[which(rownames(ctl$size_selex_parms) == 'SizeSel_P_6_LLSrv(5)')] <- 0
# ctl$size_selex_parms$INIT[which(rownames(ctl$size_selex_parms) == 'SizeSel_P_6_LLSrv(5)')] <- 5
# ctl$size_selex_parms$PRIOR[which(rownames(ctl$size_selex_parms) == 'SizeSel_P_6_LLSrv(5)')] <- 5
# ctl$size_selex_parms$PR_SD[which(rownames(ctl$size_selex_parms) == 'SizeSel_P_6_LLSrv(5)')] <- 0.5
# ctl$size_selex_parms$PR_type[which(rownames(ctl$size_selex_parms) == 'SizeSel_P_6_LLSrv(5)')] <- 6
# # set blocks to 0
# ctl$size_selex_parms$Block[seq(which(rownames(ctl$size_selex_parms) == 'SizeSel_P_1_Srv(4)'),
#                                which(rownames(ctl$size_selex_parms) == 'SizeSel_P_6_Srv(4)'))] <- 0
# ctl$size_selex_parms$Block_Fxn[seq(which(rownames(ctl$size_selex_parms) == 'SizeSel_P_1_Srv(4)'),
#                                    which(rownames(ctl$size_selex_parms) == 'SizeSel_P_6_Srv(4)'))] <- 0
# # remove from tv section
# ctl$size_selex_parms_tv <- ctl$size_selex_parms_tv[-seq(which(rownames(ctl$size_selex_parms_tv) == 'SizeSel_P_1_Srv(4)_BLK1repl_1996'),
#                                                         length(rownames(ctl$size_selex_parms_tv))),]
# # freeze twl srv to logistic
# ctl$size_selex_parms$INIT[which(rownames(ctl$size_selex_parms) == 'SizeSel_P_6_Srv(4)')] <- 10
# ctl$size_selex_parms$PHASE[which(rownames(ctl$size_selex_parms) == 'SizeSel_P_6_Srv(4)')] <- -2
# # write new ctl file
# r4ss::SS_writectl_3.30(ctllist = ctl,
#                        outfile = here::here(asmnt_yr, 'rsch', new_base_selex2, 'Model24_3.ctl'),
#                        overwrite = TRUE)
# 
# ## run model ----
# run_ss3_model(asmnt_yr, 
#               folder = 'rsch',
#               mdl = new_base_selex2,
#               ctl_filename = "Model24_3.ctl")
# 
# ## get and plot model output ----
# # get output
# new_base_selex2_res <- r4ss::SS_output(dir = here::here(asmnt_yr, 'rsch', new_base_selex2))
# # if exists, delete plot folder
# if(file.exists(here::here(asmnt_yr, 'rsch', new_base_selex2, 'plots'))){
#   unlink(here::here(asmnt_yr, 'rsch', new_base_selex2, 'plots'), recursive = TRUE)
# }
# # plot results
# r4ss::SS_plots(new_base_selex2_res,
#                printfolder = "",
#                dir = here::here(asmnt_yr, 'rsch', new_base_selex2, "plots"))
# 
# 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2024.3: logistic & time-invariant survey selex, full t-v for fishery (2024.3) ----
# new_selex <- "2024.3-2024"
# copy_mdl <- new_base_lcomp_bin5
# copy_mdl_res <- new_base_lcomp_bin5_res
# 
# ## copy ss input files ----
# if(!file.exists(here::here(asmnt_yr, 'rsch', new_selex, 'ss3.exe'))){
#   start_ss_fldr(from = here::here(asmnt_yr, 'rsch', copy_mdl),
#                 to = here::here(asmnt_yr, 'rsch', new_selex))
# }
# 
# ## update files ----
# update_ss3_files(asmnt_yr, 
#                  folder = 'rsch',
#                  mdl = new_selex, 
#                  dat_filename = paste0("GOAPcod2024", dat_day, "_lcomp_bin5.dat"),
#                  ctl_in = "updated_ae.ctl",
#                  ctl_out = "Model24_3.ctl")
# 
# ## make 2024 changes to ctl file ----
# ctl_2024(asmnt_yr, 
#          folder = 'rsch',
#          mdl = new_selex, 
#          ctl_filename = 'Model24_3.ctl')
# 
# ## change selex in ctl ----
# # read ctl file
# ctl <- r4ss::SS_readctl_3.30(here::here(asmnt_yr, 'rsch', new_selex, 'Model24_3.ctl'))
# 
# # freeze ll srv to logistic
# ctl$size_selex_parms$INIT[which(rownames(ctl$size_selex_parms) == 'SizeSel_P_6_LLSrv(5)')] <- 10
# ctl$size_selex_parms$PHASE[which(rownames(ctl$size_selex_parms) == 'SizeSel_P_6_LLSrv(5)')] <- -2
# # set blocks to 0
# ctl$size_selex_parms$Block = 0
# ctl$size_selex_parms$Block_Fxn = 0
# # remove from tv section
# ctl$size_selex_parms_tv <- ctl$size_selex_parms_tv[-seq(which(rownames(ctl$size_selex_parms_tv) == 'SizeSel_P_1_Srv(4)_BLK1repl_1996'),
#                                                         length(rownames(ctl$size_selex_parms_tv))),]
# # freeze twl srv to logistic
# ctl$size_selex_parms$INIT[which(rownames(ctl$size_selex_parms) == 'SizeSel_P_6_Srv(4)')] <- 10
# ctl$size_selex_parms$PHASE[which(rownames(ctl$size_selex_parms) == 'SizeSel_P_6_Srv(4)')] <- -2
# # twl fishery
# ctl$size_selex_parms$dev_maxyr[which(rownames(ctl$size_selex_parms) == 'SizeSel_P_1_FshTrawl(1)')] = asmnt_yr
# ctl$size_selex_parms$dev_maxyr[which(rownames(ctl$size_selex_parms) == 'SizeSel_P_2_FshTrawl(1)')] = asmnt_yr
# ctl$size_selex_parms$dev_maxyr[which(rownames(ctl$size_selex_parms) == 'SizeSel_P_3_FshTrawl(1)')] = asmnt_yr
# ctl$size_selex_parms$dev_maxyr[which(rownames(ctl$size_selex_parms) == 'SizeSel_P_4_FshTrawl(1)')] = asmnt_yr
# ctl$size_selex_parms_tv = ctl$size_selex_parms_tv[-which(rownames(ctl$size_selex_parms_tv) == 'SizeSel_P_1_FshTrawl(1)_BLK2repl_1990'),]
# ctl$size_selex_parms_tv = ctl$size_selex_parms_tv[-which(rownames(ctl$size_selex_parms_tv) == 'SizeSel_P_1_FshTrawl(1)_BLK2repl_2005'),]
# ctl$size_selex_parms_tv = ctl$size_selex_parms_tv[-which(rownames(ctl$size_selex_parms_tv) == 'SizeSel_P_1_FshTrawl(1)_BLK2repl_2007'),]
# ctl$size_selex_parms_tv = ctl$size_selex_parms_tv[-which(rownames(ctl$size_selex_parms_tv) == 'SizeSel_P_1_FshTrawl(1)_BLK2repl_2017'),]
# ctl$size_selex_parms_tv = ctl$size_selex_parms_tv[-which(rownames(ctl$size_selex_parms_tv) == 'SizeSel_P_2_FshTrawl(1)_BLK2repl_1990'),]
# ctl$size_selex_parms_tv = ctl$size_selex_parms_tv[-which(rownames(ctl$size_selex_parms_tv) == 'SizeSel_P_2_FshTrawl(1)_BLK2repl_2005'),]
# ctl$size_selex_parms_tv = ctl$size_selex_parms_tv[-which(rownames(ctl$size_selex_parms_tv) == 'SizeSel_P_2_FshTrawl(1)_BLK2repl_2007'),]
# ctl$size_selex_parms_tv = ctl$size_selex_parms_tv[-which(rownames(ctl$size_selex_parms_tv) == 'SizeSel_P_2_FshTrawl(1)_BLK2repl_2017'),]
# ctl$size_selex_parms_tv = ctl$size_selex_parms_tv[-which(rownames(ctl$size_selex_parms_tv) == 'SizeSel_P_3_FshTrawl(1)_BLK2repl_1990'),]
# ctl$size_selex_parms_tv = ctl$size_selex_parms_tv[-which(rownames(ctl$size_selex_parms_tv) == 'SizeSel_P_3_FshTrawl(1)_BLK2repl_2005'),]
# ctl$size_selex_parms_tv = ctl$size_selex_parms_tv[-which(rownames(ctl$size_selex_parms_tv) == 'SizeSel_P_3_FshTrawl(1)_BLK2repl_2007'),]
# ctl$size_selex_parms_tv = ctl$size_selex_parms_tv[-which(rownames(ctl$size_selex_parms_tv) == 'SizeSel_P_3_FshTrawl(1)_BLK2repl_2017'),]
# ctl$size_selex_parms_tv = ctl$size_selex_parms_tv[-which(rownames(ctl$size_selex_parms_tv) == 'SizeSel_P_4_FshTrawl(1)_BLK2repl_1990'),]
# ctl$size_selex_parms_tv = ctl$size_selex_parms_tv[-which(rownames(ctl$size_selex_parms_tv) == 'SizeSel_P_4_FshTrawl(1)_BLK2repl_2005'),]
# ctl$size_selex_parms_tv = ctl$size_selex_parms_tv[-which(rownames(ctl$size_selex_parms_tv) == 'SizeSel_P_4_FshTrawl(1)_BLK2repl_2007'),]
# ctl$size_selex_parms_tv = ctl$size_selex_parms_tv[-which(rownames(ctl$size_selex_parms_tv) == 'SizeSel_P_4_FshTrawl(1)_BLK2repl_2017'),]
# # ll fishery
# ctl$size_selex_parms$dev_maxyr[which(rownames(ctl$size_selex_parms) == 'SizeSel_P_1_FshLL(2)')] = asmnt_yr
# ctl$size_selex_parms$dev_maxyr[which(rownames(ctl$size_selex_parms) == 'SizeSel_P_2_FshLL(2)')] = asmnt_yr
# ctl$size_selex_parms$dev_maxyr[which(rownames(ctl$size_selex_parms) == 'SizeSel_P_3_FshLL(2)')] = asmnt_yr
# ctl$size_selex_parms_tv = ctl$size_selex_parms_tv[-which(rownames(ctl$size_selex_parms_tv) == 'SizeSel_P_1_FshLL(2)_BLK2repl_1990'),]
# ctl$size_selex_parms_tv = ctl$size_selex_parms_tv[-which(rownames(ctl$size_selex_parms_tv) == 'SizeSel_P_1_FshLL(2)_BLK2repl_2005'),]
# ctl$size_selex_parms_tv = ctl$size_selex_parms_tv[-which(rownames(ctl$size_selex_parms_tv) == 'SizeSel_P_1_FshLL(2)_BLK2repl_2007'),]
# ctl$size_selex_parms_tv = ctl$size_selex_parms_tv[-which(rownames(ctl$size_selex_parms_tv) == 'SizeSel_P_1_FshLL(2)_BLK2repl_2017'),]
# ctl$size_selex_parms_tv = ctl$size_selex_parms_tv[-which(rownames(ctl$size_selex_parms_tv) == 'SizeSel_P_2_FshLL(2)_BLK2repl_1990'),]
# ctl$size_selex_parms_tv = ctl$size_selex_parms_tv[-which(rownames(ctl$size_selex_parms_tv) == 'SizeSel_P_2_FshLL(2)_BLK2repl_2005'),]
# ctl$size_selex_parms_tv = ctl$size_selex_parms_tv[-which(rownames(ctl$size_selex_parms_tv) == 'SizeSel_P_2_FshLL(2)_BLK2repl_2007'),]
# ctl$size_selex_parms_tv = ctl$size_selex_parms_tv[-which(rownames(ctl$size_selex_parms_tv) == 'SizeSel_P_2_FshLL(2)_BLK2repl_2017'),]
# ctl$size_selex_parms_tv = ctl$size_selex_parms_tv[-which(rownames(ctl$size_selex_parms_tv) == 'SizeSel_P_3_FshLL(2)_BLK2repl_1990'),]
# ctl$size_selex_parms_tv = ctl$size_selex_parms_tv[-which(rownames(ctl$size_selex_parms_tv) == 'SizeSel_P_3_FshLL(2)_BLK2repl_2005'),]
# ctl$size_selex_parms_tv = ctl$size_selex_parms_tv[-which(rownames(ctl$size_selex_parms_tv) == 'SizeSel_P_3_FshLL(2)_BLK2repl_2007'),]
# ctl$size_selex_parms_tv = ctl$size_selex_parms_tv[-which(rownames(ctl$size_selex_parms_tv) == 'SizeSel_P_3_FshLL(2)_BLK2repl_2017'),]
# ctl$size_selex_parms_tv = ctl$size_selex_parms_tv[-which(rownames(ctl$size_selex_parms_tv) == 'SizeSel_P_4_FshLL(2)_BLK2repl_1990'),]
# ctl$size_selex_parms_tv = ctl$size_selex_parms_tv[-which(rownames(ctl$size_selex_parms_tv) == 'SizeSel_P_4_FshLL(2)_BLK2repl_2005'),]
# ctl$size_selex_parms_tv = ctl$size_selex_parms_tv[-which(rownames(ctl$size_selex_parms_tv) == 'SizeSel_P_4_FshLL(2)_BLK2repl_2007'),]
# ctl$size_selex_parms_tv = ctl$size_selex_parms_tv[-which(rownames(ctl$size_selex_parms_tv) == 'SizeSel_P_4_FshLL(2)_BLK2repl_2017'),]
# # pot fishery
# ctl$size_selex_parms$dev_link[which(rownames(ctl$size_selex_parms) == 'SizeSel_P_1_FshPot(3)')] = 1
# ctl$size_selex_parms$dev_link[which(rownames(ctl$size_selex_parms) == 'SizeSel_P_3_FshPot(3)')] = 1
# ctl$size_selex_parms$dev_link[which(rownames(ctl$size_selex_parms) == 'SizeSel_P_6_FshPot(3)')] = 1
# ctl$size_selex_parms$dev_minyr[which(rownames(ctl$size_selex_parms) == 'SizeSel_P_1_FshPot(3)')] = 1991
# ctl$size_selex_parms$dev_minyr[which(rownames(ctl$size_selex_parms) == 'SizeSel_P_3_FshPot(3)')] = 1991
# ctl$size_selex_parms$dev_minyr[which(rownames(ctl$size_selex_parms) == 'SizeSel_P_6_FshPot(3)')] = 1991
# ctl$size_selex_parms$dev_maxyr[which(rownames(ctl$size_selex_parms) == 'SizeSel_P_1_FshPot(3)')] = asmnt_yr
# ctl$size_selex_parms$dev_maxyr[which(rownames(ctl$size_selex_parms) == 'SizeSel_P_3_FshPot(3)')] = asmnt_yr
# ctl$size_selex_parms$dev_maxyr[which(rownames(ctl$size_selex_parms) == 'SizeSel_P_6_FshPot(3)')] = asmnt_yr
# ctl$size_selex_parms$dev_PH[which(rownames(ctl$size_selex_parms) == 'SizeSel_P_1_FshPot(3)')] = 3
# ctl$size_selex_parms$dev_PH[which(rownames(ctl$size_selex_parms) == 'SizeSel_P_3_FshPot(3)')] = 3
# ctl$size_selex_parms$dev_PH[which(rownames(ctl$size_selex_parms) == 'SizeSel_P_6_FshPot(3)')] = 3
# ctl$size_selex_parms_tv = ctl$size_selex_parms_tv[-which(rownames(ctl$size_selex_parms_tv) == 'SizeSel_P_1_FshPot(3)_BLK3repl_2017'),]
# ctl$size_selex_parms_tv = ctl$size_selex_parms_tv[-which(rownames(ctl$size_selex_parms_tv) == 'SizeSel_P_2_FshPot(3)_BLK3repl_2017'),]
# ctl$size_selex_parms_tv = ctl$size_selex_parms_tv[-which(rownames(ctl$size_selex_parms_tv) == 'SizeSel_P_3_FshPot(3)_BLK3repl_2017'),]
# t1 = ctl$size_selex_parms_tv[which(rownames(ctl$size_selex_parms_tv) == 'SizeSel_P_1_FshTrawl(1)_dev_se'),]
# rownames(t1) = 'SizeSel_P_1_FshPot(3)_dev_se'
# t2 = ctl$size_selex_parms_tv[which(rownames(ctl$size_selex_parms_tv) == 'SizeSel_P_1_FshTrawl(1)_dev_autocorr'),]
# rownames(t2) = 'SizeSel_P_1_FshPot(3)_dev_autocorr'
# t3 = ctl$size_selex_parms_tv[which(rownames(ctl$size_selex_parms_tv) == 'SizeSel_P_1_FshTrawl(1)_dev_se'),]
# rownames(t3) = 'SizeSel_P_3_FshPot(3)_dev_se'
# t4 = ctl$size_selex_parms_tv[which(rownames(ctl$size_selex_parms_tv) == 'SizeSel_P_1_FshTrawl(1)_dev_autocorr'),]
# rownames(t4) = 'SizeSel_P_3_FshPot(3)_dev_autocorr'
# t5 = ctl$size_selex_parms_tv[which(rownames(ctl$size_selex_parms_tv) == 'SizeSel_P_1_FshTrawl(1)_dev_se'),]
# rownames(t5) = 'SizeSel_P_6_FshPot(3)_dev_se'
# t6 = ctl$size_selex_parms_tv[which(rownames(ctl$size_selex_parms_tv) == 'SizeSel_P_1_FshTrawl(1)_dev_autocorr'),]
# rownames(t6) = 'SizeSel_P_6_FshPot(3)_dev_autocorr'
# ctl$size_selex_parms_tv = ctl$size_selex_parms_tv %>% 
#   dplyr::bind_rows(t1) %>% 
#   dplyr::bind_rows(t2) %>% 
#   dplyr::bind_rows(t3) %>% 
#   dplyr::bind_rows(t4) %>% 
#   dplyr::bind_rows(t5) %>% 
#   dplyr::bind_rows(t6)
# # write new ctl file
# r4ss::SS_writectl_3.30(ctllist = ctl,
#                        outfile = here::here(asmnt_yr, 'rsch', new_selex, 'Model24_3.ctl'),
#                        overwrite = TRUE)
# 
# ## run model ----
# run_ss3_model(asmnt_yr, 
#               folder = 'rsch',
#               mdl = new_selex,
#               ctl_filename = "Model24_3.ctl")
# 
# ## get and plot model output ----
# # get output
# new_selex_res <- r4ss::SS_output(dir = here::here(asmnt_yr, 'rsch', new_selex))
# # if exists, delete plot folder
# if(file.exists(here::here(asmnt_yr, 'rsch', new_selex, 'plots'))){
#   unlink(here::here(asmnt_yr, 'rsch', new_selex, 'plots'), recursive = TRUE)
# }
# # plot results
# r4ss::SS_plots(new_selex_res,
#                printfolder = "",
#                dir = here::here(asmnt_yr, 'rsch', new_selex, "plots"))
