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

# source functions
source_files <- list.files(here::here(asmnt_yr, "R", "assessment"), "*.r$")
purrr::map(here::here(asmnt_yr, "R", "assessment", source_files), source)
source(here::here(asmnt_yr, "R", "utils.r"))

# run models? if not just get results
run_mdl = FALSE
run_retro = FALSE
# ret_yr <- 2 # For testing
ret_yr <- 10 # For full



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2023 base model (2019.1b-2023) ----
# read results from base model
base_mdl <- "2019.1b-2023" # 2023 accepted model

base_res_23 <- r4ss::SS_output(dir = here::here(asmnt_yr - 1, 'mgmt', base_mdl),
                               verbose = TRUE,
                               printstats = TRUE)

# retro_base <- r4ss::SSsummarize(r4ss::SSgetoutput(dirvec = file.path(
#   here::here(asmnt_yr, 'rsch', base_mdl), "retrospectives",
#   paste("retro", 0:-ret_yr, sep = ""))))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# updated base model (2019.1b) ----
# includes updated GAP tables and length-weight relationship
base_mdl_update <- "2019.1b-2024"

## copy ss input files ----
start_ss_fldr(from = here::here(asmnt_yr - 1, 'mgmt', base_mdl),
              to = here::here(asmnt_yr, 'rsch', base_mdl_update))

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
                 dat_filename = "GOAPcod2024Aug22_old.dat",
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

## run management scens ----
update_base_mscen <- Do_AK_TIER_3_Scenarios(DIR = here::here(asmnt_yr, 'rsch', base_mdl_update), 
                                            CYR = asmnt_yr,  
                                            FLEETS = c(1:3),
                                            do_fig = FALSE,
                                            do_mark = FALSE)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# new base model  (2019.1c) ----
# includes:
# - corrected ll sruvey sd
# - corrected ll survey length comps
# - fishery iss set at number of hauls that data is used from
# - surveyISS used for survey data
# - plus length bin set at 100 cm
new_base <- "2019.1c-2024"

## copy ss input files ----
start_ss_fldr(from = here::here(asmnt_yr, 'rsch', base_mdl_update),
              to = here::here(asmnt_yr, 'rsch', new_base))

## update files ----
update_ss3_files(asmnt_yr, 
                 folder = 'rsch',
                 mdl = new_base, 
                 dat_filename = "GOAPcod2024Aug22.dat",
                 ctl_in = "Model19_1b.ctl",
                 ctl_out = "Model19_1c.ctl")

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

## run management scens ----
new_base_mscen <- Do_AK_TIER_3_Scenarios(DIR = here::here(asmnt_yr, 'rsch', new_base), 
                                         CYR = asmnt_yr,  
                                         FLEETS = c(1:3),
                                         do_fig = FALSE,
                                         do_mark = FALSE)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# update ageing error (2019.1d) ----
new_base_ae <- "2019.1d-2024"

## copy ss input files ----
start_ss_fldr(from = here::here(asmnt_yr, 'rsch', new_base),
              to = here::here(asmnt_yr, 'rsch', new_base_ae))

## update files ----
update_ss3_files(asmnt_yr, 
                 folder = 'rsch',
                 mdl = new_base_ae, 
                 dat_filename = "GOAPcod2024Aug22.dat",
                 ctl_in = "updated_ae.ctl",
                 ctl_out = "Model19_1d.ctl")

## run model ----
run_ss3_model(asmnt_yr, 
              folder = 'rsch',
              mdl = new_base_ae,
              ctl_filename = "Model19_1d.ctl")

## get and plot model output ----
# get output
new_base_ae_res <- r4ss::SS_output(dir = here::here(asmnt_yr, 'rsch', new_base_ae))
# if exists, delete plot folder
if(file.exists(here::here(asmnt_yr, 'rsch', new_base_ae, 'plots'))){
  unlink(here::here(asmnt_yr, 'rsch', new_base_ae, 'plots'), recursive = TRUE)
}
# plot results
r4ss::SS_plots(new_base_ae_res,
               printfolder = "",
               dir = here::here(asmnt_yr, 'rsch', new_base_ae, "plots"))

## run management scens ----
new_base_ae_mscen <- Do_AK_TIER_3_Scenarios(DIR = here::here(asmnt_yr, 'rsch', new_base_ae), 
                                            CYR = asmnt_yr,  
                                            FLEETS = c(1:3),
                                            do_fig = FALSE,
                                            do_mark = FALSE)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# new fishery length comps (2019.1e) ----
# remove filters and aggregate at trimester-area-gear
new_base_lcomp <- "2019.1e-2024"

## copy ss input files ----
start_ss_fldr(from = here::here(asmnt_yr, 'rsch', new_base_ae),
              to = here::here(asmnt_yr, 'rsch', new_base_lcomp))

## update files ----
update_ss3_files(asmnt_yr, 
                 folder = 'rsch',
                 mdl = new_base_lcomp, 
                 dat_filename = "GOAPcod2024Aug22_lcomp.dat",
                 ctl_in = "updated_ae.ctl",
                 ctl_out = "Model19_1e.ctl")

## run model ----
run_ss3_model(asmnt_yr, 
              folder = 'rsch',
              mdl = new_base_lcomp,
              ctl_filename = "Model19_1e.ctl")

## get and plot model output ----
# get output
new_base_lcomp_res <- r4ss::SS_output(dir = here::here(asmnt_yr, 'rsch', new_base_lcomp))
# if exists, delete plot folder
if(file.exists(here::here(asmnt_yr, 'rsch', new_base_lcomp, 'plots'))){
  unlink(here::here(asmnt_yr, 'rsch', new_base_lcomp, 'plots'), recursive = TRUE)
}
# plot results
r4ss::SS_plots(new_base_lcomp_res,
               printfolder = "",
               dir = here::here(asmnt_yr, 'rsch', new_base_lcomp, "plots"))

## run management scens ----
new_base_lcomp_mscen <- Do_AK_TIER_3_Scenarios(DIR = here::here(asmnt_yr, 'rsch', new_base_lcomp), 
                                               CYR = asmnt_yr,  
                                               FLEETS = c(1:3),
                                               do_fig = FALSE,
                                               do_mark = FALSE)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2 cm bin (2019.1f.2) ----
new_base_bin2 <- "2019.1f.2-2024"

## copy ss input files ----
start_ss_fldr(from = here::here(asmnt_yr, 'rsch', new_base_lcomp),
              to = here::here(asmnt_yr, 'rsch', new_base_bin2))

## update files ----
update_ss3_files(asmnt_yr, 
                 folder = 'rsch',
                 mdl = new_base_bin2, 
                 dat_filename = "GOAPcod2024Aug22_bin2.dat",
                 ctl_in = "updated_ae.ctl",
                 ctl_out = "Model19_1f.ctl")

## run model ----
run_ss3_model(asmnt_yr, 
              folder = 'rsch',
              mdl = new_base_bin2,
              ctl_filename = "Model19_1f.ctl")

## get and plot model output ----
# get output
new_base_bin2_res <- r4ss::SS_output(dir = here::here(asmnt_yr, 'rsch', new_base_bin2))
# if exists, delete plot folder
if(file.exists(here::here(asmnt_yr, 'rsch', new_base_bin2, 'plots'))){
  unlink(here::here(asmnt_yr, 'rsch', new_base_bin2, 'plots'), recursive = TRUE)
}
# plot results
r4ss::SS_plots(new_base_bin2_res,
               printfolder = "",
               dir = here::here(asmnt_yr, 'rsch', new_base_bin2, "plots"))

## run management scens ----
new_base_bin2_mscen <- Do_AK_TIER_3_Scenarios(DIR = here::here(asmnt_yr, 'rsch', new_base_bin2), 
                                              CYR = asmnt_yr,  
                                              FLEETS = c(1:3),
                                              do_fig = FALSE,
                                              do_mark = FALSE)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 5 cm bin (2019.1f.5) ----
new_base_bin5 <- "2019.1f.5-2024"

## copy ss input files ----
start_ss_fldr(from = here::here(asmnt_yr, 'rsch', new_base_lcomp),
              to = here::here(asmnt_yr, 'rsch', new_base_bin5))

## update files ----
update_ss3_files(asmnt_yr, 
                 folder = 'rsch',
                 mdl = new_base_bin5, 
                 dat_filename = "GOAPcod2024Aug22_bin5.dat",
                 ctl_in = "updated_ae.ctl",
                 ctl_out = "Model19_1f.ctl")

## run model ----
run_ss3_model(asmnt_yr, 
              folder = 'rsch',
              mdl = new_base_bin5,
              ctl_filename = "Model19_1f.ctl")

## get and plot model output ----
# get output
new_base_bin5_res <- r4ss::SS_output(dir = here::here(asmnt_yr, 'rsch', new_base_bin5))
# if exists, delete plot folder
if(file.exists(here::here(asmnt_yr, 'rsch', new_base_bin5, 'plots'))){
  unlink(here::here(asmnt_yr, 'rsch', new_base_bin5, 'plots'), recursive = TRUE)
}
# plot results
r4ss::SS_plots(new_base_bin5_res,
               printfolder = "",
               dir = here::here(asmnt_yr, 'rsch', new_base_bin5, "plots"))

## run management scens ----
new_base_bin5_mscen <- Do_AK_TIER_3_Scenarios(DIR = here::here(asmnt_yr, 'rsch', new_base_bin5), 
                                              CYR = asmnt_yr,  
                                              FLEETS = c(1:3),
                                              do_fig = FALSE,
                                              do_mark = FALSE)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# plot data comparisons ----
if (!file.exists(here::here(asmnt_yr, 'rsch', 'output', 'compare', 'data_plots'))){
  dir.create(here::here(asmnt_yr, 'rsch', 'output', 'compare', 'data_plots'), recursive = TRUE)
}

data_summ <- r4ss::SSsummarize(list(base_res_23, update_base_res, new_base_res, new_base_ae_res, new_base_lcomp_res, new_base_bin2_res, new_base_bin5_res))

r4ss::SSplotComparisons(data_summ,
                        print = TRUE,
                        legendlabels = c(base_mdl, base_mdl_update, new_base, new_base_ae, new_base_lcomp, new_base_bin2, new_base_bin5),
                        plotdir = here::here(asmnt_yr, 'rsch', 'output', 'compare', 'data_plots'))


vroom::vroom_write(data_summ$likelihoods %>% 
                     tidytable::rename('2019.1b-23' = model1,
                                       '2019.1b-24' = model2,
                                       '2019.1c' = model3,
                                       '2019.1d' = model4,
                                       '2019.1e' = model5,
                                       '2019.1f.2' = model6,
                                       '2019.1f.5' = model7), 
                   here::here(asmnt_yr, 'rsch', 'output', 'compare', 'data_summ_likes.csv'), delim = ",")
vroom::vroom_write(data_summ$likelihoods_by_fleet %>% 
                     tidytable::mutate(model = case_when(model == 1 ~ '2019.1b-23',
                                                         model == 2 ~ '2019.1b-24',
                                                         model == 3 ~ '2019.1c',
                                                         model == 4 ~ '2019.1d',
                                                         model == 5 ~ '2019.1e',
                                                         model == 6 ~ '2019.1f.2',
                                                         model == 7 ~ '2019.1f.5')), 
                   here::here(asmnt_yr, 'rsch', 'output', 'compare', 'data_summ_likes_by_fleet.csv'), delim = ",")


abc_comp <- data.frame(model = c(base_mdl_update, new_base, new_base_ae, new_base_lcomp, new_base_bin2, new_base_bin5)) %>% 
  tidytable::bind_cols(data.frame(abc = update_base_mscen$Two_year$C_ABC[1]) %>% 
                         tidytable::bind_rows(data.frame(abc = new_base_mscen$Two_year$C_ABC[1])) %>% 
                         tidytable::bind_rows(data.frame(abc = new_base_ae_mscen$Two_year$C_ABC[1])) %>% 
                         tidytable::bind_rows(data.frame(abc = new_base_lcomp_mscen$Two_year$C_ABC[1])) %>% 
                         tidytable::bind_rows(data.frame(abc = new_base_bin2_mscen$Two_year$C_ABC[1])) %>% 
                         tidytable::bind_rows(data.frame(abc = new_base_bin5_mscen$Two_year$C_ABC[1])))


vroom::vroom_write(abc_comp, here::here(asmnt_yr, 'rsch', 'output', 'compare', 'data_abc_comp.csv'), delim = ",")




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# time-invariant trawl survey selex (2024.0) ----
new_base_twlsel <- "2024.0-2024"

## copy ss input files ----
start_ss_fldr(from = here::here(asmnt_yr, 'rsch', new_base_bin5),
              to = here::here(asmnt_yr, 'rsch', new_base_twlsel))

## update files ----
update_ss3_files(asmnt_yr, 
                 folder = 'rsch',
                 mdl = new_base_twlsel, 
                 dat_filename = "GOAPcod2024Aug22_bin5.dat",
                 ctl_in = "updated_ae.ctl",
                 ctl_out = "Model24_0.ctl")

## change selex in ctl ----

# copy data_echo.ss_new file so you can read ctl file
file.copy(here::here(asmnt_yr, 'rsch', new_base_bin5, 'data_echo.ss_new'),
          here::here(asmnt_yr, 'rsch', new_base_twlsel, 'data_echo.ss_new'))

# read ctl file
# read in previous assessment ss3 ctl
ctl <- r4ss::SS_readctl_3.30(here::here(asmnt_yr, 'rsch', new_base_twlsel, 'Model24_0.ctl'))
# set blocks to 0
ctl$size_selex_parms$Block[seq(which(rownames(ctl$size_selex_parms) == 'SizeSel_P_1_Srv(4)'),
                               which(rownames(ctl$size_selex_parms) == 'SizeSel_P_6_Srv(4)'))] <- 0
ctl$size_selex_parms$Block_Fxn[seq(which(rownames(ctl$size_selex_parms) == 'SizeSel_P_1_Srv(4)'),
                                   which(rownames(ctl$size_selex_parms) == 'SizeSel_P_6_Srv(4)'))] <- 0
# remove from tv section
ctl$size_selex_parms_tv <- ctl$size_selex_parms_tv[-seq(which(rownames(ctl$size_selex_parms_tv) == 'SizeSel_P_1_Srv(4)_BLK1repl_1996'),
                                                        length(rownames(ctl$size_selex_parms_tv))),]

# set init params at most recent values
ctl$size_selex_parms$INIT[which(rownames(ctl$size_selex_parms) == 'SizeSel_P_1_Srv(4)')] <- new_base_bin5_res$estimated_non_dev_parameters$Value[which(rownames(new_base_bin5_res$estimated_non_dev_parameters) == 'Size_DblN_peak_Srv(4)_BLK1repl_2006')]
ctl$size_selex_parms$INIT[which(rownames(ctl$size_selex_parms) == 'SizeSel_P_2_Srv(4)')] <- new_base_bin5_res$estimated_non_dev_parameters$Value[which(rownames(new_base_bin5_res$estimated_non_dev_parameters) == 'Size_DblN_top_logit_Srv(4)_BLK1repl_2006')]
ctl$size_selex_parms$INIT[which(rownames(ctl$size_selex_parms) == 'SizeSel_P_3_Srv(4)')] <- new_base_bin5_res$estimated_non_dev_parameters$Value[which(rownames(new_base_bin5_res$estimated_non_dev_parameters) == 'Size_DblN_ascend_se_Srv(4)_BLK1repl_2006')]
ctl$size_selex_parms$INIT[which(rownames(ctl$size_selex_parms) == 'SizeSel_P_4_Srv(4)')] <- new_base_bin5_res$estimated_non_dev_parameters$Value[which(rownames(new_base_bin5_res$estimated_non_dev_parameters) == 'Size_DblN_descend_se_Srv(4)_BLK1repl_2006')]
ctl$size_selex_parms$INIT[which(rownames(ctl$size_selex_parms) == 'SizeSel_P_5_Srv(4)')] <- new_base_bin5_res$estimated_non_dev_parameters$Value[which(rownames(new_base_bin5_res$estimated_non_dev_parameters) == 'Size_DblN_start_logit_Srv(4)_BLK1repl_2006')]
ctl$size_selex_parms$INIT[which(rownames(ctl$size_selex_parms) == 'SizeSel_P_6_Srv(4)')] <- 10

# fix width of srvy selex
ctl$size_selex_parms$PHASE[which(rownames(ctl$size_selex_parms) == 'SizeSel_P_1_Srv(4)')] <- -2
ctl$size_selex_parms$PHASE[which(rownames(ctl$size_selex_parms) == 'SizeSel_P_2_Srv(4)')] <- -2
ctl$size_selex_parms$PHASE[which(rownames(ctl$size_selex_parms) == 'SizeSel_P_3_Srv(4)')] <- -2
ctl$size_selex_parms$PHASE[which(rownames(ctl$size_selex_parms) == 'SizeSel_P_4_Srv(4)')] <- -2
ctl$size_selex_parms$PHASE[which(rownames(ctl$size_selex_parms) == 'SizeSel_P_5_Srv(4)')] <- -2
ctl$size_selex_parms$PHASE[which(rownames(ctl$size_selex_parms) == 'SizeSel_P_6_Srv(4)')] <- -2

# write new ctl file
r4ss::SS_writectl_3.30(ctllist = ctl,
                       outfile = here::here(asmnt_yr, 'rsch', new_base_twlsel, 'Model24_0.ctl'),
                       overwrite = TRUE)

## run model ----
run_ss3_model(asmnt_yr, 
              folder = 'rsch',
              mdl = new_base_twlsel,
              ctl_filename = "Model24_0.ctl",
              iters = 3)

## get and plot model output ----
# get output
new_base_twlsel_res <- r4ss::SS_output(dir = here::here(asmnt_yr, 'rsch', new_base_twlsel))
# if exists, delete plot folder
if(file.exists(here::here(asmnt_yr, 'rsch', new_base_twlsel, 'plots'))){
  unlink(here::here(asmnt_yr, 'rsch', new_base_twlsel, 'plots'), recursive = TRUE)
}
# plot results
r4ss::SS_plots(new_base_twlsel_res,
               printfolder = "",
               dir = here::here(asmnt_yr, 'rsch', new_base_twlsel, "plots"))

## run management scens ----
new_base_twlsel_mscen <- Do_AK_TIER_3_Scenarios(DIR = here::here(asmnt_yr, 'rsch', new_base_twlsel), 
                                                CYR = asmnt_yr,  
                                                FLEETS = c(1:3),
                                                do_fig = FALSE,
                                                do_mark = FALSE)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# time-invariant selex (2024.1) ----
new_base_selex <- "2024.1-2024"

## copy ss input files ----
start_ss_fldr(from = here::here(asmnt_yr, 'rsch', new_base_twlsel),
              to = here::here(asmnt_yr, 'rsch', new_base_selex))

## update files ----
update_ss3_files(asmnt_yr, 
                 folder = 'rsch',
                 mdl = new_base_selex, 
                 dat_filename = "GOAPcod2024Aug22_bin5.dat",
                 ctl_in = "updated_ae.ctl",
                 ctl_out = "Model24_1.ctl")

## change selex in ctl ----

# copy data_echo.ss_new file so you can read ctl file
file.copy(here::here(asmnt_yr, 'rsch', new_base_twlsel, 'data_echo.ss_new'),
          here::here(asmnt_yr, 'rsch', new_base_selex, 'data_echo.ss_new'))

# read ctl file
# read in previous assessment ss3 ctl
ctl <- r4ss::SS_readctl_3.30(here::here(asmnt_yr, 'rsch', new_base_selex, 'Model24_1.ctl'))
# set td pars to 0
ctl$size_selex_parms$dev_link <- 0
ctl$size_selex_parms$dev_minyr <- 0
ctl$size_selex_parms$dev_maxyr <- 0
ctl$size_selex_parms$dev_PH <- 0
ctl$size_selex_parms$Block <- 0
ctl$size_selex_parms$Block_Fxn <- 0

# remove from tv section
ctl$size_selex_parms_tv <- NULL

# write new ctl file
r4ss::SS_writectl_3.30(ctllist = ctl,
                       outfile = here::here(asmnt_yr, 'rsch', new_base_selex, 'Model24_1.ctl'),
                       overwrite = TRUE)

## run model ----
run_ss3_model(asmnt_yr, 
              folder = 'rsch',
              mdl = new_base_selex,
              ctl_filename = "Model24_1.ctl")

## get and plot model output ----
# get output
new_base_selex_res <- r4ss::SS_output(dir = here::here(asmnt_yr, 'rsch', new_base_selex))
# if exists, delete plot folder
if(file.exists(here::here(asmnt_yr, 'rsch', new_base_selex, 'plots'))){
  unlink(here::here(asmnt_yr, 'rsch', new_base_selex, 'plots'), recursive = TRUE)
}
# plot results
r4ss::SS_plots(new_base_selex_res,
               printfolder = "",
               dir = here::here(asmnt_yr, 'rsch', new_base_selex, "plots"))

## run management scens ----
new_base_selex_mscen <- Do_AK_TIER_3_Scenarios(DIR = here::here(asmnt_yr, 'rsch', new_base_selex), 
                                               CYR = asmnt_yr,  
                                               FLEETS = c(1:3),
                                               do_fig = FALSE,
                                               do_mark = FALSE)





# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# plot model comparisons ----
if (!file.exists(here::here(asmnt_yr, 'rsch', 'output', 'compare', 'model_plots'))){
  dir.create(here::here(asmnt_yr, 'rsch', 'output', 'compare', 'model_plots'), recursive = TRUE)
}

mdl_summ <- r4ss::SSsummarize(list(base_res_23, update_base_res, new_base_ae_res, new_base_bin5_res, new_base_twlsel_res, new_base_selex_res))

r4ss::SSplotComparisons(mdl_summ,
                        print = TRUE,
                        legendlabels = c(base_mdl, base_mdl_update, new_base_ae, new_base_bin5, new_base_twlsel, new_base_selex),
                        plotdir = here::here(asmnt_yr, 'rsch', 'output', 'compare', 'model_plots'))


vroom::vroom_write(mdl_summ$likelihoods %>% 
                     tidytable::rename('2019.1b-23' = model1,
                                       '2019.1b-24' = model2,
                                       '2019.1d' = model3,
                                       '2019.1f.5' = model4,
                                       '2024.0' = model5,
                                       '2024.1' = model6), 
                   here::here(asmnt_yr, 'rsch', 'output', 'compare', 'model_summ_likes.csv'), delim = ",")
vroom::vroom_write(mdl_summ$likelihoods_by_fleet %>% 
                     tidytable::mutate(model = case_when(model == 1 ~ '2019.1b-23',
                                                         model == 2 ~ '2019.1b-24',
                                                         model == 3 ~ '2019.1d',
                                                         model == 4 ~ '2019.1f.5',
                                                         model == 5 ~ '2024.0',
                                                         model == 6 ~ '2024.1')), 
                   here::here(asmnt_yr, 'rsch', 'output', 'compare', 'model_summ_likes_by_fleet.csv'), delim = ",")


abc_comp <- data.frame(model = c(base_mdl_update, new_base_ae, new_base_bin5, new_base_twlsel, new_base_selex)) %>% 
  tidytable::bind_cols(data.frame(abc = update_base_mscen$Two_year$C_ABC[1]) %>% 
                         tidytable::bind_rows(data.frame(abc = new_base_ae_mscen$Two_year$C_ABC[1])) %>% 
                         tidytable::bind_rows(data.frame(abc = new_base_bin5_mscen$Two_year$C_ABC[1])) %>% 
                         tidytable::bind_rows(data.frame(abc = new_base_twlsel_mscen$Two_year$C_ABC[1])) %>% 
                         tidytable::bind_rows(data.frame(abc = new_base_selex_mscen$Two_year$C_ABC[1])))


vroom::vroom_write(abc_comp, here::here(asmnt_yr, 'rsch', 'output', 'compare', 'model_abc_comp.csv'), delim = ",")













# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# minsamplesize corr (model 2019.1b) ----

# copy over the stock synthesis model files to the new directory for model 2019.1b
# start_ss_fldr(from = here::here(asmnt_yr, 'rsch', base_mdl),
#               to = here::here(asmnt_yr, 'rsch', new_base))

if(isTRUE(run_mdl)){
  
  # correct minsamplesize in dat file
  dat <- r4ss::SS_readdat(here::here(asmnt_yr, 'rsch', base_mdl, list.files(here::here(asmnt_yr, 'rsch', base_mdl), pattern = '.dat')))
  
  dat$age_info$minsamplesize <- 0.01
  
  r4ss::SS_writedat(datlist = dat, 
                    outfile = here::here(asmnt_yr, 'rsch', new_base, list.files(here::here(asmnt_yr, 'rsch', new_base), pattern = '.dat')),
                    overwrite = TRUE)
  
  # run model
  run_ss_model(asmnt_yr, new_base)
}

# read the model output and print diagnostic messages
new_base_res <- r4ss::SS_output(dir = here::here(asmnt_yr, 'rsch', new_base),
                                verbose = TRUE,
                                printstats = TRUE)

# run retro
if(isTRUE(run_retro)){
  r4ss::SS_doRetro(masterdir = here::here(asmnt_yr, 'rsch', new_base),
                   oldsubdir = "",
                   newsubdir = "retrospectives",
                   years = 0:-ret_yr)
}

# get retro results
retro_new_base <- r4ss::SSsummarize(r4ss::SSgetoutput(dirvec = file.path(
  here::here(asmnt_yr, 'rsch', new_base), "retrospectives",
  paste("retro", 0:-ret_yr, sep = ""))))

vroom::vroom_write(retro_new_base$likelihoods, here::here(asmnt_yr, 'rsch', 'output', 'retro_new_base_likes.csv'), delim = ",")
base::save(retro_new_base, file = here::here(asmnt_yr, 'rsch', 'output', 'retro_new_base.RData'))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# plots, results and stuff ----


base_mdl <- "2019.1a-2022" # 2022 accepted model
new_base <- "2019.1b-2022" # 2022 model with minsamplesize correction
new_base_noll <- "2019.1c-2022" # 2022 model with no llq env link
new_mdl1 <- "2023.1-2022" # 2022 model with env growth link
new_mdl2 <- "2023.2-2022" # 2022 model with env growth link and pref llq env link


# 2019.1a
r4ss::SS_plots(base_res,
               printfolder = "",
               dir = here::here(asmnt_yr, 'rsch', base_mdl, "plots"))

# 2019.1b
r4ss::SS_plots(new_base_res,
               printfolder = "",
               dir = here::here(asmnt_yr, 'rsch', new_base, "plots"))

# 2019.1c
r4ss::SS_plots(new_base_noll,
               printfolder = "",
               dir = here::here(asmnt_yr, 'rsch', new_base_noll, "plots"))

# 2023.1
r4ss::SS_plots(mdl1_res,
               printfolder = "",
               dir = here::here(asmnt_yr, 'rsch', new_mdl1, "plots"))

# 2023.2
r4ss::SS_plots(mdl2_res,
               printfolder = "",
               dir = here::here(asmnt_yr, 'rsch', new_mdl2, "plots"))

# plot 2019.1a vs 2019.1b for new base
base_summ <- r4ss::SSsummarize(list(base_res, new_base_res))

r4ss::SSplotComparisons(base_summ,
                        print = TRUE,
                        legendlabels = c(base_mdl, new_base),
                        plotdir = here::here(asmnt_yr, 'rsch', 'output', 'plots', 'new_base'))

vroom::vroom_write(base_summ$likelihoods, here::here(asmnt_yr, 'rsch', 'output', 'plots', 'new_base', 'base_summ_likes.csv'), delim = ",")
vroom::vroom_write(base_summ$likelihoods_by_fleet, here::here(asmnt_yr, 'rsch', 'output', 'plots', 'new_base', 'base_summ_likes_by_fleet.csv'), delim = ",")

# plot 2019.1a vs 2019.1b vs 2019.1d
llq_summ <- r4ss::SSsummarize(list(base_res, new_base_res, new_base_llq_res))

r4ss::SSplotComparisons(llq_summ,
                        print = TRUE,
                        pch = "",
                        legendlabels = c(base_mdl, new_base, new_base_llq),
                        plotdir = here::here(asmnt_yr, 'rsch', 'output', 'plots', 'llq'))

vroom::vroom_write(llq_summ$likelihoods, here::here(asmnt_yr, 'rsch', 'output', 'plots', 'llq', 'llq_summ_likes.csv'), delim = ",")
vroom::vroom_write(llq_summ$likelihoods_by_fleet, here::here(asmnt_yr, 'rsch', 'output', 'plots', 'llq', 'llq_summ_likes_by_fleet.csv'), delim = ",")

# plot 2019.1b vs 2019.1c
llq_noel_summ <- r4ss::SSsummarize(list(new_base_res, new_base_noll_res))

r4ss::SSplotComparisons(llq_noel_summ,
                        print = TRUE,
                        pch = "",
                        legendlabels = c(new_base, new_base_noll),
                        plotdir = here::here(asmnt_yr, 'rsch', 'output', 'plots', 'llq_no_el'))

rstats_new_base <- r4ss::SSmohnsrho(retro_new_base)
rstats_new_base_noll <- r4ss::SSmohnsrho(retro_new_base_noll)
rstats_new_base_llq <- r4ss::SSmohnsrho(retro_new_base_llq)

rstats_new_base$AFSC_Hurtado_SSB
rstats_new_base_noll$AFSC_Hurtado_SSB
rstats_new_base_llq$AFSC_Hurtado_SSB


# make statistics table for 2019.1b vs 2019.1c vs 2019.1d
llq_summ$likelihoods %>% 
  tidytable::filter(Label == 'TOTAL') %>%
  tidytable::select(-Label) %>% 
  tidytable::pivot_longer(names_to = 'model', values_to = 'tot_like') %>% 
  tidytable::mutate(model = c(new_base, new_base_noll, new_base_llq)) %>% 
  tidytable::bind_cols(dplyr::as_tibble(llq_summ$npars)) %>% 
  tidytable::rename(npars = 'value') %>% 
  tidytable::mutate(aic = 2 * npars + 2* tot_like) %>% 
  vroom::vroom_write(., here::here(asmnt_yr, 'rsch', 'output', 'plots', 'llq', 'aic_tbl.csv'), delim = ",")
















# plot 2019.1b vs 2023.1
grwth_summ <- r4ss::SSsummarize(list(new_base_res, mdl1_res))

r4ss::SSplotComparisons(grwth_summ,
                        print = TRUE,
                        pch = "",
                        legendlabels = c(new_base, new_mdl1),
                        plotdir = here::here(asmnt_yr, 'rsch', 'output', 'plots', 'grwth'))

vroom::vroom_write(grwth_summ$likelihoods, here::here(asmnt_yr, 'rsch', 'output', 'plots', 'grwth', 'grwth_summ_likes.csv'), delim = ",")
vroom::vroom_write(grwth_summ$likelihoods_by_fleet, here::here(asmnt_yr, 'rsch', 'output', 'plots', 'grwth', 'grwth_summ_likes_by_fleet.csv'), delim = ",")


# make statistics table for 2019.1b vs 2023.1
grwth_summ$likelihoods %>% 
  tidytable::filter(Label == 'TOTAL') %>%
  tidytable::select(-Label) %>% 
  tidytable::pivot_longer(names_to = 'model', values_to = 'tot_like') %>% 
  tidytable::mutate(model = c(new_base, new_mdl1)) %>% 
  tidytable::bind_cols(dplyr::as_tibble(grwth_summ$npars)) %>% 
  tidytable::rename(npars = 'value') %>% 
  tidytable::mutate(aic = 2 * npars + 2* tot_like) %>% 
  vroom::vroom_write(., here::here(asmnt_yr, 'rsch', 'output', 'plots', 'grwth', 'aic_tbl.csv'), delim = ",")

# do retro comp for 2019.1b vs 2023.1

retro_new_base$likelihoods %>% 
  tidytable::filter(Label == 'TOTAL') %>% 
  tidytable::mutate(model = new_base) %>% 
  tidytable::bind_rows(retro_mdl1$likelihoods %>% 
                         tidytable::filter(Label == 'TOTAL') %>% 
                         tidytable::mutate(model = new_mdl1)) %>% 
  vroom::vroom_write(., here::here(asmnt_yr, 'rsch', 'output', 'plots', 'grwth', 'retro_like_tbl.csv'), delim = ",")

r4ss::SSmohnsrho(retro_new_base)
r4ss::SSmohnsrho(retro_mdl1)







# plot 2019.1b vs 2019.1c vs 2023.2
llq_summ <- r4ss::SSsummarize(list(new_base_res, new_base_noll_res, mdl2_res))

r4ss::SSplotComparisons(llq_summ,
                        print = TRUE,
                        legendlabels = c(new_base, new_base_noll, new_mdl2),
                        plotdir = here::here(asmnt_yr, 'rsch', 'output', 'plots', 'llq'))















# do retro comp for  2019.1b vs 2019.1c vs 2023.2

retro_new_base$likelihoods %>% 
  tidytable::filter(Label == 'TOTAL') %>% 
  tidytable::mutate(model = new_base) %>% 
  tidytable::bind_rows(retro_mdl1$likelihoods %>% 
                         tidytable::filter(Label == 'TOTAL') %>% 
                         tidytable::mutate(model = new_mdl1)) %>% 
  vroom::vroom_write(., here::here(asmnt_yr, 'rsch', 'output', 'plots', 'grwth', 'retro_like_tbl.csv'), delim = ",")






retro_new_base$pars %>% 
  tidytable::select(Label, replist2) %>% 
  tidytable::filter(replist2 != 0 & is.na(replist2) == FALSE) %>% 
  tidytable::mutate(strs = as.character(format(replist2, scientific = F))) %>% 
  print(n = 230)


get_p <- function(retro){
  retro_new_base$parsSD %>% 
    tidytable::select(retro) %>% 
    tidytable::rename(pars = retro) %>% 
    tidytable::filter(is.na(pars) == FALSE & pars != 1 & pars != 0.44) %>% 
    tidytable::summarise(p = length(pars))
}

get_p('replist1')
get_p('replist2')

retro = 'replist1'




countDecimalPlaces <- function(x) {
  if ((x %% 1) != 0) {
    strs <- strsplit(as.character(format(x, scientific = F)), "\\.")
    n <- nchar(strs[[1]][2])
  } else {
    n <- 0
  }
  return(n) 
}


ss3diags::SSplotModelcomp(all_summ)



# rho retro
r4ss::SSmohnsrho(retro_base)
r4ss::SSmohnsrho(retro_mdl1)
r4ss::SSmohnsrho(retro_mdl2)



all_summ <- r4ss::SSsummarize(list(base_res, mdl1_res, mdl2_best_res))

names(all_summ)

all_summ$SpawnBio

all_summ$likelihoods %>% 
  tidytable::filter(Label == 'TOTAL') %>%
  tidytable::select(-Label) %>% 
  tidytable::pivot_longer(names_to = 'model', values_to = 'tot_like') %>% 
  tidytable::mutate(model = c(base_mdl, new_base, new_mdl2)) %>% 
  tidytable::bind_cols(dplyr::as_tibble(all_summ$npars)) %>% 
  tidytable::rename(npars = 'value') %>% 
  tidytable::mutate(aic = 2 * npars + 2* tot_like)


r4ss::SSplotComparisons(all_summ)

ss3diags::SSplotModelcomp(all_summ)





all_res <- r4ss::SSgetoutput(dirvec = c(here::here(asmnt_yr, 'rsch', base_mdl), here::here(asmnt_yr, 'rsch', new_base)))


base_res$likelihoods_by_fleet
mdl1_res$likelihoods_by_fleet


base_res$likelihoods_used
mdl1_res$likelihoods_used




dat$MeanSize_at_Age_obs

list.files(here::here(asmnt_yr, 'rsch', new_base), pattern = 'dat')

