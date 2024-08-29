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
dat_day <- "Aug28"

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
# 2019.1b-2023: 2023 base model ----
# read results from base model
base_mdl <- "2019.1b-2023" # 2023 accepted model

base_res_23 <- r4ss::SS_output(dir = here::here(asmnt_yr - 1, 'mgmt', base_mdl),
                               verbose = TRUE,
                               printstats = TRUE)

# retro_base <- r4ss::SSsummarize(r4ss::SSgetoutput(dirvec = file.path(
#   here::here(asmnt_yr, 'rsch', base_mdl), "retrospectives",
#   paste("retro", 0:-ret_yr, sep = ""))))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2019.1b: updated base model ----
# includes updated GAP tables and length-weight relationship
base_mdl_update <- "2019.1b-2024"

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

## run management scens ----
# update_base_mscen <- Do_AK_TIER_3_Scenarios(DIR = here::here(asmnt_yr, 'rsch', base_mdl_update), 
#                                             CYR = asmnt_yr,  
#                                             FLEETS = c(1:3),
#                                             do_fig = FALSE,
#                                             do_mark = FALSE)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2019.1c: new base model ----
# includes:
# - corrected ll survey sd
# - corrected ll survey length comps
# - fishery iss set at number of hauls that data is used from
# - surveyISS used for survey data
# - plus length bin set at 100 cm
new_base <- "2019.1c-2024"

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

## run management scens ----
# new_base_mscen <- Do_AK_TIER_3_Scenarios(DIR = here::here(asmnt_yr, 'rsch', new_base), 
#                                          CYR = asmnt_yr,  
#                                          FLEETS = c(1:3),
#                                          do_fig = FALSE,
#                                          do_mark = FALSE)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2019.1d: update ageing error ----
new_base_ae <- "2019.1d-2024"

## copy ss input files ----
if(!file.exists(here::here(asmnt_yr, 'rsch', new_base_ae, 'ss3.exe'))){
  start_ss_fldr(from = here::here(asmnt_yr, 'rsch', new_base),
                to = here::here(asmnt_yr, 'rsch', new_base_ae))
}

## update files ----
update_ss3_files(asmnt_yr, 
                 folder = 'rsch',
                 mdl = new_base_ae, 
                 dat_filename = paste0("GOAPcod2024", dat_day, ".dat"),
                 ctl_in = "updated_ae.ctl",
                 ctl_out = "Model19_1d.ctl")

## make 2024 changes to ctl file ----
ctl_2024(asmnt_yr, 
         folder = 'rsch',
         mdl = new_base_ae, 
         ctl_filename = 'Model19_1d.ctl')

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
# new_base_ae_mscen <- Do_AK_TIER_3_Scenarios(DIR = here::here(asmnt_yr, 'rsch', new_base_ae), 
#                                             CYR = asmnt_yr,  
#                                             FLEETS = c(1:3),
#                                             do_fig = FALSE,
#                                             do_mark = FALSE)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2019.1e: new fishery length comps ----
# remove filters and aggregate at trimester-area-gear
new_base_lcomp <- "2019.1e-2024"

## copy ss input files ----
if(!file.exists(here::here(asmnt_yr, 'rsch', new_base_lcomp, 'ss3.exe'))){
  start_ss_fldr(from = here::here(asmnt_yr, 'rsch', new_base_ae),
                to = here::here(asmnt_yr, 'rsch', new_base_lcomp))
}

## update files ----
update_ss3_files(asmnt_yr, 
                 folder = 'rsch',
                 mdl = new_base_lcomp, 
                 dat_filename = paste0("GOAPcod2024", dat_day, "_lcomp.dat"),
                 ctl_in = "updated_ae.ctl",
                 ctl_out = "Model19_1e.ctl")

## make 2024 changes to ctl file ----
ctl_2024(asmnt_yr, 
         folder = 'rsch',
         mdl = new_base_lcomp, 
         ctl_filename = 'Model19_1e.ctl')

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
# new_base_lcomp_mscen <- Do_AK_TIER_3_Scenarios(DIR = here::here(asmnt_yr, 'rsch', new_base_lcomp), 
#                                                CYR = asmnt_yr,  
#                                                FLEETS = c(1:3),
#                                                do_fig = FALSE,
#                                                do_mark = FALSE)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2019.1e.2: new fishery length comps at 2 cm bin ----
new_base_lcomp_bin2 <- "2019.1e.2-2024"

## copy ss input files ----
if(!file.exists(here::here(asmnt_yr, 'rsch', new_base_lcomp_bin2, 'ss3.exe'))){
  start_ss_fldr(from = here::here(asmnt_yr, 'rsch', new_base_lcomp),
                to = here::here(asmnt_yr, 'rsch', new_base_lcomp_bin2))
}

## update files ----
update_ss3_files(asmnt_yr, 
                 folder = 'rsch',
                 mdl = new_base_lcomp_bin2, 
                 dat_filename = paste0("GOAPcod2024", dat_day, "_lcomp_bin2.dat"),
                 ctl_in = "updated_ae.ctl",
                 ctl_out = "Model19_1e.ctl")

## make 2024 changes to ctl file ----
ctl_2024(asmnt_yr, 
         folder = 'rsch',
         mdl = new_base_lcomp_bin2, 
         ctl_filename = 'Model19_1e.ctl')

## run model ----
run_ss3_model(asmnt_yr, 
              folder = 'rsch',
              mdl = new_base_lcomp_bin2,
              ctl_filename = "Model19_1e.ctl")

## get and plot model output ----
# get output
new_base_lcomp_bin2_res <- r4ss::SS_output(dir = here::here(asmnt_yr, 'rsch', new_base_lcomp_bin2))
# if exists, delete plot folder
if(file.exists(here::here(asmnt_yr, 'rsch', new_base_lcomp_bin2, 'plots'))){
  unlink(here::here(asmnt_yr, 'rsch', new_base_lcomp_bin2, 'plots'), recursive = TRUE)
}
# plot results
r4ss::SS_plots(new_base_lcomp_bin2_res,
               printfolder = "",
               dir = here::here(asmnt_yr, 'rsch', new_base_lcomp_bin2, "plots"))

## run management scens ----
# new_base_lcomp_bin2_mscen <- Do_AK_TIER_3_Scenarios(DIR = here::here(asmnt_yr, 'rsch', new_base_lcomp_bin2), 
#                                                     CYR = asmnt_yr,  
#                                                     FLEETS = c(1:3),
#                                                     do_fig = FALSE,
#                                                     do_mark = FALSE)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2019.1e.5: new fishery length comps at 5 cm bin ----
new_base_lcomp_bin5 <- "2019.1e.5-2024"

## copy ss input files ----
if(!file.exists(here::here(asmnt_yr, 'rsch', new_base_lcomp_bin5, 'ss3.exe'))){
  start_ss_fldr(from = here::here(asmnt_yr, 'rsch', new_base_lcomp),
                to = here::here(asmnt_yr, 'rsch', new_base_lcomp_bin5))
}

## update files ----
update_ss3_files(asmnt_yr, 
                 folder = 'rsch',
                 mdl = new_base_lcomp_bin5, 
                 dat_filename = paste0("GOAPcod2024", dat_day, "_lcomp_bin5.dat"),
                 ctl_in = "updated_ae.ctl",
                 ctl_out = "Model19_1e.ctl")

## make 2024 changes to ctl file ----
ctl_2024(asmnt_yr, 
         folder = 'rsch',
         mdl = new_base_lcomp_bin5, 
         ctl_filename = 'Model19_1e.ctl')

## run model ----
run_ss3_model(asmnt_yr, 
              folder = 'rsch',
              mdl = new_base_lcomp_bin5,
              ctl_filename = "Model19_1e.ctl")

## get and plot model output ----
# get output
new_base_lcomp_bin5_res <- r4ss::SS_output(dir = here::here(asmnt_yr, 'rsch', new_base_lcomp_bin5))
# if exists, delete plot folder
if(file.exists(here::here(asmnt_yr, 'rsch', new_base_lcomp_bin5, 'plots'))){
  unlink(here::here(asmnt_yr, 'rsch', new_base_lcomp_bin5, 'plots'), recursive = TRUE)
}
# plot results
r4ss::SS_plots(new_base_lcomp_bin5_res,
               printfolder = "",
               dir = here::here(asmnt_yr, 'rsch', new_base_lcomp_bin5, "plots"))

## run management scens ----
# new_base_lcomp_bin5_mscen <- Do_AK_TIER_3_Scenarios(DIR = here::here(asmnt_yr, 'rsch', new_base_lcomp_bin5), 
#                                                     CYR = asmnt_yr,  
#                                                     FLEETS = c(1:3),
#                                                     do_fig = FALSE,
#                                                     do_mark = FALSE)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# plot data comparisons ----
if (!file.exists(here::here(asmnt_yr, 'rsch', 'output', 'compare', 'data_plots'))){
  dir.create(here::here(asmnt_yr, 'rsch', 'output', 'compare', 'data_plots'), recursive = TRUE)
}

data_summ <- r4ss::SSsummarize(list(base_res_23,
                                    update_base_res, 
                                    new_base_res, 
                                    new_base_ae_res, 
                                    new_base_lcomp_res, 
                                    new_base_lcomp_bin2_res, 
                                    new_base_lcomp_bin5_res))

r4ss::SSplotComparisons(data_summ,
                        print = TRUE,
                        legendlabels = c(base_mdl, 
                                         base_mdl_update, 
                                         new_base, 
                                         new_base_ae,  
                                         new_base_lcomp, 
                                         new_base_lcomp_bin2, 
                                         new_base_lcomp_bin5),
                        plotdir = here::here(asmnt_yr, 'rsch', 'output', 'compare', 'data_plots'))


vroom::vroom_write(data_summ$likelihoods %>% 
                     tidytable::rename('2019.1b-23' = model1,
                                       '2019.1b-24' = model2,
                                       '2019.1c' = model3,
                                       '2019.1d' = model4,
                                       '2019.1e' = model5,
                                       '2019.1e.2' = model6,
                                       '2019.1e.5' = model7), 
                   here::here(asmnt_yr, 'rsch', 'output', 'compare', 'data_summ_likes.csv'), delim = ",")
vroom::vroom_write(data_summ$likelihoods_by_fleet %>% 
                     tidytable::mutate(model = case_when(model == 1 ~ '2019.1b-23',
                                                         model == 2 ~ '2019.1b-24',
                                                         model == 3 ~ '2019.1c',
                                                         model == 4 ~ '2019.1d',
                                                         model == 5 ~ '2019.1e',
                                                         model == 6 ~ '2019.1e.2',
                                                         model == 7 ~ '2019.1e.5')), 
                   here::here(asmnt_yr, 'rsch', 'output', 'compare', 'data_summ_likes_by_fleet.csv'), delim = ",")

abc_comp <- data.frame(model = c(base_mdl,
                                 base_mdl_update, 
                                 new_base, 
                                 new_base_ae, 
                                 new_base_lcomp, 
                                 new_base_lcomp_bin2, 
                                 new_base_lcomp_bin5),
                       abc = c(as.numeric(base_res_23$derived_quants %>% 
                                            filter(Label == 'ForeCatch_2024') %>% 
                                            select(Value)),
                               as.numeric(update_base_res$derived_quants %>% 
                                            filter(Label == 'ForeCatch_2025') %>% 
                                            select(Value)),
                               as.numeric(new_base_res$derived_quants %>% 
                                            filter(Label == 'ForeCatch_2025') %>% 
                                            select(Value)),
                               as.numeric(new_base_ae_res$derived_quants %>% 
                                            filter(Label == 'ForeCatch_2025') %>% 
                                            select(Value)),
                               as.numeric(new_base_lcomp_res$derived_quants %>% 
                                            filter(Label == 'ForeCatch_2025') %>% 
                                            select(Value)),
                               as.numeric(new_base_lcomp_bin2_res$derived_quants %>% 
                                            filter(Label == 'ForeCatch_2025') %>% 
                                            select(Value)),
                               as.numeric(new_base_lcomp_bin5_res$derived_quants %>% 
                                            filter(Label == 'ForeCatch_2025') %>% 
                                            select(Value))))
vroom::vroom_write(abc_comp, here::here(asmnt_yr, 'rsch', 'output', 'compare', 'data_abc_comp.csv'), delim = ",")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# logistic ll survey selex (2024.0) ----
new_base_llsel <- "2024.0-2024"
copy_mdl <- new_base_lcomp_bin5
copy_mdl_res <- new_base_lcomp_bin5_res

## copy ss input files ----
if(!file.exists(here::here(asmnt_yr, 'rsch', new_base_llsel, 'ss3.exe'))){
  start_ss_fldr(from = here::here(asmnt_yr, 'rsch', copy_mdl),
                to = here::here(asmnt_yr, 'rsch', new_base_llsel))
}

## update files ----
update_ss3_files(asmnt_yr, 
                 folder = 'rsch',
                 mdl = new_base_llsel, 
                 dat_filename = paste0("GOAPcod2024", dat_day, "_lcomp_bin5.dat"),
                 ctl_in = "updated_ae.ctl",
                 ctl_out = "Model24_0.ctl")

## make 2024 changes to ctl file ----
ctl_2024(asmnt_yr, 
         folder = 'rsch',
         mdl = new_base_llsel, 
         ctl_filename = 'Model24_0.ctl')

## change selex in ctl ----
# read ctl file
ctl <- r4ss::SS_readctl_3.30(here::here(asmnt_yr, 'rsch', new_base_llsel, 'Model24_0.ctl'))
# freeze ll srv to logistic
ctl$size_selex_parms$INIT[which(rownames(ctl$size_selex_parms) == 'SizeSel_P_6_LLSrv(5)')] <- 10
ctl$size_selex_parms$PHASE[which(rownames(ctl$size_selex_parms) == 'SizeSel_P_6_LLSrv(5)')] <- -2
# write new ctl file
r4ss::SS_writectl_3.30(ctllist = ctl,
                       outfile = here::here(asmnt_yr, 'rsch', new_base_llsel, 'Model24_0.ctl'),
                       overwrite = TRUE)

## run model ----
run_ss3_model(asmnt_yr, 
              folder = 'rsch',
              mdl = new_base_llsel,
              ctl_filename = "Model24_0.ctl")

## get and plot model output ----
# get output
new_base_llsel_res <- r4ss::SS_output(dir = here::here(asmnt_yr, 'rsch', new_base_llsel))
# if exists, delete plot folder
if(file.exists(here::here(asmnt_yr, 'rsch', new_base_llsel, 'plots'))){
  unlink(here::here(asmnt_yr, 'rsch', new_base_llsel, 'plots'), recursive = TRUE)
}
# plot results
r4ss::SS_plots(new_base_llsel_res,
               printfolder = "",
               dir = here::here(asmnt_yr, 'rsch', new_base_llsel, "plots"))




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# time-invariant trawl survey selex (2024.1) ----
new_base_twlsel <- "2024.1-2024"
copy_mdl <- new_base_lcomp_bin5
copy_mdl_res <- new_base_lcomp_bin5_res

## copy ss input files ----
if(!file.exists(here::here(asmnt_yr, 'rsch', new_base_twlsel, 'ss3.exe'))){
  start_ss_fldr(from = here::here(asmnt_yr, 'rsch', copy_mdl),
                to = here::here(asmnt_yr, 'rsch', new_base_twlsel))
}

## update files ----
update_ss3_files(asmnt_yr, 
                 folder = 'rsch',
                 mdl = new_base_twlsel, 
                 dat_filename = paste0("GOAPcod2024", dat_day, "_lcomp_bin5.dat"),
                 ctl_in = "updated_ae.ctl",
                 ctl_out = "Model24_1.ctl")

## make 2024 changes to ctl file ----
ctl_2024(asmnt_yr, 
         folder = 'rsch',
         mdl = new_base_twlsel, 
         ctl_filename = 'Model24_1.ctl')

## change selex in ctl ----
# read ctl file
ctl <- r4ss::SS_readctl_3.30(here::here(asmnt_yr, 'rsch', new_base_twlsel, 'Model24_1.ctl'))
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
# write new ctl file
r4ss::SS_writectl_3.30(ctllist = ctl,
                       outfile = here::here(asmnt_yr, 'rsch', new_base_twlsel, 'Model24_1.ctl'),
                       overwrite = TRUE)

## run model ----
run_ss3_model(asmnt_yr, 
              folder = 'rsch',
              mdl = new_base_twlsel,
              ctl_filename = "Model24_1.ctl")

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

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# logistic & time-invariant survey selex (2024.2) ----
new_base_selex <- "2024.2-2024"
copy_mdl <- new_base_lcomp_bin5
copy_mdl_res <- new_base_lcomp_bin5_res

## copy ss input files ----
if(!file.exists(here::here(asmnt_yr, 'rsch', new_base_selex, 'ss3.exe'))){
  start_ss_fldr(from = here::here(asmnt_yr, 'rsch', copy_mdl),
                to = here::here(asmnt_yr, 'rsch', new_base_selex))
}

## update files ----
update_ss3_files(asmnt_yr, 
                 folder = 'rsch',
                 mdl = new_base_selex, 
                 dat_filename = paste0("GOAPcod2024", dat_day, "_lcomp_bin5.dat"),
                 ctl_in = "updated_ae.ctl",
                 ctl_out = "Model24_2.ctl")

## make 2024 changes to ctl file ----
ctl_2024(asmnt_yr, 
         folder = 'rsch',
         mdl = new_base_selex, 
         ctl_filename = 'Model24_2.ctl')

## change selex in ctl ----
# read ctl file
ctl <- r4ss::SS_readctl_3.30(here::here(asmnt_yr, 'rsch', new_base_selex, 'Model24_2.ctl'))

# freeze ll srv to logistic
ctl$size_selex_parms$INIT[which(rownames(ctl$size_selex_parms) == 'SizeSel_P_6_LLSrv(5)')] <- 10
ctl$size_selex_parms$PHASE[which(rownames(ctl$size_selex_parms) == 'SizeSel_P_6_LLSrv(5)')] <- -2
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
# write new ctl file
r4ss::SS_writectl_3.30(ctllist = ctl,
                       outfile = here::here(asmnt_yr, 'rsch', new_base_selex, 'Model24_2.ctl'),
                       overwrite = TRUE)

## run model ----
run_ss3_model(asmnt_yr, 
              folder = 'rsch',
              mdl = new_base_selex,
              ctl_filename = "Model24_2.ctl")

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


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# logistic & time-invariant survey selex, full t-v for fishery (2024.3) ----
new_selex <- "2024.3-2024"
copy_mdl <- new_base_lcomp_bin5
copy_mdl_res <- new_base_lcomp_bin5_res

## copy ss input files ----
if(!file.exists(here::here(asmnt_yr, 'rsch', new_selex, 'ss3.exe'))){
  start_ss_fldr(from = here::here(asmnt_yr, 'rsch', copy_mdl),
                to = here::here(asmnt_yr, 'rsch', new_selex))
}

## update files ----
update_ss3_files(asmnt_yr, 
                 folder = 'rsch',
                 mdl = new_selex, 
                 dat_filename = paste0("GOAPcod2024", dat_day, "_lcomp_bin5.dat"),
                 ctl_in = "updated_ae.ctl",
                 ctl_out = "Model24_3.ctl")

## make 2024 changes to ctl file ----
ctl_2024(asmnt_yr, 
         folder = 'rsch',
         mdl = new_selex, 
         ctl_filename = 'Model24_3.ctl')

## change selex in ctl ----
# read ctl file
ctl <- r4ss::SS_readctl_3.30(here::here(asmnt_yr, 'rsch', new_selex, 'Model24_3.ctl'))

# freeze ll srv to logistic
ctl$size_selex_parms$INIT[which(rownames(ctl$size_selex_parms) == 'SizeSel_P_6_LLSrv(5)')] <- 10
ctl$size_selex_parms$PHASE[which(rownames(ctl$size_selex_parms) == 'SizeSel_P_6_LLSrv(5)')] <- -2
# set blocks to 0
ctl$size_selex_parms$Block = 0
ctl$size_selex_parms$Block_Fxn = 0
# remove from tv section
ctl$size_selex_parms_tv <- ctl$size_selex_parms_tv[-seq(which(rownames(ctl$size_selex_parms_tv) == 'SizeSel_P_1_Srv(4)_BLK1repl_1996'),
                                                        length(rownames(ctl$size_selex_parms_tv))),]
# freeze twl srv to logistic
ctl$size_selex_parms$INIT[which(rownames(ctl$size_selex_parms) == 'SizeSel_P_6_Srv(4)')] <- 10
ctl$size_selex_parms$PHASE[which(rownames(ctl$size_selex_parms) == 'SizeSel_P_6_Srv(4)')] <- -2
# twl fishery
ctl$size_selex_parms$dev_maxyr[which(rownames(ctl$size_selex_parms) == 'SizeSel_P_1_FshTrawl(1)')] = asmnt_yr
ctl$size_selex_parms$dev_maxyr[which(rownames(ctl$size_selex_parms) == 'SizeSel_P_2_FshTrawl(1)')] = asmnt_yr
ctl$size_selex_parms$dev_maxyr[which(rownames(ctl$size_selex_parms) == 'SizeSel_P_3_FshTrawl(1)')] = asmnt_yr
ctl$size_selex_parms$dev_maxyr[which(rownames(ctl$size_selex_parms) == 'SizeSel_P_4_FshTrawl(1)')] = asmnt_yr
ctl$size_selex_parms_tv = ctl$size_selex_parms_tv[-which(rownames(ctl$size_selex_parms_tv) == 'SizeSel_P_1_FshTrawl(1)_BLK2repl_1990'),]
ctl$size_selex_parms_tv = ctl$size_selex_parms_tv[-which(rownames(ctl$size_selex_parms_tv) == 'SizeSel_P_1_FshTrawl(1)_BLK2repl_2005'),]
ctl$size_selex_parms_tv = ctl$size_selex_parms_tv[-which(rownames(ctl$size_selex_parms_tv) == 'SizeSel_P_1_FshTrawl(1)_BLK2repl_2007'),]
ctl$size_selex_parms_tv = ctl$size_selex_parms_tv[-which(rownames(ctl$size_selex_parms_tv) == 'SizeSel_P_1_FshTrawl(1)_BLK2repl_2017'),]
ctl$size_selex_parms_tv = ctl$size_selex_parms_tv[-which(rownames(ctl$size_selex_parms_tv) == 'SizeSel_P_2_FshTrawl(1)_BLK2repl_1990'),]
ctl$size_selex_parms_tv = ctl$size_selex_parms_tv[-which(rownames(ctl$size_selex_parms_tv) == 'SizeSel_P_2_FshTrawl(1)_BLK2repl_2005'),]
ctl$size_selex_parms_tv = ctl$size_selex_parms_tv[-which(rownames(ctl$size_selex_parms_tv) == 'SizeSel_P_2_FshTrawl(1)_BLK2repl_2007'),]
ctl$size_selex_parms_tv = ctl$size_selex_parms_tv[-which(rownames(ctl$size_selex_parms_tv) == 'SizeSel_P_2_FshTrawl(1)_BLK2repl_2017'),]
ctl$size_selex_parms_tv = ctl$size_selex_parms_tv[-which(rownames(ctl$size_selex_parms_tv) == 'SizeSel_P_3_FshTrawl(1)_BLK2repl_1990'),]
ctl$size_selex_parms_tv = ctl$size_selex_parms_tv[-which(rownames(ctl$size_selex_parms_tv) == 'SizeSel_P_3_FshTrawl(1)_BLK2repl_2005'),]
ctl$size_selex_parms_tv = ctl$size_selex_parms_tv[-which(rownames(ctl$size_selex_parms_tv) == 'SizeSel_P_3_FshTrawl(1)_BLK2repl_2007'),]
ctl$size_selex_parms_tv = ctl$size_selex_parms_tv[-which(rownames(ctl$size_selex_parms_tv) == 'SizeSel_P_3_FshTrawl(1)_BLK2repl_2017'),]
ctl$size_selex_parms_tv = ctl$size_selex_parms_tv[-which(rownames(ctl$size_selex_parms_tv) == 'SizeSel_P_4_FshTrawl(1)_BLK2repl_1990'),]
ctl$size_selex_parms_tv = ctl$size_selex_parms_tv[-which(rownames(ctl$size_selex_parms_tv) == 'SizeSel_P_4_FshTrawl(1)_BLK2repl_2005'),]
ctl$size_selex_parms_tv = ctl$size_selex_parms_tv[-which(rownames(ctl$size_selex_parms_tv) == 'SizeSel_P_4_FshTrawl(1)_BLK2repl_2007'),]
ctl$size_selex_parms_tv = ctl$size_selex_parms_tv[-which(rownames(ctl$size_selex_parms_tv) == 'SizeSel_P_4_FshTrawl(1)_BLK2repl_2017'),]
# ll fishery
ctl$size_selex_parms$dev_maxyr[which(rownames(ctl$size_selex_parms) == 'SizeSel_P_1_FshLL(2)')] = asmnt_yr
ctl$size_selex_parms$dev_maxyr[which(rownames(ctl$size_selex_parms) == 'SizeSel_P_2_FshLL(2)')] = asmnt_yr
ctl$size_selex_parms$dev_maxyr[which(rownames(ctl$size_selex_parms) == 'SizeSel_P_3_FshLL(2)')] = asmnt_yr
ctl$size_selex_parms_tv = ctl$size_selex_parms_tv[-which(rownames(ctl$size_selex_parms_tv) == 'SizeSel_P_1_FshLL(2)_BLK2repl_1990'),]
ctl$size_selex_parms_tv = ctl$size_selex_parms_tv[-which(rownames(ctl$size_selex_parms_tv) == 'SizeSel_P_1_FshLL(2)_BLK2repl_2005'),]
ctl$size_selex_parms_tv = ctl$size_selex_parms_tv[-which(rownames(ctl$size_selex_parms_tv) == 'SizeSel_P_1_FshLL(2)_BLK2repl_2007'),]
ctl$size_selex_parms_tv = ctl$size_selex_parms_tv[-which(rownames(ctl$size_selex_parms_tv) == 'SizeSel_P_1_FshLL(2)_BLK2repl_2017'),]
ctl$size_selex_parms_tv = ctl$size_selex_parms_tv[-which(rownames(ctl$size_selex_parms_tv) == 'SizeSel_P_2_FshLL(2)_BLK2repl_1990'),]
ctl$size_selex_parms_tv = ctl$size_selex_parms_tv[-which(rownames(ctl$size_selex_parms_tv) == 'SizeSel_P_2_FshLL(2)_BLK2repl_2005'),]
ctl$size_selex_parms_tv = ctl$size_selex_parms_tv[-which(rownames(ctl$size_selex_parms_tv) == 'SizeSel_P_2_FshLL(2)_BLK2repl_2007'),]
ctl$size_selex_parms_tv = ctl$size_selex_parms_tv[-which(rownames(ctl$size_selex_parms_tv) == 'SizeSel_P_2_FshLL(2)_BLK2repl_2017'),]
ctl$size_selex_parms_tv = ctl$size_selex_parms_tv[-which(rownames(ctl$size_selex_parms_tv) == 'SizeSel_P_3_FshLL(2)_BLK2repl_1990'),]
ctl$size_selex_parms_tv = ctl$size_selex_parms_tv[-which(rownames(ctl$size_selex_parms_tv) == 'SizeSel_P_3_FshLL(2)_BLK2repl_2005'),]
ctl$size_selex_parms_tv = ctl$size_selex_parms_tv[-which(rownames(ctl$size_selex_parms_tv) == 'SizeSel_P_3_FshLL(2)_BLK2repl_2007'),]
ctl$size_selex_parms_tv = ctl$size_selex_parms_tv[-which(rownames(ctl$size_selex_parms_tv) == 'SizeSel_P_3_FshLL(2)_BLK2repl_2017'),]
ctl$size_selex_parms_tv = ctl$size_selex_parms_tv[-which(rownames(ctl$size_selex_parms_tv) == 'SizeSel_P_4_FshLL(2)_BLK2repl_1990'),]
ctl$size_selex_parms_tv = ctl$size_selex_parms_tv[-which(rownames(ctl$size_selex_parms_tv) == 'SizeSel_P_4_FshLL(2)_BLK2repl_2005'),]
ctl$size_selex_parms_tv = ctl$size_selex_parms_tv[-which(rownames(ctl$size_selex_parms_tv) == 'SizeSel_P_4_FshLL(2)_BLK2repl_2007'),]
ctl$size_selex_parms_tv = ctl$size_selex_parms_tv[-which(rownames(ctl$size_selex_parms_tv) == 'SizeSel_P_4_FshLL(2)_BLK2repl_2017'),]
# pot fishery
ctl$size_selex_parms$dev_link[which(rownames(ctl$size_selex_parms) == 'SizeSel_P_1_FshPot(3)')] = 1
ctl$size_selex_parms$dev_link[which(rownames(ctl$size_selex_parms) == 'SizeSel_P_3_FshPot(3)')] = 1
ctl$size_selex_parms$dev_link[which(rownames(ctl$size_selex_parms) == 'SizeSel_P_6_FshPot(3)')] = 1
ctl$size_selex_parms$dev_minyr[which(rownames(ctl$size_selex_parms) == 'SizeSel_P_1_FshPot(3)')] = 1991
ctl$size_selex_parms$dev_minyr[which(rownames(ctl$size_selex_parms) == 'SizeSel_P_3_FshPot(3)')] = 1991
ctl$size_selex_parms$dev_minyr[which(rownames(ctl$size_selex_parms) == 'SizeSel_P_6_FshPot(3)')] = 1991
ctl$size_selex_parms$dev_maxyr[which(rownames(ctl$size_selex_parms) == 'SizeSel_P_1_FshPot(3)')] = asmnt_yr
ctl$size_selex_parms$dev_maxyr[which(rownames(ctl$size_selex_parms) == 'SizeSel_P_3_FshPot(3)')] = asmnt_yr
ctl$size_selex_parms$dev_maxyr[which(rownames(ctl$size_selex_parms) == 'SizeSel_P_6_FshPot(3)')] = asmnt_yr
ctl$size_selex_parms$dev_PH[which(rownames(ctl$size_selex_parms) == 'SizeSel_P_1_FshPot(3)')] = 3
ctl$size_selex_parms$dev_PH[which(rownames(ctl$size_selex_parms) == 'SizeSel_P_3_FshPot(3)')] = 3
ctl$size_selex_parms$dev_PH[which(rownames(ctl$size_selex_parms) == 'SizeSel_P_6_FshPot(3)')] = 3
ctl$size_selex_parms_tv = ctl$size_selex_parms_tv[-which(rownames(ctl$size_selex_parms_tv) == 'SizeSel_P_1_FshPot(3)_BLK3repl_2017'),]
ctl$size_selex_parms_tv = ctl$size_selex_parms_tv[-which(rownames(ctl$size_selex_parms_tv) == 'SizeSel_P_2_FshPot(3)_BLK3repl_2017'),]
ctl$size_selex_parms_tv = ctl$size_selex_parms_tv[-which(rownames(ctl$size_selex_parms_tv) == 'SizeSel_P_3_FshPot(3)_BLK3repl_2017'),]
t1 = ctl$size_selex_parms_tv[which(rownames(ctl$size_selex_parms_tv) == 'SizeSel_P_1_FshTrawl(1)_dev_se'),]
rownames(t1) = 'SizeSel_P_1_FshPot(3)_dev_se'
t2 = ctl$size_selex_parms_tv[which(rownames(ctl$size_selex_parms_tv) == 'SizeSel_P_1_FshTrawl(1)_dev_autocorr'),]
rownames(t2) = 'SizeSel_P_1_FshPot(3)_dev_autocorr'
t3 = ctl$size_selex_parms_tv[which(rownames(ctl$size_selex_parms_tv) == 'SizeSel_P_1_FshTrawl(1)_dev_se'),]
rownames(t3) = 'SizeSel_P_3_FshPot(3)_dev_se'
t4 = ctl$size_selex_parms_tv[which(rownames(ctl$size_selex_parms_tv) == 'SizeSel_P_1_FshTrawl(1)_dev_autocorr'),]
rownames(t4) = 'SizeSel_P_3_FshPot(3)_dev_autocorr'
t5 = ctl$size_selex_parms_tv[which(rownames(ctl$size_selex_parms_tv) == 'SizeSel_P_1_FshTrawl(1)_dev_se'),]
rownames(t5) = 'SizeSel_P_6_FshPot(3)_dev_se'
t6 = ctl$size_selex_parms_tv[which(rownames(ctl$size_selex_parms_tv) == 'SizeSel_P_1_FshTrawl(1)_dev_autocorr'),]
rownames(t6) = 'SizeSel_P_6_FshPot(3)_dev_autocorr'
ctl$size_selex_parms_tv = ctl$size_selex_parms_tv %>% 
  dplyr::bind_rows(t1) %>% 
  dplyr::bind_rows(t2) %>% 
  dplyr::bind_rows(t3) %>% 
  dplyr::bind_rows(t4) %>% 
  dplyr::bind_rows(t5) %>% 
  dplyr::bind_rows(t6)
# write new ctl file
r4ss::SS_writectl_3.30(ctllist = ctl,
                       outfile = here::here(asmnt_yr, 'rsch', new_selex, 'Model24_3.ctl'),
                       overwrite = TRUE)

## run model ----
run_ss3_model(asmnt_yr, 
              folder = 'rsch',
              mdl = new_selex,
              ctl_filename = "Model24_3.ctl")

## get and plot model output ----
# get output
new_selex_res <- r4ss::SS_output(dir = here::here(asmnt_yr, 'rsch', new_selex))
# if exists, delete plot folder
if(file.exists(here::here(asmnt_yr, 'rsch', new_selex, 'plots'))){
  unlink(here::here(asmnt_yr, 'rsch', new_selex, 'plots'), recursive = TRUE)
}
# plot results
r4ss::SS_plots(new_selex_res,
               printfolder = "",
               dir = here::here(asmnt_yr, 'rsch', new_selex, "plots"))




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# plot model comparisons ----
if (!file.exists(here::here(asmnt_yr, 'rsch', 'output', 'compare', 'model_plots'))){
  dir.create(here::here(asmnt_yr, 'rsch', 'output', 'compare', 'model_plots'), recursive = TRUE)
}

mdl_summ <- r4ss::SSsummarize(list(base_res_23, update_base_res, new_base_ae_res, new_base_bin5_res, new_base_twlsel_res, new_base_selex_res, new_base_fcaal_res))

r4ss::SSplotComparisons(mdl_summ,
                        print = TRUE,
                        legendlabels = c(base_mdl, base_mdl_update, new_base_ae, new_base_bin5, new_base_twlsel, new_base_selex, new_base_fcaal),
                        plotdir = here::here(asmnt_yr, 'rsch', 'output', 'compare', 'model_plots'))


vroom::vroom_write(mdl_summ$likelihoods %>% 
                     tidytable::rename('2019.1b-23' = model1,
                                       '2019.1b-24' = model2,
                                       '2019.1d' = model3,
                                       '2019.1f.5' = model4,
                                       '2024.0' = model5,
                                       '2024.1' = model6,
                                       '2024.2' = model7), 
                   here::here(asmnt_yr, 'rsch', 'output', 'compare', 'model_summ_likes.csv'), delim = ",")
vroom::vroom_write(mdl_summ$likelihoods_by_fleet %>% 
                     tidytable::mutate(model = case_when(model == 1 ~ '2019.1b-23',
                                                         model == 2 ~ '2019.1b-24',
                                                         model == 3 ~ '2019.1d',
                                                         model == 4 ~ '2019.1f.5',
                                                         model == 5 ~ '2024.0',
                                                         model == 6 ~ '2024.1',
                                                         model == 7 ~ '2024.2')), 
                   here::here(asmnt_yr, 'rsch', 'output', 'compare', 'model_summ_likes_by_fleet.csv'), delim = ",")


abc_comp <- data.frame(model = c(base_mdl_update, new_base_ae, new_base_bin5, new_base_twlsel, new_base_selex, new_base_fcaal)) %>% 
  tidytable::bind_cols(data.frame(abc = update_base_mscen$Two_year$C_ABC[1]) %>% 
                         tidytable::bind_rows(data.frame(abc = new_base_ae_mscen$Two_year$C_ABC[1])) %>% 
                         tidytable::bind_rows(data.frame(abc = new_base_bin5_mscen$Two_year$C_ABC[1])) %>% 
                         tidytable::bind_rows(data.frame(abc = new_base_twlsel_mscen$Two_year$C_ABC[1])) %>% 
                         tidytable::bind_rows(data.frame(abc = new_base_selex_mscen$Two_year$C_ABC[1])) %>% 
                         tidytable::bind_rows(data.frame(abc = new_base_fcaal_mscen$Two_year$C_ABC[1])))


vroom::vroom_write(abc_comp, here::here(asmnt_yr, 'rsch', 'output', 'compare', 'model_abc_comp.csv'), delim = ",")









