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

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2019.1d: update ageing error ----
new_base_ae <- "2019.1d-2024"

## copy ss input files ----
if(!file.exists(here::here(asmnt_yr, 'rsch', new_base_ae, 'ss3.exe'))){
  start_ss_fldr(from = here::here(asmnt_yr, 'rsch', base_mdl_update),
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

r4ss::run(dir = here::here(asmnt_yr, 'rsch', new_base_ae),
          skipfinished = FALSE,
          show_in_console = TRUE)

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

new_base_ae_res$parameters[99,]



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2019.1e: new fishery length comps ----
# remove filters and aggregate at trimester-area-gear
new_base_lcomp <- "2019.1e-2024"

## copy ss input files ----
if(!file.exists(here::here(asmnt_yr, 'rsch', new_base_lcomp, 'ss3.exe'))){
  start_ss_fldr(from = here::here(asmnt_yr, 'rsch', new_base),
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

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2019.1e.2: new fishery length comps at 2 cm bin ----
new_base_lcomp_bin2 <- "2019.1e.2cm-2024"

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

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2019.1d.5: new fishery length comps at 5 cm bin ----
new_base_lcomp_bin5 <- "2019.1e.5cm-2024"

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

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# plot data comparisons ----
if (!file.exists(here::here(asmnt_yr, 'rsch', 'output', 'compare', 'data_plots_c'))){
  dir.create(here::here(asmnt_yr, 'rsch', 'output', 'compare', 'data_plots_c'), recursive = TRUE)
}
if (!file.exists(here::here(asmnt_yr, 'rsch', 'output', 'compare', 'data_plots_cseries'))){
  dir.create(here::here(asmnt_yr, 'rsch', 'output', 'compare', 'data_plots_cseries'), recursive = TRUE)
}
if (!file.exists(here::here(asmnt_yr, 'rsch', 'output', 'compare', 'data_plots_cd'))){
  dir.create(here::here(asmnt_yr, 'rsch', 'output', 'compare', 'data_plots_cd'), recursive = TRUE)
}
if (!file.exists(here::here(asmnt_yr, 'rsch', 'output', 'compare', 'data_plots_d'))){
  dir.create(here::here(asmnt_yr, 'rsch', 'output', 'compare', 'data_plots_d'), recursive = TRUE)
}


## base compared to 2019.1c ----
data_summ_c <- r4ss::SSsummarize(list(base_res_23,
                                      update_base_res, 
                                      new_base_res))

r4ss::SSplotComparisons(data_summ_c,
                        print = TRUE,
                        legendlabels = c(base_mdl, 
                                         base_mdl_update, 
                                         new_base),
                        plotdir = here::here(asmnt_yr, 'rsch', 'output', 'compare', 'data_plots_c'))

## 2019.1c model series ----
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


# 2019.1c
r4ss::SSplotComparisons(data_summ_cseries, subplots = 1, 
                        models = c(1,2),
                        legendlabels = c(base_mdl_update, 
                                         '2019.1c'))

# 2019.1c.1-4
data_summ1 <- r4ss::SSsummarize(list(update_base_res, 
                                     new_base.1_4_res))
r4ss::SSplotComparisons(data_summ1, subplots = 1, 
                        models = c(1,2),
                        legendlabels = c(base_mdl_update, 
                                         '2019.1c.1_4'))

vroom::vroom_write(data_summ1$likelihoods_by_fleet, here::here('test.csv'), delim = ',')

# 2019.1c.1
r4ss::SSplotComparisons(data_summ_cseries, subplots = 1, 
                        models = c(1,3),
                        legendlabels = c(base_mdl_update, 
                                         '2019.1c.1-LLSrvSD'))
# 2019.1c.2
r4ss::SSplotComparisons(data_summ_cseries, subplots = 1, 
                        models = c(1,4),
                        legendlabels = c(base_mdl_update, 
                                         '2019.1c.2-LLSrvLComp'))
# 2019.1c.3
r4ss::SSplotComparisons(data_summ_cseries, subplots = 1, 
                        models = c(1,5),
                        legendlabels = c(base_mdl_update, 
                                         '2019.1c.3-FishISS'))
# 2019.1c.4
r4ss::SSplotComparisons(data_summ_cseries, subplots = 1, 
                        models = c(1,6),
                        legendlabels = c(base_mdl_update, 
                                         '2019.1c.4-104cm'))
# 2019.1c.5
r4ss::SSplotComparisons(data_summ_cseries, subplots = 1, 
                        models = c(1,7),
                        legendlabels = c(base_mdl_update, 
                                         '2019.1c.5-CVgrwth'))
# 2019.1c.6
r4ss::SSplotComparisons(data_summ_cseries, subplots = 1, 
                        models = c(1,8),
                        legendlabels = c(base_mdl_update, 
                                         '2019.1c.6-recfore'))
# 2019.1c.7
r4ss::SSplotComparisons(data_summ_cseries, subplots = 1, 
                        models = c(1,9),
                        legendlabels = c(base_mdl_update, 
                                         '2019.1c.7-LatAmin'))
# 2019.1c.8
r4ss::SSplotComparisons(data_summ_cseries, subplots = 1, 
                        models = c(1,10),
                        legendlabels = c(base_mdl_update, 
                                         '2019.1c.8-start_logit'))
# 2019.1c.9
r4ss::SSplotComparisons(data_summ_cseries, subplots = 1, 
                        models = c(1,11),
                        legendlabels = c(base_mdl_update, 
                                         '2019.1c.9-ae'))




## base compared to 2019.1c & 1d ----
data_summ_cd <- r4ss::SSsummarize(list(base_res_23,
                                       update_base_res, 
                                       new_base_res, 
                                       new_base_lcomp_res))

r4ss::SSplotComparisons(data_summ_cd,
                        print = TRUE,
                        legendlabels = c(base_mdl, 
                                         base_mdl_update, 
                                         new_base,  
                                         new_base_lcomp),
                        plotdir = here::here(asmnt_yr, 'rsch', 'output', 'compare', 'data_plots_cd'))

## 2019.1d ----
data_summ_d <- r4ss::SSsummarize(list(new_base_lcomp_res, 
                                      new_base_lcomp_bin2_res, 
                                      new_base_lcomp_bin5_res))

r4ss::SSplotComparisons(data_summ_d,
                        print = TRUE,
                        legendlabels = c(new_base_lcomp, 
                                         new_base_lcomp_bin2, 
                                         new_base_lcomp_bin5),
                        plotdir = here::here(asmnt_yr, 'rsch', 'output', 'compare', 'data_plots_d'))

## likes & abc ----
data_summ_all <- r4ss::SSsummarize(list(base_res_23,
                                        update_base_res, 
                                        new_base_res, 
                                        new_base.1_res,
                                        new_base.2_res,
                                        new_base.3_res,
                                        new_base.4_res,
                                        new_base.5_res,
                                        new_base.6_res,
                                        new_base.7_res,
                                        new_base.8_res,
                                        new_base.9_res,
                                        new_base_lcomp_res, 
                                        new_base_lcomp_bin2_res, 
                                        new_base_lcomp_bin5_res))


vroom::vroom_write(data_summ_all$likelihoods %>% 
                     tidytable::rename('2019.1b-23' = model1,
                                       '2019.1b-24' = model2,
                                       '2019.1c' = model3,
                                       '2019.1c.1' = model4,
                                       '2019.1c.2' = model5,
                                       '2019.1c.3' = model6,
                                       '2019.1c.4' = model7,
                                       '2019.1c.5' = model8,
                                       '2019.1c.6' = model9,
                                       '2019.1c.7' = model10,
                                       '2019.1c.8' = model11,
                                       '2019.1c.9' = model12,
                                       '2019.1d' = model13,
                                       '2019.1d.2' = model14,
                                       '2019.1d.5' = model15), 
                   here::here(asmnt_yr, 'rsch', 'output', 'compare', 'data_summ_likes.csv'), delim = ",")
vroom::vroom_write(data_summ_all$likelihoods_by_fleet %>% 
                     tidytable::mutate(model = case_when(model == 1 ~ '2019.1b-23',
                                                         model == 2 ~ '2019.1b-24',
                                                         model == 3 ~ '2019.1c',
                                                         model == 4 ~ '2019.1c.1',
                                                         model == 5 ~ '2019.1c.2',
                                                         model == 6 ~ '2019.1c.3',
                                                         model == 7 ~ '2019.1c.4',
                                                         model == 8 ~ '2019.1c.5',
                                                         model == 9 ~ '2019.1c.6',
                                                         model == 10 ~ '2019.1c.7',
                                                         model == 11 ~ '2019.1c.8',
                                                         model == 12 ~ '2019.1c.9',
                                                         model == 13 ~ '2019.1d',
                                                         model == 14 ~ '2019.1d.2',
                                                         model == 15 ~ '2019.1d.5')), 
                   here::here(asmnt_yr, 'rsch', 'output', 'compare', 'data_summ_likes_by_fleet.csv'), delim = ",")

abc_comp <- data.frame(model = c(base_mdl,
                                 base_mdl_update, 
                                 new_base, 
                                 new_base.1,
                                 new_base.2,
                                 new_base.3,
                                 new_base.4,
                                 new_base.5,
                                 new_base.6,
                                 new_base.7,
                                 new_base.8,
                                 new_base.9,
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

