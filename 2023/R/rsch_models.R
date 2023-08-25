# Script to run 2023 GOA Pacific Cod Assessment analyses (P. Hulson)

# Load required packages & define parameters ----

libs <- c("data.table",
          "dplyr",
          "ggplot2",
          "magrittr", 
          "r4ss")

if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )]) > 0) {
  install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE)])}

lapply(libs, library, character.only = TRUE)

# model names
base_mdl <- "2019.1a-2022" # 2022 accepted model
new_mdl1 <- "2019.1b-2022" # 2022 model with minsamplesize correction
new_mdl2 <- "2023.1-2022" # 2022 model with env growth link

# assessment year
ass_yr <- as.numeric(format(Sys.Date(), format = "%Y"))
  
# run/develop/read results from models ----

# read results from base model

base_res <- r4ss::SS_output(dir = here::here(ass_yr, 'rsch', base_mdl),
                            verbose = TRUE,
                            printstats = TRUE)

# model 2019.1b analysis

# copy over the stock synthesis model files to the new directory for model 2019.1b
r4ss::copy_SS_inputs(dir.old = here::here(ass_yr, 'rsch', base_mdl), 
                     dir.new = here::here(ass_yr, 'rsch', new_mdl1),
                     overwrite = TRUE)
base::file.copy(from = here::here(ass_yr, 'rsch', base_mdl, "ss.exe"),
                to = here::here(ass_yr, 'rsch', new_mdl1, "ss.exe"),
                overwrite = TRUE)
base::file.copy(from = here::here(ass_yr, 'rsch', base_mdl, "ss.par"),
                to = here::here(ass_yr, 'rsch', new_mdl1, "ss.par"),
                overwrite = TRUE)

# correct minsamplesize in dat file
dat <- r4ss::SS_readdat(here::here(ass_yr, 'rsch', base_mdl, list.files(here::here(ass_yr, 'rsch', base_mdl), pattern = 'dat')))

dat$age_info$minsamplesize <- 0.01

r4ss::SS_writedat(datlist = dat, 
                  outfile = here::here(ass_yr, 'rsch', new_mdl1, list.files(here::here(ass_yr, 'rsch', new_mdl1), pattern = 'dat')),
                  overwrite = TRUE)

# run model
r4ss::run_SS_models(dirvec = here::here(ass_yr, 'rsch', new_mdl1),
                    skipfinished = FALSE,
                    intern = TRUE)

# read the model output and print diagnostic messages
mdl1_res <- r4ss::SS_output(dir = here::here(ass_yr, 'rsch', new_mdl1),
                            verbose = TRUE,
                            printstats = TRUE)


# model 2023.1 analysis (env link for growth)

# copy over the stock synthesis model files to the new directory for model 2019.1b
r4ss::copy_SS_inputs(dir.old = here::here(ass_yr, 'rsch', base_mdl), 
                     dir.new = here::here(ass_yr, 'rsch', new_mdl2),
                     overwrite = TRUE)
base::file.copy(from = here::here(ass_yr, 'rsch', base_mdl, "ss.exe"),
                to = here::here(ass_yr, 'rsch', new_mdl2, "ss.exe"),
                overwrite = TRUE)
base::file.copy(from = here::here(ass_yr, 'rsch', base_mdl, "ss.par"),
                to = here::here(ass_yr, 'rsch', new_mdl2, "ss.par"),
                overwrite = TRUE)



# add env link to ctl file - something doesn't work here, changing it manually

# ctl <- r4ss::SS_readctl(here::here(ass_yr, 'rsch', base_mdl, list.files(here::here(ass_yr, 'rsch', base_mdl), pattern = 'ctl')),
#                         datlist = here::here(ass_yr, 'rsch', base_mdl, list.files(here::here(ass_yr, 'rsch', base_mdl), pattern = 'dat')[2]))
# 
# ctl$MG_parms$`env_var&link`[2:3] = 101
# 
# r4ss::SS_writectl(ctllist = ctl,
#                   outfile = here::here(ass_yr, 'rsch', new_mdl1, list.files(here::here(ass_yr, 'rsch', new_mdl2), pattern = 'ctl')),
#                   overwrite = TRUE)


# run model
r4ss::run_SS_models(dirvec = here::here(ass_yr, 'rsch', new_mdl2),
                    skipfinished = FALSE,
                    intern = TRUE)

# read the model output and print diagnostic messages
mdl2_res <- r4ss::SS_output(dir = here::here(ass_yr, 'rsch', new_mdl2),
                            verbose = TRUE,
                            printstats = TRUE)






# plots results and stuff ----


all_summ <- r4ss::SSsummarize(list(base_res, mdl1_res, mdl2_res))

names(all_summ)

all_summ$SpawnBio

r4ss::SSplotComparisons(all_summ)



all_res <- r4ss::SSgetoutput(dirvec = c(here::here(ass_yr, 'rsch', base_mdl), here::here(ass_yr, 'rsch', new_mdl1)))


base_res$likelihoods_by_fleet
mdl1_res$likelihoods_by_fleet


base_res$likelihoods_used
mdl1_res$likelihoods_used




dat$MeanSize_at_Age_obs

list.files(here::here(ass_yr, 'rsch', new_mdl1), pattern = 'dat')

