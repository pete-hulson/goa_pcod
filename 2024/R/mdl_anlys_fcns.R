#' ALASKA PROJECTION SCENARIOS FOR STOCK SYNTHESIS 3 FOR TIER 3 MODELS WITH PARALLEL COMPUTING
#' Version JAN 11, 2024
#' Created by Steve Barbeaux E-mail: steve.barbeaux@noaa.gov  Phone: (206) 729-0871 3
#' Redeveloped without markdown option by P Hulson
#' 
#' @param dir is the model directory (default = NULL)
#' @param cyr is the model current year (default = NULL)
#' @param syr is the start year for the model (default = 1977)
#' @param sexes is the number of sexes in model (default = 1) 
#' @param fleets the fleet number in SS for your fisheries (default = c(1:3) for GOA, would be 1 for EBS/AI)
#' @param Scenario2 indicates whether you wish to have a different catch for scenario 2 (1 = FmaxABC,2 = F as s2_F, 3 = specified catch from a 
#'            formatted csv saved in the root directory named 'Scenario2_catch.csv', must have an entry for each year, season, and fleet for the years 
#'            that differ from Fmaxabc with columns "Year,Seas,Fleet,Catch_or_F" (default = 1)
#' @param s2_F F for scenario 2 (default = 0.4)
#' @param s4_F is the F for scenario 4, defaults to 0.75, should be 0.65 for some species (check your requirements)
#' @param output_name name for saved output file (default = NULL)
#'
run_ss3_mgmnt_scen <- function(dir = NULL,
                               cyr = NULL,
                               syr = 1977,
                               sexes = 1,
                               fleets = c(1:3), 
                               Scenario2 = 1, 
                               s2_F = 0.4,
                               s4_F = 0.75,
                               output_name = NULL) {
  
  # Check if the directory for management scenrios exists, if it doesn't, create it
  if (!dir.exists(here::here(dir, "mscen"))) {
    dir.create(here::here(dir, "mscen"), recursive = TRUE)
  }
  
  # get model executable name
  exe_name <- ss3_exename(dir)
  
  # set up and run scenario 1
  
  # set up scenario 1 model folder
  R.utils::copyDirectory(dir, here::here(dir, "mscen", "scenario_1"), recursive = FALSE)
  # read forecast file and set scenario 1 params
  scenario_1 <- r4ss::SS_readforecast(file = here::here(dir, "forecast.ss"),
                                      verbose = FALSE)
  scenario_1$Btarget   <- 0.4
  scenario_1$SPRtarget <- 0.4
  scenario_1$Flimitfraction <- 1.0
  r4ss::SS_writeforecast(scenario_1, 
                         dir = here::here(dir, "mscen", "scenario_1"), 
                         file = "forecast.ss", 
                         writeAll = TRUE, 
                         overwrite = TRUE,
                         verbose = FALSE)
  # get number of forecast years (for tables later)
  fcasty <- scenario_1$Nforecastyrs
  # set starter to start at params
  mdl_starter <- r4ss::SS_readstarter(file = here::here(dir, "mscen", "scenario_1", "starter.ss"),
                                      verbose = FALSE)
  mdl_starter$init_values_src = 1
  r4ss::SS_writestarter(mdl_starter, 
                        dir = here::here(dir, "mscen", "scenario_1"),
                        overwrite = TRUE,
                        verbose = FALSE)
  # get results from report file for base model (for tables later)
  rep1 <- r4ss::SS_output(dir = dir,
                          verbose = FALSE,
                          printstats = FALSE)
  # run scenario 1 first to get the forecast parameters for scenarios 6 and 7
  r4ss::run(dir = here::here(dir, "mscen", "scenario_1"),
            exe = exe_name,
            skipfinished = FALSE, 
            verbose = FALSE,
            show_in_console = FALSE)
  
  # run models in parallel
  # Define the list of remaining scenarios
  scenarios <- paste0("scenario_", seq(2, 8))
  
  # Get the number of available cores
  num_cores <- parallel::detectCores()
  if(num_cores > length(scenarios)) num_cores = length(scenarios)
  
  # Set the number of cores to be used for parallel computing
  doParallel::registerDoParallel(cores = num_cores)
  
  # Run scenarios
  foreach::foreach(scenario = scenarios) %dopar% {
    
    # read forecast file
    scenario_C <- r4ss::SS_readforecast(file = here::here(dir, "mscen", "scenario_1", "forecast.ss"),
                                        verbose = FALSE)
    
    # Create a directory for the scenario
    dir.create(here::here(dir, "mscen", scenario), recursive = TRUE, showWarnings = FALSE)
    R.utils::copyDirectory(dir, here::here(dir, "mscen", scenario), recursive = FALSE)
    
    # Modify the scenario attributes based on the scenario number
    if (scenario == 'scenario_2') {
      # scenario 2 options
      if(Scenario2 == 2){
        # set F at specified s2_F value
        Fyear <- paste0("F_", cyr + 1)
        years <- seq(cyr + 1, cyr + 15, 1)
        f2 <- s2_F * data.table::data.table(rep1$derived_quants)[Label == Fyear]$Value
        newF <- data.table::data.table(Year = years, Seas = 1, Fleet = fleets, F = f2)
        scenario_C$Forecast <- 4
        scenario_C$BforconstantF <- 0.001  ## cluge to get rid of control rule of ave. F
        scenario_C$BfornoF <- 0.0001      ## cluge to get rid of control rule scaling of ave. F
        scenario_C$InputBasis <- 99
        scenario_C$ForeCatch <- newF
      }
      if(Scenario2 == 3){
        # read in specified future catch
        scenario_C$ForeCatch <- read.csv("Scenario2_catch.csv", header = TRUE)
      }
    } else if (scenario == 'scenario_3') {
      # scenario 3
      scenario_C$Forecast <- 4
      scenario_C$BforconstantF <- 0.001  ## cluge to get rid of control rule of ave. F
      scenario_C$BfornoF <- 0.0001      ## cluge to get rid of control rule scaling of ave. F
      scenario_C$Fcast_years[c(3,4)] <- c(cyr - 5, cyr - 1)
    } else if (scenario == 'scenario_4') {
      # scenario 4
      scenario_C$Flimitfraction <- s4_F
    } else if (scenario == 'scenario_5') {
      # scenario 5
      catch <- expand.grid(Year = c((cyr + 1):(cyr + fcasty)), Seas = 1, Fleet = fleets, Catch_or_F = 0)
      names(catch) <- names(scenario_C$ForeCatch)
      scenario_C$ForeCatch <- rbind(scenario_C$ForeCatch,catch)
    } else if (scenario == 'scenario_6') {
      # scenario 6
      scenario_C$Btarget <- 0.35
      scenario_C$SPRtarget <- 0.35
      scenario_C$Flimitfraction <- 1.0
    } else if (scenario == 'scenario_7') {
      # scenario 7
      scenario_C$Btarget <- 0.35
      scenario_C$SPRtarget <- 0.35
      scenario_C$Flimitfraction <- 1.0
      x <- r4ss::SS_output(dir = here::here(dir, "mscen", "scenario_1"))
      scenario_C$ForeCatch <- r4ss::SS_ForeCatch(x, yrs = cyr:(cyr + 2))
    } else if (scenario == 'scenario_8') {
      # scenario 8
      scenario_C$Btarget   <- 0.35
      scenario_C$SPRtarget <- 0.35
      scenario_C$Flimitfraction <- 1.0
      x <- r4ss::SS_output(dir = here::here(dir, "mscen", "scenario_1"))
      scenario_C$ForeCatch <- r4ss::SS_ForeCatch(x, yrs = cyr:(cyr + 1))
    }
    # Write the modified scenario to the scenario directory
    r4ss::SS_writeforecast(scenario_C, 
                           dir = here::here(dir, "mscen", scenario), 
                           file = "forecast.ss", 
                           writeAll = TRUE, 
                           overwrite = TRUE,
                           verbose = FALSE)
    # set starter to start at params
    mdl_starter <- r4ss::SS_readstarter(file = here::here(dir, "mscen", scenario, "starter.ss"),
                                        verbose = FALSE)
    mdl_starter$init_values_src = 1
    r4ss::SS_writestarter(mdl_starter, 
                          dir = here::here(dir, "mscen", scenario),
                          overwrite = TRUE,
                          verbose = FALSE)
    
    # Run the scenario
    r4ss::run(dir = here::here(dir, "mscen", scenario),
              exe = exe_name,
              skipfinished = FALSE, 
              verbose = FALSE,
              show_in_console = FALSE)
  }
  
  # Stop parallel computing
  doParallel::stopImplicitCluster()
  
  # Get the output from each scenario
  
  # determine number of sexes to divide ssb by
  if(sexes == 1) {sex = 2} else sex = 1
  
  # get model outputs
  scenarios <- c("scenario_1", scenarios)
  mod_outs <- r4ss::SSgetoutput(dirvec = here::here(dir, "mscen", scenarios),
                                verbose = FALSE)
  mod_summ <- r4ss::SSsummarize(mod_outs,
                                verbose = FALSE)
  
  # ssb
  mscen_ssb <- mod_summ$quants %>% 
    tidytable::filter(Label %in% paste0("SSB_", seq(cyr, cyr + 13))) %>% 
    tidytable::select(-Label, -replist8) %>% 
    tidytable::rename_with(~paste("Scenario", seq(1, 7)), paste0("replist", seq(1,7))) %>% 
    tidytable::select(Year = Yr, paste("Scenario", seq(1, 7))) %>% 
    tidytable::mutate(across(.cols = names(.)[2:length(names(.))], ~round(. / sex, digits = 0)),
                      across(.cols = names(.)[2:length(names(.))], ~format(., big.mark = ",")))
  
  
  # catch
  cyr_c <- format(round(as.numeric(mod_outs[[1]]$catch %>% 
                                     tidytable::filter(Yr == cyr) %>% 
                                     tidytable::summarise(catch = sum(Obs))), digits = 0), big.mark = ",")
  mscen_catch <- data.table(Year = cyr,
                            "Scenario 1" = cyr_c,
                            "Scenario 2" = cyr_c,
                            "Scenario 3" = cyr_c,
                            "Scenario 4" = cyr_c,
                            "Scenario 5" = cyr_c,
                            "Scenario 6" = cyr_c,
                            "Scenario 7" = cyr_c) %>% 
    tidytable::bind_rows(mod_summ$quants %>% 
                           tidytable::filter(Label %in% paste0("ForeCatch_", seq(cyr, cyr + 13))) %>% 
                           tidytable::select(-Label, -replist8) %>% 
                           tidytable::rename_with(~paste("Scenario", seq(1, 7)), paste0("replist", seq(1,7))) %>% 
                           tidytable::select(Year = Yr, paste("Scenario", seq(1, 7))) %>% 
                           tidytable::mutate(across(.cols = names(.)[2:length(names(.))], ~round(., digits = 0)),
                                             across(.cols = names(.)[2:length(names(.))], ~format(., big.mark = ","))))
  
  # f
  mscen_f <- mod_summ$Fvalue %>% 
    tidytable::filter(Yr %in% seq(cyr, cyr + 13)) %>% 
    tidytable::select(-Label, -replist8) %>% 
    tidytable::rename_with(~paste("Scenario", seq(1, 7)), paste0("replist", seq(1,7))) %>% 
    tidytable::select(Year = Yr, paste("Scenario", seq(1, 7))) %>% 
    tidytable::mutate(across(.cols = names(.)[2:length(names(.))], ~round(., digits = 2)),
                      across(.cols = names(.)[2:length(names(.))], ~format(., big.mark = ",")))
  
  
  # get 2-year projections for catch and F
  SB100 <- mod_summ$quants[which(mod_summ$quants$Label == "SSB_unfished"), 1] / sex
  SB40 <- mod_summ$quants[which(mod_summ$quants$Label == "SSB_SPR"), 1] / sex
  SB35 <- mod_summ$quants[which(mod_summ$quants$Label == "SSB_SPR"), 8] / sex
  F40_1 <- mod_summ$quants[which(mod_summ$quants$Label == paste0("F_", cyr + 1)), 1]
  F35_1 <- mod_summ$quants[which(mod_summ$quants$Label == paste0("F_", cyr + 1)), 6]
  catchABC_1 <- mod_summ$quants[which(mod_summ$quants$Label == paste0("ForeCatch_", cyr + 1)), 1]
  catchOFL_1 <- mod_summ$quants[which(mod_summ$quants$Label == paste0("ForeCatch_", cyr + 1)), 6]
  F40_2 <- mod_summ$quants[which(mod_summ$quants$Label == paste0("F_", cyr + 2)), 1]
  F35_2 <- mod_summ$quants[which(mod_summ$quants$Label == paste0("F_", cyr + 2)), 6]
  catchABC_2 <- mod_summ$quants[which(mod_summ$quants$Label == paste0("ForeCatch_", cyr + 2)), 1]
  catchOFL_2 <- mod_summ$quants[which(mod_summ$quants$Label == paste0("ForeCatch_", cyr + 2)), 6]
  SSB_1 <- mod_summ$quants[which(mod_summ$quants$Label == paste0("SSB_", cyr + 1)), 1] / sex
  SSB_2 <- mod_summ$quants[which(mod_summ$quants$Label == paste0("SSB_", cyr + 2)), 1] / sex
  
  Two_Year <- data.table(Yr = c((cyr + 1):(cyr + 2)),
                         SSB = c(SSB_1, SSB_2),
                         SSB_PER = c(SSB_1 / SB100, SSB_2 / SB100),
                         SB100 = rep(SB100, 2),
                         SB40 = rep(SB40, 2),
                         SB35 = rep(SB35, 2),
                         F40 = c(F40_1, F40_2),
                         F35 = c(F35_1, F35_2),
                         C_ABC = c(catchABC_1, catchABC_2),
                         C_OFL = c(catchOFL_1, catchOFL_2))
  
  # output list
  mscen <- list(mscen_ssb = mscen_ssb,
                mscen_catch = mscen_catch,
                mscen_f = mscen_f,
                Two_year = Two_Year)
  
  # save results
  if (!dir.exists(here::here(new_year, "output", "mscen"))) {
    dir.create(here::here(new_year, "output", "mscen"), recursive = TRUE)
  }
  save(mscen, file = here::here(new_year, "output", "mscen", paste0(output_name, ".RData")))
  
}

#' function to get F when previous year catch set at ofl
#' developed by p hulson
#' 
#' @param new_year current assessment year (default = NULL)
#' @param rec_mdl recommended model name (default = NULL)
#' 
run_fofl_prev <- function(new_year = NULL,
                          rec_mdl = NULL){
  
  # get previous year ofl
  specs <- SimDesign::quiet(vroom::vroom(here::here(new_year, 'data', 'raw', 'specs.csv'), delim = ',', 
                                         progress = FALSE, 
                                         show_col_types = FALSE))
  # get ofl
  specs %>% 
    tidytable::filter(year == new_year - 1,
                      area_label == "GOA") %>% 
    tidytable::select(year, overfishing_level) -> ofl
  
  # set up model folder
  # copy ss input files
  if(!file.exists(here::here(new_year, 'mgmt', rec_mdl, "f_ofl", 'ss3.exe'))){
    r4ss::copy_SS_inputs(dir.old = here::here(new_year, 'mgmt', rec_mdl), 
                         dir.new = here::here(new_year, 'mgmt', rec_mdl, "f_ofl"),
                         copy_par = TRUE,
                         copy_exe = TRUE,
                         overwrite = TRUE,
                         verbose = FALSE)
  }
  
  # replace previous year catch with ofl
  # read in ss3 data file
  datafile <- r4ss::SS_readdat_3.30(here::here(new_year, "mgmt", rec_mdl, "f_ofl", list.files(here::here(new_year, "mgmt", rec_mdl), "GOAPcod")),
                                    verbose = FALSE)
  # compute new catch for previous year
  datafile$catch %>% 
    tidytable::filter(year == new_year - 1) %>% 
    tidytable::mutate(catch_prop = catch / sum(catch),
                      new_catch = catch_prop * ofl$overfishing_level) %>% 
    tidytable::select(year, fleet, catch = new_catch) -> new_catch
  
  # replace catches
  datafile$catch[which(datafile$catch$year == new_year - 1 & datafile$catch$fleet == 1), 'catch'] <- new_catch$catch[which(new_catch$fleet == 1)]
  datafile$catch[which(datafile$catch$year == new_year - 1 & datafile$catch$fleet == 2), 'catch'] <- new_catch$catch[which(new_catch$fleet == 2)]
  datafile$catch[which(datafile$catch$year == new_year - 1 & datafile$catch$fleet == 3), 'catch'] <- new_catch$catch[which(new_catch$fleet == 3)]
  # write out datafile
  r4ss::SS_writedat_3.30(datlist = datafile,
                         outfile = here::here(new_year, "mgmt", rec_mdl, "f_ofl", list.files(here::here(new_year, "mgmt", rec_mdl), "GOAPcod")),
                         overwrite = TRUE,
                         verbose = FALSE)

  # run model
  r4ss::run(dir = here::here(new_year, "mgmt", rec_mdl, "f_ofl"),
            extras = "-stopph 0",
            skipfinished = FALSE,
            show_in_console = FALSE,
            verbose = FALSE)
  
  # get results
  fofl_prev <- r4ss::SS_output(dir = here::here(new_year, "mgmt", rec_mdl, "f_ofl"),
                               verbose = FALSE,
                               printstats = FALSE,
                               covar = FALSE)
  
  # save results
  if (!dir.exists(here::here(new_year, "output", "fofl_prev"))) {
    dir.create(here::here(new_year, "output", "fofl_prev"), recursive = TRUE)
  }
  save(fofl_prev, file = here::here(new_year, "output", "fofl_prev", "fofl_prev.RData"))
  
}

#' function to run retrospective analysis
#' developed by p hulson
#' 
#' @param new_year current assessment year (default = NULL)
#' @param full_run boolean, whether to run full analysis or test (default = NULL)
#' @param base_mdl base model for current assessment (default = NULL)
#' @param rec_mdl recommended model for current assessment (default = NULL)
#' 
run_retro <- function(new_year = NULL,
                      full_run = NULL,
                      base_mdl = NULL,
                      rec_mdl = NULL){
  
  # define how many retro years you want to go back
  if(isTRUE(full_run)){
    ret_yr <- 10
  } else{ret_yr <- 1}
  
  ## run retro in parallel
  # list models
  mdls = c(base_mdl, rec_mdl)
  # set up index
  indx = seq(1, length(mdls))
  
  # Get the number of available cores
  num_cores <- parallel::detectCores()
  if(num_cores > length(mdls)) num_cores = length(mdls)
  # Set the number of cores to be used for parallel computing
  doParallel::registerDoParallel(cores = num_cores)
  
  foreach::foreach(i = indx) %dopar% {
    
    r4ss::retro(dir = here::here(new_year, "mgmt", mdls[i]),
                years = 0:-ret_yr)
    
  }
  
  # Stop parallel computing
  doParallel::stopImplicitCluster()
  
  # load the retrospective models
  retro_base <- r4ss::SSgetoutput(dirvec = here::here(new_year, "mgmt", base_mdl, "retrospectives", paste("retro", 0:-ret_yr, sep = "")),
                                  verbose = FALSE)
  retro_rec <- r4ss::SSgetoutput(dirvec = here::here(new_year, "mgmt", rec_mdl, "retrospectives", paste("retro", 0:-ret_yr, sep = "")),
                                 verbose = FALSE)
  # summarize the model results
  retrosumm_rec <- r4ss::SSsummarize(retro_rec,
                                     verbose = FALSE)
  retrosumm_base <- r4ss::SSsummarize(retro_base,
                                      verbose = FALSE)
  
  retro_res <- list(retrosumm_rec = retrosumm_rec, retrosumm_base = retrosumm_base)
  
  # save results
  if(isTRUE(full_run)){
    if (!dir.exists(here::here(new_year, "output", "retro"))) {
      dir.create(here::here(new_year, "output", "retro"), recursive = TRUE)
    }
    save(retro_res, file = here::here(new_year, "output", "retro", "retro_res.RData"))
  }
  
}

#' Function to perform Leave-One-Out analysis
#' Originally written by Steve Barbeaux, altered by Pete Hulson in 2022, 2023, and 2024
#' 
#' @param full_run boolean, whether to do full loo analysis (default = NULL)
#' @param dir is the model directory (default = NULL)
#' @param cyr current year (default = NULL)
#' 
run_loo <- function(full_run = NULL,
                    dir = NULL,
                    cyr = NULL){
  
  # define how many loo years you want to go back
  if(isTRUE(full_run)){
    years = 0:-10
  } else{years = 0:-1}
  
  # check if the directory for leave-one-out exists, if it doesn't, create it
  if (!dir.exists(here::here(dir, "loo"))) {
    dir.create(here::here(dir, "loo"), recursive = TRUE)
  }
  
  # run models in parallel 
  
  ## get needed stuff
  # get model executable name
  exe_name <- ss3_exename(dir)
  # set up an index vector defining which year is being left-out
  idx = seq(1, length(years))
  # define the list of loo models
  loo_mdls <- paste0("loo", years)
  # define datafile name
  datafilename <- list.files(dir, pattern = "GOAPcod")
  # get datafile input
  datafile <- r4ss::SS_readdat_3.30(here::here(dir, datafilename),
                                    verbose = FALSE)
  
  ## prep models
  for(i in idx){
    # Write SS files
    r4ss::copy_SS_inputs(dir.old = dir, 
                         dir.new = here::here(dir, "loo", loo_mdls[i]),
                         copy_par = TRUE,
                         copy_exe = TRUE,
                         overwrite = TRUE,
                         verbose = FALSE)
    
    # Change up data file for LOO
    cpue <- data.table::data.table(datafile$CPUE)
    agecomp <- data.table::data.table(datafile$agecomp)
    lencomp <- data.table::data.table(datafile$lencomp)
    yr <- cyr + years[i]
    
    cpue[year == yr & index > 0]$index <- cpue[year == yr & index > 0]$index * -1
    agecomp[year == yr & fleet > 0]$fleet <- agecomp[year == yr & fleet > 0]$fleet * -1
    lencomp[year == yr & fleet > 0]$fleet <- lencomp[year == yr & fleet > 0]$fleet * -1
    
    datafile2 <- datafile
    datafile2$CPUE <- data.frame(cpue)
    datafile2$agecomp <- data.frame(agecomp)
    datafile2$lencomp <- data.frame(lencomp)
    
    # Write out data script
    r4ss::SS_writedat_3.30(datafile2,
                           here::here(dir, "loo", loo_mdls[i], datafilename),
                           overwrite = TRUE,
                           verbose = FALSE)
    
    # read starter file
    starter <- r4ss::SS_readstarter(here::here(dir, "loo", loo_mdls[i], "starter.ss"),
                                    verbose = FALSE)
    # change init vals source
    starter$init_values_src <- 0
    # write modified starter file
    r4ss::SS_writestarter(starter, 
                          dir = here::here(dir, "loo", loo_mdls[i]), 
                          overwrite = TRUE,
                          verbose = FALSE)
  }
  
  ## Run scenarios
  # Get the number of available cores
  num_cores <- parallel::detectCores()
  if(num_cores > length(loo_mdls)) num_cores = length(loo_mdls)
  # Set the number of cores to be used for parallel computing
  doParallel::registerDoParallel(cores = num_cores)
  
  foreach::foreach(loo_mdl = loo_mdls) %dopar% {
    
    # Run the scenario
    r4ss::run(dir = here::here(dir, "loo", loo_mdl),
              exe = exe_name,
              skipfinished = FALSE, 
              verbose = FALSE,
              show_in_console = FALSE)
    
  }
  
  # Stop parallel computing
  doParallel::stopImplicitCluster()
  
  # Compile output
  loo_res <- r4ss::SSgetoutput(dirvec = here::here(dir, "loo", loo_mdls),
                               verbose = FALSE)
  loo_res <- r4ss::SSsummarize(loo_res,
                               verbose = FALSE)
  
  # save results
  if(isTRUE(full_run)){
    if (!dir.exists(here::here(new_year, "output", "loo"))) {
      dir.create(here::here(new_year, "output", "loo"), recursive = TRUE)
    }
    save(loo_res, file = here::here(new_year, "output", "loo", "loo_res.RData"))
  }
  
}


#' function to run add=one-in for most recent year of data
#' added in 2022 by p hulson, redeveloped in 2024
#' 
#' @param dir is the model directory (default = NULL)
#' @param cyr current year (default = NULL)
#' 
run_aoi <- function(dir = NULL,
                    cyr = NULL){
  
  # check if the directory for leave-one-out exists, if it doesn't, create it
  if (!dir.exists(here::here(dir, "aoi"))) {
    dir.create(here::here(dir, "aoi"), recursive = TRUE)
  }
  
  # determine which data is new
  
  # read in previous assessment ss3 datafile
  old_datafilename <- list.files(here::here(cyr, "data"), pattern = "GOAPcod")
  old_datafile <- r4ss::SS_readdat_3.30(here::here(cyr, "data", old_datafilename),
                                        verbose = FALSE)
  # read in current assessment ss3 datafile
  new_datafilename <- list.files(dir, pattern = "GOAPcod")
  new_datafile <- r4ss::SS_readdat_3.30(here::here(dir, new_datafilename),
                                        verbose = FALSE)
  # find added cpue data
  new_datafile$CPUE %>% 
    tidytable::select(year, index) %>% 
    tidytable::anti_join(old_datafile$CPUE %>% 
                           tidytable::select(year, index)) %>% 
    tidytable::filter(index > 0) %>% 
    tidytable::distinct(year, index) %>% 
    tidytable::mutate(dataset = case_when(index == 4 ~ 'BTsurv_Indx',
                                          index == 5 ~ 'LLsurv_Indx')) -> cpue_diff
  # find added agecomp data
  new_datafile$agecomp %>% 
    tidytable::select(year, fleet) %>% 
    tidytable::anti_join(old_datafile$agecomp %>% 
                           tidytable::select(year, fleet)) %>% 
    tidytable::filter(fleet > 0) %>% 
    tidytable::distinct(year, fleet) %>% 
    tidytable::mutate(dataset = case_when(fleet == 1 ~ 'Fish_CAAL_TWL',
                                          fleet == 2 ~ 'Fish_CAAL_LL',
                                          fleet == 3 ~ 'Fish_CAAL_POT',
                                          fleet == 4 ~ 'BTsurv_CAAL')) -> agecomp_diff
  # find added lencomp data
  new_datafile$lencomp %>% 
    tidytable::select(year, fleet) %>% 
    tidytable::anti_join(old_datafile$lencomp %>% 
                           tidytable::select(year, fleet)) %>% 
    tidytable::filter(fleet > 0) %>% 
    tidytable::distinct(year, fleet) %>% 
    tidytable::mutate(dataset = case_when(fleet == 1 ~ 'Fish_LC_TWL',
                                          fleet == 2 ~ 'Fish_LC_LL',
                                          fleet == 3 ~ 'Fish_LC_POT',
                                          fleet == 4 ~ 'BTsurv_LC',
                                          fleet == 5 ~ 'LLsurv_LC')) -> lencomp_diff
  # bind new data
  cpue_diff %>% 
    tidytable::bind_rows(agecomp_diff %>% 
                           tidytable::rename(index = fleet)) %>% 
    tidytable::bind_rows(lencomp_diff %>% 
                           tidytable::rename(index = fleet)) -> data_diff
  
  
  # run models in parallel 
  
  ## get needed stuff
  # get model executable name
  exe_name <- ss3_exename(dir)
  # set up an index vector defining which data is being left-out
  idx = seq(1, length(data_diff$dataset) + 1)
  # define the list of loo models
  aoi_mdls <- c("Base", data_diff$dataset)
  # define datafile name
  datafilename <- list.files(dir, pattern = "GOAPcod")
  # Get the number of available cores
  num_cores <- parallel::detectCores()
  if(num_cores > length(aoi_mdls)) num_cores = length(aoi_mdls)
  # Set the number of cores to be used for parallel computing
  doParallel::registerDoParallel(cores = num_cores)
  
  ## prep models
  for(i in idx){
    # Write SS files
    r4ss::copy_SS_inputs(dir.old = dir, 
                         dir.new = here::here(dir, "aoi", aoi_mdls[i]),
                         copy_par = TRUE,
                         copy_exe = TRUE,
                         overwrite = TRUE,
                         verbose = FALSE)
    
    # turn all new data off
    cpue <- data.table::data.table(new_datafile$CPUE)
    if(length(cpue_diff$year) > 0){
      cpue[year %in% cpue_diff$year & index %in% cpue_diff$index]$index <- cpue[year %in% cpue_diff$year & index %in% cpue_diff$index]$index * -1
    }
    agecomp <- data.table::data.table(new_datafile$agecomp)
    if(length(agecomp_diff$year) > 0){
      agecomp[year %in% agecomp_diff$year & fleet %in% agecomp_diff$fleet]$fleet <- agecomp[year %in% agecomp_diff$year & fleet %in% agecomp_diff$fleet]$fleet * -1
    }
    lencomp <- data.table::data.table(new_datafile$lencomp)
    if(length(lencomp_diff$year) > 0){
      lencomp[year %in% lencomp_diff$year & fleet %in% lencomp_diff$fleet]$fleet <- lencomp[year %in% lencomp_diff$year & fleet %in% lencomp_diff$fleet]$fleet * -1
    }
    
    # cpue data
    if(aoi_mdls[i] == 'BTsurv_Indx'){
      cpue[year == data_diff$year[which(data_diff$dataset == "BTsurv_Indx")] & index == -4]$index <- cpue[year == data_diff$year[which(data_diff$dataset == "BTsurv_Indx")] & index == -4]$index * -1
    }
    if(aoi_mdls[i] == 'LLsurv_Indx'){
      cpue[year == data_diff$year[which(data_diff$dataset == "LLsurv_Indx")] & index == -5]$index <- cpue[year == data_diff$year[which(data_diff$dataset == "LLsurv_Indx")] & index == -5]$index * -1
    }
    # caal data
    if(aoi_mdls[i] == 'Fish_CAAL_TWL'){
      agecomp[year == data_diff$year[which(data_diff$dataset == "Fish_CAAL_TWL")] & fleet == -1]$fleet <- agecomp[year == data_diff$year[which(data_diff$dataset == "Fish_CAAL_TWL")] & fleet == -1]$fleet * -1
    }
    if(aoi_mdls[i] == 'Fish_CAAL_LL'){
      agecomp[year == data_diff$year[which(data_diff$dataset == "Fish_CAAL_LL")] & fleet == -2]$fleet <- agecomp[year == data_diff$year[which(data_diff$dataset == "Fish_CAAL_LL")] & fleet == -2]$fleet * -1
    }
    if(aoi_mdls[i] == 'Fish_CAAL_POT'){
      agecomp[year == data_diff$year[which(data_diff$dataset == "Fish_CAAL_POT")] & fleet == -3]$fleet <- agecomp[year == data_diff$year[which(data_diff$dataset == "Fish_CAAL_POT")] & fleet == -3]$fleet * -1
    }
    if(aoi_mdls[i] == 'BTsurv_CAAL'){
      agecomp[year == data_diff$year[which(data_diff$dataset == "BTsurv_CAAL")] & fleet == -4]$fleet <- agecomp[year == data_diff$year[which(data_diff$dataset == "BTsurv_CAAL")] & fleet == -4]$fleet * -1
    }
    # length comp
    if(aoi_mdls[i] == 'Fish_LC_TWL'){
      lencomp[year == data_diff$year[which(data_diff$dataset == "Fish_LC_TWL")] & fleet == -1]$fleet <- lencomp[year == data_diff$year[which(data_diff$dataset == "Fish_LC_TWL")] & fleet == -1]$fleet * -1
    }
    if(aoi_mdls[i] == 'Fish_LC_LL'){
      lencomp[year == data_diff$year[which(data_diff$dataset == "Fish_LC_LL")] & fleet == -2]$fleet <- lencomp[year == data_diff$year[which(data_diff$dataset == "Fish_LC_LL")] & fleet == -2]$fleet * -1
    }
    if(aoi_mdls[i] == 'Fish_LC_POT'){
      lencomp[year == data_diff$year[which(data_diff$dataset == "Fish_LC_POT")] & fleet == -3]$fleet <- lencomp[year == data_diff$year[which(data_diff$dataset == "Fish_LC_POT")] & fleet == -3]$fleet * -1
    }
    if(aoi_mdls[i] == 'BTsurv_LC'){
      lencomp[year == data_diff$year[which(data_diff$dataset == "BTsurv_LC")] & fleet == -4]$fleet <- lencomp[year == data_diff$year[which(data_diff$dataset == "BTsurv_LC")] & fleet == -4]$fleet * -1
    }
    if(aoi_mdls[i] == 'LLsurv_LC'){
      lencomp[year == data_diff$year[which(data_diff$dataset == "LLsurv_LC")] & fleet == -5]$fleet <- lencomp[year == data_diff$year[which(data_diff$dataset == "LLsurv_LC")] & fleet == -5]$fleet * -1
    }
    
    datafile2 <- new_datafile
    datafile2$CPUE <- data.frame(cpue)
    datafile2$agecomp <- data.frame(agecomp)
    datafile2$lencomp <- data.frame(lencomp)
    
    # Write out data script
    r4ss::SS_writedat_3.30(datafile2,
                           here::here(dir, "aoi", aoi_mdls[i], datafilename),
                           overwrite = TRUE,
                           verbose = FALSE)
    
    # read starter file
    starter <- r4ss::SS_readstarter(here::here(dir, "aoi", aoi_mdls[i], "starter.ss"),
                                    verbose = FALSE)
    # change init vals source
    starter$init_values_src <- 0
    # write modified starter file
    r4ss::SS_writestarter(starter, 
                          dir = here::here(dir, "aoi", aoi_mdls[i]), 
                          overwrite = TRUE,
                          verbose = FALSE)
    
  }
  
  ## Run scenarios
  foreach::foreach(aoi_mdl = aoi_mdls) %dopar% {
    
    # Run the scenario
    r4ss::run(dir = here::here(dir, "aoi", aoi_mdl),
              exe = exe_name,
              skipfinished = FALSE, 
              verbose = FALSE,
              show_in_console = FALSE)
    
  }
  
  # Stop parallel computing
  doParallel::stopImplicitCluster()
  
  
  # Compile output
  aoi_res <- r4ss::SSgetoutput(dirvec = here::here(dir, "aoi", aoi_mdls[2:length(aoi_mdls)]),
                               verbose = FALSE)
  aoi_res <- r4ss::SSsummarize(aoi_res,
                               verbose = FALSE)
  base_res <- r4ss::SS_output(dir = here::here(dir, "aoi", aoi_mdls[1]),
                              verbose = FALSE,
                              printstats = FALSE)
  aoi <- list(aoi_res = aoi_res, 
              base_res = base_res,
              aoi_mdls = aoi_mdls)
  
  # save results
  if (!dir.exists(here::here(new_year, "output", "aoi"))) {
    dir.create(here::here(new_year, "output", "aoi"), recursive = TRUE)
  }
  save(aoi, file = here::here(new_year, "output", "aoi", "aoi.RData"))
  
}

#' function to run jitter analysis
#' 
#' @param full_run boolean, whether to do full loo analysis (default = NULL)
#' @param new_year current assessment year (default = NULL)
#' @param rec_mdl recommended model name (default = NULL)
#' 
run_jitter <- function(full_run = NULL,
                       new_year = NULL,
                       rec_mdl = NULL){
  
  # define how many jitters you want to go back
  if(isTRUE(full_run)){
    Njitter <- 50
  } else{Njitter <- 5}
  
  # set up folder
  if (!file.exists(here::here(new_year, "mgmt", rec_mdl, "jitter"))) {
    dir.create(here::here(new_year, "mgmt", rec_mdl, "jitter"), recursive = TRUE)
  }
  
  # copy ss3 files
  r4ss::copy_SS_inputs(dir.old = here::here(new_year, "mgmt", rec_mdl), 
                       dir.new = here::here(new_year, "mgmt", rec_mdl, "jitter"),
                       copy_par = TRUE,
                       copy_exe = TRUE,
                       overwrite = TRUE,
                       verbose = FALSE)
  
  # run the jitters in parallel
  ncores <- parallelly::availableCores(omit = 1)
  future::plan(future::multisession, workers = ncores)
  r4ss::jitter(dir = here::here(new_year, "mgmt", rec_mdl, "jitter"),
               Njitter = Njitter,
               jitter_fraction = 0.05,
               init_values_src = 1,
               exe = "ss3",
               printlikes = FALSE,
               verbose = FALSE)
  future::plan(future::sequential)
  
  # get results
  jitt_res <- SSgetoutput(dirvec = here::here(new_year, "mgmt", rec_mdl, "jitter"),, 
                          keyvec = 1:Njitter, 
                          getcovar = FALSE,
                          verbose = FALSE)
  
  jitt_res <- r4ss::SSsummarize(jitt_res,
                                verbose = FALSE)
  
  # save results
  if(isTRUE(full_run)){
    if (!dir.exists(here::here(new_year, "output", "jitter"))) {
      dir.create(here::here(new_year, "output", "jitter"), recursive = TRUE)
    }
    save(jitt_res, file = here::here(new_year, "output", "jitter", "jitt_res.RData"))
  }
}

#' function to run parameter profiles
#' developed in 2024 by p hulson
#' 
#' @param mdl_dir is the model directory (default = NULL)
#' @param res_dir is the results directory (default = NULL)
#' @param params parameters for which to perform profile (default = NULL)
#' @param profilevec vector of parameter values over which to profile (default = NULL)
#' @param linenum line number for parameter in ctl file (default = NULL)
#' @param mod_ctl model ctl name (default = NULL)
#' @param full_run boolean, if full_run = TRUE then write results (default = FALSE)
#' 
run_profile <- function(mdl_dir = NULL,
                        res_dir = NULL,
                        params = NULL,
                        profilevec = NULL,
                        linenum = NULL,
                        mod_ctl = NULL,
                        full_run = FALSE){
  
  # get model executable name
  exe_name <- ss3_exename(mdl_dir)
  # set up an index vector defining which year is being left-out
  idx = seq(1, length(params))
  
  # run profiles
  # Get the number of available cores
  num_cores <- parallel::detectCores()
  if(num_cores > length(params)) num_cores = length(params)
  # Set the number of cores to be used for parallel computing
  doParallel::registerDoParallel(cores = num_cores)
  # run in parallel
  foreach::foreach(i = idx) %dopar% {
    
    # set up folder for profile
    R.utils::copyDirectory(mdl_dir, here::here(mdl_dir, "profile", params[i]), recursive = FALSE)
    
    # read starter file
    starter <- r4ss::SS_readstarter(here::here(mdl_dir, "profile", params[i], "starter.ss"),
                                    verbose = FALSE)
    # change init vals source
    starter$init_values_src <- 0
    # write modified starter file
    r4ss::SS_writestarter(starter, 
                          dir = here::here(mdl_dir, "profile", params[i]), 
                          overwrite = TRUE,
                          verbose = FALSE)
    
    # run profile
    r4ss::profile(dir = here::here(mdl_dir, "profile", params[i]),
                  oldctlfile = mod_ctl,
                  newctlfile = mod_ctl,
                  linenum = linenum[i],
                  profilevec = profilevec[[i]],
                  verbose = FALSE)
    
    # write results
    if(isTRUE(full_run)){
      # create results folder
      if (!dir.exists(res_dir)) {
        dir.create(res_dir, recursive = TRUE)
      }
      # get results
      res_prof <- r4ss::SSgetoutput(dirvec = here::here(mdl_dir, "profile", params[i]),
                                    keyvec = 1:length(profilevec[[i]]),
                                    verbose = FALSE)
      summ_prof <- r4ss::SSsummarize(res_prof,
                                     verbose = FALSE)
      # write results
      save(summ_prof, file = here::here(res_dir, paste0(params[i], "_prof.RData")))
    }
  }
  
  # Stop parallel computing
  doParallel::stopImplicitCluster()
  
}

#' Function to perform ll survey catchability analysis
#' written by by Pete Hulson in 2023, and made intoa  function in 2024
#' 
#' @param dir is the model directory (default = NULL)
#' @param ctl_filename name of model ctl (default = NULL)
#' @param full_run boolean, whether full run should be performed for white noise case (default = NULL)
#' 
run_llq <- function(dir = NULL,
                    ctl_filename = NULL,
                    full_run = NULL){
  
  # check if the directory for ll q analysis exists, if it doesn't, create it
  if (!dir.exists(here::here(dir, "llq"))) {
    dir.create(here::here(dir, "llq", "no_cov"), recursive = TRUE)
    dir.create(here::here(dir, "llq", "rand_cov"), recursive = TRUE)
  }
  
  # copy ss3 files
  # for no covariate
  r4ss::copy_SS_inputs(dir.old = dir, 
                       dir.new = here::here(dir, "llq", "no_cov"),
                       copy_exe = TRUE,
                       overwrite = TRUE,
                       verbose = FALSE)
  # for random covariate
  r4ss::copy_SS_inputs(dir.old = dir, 
                       dir.new = here::here(dir, "llq", "rand_cov"),
                       copy_exe = TRUE,
                       overwrite = TRUE,
                       verbose = FALSE)
  
  # run no llq covariate
  
  # turn off llq covariate in ctl
  ctl <- r4ss::SS_readctl_3.30(here::here(dir, ctl_filename),
                               verbose = FALSE)
  ctl$Q_parms$`env_var&link` = 0
  ctl$Q_parms_tv = NULL
  r4ss::SS_writectl_3.30(ctllist = ctl,
                         outfile = here::here(dir, "llq", "no_cov",ctl_filename),
                         overwrite = TRUE,
                         verbose = FALSE)
  
  # change starter file
  # read starter file
  starter <- r4ss::SS_readstarter(here::here(dir, "llq", "no_cov", "starter.ss"),
                                  verbose = FALSE)
  # change init vals source
  starter$init_values_src <- 0
  # write modified starter file
  r4ss::SS_writestarter(starter, 
                        dir = here::here(dir, "llq", "no_cov"), 
                        overwrite = TRUE,
                        verbose = FALSE)
  
  # run model
  suppressWarnings(r4ss::run(dir = here::here(dir, "llq", "no_cov"),
                             skipfinished = FALSE,
                             show_in_console = FALSE,
                             verbose = FALSE))
  
  
  # white noise llq covariate
  
  # change starter file
  # read starter file
  starter <- r4ss::SS_readstarter(here::here(dir, "llq", "rand_cov", "starter.ss"),
                                  verbose = FALSE)
  # change init vals source
  starter$init_values_src <- 0
  # write modified starter file
  r4ss::SS_writestarter(starter, 
                        dir = here::here(dir, "llq", "rand_cov"), 
                        overwrite = TRUE,
                        verbose = FALSE)
  
  # read in ss3 data
  # define datafile name
  datafilename <- list.files(dir, pattern = "GOAPcod")
  # get datafile input
  datafile <- r4ss::SS_readdat_3.30(here::here(dir, datafilename),
                                    verbose = FALSE)
  
  # run models
  # determine number of iterations
  if(isTRUE(full_run)){
    iters <- 50 # for full
  } else{
    iters <- 5 # for testing
  }
  # run
  rand_res <- SimDesign::quiet(purrr::map(1:iters, ~ llq_rand(dat = datafile, 
                                                              dir = here::here(dir, "llq", "rand_cov"), 
                                                              datafilename = datafilename)))
  
  # get results
  
  # read base results
  res_base <- r4ss::SS_output(dir = here::here(dir),
                              verbose = FALSE,
                              printstats = FALSE)
  # read no covariate results
  res_run_nocov <- suppressWarnings(r4ss::SS_output(dir = here::here(dir, 'llq', 'no_cov'),
                                                    covar = FALSE,
                                                    verbose = FALSE,
                                                    printstats = FALSE))
  
  # set up likelihood results
  res_base$likelihoods_used %>% 
    tidytable::mutate(like_compon = rownames(res_base$likelihoods_used)) %>% 
    tidytable::select(like_compon, values) %>% 
    tidytable::filter(like_compon == "TOTAL") %>% 
    tidytable::bind_rows(res_base$likelihoods_by_fleet %>% 
                           dplyr::rename_all(tolower) %>% 
                           tidytable::select(label, llsrv) %>% 
                           tidytable::filter(label %in% c("Surv_like", "Length_like")) %>% 
                           tidytable::rename(like_compon = "label",
                                             values = "llsrv")) %>% 
    tidytable::mutate(model = "base") %>% 
    tidytable::bind_rows(res_run_nocov$likelihoods_used %>% 
                           tidytable::mutate(like_compon = rownames(res_run_nocov$likelihoods_used)) %>% 
                           tidytable::select(like_compon, values) %>% 
                           tidytable::filter(like_compon == "TOTAL") %>% 
                           tidytable::bind_rows(res_run_nocov$likelihoods_by_fleet %>% 
                                                  dplyr::rename_all(tolower) %>% 
                                                  tidytable::select(label, llsrv) %>% 
                                                  tidytable::filter(label %in% c("Surv_like", "Length_like")) %>% 
                                                  tidytable::rename(like_compon = "label",
                                                                    values = "llsrv")) %>% 
                           tidytable::mutate(model = "no_cov")) %>% 
    tidytable::bind_rows(do.call(mapply, c(list, rand_res, SIMPLIFY = FALSE))$likes %>% 
                           tidytable::map_df(., ~as.data.frame(.x), .id = "rand") %>% 
                           tidytable::mutate(model = paste0("rand", rand)) %>% 
                           tidytable::select(like_compon, values, model)) -> likes

  # set up predicted ll surv results
  res_base$cpue %>% 
    tidytable::filter(Fleet == 5) %>% 
    tidytable::select(Yr, Exp) %>% 
    tidytable::rename(year = 'Yr',
                      values = 'Exp') %>% 
    tidytable::mutate(model = "base") %>% 
    tidytable::bind_rows(res_run_nocov$cpue %>% 
                           tidytable::filter(Fleet == 5) %>% 
                           tidytable::select(Yr, Exp) %>% 
                           tidytable::rename(year = 'Yr',
                                             values = 'Exp') %>% 
                           tidytable::mutate(model = "no_cov")) %>% 
    tidytable::bind_rows(do.call(mapply, c(list, rand_res, SIMPLIFY = FALSE))$pred_ll %>% 
                           tidytable::map_df(., ~as.data.frame(.x), .id = "rand") %>% 
                           tidytable::mutate(model = paste0("rand", rand)) %>% 
                           tidytable::select(year, values, model)) -> pred_ll
  
  # output
  llq_res <- list(likes = likes, pred_ll = pred_ll)
  
  # save results
  if(isTRUE(full_run)){
    if (!dir.exists(here::here(new_year, "output", "llq"))) {
      dir.create(here::here(new_year, "output", "llq"), recursive = TRUE)
    }
    save(llq_res, file = here::here(new_year, "output", "llq", "llq_res.RData"))
  }
  
}

#' Function to perform ll survey catchability analysis for random covariate
#' written by by Pete Hulson in 2023, and made intoa  function in 2024
#' 
#' @param dat ss3 data list (default = NULL)
#' @param dir is the model directory (default = NULL)
#' @param datafilname name of ss3 datafile (default = NULL)
#' 
llq_rand <- function(dat = NULL, 
                     dir = NULL, 
                     datafilename = NULL){
  
  # generate white noise
  dat$envdat$value <- rnorm(length(dat$envdat$value))
  # write datafile
  r4ss::SS_writedat_3.30(dat,
                         here::here(dir, datafilename), 
                         overwrite = TRUE,
                         verbose = FALSE)
  # run model
  r4ss::run(dir = dir,
            extras = "-nohess",
            skipfinished = FALSE,
            show_in_console = FALSE,
            verbose = FALSE)
  # read the model output and save
  res <- suppressWarnings(r4ss::SS_output(dir = dir,
                                          verbose = FALSE,
                                          printstats = FALSE))
  # format likelihood results
  res$likelihoods_used %>% 
    tidytable::mutate(like_compon = rownames(res$likelihoods_used)) %>% 
    tidytable::select(like_compon, values) %>% 
    tidytable::filter(like_compon == "TOTAL") %>% 
    tidytable::bind_rows(res$likelihoods_by_fleet %>% 
                           dplyr::rename_all(tolower) %>% 
                           tidytable::select(label, llsrv) %>% 
                           tidytable::filter(label %in% c("Surv_like", "Length_like")) %>% 
                           tidytable::rename(like_compon = "label",
                                             values = "llsrv")) -> likes
  # format predicted ll surv results
  res$cpue %>% 
    tidytable::filter(Fleet == 5) %>% 
    tidytable::select(Yr, Exp) %>% 
    tidytable::rename(year = 'Yr',
                      values = 'Exp') -> pred_ll
  # output
  list(likes = likes, pred_ll = pred_ll)
}

#' function to run apportionment
#' 
#' @param new_year current assessment year (default = NULL)
#' 
run_apport <- function(new_year = NULL){
  
  # get data
  biomass_dat <- vroom::vroom(here::here(new_year,'data','raw','twl_srvy_index.csv'), 
                              delim = ",", 
                              progress = FALSE, 
                              show_col_types = FALSE) %>%
    tidytable::filter(strata < 99900) %>% 
    tidytable::mutate(sd = sqrt(biom_var),
                      cv = sd / biom) %>%
    tidytable::select(strata = area,
                      year,
                      biomass = biom,
                      cv)
  
  # run rema model for trawl survey only
  apport_in <- SimDesign::quiet(rema::prepare_rema_input(model_name = paste0("pcod trawl survey"),
                                                         biomass_dat = biomass_dat))
  apport_mdl <- SimDesign::quiet(rema::fit_rema(apport_in))
  apport_out <- SimDesign::quiet(rema::tidy_rema(rema_model = apport_mdl))
  
  # save results
  if (!dir.exists(here::here(new_year, "output", "apport"))) {
    dir.create(here::here(new_year, "output", "apport"), recursive = TRUE)
  }
  save(apport_out, file = here::here(new_year, "output", "apport", "apport.RData"))
  
  
}

#' function to run mcmc
#' 
#' @param full_run boolean, whether to do full loo analysis (default = NULL)
#' @param new_year current assessment year (default = NULL)
#' @param rec_mdl recommended model name (default = NULL)
#' 
run_mcmc <- function(full_run = NULL,
                     new_year = NULL,
                     rec_mdl = NULL){
  
  # define number of iterations
  if(isTRUE(full_run)){
    iter <- 350000
    thin <- 50
    warmup <- 250
  } else{
    iter <-5000
    thin <- 5
    warmup <- 250
  }
  
  # set up model
  
  # set up folder
  if (!file.exists(here::here(new_year, "mgmt", rec_mdl, "mcmc"))) {
    dir.create(here::here(new_year, "mgmt", rec_mdl, "mcmc"), recursive = TRUE)
  }
  # copy model files
  R.utils::copyDirectory(here::here(new_year, "mgmt", rec_mdl), 
                         here::here(new_year, "mgmt", rec_mdl, "mcmc"), recursive = FALSE)
  
  # read starter file
  starter <- r4ss::SS_readstarter(here::here(new_year, "mgmt", rec_mdl, "mcmc", "starter.ss"),
                                  verbose = FALSE)
  # change init vals source
  starter$init_values_src <- 0
  # write modified starter file
  r4ss::SS_writestarter(starter, 
                        dir = here::here(new_year, "mgmt", rec_mdl, "mcmc"), 
                        overwrite = TRUE,
                        verbose = FALSE)
  
  # run adnuts
  tictoc::tic()
  mcmc_adnut <- SimDesign::quiet(adnuts::sample_rwm(model = 'ss3',
                                                    path = here::here(new_year, "mgmt", rec_mdl, "mcmc"),
                                                    iter = iter,
                                                    chains = 7,
                                                    warmup = warmup,
                                                    thin = thin,
                                                    mceval = FALSE,
                                                    control = list(metric = 'mle'),
                                                    skip_optimization = FALSE,
                                                    verbose = FALSE))
  nuts_time <- tictoc::toc(quiet = TRUE)
  
  # run mceval
  tictoc::tic()
  r4ss::run(dir = here::here(new_year, "mgmt", rec_mdl, "mcmc"),
            extras = "-mceval",
            skipfinished = FALSE,
            show_in_console = FALSE,
            verbose = FALSE)
  eval_time <- tictoc::toc(quiet = TRUE)
  
  # save results
  if(isTRUE(full_run)){
    if (!dir.exists(here::here(new_year, "output", "mcmc"))) {
      dir.create(here::here(new_year, "output", "mcmc"), recursive = TRUE)
    }
    # Read output
    mcmc_eval <- r4ss::SSgetMCMC(here::here(new_year, "mgmt", rec_mdl, "mcmc"),
                                 verbose = FALSE)
    save(mcmc_adnut, file = here::here(new_year, "output", "mcmc", "mcmc_adnut.RData"))
    save(mcmc_eval, file = here::here(new_year, "output", "mcmc", "mcmc_eval.RData"))
  }
  
  list(nuts_time = nuts_time, eval_time = eval_time)
}


