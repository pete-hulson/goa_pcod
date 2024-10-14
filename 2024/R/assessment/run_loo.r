#' Function to perform Leave-One-Out analysis
#' Originally written by Steve Barbeaux, altered by Pete Hulson in 2022, 2023, and 2024
#' 
#' @param dir is the model directory (default = NULL)
#' @param years years over which to run analysis (default = 0:-10)
#' @param cyr current year (default = NULL)
#' 
year_loo <- function(dir = NULL,
                     years = 0:-10,
                     cyr = NULL){
  
  
  # check if the directory for leave-one-out exists, if it doesn't, create it
  if (!dir.exists(here::here(dir, "loo", "year"))) {
    dir.create(here::here(dir, "loo", "year"), recursive = TRUE)
  }
  
  # run models in parallel ---- 
  
  ## get needed stuff ----
  # get model executable name
  exe_name <- ss3_exename(dir)
  # set up an index vector defining which year is being left-out
  idx = seq(1, length(years))
  # define the list of loo models
  loo_mdls <- paste0("loo", years)
  # define datafile name
  datafilename <- list.files(dir, pattern = "GOAPcod")
  # get datafile input
  datafile <- r4ss::SS_readdat_3.30(here::here(dir, datafilename))

  ## prep models ----
  for(i in idx){
    # Write SS files
    r4ss::copy_SS_inputs(dir.old = dir, 
                         dir.new = here::here(dir, "loo", "year", loo_mdls[i]),
                         copy_par = TRUE,
                         copy_exe = TRUE,
                         overwrite = TRUE)
    
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
                           here::here(dir, "loo", "year", loo_mdls[i], datafilename),
                           overwrite = TRUE)
  }
  
  ## Run scenarios ----
  # Get the number of available cores
  num_cores <- parallel::detectCores()
  if(num_cores > length(loo_mdls)) num_cores = length(loo_mdls)
  # Set the number of cores to be used for parallel computing
  doParallel::registerDoParallel(cores = num_cores)
  
  foreach::foreach(loo_mdl = loo_mdls) %dopar% {
    
    # Run the scenario
    r4ss::run(dir = here::here(dir, "loo", "year", loo_mdl),
              exe = exe_name,
              skipfinished = FALSE, 
              verbose = FALSE,
              show_in_console = FALSE)
    
  }
  
  # Stop parallel computing
  doParallel::stopImplicitCluster()
  
  # Compile output ----
  LOO_mods <- r4ss::SSgetoutput(dirvec = here::here(dir, "loo", "year", loo_mdls))
  
  Nat_M <- array()
  Q <- array()
  SSB_UN <- array()
  annF_Btgt <- array()
  Nat_M_SD <- array()
  Q_SD <- array()
  SSB_UN_SD <- array()
  annF_Btgt_SD <- array()
  SSBfore <- array()
  SSBfore_SD <- array()
  ABCfore <- array()
  ABCfore_SD <- array()
  
  for(i in 1:length(years)){
    x <- data.table(LOO_mods[[i]]$parameters)
    y <- data.table(LOO_mods[[i]]$derived_quants)
    annF_Btgt[i] <- y[Label == 'annF_Btgt']$Value
    annF_Btgt_SD[i] <- y[Label == 'annF_Btgt']$StdDev
    Nat_M[i] <- x[Label == "NatM_uniform_Fem_GP_1"]$Value
    Nat_M_SD[i] <- x[Label == "NatM_uniform_Fem_GP_1"]$Parm_StDev
    Q[i] <- x[Label %in% "LnQ_base_Srv(4)"]$Value
    Q_SD[i] <- x[Label %in% "LnQ_base_Srv(4)"]$Parm_StDev
    SSB_UN[i] <- y[Label == 'SSB_unfished']$Value
    SSB_UN_SD[i] <- y[Label == 'SSB_unfished']$StdDev
    SSBfore[i] <- y[Label == paste0('SSB_', cyr + 1)]$Value
    SSBfore_SD[i] <- y[Label == paste0('SSB_', cyr + 1)]$StdDev
    ABCfore[i] <- y[Label == paste0('ForeCatch_', cyr + 1)]$Value
    ABCfore_SD[i] <- y[Label == paste0('ForeCatch_', cyr + 1)]$StdDev
  }
  
  mods0 <- r4ss::SSgetoutput(dirvec = dir)
  
  x0 <- data.table(mods0[[1]]$parameters)
  y0 <- data.table(mods0[[1]]$derived_quants)
  annF_Btgt0 <- y0[Label == 'annF_Btgt']$Value
  annF_Btgt_SD0 <- y0[Label == 'annF_Btgt']$StdDev
  Nat_M0 <- x0[Label == "NatM_uniform_Fem_GP_1"]$Value
  Nat_M_SD0 <- x0[Label == "NatM_uniform_Fem_GP_1"]$Parm_StDev
  Q0 <- x0[Label %in% "LnQ_base_Srv(4)"]$Value
  Q_SD0 <- x0[Label %in% "LnQ_base_Srv(4)"]$Parm_StDev
  SSB_UN0 <- y0[Label == 'SSB_unfished']$Value
  SSB_UN_SD0 <- y0[Label == 'SSB_unfished']$StdDev
  SSBfore0 <- y0[Label == paste0('SSB_', cyr + 1)]$Value
  SSBfore_SD0 <- y0[Label == paste0('SSB_', cyr + 1)]$StdDev
  ABCfore0 <- y0[Label == paste0('ForeCatch_', cyr + 1)]$Value
  ABCfore_SD0 <- y0[Label == paste0('ForeCatch_', cyr + 1)]$StdDev
  
  x0 <- data.table(LOO = 0,
                   Nat_M = Nat_M0, 
                   Nat_M_SD = Nat_M_SD0,
                   annF_Btgt = annF_Btgt0, 
                   annF_Btgt_SD = annF_Btgt_SD0,
                   Q = exp(Q0), 
                   Q_SD = Q_SD0, 
                   SSB_UN = SSB_UN0,
                   SSB_UN_SD = SSB_UN_SD0,
                   SSBfore = SSBfore0,
                   SSBfore_SD = SSBfore_SD0,
                   ABCfore = ABCfore0,
                   ABCfore_SD = ABCfore_SD0)
  
  x1<-data.table(LOO = c(1:length(years)),
                 Nat_M = Nat_M, 
                 Nat_M_SD = Nat_M_SD, 
                 annF_Btgt = annF_Btgt, 
                 annF_Btgt_SD = annF_Btgt_SD,
                 Q = exp(Q), 
                 Q_SD = Q_SD, 
                 SSB_UN = SSB_UN,
                 SSB_UN_SD = SSB_UN_SD,
                 SSBfore = SSBfore,
                 SSBfore_SD = SSBfore_SD,
                 ABCfore = ABCfore,
                 ABCfore_SD = ABCfore_SD)
  
  x <- rbind(x0, x1)
  x2 <- data.table(melt(x, "LOO"))
  x3 <- x2[!variable %like% "_SD"]
  x4 <- x2[variable %like% "_SD"]
  x3$SD <- x4$value
  x3$Year <- cyr - x3$LOO + 1
  
  ## Plot LOO analysis ----
  d <- ggplot(x3[LOO != 0],
              aes(x = Year,y = value)) +
    geom_errorbar(aes(ymin = value - 1.96 * SD, ymax = value + 1.96 * SD), width = 0.25) +
    geom_point(size = 3) +
    geom_hline(data = x3[LOO == 0],
               aes(yintercept = value), linewidth = 1.25, linetype = 2, color = "red") +
    theme_bw(base_size = 16) +
    labs(x = 'Leave one out year', y = 'Parameter value') +
    facet_wrap( ~ variable, 
                scales = "free_y", 
                ncol = 2) +
    scale_x_continuous(limits = c(min(x3[LOO != 0]$Year) - 0.5, max(x3[LOO != 0]$Year) + 0.5), 
                       breaks = seq(min(x3[LOO != 0]$Year), max(x3[LOO != 0]$Year), by = 1)) +
    theme(axis.text.x = element_text(vjust = 0.5, angle = 90))
  
  ## Table of LOO analysis ----
  x3 <- data.table(Nat_M = x$Nat_M,
                   annF_Btgt = x$annF_Btgt,
                   Q = x$Q,
                   SSB_UN = x$SSB_UN,
                   SSBfore = x$SSBfore,
                   ABCfore = x$ABCfore )
  x4 <- x3[1, ]
  x4 <- data.table(Nat_M = rep(x4$Nat_M, length(years) + 1),
                   annF_Btgt = rep(x4$annF_Btgt, length(years) + 1),
                   Q = rep(x4$Q, length(years) + 1),
                   SSB_UN = rep(x4$SSB_UN, length(years) + 1),
                   SSBfore = rep(x4$SSBfore, length(years) + 1),
                   ABCfore = rep(x4$ABCfore, length(years) + 1))
  x4 <- x3[2:(1 + length(years)), ] - x4[2:(1 + length(years)), ]
  x4$LOO <- c(1:length(years))
  
  x4 <- melt(x4, 'LOO')
  
  x4 %>% 
    tidytable::summarise(bias = mean(value), .by = variable) %>% 
    tidytable::mutate(p = bias / t(x3[1, ])) -> BIAS
  
  output <- list(BIAS, d)
  
  output
  
}

#' function to run L-O-O for most recent year of data
#' added in 2022 by p hulson, redeveloped in 2024
#' 
#' @param dir is the model directory (default = NULL)
#' @param cyr current year (default = NULL)
#' 
data_loo <- function(dir = NULL,
                     cyr = NULL){
  
  # check if the directory for leave-one-out exists, if it doesn't, create it
  if (!dir.exists(here::here(dir, "loo", "data"))) {
    dir.create(here::here(dir, "loo", "data"), recursive = TRUE)
  }
  
  # determine which data is new ----
  
  # read in previous assessment ss3 datafile
  old_datafilename <- list.files(here::here(cyr, "data"), pattern = "GOAPcod")
  old_datafile <- r4ss::SS_readdat_3.30(here::here(cyr, "data", old_datafilename))
  # read in current assessment ss3 datafile
  new_datafilename <- list.files(dir, pattern = "GOAPcod")
  new_datafile <- r4ss::SS_readdat_3.30(here::here(dir, new_datafilename))
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
  
  
  # run models in parallel ---- 
  
  ## get needed stuff ----
  # get model executable name
  exe_name <- ss3_exename(dir)
  # set up an index vector defining which data is being left-out
  idx = seq(1, length(data_diff$dataset))
  # define the list of loo models
  loo_mdls <- data_diff$dataset
  # define datafile name
  datafilename <- list.files(dir, pattern = "GOAPcod")
  # Get the number of available cores
  num_cores <- parallel::detectCores()
  if(num_cores > length(loo_mdls)) num_cores = length(loo_mdls)
  # Set the number of cores to be used for parallel computing
  doParallel::registerDoParallel(cores = num_cores)
  
  ## prep models ----
  for(i in idx){
    # Write SS files
    r4ss::copy_SS_inputs(dir.old = dir, 
                         dir.new = here::here(dir, "loo", "data", loo_mdls[i]),
                         copy_par = TRUE,
                         copy_exe = TRUE,
                         overwrite = TRUE)
    
    # Change up data file for LOO
    cpue <- data.table::data.table(new_datafile$CPUE)
    agecomp <- data.table::data.table(new_datafile$agecomp)
    lencomp <- data.table::data.table(new_datafile$lencomp)
    # cpue data
    if(data_diff$dataset[i] == 'BTsurv_Indx'){
      cpue[year == data_diff$year[i] & index == 4]$index <- cpue[year == data_diff$year[i] & index == 4]$index * -1
    }
    if(data_diff$dataset[i] == 'LLsurv_Indx'){
      cpue[year == data_diff$year[i] & index == 5]$index <- cpue[year == data_diff$year[i] & index == 5]$index * -1
    }
    # caal data
    if(data_diff$dataset[i] == 'Fish_CAAL_TWL'){
      agecomp[year == data_diff$year[i] & fleet == 1]$fleet <- agecomp[year == data_diff$year[i] & fleet == 1]$fleet * -1
    }
    if(data_diff$dataset[i] == 'Fish_CAAL_LL'){
      agecomp[year == data_diff$year[i] & fleet == 2]$fleet <- agecomp[year == data_diff$year[i] & fleet == 2]$fleet * -1
    }
    if(data_diff$dataset[i] == 'Fish_CAAL_POT'){
      agecomp[year == data_diff$year[i] & fleet == 3]$fleet <- agecomp[year == data_diff$year[i] & fleet == 3]$fleet * -1
    }
    if(data_diff$dataset[i] == 'BTsurv_CAAL'){
      agecomp[year == data_diff$year[i] & fleet == 4]$fleet <- agecomp[year == data_diff$year[i] & fleet == 4]$fleet * -1
    }
    # length comp
    if(data_diff$dataset[i] == 'Fish_LC_TWL'){
      lencomp[year == data_diff$year[i] & fleet == 1]$fleet <- lencomp[year == data_diff$year[i] & fleet == 1]$fleet * -1
    }
    if(data_diff$dataset[i] == 'Fish_LC_LL'){
      lencomp[year == data_diff$year[i] & fleet == 2]$fleet <- lencomp[year == data_diff$year[i] & fleet == 2]$fleet * -1
    }
    if(data_diff$dataset[i] == 'Fish_LC_POT'){
      lencomp[year == data_diff$year[i] & fleet == 3]$fleet <- lencomp[year == data_diff$year[i] & fleet == 3]$fleet * -1
    }
    if(data_diff$dataset[i] == 'BTsurv_LC'){
      lencomp[year == data_diff$year[i] & fleet == 4]$fleet <- lencomp[year == data_diff$year[i] & fleet == 4]$fleet * -1
    }
    if(data_diff$dataset[i] == 'LLsurv_LC'){
      lencomp[year == data_diff$year[i] & fleet == 5]$fleet <- lencomp[year == data_diff$year[i] & fleet == 5]$fleet * -1
    }
    
    datafile2 <- new_datafile
    datafile2$CPUE <- data.frame(cpue)
    datafile2$agecomp <- data.frame(agecomp)
    datafile2$lencomp <- data.frame(lencomp)
    
    # Write out data script
    r4ss::SS_writedat_3.30(datafile2,
                           here::here(dir, "loo", "data", loo_mdls[i], datafilename),
                           overwrite = TRUE)
  }
  
  ## Run scenarios ----
  foreach::foreach(loo_mdl = loo_mdls) %dopar% {
    
    # Run the scenario
    r4ss::run(dir = here::here(dir, "loo", "data", loo_mdl),
              exe = exe_name,
              skipfinished = FALSE, 
              verbose = FALSE,
              show_in_console = FALSE)
    
  }
  
  # Stop parallel computing
  doParallel::stopImplicitCluster()
  
  
  # Compile output ----
  LOO_mods <- r4ss::SSgetoutput(dirvec = here::here(dir, "loo", "data", loo_mdls))
  
  Nat_M <- array()
  Q <- array()
  SSB_UN <- array()
  annF_Btgt <- array()
  Nat_M_SD <- array()
  Q_SD <- array()
  SSB_UN_SD <- array()
  annF_Btgt_SD <- array()
  SSBfore <- array()
  SSBfore_SD <- array()
  ABCfore <- array()
  ABCfore_SD <- array()
  
  for(i in 1:length(loo_mdls)){
    x <- data.table(LOO_mods[[i]]$parameters)
    y <- data.table(LOO_mods[[i]]$derived_quants)
    annF_Btgt[i] <- y[Label == 'annF_Btgt']$Value
    annF_Btgt_SD[i] <- y[Label == 'annF_Btgt']$StdDev
    Nat_M[i] <- x[Label == "NatM_uniform_Fem_GP_1"]$Value
    Nat_M_SD[i] <- x[Label == "NatM_uniform_Fem_GP_1"]$Parm_StDev
    Q[i] <- x[Label %in% "LnQ_base_Srv(4)"]$Value
    Q_SD[i] <- x[Label %in% "LnQ_base_Srv(4)"]$Parm_StDev
    SSB_UN[i] <- y[Label == 'SSB_unfished']$Value
    SSB_UN_SD[i] <- y[Label == 'SSB_unfished']$StdDev
    SSBfore[i] <- y[Label == paste0('SSB_', cyr + 1)]$Value
    SSBfore_SD[i] <- y[Label == paste0('SSB_', cyr + 1)]$StdDev
    ABCfore[i] <- y[Label == paste0('ForeCatch_', cyr + 1)]$Value
    ABCfore_SD[i] <- y[Label == paste0('ForeCatch_', cyr + 1)]$StdDev
  }
  
  mods0 <- r4ss::SSgetoutput(dirvec = dir)
  
  x0 <- data.table(mods0[[1]]$parameters)
  y0 <- data.table(mods0[[1]]$derived_quants)
  annF_Btgt0 <- y0[Label == 'annF_Btgt']$Value
  annF_Btgt_SD0 <- y0[Label == 'annF_Btgt']$StdDev
  Nat_M0 <- x0[Label == "NatM_uniform_Fem_GP_1"]$Value
  Nat_M_SD0 <- x0[Label == "NatM_uniform_Fem_GP_1"]$Parm_StDev
  Q0 <- x0[Label %in% "LnQ_base_Srv(4)"]$Value
  Q_SD0 <- x0[Label %in% "LnQ_base_Srv(4)"]$Parm_StDev
  SSB_UN0 <- y0[Label == 'SSB_unfished']$Value
  SSB_UN_SD0 <- y0[Label == 'SSB_unfished']$StdDev
  SSBfore0 <- y0[Label == paste0('SSB_', cyr + 1)]$Value
  SSBfore_SD0 <- y0[Label == paste0('SSB_', cyr + 1)]$StdDev
  ABCfore0 <- y0[Label == paste0('ForeCatch_', cyr + 1)]$Value
  ABCfore_SD0 <- y0[Label == paste0('ForeCatch_', cyr + 1)]$StdDev
  
  x0 <- data.table(LOO = 0,
                   Nat_M = Nat_M0, 
                   Nat_M_SD = Nat_M_SD0,
                   annF_Btgt = annF_Btgt0, 
                   annF_Btgt_SD = annF_Btgt_SD0,
                   Q = exp(Q0), 
                   Q_SD = Q_SD0, 
                   SSB_UN = SSB_UN0,
                   SSB_UN_SD = SSB_UN_SD0,
                   SSBfore = SSBfore0,
                   SSBfore_SD = SSBfore_SD0,
                   ABCfore = ABCfore0,
                   ABCfore_SD = ABCfore_SD0)
  
  x1<-data.table(LOO = c(1:length(loo_mdls)),
                 Nat_M = Nat_M, 
                 Nat_M_SD = Nat_M_SD, 
                 annF_Btgt = annF_Btgt, 
                 annF_Btgt_SD = annF_Btgt_SD,
                 Q = exp(Q), 
                 Q_SD = Q_SD, 
                 SSB_UN = SSB_UN,
                 SSB_UN_SD = SSB_UN_SD,
                 SSBfore = SSBfore,
                 SSBfore_SD = SSBfore_SD,
                 ABCfore = ABCfore,
                 ABCfore_SD = ABCfore_SD)
  
  x <- rbind(x0, x1)
  x2 <- data.table(melt(x, "LOO"))
  x3 <- x2[!variable %like% "_SD"]
  x4 <- x2[variable %like% "_SD"]
  x3$SD <- x4$value
  x3$Data <- rep(c('Base', loo_mdls), times = 6)
  
  ## Plot LOO analysis ----
  d <- ggplot(x3[LOO != 0],
              aes(x = Data, y = value)) +
    geom_errorbar(aes(ymin = value - 1.96 * SD, ymax = value + 1.96 * SD), width = 0.25) +
    geom_point(size = 3) +
    geom_hline(data = x3[LOO == 0],
               aes(yintercept = value), size = 1.25, linetype = 2, color = "red") +
    theme_bw(base_size = 16) +
    labs(x = 'Leave one out data', y = 'Parameter value') +
    theme(axis.text.x = element_text(vjust = 0.5, angle = 90)) +
    facet_wrap( ~ variable, scales = "free_y", ncol = 2)
  
  output <- list(d)
  
  return(output)
  
}

