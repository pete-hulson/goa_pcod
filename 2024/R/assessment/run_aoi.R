
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
  
  # determine which data is new ----
  
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

  
  # run models in parallel ---- 
  
  ## get needed stuff ----
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
  
  ## prep models ----
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
  
  ## Run scenarios ----
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
  
  
  # Compile output ----
  aoi_res <- r4ss::SSgetoutput(dirvec = here::here(dir, "aoi", aoi_mdls[2:length(aoi_mdls)]),
                               verbose = FALSE)
  aoi_res <- r4ss::SSsummarize(aoi_res,
                               verbose = FALSE)
  base_res <- r4ss::SS_output(dir = here::here(dir, "aoi", aoi_mdls[1]),
                              verbose = FALSE,
                              printstats = FALSE)
  
  aoi_res$pars %>% 
    tidytable::filter(Label %in% c("NatM_uniform_Fem_GP_1",
                                   "SR_LN(R0)",
                                   "LnQ_base_Srv(4)")) %>% 
    tidytable::select(-Yr, -recdev) %>% 
    tidytable::rename_with(~aoi_mdls[2:length(aoi_mdls)], paste0("replist", seq(1,length(aoi_mdls[2:length(aoi_mdls)])))) %>% 
    tidytable::pivot_longer(cols = as.character(aoi_mdls[2:length(aoi_mdls)])) %>% 
    tidytable::bind_rows(aoi_res$quants %>% 
                           tidytable::filter(Label %in% c(paste0("ForeCatch_", cyr + 1),
                                                          "annF_Btgt",
                                                          paste0("SSB_", cyr + 1))) %>% 
                           tidytable::select(-Yr) %>% 
                           tidytable::rename_with(~aoi_mdls[2:length(aoi_mdls)], paste0("replist", seq(1,length(aoi_mdls[2:length(aoi_mdls)])))) %>% 
                           tidytable::pivot_longer(cols = as.character(aoi_mdls[2:length(aoi_mdls)]))) %>% 
    tidytable::left_join(aoi_res$parsSD %>% 
                           tidytable::filter(Label %in% c("NatM_uniform_Fem_GP_1",
                                                          "SR_LN(R0)",
                                                          "LnQ_base_Srv(4)")) %>% 
                           tidytable::rename_with(~aoi_mdls[2:length(aoi_mdls)], paste0("replist", seq(1,length(aoi_mdls[2:length(aoi_mdls)])))) %>% 
                           tidytable::pivot_longer(cols = as.character(aoi_mdls[2:length(aoi_mdls)])) %>% 
                           tidytable::bind_rows(aoi_res$quantsSD %>% 
                                                  tidytable::filter(Label %in% c(paste0("ForeCatch_", cyr + 1),
                                                                                 "annF_Btgt",
                                                                                 paste0("SSB_", cyr + 1))) %>% 
                                                  tidytable::select(-Yr) %>% 
                                                  tidytable::rename_with(~aoi_mdls[2:length(aoi_mdls)], paste0("replist", seq(1,length(aoi_mdls[2:length(aoi_mdls)])))) %>% 
                                                  tidytable::pivot_longer(cols = as.character(aoi_mdls[2:length(aoi_mdls)]))) %>% 
                           tidytable::rename(sd = value)) %>% 
    tidytable::left_join(base_res$parameters %>% 
                           tidytable::select(Label, Value) %>% 
                           tidytable::filter(Label %in% c("NatM_uniform_Fem_GP_1",
                                                          "SR_LN(R0)",
                                                          "LnQ_base_Srv(4)")) %>% 
                           tidytable::rename(base = Value) %>% 
                           tidytable::bind_rows(base_res$derived_quants %>% 
                                                  tidytable::select(Label, Value) %>% 
                                                  tidytable::filter(Label %in% c(paste0("ForeCatch_", cyr + 1),
                                                                                 "annF_Btgt",
                                                                                 paste0("SSB_", cyr + 1))) %>% 
                                                  tidytable::rename(base = Value))) %>% 
    tidytable::mutate(par = case_when(Label == "NatM_uniform_Fem_GP_1" ~ "Base Natural Mortality",
                                       Label == "SR_LN(R0)" ~ "log(Mean Recruitment)",
                                       Label == "LnQ_base_Srv(4)" ~ "Bottom trawl survey catchability",
                                       Label == paste0("ForeCatch_", cyr + 1) ~ "One-year forecasted ABC",
                                       Label == "annF_Btgt" ~ "F40%",
                                       Label == paste0("SSB_", cyr + 1) ~ "One-year forecasted Spawning Biomass"),
                      value = case_when(Label == "LnQ_base_Srv(4)" ~ exp(value),
                                        .default = value),
                      sd = case_when(Label == "LnQ_base_Srv(4)" ~ sd* exp(value),
                                     .default = sd),
                      base = case_when(Label == "LnQ_base_Srv(4)" ~ exp(base),
                                       .default = base)) -> aoi_plot_dat

  # plot aoi analysis ----
  aoi_plot <- ggplot(data = aoi_plot_dat,
                     aes(x = name, y = value, col = name)) +
    geom_errorbar(aes(ymin = value - 1.96 * sd, ymax = value + 1.96 * sd), width = 0.25) +
    geom_point(size = 3) +
    geom_hline(aes(yintercept = base), linewidth = 1.25, linetype = 2, color = "black") +
    theme_bw(base_size = 14) +
    facet_wrap( ~ par, 
                scales = "free_y", 
                ncol = 2, 
                labeller = labeller(par = label_wrap_gen(20))) +
    scico::scale_color_scico_d(palette = 'roma') +
    labs(x = 'Dataset', y = 'Parameter value') +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(vjust = 0.5, angle = 90),
          legend.position = "none")

  aoi_plot

  
}

