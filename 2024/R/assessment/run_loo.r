#' Function to perform Leave-One-Out analysis
#' Originally written by Steve Barbeaux, altered by Pete Hulson in 2022, 2023, and 2024
#' 
#' @param dir is the model directory (default = NULL)
#' @param years years over which to run analysis (default = 0:-10)
#' @param cyr current year (default = NULL)
#' 
run_loo <- function(dir = NULL,
                    years = 0:-10,
                    cyr = NULL){
  
  
  # check if the directory for leave-one-out exists, if it doesn't, create it
  if (!dir.exists(here::here(dir, "loo"))) {
    dir.create(here::here(dir, "loo"), recursive = TRUE)
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
  datafile <- r4ss::SS_readdat_3.30(here::here(dir, datafilename),
                                    verbose = FALSE)

  ## prep models ----
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
  
  ## Run scenarios ----
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
  
  # Compile output ----
  loo_res <- r4ss::SSgetoutput(dirvec = here::here(dir, "loo", loo_mdls),
                                verbose = FALSE)
  loo_res <- r4ss::SSsummarize(loo_res,
                               verbose = FALSE)
  base_res <- r4ss::SS_output(dir = dir,
                              verbose = FALSE,
                              printstats = FALSE)

  loo_res$pars %>% 
    tidytable::filter(Label %in% c("NatM_uniform_Fem_GP_1",
                                   "SR_LN(R0)",
                                   "LnQ_base_Srv(4)")) %>% 
    tidytable::select(-Yr, -recdev) %>% 
    tidytable::rename_with(~as.character(cyr + years), paste0("replist", seq(1,length(years)))) %>% 
    tidytable::pivot_longer(cols = as.character(cyr + years)) %>% 
    tidytable::bind_rows(loo_res$quants %>% 
                           tidytable::filter(Label %in% c(paste0("ForeCatch_", cyr + 1),
                                                          "annF_Btgt",
                                                          paste0("SSB_", cyr + 1))) %>% 
                           tidytable::select(-Yr) %>% 
                           tidytable::rename_with(~as.character(cyr + years), paste0("replist", seq(1,length(years)))) %>% 
                           tidytable::pivot_longer(cols = as.character(cyr + years))) %>% 
    tidytable::left_join(loo_res$parsSD %>% 
                           tidytable::filter(Label %in% c("NatM_uniform_Fem_GP_1",
                                                          "SR_LN(R0)",
                                                          "LnQ_base_Srv(4)")) %>% 
                           tidytable::rename_with(~as.character(cyr + years), paste0("replist", seq(1,length(years)))) %>% 
                           tidytable::pivot_longer(cols = as.character(cyr + years)) %>% 
                           tidytable::bind_rows(loo_res$quantsSD %>% 
                                                  tidytable::filter(Label %in% c(paste0("ForeCatch_", cyr + 1),
                                                                                 "annF_Btgt",
                                                                                 paste0("SSB_", cyr + 1))) %>% 
                                                  tidytable::select(-Yr) %>% 
                                                  tidytable::rename_with(~as.character(cyr + years), paste0("replist", seq(1,length(years)))) %>% 
                                                  tidytable::pivot_longer(cols = as.character(cyr + years))) %>% 
                           tidytable::rename(sd = value)) %>% 
    tidytable::mutate(year = as.numeric(name)) %>% 
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
    tidytable::mutate(name = case_when(Label == "NatM_uniform_Fem_GP_1" ~ "Base Natural Mortality",
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
                                        .default = base)) -> loo_plot_dat
  

  # plot loo analysis ----
  loo_plot <- ggplot(data = loo_plot_dat,
         aes(x = year, y = value, col = year)) +
    geom_errorbar(aes(ymin = value - 1.96 * sd, ymax = value + 1.96 * sd), width = 0.25) +
    geom_point(size = 3) +
    geom_hline(aes(yintercept = base), linewidth = 1.25, linetype = 2, color = "black") +
    theme_bw(base_size = 14) +
    facet_wrap( ~ name, 
                scales = "free_y", 
                ncol = 2, 
                labeller = labeller(name = label_wrap_gen(20))) +
    scico::scale_color_scico(palette = 'roma') +
    labs(x = 'Year', y = 'Parameter value') +
    scale_x_continuous(limits = c(min(loo_plot_dat$year) - 0.5, max(loo_plot_dat$year) + 0.5), 
                       breaks = seq(min(loo_plot_dat$year), max(loo_plot_dat$year), by = 1)) +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(vjust = 0.5, angle = 90),
          legend.position = "none")
  
  loo_plot
  
}
