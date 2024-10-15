#' Function to perform ll survey catchability analysis
#' written by by Pete Hulson in 2023, and made intoa  function in 2024
#' 
#' @param dir is the model directory (default = NULL)
#' @param ctl_filename name of model ctl (default = NULL)
#' @param full_run boolean, whether full run should be performed for white noise case (default = NULL)
#' 
llq <- function(dir = NULL,
                years = NULL,
                full_run = NULL){

  # check if the directory for ll q analysis exists, if it doesn't, create it
  if (!dir.exists(here::here(dir, "llq"))) {
    dir.create(here::here(dir, "llq", "no_cov"), recursive = TRUE)
    dir.create(here::here(dir, "llq", "rand_cov"), recursive = TRUE)
  }
  
  # copy ss3 files ----
  # for no covariate
  r4ss::copy_SS_inputs(dir.old = dir, 
                       dir.new = here::here(dir, "llq", "no_cov"),
                       copy_par = TRUE,
                       copy_exe = TRUE,
                       overwrite = TRUE)
  # for random covariate
  r4ss::copy_SS_inputs(dir.old = dir, 
                       dir.new = here::here(dir, "llq", "rand_cov"),
                       copy_par = TRUE,
                       copy_exe = TRUE,
                       overwrite = TRUE)

  
  # run no llq covariate ----
  
  ## turn off llq covariate in ctl ----
  ctl <- r4ss::SS_readctl_3.30(here::here(dir, ctl_filename))
  ctl$Q_parms$`env_var&link` = 0
  ctl$Q_parms_tv = NULL
  r4ss::SS_writectl_3.30(ctllist = ctl,
                         outfile = here::here(dir, "llq", "no_cov",ctl_filename),
                         overwrite = TRUE)

  ## change starter file ----
  # read starter file
  starter <- r4ss::SS_readstarter(here::here(dir, "llq", "no_cov", "starter.ss"))
  # change init vals source
  starter$init_values_src <- 0
  # write modified starter file
  r4ss::SS_writestarter(starter, 
                        dir = here::here(dir, "llq", "no_cov"), 
                        overwrite = TRUE)

  ## run model ----
  r4ss::run(dir = here::here(dir, "llq", "no_cov"),
            skipfinished = FALSE,
            show_in_console = TRUE)

  
  # white noise llq covariate ----
  
  ## change starter file ----
  # read starter file
  starter <- r4ss::SS_readstarter(here::here(dir, "llq", "rand_cov", "starter.ss"))
  # change init vals source
  starter$init_values_src <- 0
  # write modified starter file
  r4ss::SS_writestarter(starter, 
                        dir = here::here(dir, "llq", "rand_cov"), 
                        overwrite = TRUE)
  
  ## read in ss3 data ----
  # define datafile name
  datafilename <- list.files(dir, pattern = "GOAPcod")
  # get datafile input
  datafile <- r4ss::SS_readdat_3.30(here::here(dir, datafilename))

  ## run models ----
  # determine number of iterations
  if(isTRUE(full_run)){
    iters <- 50 # for full
  } else{
    iters <- 5 # for testing
  }
  # run
  rand_res <- purrr::map(1:iters, ~ llq_rand(dat = datafile, 
                                             dir = here::here(dir, "llq", "rand_cov"), 
                                             datafilename = datafilename))
  
  # get results ----

  # read base results
  res_base <- r4ss::SS_output(dir = here::here(dir),
                              verbose = FALSE,
                              printstats = FALSE)
  # read no covariate results
  res_run_nocov <- r4ss::SS_output(dir = here::here(dir, 'llq', 'no_cov'),
                                   verbose = FALSE,
                                   printstats = FALSE)

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
  list(likes = likes, pred_ll = pred_ll)

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
                         overwrite = TRUE)
  # run model
  r4ss::run(dir = dir,
            skipfinished = FALSE,
            show_in_console = TRUE)
  # read the model output and save
  res <- r4ss::SS_output(dir = dir,
                         verbose = FALSE,
                         printstats = FALSE)
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






















