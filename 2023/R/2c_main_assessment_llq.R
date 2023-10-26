## Script to run 2023 GOA Pacific Cod Assessment (P. Hulson)

# Load required packages & define parameters ----

libs <- c("data.table",
          "dplyr",
          "ggplot2",
          "magrittr", 
          "r4ss",
          "adnuts")

if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )]) > 0) {
  install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE)])}

lapply(libs, library, character.only = TRUE)

# Current model name
Model_name_new <- "2019.1b-2023"

# Current assessment year
new_SS_dat_year <- as.numeric(format(Sys.Date(), format = "%Y"))


# Write SS files in llq subfolder
llq_dir <- here::here(new_SS_dat_year, "mgmt", Model_name_new, "llq")

if (!file.exists(here::here(new_SS_dat_year, "mgmt", Model_name_new, "llq"))) {
  dir.create(here::here(new_SS_dat_year, "mgmt", Model_name_new, "llq"))
  
  r4ss::copy_SS_inputs(dir.old = here::here(new_SS_dat_year, "mgmt", Model_name_new), 
                       dir.new = paste0(llq_dir, "/no_cov"),
                       copy_exe = TRUE,
                       overwrite = TRUE)
  r4ss::copy_SS_inputs(dir.old = here::here(new_SS_dat_year, "mgmt", Model_name_new), 
                       dir.new = paste0(llq_dir, "/rand_cov"),
                       copy_exe = TRUE,
                       overwrite = TRUE)
}

# No llq covariate ----
# note: have to manually turn off covariate in ctl

# switch from reading par to 0 in starter file
nocov_starter <- r4ss::SS_readstarter(file = here::here(new_SS_dat_year, "mgmt", Model_name_new,'llq', 'no_cov', 'starter.ss'))
nocov_starter$init_values_src = 0
r4ss::SS_writestarter(mylist = nocov_starter,
                      dir = here::here(new_SS_dat_year, "mgmt", Model_name_new, 'llq', 'no_cov'),
                      overwrite = TRUE)

# run model
r4ss::run(dir = here::here(new_SS_dat_year, "mgmt", Model_name_new, 'llq', 'no_cov'),
          skipfinished = FALSE,
          show_in_console = TRUE)

# get results
res_run_nocov <- r4ss::SS_output(dir = here::here(new_SS_dat_year, "mgmt", Model_name_new, 'llq', 'no_cov'),
                                   verbose = TRUE,
                                   printstats = FALSE)


# White noise llq covariate ----

# switch from reading par to 0 in starter file
randcov_starter <- r4ss::SS_readstarter(file = here::here(new_SS_dat_year, "mgmt", Model_name_new,'llq', 'rand_cov', 'starter.ss'))
randcov_starter$init_values_src = 0
r4ss::SS_writestarter(mylist = randcov_starter,
                      dir = here::here(new_SS_dat_year, "mgmt", Model_name_new, 'llq', 'rand_cov'),
                      overwrite = TRUE)


# determine number of iterations
iter <- 5 # for testing
# iter <- 100 # for full

# read in ss data
dat <- r4ss::SS_readdat(here::here(new_SS_dat_year, "mgmt", Model_name_new, list.files(here::here(new_SS_dat_year, "mgmt", Model_name_new), pattern = '.dat')))

res_base <- r4ss::SS_output(dir = here::here(new_SS_dat_year, "mgmt", Model_name_new))

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
                         tidytable::mutate(model = "no_cov")) -> likes

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
                         tidytable::mutate(model = "no_cov")) -> pred_ll

# create output folder
if (!file.exists(here::here(new_SS_dat_year, "mgmt", Model_name_new, "llq", "rand_cov", "output"))) 
  dir.create(here::here(new_SS_dat_year, "mgmt", Model_name_new, "llq", "rand_cov", "output"))

for(i in 1:iter){
  
  # generate white noise and write data file
  dat$envdat$Value <- rnorm(length(dat$envdat$Value))
  
  r4ss::SS_writedat(datlist = dat, 
                    outfile = here::here(new_SS_dat_year, "mgmt", Model_name_new, 'llq', 'rand_cov', list.files(here::here(new_SS_dat_year, "mgmt", Model_name_new, 'llq', 'rand_cov'), pattern = '.dat')),
                    overwrite = TRUE)
  
  # run model
  r4ss::run(dir = here::here(new_SS_dat_year, "mgmt", Model_name_new, 'llq', 'rand_cov'),
            skipfinished = FALSE,
            show_in_console = TRUE)
  

  # read the model output and save
  
  res <- r4ss::SS_output(dir = here::here(new_SS_dat_year, "mgmt", Model_name_new, 'llq', 'rand_cov'))
  
  res$likelihoods_used %>% 
    tidytable::mutate(like_compon = rownames(res$likelihoods_used)) %>% 
    tidytable::select(like_compon, values) %>% 
    tidytable::filter(like_compon == "TOTAL") %>% 
    tidytable::bind_rows(res$likelihoods_by_fleet %>% 
                           dplyr::rename_all(tolower) %>% 
                           tidytable::select(label, llsrv) %>% 
                           tidytable::filter(label %in% c("Surv_like", "Length_like")) %>% 
                           tidytable::rename(like_compon = "label",
                                             values = "llsrv")) %>% 
    tidytable::mutate(model = paste0("rand", i))
  
  likes %>% 
    tidytable::bind_rows(res$likelihoods_used %>% 
                           tidytable::mutate(like_compon = rownames(res$likelihoods_used)) %>% 
                           tidytable::select(like_compon, values) %>% 
                           tidytable::filter(like_compon == "TOTAL") %>% 
                           tidytable::bind_rows(res$likelihoods_by_fleet %>% 
                                                  dplyr::rename_all(tolower) %>% 
                                                  tidytable::select(label, llsrv) %>% 
                                                  tidytable::filter(label %in% c("Surv_like", "Length_like")) %>% 
                                                  tidytable::rename(like_compon = "label",
                                                                    values = "llsrv")) %>% 
                           tidytable::mutate(model = paste0("rand", i))) -> likes

  pred_ll %>% 
    tidytable::bind_rows(res$cpue %>% 
                           tidytable::filter(Fleet == 5) %>% 
                           tidytable::select(Yr, Exp) %>% 
                           tidytable::rename(year = 'Yr',
                                             values = 'Exp') %>% 
                           tidytable::mutate(model = paste0("rand", i))) -> pred_ll
  
  
}

# write results
vroom::vroom_write(likes, file = here::here(new_SS_dat_year, "mgmt", "output", "llq_cov_likes.csv"), delim = ",")
vroom::vroom_write(pred_ll, file = here::here(new_SS_dat_year, "mgmt", "output", "llq_cov_pred_ll.csv"), delim = ",")


