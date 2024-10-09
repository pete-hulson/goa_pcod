#' function to get ageing error and bias statistics for goa pcod
#' 
#' @param new_year current assessment year
#' @param type type of input file format (either dat or ctl, default = 'ctl)
#' @param max_age user defined maximum age (default = 10)
#' 
get_agerr <- function(new_year,
                      type = 'ctl',
                      max_age = 10){
  
  # get ageing error stats ----
  ## format reader-tester data ----
  read_test %>% 
    tidytable::filter(species_code == 21720) %>%  
    tidytable::summarise(n = .N, .by = c(age, test_age)) -> r_t
  
  c("Range_of_ages",
    paste(min(c(r_t$age, r_t$test_age)), max(c(r_t$age, r_t$test_age))),
    "Data_set_1",
    paste(nrow(r_t), "# number of lines"),
    "2 # number of readers",
    "0 10 1 # minus group; plus group; reference age",
    "1 2 # which readers",
    "",
    paste(r_t$n, r_t$age, r_t$test_age),
    "-999 -999 -999") %>% 
    writeLines(.,
               here::here(new_year, 'data', 'ageing_error', 'agerr.dat'))
  
  data_ae <- AgeingError:::CreateData(here::here(new_year, 'data', 'ageing_error', 'agerr.dat'), 
                                      NDataSet = 1, 
                                      verbose = FALSE, 
                                      EchoFile = "")
  
  ## format specifications file ----
  # cv by age (1 param)
  c("# reader BiasOpt SigmaOpt",
    "1 0 1",
    "2 0 -1",
    "Sigma_Pars (low high init, on/off)",
    "0.001 5 0.5 1") %>% 
    writeLines(.,
               here::here(new_year, 'data', 'ageing_error', 'agerr.spc'))
  spc_ae <- AgeingError::CreateSpecs(here::here(new_year, 'data', 'ageing_error', 'agerr.spc'), 
                                     DataSpecs = data,
                                     verbose = TRUE)
  
  ## run model ----
  agerr_mod <- AgeingError::DoApplyAgeError(Species = "Pcod",
                                            DataSpecs = data_ae,
                                            ModelSpecsInp = spc_ae,
                                            AprobWght = 1e-06,
                                            SlopeWght = 0.01,
                                            SaveDir = here::here(new_year, 'data', 'ageing_error', 'agerr_res'),
                                            verbose = TRUE)
  agerr_out <- AgeingError::ProcessResults(Species = "Pcod", 
                                           SaveDir = here::here(new_year, 'data', 'ageing_error', 'agerr_res'), 
                                           CalcEff = TRUE, 
                                           verbose = FALSE)
  
  
  # get ageing bias stats ----
  ## format reread data ----
  vroom::vroom(here::here(new_year, 'data', 'raw', 'twl_srvy_age_bias.csv'), delim = ',') %>% 
    tidytable::left_join(vroom::vroom(here::here(new_year, 'data', 'reread_2017.csv'), delim = ',') %>% 
                           tidytable::filter(age > 0) %>% 
                           tidytable::select(vessel, cruise, haul, sex, length, specimen, weight, age) %>% 
                           tidytable::mutate(length = length / 10,
                                             weight = weight / 1000) %>% 
                           tidytable::rename(reread_age = age)) %>% 
    tidytable::drop_na(reread_age) %>% 
    tidytable::summarise(num_reread = .N,
                         .by = c(reread_age, original_age)) %>% 
    tidytable::arrange(original_age) -> reread
  
  c("Range_of_ages",
    paste(min(c(reread$reread_age, reread$original_age)), max(c(reread$reread_age, reread$original_age))),
    "Data_set_1",
    paste(nrow(reread), "# number of lines"),
    "2 # number of readers",
    "0 10 1 # minus group; plus group; reference age",
    "1 2 # which readers",
    "",
    paste(reread$num_reread, reread$original_age, reread$reread_age),
    "-999 -999 -999") %>% 
    writeLines(.,
               here::here(new_year, 'data', 'ageing_error', 'agebias.dat'))
  
  data_bias <- AgeingError:::CreateData(here::here(new_year, 'data', 'ageing_error', 'agebias.dat'), 
                                        NDataSet = 1, 
                                        verbose = FALSE, 
                                        EchoFile = "")
  
  ## format specifications file ----
  c("# reader BiasOpt SigmaOpt",
    "1 1 1",
    "2 0 -1",
    "Bias_Pars (low high init, on/off)",
    "-3 3 0 1", 
    "Sigma_Pars (low high init, on/off)",
    "0.001 5 0.5 1") %>% 
    writeLines(.,
               here::here(new_year, 'data', 'ageing_error', 'agebias.spc'))
  spc_bias <- AgeingError::CreateSpecs(here::here(new_year, 'data', 'ageing_error', 'agebias.spc'), 
                                       DataSpecs = data,
                                       verbose = TRUE)
  
  ## run model ----
  agebias_mod <- AgeingError::DoApplyAgeError(Species = "Pcod",
                                              DataSpecs = data_bias,
                                              ModelSpecsInp = spc_bias,
                                              AprobWght = 1e-06,
                                              SlopeWght = 0.01,
                                              SaveDir = here::here(new_year, 'data', 'ageing_error', 'agebias_res'),
                                              verbose = TRUE)
  agebias_out <- AgeingError::ProcessResults(Species = "Pcod", 
                                             SaveDir = here::here(new_year, 'data', 'ageing_error', 'agebias_res'), 
                                             CalcEff = TRUE, 
                                             verbose = FALSE)
  
  
  # format for ss3 ----
  # for ageing error in the dat file
  if(type == 'dat'){
    rbind(agebias_out$ErrorAndBiasArray[,,1][5, 2:(max_age + 1)],
          agerr_out$ErrorAndBiasArray[,,1][4, 2:(max_age + 1)],
          rep(-1, length.out = max_age),
          agerr_out$ErrorAndBiasArray[,,1][4, 2:(max_age + 1)]) -> ae_mtx
    colnames(ae_mtx) <- paste0("age", seq(1, max_age))
    ae_info <- data.table(ae_mtx)
  } else{
    # for ageing error in the ctl
    ae_info <- list(sd_young = agerr_out$ErrorAndBiasArray[,,1][4, 2],
                    sd_old = agerr_out$ErrorAndBiasArray[,,1][4, max_age + 1],
                    bias_young = agebias_out$ErrorAndBiasArray[,,1][5, 2] - 1.5,
                    bias_old = agebias_out$ErrorAndBiasArray[,,1][5, max_age + 1] - (max_age + 0.5))
  }
  ae_info
}