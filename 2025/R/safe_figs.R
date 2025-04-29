#' function to produce plots for goa pcod safe
#' 
#' @param new_year current assessment year (default = NULL)
#' @param rec_mdl recommended model name (default = NULL)
#' @param base_mdl base model, i.e., previous model with updated data (default = NULL)
#' @param prev_mdl previous accepted model (default = NULL)
#' 
safe_figs <- function(new_year = NULL,
                      rec_mdl = NULL,
                      base_mdl = NULL,
                      prev_mdl = NULL){
  
  # set up directory to plop plots into
  if (!dir.exists(here::here(new_year, "output", "safe_plots"))) {
    dir.create(here::here(new_year, "output", "safe_plots"), recursive = TRUE)
  }
  
  # read in model output ----
  ## note: will need to adjust this based on what alternative models are being looked at ----
  cat("\u231b", crayon::blue("Gathering model output..."), "\n")
  rec_mdl_res <- r4ss::SS_output(dir = here::here(new_year, "mgmt", rec_mdl),
                                 verbose = FALSE,
                                 printstats = FALSE)
  base_mdl_res <- r4ss::SS_output(dir = here::here(new_year, "mgmt", base_mdl),
                                  verbose = FALSE,
                                  printstats = FALSE)
  prev_mdl_res <- r4ss::SS_output(dir = here::here(new_year - 1, "mgmt", prev_mdl),
                                  verbose = FALSE,
                                  printstats = FALSE)
  res_19_1c <- r4ss::SS_output(dir = here::here(new_year, "mgmt", "19.1c"),
                               verbose = FALSE,
                               printstats = FALSE)
  res_19_1d <- r4ss::SS_output(dir = here::here(new_year, "mgmt", "19.1d"),
                               verbose = FALSE,
                               printstats = FALSE)
  res_19_1e <- r4ss::SS_output(dir = here::here(new_year, "mgmt", "19.1e"),
                               verbose = FALSE,
                               printstats = FALSE)
  res_mdl_comp <- suppressWarnings(r4ss::SSsummarize(biglist = list(prev_mdl_res,
                                                                    base_mdl_res,
                                                                    res_19_1c,
                                                                    res_19_1d,
                                                                    res_19_1e,
                                                                    rec_mdl_res),
                                                     verbose = FALSE))
  res_bin_comp <- r4ss::SSsummarize(biglist = list(res_19_1e,
                                                   rec_mdl_res),
                                    verbose = FALSE)
  
  # run r4ss for recommended model ----
  cat("\u231b", crayon::blue("Working on r4ss plots..."), "\n")
  dir.create(here::here(new_year, "output", "safe_plots", "r4ss"), recursive = TRUE)
  r4ss::SS_plots(rec_mdl_res,
                 printfolder = "",
                 dir = here::here(new_year, "output", "safe_plots", "r4ss"),
                 verbose = FALSE)
  # print message when done
  cat(crayon::green$bold("\u2713"), crayon::blue("r4ss plots"), crayon::green$underline$bold$italic("DONE"), "\n")
  
  # catch by fleet ----
  plot_catch(new_year)
  # print message when done
  cat(crayon::green$bold("\u2713"), crayon::blue("Catch plot"), crayon::green$underline$bold$italic("DONE"), "\n")
  
  # mean length ----
  plot_meanlen(new_year)
  # print message when done
  cat(crayon::green$bold("\u2713"), crayon::blue("Mean length plot"), crayon::green$underline$bold$italic("DONE"), "\n")
  
  # vessel participation ----
  plot_vessnum(new_year)
  # print message when done
  cat(crayon::green$bold("\u2713"), crayon::blue("Number of vessels plot"), crayon::green$underline$bold$italic("DONE"), "\n")
  
  # cumulative catch ----
  plot_cumcatch(new_year)
  # print message when done
  cat(crayon::green$bold("\u2713"), crayon::blue("Cumulative catch plot"), crayon::green$underline$bold$italic("DONE"), "\n")
  
  # auxiliary indices ----
  plot_auxind(new_year)
  # print message when done
  cat(crayon::green$bold("\u2713"), crayon::blue("Auxiliary index plot"), crayon::green$underline$bold$italic("DONE"), "\n")
  
  # plot model data ----
  ## note: pull this out of r4ss ----
  r4ss::SSplotData(rec_mdl_res,
                   subplots = 2,
                   fleetcol = scico::scico(5, palette = 'roma'),
                   plot = FALSE,
                   print = TRUE,
                   pheight = 7,
                   plotdir = here::here(new_year, "output", "safe_plots"))
  invisible(file.rename(from = here::here(new_year, "output", "safe_plots", "data_plot2.png"),
                        to = here::here(new_year, "output", "safe_plots", "data.png")))
  # print message when done
  cat(crayon::green$bold("\u2713"), crayon::blue("Model data plot"), crayon::green$underline$bold$italic("DONE"), "\n")
  
  # temp vs depth ----
  plot_deptem(new_year)
  # print message when done
  cat(crayon::green$bold("\u2713"), crayon::blue("Temp-Depth plot"), crayon::green$underline$bold$italic("DONE"), "\n")
  
  # cfsr index ----
  plot_env(new_year)
  # print message when done
  cat(crayon::green$bold("\u2713"), crayon::blue("Env index plot"), crayon::green$underline$bold$italic("DONE"), "\n")
  
  # ageing error-bias ----
  plot_agerr(new_year)
  # print message when done
  cat(crayon::green$bold("\u2713"), crayon::blue("Ageing error & bias plot"), crayon::green$underline$bold$italic("DONE"), "\n")
  
  # length-weight ----
  plot_lenwt(new_year)
  # print message when done
  cat(crayon::green$bold("\u2713"), crayon::blue("Length-weight plot"), crayon::green$underline$bold$italic("DONE"), "\n")
  
  # ssb and rec comparison ----
  
  # get plot data together
  res_mdl_comp$SpawnBio %>% 
    tidytable::filter(Yr >= 1977,
                      Yr <= new_year) %>% 
    tidytable::select(-Label) %>% 
    tidytable::mutate(model1 = case_when(Yr == new_year ~ NA,
                                         .default = model1)) %>% 
    tidytable::pivot_longer(cols = paste0("model", seq(1, length(colnames(.)) - 1))) %>% 
    tidytable::mutate(name = case_when(name == "model1" ~ "19.1b-23",
                                       name == "model2" ~ "19.1b",
                                       name == "model3" ~ "19.1c",
                                       name == "model4" ~ "19.1d",
                                       name == "model5" ~ "19.1e",
                                       name == "model6" ~ "24.0")) %>% 
    tidytable::mutate(type = "Spawning biomass (t)") %>% 
    tidytable::bind_rows(res_mdl_comp$recruits %>% 
                           tidytable::filter(Yr >= 1977,
                                             Yr <= new_year) %>% 
                           tidytable::select(-Label) %>% 
                           tidytable::mutate(model1 = case_when(Yr == new_year ~ NA,
                                                                .default = model1)) %>% 
                           tidytable::pivot_longer(cols = paste0("model", seq(1, length(colnames(.)) - 1))) %>% 
                           tidytable::mutate(name = case_when(name == "model1" ~ "19.1b-23",
                                                              name == "model2" ~ "19.1b",
                                                              name == "model3" ~ "19.1c",
                                                              name == "model4" ~ "19.1d",
                                                              name == "model5" ~ "19.1e",
                                                              name == "model6" ~ "24.0")) %>% 
                           tidytable::mutate(type = "Age-0 recruitment (1000s)")) %>% 
    tidytable::rename(year = Yr) -> ssb_rec_comp
  # plot
  suppressWarnings(plot_ts_comp(ssb_rec_comp))
  # print message when done
  cat(crayon::green$bold("\u2713"), crayon::blue("SSB & rec comparison plot"), crayon::green$underline$bold$italic("DONE"), "\n")
  
  
  # key parameters comparison ----
  res_mdl_comp$pars %>% 
    tidytable::select(-Yr, -recdev) %>% 
    tidytable::filter(Label %in% c("NatM_uniform_Fem_GP_1",
                                   "L_at_Amax_Fem_GP_1",
                                   "NatM_uniform_Fem_GP_1_BLK4repl_2014",
                                   "SR_LN(R0)",
                                   "LnQ_base_Srv(4)",
                                   "LnQ_base_LLSrv(5)")) %>% 
    tidytable::rename(par = Label) %>% 
    tidytable::mutate(par = case_when(par == "NatM_uniform_Fem_GP_1" ~ "M",
                                      par == "L_at_Amax_Fem_GP_1" ~ "Linf",
                                      par == "NatM_uniform_Fem_GP_1_BLK4repl_2014" ~ "M (2014-2016)",
                                      par == "SR_LN(R0)" ~ "R0",
                                      par == "LnQ_base_Srv(4)" ~ "q (Trawl survey)",
                                      par == "LnQ_base_LLSrv(5)" ~ "q (Longline survey)")) %>% 
    tidytable::pivot_longer(cols = paste0("model", seq(1, length(colnames(.)) - 1))) %>% 
    tidytable::mutate(model = case_when(name == "model1" ~ "19.1b-23",
                                        name == "model2" ~ "19.1b",
                                        name == "model3" ~ "19.1c",
                                        name == "model4" ~ "19.1d",
                                        name == "model5" ~ "19.1e",
                                        name == "model6" ~ "24.0"),
                      value = case_when(par %in% c("q (Trawl survey)", "q (Longline survey)") ~ exp(value),
                                        .default = value)) %>% 
    tidytable::left_join(res_mdl_comp$parsSD %>% 
                           tidytable::filter(Label %in% c("NatM_uniform_Fem_GP_1",
                                                          "L_at_Amax_Fem_GP_1",
                                                          "NatM_uniform_Fem_GP_1_BLK4repl_2014",
                                                          "SR_LN(R0)",
                                                          "LnQ_base_Srv(4)",
                                                          "LnQ_base_LLSrv(5)")) %>% 
                           tidytable::rename(par = Label) %>% 
                           tidytable::mutate(par = case_when(par == "NatM_uniform_Fem_GP_1" ~ "M",
                                                             par == "L_at_Amax_Fem_GP_1" ~ "Linf",
                                                             par == "NatM_uniform_Fem_GP_1_BLK4repl_2014" ~ "M (2014-2016)",
                                                             par == "SR_LN(R0)" ~ "R0",
                                                             par == "LnQ_base_Srv(4)" ~ "q (Trawl survey)",
                                                             par == "LnQ_base_LLSrv(5)" ~ "q (Longline survey)")) %>% 
                           tidytable::pivot_longer(cols = paste0("model", seq(1, length(colnames(.)) - 1))) %>% 
                           tidytable::mutate(model = case_when(name == "model1" ~ "19.1b-23",
                                                               name == "model2" ~ "19.1b",
                                                               name == "model3" ~ "19.1c",
                                                               name == "model4" ~ "19.1d",
                                                               name == "model5" ~ "19.1e",
                                                               name == "model6" ~ "24.0")) %>% 
                           tidytable::rename(sd = value)) %>% 
    tidytable::select(-name) %>% 
    tidytable::mutate(sd = case_when(par %in% c("q (Trawl survey)", "q (Longline survey)") ~ sd * value,
                                     .default = sd)) -> par_comp
  # plot
  plot_par_comp(par_comp)
  # print message when done
  cat(crayon::green$bold("\u2713"), crayon::blue("Parameter comparison plot"), crayon::green$underline$bold$italic("DONE"), "\n")

  # curr year selex comparison ----
  res_bin_comp$sizesel %>% 
    dplyr::rename_all(tolower) %>% 
    tidytable::filter(yr == new_year,
                      fleet <= 5,
                      factor == "Lsel") %>% 
    tidytable::select(-factor, -yr, -sex, -label, -imodel) %>% 
    tidytable::rename(model = name) %>% 
    tidytable::mutate(model = case_when(model == "model1" ~ "19.1e",
                                        model == "model2" ~ "24.0")) %>% 
    tidytable::pivot_longer(cols = as.character(seq(1, 105))) %>% 
    tidytable::rename(length = name) %>% 
    tidytable::mutate(fleet = case_when(fleet == 1 ~ "Trawl fishery",
                                        fleet == 2 ~ "Longline fishery",
                                        fleet == 3 ~ "Pot fishery",
                                        fleet == 4 ~ "AFSC bottom trawl survey",
                                        fleet == 5 ~ "AFSC longline survey"),
                      length = as.numeric(length)) %>% 
    filter(length >= 5) -> selex_comp
  # plot
  plot_selex_comp(selex_comp)
  # print message when done
  cat(crayon::green$bold("\u2713"), crayon::blue("Selex comparison plot"), crayon::green$underline$bold$italic("DONE"), "\n")
  
  # retrospectives ----
  plot_retro(rec_mdl_res, new_year)
  # print message when done
  cat(crayon::green$bold("\u2713"), crayon::blue("Retrospective plot"), crayon::green$underline$bold$italic("DONE"), "\n")
  
  # leave-one-out ----
  plot_loo(rec_mdl_res, new_year)
  # print message when done
  cat(crayon::green$bold("\u2713"), crayon::blue("Leave-one-out plot"), crayon::green$underline$bold$italic("DONE"), "\n")
  
  # add-one-in ----
  plot_aoi(new_year)
  # print message when done
  cat(crayon::green$bold("\u2713"), crayon::blue("Add-one-in plot"), crayon::green$underline$bold$italic("DONE"), "\n")
  
  # ll survey sensitivities ----
  ## note: need to figure out what's up with no_cov ----
  # plot_llq(new_year)
  # # print message when done
  # cat(crayon::green$bold("\u2713"), crayon::blue("Longline srv q plot"), crayon::green$underline$bold$italic("DONE"), "\n")
  # 
  # profiles ----
  plot_profile(new_year)
  # print message when done
  cat(crayon::green$bold("\u2713"), crayon::blue("Profile plot"), crayon::green$underline$bold$italic("DONE"), "\n")
  
  # fitted survey indices ----
  plot_indxfit(rec_mdl_res)
  # print message when done
  cat(crayon::green$bold("\u2713"), crayon::blue("Index fit plot"), crayon::green$underline$bold$italic("DONE"), "\n")
  
  # osa resids ----
  # fishery osa
  plot_fsh_osa(rec_mdl_res, new_year)
  # survey osa
  plot_srv_osa(rec_mdl_res, new_year)
  # print message when done
  cat(crayon::green$bold("\u2713"), crayon::blue("One-step-ahead residual plots"), crayon::green$underline$bold$italic("DONE"), "\n")
  
  # pearson resids ----
  plot_pearson(rec_mdl_res, new_year)
  # print message when done
  cat(crayon::green$bold("\u2713"), crayon::blue("Pearson residual plot"), crayon::green$underline$bold$italic("DONE"), "\n")
  
  # growth fits ----
  suppressWarnings(plot_grwth(rec_mdl_res, new_year))
  # print message when done
  cat(crayon::green$bold("\u2713"), crayon::blue("Growth fit plot"), crayon::green$underline$bold$italic("DONE"), "\n")
  
  # time-dep selex ----
  plot_td_selex(rec_mdl_res, new_year)
  # print message when done
  cat(crayon::green$bold("\u2713"), crayon::blue("Time-dep selex plot"), crayon::green$underline$bold$italic("DONE"), "\n")
  
  # current year selex ----
  plot_curr_selex(rec_mdl_res, new_year)
  # print message when done
  cat(crayon::green$bold("\u2713"), crayon::blue("Current year selex plot"), crayon::green$underline$bold$italic("DONE"), "\n")
  
  # tot bio & ssb ----
  plot_tot_ssb(rec_mdl_res, new_year)
  # print message when done
  cat(crayon::green$bold("\u2713"), crayon::blue("Total & SSB plot"), crayon::green$underline$bold$italic("DONE"), "\n")
  
  # rec & devs ----
  plot_rec(rec_mdl_res, new_year)
  # print message when done
  cat(crayon::green$bold("\u2713"), crayon::blue("Recruitment & devs plot"), crayon::green$underline$bold$italic("DONE"), "\n")
  
  # f combined & gear ----
  plot_f(rec_mdl_res, new_year)
  # print message when done
  cat(crayon::green$bold("\u2713"), crayon::blue("F estimates plot"), crayon::green$underline$bold$italic("DONE"), "\n")
  
  # phase-plane ----
  plot_phspln(rec_mdl_res, new_year)
  # print message when done
  cat(crayon::green$bold("\u2713"), crayon::blue("Phase-plane plot"), crayon::green$underline$bold$italic("DONE"), "\n")
  
  # mcmc adnuts pairs ----
  plot_pairs(new_year)
  # print message when done
  cat(crayon::green$bold("\u2713"), crayon::blue("MCMC pairs plot"), crayon::green$underline$bold$italic("DONE"), "\n")
  
  # mcmc histogram for key parameters ----
  plot_hists(rec_mdl_res, new_year)
  # print message when done
  cat(crayon::green$bold("\u2713"), crayon::blue("MCMC histogram plot"), crayon::green$underline$bold$italic("DONE"), "\n")
  
  # apportionment ----
  suppressWarnings(plot_apport(new_year))
  # print message when done
  cat(crayon::green$bold("\u2713"), crayon::blue("Apportionment plot"), crayon::green$underline$bold$italic("DONE"), "\n")
  
}