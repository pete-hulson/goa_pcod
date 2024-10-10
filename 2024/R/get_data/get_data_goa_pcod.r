#' Originally adapted/generalized from Steve Barbeaux's files for generating SS files for EBS/AI Greenland Turbot
#' Modified in 2022 and 2023, and then completely re-developed in 2024 by Pete Hulson
#'  
#' @param new_data ss3 data file for current assessment year to be filled in with updated data
#' @param new_file new ss3 data file name
#' @param new_year current assessment year
#' @param query switch for whether to run sql query and update data (default = TRUE)
#' @param fsh_sp species label for observer/catch data (default = "PCOD")
#' @param fsh_sp_code species code for observer/catch data (default = 202)
#' @param fsh_subarea NPFMC subareas (default to goa subareas c("CG","PWSI","SE","SEI","WG","WY"))
#' @param fsh_age_st_yr beginning year for fishery age comps (default = 2007)
#' @param twl_srvy gap survey id number (default = 47 for goa)
#' @param srv_sp gap species code (default = 21720)
#' @param area assessment region (default = 'goa')
#' @param indx get survey indices for either numbers 'num', or biomass 'biom' (default = 'num')
#' @param run_glm switch for whether to run delta glm model for adf&g index (default = FALSE)
#' @param len_bins user-defined length bins for length comps (default = NULL)
#' @param fltr switch for whether to filter small number of length samples (default = TRUE)
#' @param new_lcomp use old or new method for computing fishery length comps (default = FALSE)
#' @param update_ae switch to update ageing error
#' @param ss3_frmt whether to format comp data for ss3 data file (default = TRUE)
#' @param iss test for whether input sample size comes from surveyISS package (TRUE) or not (FALSE)
#' @param bin_iss value for special case of iss based on bin size (e.g., 'bin2', default = NULL)
#' @param max_age maximum age for age comps (default = 10)
#' 

get_data_goa_pcod <- function(new_data = new_data,
                              new_file = "blarYYYY.dat",
                              new_year = 9999,
                              query = TRUE,
                              fsh_sp = "PCOD",
                              fsh_sp_code = 202,
                              fsh_subarea = c("CG","PWSI","SE","SEI","WG","WY"),
                              fsh_age_st_yr = 2007,
                              twl_srvy = 47,
                              srv_sp = 21720,
                              area = 'goa',
                              indx = 'num',
                              run_glm = FALSE,
                              len_bins = NULL,
                              fltr = TRUE,
                              new_lcomp = FALSE,
                              update_ae = FALSE,
                              ss3_frmt = TRUE,
                              iss = FALSE,
                              bin_iss = NULL,
                              max_age = 10) {
  
  new_data$sourcefile <- new_file
  new_data$endyr <- new_year
  
  # query data ----
  
  if(isTRUE(query)){
    query_goa_pcod(new_year = new_year,
                   fsh_sp = fsh_sp,
                   fsh_sp_code = fsh_sp_code,
                   fsh_subarea = fsh_subarea,
                   twl_srvy = twl_srvy,
                   srv_sp = srv_sp,
                   area = area)
    cat(crayon::green$bold("\u2713"), crayon::blue("data query"), crayon::green$underline$bold$italic("DONE"), "\n")
  }

  # get catch ----
  
  ss3_catch <- get_catch_goa_pcod(new_year = new_year)
  
  # put into ss3 data file
  new_data$N_catch <- nrow(ss3_catch)
  new_data$catch <- ss3_catch

  cat(crayon::green$bold("\u2713"), crayon::blue("catch data"), crayon::green$underline$bold$italic("DONE"), "\n")

  # get survey indices ----
  
  ## afsc bottom trawl survey ----
  ss3_twl_indx <- get_twl_srvy_index(new_year = new_year,
                                     indx = indx)
  cat(crayon::green$bold("\u2713"), crayon::blue("trawl survey index data"), crayon::green$underline$bold$italic("DONE"), "\n")
  
  ## afsc longline survey ----
  ss3_ll_indx <- get_ll_srvy_index(new_year = new_year,
                                   indx = indx)
  cat(crayon::green$bold("\u2713"), crayon::blue("longline survey index data"), crayon::green$underline$bold$italic("DONE"), "\n")

  ## iphc longline survey ----
  ss3_iphc_indx <- get_iphc_srvy_index(new_year = new_year)
  cat(crayon::green$bold("\u2713"), crayon::blue("iphc survey index data"), crayon::green$underline$bold$italic("DONE"), "\n")
  
  ## adf&g trawl survey ----
  ss3_adfg_indx <- get_adfg_srvy_index(new_year = new_year,
                                       run_glm = run_glm)
  cat(crayon::green$bold("\u2713"), crayon::blue("adf&g survey index data"), crayon::green$underline$bold$italic("DONE"), "\n")
  
  ## larval and beach seine indices ----
  # note: for time-being, these are entered by hand from emailed data
  ss3_other_indx <- data.frame(vroom::vroom(here::here(new_year, 'data', 'other_indices.csv')))
  
  ## plop into ss3 data file ----
  cpue <- rbind(ss3_twl_indx,
                ss3_ll_indx,
                ss3_iphc_indx,
                ss3_adfg_indx,
                ss3_other_indx)
  
  new_data$N_cpue <- nrow(cpue)
  new_data$CPUE <- cpue
  cat(crayon::green$bold("\u2713"), crayon::blue("ss3 survey index data"), crayon::green$underline$bold$italic("DONE"), "\n")

  # get length composition data ----

  ## afsc bottom trawl survey ----
  ss3_twl_lcomp <- get_twl_srvy_lcomp(new_year = new_year,
                                      bins = len_bins,
                                      ss3_frmt = ss3_frmt,
                                      iss = iss,
                                      bin_iss = bin_iss)
  cat(crayon::green$bold("\u2713"), crayon::blue("trawl survey length comp data"), crayon::green$underline$bold$italic("DONE"), "\n")
  
  ## afsc longline survey ----
  ss3_ll_lcomp <- get_ll_srvy_lcomp(new_year = new_year,
                                    bins = len_bins,
                                    ss3_frmt = ss3_frmt)
  cat(crayon::green$bold("\u2713"), crayon::blue("longline survey length comp data"), crayon::green$underline$bold$italic("DONE"), "\n")
  
  ## fishery ----
  
  ### pre-1991 ----
  pre_fsh_lcomp <- get_fsh_len_pre91(new_year = new_year,
                                     bins = len_bins,
                                     ss3_frmt = ss3_frmt)
  cat(crayon::green$bold("\u2713"), crayon::blue("pre-1991 fishery length comp data"), crayon::green$underline$bold$italic("DONE"), "\n")
  
  ### post-1991 ----
  if(isTRUE(new_lcomp)){
    post_fsh_lcomp <- get_fsh_len_post91_new(new_year = new_year,
                                             bins = len_bins,
                                             ss3_frmt = ss3_frmt)
  } else{
    post_fsh_lcomp <- get_fsh_len_post91(new_year = new_year,
                                         fltr = fltr,
                                         bins = len_bins,
                                         ss3_frmt = ss3_frmt)
  }

  cat(crayon::green$bold("\u2713"), crayon::blue("post-1991 fishery length comp data"), crayon::green$underline$bold$italic("DONE"), "\n")
  
  data.frame(pre_fsh_lcomp %>% 
               tidytable::bind_rows(post_fsh_lcomp) %>% 
               tidytable::arrange(fltsrv)) -> ss3_fsh_lcomp

  ## plop into ss3 data file ----
  lcomp <- rbind(ss3_fsh_lcomp,
                 ss3_twl_lcomp,
                 ss3_ll_lcomp)
  
  new_data$N_lencomp <- nrow(lcomp)
  new_data$lencomp <- lcomp
  cat(crayon::green$bold("\u2713"), crayon::blue("ss3 length comp data"), crayon::green$underline$bold$italic("DONE"), "\n")

  # get age composition data ----

  ## afsc bottom trawl survey -----
  ss3_twl_acomp <- get_twl_srvy_acomp(new_year = new_year,
                                      max_age = max_age,
                                      ss3_frmt = ss3_frmt,
                                      iss = iss,
                                      bin_iss = bin_iss)
  cat(crayon::green$bold("\u2713"), crayon::blue("trawl survey age comp data"), crayon::green$underline$bold$italic("DONE"), "\n")

  ## fishery -----
  if(isTRUE(new_lcomp)){
    ss3_fsh_acomp <- get_fsh_age_new(new_year = new_year,
                                     st_yr = fsh_age_st_yr,
                                     max_age = max_age,
                                     ss3_frmt = ss3_frmt,
                                     fit = FALSE)
  } else{
  ss3_fsh_acomp <- get_fsh_age(new_year = new_year,
                               st_yr = fsh_age_st_yr,
                               max_age = max_age,
                               fltr = fltr,
                               add_a1 = TRUE,
                               use_FSA = TRUE,
                               iters = 1,
                               by_sex = TRUE,
                               ss3_frmt = ss3_frmt,
                               fit = FALSE)
  }
  
  cat(crayon::green$bold("\u2713"), crayon::blue("fishery age comp data"), crayon::green$underline$bold$italic("DONE"), "\n")

  # get conditional age-at-length data ----

  ## afsc bottom trawl survey -----
  ss3_twl_caal <- get_twl_srvy_caal(new_year = new_year,
                                    max_age = max_age,
                                    bins = len_bins,
                                    ss3_frmt = ss3_frmt,
                                    iss = iss,
                                    bin_iss = bin_iss)
  cat(crayon::green$bold("\u2713"), crayon::blue("trawl survey conditional age-at-length data"), crayon::green$underline$bold$italic("DONE"), "\n")

  ## fishery -----
  ss3_fsh_caal <- get_fsh_caal(new_year = new_year,
                               st_yr = fsh_age_st_yr,
                               max_age = max_age,
                               bins = len_bins,
                               ss3_frmt = ss3_frmt)
  cat(crayon::green$bold("\u2713"), crayon::blue("fishery conditional age-at-length data"), crayon::green$underline$bold$italic("DONE"), "\n")

  ## plop into ss3 data file ----
  acomp <- rbind(ss3_fsh_acomp,
                 ss3_twl_acomp,
                 ss3_fsh_caal,
                 ss3_twl_caal)
  
  new_data$N_agecomp <- nrow(acomp)
  new_data$agecomp <- acomp
  cat(crayon::green$bold("\u2713"), crayon::blue("ss3 age comp data"), crayon::green$underline$bold$italic("DONE"), "\n")

  # get growth data -----
  ss3_grwth <- get_growth(new_year = new_year,
                          type = 'len',
                          max_age = max_age,
                          ss3_frmt = ss3_frmt)

  ## plop into ss3 data file ----
  new_data$MeanSize_at_Age_obs <- ss3_grwth
  new_data$N_MeanSize_at_Age_obs <- nrow(ss3_grwth)
  cat(crayon::green$bold("\u2713"), crayon::blue("growth data"), crayon::green$underline$bold$italic("DONE"), "\n")

  # ageing error ----
  new_data$agebin_vector = seq(1, max_age, 1)
  
  if(isTRUE(update_ae)){
    # update with new reader-tester data and reread data for bias within the dat file
    age_error <- get_agerr(new_year,
                           type = 'dat',
                           max_age)
    new_data$N_ageerror_definitions <- 2
    new_data$ageerror <- age_error
    new_data$agecomp[which(new_data$agecomp$year >= 2007),'ageerr'] = 2
  } else{
    # turn off and read through ctl instead
    age_error <- data.frame(rbind(rep(-1, max_age + 1),
                                  rep(-0.001, max_age) + 1))
    colnames(age_error) <- paste0("age", seq(0, max_age))
    new_data$ageerror <- age_error
  }
  cat(crayon::green$bold("\u2713"), crayon::blue("ageing error"), crayon::green$underline$bold$italic("DONE"), "\n")
  
  # length bins ----
  new_data$maximum_size <- max(len_bins)
  new_data$N_lbins <- length(len_bins)
  new_data$lbin_vector <- len_bins
  
  # environmental data (look at old Steve function for other indices, this is trimmed down to LL survey q index) -----
  ss3_lls_env <- get_lls_env(new_year = new_year,
                             ss3_frmt = ss3_frmt,
                             var = 1)

  ## plop into ss3 data file ----
  new_data$envdat <- ss3_lls_env
  cat(crayon::green$bold("\u2713"), crayon::blue("env link data"), crayon::green$underline$bold$italic("DONE"), "\n")
 
  cat(crayon::green$bold("\u2713"), crayon::blue("ss3 data file"), crayon::green$underline$bold$italic("DONE"), "\n")
  new_data

}
