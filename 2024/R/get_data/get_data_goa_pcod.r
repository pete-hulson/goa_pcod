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
#' @param fsh_start_yr beginning year for fishery (default = 1977)
#' @param database switch for which database to pull fishery length data from (default = 'akfin')
#' @param twl_srvy gap survey id number (default = 47 for goa)
#' @param srv_sp gap species code (default = 21720)
#' @param srv_area survey area (default = 'goa')
#' @param indx get survey indices for either numbers 'num', or biomass 'biom' (default = 'num')
#' @param run_glm switch for whether to run delta glm model for adf&g index (default = FALSE)
#' @param len_bins length bins for length comps (default = NULL)
#' @param ss3_frmt whether to format comp data for ss3 data file (default = TRUE)
#' @param max_age maximum age for age comps (default = 10)
#' 
#' @return
#' @export get_data_goa_pcod
#' 
#' @examples
#' 

get_data_goa_pcod <- function(new_data = new_data,
                              new_file = "blarYYYY.dat",
                              new_year = 9999,
                              query = TRUE,
                              fsh_sp = "PCOD",
                              fsh_sp_code = 202,
                              fsh_subarea = c("CG","PWSI","SE","SEI","WG","WY"),
                              fsh_start_yr = 1977,
                              database = 'akfin',
                              twl_srvy = 47,
                              srv_sp = 21720,
                              srv_area = 'goa',
                              indx = 'num',
                              run_glm = FALSE,
                              len_bins = NULL,
                              ss3_frmt = TRUE,
                              max_age = 10) {
  
  new_data$sourcefile <- new_file
  new_data$endyr <- new_year
  
  # query data ----
  
  if(isTRUE(query)){
    query_goa_pcod(new_year,
                   fsh_sp,
                   fsh_sp_code,
                   fsh_subarea,
                   twl_srvy,
                   srv_sp,
                   srv_area,
                   database)
    cat(crayon::green$bold("\u2713"), crayon::blue("data query"), crayon::green$underline$bold$italic("DONE"), "\n")
  }
  
  # get catch ----
  
  ss3_catch <- get_catch_goa_pcod(new_year)
  
  # put into ss3 data file
  new_data$N_catch <- nrow(ss3_catch)
  new_data$catch <- ss3_catch

  cat(crayon::green$bold("\u2713"), crayon::blue("catch data"), crayon::green$underline$bold$italic("DONE"), "\n")

  # get survey indices ----
  
  ## afsc bottom trawl survey ----
  ss3_twl_indx <- get_twl_srvy_index(new_year,
                                     indx = indx)
  cat(crayon::green$bold("\u2713"), crayon::blue("trawl survey index data"), crayon::green$underline$bold$italic("DONE"), "\n")
  
  ## afsc longline survey ----
  ss3_ll_indx <- get_ll_srvy_index(new_year,
                                   indx)
  cat(crayon::green$bold("\u2713"), crayon::blue("longline survey index data"), crayon::green$underline$bold$italic("DONE"), "\n")

  ## iphc longline survey ----
  ss3_iphc_indx <- get_iphc_srvy_index(new_year)
  cat(crayon::green$bold("\u2713"), crayon::blue("iphc survey index data"), crayon::green$underline$bold$italic("DONE"), "\n")
  
  ## adf&g trawl survey ----
  ss3_adfg_indx <- get_adfg_srvy_index(new_year,
                                       run_glm)
  cat(crayon::green$bold("\u2713"), crayon::blue("adf&g survey index data"), crayon::green$underline$bold$italic("DONE"), "\n")
  
  ## larval indices ----
  # note: for time-being, these are entered by hand from emailed data
  ss3_larval_indx <- vroom::vroom(here::here(new_year, 'data', 'raw', 'larval_indices.csv'))
  
  ## plop into ss3 data file ----
  cpue <- ss3_twl_indx %>% 
    tidytable::bind_rows(ss3_ll_indx) %>% 
    tidytable::bind_rows(ss3_iphc_indx) %>% 
    tidytable::bind_rows(ss3_adfg_indx) %>% 
    tidytable::bind_rows(ss3_larval_indx)
  
  new_data$N_cpue <- nrow(cpue)
  new_data$CPUE <- cpue
  cat(crayon::green$bold("\u2713"), crayon::blue("ss3 survey index data"), crayon::green$underline$bold$italic("DONE"), "\n")

  # get length composition data ----

  ## afsc bottom trawl survey ----
  ss3_twl_lcomp <- get_twl_srvy_lcomp(new_year,
                                      len_bins,
                                      ss3_frmt,
                                      iss = FALSE,
                                      nsamp = 100)
  cat(crayon::green$bold("\u2713"), crayon::blue("trawl survey length comp data"), crayon::green$underline$bold$italic("DONE"), "\n")
  
  ## afsc longline survey ----
  ss3_ll_lcomp <- get_ll_srvy_lcomp(new_year,
                                    len_bins,
                                    ss3_frmt,
                                    iss = FALSE,
                                    nsamp = 100)
  cat(crayon::green$bold("\u2713"), crayon::blue("longline survey length comp data"), crayon::green$underline$bold$italic("DONE"), "\n")
  
  ## fishery ----
  
  ### pre-1991 ----
  pre_fsh_lcomp <- get_fsh_len_pre91(new_year,
                                     bins = len_bins)
  cat(crayon::green$bold("\u2713"), crayon::blue("pre-1991 fishery length comp data"), crayon::green$underline$bold$italic("DONE"), "\n")
  
  ### post-1991 ----
  post_fsh_lcomp <- get_fsh_len_post91(new_year,
                                       fltr = TRUE,
                                       bins = len_bins)
  cat(crayon::green$bold("\u2713"), crayon::blue("post-1991 fishery length comp data"), crayon::green$underline$bold$italic("DONE"), "\n")
  
  pre_fsh_lcomp %>% 
    tidytable::bind_rows(post_fsh_lcomp) %>% 
    tidytable::arrange(fltsrv) -> ss3_fsh_lcomp

  ## plop into ss3 data file ----
  lcomp <- ss3_fsh_lcomp %>% 
    tidytable::bind_rows(ss3_twl_lcomp) %>% 
    tidytable::bind_rows(ss3_ll_lcomp)
  
  new_data$N_lencomp <- nrow(lcomp)
  new_data$lencomp <- lcomp
  cat(crayon::green$bold("\u2713"), crayon::blue("ss3 length comp data"), crayon::green$underline$bold$italic("DONE"), "\n")

  # get age composition data ----

  ## afsc bottom trawl survey -----
  ss3_twl_acomp <- get_twl_srvy_acomp(new_year)
  cat(crayon::green$bold("\u2713"), crayon::blue("trawl survey age comp data"), crayon::green$underline$bold$italic("DONE"), "\n")

  ## fishery -----
  ss3_fsh_acomp <- get_fsh_age(new_year)
  cat(crayon::green$bold("\u2713"), crayon::blue("fishery age comp data"), crayon::green$underline$bold$italic("DONE"), "\n")

  
  # get conditional age-at-length data ----
  
  
  ## ----- Get trawl survey conditional age-length data -----
  
  svr_cond_al <- cond_length_age_cor(species = srv_sp_str,
                                     area = sp_area,
                                     start_year = fsh_start_yr,
                                     max_age1 = max_age,
                                     len_bins = len_bins,
                                     new_year)
  cond_age_length <- data.frame(svr_cond_al$norm)
  names(cond_age_length) <- names(GOA_ACOMP)
  print("Conditional survey age length done")      
  
  ## ----- Get fishery conditional age-length data -----
  
  fish_cond_al <- cond_length_age_corFISH(species = fsh_sp_str,
                                          area = sp_area,
                                          start_year = fsh_start_yr,
                                          max_age1 = max_age,
                                          len_bins = len_bins,
                                          new_year)
  cond_age_lengthFISH <- data.frame(fish_cond_al$norm)
  
  ## negating the older fish ages from the file
  cond_age_lengthFISH <- data.table(cond_age_lengthFISH)
  cond_age_lengthFISH[X1 < 2007]$X3 = cond_age_lengthFISH[X1 < 2007]$X3 * -1
  cond_age_lengthFISH <- data.frame(cond_age_lengthFISH)
  names(cond_age_lengthFISH) <- names(GOA_ACOMP)
  print("Conditional fisheries age length done")     
  
  ## combine all the age comp data
  
  ACOMP <- rbind(GOA_ACOMPF,
                 GOA_ACOMP,
                 cond_age_lengthFISH,
                 cond_age_length)
  ACOMP[10:ncol(ACOMP), ] <- round(ACOMP[10:ncol(ACOMP), ], 5)
  
  ## write into SS3 files
  new_data$agecomp<-ACOMP
  new_data$N_agecomp<-nrow(ACOMP)
  
  ## ----- Get trawl survey mean size-at-age data data -----
  
  ## Get all survey Age Data
  
  Age <- GET_SURV_AGE_cor(sp_area = sp_area,
                          srv_sp_str = srv_sp_str,
                          start_yr = srv_start_yr,
                          max_age = max_age,
                          new_year)
  Age$Sur <- 4          #Survey 4 is bottom trawl
  
  
  ## format survey mean size-at-age data for SS3
  AGE_LENGTH_SS <- data.frame(FORMAT_AGE_MEANS1(srv_age_samples = Age,
                                                max_age = max_age,
                                                type = "L",
                                                seas = 1,
                                                flt = -4,
                                                gender = 0,
                                                part = 0))
  
  names(AGE_LENGTH_SS) <- c("Yr", 
                            "Seas", 
                            "FltSrv", 
                            "Gender", 
                            "Part", 
                            "Ageerr", 
                            "Ignore", 
                            paste0("a", rep(seq(1, max_age, 1))), 
                            paste0("N_a", rep(seq(1, max_age, 1))))

  
  ## write into SS3 files
  new_data$MeanSize_at_Age_obs <- AGE_LENGTH_SS
  new_data$N_MeanSize_at_Age_obs <- nrow(AGE_LENGTH_SS)
  print("Mean size at age done")
  
  ## ----- Get ageing error specs -----
  
  ## Add in ageing error specs
  new_data$agebin_vector = seq(1, max_age, 1)
  error <- matrix(ncol = (max_age + 1), nrow = 2)
  error[1, ] <- rep(-1, max_age + 1)
  error[2, ] <- rep(-0.001, max_age + 1)
  new_data$ageerror <- data.frame(error)
  
  ## ----- Add environmental data (look at old Steve function for other indices, this is trimmed down to LL survey q index) -----
  
  TEMPHW <- data.table(TEMPHW)
  x1 <- data.table(Yr = TEMPHW$YR, Variable = 1, Value = TEMPHW$TEMP)
  envdata<-data.frame(x1) # whittling down to LL q link
  new_data$envdat <- envdata
  
  new_data
}
