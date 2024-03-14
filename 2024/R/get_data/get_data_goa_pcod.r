# Originally adapted/generalized from Steve Barbeaux' files for generating SS files for EBS/AI Greenland Turbot
# Modified in 2022 and 2023 by Pete Hulson
# Re-developed in 2024 by Pete Hulson
#' @param new_data new data file for current assessment year
#' @param new_file new data file name
#' @param new_year current assessment year
#' @param sp_area assessment region (default = 'GOA')
#' @param fsh_sp_label species label for observer/catch data (default = 'PCOD')
#' @param fsh_sp_area NPFMC subareas (default to goa subareas)
#' @param fsh_sp_str species code for observer/catch data (default = '202')
#' @param fsh_start_yr beginning year for fishery (default = 1977)
#' @param srv_sp_str species code for AFSC surveys (default = 21720)
#' @param twl_srv_st_yr start year for trawl survey (default = 1990)
#' @param ll_srv_st_yr start year for longline survey (default = 1990)
#' @param len_bins length bins for length comps (default = NULL)
#' @param max_age maximum age for age comps (default = 10)
#' @param is_new_datafile switch whether data file is new (default = TRUE)
#' @param update_adfg_iphc switch for whether adf&g and iphc data should be updated (involves running models, default = FALSE)
#' @param catch_table switch to get catch tables for SAFE (move somewhere else)
#' @param aux_fsh_comp switch for whether to include auxiliary state fishery length comp data (default = 3)
#' 
#' @return
#' @export get_data_goa_pcod
#' 
#' @examples
#' 

get_data_goa_pcod <- function(new_data = new_data,
                              new_file = "blarYYYY.dat",
                              new_year = 9999,
                              sp_area = "'GOA'",
                              fsh_sp_label = "PCOD",
                              fsh_sp_area = c("CG","PWSI","SE","SEI","WG","WY"),
                              fsh_sp_str = "202",
                              fsh_start_yr = 1977,
                              srv_sp_str = "21720",
                              twl_srv_start_yr = 1990,
                              ll_srv_start_year = 1990,
                              len_bins = NULL,
                              max_age = 10,
                              is_new_datafile = TRUE,
                              update_adfg_iphc = FALSE,
                              catch_table = FALSE,
                              aux_fsh_comp = 3) {
  
  new_data$sourcefile <- new_file
  new_data$endyr <- new_year
  
  # get catch ----
  
  ss3_catch <- get_catch_goa_pcod(new_year = new_dat_year,
                                  fsh_sp_label = fsh_sp_label,
                                  fsh_sp_area = fsh_sp_area,
                                  query = FALSE)
  
  # put into ss3 data file
  new_data$N_catch <- nrow(ss3_catch)
  new_data$catch <- ss3_catch

  
  
  
  
  
  
  ## ----- Get trawl survey pop'n estimates -----
  
  GOA_BIOM <- GET_GOA_BIOM(srv_sp_str, new_year)
  GOA_BIOM$index <- 4
  
  BIOM <- rbind(GOA_BIOM)
  
  BIOM$CV <- sqrt(BIOM$POPVAR) / BIOM$POP
  BIOM$se_log <- sqrt(log(1.0 + (BIOM$CV^2)))
  
  CPUE <- data.frame(year = BIOM$YEAR, 
                     seas = 7, 
                     index = BIOM$index, 
                     obs = BIOM$POP / 1000, 
                     se_log = BIOM$se_log)
  
  gridc <- expand.grid(year = min(CPUE$year):max(CPUE$year))
  CPUE <- merge(CPUE, gridc, by = "year", all = T)
  CPUE$seas <- 7
  CPUE$index <- 4
  CPUE[is.na(CPUE)] <- 1
  CPUE$index[CPUE$year < 1990] <- -4
  CPUE$index[CPUE$obs == 1] <- -4
  
  ## ----- Get LL survey RPN estimates -----
  
  LLsrv_start_yr <- 1990
  LL_RPN <- GET_GOA_LL_RPN(species = srv_sp_str, FYR = LLsrv_start_yr, new_year)
  LL_RPN <- LL_RPN[year >= LLsrv_start_yr]
  LL_CPUE <- data.frame(year = LL_RPN$year,
                        seas = 7,
                        index = 5,
                        obs = LL_RPN$rpn,
                        se_log = LL_RPN$se / LL_RPN$rpn)
  CPUE <- rbind(CPUE, LL_CPUE)
  
  ## ----- Get other survey index estimates -----
  
  ## ADF&G and IPHC survey files included here

  if(exists("ADFG_IPHC")){ 
    
    # If update to ADF%G and IPHC surveys desired
    if(update_adfg_iphc == TRUE){
      
      # Update IPHC
      IPHC <- sqlQuery(CHINA, query = ("
                select    *
                from      afsc_host.fiss_rpn
                where     species in ('Pacific cod')")) %>% 
        vroom::vroom_write(., here::here(new_year, 'data', 'raw', 'iphc_ll.csv'), delim = ",")
      
      IPHC %>%
        rename_all(tolower) %>%
        filter(fmp_sub_area %in% c("CGOA", "EY/SE", "WGOA", "WY")) %>%
        group_by(survey_year) %>%
        summarise(rpn2 = sum(strata_rpn),
                  cv = sqrt(sum(boot_sd^2)) / sum(strata_rpn)) %>%
        rename(year = survey_year) %>%
        mutate(seas = 7, index = -6) %>%
        select(year, seas, index, rpn2, cv) %>% 
        left_join(filter(ADFG_IPHC, index == -6)) %>% 
        filter(year != 2020) %>% 
        mutate(se_log = replace_na(se_log,mean(se_log, na.rm = TRUE))) %>% # for now new year cv is mean of historical cv's
        select(-obs, -cv) %>% 
        rename(obs = rpn2) -> iphc
      
      # Update ADF&G (model and raw catches not included in github)
      dglm = dget(here::here(new_year, 'data', "delta_glm_1-7-2.get"))
      coddata <- vroom::vroom(here::here(new_year, 'data', 'ADFGsurvcatch.csv'))
      
      coddata %>% 
        filter(district %in% c(1,2,6),
               !is.na(avg_depth_fm),
               !is.na(start_longitude)) %>% 
        mutate(depth = case_when(avg_depth_fm <= 30 ~ 1,
                                 avg_depth_fm > 30 & avg_depth_fm <= 70 ~ 2,
                                 avg_depth_fm > 70 ~ 3),
               density = total_weight_kg / area_km2) %>% 
        select(density, year, district, depth) -> mydata
      
      codout = dglm(mydata, dist = "lognormal", write = F, J = T)
      
      codout$deltaGLM.index %>% 
        mutate(year = as.numeric(rownames(.))) %>% 
        as_tibble(.)  %>% 
        rename(obs = index,
               mean = jack.mean,
               se = jack.se,
               se_log = jack.cv) %>% 
        mutate(seas = 7, index = -7) %>%
        dplyr::select(year, seas, index, obs, se_log) -> adfg
      
      # Update both indices in SS file and data folder
      iphc %>% 
        bind_rows(adfg) -> adfg_iphc 
      vroom::vroom_write(adfg_iphc, here::here(new_year, 'data', 'ADFG_IPHC_updated.csv'), delim = ",")
      names(adfg_iphc) <- names(CPUE)
      CPUE <- rbind(CPUE, iphc, adfg)
    }
    
    # If update to ADF%G and IPHC surveys desired
    if(update_adfg_iphc == FALSE){
      names(ADFG_IPHC) <- names(CPUE)
      CPUE <- rbind(CPUE, ADFG_IPHC)
    }
    
  }else {print("Warning:  no ADFG_IPHC file appears to exist here")}
  
  ## Larval survey indices included here
  if(exists("Larval_indices")){ 
    names(Larval_indices) <- names(CPUE)
    CPUE <- rbind(CPUE, Larval_indices)
  }else {print("Warning:  no larval indices file appears to exist here")}

  ## write to new data file
  new_data$N_cpue<-nrow(CPUE)
  new_data$CPUE<-CPUE
  
  ## ----- Get trawl survey size composition data -----
  
  SRV_LCOMP_SS <- data.frame(GET_GOA_LCOMP1(species = srv_sp_str,
                                            bins = len_bins,
                                            bin = TRUE,
                                            SS = TRUE,
                                            seas = 7,
                                            flt = 4,
                                            gender = 0,
                                            part = 0,
                                            Nsamp = 100,
                                            new_year))
  names(SRV_LCOMP_SS) <- c("Year", "Seas", "FltSrv", "Gender", "Part", "Nsamp", len_bins)
  
  
  ## ----- Get fishery size composition data -----
  
  # If no auxiliary state data
  FISHLCOMP <- data.frame(GET_GOA_LENCOMP2(fsh_sp_str1 = 202, 
                                           len_bins1 = len_bins, 
                                           fsh_start_yr1 = fsh_start_yr, 
                                           new_SS_dat_year1 = new_year, 
                                           seas = 1,
                                           gender = 0,
                                           part = 0,
                                           Nsamp = -1)) 
  names(FISHLCOMP) <- c("Year", "Seas", "FltSrv", "Gender", "Part", "Nsamp", len_bins)
  
  # if state lengths included in length dataset
  if(AUXFCOMP > 0){
    
      auxFLCOMP <- LENGTH_BY_CATCH_GOA(fsh_sp_str = fsh_sp_str,
                                       fsh_sp_label = fsh_sp_label,
                                       ly = new_year)
      if(AUXFCOMP == 1) auxFLCOMP <- auxFLCOMP[[1]]
      if(AUXFCOMP == 2) auxFLCOMP <- auxFLCOMP[[2]]
      if(AUXFCOMP == 3) auxFLCOMP <- auxFLCOMP[[3]]
      
      auxFLCOMP$FltSrv <- 1
      auxFLCOMP$FltSrv[auxFLCOMP$GEAR == "LONGLINE"] <- 2
      auxFLCOMP$FltSrv[auxFLCOMP$GEAR == "POT"] <- 3
      
      auxflCOMP1 = data.frame(Year = auxFLCOMP$YEAR,
                              Seas = rep(1, nrow(auxFLCOMP)),
                              FltSrv = auxFLCOMP$FltSrv,
                              gender = rep(0, nrow(auxFLCOMP)),
                              Part = rep(0, nrow(auxFLCOMP)),
                              Nsamp = auxFLCOMP$Nsamp,
                              auxFLCOMP[ , 4:(ncol(auxFLCOMP) - 1)])
      names(auxflCOMP1) <- c("Year", "Seas", "FltSrv", "Gender", "Part", "Nsamp", len_bins)
      
      fishLCOMP = subset(FISHLCOMP, FISHLCOMP$Year < 1991)
      fishLCOMP <- rbind(fishLCOMP, auxflCOMP1)
      FISHLCOMP <- fishLCOMP[order(fishLCOMP$FltSrv, fishLCOMP$Year), ]
    
    # standardize length comps
    if(sndz_lc == TRUE){
      for(i in 1:length(FISHLCOMP[,1])){
        FISHLCOMP[i,7:length(FISHLCOMP[1,])] <- FISHLCOMP[i,7:length(FISHLCOMP[1,])] / sum(FISHLCOMP[i,7:length(FISHLCOMP[1,])])
      }
    }
  }
  
  print("Fisheries LCOMP2 done")
  
  
  ## ----- Get LL survey size composition data -----
  
  LL_length <- GET_GOA_LL_LENGTH(species = srv_sp_str,
                                 FYR = LLsrv_start_yr,
                                 new_year)
  
  names(LL_length) <- c("year", "length", "FREQ")
  
  LL_LENGTHY <- LL_length[ ,list(TOT = sum(FREQ)), by = "year"]
  LL_LENGTH <- merge(LL_length, LL_LENGTHY, by = "year")
  LL_LENGTH$PROP <- LL_LENGTH$FREQ / LL_LENGTH$TOT
  
  grid <- expand.grid(year = sort(unique(LL_LENGTH$year)), length = seq(0, 116, by = 1))
  LL_LENGTHG <- merge(LL_LENGTH, grid, by = c("year", "length"), all = T)
  LL_LENGTHG$PROP[is.na(LL_LENGTHG$PROP)] <- 0
  
  SS3_LLL <- reshape2::dcast(LL_LENGTHG, formula = year ~ length, value.var = "PROP")
  LL_LENGTH <- data.frame(Year = SS3_LLL$year,
                          Seas = 1,
                          FltSrv = 5,
                          Gender = 0,
                          part = 0,
                          Nsamp = 100)
  LL_LENGTH <- cbind(LL_LENGTH, SS3_LLL[2:ncol(SS3_LLL)])
  names(LL_LENGTH) <- c("Year", "Seas", "FltSrv", "Gender", "Part", "Nsamp", len_bins)
  
  ## combine all the length comp data
  LCOMP <- rbind(FISHLCOMP, SRV_LCOMP_SS, LL_LENGTH)
  LCOMP[7:ncol(LCOMP), ] <- round(LCOMP[7:ncol(LCOMP), ], 5)
  
  ## write into SS3 files
  new_data$lencomp <- LCOMP
  new_data$lencomp$Nsamp[new_data$lencomp$Nsamp >= 200] <- 200
  new_data$N_lencomp <- nrow(LCOMP)
  
  print("All LCOMP done")
  
  
  ## ----- Get trawl survey age composition data -----
  
  GOA_ACOMP <- GET_GOA_ACOMP1(srv_sp_str = srv_sp_str, 
                              max_age = max_age,
                              Seas = 7,
                              FLT = -4,
                              Gender = 0,
                              Part = 0,
                              Ageerr = 1,
                              Lgin_lo = -1,
                              Lgin_hi = -1,
                              Nsamp = 100,
                              new_year)
  print("Survey agecomp done")
  
  ## ----- Get fishery age composition data -----
  
  GOA_ACOMPF <- LENGTH4AGE_BY_CATCH_GOA(fsh_sp_str = 202,
                                        fsh_sp_label = "'PCOD'",
                                        ly = new_year, 
                                        STATE = 3, 
                                        max_age = max_age)
  
  ## Note that these aren't used in the current model so are turned off here...
  GOA_ACOMPF$FltSrv <- GOA_ACOMPF$FltSrv * -1 
  names(GOA_ACOMPF) <- names(GOA_ACOMP)
  print("Fisheries agecomp done")
  
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