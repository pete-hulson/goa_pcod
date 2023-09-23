# adapted/generalized from Steve Barbeaux' files for
# generating SS files for EBS/AI Greenland Turbot
# ZTA, 2013-05-08, R version 2.15.1, 32-bit
# Adapted in 2022 by Pete Hulson

# this function gets data for one region at a time
# currently all functions are hardcoded for 4 fishing fleets and 1 survey -> 5 total

# new_data           = new_data
#                               new_file           = new_SS_dat_filename
#                               new_year           = new_SS_dat_year
#                               sp_area            = sp_area
#                               fsh_sp_label       = fsh_sp_label
#                               fsh_sp_area        = fsh_sp_area
#                               fsh_sp_str         = fsh_sp_str
#                               fsh_start_yr       = fsh_start_yr
#                               srv_sp_str         = srv_sp_str
#                               srv_start_yr       = srv_start_yr
#                               len_bins           = len_bins
#                               max_age            = max_age
#                               is_new_SS_DAT_file = is_new_SS_DAT_file
#                               AUXFCOMP           = AUXFCOMP



SBSS_GET_ALL_DATA <- function(new_data = new_data,
                              new_file = "blarYYYY.dat",
                              new_year = 9999,
                              sp_area = "'foo'",
                              fsh_sp_label = "'foo'",
                              fsh_sp_area = "'foo'",
                              fsh_sp_str = "999",
                              fsh_start_yr = 1977,
                              srv_sp_str = "99999",
                              srv_start_yr = 1977,
                              new_SS_dat_year = new_SS_dat_year,
                              len_bins = seq(4,109,3),
                              max_age = 10,
                              is_new_SS_DAT_file = FALSE,
                              update_adfg_iphc = FALSE,
                              inc_ADFG = FALSE,
                              sndz_lc = FALSE,
                              catch_table = FALSE,
                              AUXFCOMP = 3)
{
  
  new_data$sourcefile <- new_file
  new_data$endyr <- new_year
  
  ## ----- get REGION catch -----
  
  test <- paste("SELECT SUM(COUNCIL.COMPREHENSIVE_BLEND_CA.WEIGHT_POSTED)AS TONS,\n ",
                "COUNCIL.COMPREHENSIVE_BLEND_CA.FMP_SUBAREA AS ZONE,\n ",
                "COUNCIL.COMPREHENSIVE_BLEND_CA.FMP_GEAR AS GEAR,\n ",
                "COUNCIL.COMPREHENSIVE_BLEND_CA.RETAINED_OR_DISCARDED AS TYPE,\n ",
                "COUNCIL.COMPREHENSIVE_BLEND_CA.YEAR AS YEAR,\n ",
                "TO_CHAR(COUNCIL.COMPREHENSIVE_BLEND_CA.WEEK_END_DATE,'MM') AS MONTH, \n",
                "COUNCIL.COMPREHENSIVE_BLEND_CA.SPECIES_GROUP_CODE AS SPECIES_GROUP\n ",
                "FROM COUNCIL.COMPREHENSIVE_BLEND_CA\n ",
                "WHERE COUNCIL.COMPREHENSIVE_BLEND_CA.FMP_SUBAREA in (",fsh_sp_area,")\n ",
                "AND COUNCIL.COMPREHENSIVE_BLEND_CA.YEAR <= ",new_year,"\n ",
                "AND COUNCIL.COMPREHENSIVE_BLEND_CA.SPECIES_GROUP_CODE in (",fsh_sp_label,")\n ",
                "GROUP BY COUNCIL.COMPREHENSIVE_BLEND_CA.SPECIES_GROUP_CODE,\n ",
                "COUNCIL.COMPREHENSIVE_BLEND_CA.FMP_SUBAREA,\n ",
                "COUNCIL.COMPREHENSIVE_BLEND_CA.FMP_GEAR,\n ",
                "COUNCIL.COMPREHENSIVE_BLEND_CA.RETAINED_OR_DISCARDED,\n ",
                "COUNCIL.COMPREHENSIVE_BLEND_CA.YEAR,\n ", 
                "TO_CHAR(COUNCIL.COMPREHENSIVE_BLEND_CA.WEEK_END_DATE,'MM')", sep="")
  
  
  CATCH <- sqlQuery(CHINA, test)
  
  CATCH$GEAR1 <- "TRAWL"
  
  CATCH$GEAR1[CATCH$GEAR == "POT"] <- "POT"
  CATCH$GEAR1[CATCH$GEAR == "HAL"] <- "LONGLINE"
  CATCH$GEAR1[CATCH$GEAR == "JIG"] <- "LONGLINE"
  
  CATCH$SEASON <- 1
  CATCH$SEASON[CATCH$MONTH >= 3] <- 2
  CATCH$SEASON[CATCH$MONTH >= 5] <- 3
  CATCH$SEASON[CATCH$MONTH >= 9] <- 4
  CATCH$SEASON[CATCH$MONTH >= 11] <- 5
  
  CATCH_GEAR <- aggregate(list(TONS = CATCH$TONS), by = list(YEAR = CATCH$YEAR, GEAR = CATCH$GEAR1), FUN = sum)
  
  ## get the old catch data that isn't in the catch accounting system...
  if(exists("OLD_SEAS_GEAR_CATCH")){
    OLD_SEAS_GEAR_CATCH$GEAR1 <- "TRAWL"
    OLD_SEAS_GEAR_CATCH$GEAR1[OLD_SEAS_GEAR_CATCH$GEAR == "POT"] <- "POT"
    OLD_SEAS_GEAR_CATCH$GEAR1[OLD_SEAS_GEAR_CATCH$GEAR == "LONGLINE"] <- "LONGLINE"
    OLD_SEAS_GEAR_CATCH$GEAR1[OLD_SEAS_GEAR_CATCH$GEAR == "OTHER"] <- "LONGLINE"
    OLD_GEAR_CATCH <- aggregate(list(TONS = OLD_SEAS_GEAR_CATCH$TONS),
                                by = list(YEAR = OLD_SEAS_GEAR_CATCH$YEAR, GEAR = OLD_SEAS_GEAR_CATCH$GEAR1),
                                FUN = sum)
    CATCH2 <- rbind(OLD_GEAR_CATCH, CATCH_GEAR)
    CATCH2 <- CATCH2[order(CATCH2$GEAR, CATCH2$YEAR), ]
  }else{
    print("Warning: No old catch information provided")
    CATCH2 <- CATCH_GEAR
  }
  
  CATCH2 <- CATCH2[order(CATCH2$GEAR, CATCH2$YEAR), ]
  
  ## check that all gears have the same number of years covered; add zeros if necessary
  c_y <- sort(unique(CATCH2$YEAR))
  grid <- expand.grid(YEAR = c(min(c_y):max(c_y)))
  CATCH2 <- merge(CATCH2, grid, all = T)
  CATCH2$TONS[is.na(CATCH2$TONS)] <- 0.0
  
  ## add 97 - 02 adf&g catch
  if(inc_ADFG == TRUE){
    
    test <- paste0("SELECT akfin_year,\n ",
                   "FMP_AREA,\n ",
                   "adfg_i_harvest_code,\n ",
                   "fmp_gear,\n ",
                   "SUM (cfec_whole_pounds / 2204.622) AS CATCH_MT\n ",
                   "FROM council.comprehensive_Ft\n ",
                   "WHERE akfin_year BETWEEN 1997 AND 2002\n ",
                   "AND adfg_i_species_code = 110\n ",
                   "AND adfg_i_harvest_code = 80\n ",
                   "AND fmp_area = 'GOA'\n ",
                   "GROUP BY adfg_i_harvest_code,\n ",
                   "FMP_AREA,\n ",
                   "fmp_gear,\n ",
                   "akfin_year")
    
    ADFG_CATCH <- sqlQuery(CHINA, test)
    
    ADFG_CATCH %>% 
      group_by(AKFIN_YEAR, FMP_GEAR) %>% 
      mutate(GEAR = if(FMP_GEAR %in% c("JIG", "HAL")){"LONGLINE"}else{"POT"},
             YEAR = AKFIN_YEAR) %>% 
      group_by(YEAR, GEAR) %>% 
      summarise(CATCH = sum(CATCH_MT)) -> ADFG_CATCH2
    
    CATCH2 %>% 
      filter(YEAR %in% seq(1997,2002) & GEAR != 'TRAWL') %>% 
      group_by(YEAR, GEAR) %>% 
      left_join(ADFG_CATCH2) %>% 
      mutate(TOTAL = TONS + CATCH) %>% 
      select(YEAR, GEAR, TOTAL) %>% 
      rename(TONS = TOTAL) -> CATCH2wADFG
    
    CATCH2 %>% 
      filter(!(YEAR%in% seq(1997,2002) & GEAR != 'TRAWL')) %>% 
      bind_rows(CATCH2wADFG) -> CATCH2
    
  }
  
  ## sort CATCH by gear and year
  CATCH <- CATCH2[order(CATCH2$GEAR,CATCH2$YEAR), ]
  CATCH <- data.table(CATCH)
  CATCH$fleet = 1
  CATCH[GEAR == "LONGLINE"]$fleet <- 2
  CATCH[GEAR == "POT"]$fleet <- 3
  CATCH <- CATCH[order(fleet,YEAR),]
  
  catch <- rbind(
    data.frame(year = c(-999, -999, -999), 
               seas = c(1, 1, 1), 
               fleet = c(1, 2, 3), 
               catch = c(0, 0, 0), 
               catch_se = rep(0.05, 3)),
    data.frame(year = CATCH$YEAR, 
               seas = 1, 
               fleet = CATCH$fleet, 
               catch = CATCH$TONS, 
               catch_se = 0.05))
  
  catch <- catch[order(catch$fleet,catch$year),]
  
  ## write catch data into new data files
  new_data$N_catch <- nrow(catch)
  new_data$catch <- catch
  
  ## Get catch for SAFE catch tables
  if(catch_table == TRUE){
    
    test <- paste("SELECT SUM(COUNCIL.COMPREHENSIVE_BLEND_CA.WEIGHT_POSTED)AS TONS,\n ",
                  "COUNCIL.COMPREHENSIVE_BLEND_CA.FMP_SUBAREA AS ZONE,\n ",
                  "COUNCIL.COMPREHENSIVE_BLEND_CA.FMP_GEAR AS GEAR,\n ",
                  "COUNCIL.COMPREHENSIVE_BLEND_CA.RETAINED_OR_DISCARDED AS TYPE,\n ",
                  "COUNCIL.COMPREHENSIVE_BLEND_CA.YEAR AS YEAR,\n ",
                  "COUNCIL.COMPREHENSIVE_BLEND_CA.AKR_STATE_FISHERY_FLAG AS STATE_FLAG,\n ",
                  "TO_CHAR(COUNCIL.COMPREHENSIVE_BLEND_CA.WEEK_END_DATE,'MM') AS MONTH, \n",
                  "COUNCIL.COMPREHENSIVE_BLEND_CA.SPECIES_GROUP_CODE AS SPECIES_GROUP\n ",
                  "FROM COUNCIL.COMPREHENSIVE_BLEND_CA\n ",
                  "WHERE COUNCIL.COMPREHENSIVE_BLEND_CA.FMP_SUBAREA in (",fsh_sp_area,")\n ",
                  "AND COUNCIL.COMPREHENSIVE_BLEND_CA.YEAR <= ",new_year,"\n ",
                  "AND COUNCIL.COMPREHENSIVE_BLEND_CA.SPECIES_GROUP_CODE in (",fsh_sp_label,")\n ",
                  "GROUP BY COUNCIL.COMPREHENSIVE_BLEND_CA.SPECIES_GROUP_CODE,\n ",
                  "COUNCIL.COMPREHENSIVE_BLEND_CA.FMP_SUBAREA,\n ",
                  "COUNCIL.COMPREHENSIVE_BLEND_CA.FMP_GEAR,\n ",
                  "COUNCIL.COMPREHENSIVE_BLEND_CA.RETAINED_OR_DISCARDED,\n ",
                  "COUNCIL.COMPREHENSIVE_BLEND_CA.YEAR,\n ", 
                  "COUNCIL.COMPREHENSIVE_BLEND_CA.AKR_STATE_FISHERY_FLAG,\n ",
                  "TO_CHAR(COUNCIL.COMPREHENSIVE_BLEND_CA.WEEK_END_DATE,'MM')", sep="")
    
    CATCH <- sqlQuery(CHINA, test)
    
    test <- paste0("SELECT akfin_year,\n ",
                   "FMP_AREA,\n ",
                   "adfg_i_harvest_code,\n ",
                   "fmp_gear,\n ",
                   "SUM (cfec_whole_pounds / 2204.622) AS CATCH_MT\n ",
                   "FROM council.comprehensive_Ft\n ",
                   "WHERE akfin_year BETWEEN 1997 AND 2002\n ",
                   "AND adfg_i_species_code = 110\n ",
                   "AND adfg_i_harvest_code = 80\n ",
                   "AND fmp_area = 'GOA'\n ",
                   "GROUP BY adfg_i_harvest_code,\n ",
                   "FMP_AREA,\n ",
                   "fmp_gear,\n ",
                   "akfin_year")
    
    ADFG_CATCH <- sqlQuery(CHINA, test)

    # Get table by jurisdiction and gear
    CATCH %>% 
      rename_all(tolower) %>%  
      mutate(state_flag = replace_na(state_flag, "N")) %>% 
      filter(state_flag != "Y") %>% 
      mutate(gear = case_when(gear == "TRW" ~ "fed_trawl",
                              gear == "HAL" ~ "fed_longline",
                              gear == "POT" ~ "fed_pot",
                              gear %in% c("JIG", "OTH", "GLN") ~ "fed_other")) %>% 
      group_by(year, gear) %>% 
      summarise(catch = round(sum(tons))) %>% 
      pivot_wider(names_from = gear, values_from = catch) %>% 
      select(year, fed_trawl, fed_longline, fed_pot, fed_other) %>% 
      mutate(fed_other = replace_na(fed_other, 0)) %>% 
      mutate(fed_tot = fed_trawl + fed_longline + fed_pot + fed_other) -> fed_gr_tbl

    CATCH %>% 
      rename_all(tolower) %>%  
      mutate(state_flag = replace_na(state_flag, "N")) %>% 
      filter(state_flag == "Y") %>% 
      mutate(gear = case_when(gear == "HAL" ~ "st_longline",
                              gear == "POT" ~ "st_pot",
                              gear %in% c("JIG", "OTH", "GLN") ~ "st_other")) %>% 
      group_by(year, gear) %>% 
      summarise(catch = round(sum(tons))) %>% 
      pivot_wider(names_from = gear, values_from = catch) %>% 
      select(year, st_longline, st_pot, st_other) %>% 
      mutate(st_other = replace_na(st_other, 0)) %>% 
      mutate(st_tot = st_longline + st_pot + st_other) -> st_gr_tbl1  
    
    ADFG_CATCH %>% 
      rename_all(tolower) %>% 
      select(akfin_year, fmp_gear, catch_mt) %>% 
      rename(year = akfin_year,
             gear = fmp_gear,
             tons = catch_mt) %>% 
      mutate(gear = case_when(gear == "HAL" ~ "st_longline",
                              gear == "POT" ~ "st_pot",
                              gear %in% c("JIG", "OTH", "GLN") ~ "st_other")) %>% 
      group_by(year, gear) %>% 
      summarise(catch = round(sum(tons))) %>% 
      pivot_wider(names_from = gear, values_from = catch) %>% 
      mutate(st_longline = replace_na(st_longline, 0)) %>% 
      select(year, st_longline, st_pot, st_other) %>% 
      mutate(st_tot = st_longline + st_pot + st_other) %>% 
      bind_rows(st_gr_tbl1) -> st_gr_tbl
    
    fed_gr_tbl %>% 
      left_join(st_gr_tbl) %>% 
      replace(is.na(.), 0) %>% 
      mutate(tot = fed_tot + st_tot) -> juris_gr_tbl
    
    # Get retained/discarded table
    ADFG_CATCH %>% 
      rename_all(tolower) %>% 
      select(akfin_year, fmp_gear, catch_mt) %>% 
      rename(year = akfin_year,
             gear = fmp_gear,
             tons = catch_mt) %>% 
      group_by(year) %>% 
      summarise(catch = sum(tons)) -> st_tot
      
    CATCH %>% 
      rename_all(tolower) %>% 
      group_by(year, type) %>% 
      summarise(catch = sum(tons)) %>%  
      pivot_wider(names_from = type, values_from = catch) %>% 
      left_join(st_tot) %>% 
      replace(is.na(.), 0) %>% 
      mutate(retained = R + catch) %>% 
      rename(discarded = D) %>% 
      select(year, discarded, retained) %>% 
      mutate(total = discarded + retained) -> dr_tbl
    
    # Write out results
    vroom::vroom_write(juris_gr_tbl, here::here(new_year, 'output', 'juris_gr_tbl.csv'), delim = ",")
    vroom::vroom_write(dr_tbl, here::here(new_year, 'output', 'dr_tbl.csv'), delim = ",")

  }
  
  
  ## ----- Get trawl survey pop'n estimates -----
  
  GOA_BIOM <- GET_GOA_BIOM(srv_sp_str)
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
  LL_RPN <- GET_GOA_LL_RPN(species = srv_sp_str, FYR = LLsrv_start_yr)
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
                where     species in ('Pacific cod')"))
      
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
                                            Nsamp = 100))
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
                                 FYR = LLsrv_start_yr)
  
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
                              Nsamp = 100)
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
                                     len_bins = len_bins)
  cond_age_length <- data.frame(svr_cond_al$norm)
  names(cond_age_length) <- names(GOA_ACOMP)
  print("Conditional survey age length done")      
  
  ## ----- Get fishery conditional age-length data -----
  
  fish_cond_al <- cond_length_age_corFISH(species = fsh_sp_str,
                                          area = sp_area,
                                          start_year = fsh_start_yr,
                                          max_age1 = max_age,
                                          len_bins = len_bins)
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
                          max_age = max_age)
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