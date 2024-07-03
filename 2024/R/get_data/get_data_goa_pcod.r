#' Originally adapted/generalized from Steve Barbeaux's files for generating SS files for EBS/AI Greenland Turbot
#' Modified in 2022 and 2023, and then completely re-developed in 2024 by Pete Hulson
#'  
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
                                  query = query)
  
  # put into ss3 data file
  new_data$N_catch <- nrow(ss3_catch)
  new_data$catch <- ss3_catch
  print("catch done")

  
  # get survey indices ----
  
  ## afsc bottom trawl survey ----
  ss3_twl_indx <- get_twl_srvy_index(new_year = new_dat_year,
                                     twl_srvy = twl_srvy,
                                     species = srv_sp_str,
                                     query = query,
                                     indx = indx)
  
  ## afsc longline survey ----
  ss3_ll_indx <- get_ll_srvy_index(new_year = new_dat_year,
                                   area = sp_area,
                                   species = srv_sp_str,
                                   query = query,
                                   indx = indx)

  ## iphc longline survey ----
  ss3_iphc_indx <- get_iphc_srvy_index(new_year = new_dat_year,
                                       query = query)
  
  ## adf&g trawl survey ----
  ss3_adfg_indx <- get_adfg_srvy_index(new_year = new_dat_year,
                                       run_glm = run_glm)
  
  ## larval indices ----
  # note: for time-being, these are entered by hand from emailed data
  ss3_larval_indx <- vroom::vroom(here::here(new_dat_year, 'data', 'raw', 'larval_indices.csv'))
  
  ## format for ss3 data file ----
  cpue <- ss3_twl_indx %>% 
    tidytable::bind_rows(ss3_ll_indx) %>% 
    tidytable::bind_rows(ss3_iphc_indx) %>% 
    tidytable::bind_rows(ss3_adfg_indx) %>% 
    tidytable::bind_rows(ss3_larval_indx)
  
  new_data$N_cpue <- nrow(cpue)
  new_data$CPUE <- cpue
  print("indices done")

  # get length composition data ----
  
  

  
  
  
  ## afsc bottom trawl survey ----
  ss3_twl_lcomp <- get_twl_srvy_lcomp(new_year = new_dat_year,
                                      twl_srvy = twl_srvy,
                                      species = srv_sp_str,
                                      query = query,
                                      bins = len_bins,
                                      iss = FALSE,
                                      nsamp = 100)

  ## afsc longline survey ----
  ss3_ll_lcomp <- get_ll_srvy_lcomp(new_year = new_dat_year,
                                    area = sp_area,
                                    species = srv_sp_str, 
                                    query = query,
                                    bins = len_bins,
                                    iss = FALSE,
                                    nsamp = 100)
  

  
  
  # test fishery fcns
  AFSC = odbcConnect("AFSC", 
                     'hulsonp', 
                     'Bri3+Fin2+Liam1', 
                     believeNRows=FALSE)
  AKFIN = odbcConnect("AKFIN", 
                     'phulson', 
                     '$blwins1', 
                     believeNRows=FALSE)
  fsh_sp_str = 202
  fsh_sp_label = "'PCOD'"
  ly = 2024
  new_year = 2024
  fsh_sp_code = 202
  query = TRUE
  database = 'afsc'

  
  
  fsh_len_comp_f %>% 
    data.table()
  
  fsh_len_full_f %>% 
    # haul-level length frequency
    tidytable::summarise(freq = sum(freq),
                         .by = c(year, wed, trimester, area, haul1, gear, length)) %>% 
    # join number of total fish sampled per haul
    tidytable::left_join(fsh_len_full_f %>% 
                           tidytable::summarise(n1 = min(numb),
                                                hfreq = sum(freq),
                                                .by = c(year, wed, trimester, area, haul1, gear))) %>% 
    # join number of fish by year, week, area, and gear
    tidytable::left_join(fsh_len_full_f %>% 
                           tidytable::summarise(n1 = min(numb),
                                                hfreq = sum(freq),
                                                .by = c(year, wed, trimester, area, haul1, gear)) %>% 
                           tidytable::summarise(n2 = sum(n1),
                                                tfreq = sum(hfreq),
                                                .by = c(year, wed, trimester, area, gear))) %>% 
    # proportion of haul catch-at-length by gear-area-week observed catch
    # expand the haul length composition by haul catch, then divide by the gear-area-week total observed haul catches
    tidytable::mutate(prop = ((freq / hfreq) * n1) / n2) %>%
    # summarise to length composition by week-area-gear
    tidytable::summarise(prop = sum(prop),
                         .by = c(year, wed, trimester, area, gear, length)) %>% 
    full_join(D_SPCOMP %>% 
                dplyr::rename_all(tolower) %>% 
                mutate(gear = tolower(gear)) %>% 
                rename(prop_old = prop)) %>% 
    drop_na() %>% 
    mutate(diff = prop - prop_old) %>% 
    filter(diff > 0) %>% 
    data.table()
  
  catch_p %>% 
    # summarise(test_new = sum(catch_prop), .by = year) %>%
    data.table()
  
  x2 %>% 
    dplyr::rename_all(tolower) %>% 
    data.table()
  
  x2 %>% 
    dplyr::rename_all(tolower) %>% 
    mutate(gear = tolower(gear)) %>% 
    full_join(catch_p %>% 
                rename(catch_prop_new = catch_prop)) %>% 
    # summarise(test = sum(catch_prop), .by = year) %>% 
    data.table()
  
  
  DLENGTH_NS %>% 
    dplyr::rename_all(tolower) %>% 
    summarise(test = sum(prop), .by = year) %>% 
  left_join(fsh_len_comp_f %>% 
    summarise(test_new = sum(prop), .by = year)) %>% 
    mutate(diff = test - test_new) %>% 
    data.table()
  
  
  
  auxFLCOMP <- LENGTH_BY_CATCH_GOA(fsh_sp_str = 202,
                                   fsh_sp_label = "'PCOD'",
                                   ly = 2024)
  auxFLCOMP <- auxFLCOMP[[3]]
  
  new_fsh_comp <- get_catch_len(new_year = 2024,
                                fsh_sp_code = 202,
                                query = TRUE,
                                database = 'afsc',
                                fltr = TRUE,
                                fill_st = TRUE)
  
  new_fsh_comp %>% 
    pivot_longer(cols = as.character(seq(1, 117)), names_to = 'length', values_to = 'prop') %>% 
    mutate(prop_t = sum(prop), .by = c(year, gear)) %>% 
    mutate(prop_new = prop / prop_t) %>% 
    select(year, gear, length, nsamp_new = nsamp, prop_new) %>% 
    left_join(auxFLCOMP %>% 
                dplyr::rename_all(tolower) %>% 
                mutate(gear = tolower(gear)) %>% 
                pivot_longer(cols = as.character(seq(1, 117)), names_to = 'length', values_to = 'prop') %>% 
                mutate(prop_t = sum(prop), .by = c(year, gear)) %>% 
                mutate(prop_old = prop / prop_t) %>% 
                select(year, gear, length, nsamp_old = nsamp, prop_old)) %>% 
    mutate(diff_p = prop_new - prop_old,
           diff_n = nsamp_new - nsamp_old) %>% 
    # filter(diff_p != 0) %>% 
    # summarise(test1 = sum(diff_p),
    #           test2 = sum(diff_n)) %>% 
    # summarise(test = max(diff_p)) %>% 
    data.table()
  
  

  new_fsh_comp %>% 
    pivot_longer(cols = as.character(seq(1, 117)), names_to = 'length', values_to = 'prop') %>% 
    mutate(prop_t = sum(prop), .by = c(year, gear)) %>% 
    mutate(prop_new = prop / prop_t) %>% 
    select(year, gear, length, nsamp_new = nsamp, prop_new) %>% 
    left_join(auxFLCOMP %>% 
                dplyr::rename_all(tolower) %>% 
                mutate(gear = tolower(gear)) %>% 
                pivot_longer(cols = as.character(seq(1, 117)), names_to = 'length', values_to = 'prop') %>% 
                mutate(prop_t = sum(prop), .by = c(year, gear)) %>% 
                mutate(prop_old = prop / prop_t) %>% 
                select(year, gear, length, nsamp_old = nsamp, prop_old)) -> dat
  
  ggplot(data = dat %>% filter(year == 2023), aes(x = length, y = prop_old)) +
    geom_bar(stat = "identity") +
    facet_wrap(~year)
  
  ggplot(data = dat %>% filter(year == 2023), aes(x = length, y = prop_new)) +
    geom_bar(stat = "identity") +
    facet_wrap(~year)
  
  
  dat %>% 
    pivot_longer(cols = c(prop_new, prop_old)) %>% 
    filter(year == 2022) -> dat1
  
  ggplot(data = dat1, aes(x = length, y = value, fill = name)) +
    geom_bar(stat = "identity", position = position_dodge()) 
  
    geom_bar(aes(x = length, y = prop_new))
  
  
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
