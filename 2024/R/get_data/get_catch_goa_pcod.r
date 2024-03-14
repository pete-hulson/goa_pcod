# Get fishery catch for goa pcod
# Originally adapted/generalized from Steve Barbeaux' files for generating SS files for EBS/AI Greenland Turbot
# Catch function developed in 2024 by Pete Hulson to develop link to afscdata package
#' @param new_year current assessment year
#' @param fsh_sp_label species label for observer/catch data (default = 'PCOD')
#' @param fsh_sp_area NPFMC subareas (default to goa subareas)
#' @param query switch for whether to run sql query for data (default = FALSE)
#' 
#' @return
#' @export get_catch_goa_pcod
#' 
#' @examples
#' 

get_catch_goa_pcod <- function(new_year = 9999,
                               fsh_sp_label = "PCOD",
                               fsh_sp_area = c("CG","PWSI","SE","SEI","WG","WY"),
                               query = FALSE){
  
  # query data ----
  if(isTRUE(query)){
    
    ## Open up data base connections
    db_specs <- vroom::vroom(here::here(new_dat_year, "database_specs.csv"))
    akfin_user = db_specs$username[db_specs$database == "AKFIN"]
    akfin_pass = db_specs$password[db_specs$database == "AKFIN"]
    database = 'akfin'
    
    conn = DBI::dbConnect(odbc::odbc(),
                          database,
                          UID = akfin_user,
                          PWD = akfin_pass)
    
    # query catch data and write raw data to folder
    afscdata::q_catch(year = new_dat_year,
                      species = fsh_sp_label,
                      area = fsh_sp_area,
                      db = conn)
    
    # query ADF&G data from 1997-2002
    adfg = readLines(here::here(new_year, 'inst', 'sql', 'adfg_fsh_catch.sql'))
    
    sql_run(conn, adfg) %>% 
      dplyr::rename_all(tolower) %>% 
      vroom::vroom_write(., here::here(new_year, 'data', 'raw', 'adfg_catch.csv'), delim = ",")

  }
  
  # current catch ----

  # summarize catch data to year, subarea, gear, type (retained/discarded), and month
  # add gear description and summarize months to season
  vroom::vroom(here::here(new_year, "data", "raw", "fsh_catch_data.csv")) %>% 
    tidytable::select(year, 
                      zone = fmp_subarea,
                      species_group = species_group_code, 
                      gear = fmp_gear,
                      type = retained_or_discarded, 
                      week_end_date, 
                      weight_posted) %>% 
    tidytable::mutate(month = month(week_end_date)) %>% 
    tidytable::select(-week_end_date) %>% 
    tidytable::summarise(tons = sum(weight_posted), 
                         .by = c(year, species_group, zone, gear, type, month)) %>% 
    tidytable::mutate(gear_desc = case_when(gear %in% c("TRW", "OTH", "GLN") ~ "trawl",
                                            gear %in% c("HAL", "JIG") ~ "longline",
                                            gear == "POT" ~ "pot"),
                      season = case_when(month %in% c(1, 2) ~ 1,
                                         month %in% c(3, 4) ~ 2,
                                         month %in% c(5, 6, 7, 8) ~ 3,
                                         month %in% c(9, 10) ~ 4,
                                         month %in% c(11, 12) ~ 5)) -> catch_summ
  
  # get total catch by year and gear
  catch_summ %>% 
    tidytable::summarise(tons = sum(tons), .by = c(year, gear_desc)) %>% 
    tidytable::rename(gear = gear_desc) -> curr_catch

  # old catch ----
  
  # read in fixed old catch and add gear descriptions
  vroom::vroom(here::here(new_year, 'data', 'old_catch.csv')) %>% 
    dplyr::rename_all(tolower) %>% 
    tidytable::mutate(gear_desc = case_when(gear %in% c("TRAWL") ~ "trawl",
                                            gear %in% c("LONGLINE", "OTHER") ~ "longline",
                                            gear == "POT" ~ "pot")) %>% 
    tidytable::summarise(tons = sum(tons), .by = c(year, gear_desc)) %>% 
    tidytable::rename(gear = gear_desc) -> old_catch
    
  
  # adf&g catch ----
  
  # read in adf&g catch and add gear descriptions
  vroom::vroom(here::here(new_year, 'data', 'raw', 'adfg_catch.csv')) %>% 
    tidytable::rename(year = akfin_year) %>% 
    tidytable::mutate(gear = case_when(fmp_gear %in% c("JIG", "HAL") ~ "longline",
                                       fmp_gear == "POT" ~ "pot")) %>% 
    tidytable::summarise(adfg_tons = sum(catch_mt), .by = c(year, gear)) -> adfg_catch

  # total catch ----
  
  # bind old and current catch and get total catch by year and gear
  curr_catch %>% 
    tidytable::left_join(adfg_catch) %>% 
    tidytable::mutate(adfg_tons = replace_na(adfg_tons, 0)) %>% 
    tidytable::mutate(tot_tons = tons + adfg_tons) %>% 
    tidytable::select(year, gear, tot_tons) %>% 
    tidytable::rename(tons = tot_tons) %>% 
    tidytable::bind_rows(old_catch) %>% 
    tidytable::arrange(gear, year) -> tot_catch

  
  
  

  
  # old code
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
  vroom::vroom_write(CATCH, here::here(new_year, 'data', 'raw', 'catch.csv'), delim = ",")
  
  catch_og <- tidytable::as_tidytable(CATCH) %>% 
    dplyr::rename_all(tolower) 
  
  
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
    vroom::vroom_write(ADFG_CATCH, here::here(new_year, 'data', 'raw', 'adfg_catch.csv'), delim = ",")
    
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
  
  
  
  
  
  
  
  
  
  
  
  

}
