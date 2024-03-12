
  
LENGTH_BY_CATCH_GOA2<-function(fsh_sp_str=202 ,fsh_sp_label = "'PCOD'",ly=new_year,statdat=State_dat_loc){

  # Get observer length frequency
  test <- paste("SELECT \n ",
      "CASE \n ",
      "  WHEN OBSINT.DEBRIEFED_LENGTH.GEAR in (1,2,3,4) \n ",
      "  THEN 0\n ",
      "  WHEN OBSINT.DEBRIEFED_LENGTH.GEAR in 6 \n ",
      "  THEN 2 \n ",
      "  WHEN OBSINT.DEBRIEFED_LENGTH.GEAR in (5,7,9,10,11,68,8) \n ",
      "  THEN 3 \n ",
      "END                                              AS GEAR, \n ",
      "CONCAT('H',TO_CHAR(OBSINT.DEBRIEFED_SPCOMP.HAUL_JOIN))       AS HAUL_JOIN, \n ",
      "TO_CHAR(OBSINT.DEBRIEFED_SPCOMP.HAUL_DATE, 'MM') AS MONTH, \n ",
      "CASE \n ",
      "  WHEN TO_CHAR(OBSINT.DEBRIEFED_SPCOMP.HAUL_DATE, 'MM') <= 2 \n ",
      "  THEN 1 \n ",
      "  WHEN TO_CHAR(OBSINT.DEBRIEFED_SPCOMP.HAUL_DATE, 'MM') > 2 \n ",
      "  AND TO_CHAR(OBSINT.DEBRIEFED_SPCOMP.HAUL_DATE, 'MM') <= 4 \n ",
      "  THEN 2 \n ",
      "  WHEN TO_CHAR(OBSINT.DEBRIEFED_SPCOMP.HAUL_DATE, 'MM') > 4 \n ",
      "  AND TO_CHAR(OBSINT.DEBRIEFED_SPCOMP.HAUL_DATE, 'MM') <= 8 \n ",
      "  THEN 3 \n ",
      "  WHEN TO_CHAR(OBSINT.DEBRIEFED_SPCOMP.HAUL_DATE, 'MM') > 8 \n ",
      "  AND TO_CHAR(OBSINT.DEBRIEFED_SPCOMP.HAUL_DATE, 'MM') <= 10 \n ",
      "  THEN 4 \n ",
      "  WHEN TO_CHAR(OBSINT.DEBRIEFED_SPCOMP.HAUL_DATE, 'MM') > 10 \n ",
      "  THEN 5 \n ",
      "END                                                AS SEASON, \n ",
      "CASE \n ",
      "  WHEN TO_CHAR(OBSINT.DEBRIEFED_SPCOMP.HAUL_DATE, 'MM') in (1,2,3) \n ",
      "  THEN 1 \n ",
      "  WHEN TO_CHAR(OBSINT.DEBRIEFED_SPCOMP.HAUL_DATE, 'MM') in (4,5,6) \n ",
       "  THEN 2 \n ",
      "  WHEN TO_CHAR(OBSINT.DEBRIEFED_SPCOMP.HAUL_DATE, 'MM') in (7,8,9) \n ",
      "  THEN 3 \n ",
      "  WHEN TO_CHAR(OBSINT.DEBRIEFED_SPCOMP.HAUL_DATE, 'MM') in (10,11,12) \n ",
      "  THEN 4 \n ",
      "END                                                AS QUARTER, \n ",
       "CASE \n ",
      "  WHEN TO_CHAR(OBSINT.DEBRIEFED_SPCOMP.HAUL_DATE, 'MM') in (1,2,3,4) \n ",
      "  THEN 1 \n ",
      "  WHEN TO_CHAR(OBSINT.DEBRIEFED_SPCOMP.HAUL_DATE, 'MM') in (5,6,7,8) \n ",
       "  THEN 2 \n ",
      "  WHEN TO_CHAR(OBSINT.DEBRIEFED_SPCOMP.HAUL_DATE, 'MM') in (9,10,11,12) \n ",
      "  THEN 3 \n ",
      "END                                                AS TRIMESTER, \n ",
      "TO_CHAR(OBSINT.DEBRIEFED_SPCOMP.HAUL_DATE, 'YYYY') AS YEAR, \n ",
      "OBSINT.DEBRIEFED_SPCOMP.EXTRAPOLATED_NUMBER        AS NUMB, \n ",
      "OBSINT.DEBRIEFED_SPCOMP.CRUISE        AS CRUISE, \n ",
      "OBSINT.DEBRIEFED_SPCOMP.PERMIT        AS PERMIT, \n ",
       "OBSINT.DEBRIEFED_SPCOMP.HAUL        AS HAUL, \n ",
      "OBSINT.DEBRIEFED_SPCOMP.EXTRAPOLATED_WEIGHT / 1000 AS WEIGHT, \n ",
      "OBSINT.DEBRIEFED_LENGTH.LENGTH                     AS LENGTH, \n ",
      "OBSINT.DEBRIEFED_LENGTH.FREQUENCY                  AS FREQ, \n ",
       "OBSINT.DEBRIEFED_HAUL.LONDD_END AS LON, \n",
        "OBSINT.DEBRIEFED_HAUL.LATDD_END AS LAT, \n",
        "OBSINT.DEBRIEFED_SPCOMP.HAUL_DATE AS HDAY, \n",
      "OBSINT.DEBRIEFED_HAUL.NMFS_AREA AS AREA \n",
      "FROM OBSINT.DEBRIEFED_HAUL \n ",
      "INNER JOIN OBSINT.DEBRIEFED_SPCOMP \n ",
      "ON OBSINT.DEBRIEFED_HAUL.HAUL_JOIN = OBSINT.DEBRIEFED_SPCOMP.HAUL_JOIN \n ",
      "INNER JOIN OBSINT.DEBRIEFED_LENGTH \n ",
      "ON OBSINT.DEBRIEFED_HAUL.HAUL_JOIN = OBSINT.DEBRIEFED_LENGTH.HAUL_JOIN \n ",
      "WHERE OBSINT.DEBRIEFED_HAUL.NMFS_AREA BETWEEN 600 AND 699 \n",
      "AND OBSINT.DEBRIEFED_LENGTH.NMFS_AREA != 670 \n",
      "AND OBSINT.DEBRIEFED_SPCOMP.SPECIES  in  (",fsh_sp_str,")",
      "AND OBSINT.DEBRIEFED_LENGTH.SPECIES    in  (",fsh_sp_str,")",sep="")

  Dspcomp = data.table(sqlQuery(AFSC, test))
  
  WED <- function(x = Dspcomp$HDAY[1]){
    y <- data.table(
      weekday = weekdays(x),
      wed = ceiling_date(x, "week"),  
      plus = ifelse(weekdays(x) %in% c("Sunday"), 6, -1),
      YR = year(x))
    y$next_saturday <- date(y$wed) + y$plus
    y[YR < 1993]$next_saturday <- date(y[YR < 1993]$wed)
    y$yr2 <- year(y$next_saturday)
    y[YR != yr2]$next_saturday <- date(paste0(y[YR != yr2]$YR,"-12-31"))
    return(y$next_saturday)
  }

  Dspcomp %>% 
    rename_all(tolower) %>% 
    mutate(haul1 = as.character(paste(cruise, permit, haul, sep = "_")),
           area = trunc(area/10)*10,
           gear = case_when(gear == 0 ~ "TRAWL",
                            gear == 2 ~ "POT",
                            gear == 3 ~ "LONGLINE"),
           wed = WED(hday)) -> Dspcomp1

  Dspcomp1 %>%
    group_by(year, gear) %>% 
    summarize(Nsamp = length(unique(haul_join))) -> HJ
  
  Dspcomp1 %>% 
    group_by(year, gear, area, trimester) %>% 
    summarize(Nsamp = length(unique(haul_join))) -> HJA
  
  Dspcomp1 %>% 
    group_by(haul_join, numb) %>% 
    mutate(t2freq = sum(freq)) %>% 
    filter(t2freq >= 10)  %>% 
    group_by(year, wed, trimester, area, haul1, gear) %>% 
    summarise(n1 = min(numb),
              hfreq = sum(freq)) -> x ## get individual haul extrapolated numbers of fish
  
  x %>% 
    group_by(year, wed, area, trimester, gear) %>% 
    summarise(n2 = sum(n1),
              tfreq = sum(hfreq)) -> y ## get total observed numbers of fish per year, area,state,  and gear
  
  Dspcomp1 %>%
    group_by(year, wed, trimester, area, haul1, length, gear) %>% 
    summarise(freq = sum(freq)) -> z ## number of fish by length bin, haul, gear and year

  x %>% 
    left_join(y) %>% 
    left_join(z) %>% 
    filter(year >= 1991) %>% 
    mutate(prop = ((freq / hfreq) * n1) / n2) %>% ## for each length bin, haul, gear and year calculate the proportion of fish 
    group_by(year, wed, trimester, area, gear, length) %>% 
    summarise(prop = sum(prop)) -> D_SPCOMP
    
  D_SPCOMP %>% 
    filter(length > 116) %>% 
    mutate(length = 117) -> D_SPlarge
  
  D_SPCOMP %>% 
    filter(length <= 116) %>%
    bind_rows(D_SPlarge) -> D_SPCOMP
  
  
  
  test <- paste("SELECT SUM(COUNCIL.COMPREHENSIVE_BLEND_CA.WEIGHT_POSTED)AS TONS, \n ",
                  "to_char(COUNCIL.COMPREHENSIVE_BLEND_CA.WEEK_END_DATE,'MM') AS MONTH, \n",
                  "CASE \n ",
      				"  WHEN TO_CHAR(COUNCIL.COMPREHENSIVE_BLEND_CA.WEEK_END_DATE, 'MM') <= 2 \n ",
     				"  THEN 1 \n ",
      				"  WHEN TO_CHAR(COUNCIL.COMPREHENSIVE_BLEND_CA.WEEK_END_DATE, 'MM') > 2 \n ",
      				"  AND TO_CHAR(COUNCIL.COMPREHENSIVE_BLEND_CA.WEEK_END_DATE, 'MM') <= 4 \n ",
      				"  THEN 2 \n ",
      				"  WHEN TO_CHAR(COUNCIL.COMPREHENSIVE_BLEND_CA.WEEK_END_DATE, 'MM') > 4 \n ",
      				"  AND TO_CHAR(COUNCIL.COMPREHENSIVE_BLEND_CA.WEEK_END_DATE, 'MM') <= 8 \n ",
      				"  THEN 3 \n ",
      				"  WHEN TO_CHAR(COUNCIL.COMPREHENSIVE_BLEND_CA.WEEK_END_DATE, 'MM') > 8 \n ",
      				"  AND TO_CHAR(COUNCIL.COMPREHENSIVE_BLEND_CA.WEEK_END_DATE, 'MM') <= 10 \n ",
      				"  THEN 4 \n ",
      				"  WHEN TO_CHAR(COUNCIL.COMPREHENSIVE_BLEND_CA.WEEK_END_DATE, 'MM') > 10 \n ",
      				"  THEN 5 \n ",
      			"END                                                AS SEASON, \n ",
      			"CASE \n ",
      				"  WHEN TO_CHAR(COUNCIL.COMPREHENSIVE_BLEND_CA.WEEK_END_DATE, 'MM') in (1,2,3) \n ",
      				"  THEN 1 \n ",
      				"  WHEN TO_CHAR(COUNCIL.COMPREHENSIVE_BLEND_CA.WEEK_END_DATE, 'MM') in (4,5,6) \n ",
       				"  THEN 2 \n ",
      				"  WHEN TO_CHAR(COUNCIL.COMPREHENSIVE_BLEND_CA.WEEK_END_DATE, 'MM') in (7,8,9) \n ",
      				"  THEN 3 \n ",
      				"  WHEN TO_CHAR(COUNCIL.COMPREHENSIVE_BLEND_CA.WEEK_END_DATE, 'MM') in (10,11,12) \n ",
      				"  THEN 4 \n ",
      			"END                                                AS QUARTER, \n ", 
            "CASE \n ",
              "  WHEN TO_CHAR(COUNCIL.COMPREHENSIVE_BLEND_CA.WEEK_END_DATE, 'MM') in (1,2,3,4) \n ",
              "  THEN 1 \n ",
              "  WHEN TO_CHAR(COUNCIL.COMPREHENSIVE_BLEND_CA.WEEK_END_DATE, 'MM') in (5,6,7,8) \n ",
              "  THEN 2 \n ",
              "  WHEN TO_CHAR(COUNCIL.COMPREHENSIVE_BLEND_CA.WEEK_END_DATE, 'MM') in (9,10,11,12) \n ",
              "  THEN 3 \n ",
              "END                                                AS TRIMESTER, \n ",       
                  "COUNCIL.COMPREHENSIVE_BLEND_CA.FMP_GEAR AS GEAR, \n ",
                  "COUNCIL.COMPREHENSIVE_BLEND_CA.YEAR AS YEAR, \n ",
                  "COUNCIL.COMPREHENSIVE_BLEND_CA.REPORTING_AREA_CODE AS AREA, \n",
                  "COUNCIL.COMPREHENSIVE_BLEND_CA.WEEK_END_DATE AS WED,  \n",
                  "COUNCIL.COMPREHENSIVE_BLEND_CA.AKR_STATE_FEDERAL_WATERS_CODE AS STATE \n ",
                  "FROM COUNCIL.COMPREHENSIVE_BLEND_CA \n ",
                   "WHERE COUNCIL.COMPREHENSIVE_BLEND_CA.FMP_SUBAREA in ('CG','PWSI','SE','SEI','WG','WY') \n ",
                  "AND COUNCIL.COMPREHENSIVE_BLEND_CA.YEAR <= ",ly," \n ",
                  "AND COUNCIL.COMPREHENSIVE_BLEND_CA.SPECIES_GROUP_CODE in (",fsh_sp_label,")\n ",
                  "GROUP BY COUNCIL.COMPREHENSIVE_BLEND_CA.SPECIES_GROUP_CODE, \n ",
                  "COUNCIL.COMPREHENSIVE_BLEND_CA.FMP_SUBAREA, \n ",
                  "COUNCIL.COMPREHENSIVE_BLEND_CA.FMP_GEAR, \n ",
                  "COUNCIL.COMPREHENSIVE_BLEND_CA.RETAINED_OR_DISCARDED, \n ",
                  "COUNCIL.COMPREHENSIVE_BLEND_CA.REPORTING_AREA_CODE, \n ",
                  "COUNCIL.COMPREHENSIVE_BLEND_CA.WEEK_END_DATE, \n ",
                  "COUNCIL.COMPREHENSIVE_BLEND_CA.AKR_STATE_FEDERAL_WATERS_CODE, \n ",
                  "COUNCIL.COMPREHENSIVE_BLEND_CA.YEAR \n ", sep="")


    CATCH <- data.table(sqlQuery(CHINA, test))

    CATCH %>% 
      rename_all(tolower) %>% 
      mutate(wed = date(wed),
             state = replace_na("F"),
             gear = case_when(gear %in% c("HAL", "JIG") ~ "LONGLINE",
                              gear == "TRW" ~ "TRAWL",
                              gear == "POT" ~ "POT"),
             area = trunc(area/10)*10) -> CATCH1
    
    CATCH1 %>% 
      group_by(year, wed, trimester, area, gear) %>% 
      summarise(tons = sum(tons)) -> z2
    
    CATCH1 %>% 
      group_by(year) %>% 
      summarise(total = sum(tons)) %>% 
      left_join(z2) %>% 
      mutate(catch_prop = tons / total) -> x2
    
    Dspcomp1 %>% 
      group_by(trimester, year, area, gear) %>% 
      summarise(tfreq = sum(freq)) -> t1
    
    D_SPCOMP %>% 
      left_join(x2) %>% 
      mutate(prop1 = prop * catch_prop) %>% 
      filter(!is.na(prop1)) %>% 
      group_by(year, trimester, area, gear, length) %>% 
      summarise(prop1 = sum(prop1)) -> DLENGTH

    grid <- data.table(expand.grid(
      year = sort(unique(DLENGTH$year)),
      trimester = c(1:3),
      area = unique(DLENGTH$area),
      gear = unique(DLENGTH$gear),
      length = seq(1,117,1)))
    
    grid %>% 
      left_join(DLENGTH) %>% 
      mutate(prop1 = replace_na(prop1, 0)) -> DLENGTH1
    
    DLENGTH1 %>% 
      group_by(year) %>% 
      summarise(tot = sum(prop1)) -> tot_prop
    
    DLENGTH1 %>% 
      left_join(tot_prop) %>% 
      mutate(prop1 = prop1 / tot) %>% 
      select(-tot) -> DLENGTH1 # rescale length comps
    
    DLENGTH1 %>% 
      group_by(year, gear, length) %>% 
      summarise(prop = sum(prop1)) %>% 
      reshape2::dcast(formula= gear + year ~ length, value.var = "prop") %>% 
      left_join(HJ) -> SS3_DLENGTH_NS
    

 ## pulling state data from file.
    if(exists("ALL_STATE_LENGTHS")){print("State lengths exist")}else{
      LENGTHS <- list(NO_STATE = SS3_DLENGTH_NS)
      return(LENGTHS)
      stop("There are no state lengths available")}

    ALL_STATE_LENGTHS %>% 
      rename_all(tolower) %>%
      filter(length > 116) %>% 
      mutate(length = 117) -> Big
    
    
    ALL_STATE_LENGTHS %>% 
      rename_all(tolower) %>% 
      filter(length <= 116) %>% 
      bind_rows(Big) %>% 
      filter(length > 0) %>% 
      mutate(area = trunc(area/10)*10,
             gear = case_when(gear == 91 ~ "POT",
                              gear %in% c(5, 26, 61) ~ "LONGLINE",
                              !(gear %in% c(5, 26, 61, 91)) ~ "TRAWL"),
             trimester = case_when(month <= 4 ~ 1,
                                   month > 4 & month <= 8 ~ 2,
                                   month > 8 ~ 3)) -> SLENGTH

    SLENGTH %>% 
      group_by(trimester, year, area, gear) %>% 
      summarise(sfreq = sum(freq)) -> s1
    
    SLENGTH %>% 
      group_by(trimester, year, area, gear) %>% 
      summarise(tot = sum(freq)) %>% 
      mutate(Nsamp = round(tot/50)) %>% 
      select(-tot) -> SHJA

    SLENGTH %>% 
      group_by(year, trimester, area, gear, length) %>% 
      summarise(freq = sum(freq)) -> Sx
    
    SLENGTH %>% 
      group_by(year, trimester, area, gear) %>% 
      summarise(total = sum(freq)) -> Sy

    SLENGTH %>% 
      group_by(year, trimester, area, gear, length) %>% 
      summarise(freq = sum(freq)) %>% 
      left_join(Sy) %>% 
      mutate(prop = freq / total) %>% 
      select(-freq, -total) %>% 
      left_join(s1) %>% 
      filter(sfreq >= 30) %>% 
      select(-sfreq) -> s3
    
    

   grid <- data.table(expand.grid(year = sort(unique(SLENGTH$year)),
                                  trimester = c(1:3),
                                  area = unique(SLENGTH$area),
                                  gear = unique(SLENGTH$gear),
                                  length = seq(1,117,1)))
   
   x2 %>% 
     group_by(year, trimester, area, gear) %>% 
     summarize(catch_prop = sum(catch_prop)) -> SCATCH
   
   grid %>% 
     left_join(s3) %>% 
     left_join(SCATCH) %>% 
     mutate(prop = replace_na(prop, 0),
            catch_prop = replace_na(catch_prop, 0),
            prop1 = prop * catch_prop) %>% 
     group_by(year, trimester, area, gear, length) %>% 
     summarize(prop1 = sum(prop1)) -> SLENGTH1
     
 
## take the one that has the most lengths recorded
   s1 %>% 
     left_join(t1) %>% 
     mutate(sfreq = replace_na(sfreq, 0),
            tfreq = replace_na(tfreq, 0),
            state = case_when(sfreq > tfreq ~ 1,
                              sfreq <= tfreq ~ 0)) -> yy ## test to see which dataset has more lengths measured
     
   yy %>% 
     filter(state == 1) %>% 
     select(-sfreq, -tfreq) -> yy1
   
   SLENGTH1 %>% 
     left_join(yy1) %>% 
     filter(!is.na(state)) %>% 
     left_join(SHJA) %>% 
     mutate(state = replace_na(state, 0),
            Nsamp = replace_na(Nsamp, 0)) -> S_LENGTHx

   
   DLENGTH1 %>% 
     left_join(yy1) %>% 
     filter(is.na(state)) %>% 
     left_join(HJA) %>% 
     mutate(state = replace_na(state, 0),
            Nsamp = replace_na(Nsamp, 0)) %>% 
     bind_rows(S_LENGTHx) %>% 
     group_by(year, gear, length) %>% 
     summarise(prop = sum(prop1),
               Nsamp = sum(Nsamp)) -> DLENGTH_S1
   
   DLENGTH_S1 %>% 
     group_by(gear, year) %>% 
     summarise(Nsamp = mean(Nsamp)) -> NSAMP
   
   DLENGTH_S1 %>% 
     group_by(year) %>% 
     summarise(tot = sum(prop)) -> tot_props
   
   DLENGTH_S1 %>% 
     left_join(tot_props) %>% 
     mutate(prop = prop / tot) %>% 
     select(-tot) -> DLENGTH_S1 # rescale length comps

   DLENGTH_S1 %>% 
     reshape2::dcast(formula = gear + year ~ length, value.var = "prop") %>% 
     left_join(NSAMP) -> SS3_DLENGTH_S1

## fill in the blanks code  Fill in years,trimester,area,gear with state port data if federal data are missing

   yy %>% 
     filter(tfreq < 30 & sfreq >= 30) %>% 
     select(-sfreq, -tfreq) -> yy3
   
   SLENGTH1 %>% 
     left_join(yy3) %>% 
     filter(!is.na(state)) %>% 
     left_join(SHJA) %>% 
     mutate(state = replace_na(state, 0),
            Nsamp = replace_na(Nsamp, 0)) -> S_LENGTHx
   
   
   DLENGTH1 %>% 
     left_join(yy3) %>% 
     filter(is.na(state)) %>% 
     left_join(HJA) %>% 
     mutate(state = replace_na(state, 0),
            Nsamp = replace_na(Nsamp, 0)) %>% 
     bind_rows(S_LENGTHx) %>% 
     group_by(year, gear, length) %>% 
     summarise(prop = sum(prop1),
               Nsamp = sum(Nsamp)) -> DLENGTH_S2
   
   DLENGTH_S2 %>% 
     group_by(gear, year) %>% 
     summarise(Nsamp = mean(Nsamp)) -> NSAMP
   
   DLENGTH_S2 %>% 
     group_by(year) %>% 
     summarise(tot = sum(prop)) -> tot_props
   
   DLENGTH_S2 %>% 
     left_join(tot_props) %>% 
     mutate(prop = prop / tot) %>% 
     select(-tot) -> DLENGTH_S2 # rescale length comps
   
   DLENGTH_S2 %>% 
     reshape2::dcast(formula = gear + year ~ length, value.var = "prop") %>% 
     left_join(NSAMP) -> SS3_DLENGTH_S2
   
  LENGTHS<-list(NO_STATE = SS3_DLENGTH_NS, STATE_HIGH = SS3_DLENGTH_S1, STATE_HOLE = SS3_DLENGTH_S2)
  return(LENGTHS)
  }
