#' Get fishery catch age composition
#' Originally adapted/generalized from Steve Barbeaux' files for generating SS files for EBS/AI Greenland Turbot
#' Re-developed in 2024 by p. hulson from FISH_AGE_COMP.r file
#' 
#' @param new_year current assessment year
#' @param st_yr start year for age data (default = 2007)
#' @param max_age max age for age comps (i.e., plus group, default = 10)
#' @param fltr switch for whether to filter small number of length samples (default = TRUE)
#' 

get_fsh_age <- function(new_year = 9999,
                        st_yr = 2007,
                        max_age = 10,
                        fltr = TRUE){
  
  # get expanded length comps ----
  
  fsh_len_comp <- get_fsh_len4age(new_year, fltr)
  
  
  # test with old script
  ldata %>% 
    dplyr::rename_all(tolower) %>% 
    mutate(gear = tolower(gear)) %>% 
    select(-nsamp) %>%
    left_join(fsh_len_comp %>% 
                rename(prop_new = prop) %>% 
                select(-nsamp)) %>%
    mutate(diff = prop - prop_new) %>% 
    # filter(abs(diff) > 0.001) %>% 
    arrange(diff) %>% 
    data.table()
  
  
  
  
  # age data ----
  fsh_age <- vroom::vroom(here::here(new_year, 'data', 'raw', 'fish_age_domestic.csv')) %>% 
    # filter to years post-2007 (as default)
    tidytable::filter(year > st_yr)

  # compute age comps ----
  
  fsh_len_comp %>% 
    tidytable::mutate(freq = prop * 10000) %>% 
    # filter to years post-2007 (as default)
    tidytable::filter(year > st_yr) -> fsh_len_comp

  
  ldata %>% 
    dplyr::rename_all(tolower) %>% 
    mutate(gear = tolower(gear)) %>% 
    select(-nsamp) %>% 
    left_join(fsh_len_comp %>% 
                rename(prop_new = prop) %>% 
                select(-nsamp)) %>%
    filter(year == 2008,
           sex == 'F',
           gear == 'longline') %>% 
    mutate(diff = prop - prop_new) %>% 
    filter(diff != 0) %>%
    # filter(abs(diff) > 0.001) %>% 
    arrange(length) %>% 
    data.table()
  
  fsh_age %>% 
    data.table()
  
  fsh_len_comp %>% 
    filter(year == 2008,
           sex == 'F',
           gear == 'longline') %>% 
    data.table()
  
  gears<-unique(age$GEAR)
  z<-data.frame(matrix(ncol=5,nrow=1))
  names(z)<-c("YEAR","GEAR","SEX","AGE","LENGTH" )
  
  for(j in 1:length(gears)){
    age1<-age[GEAR==gears[j]]
    ldata1<-ldata[GEAR==gears[j]]
    years<-sort(unique(age1$YEAR))
    for( i in 1:length(years)){
      x=find_ALF(Adata=age1,Ldata=ldata1,year=years[i])
      y1=data.frame(YEAR=years[i],GEAR=gears[j],SEX=1,AGE=x$len1$age,LENGTH=x$len1$tl)
      y2=data.frame(YEAR=years[i],GEAR=gears[j],SEX=2,AGE=x$len2$age,LENGTH=x$len2$tl)
      z=rbind(z,y1,y2)
    }
    
  }
  
  year = 2008
  adata <- fsh_age %>% 
    tidytable::filter(gear == 'longline',
                      year == 2008)
  Adata <- age[GEAR==gears[1]]
  
  ldata_new <- fsh_len_comp %>% 
    tidytable::filter(gear == 'longline',
                      year == 2008)
  Ldata = ldata[GEAR==gears[1]]
  
  
  
  
  get_alk<-function(adata,ldata,year){

    adata %>% 
      tidytable::mutate(age = case_when(age > 20 ~ 20,
                                        .default = age)) -> adata
    
    Adata$AGE1<-Adata$AGE
    Adata$AGE1[Adata$AGE>20] <- 20
    
    adata_m <- adata %>% 
      tidytable::filter(sex == 'M') %>% 
      tidytable::select(age, tl = length)
    adata_f <- adata %>% 
      tidytable::filter(sex == 'F') %>% 
      tidytable::select(age, tl = length)
    
    ldata_m <- ldata_new %>% 
      tidytable::filter(sex == 'M') %>% 
      tidytable::select(length, freq)
    ldata_f <- ldata_new %>% 
      tidytable::filter(sex == 'F') %>% 
      tidytable::select(length, freq)
    
    rb.age1<-subset(Adata,Adata$YEAR==year&Adata$SEX=="M")
    rb.age2<-subset(Adata,Adata$YEAR==year&Adata$SEX=="F")
    rb.len1<-subset(Ldata,Ldata$YEAR==year&Ldata$SEX=="M")
    rb.len2<-subset(Ldata,Ldata$YEAR==year&Ldata$SEX=="F")

    rb.age1<-data.frame(age=rb.age1$AGE1,tl=rb.age1$LENGTH)
    rb.age2<-data.frame(age=rb.age2$AGE1,tl=rb.age2$LENGTH)
    rb.len1<-aggregate(list(FREQ=rb.len1$FREQUENCY),by=list(LENGTH=rb.len1$LENGTH),FUN=sum)
    rb.len2<-aggregate(list(FREQ=rb.len2$FREQUENCY),by=list(LENGTH=rb.len2$LENGTH),FUN=sum)
    
    
    expand.length<-function(data){
      x<-rep(data$LENGTH[1],data$FREQ[1])
      for( i  in 2:nrow(data)){
        x1<-rep(data$LENGTH[i],data$FREQ[i])
        x<-c(x,x1)
      }
      y<-data.frame(age=NA,tl=x)
      y
    }
    
    rb.len1<-expand.length(rb.len1)
    rb.len2<-expand.length(rb.len2)
    
    rb.age1=rbind(rb.age1,expand.grid(age=1,tl=c(min(rb.len1$tl),(min(rb.age1$tl)-1))))
    rb.age2=rbind(rb.age2,expand.grid(age=1,tl=c(min(rb.len2$tl),(min(rb.age2$tl)-1))))
    
    
    
    #rb.age1.1<-lencat(rb.age1,"tl",startcat=5,w=5)
    #rb.age2.1<-lencat(rb.age2,"tl",startcat=5,w=5)
    rb.age1.1<-lencat(x=~tl,data=rb.age1,startcat=5,w=5)
    rb.age2.1<-lencat(x=~tl,data=rb.age2,startcat=5,w=5)
    
    
    
    rb.raw1 <- table(rb.age1.1$LCat,rb.age1.1$age)
    rb.key1 <- prop.table(rb.raw1,margin=1)
    rb.raw2 <- table(rb.age2.1$LCat,rb.age2.1$age)
    rb.key2 <- prop.table(rb.raw2,margin=1)
    
    rb.len1.1 <- alkIndivAge(rb.key1,formula=~tl,data=rb.len1) 
    rb.len2.1 <- alkIndivAge(rb.key2,formula=~tl,data=rb.len2) 
    
    
    
    #rb.len1.1 <- ageKey(rb.key1,cl="tl",ca="age",dl=rb.len1,type="SR")
    #  rb.len2.1 <- ageKey(rb.key2,formula=age~tl,data=rb.len2,type="SR")
    #rb.len2.1 <- ageKey(rb.key2,cl="tl",ca="age",dl=rb.len2,type="SR")
    
    AL<-list(len1=rb.len1.1,len2=rb.len2.1)
    AL
  }
  
  
  
  
  
  
  
  
  
  
  
  ## length freq data ----
  ### federal ----
  fsh_len_f <- vroom::vroom(here::here(new_year, 'data', 'raw', 'fish_lfreq_domestic.csv')) %>% 
    # filter to years post-1991
    tidytable::filter(year >= 1991) %>% 
    # unique cruise-permit-haul description
    tidytable::mutate(haul1 = paste(cruise, permit, haul, sep = "_")) %>% 
    # get correct week end date
    tidytable::mutate(weekday = weekdays(hday),
                      wed = lubridate::ceiling_date(hday, "week"),
                      plus = tidytable::case_when(weekdays(hday) == "Sunday" ~ 6, 
                                                  .default = -1),
                      yr = lubridate::year(hday),
                      next_saturday = tidytable::case_when(yr >= 1993 ~ date(wed) + plus,
                                                           .default = date(wed)),
                      yr2 = lubridate::year(next_saturday),
                      wed2 = tidytable::case_when(yr != yr2 ~ date(paste0(yr, '-12-31')), 
                                                  .default = next_saturday)) %>% 
    tidytable::select(-weekday, - wed, -plus, -yr, -next_saturday, -yr2) %>% 
    tidytable::rename(wed = wed2) %>% 
    # truncate area to nearest 10 (e.g., 649 becomes 640)
    tidytable::mutate(area = trunc(area / 10) * 10)
  
  ### state ----
  fsh_len_s <- vroom::vroom(here::here(new_year, 'data', 'ALL_STATE_LENGTHS.csv')) %>% 
    dplyr::rename_all(tolower) %>% 
    #filter to positive lengths
    tidytable::filter(length > 0) %>% 
    # define area, gear, plus length, trimester
    tidytable::mutate(area = trunc(area / 10) * 10, # truncate area to nearest 10 (e.g., 649 becomes 640)
                      gear1 = tidytable::case_when(gear == 91 ~ 'pot',
                                                   gear %in% c(5, 26, 61) ~ 'longline',
                                                   .default = 'trawl'), # define gears
                      length = tidytable::case_when(length > 116 ~ 117,
                                                    .default = length), # set plus length to 117 cm
                      trimester = tidytable::case_when(month <= 4 ~ 1,
                                                       month %in% seq(5, 8) ~ 2,
                                                       month >= 9 ~ 3),
                      sex = tidytable::case_when(sex == 1 ~ 'M',
                                                 sex == 2 ~ 'F',
                                                 .default = 'U')) %>% 
    tidytable::select(year, area, gear = gear1, month, trimester, quarter, sex, length, freq)
  
  ## catch data ----
  vroom::vroom(here::here(new_year, 'data', 'raw', 'fsh_catch_data.csv')) %>% 
    tidytable::mutate(month = lubridate::month(week_end_date),
                      trimester = tidytable::case_when(month <= 4 ~ 1,
                                                       month %in% c(5, 6, 7, 8) ~ 2,
                                                       month >= 9 ~ 3)) %>% 
    # define gear type, and truncate area to nearest 10
    tidytable::mutate(wed = date(week_end_date),
                      gear = tidytable::case_when(fmp_gear %in% c('TRW', 'GLN', 'OTH') ~ 'trawl',
                                                  fmp_gear == 'POT' ~ 'pot',
                                                  fmp_gear %in% c('HAL', 'JIG') ~ 'longline'),
                      area = trunc(reporting_area_code / 10) * 10) %>% 
    # compute proportion of annual catch by week-area-gear
    tidytable::summarise(tons = sum(weight_posted), .by = c(year, wed, trimester, area, gear)) %>% 
    tidytable::mutate(total = sum(tons), .by = year) %>% 
    tidytable::mutate(catch_prop = tons / total) -> catch_p
  
  # filter length freq data ----
  if(isTRUE(fltr)){
    ## filtering data to hauls with greater than 10 (federal) and 30 (state) lengths (old way) ----
    fsh_len_f %>% 
      # filter hauls w/ less than 10 lengths observed
      tidytable::mutate(n_hl = sum(freq), .by = c(haul_join, numb)) %>% 
      tidytable::filter(n_hl >= 10) %>% 
      tidytable::select(-n_hl) -> fsh_len_full_f
    
    fsh_len_s %>% 
      # length freq at trimester-area-gear
      tidytable::summarise(freq = sum(freq), .by = c(year, month, trimester, area, gear, length)) %>% 
      tidytable::drop_na() %>% 
      # filter to >30 lengths per trimester, area, and gear
      tidytable::left_join(fsh_len_s %>% 
                             tidytable::summarise(sfreq = sum(freq), .by = c(year, trimester, area, gear)) %>% 
                             tidytable::drop_na()) %>% 
      tidytable::filter(sfreq >= 30) %>% 
      tidytable::select(-sfreq) -> fsh_len_full_s
  } else{
    ## not filtering data  ----
    fsh_len_full_f <- fsh_len_f
    
    fsh_len_s %>% 
      # length freq at trimester-area-gear
      tidytable::summarise(freq = sum(freq), .by = c(year, month, trimester, area, gear, length)) %>% 
      tidytable::drop_na() -> fsh_len_full_s
    
  }
  
  # fill-in with state data ----
  ## federal catch weighted length comp ----
  fsh_len_full_f %>% 
    # haul-level length frequency
    tidytable::summarise(freq = sum(freq),
                         .by = c(year, wed, trimester, area, haul1, gear, sex, length)) %>% 
    # join number of total fish sampled per haul
    tidytable::left_join(fsh_len_full_f %>% 
                           tidytable::summarise(n1 = min(numb),
                                                hfreq = sum(freq),
                                                .by = c(year, wed, trimester, area, haul1, gear)) %>% 
                           # compute number of fish by year, week, area, and gear
                           tidytable::mutate(n2 = sum(n1),
                                             .by = c(year, wed, trimester, area, gear))) %>% 
    # proportion of haul catch-at-length by gear-area-week observed catch
    # expand the haul length composition by haul catch, then divide by the gear-area-week total observed haul catches
    tidytable::mutate(prop = ((freq / hfreq) * n1) / n2) %>%
    # summarise to length composition by week-area-gear
    tidytable::summarise(prop = sum(prop),
                         .by = c(year, wed, trimester, area, gear, sex, length)) %>% 
    # join length comp with catch proportion and compute catch weighted length comp by year, trimester, area, and gear
    tidytable::left_join(catch_p) %>% 
    # set plus length group to 117 cm
    tidytable::mutate(length = tidytable::case_when(length > 116 ~ 117,
                                                    .default = length)) %>% 
    # weight length comp by proportion of catch
    tidytable::mutate(prop1 = prop * catch_prop) %>% 
    tidytable::drop_na() %>% 
    tidytable::summarise(prop = sum(prop1), 
                         .by = c(year, trimester, area, gear, sex, length)) -> fsh_len_comp_f
  
  if(isTRUE(fltr)){
    ## filtering data to year-trimester-area-gear with greater than 30 observations
    fsh_len_comp_f %>% 
      tidytable::left_join(fsh_len_full_f %>% 
                             tidytable::summarise(tfreq = sum(freq), .by = c(year, trimester, area, gear))) %>% 
      tidytable::filter(tfreq >= 30) -> fsh_len_comp_f
  }
  
  
  # get grid of all possible combos of federal year-gear-area-trimester-length
  tidytable::expand_grid(year = sort(unique(fsh_len_full_f$year)),
                         trimester = c(1:3),
                         gear = unique(fsh_len_full_f$gear),
                         area = unique(fsh_len_full_f$area),
                         sex = c('M', 'F', 'U'),
                         length = seq(1,117,1)) %>% 
    # join weighted fish length comp
    tidytable::left_join(fsh_len_comp_f) %>% 
    tidytable::mutate(prop = tidytable::replace_na(prop, 0)) -> fsh_len_comp_f
  

  
  ## state catch weighted length comp ----
  # get grid of all possible combos of state year-gear-area-trimester-length
  tidytable::expand_grid(year = sort(unique(fsh_len_full_s$year)),
                         trimester = c(1:3),
                         gear = unique(fsh_len_full_s$gear),
                         area = unique(fsh_len_full_s$area),
                         sex = c('M', 'F', 'U'),
                         length = seq(1,117,1)) %>% 
    # join state length frequencies
    tidytable::left_join(fsh_len_full_s %>% 
                           # total lengths obs by trimester-area-gear
                           tidytable::mutate(total = sum(freq), .by = c(year, trimester, area, gear)) %>% 
                           # compute state length comp by trimester, area, and gear
                           tidytable::mutate(prop = freq / total) %>% 
                           tidytable::select(-total, -freq)) %>% 
    # join federal catch for trimester-area-gear
    tidytable::left_join(catch_p %>% 
                           tidytable::summarise(catch_prop = sum(catch_prop), .by = c(year, trimester, area, gear))) %>% 
    # compute catch weighted length comp
    tidytable::mutate(prop = tidytable::replace_na(prop, 0),
                      catch_prop = tidytable::replace_na(catch_prop, 0),
                      prop1 = prop * catch_prop) %>% 
    tidytable::summarise(prop = sum(prop1), .by = c(year, trimester, area, gear, sex, length)) -> fsh_len_comp_s
  
  ## merge state and federal length frequencies ----
  # test which trimester-area-gear length freqs have more state than fed data
  fsh_len_full_f %>% 
    # get federal number of length obs by trimester-area-gear
    tidytable::summarise(tfreq = sum(freq), .by = c(year, trimester, area, gear)) %>% 
    # get state number of length obs by trimester-area-gear
    tidytable::full_join(fsh_len_s %>% 
                           tidytable::summarise(sfreq = sum(freq), .by = c(year, trimester, area, gear))) %>% 
    # set index for when state > federal lengths
    tidytable::mutate(tfreq = tidytable::replace_na(tfreq, 0),
                      sfreq = tidytable::replace_na(sfreq, 0),
                      state = tidytable::case_when(sfreq > tfreq ~ 1,
                                                   .default = 0)) %>% 
    # filter to state with greater than 30 lengths and fed with less
    tidytable::filter(tfreq < 30,
                      sfreq >= 30) %>% 
    tidytable::select(-sfreq, -tfreq) -> state_test 
  
  # join state test to fed lengths
  fsh_len_comp_f %>% 
    tidytable::left_join(state_test) %>% 
    tidytable::filter(is.na(state)) %>% 
    tidytable::left_join(fsh_len_full_f %>%
                           tidytable::summarise(nsamp = length(unique(haul_join)),
                                                .by = c(year, trimester, area, gear))) %>% 
    tidytable::mutate(state = tidytable::replace_na(state, 0),
                      nsamp = tidytable::replace_na(nsamp, 0)) %>% 
    # bind state data where fed data doesn't exist
    tidytable::bind_rows(fsh_len_comp_s %>% 
                           tidytable::left_join(state_test) %>% 
                           tidytable::filter(!is.na(state)) %>% 
                           tidytable::left_join(fsh_len_full_s %>% 
                                                  tidytable::summarise(tot = sum(freq), .by = c(year, trimester, area, gear)) %>% 
                                                  tidytable::drop_na() %>% 
                                                  tidytable::mutate(nsamp = round(tot / 50)) %>% 
                                                  tidytable::select(-tot))) %>% 
    tidytable::summarise(prop = sum(prop),
                         nsamp = sum(nsamp),
                         .by = c(year, gear, sex, length)) -> fsh_len_comp
  
  fsh_len_comp

}
