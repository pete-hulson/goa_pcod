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
  
  fsh_len_comp <- get_fsh_len4age(new_year)

  # age data ----
  fsh_age <- vroom::vroom(here::here(new_year, 'data', 'raw', 'fish_age_domestic.csv')) %>% 
    # filter to years post-2007 (as default)
    tidytable::filter(year > st_yr)

  # compute age comps ----
  
  fsh_len_comp %>% 
    tidytable::mutate(freq = prop * 10000) %>% 
    # filter to years post-2007 (as default)
    tidytable::filter(year > st_yr) -> fsh_len_exp

  

  # test for single iteration
  Adata_test <- fsh_age %>% 
    tidytable::filter(gear == 'trawl',
                      year == 2008)
  Ldata_test <- fsh_len_exp %>% 
    tidytable::filter(gear == 'trawl',
                      year == 2008)
  
  Adata<-age[GEAR==gears[1]]
  Ldata<-ldata[GEAR==gears[1]]
  year<-sort(unique(age1$YEAR))[1]
  

  Adata_test <- Adata_test %>% 
    tidytable::mutate(age = tidytable::case_when(age > 20 ~ 20,
                                                 .default = age))
  
  
  Adata$AGE1<-Adata$AGE
  Adata$AGE1[Adata$AGE>20] <- 20
  
  rb.age1_test <- Adata_test %>% 
    tidytable::filter(sex == 'M') %>% 
    tidytable::select(age, tl = length)
  rb.age2_test <- Adata_test %>% 
    tidytable::filter(sex == 'F') %>% 
    tidytable::select(age, tl = length)
  rb.len1_test <- Ldata_test %>% 
    tidytable::filter(sex == 'M') %>% 
    tidytable::select(tl = length, freq)
  rb.len2_test <- Ldata_test %>% 
    tidytable::filter(sex == 'F') %>% 
    tidytable::select(tl = length, freq)
  
  rb.age1<-subset(Adata,Adata$YEAR==year&Adata$SEX=="M")
  rb.age2<-subset(Adata,Adata$YEAR==year&Adata$SEX=="F")
  rb.len1<-subset(Ldata,Ldata$YEAR==year&Ldata$SEX=="M")
  rb.len2<-subset(Ldata,Ldata$YEAR==year&Ldata$SEX=="F")

  rb.age1<-data.frame(age=rb.age1$AGE1,tl=rb.age1$LENGTH)
  rb.age2<-data.frame(age=rb.age2$AGE1,tl=rb.age2$LENGTH)

  rb.len1<-aggregate(list(FREQ=rb.len1$FREQUENCY),by=list(LENGTH=rb.len1$LENGTH),FUN=sum)
  rb.len2<-aggregate(list(FREQ=rb.len2$FREQUENCY),by=list(LENGTH=rb.len2$LENGTH),FUN=sum)

  
  rb.len1_test %>% 
    tidytable::mutate(freq = floor(freq)) %>% 
    tidytable::uncount(freq) %>% 
    tidytable::mutate(age = NA) %>% 
    tidytable::select(age, tl) -> rb.len1_test
  rb.len2_test %>% 
    tidytable::mutate(freq = floor(freq)) %>% 
    tidytable::uncount(freq) %>% 
    tidytable::mutate(age = NA) %>% 
    tidytable::select(age, tl) -> rb.len2_test
  
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

  rb.age1_test <- rb.age1_test %>% 
    tidytable::bind_rows(tidytable::expand_grid(age = 1,
                                                tl = c(min(rb.len1_test$tl),(min(rb.age1_test$tl)-1))))
  rb.age2_test <- rb.age2_test %>% 
    tidytable::bind_rows(tidytable::expand_grid(age = 1,
                                                tl = c(min(rb.len2_test$tl),(min(rb.age2_test$tl)-1))))
  
  rb.age1=rbind(rb.age1,expand.grid(age=1,tl=c(min(rb.len1$tl),(min(rb.age1$tl)-1))))
  rb.age2=rbind(rb.age2,expand.grid(age=1,tl=c(min(rb.len2$tl),(min(rb.age2$tl)-1))))
  

  rb.age1.1_test <- FSA::lencat(x = ~tl, data = rb.age1_test, startcat = 5, w = 5)
  rb.age2.1_test <- FSA::lencat(x = ~tl, data = rb.age2_test, startcat = 5, w = 5)

  rb.age1.1<-lencat(x=~tl,data=rb.age1,startcat=5,w=5)
  rb.age2.1<-lencat(x=~tl,data=rb.age2,startcat=5,w=5)
  
  rb.key1_test <- prop.table(table(rb.age1.1_test$LCat, rb.age1.1_test$age), margin = 1)
  rb.key2_test <- prop.table(table(rb.age2.1_test$LCat, rb.age2.1_test$age), margin = 1)
  
  rb.raw1 <- table(rb.age1.1$LCat,rb.age1.1$age)
  rb.key1 <- prop.table(rb.raw1,margin=1)
  rb.raw2 <- table(rb.age2.1$LCat,rb.age2.1$age)
  rb.key2 <- prop.table(rb.raw2,margin=1)
  
  
  rb.len1.1_test <- FSA::alkIndivAge(rb.key1_test, formula = ~tl, data = rb.len1_test) 
  rb.len2.1_test <- FSA::alkIndivAge(rb.key2_test, formula = ~tl, data = rb.len2_test) 
  
  rb.len1.1 <- alkIndivAge(rb.key1,formula=~tl,data=rb.len1) 
  rb.len2.1 <- alkIndivAge(rb.key2,formula=~tl,data=rb.len2) 
  
  AL<-list(len1=rb.len1.1,len2=rb.len2.1)

  al <- list(len1 = rb.len1.1_test, len2 = rb.len2.1_test)

  

  
  
  
  
  
  
  
  Adata_test %>% 
    tidytable::mutate(age = tidytable::case_when(age > 20 ~ 20,
                                                 .default = age)) %>% 
    tidytable::select(sex, age, tl = length) -> adata_test

  Ldata_test %>% 
    tidytable::select(sex, tl = length, freq) %>% 
    tidytable::mutate(freq = floor(freq)) %>% 
    tidytable::uncount(freq) %>% 
    tidytable::mutate(age = NA) %>% 
    tidytable::select(sex, age, tl) -> ldata_test
  
  adata_test %>% 
    tidytable::bind_rows(ldata_test %>% 
                           tidytable::summarise(tl = min(tl), .by = sex) %>% 
                           tidytable::bind_rows(adata_test %>% 
                                                  tidytable::summarise(tl = min(tl) - 1, .by = sex)) %>% 
                           tidytable::mutate(age = 1)) %>% 
    FSA::lencat(x = ~tl, data = ., startcat = 5, w = 5) -> adata_test

  
  
  
  
  
  
  fsh_age %>% 
    tidytable::mutate(age = tidytable::case_when(age > 20 ~ 20,
                                                 .default = age)) %>% 
    tidytable::select(year, gear, sex, age, tl = length) -> adata_alk

  fsh_len_exp %>% 
    tidytable::select(year, gear, sex, tl = length, freq) %>% 
    tidytable::mutate(freq = floor(freq)) %>% 
    tidytable::uncount(freq) %>% 
    tidytable::mutate(age = NA) %>% 
    tidytable::select(year, gear, sex, age, tl) -> ldata_alk
  
  adata_alk %>% 
    tidytable::bind_rows(ldata_alk %>% 
                           tidytable::summarise(tl = min(tl), .by = c(year, gear, sex)) %>% 
                           tidytable::bind_rows(adata_alk %>% 
                                                  tidytable::summarise(tl = min(tl) - 1, .by = c(year, gear, sex))) %>% 
                           tidytable::mutate(age = 1)) %>% 
    FSA::lencat(x = ~tl, data = ., startcat = 5, w = 5) -> adata_alk
  

  years <- sort(unique(fsh_age$year))
  gears <- unique(fsh_age$gear)
  
  
  
  
  
  get_alk <- function(adata_alk, ldata_alk){
    
    sexes <- c('M', 'F')
    
    purrr::map(1:length(sexes), ~FSA::alkIndivAge(prop.table(table(subset(adata_alk$LCat, adata_alk$sex == sexes[.]), 
                                                                   subset(adata_alk$age, adata_alk$sex == sexes[.])), 
                                                             margin = 1), 
                                                  formula = ~tl, 
                                                  data = subset(ldata_alk, ldata_alk$sex == sexes[.]))) %>% 
      tidytable::map_df(., ~as.data.frame(.x), .id = "sex_num") %>% 
      select(sex = sex_num, age, length = tl)
  }

  adata_alk_test <- adata_alk %>% 
    tidytable::filter(year == years[1],
                      gear == gears[1]) %>% 
    tidytable::select(sex, age, tl, LCat)

  ldata_alk_test <- ldata_alk %>% 
    tidytable::filter(year == years[1],
                      gear == gears[1]) %>% 
    tidytable::select(sex, age, tl)

  t <- get_alk(adata_alk_test, ldata_alk_test)
  adata_alk_test %>% 
  print(n = 50)


  rb.key1_test <- prop.table(table(rb.age1.1_test$LCat, rb.age1.1_test$age), margin = 1)
  rb.key2_test <- prop.table(table(rb.age2.1_test$LCat, rb.age2.1_test$age), margin = 1)
  

  rb.len1.1_test <- FSA::alkIndivAge(rb.key1_test, formula = ~tl, data = rb.len1_test) 
  rb.len2.1_test <- FSA::alkIndivAge(rb.key2_test, formula = ~tl, data = rb.len2_test) 

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
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
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
}
