## Created 7/19/2017 by Steve Barbeaux 
## This function pulls RACE survey haul and length data for later analysis for all species
## Username and Password are AFSC database username and password
## survey 52=AI,98=EBS Shelf, 78=EBS Slope, and 47 = GOA
## yr (years for data extraction)



Get_DATA<- function(username="",password="",survey = c("47,98,52,78"),yr=c(1982:2016)) {


  require(RODBC)
  require(data.table)

  years<-paste(yr,collapse=",")

  if(R.Version()$arch=="i386"){
    AFSC=odbcConnect("AFSC",username,password)
    }
  else {
    AFSC=odbcConnect("AFSC",username,password,believeNRows=FALSE)
    }

 
  spec<-paste("SELECT RACEBASE.SPECIES.COMMON_NAME, \n",
    "RACEBASE.SPECIES.SPECIES_NAME, \n",
    "RACEBASE.SPECIES.SPECIES_CODE \n",
    "FROM RACEBASE.SPECIES \n",sep="")
    #"WHERE RACEBASE.SPECIES.SPECIES_CODE IN (",noquote(species),")", sep="")


   SN=sqlQuery(AFSC,spec)

   SN <-data.table(subset(SN,!is.na(SN$COMMON_NAME)))

     
   test<-paste("SELECT TO_CHAR(RACEBASE.HAUL.START_TIME, 'yyyy') AS YEAR, \n",
   "RACE_DATA.V_CRUISES.SURVEY_DEFINITION_ID, \n",
   "RACEBASE.HAUL.STRATUM,\n",
   "RACEBASE.HAUL.GEAR_TEMPERATURE AS TEMP, \n",
   "RACEBASE.HAUL.SURFACE_TEMPERATURE AS STEMP, \n",
   "RACEBASE.HAUL.BOTTOM_DEPTH AS DEPTH, \n",
   "RACEBASE.HAUL.END_LATITUDE AS LAT, \n",
   "RACEBASE.HAUL.END_LONGITUDE AS LON \n",
   "FROM RACE_DATA.V_CRUISES \n",
   "INNER JOIN RACEBASE.HAUL \n",
   "ON RACE_DATA.V_CRUISES.CRUISEJOIN = RACEBASE.HAUL.CRUISEJOIN \n",
   "WHERE RACE_DATA.V_CRUISES.SURVEY_DEFINITION_ID IN (",noquote(survey),") \n",
   "AND TO_CHAR(RACEBASE.HAUL.START_TIME, 'yyyy') IN (",noquote(years),") \n",
   "AND RACEBASE.HAUL.ABUNDANCE_HAUL               = 'Y' \n",
   "AND RACEBASE.HAUL.STATIONID IS NOT NULL",sep="")  



  test2<-paste("SELECT TO_CHAR(RACEBASE.HAUL.START_TIME, 'yyyy') AS YEAR, \n",
    "RACEBASE.HAUL.STRATUM,\n",
    "RACEBASE.HAUL.END_LATITUDE AS LAT, \n",
    "RACEBASE.HAUL.END_LONGITUDE AS LON, \n",
    "RACEBASE.CATCH.NUMBER_FISH/(RACEBASE.HAUL.DISTANCE_FISHED*(RACEBASE.HAUL.NET_WIDTH/1000)) AS CPUE, \n",
    "RACE_DATA.V_CRUISES.SURVEY_DEFINITION_ID, \n",
    "RACEBASE.CATCH.SPECIES_CODE \n",
    "FROM RACE_DATA.V_CRUISES \n",
    "INNER JOIN RACEBASE.HAUL \n",
    "ON RACE_DATA.V_CRUISES.CRUISEJOIN = RACEBASE.HAUL.CRUISEJOIN \n",
    "INNER JOIN RACEBASE.CATCH \n",
    "ON RACEBASE.HAUL.CRUISEJOIN = RACEBASE.CATCH.CRUISEJOIN \n",
    "AND RACEBASE.HAUL.HAULJOIN = RACEBASE.CATCH.HAULJOIN \n",
    "WHERE RACE_DATA.V_CRUISES.SURVEY_DEFINITION_ID IN (",noquote(survey),") \n",
    "AND TO_CHAR(RACEBASE.HAUL.START_TIME, 'yyyy') IN (",noquote(years),") \n",
    "AND RACEBASE.HAUL.ABUNDANCE_HAUL               = 'Y' \n", sep="") 
   #"AND RACEBASE.CATCH.SPECIES_CODE IN (",noquote(species),")", sep="")

  test3<-paste ("SELECT TO_CHAR(RACEBASE.HAUL.START_TIME, 'yyyy') AS YEAR, \n",
    "RACEBASE.HAUL.STRATUM,\n",
    "RACEBASE.LENGTH.LENGTH, \n",
    "RACEBASE.LENGTH.FREQUENCY, \n",
    "RACEBASE.LENGTH.SEX, \n",
    "RACEBASE.HAUL.GEAR_TEMPERATURE AS TEMP, \n",
    "RACEBASE.HAUL.SURFACE_TEMPERATURE AS STEMP, \n",
    "RACEBASE.HAUL.BOTTOM_DEPTH AS DEPTH, \n",
    "RACEBASE.HAUL.END_LATITUDE AS LAT, \n",
    "RACEBASE.HAUL.END_LONGITUDE AS LON, \n",
    "RACEBASE.LENGTH.SPECIES_CODE, \n",
    "RACE_DATA.V_CRUISES.SURVEY_DEFINITION_ID \n",  
    "FROM RACE_DATA.V_CRUISES \n",
    "INNER JOIN RACEBASE.HAUL \n",
    "ON RACE_DATA.V_CRUISES.CRUISEJOIN = RACEBASE.HAUL.CRUISEJOIN \n",
    "INNER JOIN RACEBASE.LENGTH \n",
    "ON RACEBASE.HAUL.CRUISEJOIN                    = RACEBASE.LENGTH.CRUISEJOIN \n",
    "AND RACEBASE.HAUL.HAULJOIN                     = RACEBASE.LENGTH.HAULJOIN \n",
    "WHERE RACE_DATA.V_CRUISES.SURVEY_DEFINITION_ID IN (",noquote(survey),") \n",
    "AND TO_CHAR(RACEBASE.HAUL.START_TIME, 'yyyy') IN (",noquote(years),") \n", 
    "AND RACEBASE.HAUL.ABUNDANCE_HAUL               = 'Y' \n",sep="")
    #"AND RACEBASE.LENGTH.SPECIES_CODE IN (",noquote(species),")", sep="")

  location      <- data.table(sqlQuery(AFSC,test))
  location_poll <- data.table(sqlQuery(AFSC,test2))
  length        <- data.table(sqlQuery(AFSC,test3))

  l2<-length[,list(NUMBER=sum(FREQUENCY)),by=c("SURVEY_DEFINITION_ID","SPECIES_CODE")]
  
  spc<-l2[NUMBER>=2000]

  spc<-unique(spc$SPECIES_CODE)

  length<-length[SPECIES_CODE %in% spc]
  location_poll<-location_poll[SPECIES_CODE %in% spc]
  SN<- SN[SPECIES_CODE %in% spc]

  SN<-SN[order(COMMON_NAME),]


  #length        <- length[SEX<3]
  odbcClose(AFSC)

  surveys<-as.numeric(strsplit(survey,",")[[1]])
  length1<-vector("list",length=length(surveys))
  location1<-vector("list",length=length(surveys))
  location_poll1<-vector("list",length=length(surveys))
  g <- 0
## if survey is EBS, exlcude far north regions.
  if(98 %in% surveys){
    g<-g+1
    length1[[g]] <- length[SURVEY_DEFINITION_ID==98 & !is.na(STRATUM) & STRATUM<63]
    location1[[g]]      <- location[SURVEY_DEFINITION_ID==98 & !is.na(STRATUM) & STRATUM<63]
    location_poll1[[g]] <- location_poll[SURVEY_DEFINITION_ID==98 & !is.na(STRATUM) & STRATUM<63]
  }

  if(47 %in% surveys){
    g<-g+1
    length1[[g]]<-length[SURVEY_DEFINITION_ID==47]
    location1[[g]]<-location[SURVEY_DEFINITION_ID==47]
    location_poll1[[g]]<-location_poll[SURVEY_DEFINITION_ID==47]
    ## limiting data to western GOA and to depths < 500m to be consistent across time
    location1[[g]]$SL1=trunc(location1[[g]]$STRATUM/10)-(trunc(location1[[g]]$STRATUM/100)*10)
    length1[[g]]$SL1=trunc(length1[[g]]$STRATUM/10)-(trunc(length1[[g]]$STRATUM/100)*10)
    location_poll1[[g]]$SL1=trunc(location_poll1[[g]]$STRATUM/10)-(trunc(location_poll1[[g]]$STRATUM/100)*10)

    length1[[g]]        <- length1[[g]][!is.na(STRATUM)&STRATUM<400 & YEAR > 1990]
    location1[[g]]      <- location1[[g]][!is.na(STRATUM)&STRATUM<400 & YEAR > 1990]
    location_poll1[[g]] <- location_poll1[[g]][!is.na(STRATUM)&STRATUM<400 & YEAR > 1990]

    length1[[g]][length1[[g]]$SL1>=4]$SURVEY_DEFINITION_ID <-48
    location1[[g]][location1[[g]]$SL1>=4]$SURVEY_DEFINITION_ID <-48
    location_poll1[[g]][location_poll1[[g]]$SL1>=4]$SURVEY_DEFINITION_ID <-48

    length1[[g]][length1[[g]]$SL1==1]$SURVEY_DEFINITION_ID <-46
    location1[[g]][location1[[g]]$SL1==1]$SURVEY_DEFINITION_ID <-46
    location_poll1[[g]][location_poll1[[g]]$SL1==1]$SURVEY_DEFINITION_ID <-46

    length1[[g]]$SL1<-NULL
    location1[[g]]$SL1<-NULL
    location_poll1[[g]]$SL1<-NULL
  
    }

## for SLope survey exclue 2000 from all plots
  if(78  %in% surveys){
    g<-g+1
    length1[[g]]        <- length[SURVEY_DEFINITION_ID==78 & YEAR!=2000]
    location1[[g]]      <- location[SURVEY_DEFINITION_ID==78 & YEAR!=2000]
    location_poll1[[g]] <- location_poll[SURVEY_DEFINITION_ID==78 & YEAR!=2000]
    }

  if(52 %in% surveys){
    g <- g+1
    length1[[g]]        <- length[SURVEY_DEFINITION_ID==52 & !is.na(STRATUM) & STRATUM<800 & YEAR > 1991]
    location1[[g]]      <- location[SURVEY_DEFINITION_ID==52 & !is.na(STRATUM) & STRATUM<800 & YEAR > 1991]
    location_poll1[[g]] <- location_poll[SURVEY_DEFINITION_ID==52 & !is.na(STRATUM) & STRATUM<800 & YEAR > 1991]
  }

  length<-data.table(do.call(rbind,length1))
  location<-data.table(do.call(rbind,location1))
  location_poll<-data.table(do.call(rbind,location_poll1))
##Exclude null locations and transform to all positive longitudes
  location      <-location[!is.na(LON)]
  location_poll <-location_poll[!is.na(LON)]
  length        <-length[!is.na(LON)]
     
  location$LON[location$LON<0]           <-  360 + location$LON[location$LON<0]
  location_poll$LON[location_poll$LON<0] <-  360 + location_poll$LON[location_poll$LON<0]
  length$LON[length$LON<0]               <-  360 + length$LON[length$LON<0]
      
## rounding bottom depth to nearest 0.5 meters and temperature to nearest 0.1 degree to simplify calculations
  location$DEPTHR <- round(location$DEPTH)
  location$TEMPR  <- round(location$TEMP,1)
      
  length$TEMPR    <- round(length$TEMP,1)
  length$DEPTHR   <- round(length$DEPTH)
  
  length          <- merge(length,location_poll,by = c("YEAR","LON","LAT","STRATUM","SURVEY_DEFINITION_ID", "SPECIES_CODE"))

  length          <-subset(length,!is.na(length$CPUE))

  data<-list(length=length,location=location, location_poll=location_poll, SN=SN)
  return(data)
}
