
library(RODBC)
library(mgcv)
library(FSA)
 library(nlstools)
 library(data.table)
 library(ggplot2)
 library(sizeMat)
library(devtools)
library(r4ss)
library(lubridate)
library(rgdal)

usernameAKFIN = *****
passwordAKFIN = *****

usernameAFSC = *****
passwordAFSC = *****

AFSC=odbcConnect("AFSC",usernameAFSC,passwordAFSC,believeNRows=FALSE)
CHINA=odbcConnect("AKFIN",usernameAKFIN,passwordAKFIN,believeNRows=FALSE)


#    DEFINE ALL CONSTANTS FOR THIS RUN

# this assumes that the FUNCTIONS subdirectory is in the working directory
# working_dir <- "C:\\Working Folder for DOCS\\2018_Assessments"
working_dir <- "C:/WORKING_FOLDER/2021 Stock Assessments/2021 Pacific cod/DATA"

## location of state port sampling data

State_dat_loc="C:/WORKING_FOLDER/2021 Stock Assessments/2021 Pacific cod/DATA/ALL_STATE_LENGTHS.csv"

# the most recent year of data to be used
new_SS_dat_year <- 2021

final_year <- new_SS_dat_year

# the FMP area for this stock
sp_area <- "'GOA'"

# the GOA FMP sub-areas in the COUNCIL.COMPREHENSIVE_BLEND_CA database table
fsh_sp_area <- "'CG','PWSI','SE','SEI','WG','WY'"

# species label for AKFIN
fsh_sp_label <- "'PCOD'"

# the fishery species code(s) for this stock/these stocks
fsh_sp_str <- "202"

# year in which to start the fishery data
fsh_start_yr <- 1977

# fraction of the year that the fishery length- and weight-at-age calculations are done
fsh_frac <- 0.5


# length bins to use for fsh and srv length comp data
bin_width <- 1
min_size <- 0.5
max_size <- 116.5  # less than 1% of the fish in each year are 105 cm or larger (max less than 0.6%)
len_bins <- seq(min_size,max_size,bin_width)


new_year = new_SS_dat_year

AUXFCOMP =3

if(AUXFCOMP > 0){
 auxFLCOMP<-LENGTH_BY_CATCH_GOA(fsh_sp_str=fsh_sp_str ,fsh_sp_label = fsh_sp_label,ly=new_year)
 if(AUXFCOMP==1)auxFLCOMP<-auxFLCOMP[[1]]  ## do not use state data
 if(AUXFCOMP==2)auxFLCOMP<-auxFLCOMP[[2]]  ## use state data when more than observer data
 if(AUXFCOMP==3)auxFLCOMP<-auxFLCOMP[[3]]  ## use state data only to fill holes


 auxFLCOMP$FltSrv<-1
 auxFLCOMP$FltSrv[auxFLCOMP$GEAR=="LONGLINE"]<-2
 auxFLCOMP$FltSrv[auxFLCOMP$GEAR=="POT"]<-3

auxflCOMP1=data.frame(Year=auxFLCOMP$YEAR,Seas=rep(1, nrow(auxFLCOMP)),FltSrv=auxFLCOMP$FltSrv,gender=rep(0, nrow(auxFLCOMP)),Part=rep(0, nrow(auxFLCOMP)),Nsamp=auxFLCOMP$Nsamp,auxFLCOMP[,4:(ncol(auxFLCOMP)-1)])
names(auxflCOMP1) <- c("Year","Seas","FltSrv","Gender","Part","Nsamp",len_bins)

    fishLCOMP=subset(FISHLCOMP,FISHLCOMP$Year<1991)
    fishLCOMP<-rbind(fishLCOMP,auxflCOMP1)
    FISHLCOMP<-fishLCOMP[order(fishLCOMP$FltSrv,fishLCOMP$Year),]

  }

print("Fisheries LCOMP2 done")



## function for getting Length_by_CATCH Would need to alter the draw for AI region in SQl draw

  
LENGTH_BY_CATCH_GOA<-function(fsh_sp_str=202 ,fsh_sp_label = "'PCOD'",ly=new_year,statdat=State_dat_loc){
	require(RODBC)
	require(data.table)
	require(reshape2)
  require(rgdal)
  require(dplyr)
  require(lubridate)


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

  	Dspcomp=data.table(sqlQuery(AFSC,test))

    Dspcomp$GEAR1<-"TRAWL"
    Dspcomp$GEAR1[Dspcomp$GEAR==2]<-"POT"
    Dspcomp$GEAR1[Dspcomp$GEAR==3]<-"LONGLINE"
    Dspcomp$GEAR<-Dspcomp$GEAR1


    HJ <- Dspcomp[,list(Nsamp=length(unique(HAUL_JOIN))),by="YEAR,GEAR"]
    HJA <- Dspcomp[,list(Nsamp=length(unique(HAUL_JOIN))),by="YEAR,GEAR,AREA,TRIMESTER"]





    Dspcomp$HAUL1<-as.character(paste(Dspcomp$CRUISE,Dspcomp$PERMIT,Dspcomp$HAUL,sep="_"))

   

WED<-function(x=Dspcomp$HDAY[1])
  { y<-data.table(

    weekday=weekdays(x),
    wed=ceiling_date(x, "week"),  
    plus= ifelse(weekdays(x) %in% c("Sunday"), 6, -1),
    YR=year(x))

    y$next_saturday<-date(y$wed)+y$plus
    y[YR<1993]$next_saturday<-date(y[YR<1993]$wed)

    y$yr2<-year(y$next_saturday)
    y[YR!=yr2]$next_saturday<-date(paste0(y[YR!=yr2]$YR,"-12-31"))

   
    return(y$next_saturday)
}


Dspcomp$WED<-WED(Dspcomp$HDAY)


  t5<-Dspcomp[,list(T2FREQ=sum(FREQ)),by=c('HAUL_JOIN','NUMB')]
  t5$KEEP=1
  t5[T2FREQ<10]$KEEP<-0
  t6<-t5[KEEP==1]

Dspcomp<-merge(t6,Dspcomp,all.x=T)


  	

    Dspcomp$AREA<-trunc(Dspcomp$AREA/10)*10
  	


  	x<-Dspcomp[,list(N1=min(NUMB), HFREQ=sum(FREQ)),by=c("YEAR,WED,TRIMESTER,AREA,HAUL1,GEAR")] ## get individual haul extrapolated numbers of fish
  	y<-x[,list(N2=sum(N1),TFREQ=sum(HFREQ)),by=c("YEAR,WED,AREA,TRIMESTER,GEAR")]  ## get total observed numbers of fish per year, area,state,  and gear
  	z<-Dspcomp[,list(FREQ=sum(FREQ)),by=c("YEAR,WED,TRIMESTER,AREA,HAUL1,LENGTH,GEAR")] ## number of fish by length bin, haul, gear and year

  	z2<-merge(x,y,all=T,by=c("YEAR","WED","TRIMESTER","AREA","GEAR"))
    z3<-merge(z,z2,all=T,by=c("YEAR","WED","TRIMESTER","AREA","GEAR","HAUL1"))
  	
    z3$PROP<-((z3$FREQ/z3$HFREQ)*z3$N1)/z3$N2  ## for each length bin, haul, gear and year  calculated the proportion of fish 

  	#zz6<-aggregate(list(PROP=zz5$PROP),by=list(YEAR=zz5$YEAR,TRIMESTER=zz5$TRIMESTER, AREA=zz5$AREA,GEAR=zz5$GEAR,STATE=zz5$STATE,LENGTH=zz5$LENGTH),FUN=sum)  ## add the proportions together for each length bin, year, and gear.
 	  z4<-z3[YEAR>=1991]
  	z4<-z4[order(GEAR,YEAR,LENGTH),]
  	D_SPCOMP<-z4[,list(PROP=sum(PROP)),by=c("YEAR,WED,TRIMESTER,AREA,GEAR,LENGTH")]


 D_SPCOMP_161<-D_SPCOMP[,list(P1=sum(PROP)),by=c("YEAR,GEAR,AREA,TRIMESTER,LENGTH")]
 D_SPCOMP_161x<-D_SPCOMP[,list(tot=sum(PROP)),by=c("YEAR,GEAR,AREA,TRIMESTER")]
 D_SPCOMP_161<-merge(D_SPCOMP_161,D_SPCOMP_161x,all=T)
 D_SPCOMP_161$PROP<-D_SPCOMP_161$P1/D_SPCOMP_161$tot

  grid<-data.table(expand.grid(YEAR=sort(unique(D_SPCOMP_161$YEAR)),GEAR=unique(D_SPCOMP_161$GEAR),AREA=unique(D_SPCOMP_161$AREA),TRIMESTER=c(1:3),LENGTH=seq(1,117,1)))

  D_SPCOMP_161<-merge(D_SPCOMP_161,grid,all=T,by=c("YEAR","GEAR","AREA","TRIMESTER","LENGTH"))


  

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


    CATCH<-data.table(sqlQuery(CHINA,test))

    CATCH$WED<-date(CATCH$WED)
    CATCH$STATE[is.na(CATCH$STATE)]<-"F"
   

    CATCH$GEAR1<-"TRAWL"
    CATCH$GEAR1[CATCH$GEAR=="POT"]<-"POT"
    CATCH$GEAR1[CATCH$GEAR=="HAL"]<-"LONGLINE"
    CATCH$GEAR1[CATCH$GEAR=="JIG"]<-"LONGLINE"
    CATCH$GEAR<-CATCH$GEAR1

  	CATCH$AREA<-trunc(CATCH$AREA/10)*10
  
  	y2 <- CATCH[,list(TOTAL=sum(TONS)),by="YEAR"]                                                 ## get total observed numbers of fish per year and gear
  	z2 <- CATCH[,list(TONS=sum(TONS)),by=c("YEAR,WED,TRIMESTER,AREA,GEAR")] ## get total number of measured fish by haul, gear, and year

  	x2 <- merge(y2,z2,all=T)
  	x2$CATCH_PROP<-x2$TONS/x2$TOTAL


  t1<-Dspcomp[,list(TFREQ=sum(FREQ)),by=c('TRIMESTER','YEAR','AREA','GEAR1')]
  names(t1)[4]<-'GEAR'
  
  D_LENGTH<-merge(D_SPCOMP,x2,all=T, by=c("YEAR","WED","TRIMESTER","AREA","GEAR"))

	D_LENGTH$LENGTH[D_LENGTH$LENGTH>116]<-117
  
  D_LENGTH$PROP1<- D_LENGTH$PROP*D_LENGTH$CATCH_PROP
	DLENGTH<-D_LENGTH[!is.na(PROP1)]
	
  DLENGTH<-DLENGTH[,list(PROP1=sum(PROP1)),by=c("YEAR,TRIMESTER,AREA,GEAR,LENGTH")]
  
 t2<-merge(DLENGTH,t1)
 t3<-t2[TFREQ>=30]
 t3[,'TFREQ':=NULL]
  
	grid<-data.table(expand.grid(YEAR=sort(unique(t3$YEAR)),TRIMESTER=c(1:3),AREA=unique(t3$AREA),GEAR=unique(t3$GEAR),LENGTH=seq(1,117,1)))

	DLENGTH1<-merge(grid,t3,all=T,by=c("YEAR","TRIMESTER","AREA","GEAR","LENGTH"))

  
	grid<-data.table(expand.grid(YEAR=sort(unique(DLENGTH$YEAR)),TRIMESTER=c(1:3),AREA=unique(DLENGTH$AREA),GEAR=unique(DLENGTH$GEAR),LENGTH=seq(1,117,1)))

	DLENGTH1<-merge(grid,DLENGTH,all.x=T,by=c("YEAR","TRIMESTER","AREA","GEAR","LENGTH"))

  DLENGTH1[is.na(PROP1)]$PROP1<-0

  DLENGTH_NS<-DLENGTH1[,list(PROP=sum(PROP1)),by=c("YEAR,GEAR,LENGTH")]
  SS3_DLENGTH_NS <- reshape2::dcast(DLENGTH_NS,formula=GEAR+YEAR~LENGTH,value.var="PROP")
 SS3_DLENGTH_NS <-merge(SS3_DLENGTH_NS,HJ,by=c("YEAR","GEAR"),all.x=T)


 ## pulling state data from file.
	
  SLENGTH<-read.csv(statdat)

  SLENGTH<-data.table(SLENGTH[,1:9])
  SLENGTH<-SLENGTH[LENGTH>0]
  SLENGTH$AREA<-trunc(SLENGTH$AREA/10)*10

    SLENGTH$GEAR1<-"TRAWL"
    SLENGTH$GEAR1[SLENGTH$GEAR==91]<-"POT"
    SLENGTH$GEAR1[SLENGTH$GEAR %in% c(5,26,61)]<-"LONGLINE"
    SLENGTH$GEAR<-SLENGTH$GEAR1

    SLENGTH$LENGTH[SLENGTH$LENGTH>116]<-117
   
  SLENGTH$TRIMESTER<-1
  SLENGTH$TRIMESTER[SLENGTH$MONTH>4&SLENGTH$MONTH<=8]<-2
  SLENGTH$TRIMESTER[SLENGTH$MONTH>8]<-3

  S1<-SLENGTH[,list(SFREQ=sum(FREQ)),by=c('TRIMESTER','YEAR','AREA','GEAR1')]
  names(S1)[4]<-'GEAR'


  SHJA <- SLENGTH[,list(TOT=sum(FREQ)),by="YEAR,GEAR,TRIMESTER,AREA"]
  SHJA <- SHJA[order(GEAR,AREA,YEAR),]
  SHJA$Nsamp <- round(SHJA$TOT/50)
  SHJA[, TOT:=NULL]

  Sx<-SLENGTH[,list(FREQ=sum(FREQ)),by="YEAR,TRIMESTER,AREA,GEAR,LENGTH"]
  Sy<-SLENGTH[,list(TOTAL=sum(FREQ)),by="YEAR,TRIMESTER,AREA,GEAR"]

  Sz<-merge(Sx,Sy,by=c("YEAR","TRIMESTER","AREA","GEAR"))
  Sz$PROP<-Sz$FREQ/Sz$TOTAL
  Sz<-subset(Sz,select=-c(TOTAL,FREQ))

 S2<-merge(Sz,S1)
 S3<-S2[SFREQ>=30]
 S3[,'SFREQ':=NULL]
 
  
 grid<-data.table(expand.grid(YEAR=sort(unique(SLENGTH$YEAR)),TRIMESTER=c(1:3),AREA=unique(SLENGTH$AREA),GEAR=unique(SLENGTH$GEAR),LENGTH=seq(1,117,1)))
 
 Sz<-merge(grid,S3,all=T,by=c("YEAR","TRIMESTER","AREA","GEAR","LENGTH"))


 SCATCH<-x2[,list(CATCH_PROP=sum(CATCH_PROP)),by=c("YEAR,TRIMESTER,AREA,GEAR")]

 S_LENGTH<-merge(Sz,SCATCH,all=T, by=c("YEAR","TRIMESTER","AREA","GEAR"))
 S_LENGTH<-S_LENGTH[!is.na(LENGTH)]
 S_LENGTH[is.na(S_LENGTH)]<-0

 S_LENGTH$PROP1<- S_LENGTH$PROP*S_LENGTH$CATCH_PROP

 SLENGTH1<-S_LENGTH[,list(PROP1=sum(PROP1)),by="YEAR,TRIMESTER,AREA,GEAR,LENGTH"]

## take the one that has the most lengths recorded

 
 yy=merge(S1,t1,all=T)

 yy[is.na(yy)]<-0
 yy$STATE<-0
 yy$STATE[yy$SFREQ>yy$TFREQ]<-1             ## test to see which dataset has more lengths measured
 yy1<-yy[STATE==1]
 yy1<-yy1[,c(1:4,7)]

 S_LENGTHx<-merge(SLENGTH1,yy1,all.x=T,by=c("YEAR","TRIMESTER","AREA","GEAR"))
 D_LENGTHx<-merge(DLENGTH1,yy1,all.x=T,by=c("YEAR","TRIMESTER","AREA","GEAR"))

 S_LENGTHx<-S_LENGTHx[!is.na(STATE)]

 D_LENGTHx<-D_LENGTHx[is.na(STATE)]

 

 D_LENGTHx<-merge(D_LENGTHx,HJA,by=c("YEAR","TRIMESTER","AREA","GEAR"),all.x=T)
 S_LENGTHx<-merge(S_LENGTHx,SHJA,by=c("YEAR","TRIMESTER","AREA","GEAR"),all.x=T)

 D_LENGTHx[is.na(D_LENGTHx)]<-0
 S_LENGTHx[is.na(S_LENGTHx)]<-0 

 LENGTHx<-rbind(D_LENGTHx,S_LENGTHx)
 
 DLENGTH_S1<-LENGTHx[,list(PROP=sum(PROP1),Nsamp=sum(Nsamp)),by="YEAR,GEAR,LENGTH"]

 NSAMP<-DLENGTH_S1[,list(Nsamp=mean(Nsamp)),by="GEAR,YEAR"]

 SS3_DLENGTH_S1 <- reshape2::dcast(DLENGTH_S1,formula=GEAR+YEAR~LENGTH,value.var="PROP")
 SS3_DLENGTH_S1<-merge(NSAMP,SS3_DLENGTH_S1,by=c("GEAR","YEAR"),all.y=T)

  #write.csv(SS3_DLENGTH_S1,"SS3_DLENGTH_S1.csv")

 
## fill in the blanks code  Fill in years,trimester,area,gear with state port data if federal data are missing

  yy3<-yy[TFREQ<30&SFREQ>=30]
  yy3<-yy3[,c(1:4,7)]


  S_LENGTHx<-merge(SLENGTH1,yy3,all.x=T,by=c("YEAR","TRIMESTER","AREA","GEAR"))
  D_LENGTHx<-merge(DLENGTH1,yy3,all.x=T,by=c("YEAR","TRIMESTER","AREA","GEAR"))

  S_LENGTHx<-S_LENGTHx[!is.na(STATE)]

  D_LENGTHx<-D_LENGTHx[is.na(STATE)]

   D_LENGTHx<-merge(D_LENGTHx,HJA,by=c("YEAR","TRIMESTER","AREA","GEAR"),all.x=T)
 S_LENGTHx<-merge(S_LENGTHx,SHJA,by=c("YEAR","TRIMESTER","AREA","GEAR"),all.x=T)

 D_LENGTHx[is.na(D_LENGTHx)]<-0
 S_LENGTHx[is.na(S_LENGTHx)]<-0 

  LENGTHx<-rbind(D_LENGTHx,S_LENGTHx)
 
  DLENGTH_S2<-LENGTHx[,list(PROP=sum(PROP1),Nsamp=sum(Nsamp)),by="YEAR,GEAR,LENGTH"]
  NSAMP<-DLENGTH_S2[,list(Nsamp=mean(Nsamp)),by="GEAR,YEAR"]

  SS3_DLENGTH_S2 <- reshape2::dcast(DLENGTH_S2,formula=GEAR+YEAR~LENGTH,value.var="PROP")
  SS3_DLENGTH_S2<-merge(NSAMP,SS3_DLENGTH_S2,by=c("GEAR","YEAR"),all.y=T)

 # write.csv(SS3_DLENGTH_S2,"SS3_DLENGTH_S2.csv")

  LENGTHS<-list(NO_STATE=SS3_DLENGTH_NS,STATE_HIGH=SS3_DLENGTH_S1,STATE_HOLE=SS3_DLENGTH_S2)
  return(LENGTHS)
  }


