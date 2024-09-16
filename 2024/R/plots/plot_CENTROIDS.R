  ## plot_CENTROIDS() by Steven Barbeaux 7/19/2017
  ## Funtion to plot various centers of gravity (CPUE weighted means, centroids) for Alaska Fisheries
  ## Science Center (AFSC) bottom trawl surveys. 
  ## survey(52= AI , 98=EBS Shelf, 78= EBS slope, 46 = Western GOA, 47 = Central GOA, 48= Eastern GOA, 
  ## species (RACE species code)
  ## plotT  (1=D&T by shelf temp, 2= Location by shelf temp, 3= D&T for all years, 4= location for all years, 
  ## 5=D&T for each year, 6=location for each year, 7=D&T by sex and year, 8=location by sex and year, 9=longitude and depth for all years)
  ## AREA_W (T= uses stratum area weights, F = does not use stratum area weights) only applicable for GOA 
  ## 

  plot_CENTROIDS<-function(data=results,survey,species,plotT,AREA_W) {
      
      
      location      <- data$location[SURVEY_DEFINITION_ID==survey]
      length        <- data$length[SURVEY_DEFINITION_ID==survey & SPECIES_CODE==species]
      location_poll <- data$location_poll[SURVEY_DEFINITION_ID==survey & SPECIES_CODE==species]
      SN            <- data$SN[SPECIES_CODE==species]
      
      cn      <- paste(SN$COMMON_NAME)
      sn      <- paste(SN$SPECIES_NAME)
      plot.title <- bquote(.(cn)~" ("~italic(.(sn))~") ")
      
      validate(
        need(sum(length$FREQUENCY)>=500,"Not enough measurements for this species and survey"),
        need(length(unique(length$YEAR))>=2,"Not enough measurements for this species and survey")
        )
      
  
     
      length          <- data.table(merge(length,location_poll,by = c("YEAR","LON","LAT","SURVEY_DEFINITION_ID","STRATUM", "SPECIES_CODE","CPUE")))
     
      
      x<-quantile(length$LENGTH,probs=c(0.1,0.3,0.7,0.9,1.0))
      bins2<-c(0,as.numeric(x))
      
      
      ## create length bins  
      for ( i in 2:length(bins2)){
        length$BIN[length$LENGTH<bins2[i] & length$LENGTH >= bins2[(i-1)]] <- bins2[(i-1)]
      }
      
      label  <-array(dim=5)
      label2 <-array(dim=5)
      
      
      lab1<-c(trunc(bins2[1:5]/10),max(length$LENGTH/10))
      lab2<-lab1-1
      lab2[1]<-0
      
      for(i in 1:4){
        label[i]<-paste(as.character(lab1)[i],"-",as.character(lab2)[i+1]," cm",sep="")
        label2[i]<-i
        
      }
      
      label[5]<-paste(as.character(lab1)[5],"-",as.character(lab1)[6]," cm",sep="")
      label2[5]<-5
      
      label<-data.frame(BIN=bins2[1:5],LABEL=as.factor(label),LABEL2=label2) 
      length<-merge(length,label,by="BIN")
      
      
      
      data1<-list(location=location,length=length)
      td1 <- Get_TEMP(data=data1,plotT=F)
      td1$LON=1;td1$LAT=1; td1$DEPTH=1;td1$TEMP=1;td1$bin2=1;td1$CPUELAB="0"
      
      if(survey %in% c(46,47,48)){
        location <- location[YEAR>1993]
        length <- length[YEAR>1993]
        td1<-td1[YEAR>1993]
      }
      
     
      
      
      yers    <- unique(length$YEAR)
      yers2<- sort(unique(location$YEAR))
      
      if(survey==98 & !1994 %in% length$YEAR){ location<-location[YEAR %in% yers]}
      if(survey %in% c(46,47,48) & !1993 %in% length$YEAR){ location<-location[YEAR %in% yers]}
      if(survey==52 & !1994 %in% length$YEAR){ location<-location[YEAR %in% yers]}
      
      length <- length[complete.cases(length[,c('CPUE','TEMP')])]
      
      
      rgb.palette <- colorRampPalette(c("gold","goldenrod2","darkorange2","brown"),space = "rgb") 
      
      color1      <- c("gray70",rgb.palette(5))
      
      
      MTEMP <- Get_TEMP(data=data1,plotT=F)  ## access function Get_TEMP to calculate mean temperatures see appedix B
      
      length <- length[complete.cases(length[,c('CPUE','TEMP','DEPTH')])]
      Length   <- data.table(merge(length,MTEMP,by=c("YEAR"),all=T))
      
     ## Area weighted means calculated for GOA survey
     if(AREA_W==T & survey %in% c(46,47,48) )
	      {   area <- SURVEY_AREA(location=location,surveyx=survey) 
            length1<-merge(Length,area,all.x=T,by=c("YEAR","STRATUM","SURVEY_DEFINITION_ID","TEMP","STEMP","DEPTH","LAT","LON"))
          	length1<-length1[!is.na(W)]
          	length1$CPUE<-length1$CPUE*length1$W
          	length1$DEPTH<-length1$SDEPTH
          	length1$LON<-length1$MLON
          	length1$LAT <-length1$MLAT
          	Length=length1
          	}

   ## Plots on temperature and depth (1) or location (2) by weighted mean shelf temperature
      if(plotT %in% c(1,2))
      {
        if(plotT==1)  ## for temperature and depth plot by shelf temperature
        {  ## calculating weighted mean  temperature and depth
          Length <- Length[complete.cases(Length[,c('DEPTH','TEMP')])]
          data1 <- Length[,list(FREQ=sum(FREQUENCY),CPUE=mean(CPUE)),by= 'YEAR,SPECIES_CODE,REGI,BIN,LABEL,LABEL2,TEMP,DEPTH']
          data2 <- Length[,list(SUM=sum(FREQUENCY)),by = 'YEAR,SPECIES_CODE,REGI,TEMP,DEPTH']
          data3 <- merge(data1,data2,all=T,by=c("YEAR","SPECIES_CODE","REGI","TEMP","DEPTH"))
          data3 <- data3[,list(PLOT=sum((FREQ/SUM)*CPUE)),by = 'YEAR,SPECIES_CODE,REGI,BIN,LABEL,LABEL2,TEMP,DEPTH']
          data8<-data3[,list(my=-weighted.mean(DEPTH,PLOT),mx=weighted.mean(TEMP,PLOT),myVAR=weighted.var.se(DEPTH,PLOT),mxVAR=weighted.var.se(TEMP,PLOT)),by='SPECIES_CODE,REGI,BIN,LABEL,LABEL2']
          data6<-data3[,list(yC=-weighted.mean(DEPTH,PLOT),xC=weighted.mean(TEMP,PLOT),yVAR=weighted.var.se(DEPTH,PLOT),xVAR=weighted.var.se(TEMP,PLOT)),by='YEAR,SPECIES_CODE,REGI,BIN,LABEL,LABEL2']
        }
        
        if(plotT==2)  ## for location plot by shelf temperature
        {
          Length <- Length[complete.cases(Length[,c('LAT','LON')])]
          length1<-Length
         
          ## transforming latitude and longitude to UTM coordinates in KM
          length1$x<-length1$LON
          length1$y<-length1$LAT
          coordinates(length1)<-c("x","y")
          proj4string(length1)<-CRS("+proj=longlat +datum=WGS84")
          LENGTH<-spTransform(length1,CRS("+proj=utm +zone=2 ellps=WGS84"))
          Length$x<-LENGTH$x/1000
          Length$y<-LENGTH$y/1000
          Length<-Length[!is.na(x)]
          ## calculating weighted mean locations
          data1 <- Length[,list(FREQ=sum(FREQUENCY),CPUE=mean(CPUE)),by= 'YEAR,SPECIES_CODE,REGI,BIN,LABEL,LABEL2,x,y']
          data2 <- Length[,list(SUM=sum(FREQUENCY)),by = 'YEAR,SPECIES_CODE,REGI,x,y']
          data3 <- merge(data1,data2,all=T,by=c("YEAR","SPECIES_CODE","REGI","x","y"))
          data3 <- data3[,list(PLOT=sum((FREQ/SUM)*CPUE)),by = 'YEAR,SPECIES_CODE,REGI,BIN,LABEL,LABEL2,x,y']
          data8<-data3[,list(my=weighted.mean(y,PLOT),mx=weighted.mean(x,PLOT),myVAR=weighted.var.se(y,PLOT),mxVAR=weighted.var.se(x,PLOT)),by='SPECIES_CODE,REGI,BIN,LABEL,LABEL2']
          data6<-data3[,list(yC=weighted.mean(y,PLOT),xC=weighted.mean(x,PLOT),yVAR=weighted.var.se(y,PLOT),xVAR=weighted.var.se(x,PLOT)),by='YEAR,SPECIES_CODE,REGI,BIN,LABEL,LABEL2']
        }
        
        ## transforming variance to standard error for confidence intervals
        data6$xSEM <-sqrt(data6$xVAR)*1.96
        data6$ySEM <- sqrt(data6$yVAR)*1.96
        data7<-merge(data6,SN,by="SPECIES_CODE")
        
        x1<-data.table(data7)
        x1<-x1[,list(LABEL2=min(as.numeric(LABEL2))),by="LABEL"]
        x1<-x1[order(x1$LABEL2),]
        data7$LABEL <- factor(data7$LABEL,levels=x1$LABEL) 
        
        d <- ggplot(data=data7)
        d <- d+geom_point(data=data7,aes(y=yC,x=xC,color=REGI,shape=REGI,size=REGI))
        d <- d + geom_pointrange(data=data7,aes(y=yC,x=xC,ymax = yC + ySEM, ymin=yC - ySEM,color=REGI,shape=REGI,size=REGI)) 
        d <- d + geom_errorbarh(data=data7,aes(x=xC,y=yC,xmax = xC + xSEM, xmin=xC - xSEM,color=REGI,size=REGI))
        d <- d + geom_point(data=data8,aes(y=my,x=mx,shape=REGI),size=3)
        d <- d + scale_colour_manual(name="Shelf temp.",values=c("blue","gray60","red"))+scale_shape_manual(name="Shelf temp.",values=c(15,17,16))+scale_size_manual(name="Shelf temp.",values=rep(0.25,3))
        
        if(plotT==1){
          d <- d + xlab(expression("Temperature ("* degree * C *")"))
          d <- d + ylab("Depth (m)")
        }
        
        if(plotT==2){
          d <- d + xlab("Eastings UTM (km)")
          d <- d + ylab("Northings UTM (km)")
        }
        
        
        d <- d + theme1(base_size=20)
        d <- d + ggtitle(plot.title) 
        d <- d + facet_wrap(~LABEL,shrink=FALSE,ncol=length(unique(data7$LABEL2)))
        print(d)
        
      }
      
      ## Centroids across all years combined
      if(plotT %in% c(3,4))
      {
        
        
        if(plotT==3) ##  by temperature and depth
        {
          Length <- Length[complete.cases(Length[,c('DEPTH','TEMP')])]
          data1 <- Length[,list(FREQ=sum(FREQUENCY),CPUE=mean(CPUE)),by= 'YEAR,SPECIES_CODE,BIN,LABEL,LABEL2,TEMP,DEPTH']
          data2 <- Length[,list(SUM=sum(FREQUENCY)),by = 'YEAR,SPECIES_CODE,TEMP,DEPTH']
          data3 <- merge(data1,data2,all=T,by=c("YEAR","SPECIES_CODE","TEMP","DEPTH"))
          data3 <- data3[,list(PLOT=sum((FREQ/SUM)*CPUE)),by = 'YEAR,SPECIES_CODE,BIN,LABEL,LABEL2,TEMP,DEPTH']
          data8<-data3[,list(my=-weighted.mean(DEPTH,PLOT),mx=weighted.mean(TEMP,PLOT),myVAR=weighted.var.se(DEPTH,PLOT),mxVAR=weighted.var.se(TEMP,PLOT)),by='SPECIES_CODE,BIN,LABEL,LABEL2']
          data6<-data3[,list(yC=-weighted.mean(DEPTH,PLOT),xC=weighted.mean(TEMP,PLOT),yVAR=weighted.var.se(DEPTH,PLOT),xVAR=weighted.var.se(TEMP,PLOT)),by='YEAR,SPECIES_CODE,BIN,LABEL,LABEL2']
        }
        
        if(plotT==4) ## by location
        {
          Length <- Length[complete.cases(Length[,c('LAT','LON')])]
          length1<-Length
          length1$x<-length1$LON
          length1$y<-length1$LAT
          coordinates(length1)<-c("x","y")
          proj4string(length1)<-CRS("+proj=longlat +datum=WGS84")
          LENGTH<-spTransform(length1,CRS("+proj=utm +zone=2 ellps=WGS84"))
          Length$x<-LENGTH$x/1000
          Length$y<-LENGTH$y/1000
          Length<-Length[!is.na(x)]
          data1 <- Length[,list(FREQ=sum(FREQUENCY),CPUE=mean(CPUE)),by= 'YEAR,SPECIES_CODE,BIN,LABEL,LABEL2,x,y']
          data2 <- Length[,list(SUM=sum(FREQUENCY)),by = 'YEAR,SPECIES_CODE,x,y']
          data3 <- merge(data1,data2,all=T,by=c("YEAR","SPECIES_CODE","x","y"))
          data3 <- data3[,list(PLOT=sum((FREQ/SUM)*CPUE)),by = 'YEAR,SPECIES_CODE,BIN,LABEL,LABEL2,x,y']
          data8<-data3[,list(my=weighted.mean(y,PLOT),mx=weighted.mean(x,PLOT),myVAR=weighted.var.se(y,PLOT),mxVAR=weighted.var.se(x,PLOT)),by='SPECIES_CODE,BIN,LABEL,LABEL2']
          data6<-data3[,list(yC=weighted.mean(y,PLOT),xC=weighted.mean(x,PLOT),yVAR=weighted.var.se(y,PLOT),xVAR=weighted.var.se(x,PLOT)),by='YEAR,SPECIES_CODE,BIN,LABEL,LABEL2']
        }

        ## transforming variance to standard error for confidence intervals
        data6$xSEM <-sqrt(data6$xVAR)*1.96
        data6$ySEM <- sqrt(data6$yVAR)*1.96
        data7<-merge(data6,SN,by="SPECIES_CODE")
        
        x1<-data.table(data7)
        x1<-x1[,list(LABEL2=min(as.numeric(LABEL2))),by="LABEL"]
        x1<-x1[order(x1$LABEL2),]
        data7$LABEL <- factor(data7$LABEL,levels=x1$LABEL) 
        
        d <- ggplot(data=data7)
        d <- d+geom_point(data=data7,aes(y=yC,x=xC,color=factor(YEAR),size=factor(YEAR)))
        d <- d + geom_pointrange(data=data7,aes(y=yC,x=xC,ymax = yC + ySEM, ymin=yC - ySEM,color=factor(YEAR),size=factor(YEAR))) 
        d <- d + geom_errorbarh(data=data7,aes(x=xC,y=yC,xmax = xC + xSEM, xmin=xC - xSEM,color=factor(YEAR),size=factor(YEAR)))
        d <- d + geom_path(data=data7,aes(x=xC,y=yC,group=1),size=0.15,color="gray70")
        d <- d + geom_point(data=data8,aes(y=my,x=mx),size=2)
        d <- d + scale_colour_manual(name="Year",values=c(rgb.palette(length(unique(data7$YEAR))+1)))+scale_size_manual(name="Year",values=rep(0.5,length(unique(data7$YEAR))))
        
        if(plotT==4){
          d <- d + xlab("Eastings UTM (km)")
          d <- d + ylab("Northings UTM (km)")
        }
        
        if(plotT==3){
          d <- d + xlab(expression("Temperature ("* degree * C *")"))
          d <- d + ylab("Depth (m)")
        }
        
       
        d <- d + theme1(base_size=20)
        d <- d + ggtitle(plot.title) 
        d <- d + facet_wrap(~LABEL,shrink=FALSE,ncol=length(unique(data7$LABEL2)))
        
        print(d)
      }
      ## Centroids by Year
      if(plotT %in% c(5,6))
      { 
         rgb.palette <- colorRampPalette(c("gold","goldenrod2","darkorange2","brown"),space = "rgb") 
        
        if(plotT==5)
        {
          Length <- Length[complete.cases(Length[,c('DEPTH','TEMP')])]
          data1 <- Length[,list(FREQ=sum(FREQUENCY),CPUE=mean(CPUE)),by= 'YEAR,SPECIES_CODE,BIN,LABEL,LABEL2,TEMP,DEPTH']
          data2 <- Length[,list(SUM=sum(FREQUENCY)),by = 'YEAR,SPECIES_CODE,TEMP,DEPTH']
          data3 <- merge(data1,data2,all=T,by=c("YEAR","SPECIES_CODE","TEMP","DEPTH"))
          data3 <- data3[,list(PLOT=sum((FREQ/SUM)*CPUE)),by = 'YEAR,SPECIES_CODE,BIN,LABEL,LABEL2,TEMP,DEPTH']
          data8<-data3[,list(my=-weighted.mean(DEPTH,PLOT),mx=weighted.mean(TEMP,PLOT),myVAR=weighted.var.se(DEPTH,PLOT),mxVAR=weighted.var.se(TEMP,PLOT)),by='SPECIES_CODE,BIN,LABEL,LABEL2']
          data6<-data3[,list(yC=-weighted.mean(DEPTH,PLOT),xC=weighted.mean(TEMP,PLOT),yVAR=weighted.var.se(DEPTH,PLOT),xVAR=weighted.var.se(TEMP,PLOT)),by='YEAR,SPECIES_CODE,BIN,LABEL,LABEL2']
        }
        
        if(plotT==6)
        {
          Length    <- Length[complete.cases(Length[,c('LAT','LON')])]
          length1   <- Length
          length1$x <- length1$LON
          length1$y <- length1$LAT
          coordinates(length1)<-c("x","y")
          proj4string(length1)<-CRS("+proj=longlat +datum=WGS84")
          LENGTH    <- spTransform(length1,CRS("+proj=utm +zone=2 ellps=WGS84"))
          Length$x  <- LENGTH$x/1000
          Length$y  <- LENGTH$y/1000
          
          Length <- Length[!is.na(x)]
          data1  <- Length[,list(FREQ=sum(FREQUENCY),CPUE=mean(CPUE)),by= 'YEAR,SPECIES_CODE,BIN,LABEL,LABEL2,x,y']
          data2  <- Length[,list(SUM=sum(FREQUENCY)),by = 'YEAR,SPECIES_CODE,x,y']
          data3  <- merge(data1,data2,all=T,by=c("YEAR","SPECIES_CODE","x","y"))
          data3  <- data3[,list(PLOT=sum((FREQ/SUM)*CPUE)),by = 'YEAR,SPECIES_CODE,BIN,LABEL,LABEL2,x,y']
          data8  <- data3[,list(my=weighted.mean(y,PLOT),mx=weighted.mean(x,PLOT),myVAR=weighted.var.se(y,PLOT),mxVAR=weighted.var.se(x,PLOT)),by='SPECIES_CODE,BIN,LABEL,LABEL2']
          data6  <- data3[,list(yC=weighted.mean(y,PLOT),xC=weighted.mean(x,PLOT),yVAR=weighted.var.se(y,PLOT),xVAR=weighted.var.se(x,PLOT)),by='YEAR,SPECIES_CODE,BIN,LABEL,LABEL2']
        }
        
        data8$mxSEM <-sqrt(data8$mxVAR)*1.96
        data8$mySEM <- sqrt(data8$myVAR)*1.96
        data7<-merge(data6,SN,by="SPECIES_CODE")
        
        x1<-data.table(data7)
        x1<-x1[,list(LABEL2=min(as.numeric(LABEL2))),by="LABEL"]
        x1<-x1[order(x1$LABEL2),]
        data7$LABEL <- factor(data7$LABEL,levels=x1$LABEL) 
        data8$LABEL <- factor(data8$LABEL,levels=x1$LABEL) 
        data8       <- data8[order(LABEL),]
        
        d <- ggplot(data=data7)
        d <- d + geom_point(data=data7,aes(y=yC,x=xC,color=LABEL,size=LABEL,shape=LABEL))
        d <- d + geom_path(data=data8,aes(y=my,x=mx,group=1),size=0.2,color="gray70") 
        d <- d + geom_errorbar(data=data8,aes(x=mx,ymax = my + mySEM, ymin=my - mySEM),size=0.2,width=0.0) 
        d <- d + geom_errorbarh(data=data8,aes(x=mx,y=my,xmax = mx + mxSEM, xmin=mx - mxSEM),size=0.2,height=0.0)
        
        d <- d + geom_point(data=data8,aes(y=my,x=mx,shape=LABEL,color=LABEL),size=2)
        d <- d + scale_colour_manual(name="Size (cm)",values=c(rgb.palette(length(unique(data7$LABEL))+1)))
        d <- d + scale_size_manual(name="Size (cm)",values=rep(2,length(unique(data7$LABEL))))
        d <- d + scale_shape_manual(name="Size (cm)",values= c(21,25,15,19,17))
        
        if(plotT==6){
          d <- d + xlab("Eastings UTM (km)")
          d <- d + ylab("Northings UTM (km)")
        }
        
        if(plotT==5){
          d <- d + xlab(expression("Temperature ("* degree * C *")"))
          d <- d + ylab("Depth (m)")
        }
        
        d <- d + theme1(base_size=20)
        d <- d + ggtitle(plot.title) 
       
        print(d)
      }
      ## Centroids by sex and year
      if(plotT %in% c(7,8))
      {
        Length <- Length[SEX %in% c(1,2)]
        
        if(plotT==7)
        {
          Length <- Length[complete.cases(Length[,c('DEPTH','TEMP')])]
          data1 <- Length[,list(FREQ=sum(FREQUENCY),CPUE=mean(CPUE)),by= 'YEAR,SPECIES_CODE,SEX,BIN,LABEL,LABEL2,TEMP,DEPTH']
          data2 <- Length[,list(SUM=sum(FREQUENCY)),by = 'YEAR,SPECIES_CODE,SEX,TEMP,DEPTH']
          data3 <- merge(data1,data2,all=T,by=c("YEAR","SPECIES_CODE","SEX","TEMP","DEPTH"))
          data3 <- data3[,list(PLOT=sum((FREQ/SUM)*CPUE)),by = 'YEAR,SPECIES_CODE,SEX,BIN,LABEL,LABEL2,TEMP,DEPTH']
          data8<-data3[,list(my=-weighted.mean(DEPTH,PLOT),mx=weighted.mean(TEMP,PLOT),myVAR=weighted.var.se(DEPTH,PLOT),mxVAR=weighted.var.se(TEMP,PLOT)),by='SPECIES_CODE,SEX,BIN,LABEL,LABEL2']
          data6<-data3[,list(yC=-weighted.mean(DEPTH,PLOT),xC=weighted.mean(TEMP,PLOT),yVAR=weighted.var.se(DEPTH,PLOT),xVAR=weighted.var.se(TEMP,PLOT)),by='YEAR,SPECIES_CODE,SEX,BIN,LABEL,LABEL2']
        }
        
        if(plotT==8)
        {
          Length <- Length[complete.cases(Length[,c('LAT','LON')])]
          length1<-Length
          length1$x<-length1$LON
          length1$y<-length1$LAT
          coordinates(length1)<-c("x","y")
          proj4string(length1)<-CRS("+proj=longlat +datum=WGS84")
          LENGTH<-spTransform(length1,CRS("+proj=utm +zone=2 ellps=WGS84"))
          Length$x<-LENGTH$x/1000
          Length$y<-LENGTH$y/1000
          Length<-Length[!is.na(x)]
          data1 <- Length[,list(FREQ=sum(FREQUENCY),CPUE=mean(CPUE)),by= 'YEAR,SPECIES_CODE,SEX,BIN,LABEL,LABEL2,x,y']
          data2 <- Length[,list(SUM=sum(FREQUENCY)),by = 'YEAR,SPECIES_CODE,SEX,x,y']
          data3 <- merge(data1,data2,all=T,by=c("YEAR","SPECIES_CODE","SEX","x","y"))
          data3 <- data3[,list(PLOT=sum((FREQ/SUM)*CPUE)),by = 'YEAR,SPECIES_CODE,SEX,BIN,LABEL,LABEL2,x,y']
          data8 <- data3[,list(my=weighted.mean(y,PLOT),mx=weighted.mean(x,PLOT),myVAR=weighted.var.se(y,PLOT),mxVAR=weighted.var.se(x,PLOT)),by='SPECIES_CODE,SEX,BIN,LABEL,LABEL2']
          data6<-data3[,list(yC=weighted.mean(y,PLOT),xC=weighted.mean(x,PLOT),yVAR=weighted.var.se(y,PLOT),xVAR=weighted.var.se(x,PLOT)),by='YEAR,SPECIES_CODE,SEX,BIN,LABEL,LABEL2']
        }
        
        data8$SEX2 <-as.factor(ifelse(data8$SEX==1,"Male","Female"))
        data6$xSEM <-sqrt(data6$xVAR)*1.96
        data6$ySEM <- sqrt(data6$yVAR)*1.96
        data7<-merge(data6,SN,by="SPECIES_CODE")
        
        x1<-data.table(data7)
        x1<-x1[,list(LABEL2=min(as.numeric(LABEL2))),by="LABEL"]
        x1<-x1[order(x1$LABEL2),]
        data7$LABEL <- factor(data7$LABEL,levels=x1$LABEL) 
        data7$SEX2 <-as.factor(ifelse(data7$SEX==1,"Male","Female"))
        
       
        d <- ggplot(data=data7)
        d <- d + geom_point(data=data7,aes(y=yC,x=xC,color=SEX2,shape=SEX2,size=SEX2))
        d <- d + geom_pointrange(data=data7,aes(y=yC,x=xC,ymax = yC + ySEM, ymin=yC - ySEM,color=SEX2,shape=SEX2,size=SEX2)) 
        d <- d + geom_errorbarh(data=data7,aes(x=xC,y=yC,xmax = xC + xSEM, xmin=xC - xSEM,color=SEX2,size=SEX2))
        d <- d + scale_colour_manual(name="Sex",values=c("salmon","light blue"))+scale_shape_manual(name="Sex",values=c(17,16))+scale_size_manual(name="Sex",values=rep(0.25,2))
        d <- d + geom_point(data=data8,aes(y=my,x=mx,shape=SEX2),guide=FALSE,size=2)
        
        if(plotT==7)
        {
          d <- d + xlab(expression("Bottom temp. ("* degree * C *")"))
          d <- d + ylab("Bottom depth (m)")
        }
        
        if(plotT==8)
        {
          d <- d + xlab("Eastings UTM (km)")
          d <- d + ylab("Northings UTM (km)")
        }
        
        d <- d + theme1(base_size=20)
        d <- d + ggtitle(plot.title) 
        d <- d + facet_wrap(~LABEL,shrink=FALSE,ncol=length(unique(data7$LABEL2)))
        
        print(d)
      }
      ## Centroids by depth and longitude
      if(plotT %in% c(9))
      { 
        rgb.palette <- colorRampPalette(c("red","orange","green","turquoise","blue"),space = "rgb")
        Length    <- Length[complete.cases(Length[,c('LAT','LON')])]
        length1   <- Length
        length1$x <- length1$LON
        length1$y <- length1$LAT
        coordinates(length1)<-c("x","y")
        proj4string(length1)<-CRS("+proj=longlat +datum=WGS84")
        LENGTH    <- spTransform(length1,CRS("+proj=utm +zone=2 ellps=WGS84"))
        Length$x  <- LENGTH$x/1000
        Length$y  <- LENGTH$y/1000
        
        Length <- Length[complete.cases(Length[,c('x','DEPTH')])]
        data1 <- Length[,list(FREQ=sum(FREQUENCY),CPUE=mean(CPUE)),by= 'YEAR,SPECIES_CODE,BIN,LABEL,LABEL2,x,DEPTH']
        data2 <- Length[,list(SUM=sum(FREQUENCY)),by = 'YEAR,SPECIES_CODE,x,DEPTH']
        data3 <- merge(data1,data2,all=T,by=c("YEAR","SPECIES_CODE","x","DEPTH"))
        data3 <- data3[,list(PLOT=sum((FREQ/SUM)*CPUE)),by = 'YEAR,SPECIES_CODE,BIN,LABEL,LABEL2,x,DEPTH']
        data8<-data3[,list(my=-weighted.mean(DEPTH,PLOT),mx=weighted.mean(x,PLOT),myVAR=weighted.var.se(DEPTH,PLOT),mxVAR=weighted.var.se(x,PLOT)),by='SPECIES_CODE,BIN,LABEL,LABEL2']
        data6<-data3[,list(yC=-weighted.mean(DEPTH,PLOT),xC=weighted.mean(x,PLOT),yVAR=weighted.var.se(DEPTH,PLOT),xVAR=weighted.var.se(x,PLOT)),by='YEAR,SPECIES_CODE,BIN,LABEL,LABEL2']
        
        data8$mxSEM <-sqrt(data8$mxVAR)*1.96
        data8$mySEM <- sqrt(data8$myVAR)*1.96
        data7<-merge(data6,SN,by="SPECIES_CODE")
        
        x1<-data.table(data7)
        x1<-x1[,list(LABEL2=min(as.numeric(LABEL2))),by="LABEL"]
        x1<-x1[order(x1$LABEL2),]
        data7$LABEL <- factor(data7$LABEL,levels=x1$LABEL) 
        data8$LABEL <- factor(data8$LABEL,levels=x1$LABEL) 
        data8       <- data8[order(LABEL),]
        
        d <- ggplot(data=data7)
        d <- d + geom_point(data=data7,aes(y=yC,x=xC,color=LABEL,size=LABEL,shape=LABEL))
        d <- d + geom_path(data=data8,aes(y=my,x=mx,group=1),size=0.2,color="gray70") 
        d <- d + geom_errorbar(data=data8,aes(x=mx,ymax = my + mySEM, ymin=my - mySEM),size=0.2,width=0.0) 
        d <- d + geom_errorbarh(data=data8,aes(x=mx,y=my,xmax = mx + mxSEM, xmin=mx - mxSEM),size=0.2,height=0.0)
        
        d <- d + geom_point(data=data8,aes(y=my,x=mx,shape=LABEL,color=LABEL),size=2)
        d <- d + scale_colour_manual(name="Size (cm)",values=c(rgb.palette(length(unique(data7$LABEL))+1)))
        d <- d + scale_size_manual(name="Size (cm)",values=rep(2,length(unique(data7$LABEL))))
        d <- d + scale_shape_manual(name="Size (cm)",values= c(21,25,15,19,17))
        
        d <- d + xlab("Eastings UTM (km)")
        d <- d + ylab("Depth (m)")
        
        d <- d + theme1(base_size=20)
        d <- d + ggtitle(plot.title) 
        
        print(d)
      }
    }