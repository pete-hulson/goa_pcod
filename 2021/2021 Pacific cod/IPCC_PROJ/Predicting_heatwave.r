library(r4ss)
library(data.table)
library(mgcv)
library(ggplot2)

setwd("C:/WORKING_FOLDER/2021 Stock Assessments/2021 Pacific cod/IPCC_PROJ/Model21.1e")
ssdat=SS_readdat_3.30(file="GOAPcod2021OCT1_10P_CL.dat")
ssdat$envdat


setwd("C:/WORKING_FOLDER/2021 Stock Assessments/2021 Pacific cod/IPCC_PROJ")

x=dir()
x=x[x %like% "Model21.1e"][2:21]


TEMPHW<-data.table(read.csv("TEMPANDHEAT.csv"))

## total heatwave
TEMPHW$HW1<-1
TEMPHW[THW==0]$HW1<-0
TEMPHW$HW1<-factor(TEMPHW$HW1)

Gtest<-glm(HW1~TEMP,data=TEMPHW,family="binomial")

newd<-data.table(TEMP=seq(-2,2,by=0.1))
newd$PRED<-predict(Gtest,type="response",newdata=newd)

LMtest<-lm(THW~TEMP,data=TEMPHW[HW1==1])

G2test<-glm(THW~TEMP,data=TEMPHW[HW1==1],family=gaussian("log"))

G3test<-glm(THW~TEMP,data=TEMPHW[TEMP>0],family=gaussian) ## using the cutoff temp <0 provides estimates closer to the origin instead of using only those that are positive heatwave.
coef2<-c(as.numeric(coef(Gtest)[1:2]),as.numeric(coef(G3test)[1:2]))

convert<-function(TEMP=data){
  predict = exp(coef2[2]*TEMP+coef2[1])/(1+exp(coef2[2]*TEMP+coef2[1]))
  #predict2= exp(coef2[4]*TEMP+coef2[3])
  predict2= coef2[4]*TEMP+coef2[3]
  #predict2[predict2>828]<-828
  predict2[predict2<0]<-0
  predict[runif(1)>predict]<-0 ## randomize heatwaves
  predict2<-predict*predict2
  return(predict2)
  }

##summer heatwave
TEMPHW$HW2<-1
TEMPHW[SHW==0]$HW2<-0
TEMPHW$HW2<-factor(TEMPHW$HW2)

GStest<-glm(HW2~TEMP,data=TEMPHW,family="binomial")

newdS<-data.table(TEMP=seq(-2,2,by=0.1))
newdS$PRED<-predict(GStest,type="response",newdata=newdS)

LMStest<-lm(SHW~TEMP,data=TEMPHW[HW2==1])

G2Stest<-glm(SHW~TEMP,data=TEMPHW[HW2==1],family=gaussian("log"))

G3Stest<-glm(SHW~TEMP,data=TEMPHW[TEMP>0],family=gaussian) ## using the cutoff temp <0 provides estimates closer to the origin instead of using only those that are positive heatwave.
coefS2<-c(as.numeric(coef(GStest)[1:2]),as.numeric(coef(G3Stest)[1:2]))

convert2<-function(TEMP=data){
  predict = exp(coefS2[2]*TEMP+coefS2[1])/(1+exp(coefS2[2]*TEMP+coefS2[1]))
  #predict2= exp(coefS2[4]*TEMP+coefS2[3])
  predict2= coefS2[4]*TEMP+coefS2[3]
  #predict2[predict2>828]<-828
  predict2[predict2<0]<-0
  predict[runif(1)>predict]<-0 ## randomize heatwaves
  predict2<-predict*predict2
  return(predict2)
  }

growth_L0<-function(data=data,T=mean(TEMPHW[YR%in%1982:2012]$TEMP)){
  index=exp(0.2494+0.3216*(T+data$TEMP)-0.0069*(T+data$TEMP)^2-0.0004*(T+data$TEMP)^3)/exp(0.2494+0.3216*(T)-0.0069*(T)^2-0.0004*(T)^3)
  return(index)
}


  data<-data.table(read.csv("ipcc_proj.csv"))
  data$THW<-convert(data$TEMP)

  d<-ggplot(data[USED=="Y"&SERIES==4.5],aes(x=YEAR,y=THW,color=MODEL,fill=MODEL))+geom_line(size=1.25)+theme_bw(base_size=16)
  d2<-ggplot(data[USED=="Y"&SERIES==4.5],aes(x=YEAR,y=SHW,color=MODEL,fill=MODEL))+geom_line(size=1.25)+theme_bw(base_size=16)

tempHW<-data.table(YR=c(1977,1978),JUNE_TEMP=c(0,0),FEB_TEMP=c(0,0),TEMP=c(0,0),TEMP2=c(0,0),TEMP3=c(0,0),THW=c(0,0),WHW=c(0,0),SHW=c(0,0),HW1=c(0,0),HW2=c(0,0))
TEMPHW=rbind(tempHW,TEMPHW)
x1<-data.table(Yr= TEMPHW$YR, Variable=1, Value=TEMPHW$TEMP)
x2<-data.table(Yr= TEMPHW$YR, Variable=2, Value=(TEMPHW$THW^1/3)^3)
x3<-data.table(Yr= TEMPHW$YR, Variable=3, Value=TEMPHW$SHW^(1/3))
x4<-data.table(Yr= TEMPHW$YR, Variable=4, Value=growth_L0(TEMPHW))
x5<-data.table(Yr= TEMPHW$YR, Variable=5, Value=0.65/(1+exp(-0.005*(TEMPHW$THW-400))))
x6<-data.table(Yr= c(2015:2021), Variable=6, Value=1.0)  

M1<-mean(x4[Yr%in% 1982:2012]$Value)
ST1<-sd(x4[Yr%in% 1982:2012]$Value)
x4$Value<-(x4$Value-M1)/ST1


data<-data.table(read.csv("ipcc_proj.csv"))

mods<-as.character(unique(data$MODEL))

dir1<-"C:/WORKING_FOLDER/2021 Stock Assessments/2021 Pacific cod/IPCC_PROJ"

for(i in 1:length(mods)){
  ssdat2<-ssdat
  xm<-x[x%like%mods[i]]
  data1<-data[MODEL==mods[i]&YEAR>2021]

  data1$THW<-convert(data1$TEMP)
  data1$SHW<-convert2(data1$TEMP)

  x1.1<-data.table(Yr= data1$YEAR, Variable=1, Value=data1$TEMP)
  x2.1<-data.table(Yr= data1$YEAR, Variable=2, Value=(data1$THW^(1/3))^3)
  x3.1<-data.table(Yr= data1$YEAR, Variable=3, Value=data1$SHW^(1/3))
  x4.1<-data.table(Yr= data1$YEAR, Variable=4, Value=growth_L0(data1))
  x4.1$Value<-(x4.1$Value-M1)/ST1
  x5.1<-data.table(Yr= data1$YEAR, Variable=5, Value=0.65/(1+exp(-0.005*(data1$THW-400))))
   
 
  envdata<-data.frame(rbind(x1,x1.1,x2,x2.1,x3,x3.1,x4,x4.1,x5,x5.1,x6))

  for(j in 1:length(xm)){
    try(setwd(paste0(dir1,"/",xm[j])),TRUE)
    print(xm[j])
    ssdat2$envdat<-envdata
    SS_writedat(ssdat2,"GOAPcod2021OCT1_10P_CL.dat", version = "3.30", overwrite = TRUE)
  }
}





library(r4ss)
library(data.table)


dir1<-"C:/WORKING_FOLDER/2021 Stock Assessments/2021 Pacific cod/IPCC_PROJ"
setwd(dir1)

mods4.5_nofish<-x[x %like% "_4.5_nofish"]
mods2.6_nofish<-x[x %like% "_2.6_nofish"]
mods4.5<-x[x %like% "_4.5" & !x %in% mods4.5_nofish]
mods2.6<-x[x %like% "_2.6" & !x %in% mods2.6_nofish]



for(i in 1:5){
   setwd(paste0(dir1,"/",mods2.6[i]))
   system('ss')
}


for(i in 1:5){
   setwd(paste0(dir1,"/",mods4.5[i]))
   system('ss')
}

for(i in 1:5){
   setwd(paste0(dir1,"/",mods2.6_nofish[i]))
   system('ss')
}

for(i in 1:5){
   setwd(paste0(dir1,"/",mods4.5_nofish[i]))
   system('ss')
}









data$SHW<-convert(data$TEMP)

d<-ggplot(data[USED=="Y"&SERIES==4.5],aes(x=YEAR,y=SHW,color=MODEL,fill=MODEL))+geom_line(size=1.25)+theme_bw(base_size=16)





data$LO_IND<- growth_L0(data=data)

for(i in 1:5){
  datax<-data[MODEL==unique(data[SERIES==4.5&USED=="Y"]$MODEL)[i]][,1:3]
  datax2<-data[MODEL==unique(data[SERIES==4.5&USED=="Y"]$MODEL)[i]][,c(1,2,7)]
  names(datax2)<-names(datax)
  datax2$INDEX<-6
  datax<-rbind(datax,datax2)

  write.csv(datax,paste0("Data4.5HW",i,".csv"))
}

for(i in 1:5){
  datax<-data[MODEL==unique(data[SERIES==2.6&USED=="Y"]$MODEL)[i]][,1:3]
  datax2<-data[MODEL==unique(data[SERIES==2.6&USED=="Y"]$MODEL)[i]][,c(1,2,7)]
  datax3<-data[MODEL==unique(data[SERIES==2.6&USED=="Y"]$MODEL)[i]][,c(1,2,8)]
  names(datax2)<-names(datax)
  names(datax3)<-names(datax)
  datax2$INDEX<-6
  datax3$INDEX<-8
  datax<-rbind(datax,datax2,datax3)
  
  write.csv(datax,paste0("Data2.6L0_",i,".csv"))
}




setwd("C:/WORKING_FOLDER/2020_Pcod/SS3.30.16/Model20.1_Average_Cond")
ssdat=SS_readdat_3.30(file="GOAPcod2020SEP6.1_10P.dat")
ssdat$envdat



