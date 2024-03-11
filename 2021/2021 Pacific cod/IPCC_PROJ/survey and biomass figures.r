 
SS_DAT<-SS_readdat_3.30("GOAPcod2021OCT1_10P_CL.dat")
CPUE<-data.table(SS_DAT$CPUE)
CPUE$obs<-as.numeric(CPUE$obs)
CPUE$SD<-as.numeric(CPUE$se_log)*CPUE$obs



ggplot(data=CPUE[index==4], aes(x=as.numeric(year), y=obs)) + geom_line() +geom_point()+theme_bw(base_size=18)+   
#geom_ribbon(aes(ymin=obs-2.326*SD, ymax=obs+2.326*SD), alpha=0.2,fill="blue")+
geom_ribbon(aes(ymin=obs-1.96*SD, ymax=obs+1.96*SD), alpha=0.2,fill="dark blue")+
#geom_ribbon(aes(ymin=obs-1.645*SD, ymax=obs+1.645*SD), alpha=0.2,fill="purple")+
geom_errorbar(aes(ymin=obs-1.96*SD, ymax=obs+1.96*SD), width=.25) +
 xlab("Year")+ylab("AFSC trawl survey numbers (1000s)")+theme( axis.text.x = element_text(hjust=1, angle = 90))+scale_x_continuous(limits=c(1989,2022),breaks=seq(1975,2022,by=1))+ylim(0,1000000)




ggplot(data=CPUE[index==5], aes(x=as.numeric(year), y=obs)) + geom_line() +geom_point()+theme_bw(base_size=18)+   
#geom_ribbon(aes(ymin=obs-2.326*SD, ymax=obs+2.326*SD), alpha=0.2,fill="blue")+
geom_ribbon(aes(ymin=obs-1.96*SD, ymax=obs+1.96*SD), alpha=0.2,fill="purple")+
#geom_ribbon(aes(ymin=obs-1.645*SD, ymax=obs+1.645*SD), alpha=0.2,fill="purple")+
geom_errorbar(aes(ymin=obs-1.96*SD, ymax=obs+1.96*SD), width=.25) +
 xlab("Year")+ylab("AFSC longline survey RPNs")+theme( axis.text.x = element_text(hjust=1, angle = 90))+scale_x_continuous(limits=c(1989,2022),breaks=seq(1975,2022,by=1))+ylim(0,200000)




ggplot(data=CPUE[index==6], aes(x=as.numeric(year), y=obs)) + geom_line() +geom_point()+theme_bw(base_size=18)+   
#geom_ribbon(aes(ymin=obs-2.326*SD, ymax=obs+2.326*SD), alpha=0.2,fill="salmon")+
geom_ribbon(aes(ymin=obs-1.96*SD, ymax=obs+1.96*SD), alpha=0.2,fill="red")+
#geom_ribbon(aes(ymin=obs-1.645*SD, ymax=obs+1.645*SD), alpha=0.2,fill="brown")+
geom_errorbar(aes(ymin=obs-1.96*SD, ymax=obs+1.96*SD), width=.25) +
 xlab("Year")+ylab("IPHC longline survey RPNs")+theme( axis.text.x = element_text(hjust=1, angle = 90))+scale_x_continuous(limits=c(1989,2022),breaks=seq(1975,2022,by=1))+ylim(0,35000)




ggplot(data=CPUE[index==7], aes(x=as.numeric(year), y=obs)) + geom_line() +geom_point()+theme_bw(base_size=18)+   
#geom_ribbon(aes(ymin=obs-2.326*SD, ymax=obs+2.326*SD), alpha=0.2,fill="yellow")+
geom_ribbon(aes(ymin=obs-1.96*SD, ymax=obs+1.96*SD), alpha=0.2,fill="orange")+
#geom_ribbon(aes(ymin=obs-1.645*SD, ymax=obs+1.645*SD), alpha=0.2,fill="goldenrod")+
geom_errorbar(aes(ymin=obs-1.96*SD, ymax=obs+1.96*SD), width=.25) +
 xlab("Year")+ylab("ADF&G trawl survey relative biomass")+theme( axis.text.x = element_text(hjust=1, angle = 90))+scale_x_continuous(limits=c(1987,2022),breaks=seq(1975,2022,by=1))




x<-data.table(CPUE[index==9])
x$UCI<-x$obs+1.96*x$SD
x$LCI<-x$obs-1.96*x$SD
x$LCI[x$LCI<0]<-0

ggplot(data=x, aes(x=as.numeric(year), y=obs)) + geom_line() +geom_point()+theme_bw(base_size=18)+   
geom_ribbon(aes(ymin=LCI, ymax=UCI), alpha=0.2,fill="dark green")+
geom_errorbar(aes(ymin=LCI, ymax=UCI), width=.25) +
 xlab("Year")+ylab("Age-0 Beach Seine numbers/haul")+theme( axis.text.x = element_text(hjust=1, angle = 90))+scale_x_continuous(limits=c(1987,2022),breaks=seq(1987,2022,by=1))




data=data.table(read.csv("FINAL_RE2019_SURVEY.csv"))

ggplot(data=data, aes(x=YEAR, y=BIOM,color=AREA,fill=AREA,shape=AREA,linetype=AREA)) + 
     geom_ribbon(aes(ymin=BIOM-1.96*STDEV, ymax=BIOM+1.96*STDEV), alpha=0.2) +
     #geom_errorbar(aes(ymin=BIOM-1.96*STDEV, ymax=BIOM+1.96*STDEV), width=0.25)+
     geom_line() + geom_point(size=1)+
     theme_bw(base_size=20)+xlab("Year")+ylab("Pacific cod biomass (t)")+ggtitle("AFSC bottom trawl survey RE Model for allocation")






ggplot(data=CPUE[index==8], aes(x=as.numeric(year), y=obs)) + geom_line() +geom_point()+theme_bw(base_size=18)+   
#geom_ribbon(aes(ymin=obs-2.326*SD, ymax=obs+2.326*SD), alpha=0.2,fill="salmon")+
geom_ribbon(aes(ymin=obs-1.96*SD, ymax=obs+1.96*SD), alpha=0.2,fill="red")+
#geom_ribbon(aes(ymin=obs-1.645*SD, ymax=obs+1.645*SD), alpha=0.2,fill="brown")+
geom_errorbar(aes(ymin=obs-1.96*SD, ymax=obs+1.96*SD), width=.25) +
 xlab("Year")+ylab("Larval habitat index")+theme( axis.text.x = element_text(hjust=1, angle = 90))+scale_x_continuous(limits=c(1978,2022),breaks=seq(1975,2022,by=1))




library(ggridges)
library(r4ss)

SS_DAT<-SS_readdat_3.30("GOAPcod2021OCT1_10P_CL.dat")



lencomp=data.table(SS_DAT$lencomp)[,c(1,3,7:123)]
names(lencomp)<-c('Yr','Flt',seq(0.5,116.5,1))
lencom2<-melt(lencomp,c('Yr', 'Flt'))
lencom2$variable<-as.numeric(as.character(lencom2$variable))

lencom2$length<-as.numeric(as.character(lencom2$variable))
lx2<-lencom2[,list(tot=sum(value)),by=c('Yr','Flt')]
lx3<-merge(lx2,lencom2)
lx3$Prop<-lx3$value/lx3$tot

p<- ggplot(lx3[Flt==3], aes(length, factor(Yr), fill=factor(Yr),height = Prop)) + 
   theme_ridges(font_size = 20) +
   geom_density_ridges(stat='identity')
p<-p+labs(x="Length (cm)",y="Year",fill='Year')+ theme(legend.position = "none")


lx3$Type="Trawl Fishery"
lx3[Flt==2]$Type="Longline Fishery"
lx3[Flt==3]$Type="Pot Fishery"
lx3[Flt==4]$Type="AFSC Trawl Survey"
lx3[Flt==5]$Type="AFSC Longline Survey"

p<- ggplot(lx3[Yr>1989&Prop>0], aes(y=length, x=factor(Yr),weight=Prop,fill=factor(Type))) + geom_boxplot(outlier.color="gray50",outlier.size=0.5)+theme_bw(base_size=20)
p<-p+labs(y="Length (cm)",x="Year",fill='Fleet')+theme(axis.text.x = element_text(hjust=1, angle = 90))+scale_fill_manual(values=c('#d7191c','#fdae61','#ffffbf','#abd9e9','#2c7bb6'))
p


outlier.shape=NA





ggplot(data=RE, aes(x=as.numeric(index), y=value, fill=name,color=name)) + geom_line() +geom_point()+theme_bw(base_size=18)+   
geom_ribbon(aes(ymin=value-1.96*std, ymax=value+1.96*std), alpha=0.2)+
 labs(x="Year",y="Pacific cod biomass (t)",title="AFSC bottom trawl survey RE model for allocation",fill="Area",color="Area")+
 theme( axis.text.x = element_text(hjust=1, angle = 90))+scale_x_continuous(limits=c(1984,2021),breaks=seq(1980,2020,by=5))







  ggplot(data=data[name=="SPB_std"&index!=1975&index<2018], aes(x=index, y=value)) + 
      geom_errorbar(aes(ymin=value-1.96*std.dev, ymax=value+1.96*std.dev), width=.25) +
      geom_line() +
      geom_point(size=3,color="brown")+theme1(base_size=20)+xlab("Year")+ylab("Spawning biomass (t)")+ylim(0,450000)



 ggplot(data=data[name=="SPB_std"&index!=1975&index<2018], aes(x=index, y=value)) + geom_line() +geom_point()+theme_bw(base_size=20)+   
 geom_ribbon(aes(ymin=value-2.326*std.dev, ymax=value+2.326*std.dev), alpha=0.2,fill="light green")+
 geom_ribbon(aes(ymin=value-1.96*std.dev, ymax=value+1.96*std.dev), alpha=0.2,fill="green")+
 geom_ribbon(aes(ymin=value-1.645*std.dev, ymax=value+1.645*std.dev), alpha=0.2,fill="dark green")+
 geom_errorbar(aes(ymin=value-1.96*std.dev, ymax=value+1.96*std.dev), width=.25) +
  xlab("Year")+ylab("Female spawning stock biomass (t)")+scale_x_continuous(limits=c(1977,2017),breaks=seq(1977,2017,by=1))+ylim(0,500000)



mods2=c("Model19.1","Model19.110YFor")
mods12<-SSgetoutput(dirvec=mods2)
modsS2<-SSsummarize(mods12)

# mods2=c("Model21.1e","Model21.1e_10Yforecast")
# mods12<-SSgetoutput(dirvec=mods2)
# modsS2<-SSsummarize(mods12)

data<-data.table(mods12[[1]]$derived_quants)
data2<-data.table(mods12[[2]]$derived_quants)

ssb<-data[Label%like%"SSB_1"|Label%like%"SSB_2"]

ssb2<-data2[Label%like%"SSB_1"|Label%like%"SSB_2"]

ssb$Yr<-1977:2099
ssb$Value2<-ssb2$Value
ssb$StdDev2<-ssb2$StdDev

ssb$Total1<-data.table(mods12[[1]]$timeseries)[Yr>1976]$Bio_all
ssb$Total2<-data.table(mods12[[2]]$timeseries)[Yr>1976]$Bio_all

TS<-data.table(mods12[[1]]$timeseries)
colm=names(TS)[TS[,names(TS)%like%"cat"]]
ssb$CATCH<-rowSums(TS[,colm,with=F][3:125])

fcatch<-data[Label%like%"ForeCatch"]

TS2<-data.table(mods12[[2]]$timeseries)
colm=names(TS2)[TS2[,names(TS2)%like%"cat"]]

ssb$CATCH<-rowSums(TS[,colm,with=F][3:125])
ssb$CATCH2<-rowSums(TS2[,colm,with=F][3:125])

fcatch<-data[Label%like%"ForeCatch"]
fcatch2<-data2[Label%like%"ForeCatch"]

ssb[Yr>2021]$CATCH<-fcatch$Value
ssb[Yr>2021]$CATCH2<-fcatch2$Value

ssb$StdCATCH<-0
ssb[Yr>2021]$StdCATCH<-fcatch$StdDev

ssb$StdCATCH2<-0
ssb[Yr>2021]$StdCATCH2<-fcatch2$StdDev

ssb$STATUS<-ssb$Value/(data[Label== "SSB_unfished"]$Value)

ssb$STATUS2<-ssb$Value2/(data2[Label== "SSB_unfished"]$Value)

colors <- c("FSSB 1977-present" = "blue", "FSSB 2010-present" = "dark green","Total biomass M1" = "red","Total biomass M2" = "brown","Catch M1"="black","Catch M2"="gray50")


ggplot(data=ssb[Yr>1976 & Yr<2037], aes(x=Yr, y=Value/2)) + 
  geom_line(aes(color="FSSB 1977-present",fill="FSSB 1977-present"),size=1.2)+
  theme_bw(base_size=18)+ 
  geom_bar(data=ssb[Yr>1976&Yr<2022],aes(y=CATCH,fill="Catch M1",color="Catch M1"),stat="identity",alpha=0.2)+
  geom_line(aes(y=Total1,color="Total biomass M1",fill="Total biomass M1"),size=1.5)+
  geom_point(data=ssb[Yr>2021&Yr<2037],aes(y=CATCH,fill="Catch M1",color="Catch M1"),size=4,shape=17)+ 
  geom_line(data=ssb[Yr>1976&Yr<2037],aes(y=Value2/2,color="FSSB 2010-present",fill="FSSB 2010-present"),size=1.2,,linetype=2)+ 
  geom_line(data=ssb[Yr>1976&Yr<2037],aes(y=Total2,color="Total biomass M2",fill="Total biomass M2"),size=1.2,linetype=2)+
  geom_point(data=ssb[Yr>2021&Yr<2037],aes(y=CATCH2,fill="Catch M2",color="Catch M2"),size=4,shape=18)+ 
   theme( axis.text.x = element_text(hjust=1, angle = 90))+ labs(x="Year",y="Biomass (t)",color = "Legend",fill="Legend") +
   scale_color_manual(values = colors)+scale_fill_manual(values = colors)+scale_x_continuous(limits=c(1977,2037),breaks=seq(1977,2037,by=1))



ggplot(data=ssb[Yr>2014&Yr<2037], aes(x=Yr, y=Value/2)) + 
  geom_line(aes(color="FSSB 1977-present",fill="FSSB 1977-present"),size=1.2)+
  theme_bw(base_size=18)+ 
  geom_bar(data=ssb[Yr>2014&Yr<2022],aes(y=CATCH,fill="Catch M1",color="Catch M1"),stat="identity",alpha=0.2)+
  geom_line(aes(y=Total1,color="Total biomass M1",fill="Total biomass M1"),size=1.5)+
  geom_point(data=ssb[Yr>2021&Yr<2037],aes(y=CATCH,fill="Catch M1",color="Catch M1"),size=4,shape=17)+ 
  geom_line(data=ssb[Yr>2014&Yr<2037],aes(y=Value2/2,color="FSSB 2010-present",fill="FSSB 2010-present"),size=1.2,,linetype=2)+ 
  geom_line(data=ssb[Yr>2014&Yr<2037],aes(y=Total2,color="Total biomass M2",fill="Total biomass M2"),size=1.2,linetype=2)+
  geom_point(data=ssb[Yr>2021&Yr<2037],aes(y=CATCH2,fill="Catch M2",color="Catch M2"),size=4,shape=18)+ 
   theme( axis.text.x = element_text(hjust=1, angle = 90))+ labs(x="Year",y="Biomass (t)",color = "Legend",fill="Legend") +
   scale_color_manual(values = colors)+scale_fill_manual(values = colors)+scale_x_continuous(limits=c(2014,2037),breaks=seq(2014,2037,by=1))







colors <- c("Female spawning biomass" = "blue", "Total biomass" = "red")

ggplot(data=ssb, aes(x=Yr, y=Value/2)) + geom_line(aes(color="Spawning biomass"),color="blue",size=1.2)+theme_bw(base_size=20)+ geom_line(aes(y=Total,color="Total biomass"),size=1.5)+  
 geom_ribbon(data=ssb[Yr>2021],aes(ymin=Value/2-1.96*StdDev/2, ymax=Value/2+1.96*StdDev/2), alpha=0.2,fill="blue")+
 geom_errorbar(data=ssb[Yr<2022],aes(ymin=Value/2-1.96*StdDev/2, ymax=Value/2+1.96*StdDev/2), width=.25) +
 theme( axis.text.x = element_text(hjust=1, angle = 90))+ labs(x="Year",y="Biomass (t)",color = "Legend") +
    scale_color_manual(values = colors)+scale_x_continuous(limits=c(1977,2036),breaks=seq(1977,2036,by=1))



colm=names(TS)[TS[,names(TS)%like%"cat"]]
CATCH=rowSums(TS[,colm,with=F])

colors <- c("Female spawning biomass" = "blue", "Total biomass" = "red","Catch"="black")

ggplot(data=ssb[Yr<2022], aes(x=Yr, y=Value/2)) + geom_line(aes(color="Female spawning biomass",fill="Female spawning biomass"),size=1.2)+theme_bw(base_size=20)+ 
geom_line(aes(y=Total1,color="Total biomass",fill="Total biomass"),size=1.5)+
geom_bar(aes(y=CATCH,fill="Catch",color="Catch"),stat="identity",alpha=0.2)+  
geom_ribbon(data=ssb[Yr<2022],aes(ymin=Value/2-1.96*StdDev/2, ymax=Value/2+1.96*StdDev/2,fill="Female spawning biomass"), alpha=0.2)+
#geom_errorbar(data=ssb[Yr<2022],aes(ymin=Value/2-1.96*StdDev/2, ymax=Value/2+1.96*StdDev/2), width=.25) +
theme( axis.text.x = element_text(hjust=1, angle = 90))+ labs(x="Year",y="Biomass (t)",color = "Legend",fill="Legend") +
    scale_color_manual(values = colors)+scale_fill_manual(values = colors)+scale_x_continuous(limits=c(1977,2022),breaks=seq(1977,2022,by=1))



colors <- c("Female spawning biomass M1" = "blue", "Female spawning biomass M2" = "light blue","Total biomass M1" = "red","Total biomass M1" = "orange","Catch M1"="black","Catch M2"="green")


ggplot(data=ssb[Yr>2014], aes(x=Yr, y=Value/2)) + geom_line(aes(color="Female spawning biomass M1",fill="Female spawning biomass M1"),size=1.2)+theme_bw(base_size=20)+ 
  geom_line(aes(y=Total1,color="Total biomass M1",fill="Total biomass M1"),size=1.5)+
  geom_bar(aes(y=CATCH,fill="Catch M1",color="Catch M1"),stat="identity",alpha=0.2)+ geom_line(data=ssb[Yr>2021],aes(y=CATCH1,fill="Catch M1",color="Catch M1"))+ 
  geom_ribbon(data=ssb[Yr>2014],aes(ymin=Value/2-1.96*StdDev/2, ymax=Value/2+1.96*StdDev/2,fill="Female spawning biomass"), alpha=0.2)+
  geom_errorbar(data=ssb[Yr>2021],aes(ymin=CATCH-1.96*StdCATCH, ymax=CATCH+1.96*StdCATCH), width=.25)+
  theme( axis.text.x = element_text(hjust=1, angle = 90))+ labs(x="Year",y="Biomass (t)",color = "Legend",fill="Legend") +
    scale_color_manual(values = colors)+scale_fill_manual(values = colors)+scale_x_continuous(limits=c(2014,2100),breaks=seq(2014,2100,by=1))






colors <- c("Catch/Total Biomass 1977-2021 average" = "blue", "Catch/Total Biomass 2010-2021 average" = "red")
ssb$CSB<-ssb$CATCH/ssb$Total1
ssb$CSB2<-ssb$CATCH2/ssb$Total2
 


ggplot(ssb[Yr<2037],aes(x=Yr,y=CSB))+geom_line(aes(color="Catch/Total Biomass 1977-2021 average"),size=1.2)+
  theme_bw(base_size=20)+
  geom_line(aes(y=CSB2,color="Catch/Total Biomass 2010-2021 average"),size=1.5,linetype=2)+
  theme( axis.text.x = element_text(hjust=1, angle = 90))+ labs(x="Year",y="Catch/ Total Biomass",color = "Legend",fill="Legend") +
  scale_color_manual(values = colors)+scale_fill_manual(values = colors)+scale_x_continuous(limits=c(2014,2037),breaks=seq(2014,2037,by=1))




ggplot(data=ssb[Yr>2014&Yr<2037], aes(x=Yr, y=Value/2)) + geom_line(aes(color="FSSB 1977-present",fill="FSSB 1977-present"),size=1.2)+theme_bw(base_size=20)+ 
  geom_bar(data=ssb[Yr>2014&Yr<2022],aes(y=CATCH,fill="Catch M1",color="Catch M1"),stat="identity",alpha=0.2)+
  geom_point(data=ssb[Yr>2021&Yr<2037],aes(y=CATCH,fill="Catch M1",color="Catch M1"),size=4,shape=17)+ 
  geom_line(data=ssb[Yr>2014&Yr<2037],aes(y=Value2/2,color="FSSB 2010-present",fill="FSSB 2010-present"),size=1.2,,linetype=2)+ 
  geom_point(data=ssb[Yr>2021&Yr<2037],aes(y=CATCH2,fill="Catch M2",color="Catch M2"),size=4,shape=18)+ 
   theme( axis.text.x = element_text(hjust=1, angle = 90))+ labs(x="Year",y="Biomass (t)",color = "Legend",fill="Legend") +
    scale_color_manual(values = colors)+scale_fill_manual(values = colors)+scale_x_continuous(limits=c(2014,2037),breaks=seq(2014,2037,by=1))





  geom_line(aes(y=Value2/2,color="Female spawning biomass M2",fill="Female spawning biomass M2"),size=1.2)+theme_bw(base_size=20)+ 
  geom_line(aes(y=Total2,color="Total biomass",fill="Total biomass"),size=1.5)+
  geom_bar(aes(y=CATCH2,fill="Catch",color="Catch"),stat="identity",alpha=0.2)+ geom_bar(data=ssb[Yr>2021],aes(y=CATCH2,fill="Catch",color="Catch"),stat="identity",alpha=0.3)+ 
  geom_ribbon(data=ssb[Yr>2014],aes(ymin=Value2/2-1.96*StdDev2/2, ymax=Value2/2+1.96*StdDev2/2,fill="Female spawning biomass"), alpha=0.2)+
  geom_errorbar(data=ssb[Yr>2021],aes(ymin=CATCH2-1.96*StdCATCH2, ymax=CATCH2+1.96*StdCATCH2), width=.25)+
  theme( axis.text.x = element_text(hjust=1, angle = 90))+ labs(x="Year",y="Biomass (t)",color = "Legend",fill="Legend") +
    scale_color_manual(values = colors)+scale_fill_manual(values = colors)+scale_x_continuous(limits=c(2014,2100),breaks=seq(2014,2100,by=1))


colors <- c("FSSB 1977-present" = "blue", "FSSB 2010-present" = "dark green","Total biomass M1" = "red","Total biomass M2" = "brown","Catch M1"="black","Catch M2"="gray50")


MAX_CATCH<-max(ssb$CATCH2)

ggplot(data=ssb[Yr>2014&Yr<2037], aes(x=Yr, y=STATUS*MAX_CATCH)) + geom_line(aes(color="FSSB 1977-present",fill="FSSB 1977-present"),size=1.2)+
  theme_bw(base_size=20)+ 
  geom_bar(data=ssb[Yr>2014&Yr<2022],aes(y=CATCH,fill="Catch M1",color="Catch M1"),stat="identity",alpha=0.2)+
  geom_point(data=ssb[Yr>2021&Yr<2037],aes(y=CATCH,fill="Catch M1",color="Catch M1"),size=4,shape=17)+ 
  geom_line(data=ssb[Yr>2014&Yr<2037],aes(y=STATUS2*MAX_CATCH,color="FSSB 2010-present",fill="FSSB 2010-present"),size=1.2,,linetype=2)+ 
  geom_point(data=ssb[Yr>2021&Yr<2037],aes(y=CATCH2,fill="Catch M2",color="Catch M2"),size=4,shape=18)+ 
   theme( axis.text.x = element_text(hjust=1, angle = 90))+ labs(x="Year",y="Biomass (t)",color = "Legend",fill="Legend") +
  scale_y_continuous(
    
    # Features of the first axis
    name = "Catch (t)",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~./MAX_CATCH, name="Stock Status B%")
  )+
    scale_color_manual(values = colors)+scale_fill_manual(values = colors)+scale_x_continuous(limits=c(2014,2037),breaks=seq(2014,2037,by=1))






data<-data.table(mods12[[1]]$derived_quants)

ssb<-data[Label%like%"SSB_1"|Label%like%"SSB_2"]


ssb$Yr<-1977:2099

ssb$Total1<-data.table(mods12[[1]]$timeseries)[Yr>1976]$Bio_all

TS<-data.table(mods12[[1]]$timeseries)
colm=names(TS)[TS[,names(TS)%like%"cat"]]
ssb$CATCH<-rowSums(TS[,colm,with=F][3:125])

fcatch<-data[Label%like%"ForeCatch"]

TS2<-data.table(mods12[[2]]$timeseries)
colm=names(TS2)[TS2[,names(TS2)%like%"cat"]]

ssb$CATCH<-rowSums(TS[,colm,with=F][3:125])

fcatch<-data[Label%like%"ForeCatch"]

ssb[Yr>2021]$CATCH<-fcatch$Value

ssb$StdCATCH<-0
ssb[Yr>2021]$StdCATCH<-fcatch$StdDev

ssb$Status<-ssb$Value/data.table(mods12[[1]]$derived_quants)[Label=="SSB_unfished"]$Value







mods2=c("Model19.1","Model19.110YFor")
mods12<-SSgetoutput(dirvec=mods2)
modsS2<-SSsummarize(mods12)

data<-data.table(mods12[[1]]$derived_quants)
data2<-data.table(mods12[[2]]$derived_quants)

ssb<-data[Label%like%"SSB_1"|Label%like%"SSB_2"]

ssb2<-data2[Label%like%"SSB_1"|Label%like%"SSB_2"]

ssb$Yr<-1977:2036
ssb$Value2<-ssb2$Value
ssb$StdDev2<-ssb2$StdDev

ssb$Total1<-data.table(mods12[[1]]$timeseries)[Yr>1976]$Bio_all
ssb$Total2<-data.table(mods12[[2]]$timeseries)[Yr>1976]$Bio_all

TS<-data.table(mods12[[1]]$timeseries)
colm=names(TS)[TS[,names(TS)%like%"cat"]]
ssb$CATCH<-rowSums(TS[,colm,with=F][3:62])

fcatch<-data[Label%like%"ForeCatch"]

TS2<-data.table(mods12[[2]]$timeseries)
colm=names(TS2)[TS2[,names(TS2)%like%"cat"]]

ssb$CATCH<-rowSums(TS[,colm,with=F][3:62])
ssb$CATCH2<-rowSums(TS2[,colm,with=F][3:62])

fcatch<-data[Label%like%"ForeCatch"]
fcatch2<-data2[Label%like%"ForeCatch"]

ssb[Yr>2021]$CATCH<-fcatch$Value
ssb[Yr>2021]$CATCH2<-fcatch2$Value

ssb$StdCATCH<-0
ssb[Yr>2021]$StdCATCH<-fcatch$StdDev

ssb$StdCATCH2<-0
ssb[Yr>2021]$StdCATCH2<-fcatch2$StdDev

ssb$STATUS<-ssb$Value/(data[Label== "SSB_unfished"]$Value)

ssb$STATUS2<-ssb$Value2/(data2[Label== "SSB_unfished"]$Value)




colors <- c("Female spawning biomass" = "blue", "Total biomass" = "red","Catch"="black")

ggplot(data=ssb[Yr>2014&Yr<2037], aes(x=Yr, y=Value/2)) + geom_line(aes(color="Female spawning biomass",fill="Female spawning biomass"),size=1.2)+theme_bw(base_size=20)+ 
  geom_bar(data=ssb[Yr<2022],aes(y=CATCH,fill="Catch",color="Catch"),stat="identity",alpha=0.2)+
  geom_line(aes(y=Total1,color="Total biomass",fill="Total biomass"),size=1.5)+
  geom_point(data=ssb[Yr>2021&Yr<2037],aes(y=CATCH,fill="Catch",color="Catch"),size=2,shape=17)+ 
  geom_ribbon(data=ssb[Yr>2014&Yr<2037],aes(ymin=Value/2-1.96*StdDev/2, ymax=Value/2+1.96*StdDev/2,fill="Female spawning biomass"), alpha=0.2)+
  geom_errorbar(data=ssb[Yr>2021],aes(ymin=CATCH-1.96*StdCATCH, ymax=CATCH+1.96*StdCATCH), width=.25)+

   theme( axis.text.x = element_text(hjust=1, angle = 90))+ labs(x="Year",y="Biomass (t)",color = "Legend",fill="Legend") +
    scale_color_manual(values = colors)+scale_fill_manual(values = colors)+scale_x_continuous(limits=c(2014,2037),breaks=seq(2014,2037,by=1))





MAX_CATCH<-max(ssb$CATCH)

ggplot(data=ssb[Yr>2014&Yr<2037], aes(x=Yr, y=Status*MAX_CATCH)) + geom_line(aes(color="Female spawning biomass",fill="Female spawning biomass"),size=1.2)+theme_bw(base_size=20)+ 
geom_bar(data=ssb[Yr>2014&Yr<2022],aes(y=CATCH,fill="Catch",color="Catch M1"),stat="identity",alpha=0.2)+
geom_point(data=ssb[Yr>2021&Yr<2037],aes(y=CATCH,fill="Catch",color="Catch"),size=4,shape=17)+ 
theme( axis.text.x = element_text(hjust=1, angle = 90))+ labs(x="Year",color = "Legend",fill="Legend") +
 scale_y_continuous(
    
    # Features of the first axis
    name = "Catch (t)",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~./MAX_CATCH, name="Stock Status B%")
  )+
    scale_color_manual(values = colors)+scale_fill_manual(values = colors)+scale_x_continuous(limits=c(2014,2037),breaks=seq(2014,2037,by=1))






colors <- c("Base model" = "blue", "Heatwave linked" = "red")


ggplot(Ms,aes(Yr,Model1.0))+geom_line(aes(color="Base model"),size=1.2)+geom_line(aes(y=Model2.0,color="Heatwave linked"),size=1.2)+ylim(0.2,1.1)+
theme_bw(base_size=20)+labs(y="Natural mortality",x="Year")+
theme( axis.text.x = element_text(hjust=1, angle = 90))+ labs(x="Year",color = "Legend",fill="Legend")