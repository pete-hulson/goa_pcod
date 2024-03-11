library(stringr)
library(coda)
library(ggplot2)


MCMC<-data.frame(read.table("C:/WORKING_FOLDER/2021 Stock Assessments/2021 Pacific cod/Models/Model21.1e_MCMC/derived_posteriors.sso",header=T))
MCMC1<-data.frame(read.table("C:/WORKING_FOLDER/2021 Stock Assessments/2021 Pacific cod/Models/Model21.1e_MCMC/derived_posteriors.sso",header=T))

MCMC1P<-data.frame(read.table("C:/WORKING_FOLDER/2021 Stock Assessments/2021 Pacific cod/Models/Model21.1e_MCMC/posteriors.sso",header=T))

#MCMC2<-data.frame(read.table("C:/WORKING_FOLDER/2021 Stock Assessments/2021 Pacific cod/Models/Model21.1e_MCMC/derived_posteriors.sso"))

#mcmc=data.frame(read.table("derived_posteriors.sso",sep=" ",header=T))

#mcmc1<-subset(MCMC,select=-c(MODEL))
#mcmc2<-subset(MCMC1,select=-c(MODEL))
#mcmc3<-subset(MCMC2,select=-c(MODEL))

SSB3<-vector("list",length=2)

SSB3[[1]]<-MCMC[,5:64]
SSB3[[2]]<-MCMC1[,5:64]
#SSB3[[3]]<-MCMC1[,5:64]

Yrs1=as.numeric(do.call(rbind,str_split(names(SSB3[[1]]),"_"))[,2])


SSB4.1<-data.frame(Yrs=rep(Yrs1,each=nrow(SSB3[[1]])),SSB=0)
SSB4.2<-data.frame(Yrs=rep(Yrs1,each=nrow(SSB3[[2]])),SSB=0)
#SSB4.3<-data.frame(Yrs=rep(Yrs1,each=nrow(SSB3[[3]])),SSB=0)

SSB4.1$SSB[1:nrow(SSB3[[1]])]<-SSB3[[1]][1:nrow(SSB3[[1]]),1]/2
SSB4.2$SSB[1:nrow(SSB3[[2]])]<-SSB3[[2]][1:nrow(SSB3[[2]]),1]/2
#SSB4.3$SSB[1:nrow(SSB3[[3]])]<-SSB3[[3]][1:nrow(SSB3[[3]]),1]/2

for(i in 1:46){
SSB4.1$SSB[((i*nrow(SSB3[[1]]))+1):((i+1)*nrow(SSB3[[1]]))]<-SSB3[[1]][1:nrow(SSB3[[1]]),i+1]/2
SSB4.2$SSB[((i*nrow(SSB3[[2]]))+1):((i+1)*nrow(SSB3[[2]]))]<-SSB3[[2]][1:nrow(SSB3[[2]]),i+1]/2
#SSB4.3$SSB[((i*nrow(SSB3[[3]]))+1):((i+1)*nrow(SSB3[[3]]))]<-SSB3[[3]][1:nrow(SSB3[[3]]),i+1]/2
}

SSB4.1<-data.table(SSB4.1)
SSB4.2<-data.table(SSB4.2)
#SSB4.3<-data.table(SSB4.3)
MEANS2.1=SSB4.1[,list(MSSB=median(SSB)),by="Yrs"]
MEANS2.2=SSB4.2[,list(MSSB=median(SSB)),by="Yrs"]
#MEANS2.3=SSB4.3[,list(MSSB=median(SSB)),by="Yrs"]




#mcmc<-subset(MCMC3,select=-c(MODEL))


SSB<-MCMC1[,5:64]
REC<-MCMC1[,67:126]

Yrs=as.numeric(do.call(rbind,str_split(names(SSB),"_"))[,2])

SSB1<-data.frame(Yrs=rep(Yrs,each=nrow(SSB)),SSB=0,REC=0)
SSB1$SSB[1:nrow(SSB)]<-SSB[1:nrow(SSB),1]/2
SSB1$REC[1:nrow(REC)]<-REC[1:nrow(REC),1]

for(i in 1:59){
SSB1$SSB[((i*nrow(SSB))+1):((i+1)*nrow(SSB))]<-SSB[1:nrow(SSB),i+1]/2
SSB1$REC[((i*nrow(REC))+1):((i+1)*nrow(REC))]<-REC[1:nrow(SSB),i+1]
}

SSB1<-data.table(SSB1)
MEANS=SSB1[,list(MREC=median(REC),MSSB=median(SSB)),by="Yrs"]


SSB20<-quantile(MCMC[,306],0.5)/2*0.2/10^5
SSB17.5<-quantile(MCMC[,306],0.5)/2*0.175/10^5

Ribbon=SSB1[,list(SSB2=quantile(MCMC[,306],0.025)/2*0.2/10^5,SSB97=quantile(MCMC[,306],0.975)/2*0.2/10^5)]

Ribbon<-data.table(Yrs=Yrs,SSB2=Ribbon$SSB2,SSB97=Ribbon$SSB97)

d<-ggplot()+geom_ribbon(data=Ribbon,aes(x=Yrs,ymin=SSB2,ymax=SSB97),fill="orange",alpha=0.2)
d<-d+geom_violin(data=SSB1,aes(x=Yrs,y=SSB/10^5,group=Yrs),fill="gray75",alpha=0.5)+ylim(0,max(SSB1$SSB/10^5))+geom_line(data=MEANS,aes(y=MSSB/10^5,x=Yrs,group=""),size=1)+geom_point(data=MEANS,aes(y=MSSB/10^5,x=Yrs,group=""))
d<-d+theme_bw(base_size=18)+scale_x_continuous(limits=c(1976.5,2036.5),breaks=seq(1977,2036,by=2))+theme( axis.text.x = element_text(hjust=1, angle = 90))
d<-d+labs(y=expression(paste("Female spawning biomass ( ",1%*%10^5," t )")), x="")
#d<-d+geom_line(data=MEANS2.1,aes(y=MSSB/10^5,x=Yrs,group=""),color="gray50",linetype=3,size=1)
#d<-d+geom_line(data=MEANS2.2,aes(y=MSSB/10^5,x=Yrs,group=""),color="gray50",linetype=3,size=1)
#d<-d+geom_line(data=MEANS2.3,aes(y=MSSB/10^5,x=Yrs,group=""),color="gray50",linetype=3,size=1)
d
d<-d+geom_hline(yintercept=SSB20,linetype=3,size=1.1)
d<-d+geom_hline(yintercept=SSB17.5,color="red",linetype=2,size=1.1)
d


d2<-ggplot()+geom_violin(data=SSB1,aes(x=Yrs,y=REC/10^6,group=Yrs),fill="gray50")+ylim(0,max(SSB1$REC/10^6))+geom_line(data=MEANS,aes(y=MREC/10^6,x=Yrs,group=""))+geom_point(data=MEANS,aes(y=MREC/10^6,x=Yrs,group=""))
d2<-d2+theme_bw(base_size=18)+scale_x_continuous(limits=c(1976.5,2036.5),breaks=seq(1977,2036,by=2))+theme( axis.text.x = element_text(hjust=1, angle = 90))
d2<-d2+labs(y=expression(paste("Age-0 abundance ( ",1%*%10^9," t )")), x="Year")
d2



multiplot <- function(..., plotlist=NULL, cols) {
        
    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)
    
    numPlots = length(plots)
    
    # Make the panel
    plotCols = cols                          # Number of columns of plots
    plotRows = ceiling(numPlots/plotCols) # Number of rows needed, calculated from # of cols
    
    # Set up the page
    grid::grid.newpage()
    grid::pushViewport(grid::viewport(layout = grid::grid.layout(plotRows, plotCols)))
    vplayout <- function(x, y)
        grid::viewport(layout.pos.row = x, layout.pos.col = y)
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
        curRow = ceiling(i/plotCols)
        curCol = (i-1) %% plotCols + 1
        print(plots[[i]], vp = vplayout(curRow, curCol ))
    }
    
}



multiplot(d,d2,cols=1)


d<-ggplot(MCMCT)+geom_density(aes(SSB_2020/(187778*2)))
d<-d+geom_vline(xintercept=sort(MCMCT$SSB_2020/(187778*2))[495],col="blue",size=1.25)
d<-d+geom_vline(xintercept=0.2,linetype=2)+geom_vline(xintercept=0.175,linetype=3,color="red",size=1.25)
d<-d+geom_vline(xintercept=73193.4/(187778*2),linetype=4,color="goldenrod",size=1.25)
d<-d+theme_bw(base_size=20)+xlim(0.1,0.3)+xlab(expression(paste(SSB[2020]/SSB[0])))



d<-ggplot(MCMCT)+geom_density(aes(SSB_2019/(187778*2)))
d<-d+geom_vline(xintercept=sort(MCMCT$SSB_2019/(187778*2))[495],col="blue",size=1.25)
d<-d+geom_vline(xintercept=0.2,linetype=2)+geom_vline(xintercept=0.175,linetype=3,color="red",size=1.25)
d<-d+geom_vline(xintercept=70462.1/(187778*2),linetype=4,color="goldenrod",size=1.25)
d<-d+theme_bw(base_size=20)+xlim(0.1,0.3)+xlab(expression(paste(SSB[2019]/SSB[0])))

names(MCMC1P)[92]<-"LnQ_base_LLSrv_ENV_mult"
 "NatM_uniform_Fem_GP_1_ENV_add"                 
 [10] "L_at_Amin_Fem_GP_1_ENV_mult"                   
 [11] "L_at_Amax_Fem_GP_1_ENV_mult"                   
 [12] "VonBert_K_Fem_GP_1_ENV_mult"     

d1<-ggplot(MCMC1P)+geom_density(aes(NatM_uniform_Fem_GP_1_ENV_add))+theme_bw(base_size=20)+labs(x='M environmental link')+xlim(-1,3)
d2<-ggplot(MCMC1P)+geom_density(aes(L_at_Amin_Fem_GP_1_ENV_mult))+theme_bw(base_size=20)+labs(x='Lmin environmental link')+xlim(0,3)
d3<-ggplot(MCMC1P)+geom_density(aes(L_at_Amax_Fem_GP_1_ENV_mult))+theme_bw(base_size=20)+labs(x='Lmax environmental link')+xlim(-0.25,0.25)
d4<-ggplot(MCMC1P)+geom_density(aes(VonBert_K_Fem_GP_1_ENV_mult))+theme_bw(base_size=20)+labs(x='K environmental link')+xlim(-0.25,0.25)
d5<-ggplot(MCMC1P)+geom_density(aes(LnQ_base_LLSrv_ENV_mult))+theme_bw(base_size=20)+labs(x='Longline survey Q environmental link')+xlim(-1,3)
multiplot(d1,d2,d3,d4,d5,cols=2)




MCMC_quant<-matrix(ncol=4,nrow=ncol(MCMC)-1)
 for(i in 1:(ncol(MCMC)-1)){
    MCMC_quant[i,1]<-names(MCMC)[i+1]
    MCMC_quant[i,c(2:4)]<-as.numeric(quantile(MCMC[,i+1],c(0.0275,0.5,0.975)))
}
MCMC_quant<-data.table(MCMC_quant)
names(MCMC_quant)<-c("Label","P0275","P50","P975")


MCMCP_quant<-matrix(ncol=4,nrow=ncol(MCMC1P)-1)
 for(i in 1:(ncol(MCMC1P)-1)){
    MCMCP_quant[i,1]<-names(MCMC1P)[i+1]
    MCMCP_quant[i,c(2:4)]<-as.numeric(quantile(MCMC1P[,i+1],c(0.0275,0.5,0.975)))
}
MCMCP_quant<-data.table(MCMCP_quant)
names(MCMCP_quant)<-c("Label","P0275","P50","P975")



a1=ggplot(MCMC,aes(y=Objective_function,x=Iter))+geom_line()+theme_bw(base_size=20)+labs(y='Objective function',x='Iteration')
a2 =  ggplot(MCMC)+geom_density(aes(Objective_function))+theme_bw(base_size=20)+labs(x='Objective function')

conf.level <- 0.95
ciline <- qnorm((1 - conf.level)/2)/sqrt(length(MCMC$Objective_function))
bacf <- acf(MCMC$Objective_function, plot = FALSE)
bacfdf <- with(bacf, data.frame(lag, acf))

 a3 <- ggplot(data=bacfdf, mapping=aes(x=lag, y=acf)) + geom_bar(stat = "identity", position = "identity",width=0.1)+theme_bw(base_size=20)+geom_hline(yintercept=0.05,color='blue',linetype=2)

a4 <-



d1<-ggplot(MCMC)+geom_density(aes(SSB_2022/SSB_unfished))+
theme_bw(base_size=20)+labs(x='2022 female spawning biomass/unfished female spawning biomass')+
xlim(0.10,0.5)+geom_vline(xintercept=0.2,color="orange",linetype=3,size=1.0)+geom_vline(xintercept=0.175,color="red",linetype=2,size=1.0)+
geom_vline(xintercept=sort(MCMC$SSB_2022/MCMC$SSB_unfished)[245],color="Blue",size=2)

d2<-ggplot(MCMC)+geom_density(aes(SSB_2023/SSB_unfished))+
theme_bw(base_size=20)+labs(x='2023 female spawning biomass/unfished female spawning biomass')+
xlim(0.10,0.5)+geom_vline(xintercept=0.2,color="orange",linetype=3,size=1.0)+geom_vline(xintercept=0.175,color="red",linetype=2,size=1.0)+
geom_vline(xintercept=sort(MCMC$SSB_2023/MCMC$SSB_unfished)[245],color="Blue",size=2)

multiplot(d1,d2,cols=1)


