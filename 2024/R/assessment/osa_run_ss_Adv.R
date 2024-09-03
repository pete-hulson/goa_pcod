
## wrappers for Stock synthesis OSA diagnostics
## Created by Steve Barbeaux 8/8/2024 mostly stolen from work by Cole Monnahan

## Run OSA analysis for age composition
#' @model directory of the model
#' @ages ages over which to do the analysis, should match the model
#' @fleet fleet number
#' @sx sex to do the analysis
#' @N1 number of draws to be conducted
### example  
## mods=dir()[1:4]  ##In the model directory with four models
## OSA_run_SS_age(model=mods[1],ages=0:12, fleet=2, sx=1, N1=100, stck='EBS_COD',surv='EBSSHELF')


OSA_run_SS_age<-function(model=mods[1],ages=0:12, fleet=2, sx=1, N1=100, stck='EBS_COD',surv='EBSSHELF'){
   require(data.table)
   library(compResidual)
   library(ggplot2)
   mods1<-r4ss::SSgetoutput(dirvec=model)
   age<-data.table::data.table(mods1[[1]]$agedbase[,c(1,6,13,16:19)])[Bin%in%ages & Fleet==fleet & Sex==sx]
   age<-data.table::data.table(melt(age,c('Yr','Fleet','Sex','Bin')))
   o<-age[variable=='Obs']
   o<-maditr::dcast(o,Yr~Bin)
   p<-age[variable=='Exp']
   p<-maditr::dcast(p,Yr~Bin)
   pearson<-age[variable=='Pearson']
   pearson<-maditr::dcast(pearson,Yr~Bin)
   yrs<-o$Yr
   o <- as.matrix(o[,-1])
   p <- as.matrix(p[,-1])
   pearson <- as.matrix(pearson[,-1])
   Neff<-round(data.table::data.table(mods1[[1]]$agedbase)[Bin==ages[1] & Sex==sx]$effN)
   plot_osa_comps2(o, p, pearson, index=ages, index_label=paste0(model,'_age'), years=yrs, Neff=Neff, 
   stock=paste("A_sex=",sx,stck,model,sep="_"), survey=surv,N=N1)
  }


## Run OSA analysis for length composition
#' @model directory of the model
#' @lengths length bins over which to do the analysis, should match the model
#' @fleet fleet number
#' @sx sex to do the analysis
#' @N1 number of draws to be conducted
### example  
## mods=dir()[1:4]  ##In the model directory with four models
## OSA_run_SS_length(model=mods[1],lengths=seq(4.5,119.5,by=5),fleet=2, sx=1, N1=100, stck='EBS_COD',surv='EBSSHELF')


OSA_run_SS_length<-function(model=mods[1],lengths=seq(4.5,119.5,by=5), fleet=2, sx=1, stck='EBS_COD',surv='EBSSHELF',N1=100){
  library(compResidual)
  mods1<-r4ss::SSgetoutput(dirvec=model)
  age<-data.table::data.table(mods1[[1]]$lendbase[,c(1,6,13,16:19)])[Bin%in%lengths & Fleet==fleet &Sex==sx]
  age<-data.table::data.table(melt(age,c('Yr','Fleet','Sex','Bin')))
  o<-age[variable=='Obs']
  o<-maditr::dcast(o,Yr~Bin)
  p<-age[variable=='Exp']
  p<-maditr::dcast(p,Yr~Bin)
  pearson<-age[variable=='Pearson']
  pearson<-maditr::dcast(pearson,Yr~Bin)
  yrs<-o$Yr
  o <- as.matrix(o[,-1])
  p <- as.matrix(p[,-1])
  pearson <- as.matrix(pearson[,-1])
  Neff<-round(data.table::data.table(mods1[[1]]$lendbase)[Bin==lengths[1]& Fleet==fleet & Sex==sx]$effN)
  plot_osa_comps2(o,p, pearson, index=lengths, index_label=paste0(model,'_length bin'), years=yrs, Neff=Neff, 
                  stock=paste("L_sex=",sx,stck,model,sep="_"), survey=surv,N=N1)
}


#' Explore OSA residuals for multinomial composition data and
#' compare to Pearson
#' @param obs,exp,pearson the observed, expected and Pearson
#'   residual matrices with rows as years and columns as ages (or
#'   lengths)
#' @param index,years vectors giving the index of ages (or length) and years
#' @param index_label character value indicating 'age' or 'length bin' depending on comp type
#' @param stock,survey characters given the stock and survey,
#' used to create filename stock_survey.pdf
#' @param outpath folder name for output, e.g. 'figs'
#' @return returns nothing but creates a PDF file in the working
#' directory
#'
plot_osa_comps2 <- function(obs, exp, pearson, index, years=yrs, index_label, Neff,
                           stock, survey, outpath = '',N=N1){

  
  stopifnot(all.equal(nrow(obs), nrow(exp), nrow(pearson),
                      length(years)))
  stopifnot(all.equal(ncol(obs), ncol(exp), ncol(pearson), length(index)))

  filename <- paste0(stock,"_",survey,"_", gsub('\\s', '_', index_label), ".pdf")

  if(is.null(outpath)) {
    pdf(here::here(filename), onefile=TRUE, width=10, height=10)
  } else {
    pdf(here::here(outpath, filename), onefile=TRUE, width=10, height=10)
  }

  #on.exit(dev.off())
  ## Neff <- ceiling(Neff)
  o1 <- round(Neff*obs/rowSums(obs),0); p=exp/rowSums(exp)
  ## default output
  res1<-list()
  sdnr1<-list()
  for(i in 1:N){
    res1[[i]] <- resMulti(t(o1), t(p))
    sdnr1[[i]]<-sd(res1[[i]])
  }
  sdnr<-data.table(do.call(rbind,sdnr1))
  names(sdnr)="sdnr"
  sdnr$ID<-1:nrow(sdnr)
  sdnr<-sdnr[order(sdnr),]

  HCI<-sqrt(qchisq(.975, (length(res1[[1]])-1))/(length(res1[[1]])-1))
  LCI<-sqrt(qchisq(.025, (length(res1[[1]])-1))/(length(res1[[1]])-1))
  n=1
  if(N>1){n=trunc(N/2)}

  
  psdnr<-ggplot(sdnr,aes(x=sdnr))+geom_histogram(color="black",fill="salmon")
  psdnr<-psdnr+geom_vline(data=sdnr[n,],aes(xintercept =sdnr), colour="blue",linewidth=1.5,linetype=3)+theme_bw(base_size=16)+labs(x="OSA SDNR",y="Count")
  psdnr<-psdnr+geom_vline(data=sdnr[n,],aes(xintercept =HCI), colour="red",linewidth=1.5,linetype=2)
  psdnr<-psdnr+geom_vline(data=sdnr[n,],aes(xintercept =LCI), colour="red",linewidth=1.5,linetype=2)
  print(psdnr)

   
   res=res1[[sdnr$ID[n]]]

  if(!all(is.finite(res))){
    warning("failed to calculate OSA residuals for ", stock)
    return(NULL)
  }
  #par(mfrow=c(2,2))
  plot_res2(x=res,o=obs,e=p,pr=pearson,yr=years,inde=index)
  dev.off()

  print(psdnr)
  windows(width=20,height=12)
  plot_res2(x=res,o=obs,e=p,pr=pearson,yr=years,inde=index)
  
}




#' @x Residual object as returned from one of the residual functions
#' @o observed
#' @e expected
#' @pr pearson residuals

## stole key pieces from compResidual:::plot.cres
plot_res2 <- function(x=res,o=o1 ,e=p, pr=pearson,yr=years,inde=index){
  library(compResidual) 
  library(ggplot2)
  library(ggpubr)

  Neff <- colSums(o)
  ehat <- Neff*e
  V <- Neff*e*(1-e)
  
  nbins <- nrow(o)
  nyrs<-nrow(e)
  pearson<-pr
  if(!all(is.finite(x))){
    ind <- (!is.finite(x))
    message("the following  were not finite")
    message("observed counts= ", paste(o[-nbins,][ind], collapse=' '))
    message("expected counts= ", paste(round(ehat[-nbins,][ind],2), collapse=' '))
    message("Pearson resid= ", paste(round(pearson[-nbins,][ind],2), collapse=' '))
    message("OSA resid= ", paste(round(x[ind],2), collapse=' '))
    stop('non-finite residuals')
    pearson[2,17]
    pearson[!is.finite(x)]
    pearson[ind]
    x[ind]
    o[-nbins,][ind]
    ehat[-nbins,][ind]
    pearson[-nbins,][ind]
  }
  pearson=t(pearson)

  o_tab<-data.table(t(matrix(x,nrow=length(inde)-1)))
  names(o_tab)<-paste(inde[1:(length(inde)-1)])
  o_tab$yr=yr

  o_tab<-melt(o_tab,"yr")
  names(o_tab)<-c("Year","Index","Value")
  o_tab$Sign<-"<0"
  o_tab[Value>0]$Sign<-">0"

  bubble_osa<-ggplot(data=o_tab,aes(y=Index,x=Year,color=Sign,size=abs(Value),alpha=abs(Value)))+
  geom_point()+theme_bw(base_size=12)+scale_color_manual(values=c("blue","red"))+
  labs(color="resid",sign="resid",size="resid",alpha="resid", title="OSA residuals")
  
  p_tab<-data.table(t(pearson))
  p_tab$yr<-yr
  p_tab<-melt(p_tab,"yr")
  names(p_tab)<-c("Year","Index","Value")
  p_tab$Sign<-"<0"
  p_tab[Value>0]$Sign<-">0"
  bubble_pear<-ggplot(data=p_tab,aes(y=Index,x=Year,color=Sign,size=abs(Value),alpha=abs(Value)))+
  geom_point()+theme_bw(base_size=12)+scale_color_manual(values=c("blue","red"))+
  labs(color="resid",sign="resid",size="resid",alpha="resid", title="Pearson residuals")

  pfram<-data.frame(y=c(pearson))
  o_tab1<-data.table(t(matrix(x,nrow=length(inde)-1)))
  o_val<-melt(o_tab1)$value
  ofram<-data.frame(y=c(o_val))
  ofram$type="OSA"
  pfram$type="Pearson"
  fram<-rbind(ofram,pfram)

  o_sdnr<-sd(subset(fram,type=="OSA")$y)
  p_sdnr<-sd(subset(fram,type=="Pearson")$y)

  qq_p<-ggplot(fram,aes(sample=y,shape=type,color=type))+stat_qq(size=2)+scale_color_manual(values=c("blue","red"))+
   theme_bw(base_size=12)+scale_shape_manual(values=c(16,1))+geom_abline()+
   labs(color="Residual type",shape='Residual type', title="QQ plots",x="",y="")+
   annotate("text",label=paste0("OSA SDNR = ",round(o_sdnr,3)),x=0,y=min(fram$y)+(1-(min(fram$y)/8)),hjust="left")+
   annotate("text",label=paste0("Pearson SDNR = ",round(p_sdnr,3)),x=0,y = min(fram$y)+1,hjust="left")


  oagg <- colSums(o)/sum(o)
  eagg <- colSums(e)/sum(e)
  tab1<-data.table(Index=inde,Obs=oagg,Exp=eagg)

  agg_plot<-ggplot(data=tab1)+geom_bar(aes(x=Index,y=Obs),stat='identity',color="blue",fill='blue')+
   geom_line(aes(x=Index,y=Exp),color='red')+theme_bw(base_size=16)+
   labs(title="Aggregated fits",y="Proportion",x="Index")

  

   plotg<-ggpubr::ggarrange(bubble_pear, bubble_osa,qq_p,agg_plot,ncol=2,nrow=2,heights=c(1,1),widths=c(1,1),align = c("h"),
          common.legend = F, legend = "bottom")
   print(plotg)


}
