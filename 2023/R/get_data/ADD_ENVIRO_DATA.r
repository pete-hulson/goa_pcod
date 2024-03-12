Add_climate <- function(endyr, Old_datafile = NULL, New_datafile = NULL, dir1 = NULL, dir2 = NULL){

  setwd(dir2)
  ssdat = SS_readdat_3.30(file = Old_datafile)
  
  setwd(dir1)

  TEMPHW <- data.table(read.csv("TEMPANDHEAT.csv"))
  TEMPHW$HW1 <- 1
  TEMPHW[SHW == 0]$HW1 <- 0
  TEMPHW$HW1 <- factor(TEMPHW$HW1)

  TEMPHW$HW2 <- 1
  TEMPHW[THW == 0]$HW2 <- 0
  TEMPHW$HW2 <- factor(TEMPHW$HW2)

  

## Based on larval growth by temp from Laurel et al. 2016 getting growth L0 index
  growth_L0<-function(data = data,T = mean(TEMPHW[YR %in% 1982:2012]$JUNE_TEMP)){
    index = exp(0.2494 
                + 0.3216 * (T + data$TEMP) 
                - 0.0069 * (T + data$TEMP) ^ 2 
                - 0.0004 * (T + data$TEMP) ^ 3) / exp(0.2494 + 0.3216 * (T) - 0.0069 * (T) ^ 2 - 0.0004 * (T) ^ 3)
    return(index)
  }

  tempHW <- data.table(YR = c(1977,1978),
                       JUNE_TEMP = c(4.58, 4.58),
                       FEB_TEMP = c(3.93, 3.93),
                       TEMP = c(0, 0),
                       TEMP2 = c(0,0),
                       TEMP3 = c(0, 0),
                       THW = c(0, 0),
                       WHW = c(0, 0),
                       SHW = c(0, 0),
                       HW1 = c(0, 0),
                       HW2 = c(0, 0))
  TEMPHW = rbind(tempHW, TEMPHW)
  x1 <- data.table(Yr = TEMPHW$YR, Variable = 1, Value = TEMPHW$TEMP)
  x2 <- data.table(Yr = TEMPHW$YR, Variable = 2, Value = TEMPHW$THW)
  x3 <- data.table(Yr = TEMPHW$YR, Variable = 3, Value = TEMPHW$SHW^(1/3))
  x4 <- data.table(Yr = TEMPHW$YR, Variable = 4, Value = growth_L0(TEMPHW))
  x5 <- data.table(Yr = TEMPHW$YR, Variable = 5, Value = 0.65 / (1 + exp(-0.005 * (TEMPHW$THW - 400))))
  x6 <- data.table(Yr = c(2015:endyr), Variable = 6, Value = 1.0)                                   ## mortality block



  x1.1 <- data.table(Yr=c((endyr + 1):(endyr + 14)), Variable = 1, Value = mean(x1[Yr%in%(endyr - 10):endyr]$Value))
  x2.1 <- data.table(Yr=c((endyr + 1):(endyr + 14)), Variable = 2, Value = mean(x2[Yr%in%(endyr - 10):endyr]$Value ^ (1 / 3)) ^ 3)
  x3.1 <- data.table(Yr=c((endyr + 1):(endyr + 14)), Variable = 3, Value = mean(x3[Yr%in%(endyr - 10):endyr]$Value))
  x4.1 <- data.table(Yr=c((endyr + 1):(endyr + 14)), Variable = 4, Value = mean(x4[Yr%in%(endyr - 10):endyr]$Value))
  x5.1 <- data.table(Yr=c((endyr + 1):(endyr + 14)), Variable = 5, Value = mean(x5[Yr%in%(endyr - 10):endyr]$Value))


  #envdata<-data.frame(rbind(x1, x1.1, x2, x2.1, x3, x3.1, x4, x4.1, x5, x5.1, x6)) # originial
  envdata<-data.frame(x1) # whittling down to LL q link

    ssdat$envdat <- envdata
    ssdat
}
