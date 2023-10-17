# Function to perform Leave-One-Out analysis
# Originally written by Steve Barbeaux, altered by Pete Hulson in 2022 and 2023



SS_doLOO <- function (Model_name = NULL,
                      newsubdir = "LeaveOneOut", 
                      years = 0:-10,
                      datafilename = NULL,
                      CYR = NULL){
  
  # Set up LOO necessities
  if (!file.exists(here::here(CYR, "mgmt", Model_name, newsubdir))) 
    dir.create(here::here(CYR, "mgmt", Model_name, newsubdir))
  
  subdirnames <- paste0("LOO", years)
  
  datafile <- r4ss::SS_readdat(file = here::here(CYR, "mgmt", Model_name, datafilename))

  # Loop thru LOO years
  
  for (iyr in 1:length(years)) {
    
    # Write SS files
    r4ss::copy_SS_inputs(dir.old = here::here(CYR, "mgmt", Model_name), 
                         dir.new = here::here(CYR, "mgmt", Model_name, newsubdir, subdirnames[iyr]),
                         copy_par = TRUE,
                         copy_exe = TRUE,
                         overwrite = TRUE)
    
    # Change up data file for LOO
    CPUE <- data.table(datafile$CPUE)
    agecomp <- data.table(datafile$agecomp)
    lencomp <- data.table(datafile$lencomp)
    yr <- CYR + years[iyr]
    
    CPUE[year == yr & index > 0]$index <- CPUE[year == yr & index > 0]$index * -1
    agecomp[Yr == yr & FltSvy > 0]$FltSvy <- agecomp[Yr == yr & FltSvy > 0]$FltSvy * -1
    lencomp[Yr == yr & FltSvy > 0]$FltSvy <- lencomp[Yr == yr & FltSvy > 0]$FltSvy * -1
    
    datafile2 <- datafile
    datafile2$CPUE <- data.frame(CPUE)
    datafile2$agecomp <- data.frame(agecomp)
    datafile2$lencomp <- data.frame(lencomp)
    
    # Write out data script
    r4ss::SS_writedat_3.30(datafile2,
                           here::here(CYR, "mgmt", Model_name, newsubdir, subdirnames[iyr], datafilename),
                           overwrite = TRUE)
    
    # Run model
    r4ss::run(dir = here::here(CYR, "mgmt", Model_name, newsubdir, subdirnames[iyr]),
              skipfinished = FALSE,
              show_in_console = FALSE)
    
    
    
    # End loop
  }
 
  
  ### Compile output
  LOO_mods <- r4ss::SSgetoutput(dirvec = here::here(CYR, "mgmt", Model_name, newsubdir, subdirnames))
  
  Nat_M <- array()
  Q <- array()
  SSB_UN <- array()
  annF_Btgt <- array()
  Nat_M_SD <- array()
  Q_SD <- array()
  SSB_UN_SD <- array()
  annF_Btgt_SD <- array()
  SSBfore <- array()
  SSBfore_SD <- array()
  ABCfore <- array()
  ABCfore_SD <- array()

  for(i in 1:length(years)){
    x <- data.table(LOO_mods[[i]]$parameters)
    y <- data.table(LOO_mods[[i]]$derived_quants)
    annF_Btgt[i] <- y[Label == 'annF_Btgt']$Value
    annF_Btgt_SD[i] <- y[Label == 'annF_Btgt']$StdDev
    Nat_M[i] <- x[Label == "NatM_uniform_Fem_GP_1"]$Value
    Nat_M_SD[i] <- x[Label == "NatM_uniform_Fem_GP_1"]$Parm_StDev
    Q[i] <- x[Label %in% "LnQ_base_Srv(4)"]$Value
    Q_SD[i] <- x[Label %in% "LnQ_base_Srv(4)"]$Parm_StDev
    SSB_UN[i] <- y[Label == 'SSB_unfished']$Value
    SSB_UN_SD[i] <- y[Label == 'SSB_unfished']$StdDev
    SSBfore[i] <- y[Label == paste0('SSB_', CYR + 1)]$Value
    SSBfore_SD[i] <- y[Label == paste0('SSB_', CYR + 1)]$StdDev
    ABCfore[i] <- y[Label == paste0('ForeCatch_', CYR + 1)]$Value
    ABCfore_SD[i] <- y[Label == paste0('ForeCatch_', CYR + 1)]$StdDev
  }

  mods0 <- r4ss::SSgetoutput(dirvec = here::here(CYR, "mgmt", Model_name))

  x0 <- data.table(mods0[[1]]$parameters)
  y0 <- data.table(mods0[[1]]$derived_quants)
  annF_Btgt0 <- y0[Label == 'annF_Btgt']$Value
  annF_Btgt_SD0 <- y0[Label == 'annF_Btgt']$StdDev
  Nat_M0 <- x0[Label == "NatM_uniform_Fem_GP_1"]$Value
  Nat_M_SD0 <- x0[Label == "NatM_uniform_Fem_GP_1"]$Parm_StDev
  Q0 <- x0[Label %in% "LnQ_base_Srv(4)"]$Value
  Q_SD0 <- x0[Label %in% "LnQ_base_Srv(4)"]$Parm_StDev
  SSB_UN0 <- y0[Label == 'SSB_unfished']$Value
  SSB_UN_SD0 <- y0[Label == 'SSB_unfished']$StdDev
  SSBfore0 <- y0[Label == paste0('SSB_', CYR + 1)]$Value
  SSBfore_SD0 <- y0[Label == paste0('SSB_', CYR + 1)]$StdDev
  ABCfore0 <- y0[Label == paste0('ForeCatch_', CYR + 1)]$Value
  ABCfore_SD0 <- y0[Label == paste0('ForeCatch_', CYR + 1)]$StdDev

  x0 <- data.table(LOO = 0,
                   Nat_M = Nat_M0, 
                   Nat_M_SD = Nat_M_SD0,
                   annF_Btgt = annF_Btgt0, 
                   annF_Btgt_SD = annF_Btgt_SD0,
                   Q = exp(Q0), 
                   Q_SD = Q_SD0, 
                   SSB_UN = SSB_UN0,
                   SSB_UN_SD = SSB_UN_SD0,
                   SSBfore = SSBfore0,
                   SSBfore_SD = SSBfore_SD0,
                   ABCfore = ABCfore0,
                   ABCfore_SD = ABCfore_SD0)
  
  x1<-data.table(LOO = c(1:length(years)),
                 Nat_M = Nat_M, 
                 Nat_M_SD = Nat_M_SD, 
                 annF_Btgt = annF_Btgt, 
                 annF_Btgt_SD = annF_Btgt_SD,
                 Q = exp(Q), 
                 Q_SD = Q_SD, 
                 SSB_UN = SSB_UN,
                 SSB_UN_SD = SSB_UN_SD,
                 SSBfore = SSBfore,
                 SSBfore_SD = SSBfore_SD,
                 ABCfore = ABCfore,
                 ABCfore_SD = ABCfore_SD)
  
  x <- rbind(x0, x1)
  x2 <- data.table(melt(x, "LOO"))
  x3 <- x2[!variable %like% "_SD"]
  x4 <- x2[variable %like% "_SD"]
  x3$SD <- x4$value
  x3$Year <- CYR - x3$LOO
  
  # Plot LOO analysis
  d <- ggplot(x3[LOO != 0],
              aes(x = Year,y = value)) +
    geom_errorbar(aes(ymin = value - 1.96 * SD, ymax = value + 1.96 * SD), width = 0.25) +
    geom_point(size = 3) +
    geom_hline(data = x3[LOO == 0],
               aes(yintercept = value), size = 1.25, linetype = 2, color = "red") +
    theme_bw(base_size = 16) +
    labs(x = 'Leave one out year', y = 'Parameter value') +
    facet_wrap( ~ variable, scales = "free_y", ncol = 2)

  # Table of LOO analysis
  x3 <- data.table(Nat_M = x$Nat_M,
                  annF_Btgt = x$annF_Btgt,
                  Q = x$Q,
                  SSB_UN = x$SSB_UN,
                  SSBfore = x$SSBfore,
                  ABCfore = x$ABCfore )
  x4 <- x3[1, ]
  x4 <- data.table(Nat_M = rep(x4$Nat_M, length(years) + 1),
                   annF_Btgt = rep(x4$annF_Btgt, length(years) + 1),
                   Q = rep(x4$Q, length(years) + 1),
                   SSB_UN = rep(x4$SSB_UN, length(years) + 1),
                   SSBfore = rep(x4$SSBfore, length(years) + 1),
                   ABCfore = rep(x4$ABCfore, length(years) + 1))
  x4 <- x3[2:(1 + length(years)), ] - x4[2:(1 + length(years)), ]
  x4$LOO <- c(1:length(years))

  x4 <- melt(x4, 'LOO')
  
  x4 %>% 
    dplyr::group_by(variable) %>% 
    dplyr::summarise(bias = mean(value)) %>% 
    dplyr::mutate(p = bias / t(x3[1, ])) -> BIAS

  output <- list(BIAS, d)
  
  output
  
}

# L-O-O for most recent year of data
SS_doLOO_cyr <- function (Model_name = NULL,
                          newsubdir = "LeaveOneOut",
                          CYR = NULL){
  
  # Set up sub directory names for each individual data
  subdirnames <- c('Fish_CAAL_LL', 
                   'Fish_CAAL_POT',
                   'Fish_CAAL_TWL', 
                   'Fish_LC_LL', 
                   'Fish_LC_POT',
                   'Fish_LC_TWL',
                   'LLsurv_Indx',
                   'LLsurv_LC',
                   'BTsurv_Indx',
                   'BTsurv_LC')

  # Loop thru added data
  
  for (i in 1:length(subdirnames)) {
    
    # Run model
    r4ss::run(dir = here::here(CYR, "mgmt", Model_name, newsubdir, 'LOO-2023', subdirnames[i]),
              skipfinished = FALSE,
              show_in_console = FALSE)
    
    # End loop
  }
  
  
  ### Compile output
  LOO_mods <- r4ss::SSgetoutput(dirvec = here::here(CYR, "mgmt", Model_name, newsubdir, 'LOO-2023', subdirnames))
  
  Nat_M <- array()
  Q <- array()
  SSB_UN <- array()
  annF_Btgt <- array()
  Nat_M_SD <- array()
  Q_SD <- array()
  SSB_UN_SD <- array()
  annF_Btgt_SD <- array()
  SSBfore <- array()
  SSBfore_SD <- array()
  ABCfore <- array()
  ABCfore_SD <- array()
  
  for(i in 1:length(subdirnames)){
    x <- data.table(LOO_mods[[i]]$parameters)
    y <- data.table(LOO_mods[[i]]$derived_quants)
    annF_Btgt[i] <- y[Label == 'annF_Btgt']$Value
    annF_Btgt_SD[i] <- y[Label == 'annF_Btgt']$StdDev
    Nat_M[i] <- x[Label == "NatM_uniform_Fem_GP_1"]$Value
    Nat_M_SD[i] <- x[Label == "NatM_uniform_Fem_GP_1"]$Parm_StDev
    Q[i] <- x[Label %in% "LnQ_base_Srv(4)"]$Value
    Q_SD[i] <- x[Label %in% "LnQ_base_Srv(4)"]$Parm_StDev
    SSB_UN[i] <- y[Label == 'SSB_unfished']$Value
    SSB_UN_SD[i] <- y[Label == 'SSB_unfished']$StdDev
    SSBfore[i] <- y[Label == paste0('SSB_', CYR + 1)]$Value
    SSBfore_SD[i] <- y[Label == paste0('SSB_', CYR + 1)]$StdDev
    ABCfore[i] <- y[Label == paste0('ForeCatch_', CYR + 1)]$Value
    ABCfore_SD[i] <- y[Label == paste0('ForeCatch_', CYR + 1)]$StdDev
  }
  
  mods0 <- r4ss::SSgetoutput(dirvec = here::here(CYR, "mgmt", Model_name))
  
  x0 <- data.table(mods0[[1]]$parameters)
  y0 <- data.table(mods0[[1]]$derived_quants)
  annF_Btgt0 <- y0[Label == 'annF_Btgt']$Value
  annF_Btgt_SD0 <- y0[Label == 'annF_Btgt']$StdDev
  Nat_M0 <- x0[Label == "NatM_uniform_Fem_GP_1"]$Value
  Nat_M_SD0 <- x0[Label == "NatM_uniform_Fem_GP_1"]$Parm_StDev
  Q0 <- x0[Label %in% "LnQ_base_Srv(4)"]$Value
  Q_SD0 <- x0[Label %in% "LnQ_base_Srv(4)"]$Parm_StDev
  SSB_UN0 <- y0[Label == 'SSB_unfished']$Value
  SSB_UN_SD0 <- y0[Label == 'SSB_unfished']$StdDev
  SSBfore0 <- y0[Label == paste0('SSB_', CYR + 1)]$Value
  SSBfore_SD0 <- y0[Label == paste0('SSB_', CYR + 1)]$StdDev
  ABCfore0 <- y0[Label == paste0('ForeCatch_', CYR + 1)]$Value
  ABCfore_SD0 <- y0[Label == paste0('ForeCatch_', CYR + 1)]$StdDev
  
  x0 <- data.table(LOO = 0,
                   Nat_M = Nat_M0, 
                   Nat_M_SD = Nat_M_SD0,
                   annF_Btgt = annF_Btgt0, 
                   annF_Btgt_SD = annF_Btgt_SD0,
                   Q = exp(Q0), 
                   Q_SD = Q_SD0, 
                   SSB_UN = SSB_UN0,
                   SSB_UN_SD = SSB_UN_SD0,
                   SSBfore = SSBfore0,
                   SSBfore_SD = SSBfore_SD0,
                   ABCfore = ABCfore0,
                   ABCfore_SD = ABCfore_SD0)
  
  x1<-data.table(LOO = c(1:length(subdirnames)),
                 Nat_M = Nat_M, 
                 Nat_M_SD = Nat_M_SD, 
                 annF_Btgt = annF_Btgt, 
                 annF_Btgt_SD = annF_Btgt_SD,
                 Q = exp(Q), 
                 Q_SD = Q_SD, 
                 SSB_UN = SSB_UN,
                 SSB_UN_SD = SSB_UN_SD,
                 SSBfore = SSBfore,
                 SSBfore_SD = SSBfore_SD,
                 ABCfore = ABCfore,
                 ABCfore_SD = ABCfore_SD)
  
  x <- rbind(x0, x1)
  x2 <- data.table(melt(x, "LOO"))
  x3 <- x2[!variable %like% "_SD"]
  x4 <- x2[variable %like% "_SD"]
  x3$SD <- x4$value
  x3$Data <- rep(c('Base', subdirnames), times = 6)
  
  # Plot LOO analysis
  d <- ggplot(x3[LOO != 0],
              aes(x = Data, y = value)) +
    geom_errorbar(aes(ymin = value - 1.96 * SD, ymax = value + 1.96 * SD), width = 0.25) +
    geom_point(size = 3) +
    geom_hline(data = x3[LOO == 0],
               aes(yintercept = value), size = 1.25, linetype = 2, color = "red") +
    theme_bw(base_size = 16) +
    labs(x = 'Leave one out data', y = 'Parameter value') +
    theme(axis.text.x = element_text(vjust = 0.5, angle = 90)) +
    facet_wrap( ~ variable, scales = "free_y", ncol = 2)
  
  output <- list(d)
  
  return(output)
  
}

