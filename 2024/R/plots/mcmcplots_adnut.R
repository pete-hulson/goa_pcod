# Function to plot MCMC results
# Adapted by Pete Hulson in 2022 from Steve Barbeaux original code

plot_mcmc <- function(mcmc_dir,
                      CYR){
  
  # # Set up plot folder
  # if (!file.exists(here::here("Stock_Synthesis_files", Model_name, "MCMC", "MCMC-plots"))) 
  #   dir.create(here::here("Stock_Synthesis_files", Model_name, "MCMC", "MCMC-plots"))
  # 
  # Get plot data together
  MCMC <- data.frame(read.table(paste0(mcmc_dir, "/derived_posteriors.sso"), header = T))
  MCMC1 <- data.frame(read.table(paste0(mcmc_dir, "/derived_posteriors.sso"), header = T))
  MCMC1P <- data.frame(read.table(paste0(mcmc_dir, "/posteriors.sso"), header = T))
  
  SSB3 <- vector("list", length = 2)
  SSB3[[1]] <- MCMC[ ,5:(20 + CYR - 1977)]
  SSB3[[2]] <- MCMC1[ ,5:(20 + CYR - 1977)]
  
  Yrs1 = as.numeric(do.call(rbind, str_split(names(SSB3[[1]]), "_"))[ ,2])
  SSB4.1 <- data.frame(Yrs = rep(Yrs1, each = nrow(SSB3[[1]])), SSB = 0)
  SSB4.2 <- data.frame(Yrs = rep(Yrs1, each = nrow(SSB3[[2]])), SSB = 0)
  SSB4.1$SSB[1:nrow(SSB3[[1]])] <- SSB3[[1]][1:nrow(SSB3[[1]]), 1] / 2
  SSB4.2$SSB[1:nrow(SSB3[[2]])] <- SSB3[[2]][1:nrow(SSB3[[2]]), 1] / 2
  
  for(i in 1:(CYR - 1977 + 2)){
    SSB4.1$SSB[((i * nrow(SSB3[[1]])) + 1):((i + 1) * nrow(SSB3[[1]]))] <- SSB3[[1]][1:nrow(SSB3[[1]]), i + 1] / 2
    SSB4.2$SSB[((i * nrow(SSB3[[2]])) + 1):((i + 1) * nrow(SSB3[[2]]))] <- SSB3[[2]][1:nrow(SSB3[[2]]), i + 1] / 2
  }

  SSB4.1 <- data.table(SSB4.1)
  SSB4.2 <- data.table(SSB4.2)
  MEANS2.1 = SSB4.1[ ,list(MSSB = median(SSB)), by = "Yrs"]
  MEANS2.2 = SSB4.2[ ,list(MSSB = median(SSB)), by = "Yrs"]
  
  SSB <- MCMC1[ ,5:(5 + (15 + CYR - 1977))]
  REC <- MCMC1[ ,(8 + (15 + CYR - 1977)):(8 + (2 * (15 + CYR - 1977)))]
  
  Yrs = as.numeric(do.call(rbind, str_split(names(SSB), "_"))[ ,2])
  
  SSB1 <- data.frame(Yrs = rep(Yrs, each = nrow(SSB)), SSB = 0, REC = 0)
  SSB1$SSB[1:nrow(SSB)] <- SSB[1:nrow(SSB), 1] / 2
  SSB1$REC[1:nrow(REC)] <- REC[1:nrow(REC), 1]
  
  for(i in 1:(15 + (CYR - 1977))){
    SSB1$SSB[((i * nrow(SSB)) + 1):((i + 1) * nrow(SSB))] <- SSB[1:nrow(SSB), i + 1] / 2
    SSB1$REC[((i * nrow(REC)) + 1):((i + 1) * nrow(REC))] <- REC[1:nrow(SSB), i + 1]
  }

  SSB1 <- data.table(SSB1)
  MEANS = SSB1[ ,list(MREC = median(REC), MSSB = median(SSB)), by = "Yrs"]
  
  
  SSB20 <- quantile(MCMC[ ,(4 * (CYR - 1977 + 16) + (CYR -1978 + 16) + 7)], 0.5) / 2 * 0.2 / 10^5
  SSB17.5 <- quantile(MCMC[ ,(4 * (CYR - 1977 + 16) + (CYR -1978 + 16) + 7)], 0.5)/ 2 * 0.175 / 10^5
  
  Ribbon = SSB1[ ,list(SSB2 = quantile(MCMC[ ,(4 * (CYR - 1977 + 16) + (CYR -1978 + 16) + 7)], 0.025) / 2 * 0.2 / 10^5, 
                       SSB97 = quantile(MCMC[ ,(4 * (CYR - 1977 + 16) + (CYR -1978 + 16) + 7)], 0.975) / 2 * 0.2 / 10^5)]
  Ribbon <- data.table(Yrs = Yrs, SSB2 = Ribbon$SSB2, SSB97 = Ribbon$SSB97)
  
  
  ### Plot results
  
  # SSB
  SSB <- ggplot() +
    geom_ribbon(data = Ribbon,
                aes(x = Yrs, ymin = SSB2, ymax = SSB97),
                fill = "orange",
                alpha = 0.2) +
    geom_violin(data = SSB1,
                aes(x = Yrs, y = SSB / 10^5, group = Yrs),
                fill = "gray75", alpha=0.5) +
    ylim(0, max(SSB1$SSB / 10^5)) +
    geom_line(data = MEANS, aes(y = MSSB / 10^5, x = Yrs, group = ""), size = 1) +
    geom_point(data = MEANS, aes(y = MSSB / 10^5, x = Yrs, group = "")) +
    theme_bw(base_size = 21) +
    scale_x_continuous(limits = c(1976.5, (CYR + 15 + 0.5)), breaks = seq(1977, (CYR + 15), by = 2)) +
    theme(axis.text.x = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank()) +
    labs(y = expression(paste("Female spawning biomass ( ", 10^5, " t)")), x = NULL) +
    geom_hline(yintercept = SSB20, linetype = 3, size = 1.1) +
    geom_hline(yintercept = SSB17.5, color = "red", linetype = 2, size = 1.1)

  # ggsave(here::here("Stock_Synthesis_files", Model_name, "MCMC", "MCMC-plots", "SSB.png"),
  #        plot = SSB)
  # 
  # Recruitment
  Rec <- ggplot() +
    geom_violin(data = SSB1, aes(x = Yrs, y = REC / 10^6, group = Yrs), fill = "gray50") +
    ylim(0, max(SSB1$REC / 10^6)) +
    geom_line(data = MEANS, aes(y = MREC / 10^6, x = Yrs, group = "")) +
    geom_point(data = MEANS, aes(y = MREC / 10^6, x = Yrs, group = "")) +
    theme_bw(base_size = 21) +
    scale_x_continuous(limits = c(1976.5, (CYR + 15 + 0.5)), breaks = seq(1977, (CYR + 15), by = 2)) +
    theme(axis.text.x = element_text(vjust = 0.5, angle = 90),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank()) +
    labs(y = "Age-0 abundance (billions)", x = "Year")

  # ggsave(here::here("Stock_Synthesis_files", Model_name, "MCMC", "MCMC-plots", "Rec.png"),
  #        plot = Rec)

  # Get result quantiles
  MCMC_quant <- matrix(ncol = 4, nrow = ncol(MCMC) - 1)
  for(i in 1:(ncol(MCMC) - 1)){
    MCMC_quant[i, 1] <- names(MCMC)[i + 1]
    MCMC_quant[i, c(2:4)] <- as.numeric(quantile(MCMC[ ,i + 1],c(0.0275, 0.5, 0.975)))
  }
  MCMC_quant <- data.table(MCMC_quant)
  names(MCMC_quant) <- c("Label", "P0275", "P50", "P975")
  

  MCMCP_quant <- matrix(ncol = 4, nrow = ncol(MCMC1P) - 1)
  for(i in 1:(ncol(MCMC1P) - 1)){
    MCMCP_quant[i, 1] <- names(MCMC1P)[i + 1]
    MCMCP_quant[i, c(2:4)] <- as.numeric(quantile(MCMC1P[ ,i + 1],c(0.0275, 0.5, 0.975)))
  }
  MCMCP_quant <- data.table(MCMCP_quant)
  names(MCMCP_quant) <- c("Label", "P0275", "P50", "P975")
  

  # Plot bjective function trace
  obj_trace <- ggplot(MCMC,
               aes(y = Objective_function, x = Iter)) +
    geom_line() +
    theme_bw(base_size = 20) +
    labs(y = 'Objective function', x = 'Iteration')
  
  # ggsave(here::here("Stock_Synthesis_files", Model_name, "MCMC", "MCMC-plots", "obj_trace.png"),
  #        plot = obj_trace)
  # 

  # Plot objective function density
  obj_dens <- ggplot(MCMC) +
    geom_density(aes(Objective_function)) +
    theme_bw(base_size = 20) +
    labs(x = 'Objective function')

  # ggsave(here::here("Stock_Synthesis_files", Model_name, "MCMC", "MCMC-plots", "obj_dens.png"),
  #        plot = obj_dens)
  # 
  # Plot acf
  conf.level <- 0.95
  ciline <- qnorm((1 - conf.level) / 2) / sqrt(length(MCMC$Objective_function))
  bacf <- acf(MCMC$Objective_function, plot = FALSE)
  bacfdf <- with(bacf, data.frame(lag, acf))
  
  acf <- ggplot(data = bacfdf, mapping = aes(x = lag, y = acf)) +
    geom_bar(stat = "identity", position = "identity", width = 0.1) +
    theme_bw(base_size = 20) +
    geom_hline(yintercept = 0.05, color = 'blue', linetype = 2)
  
  # Plot SSB ratio
  SSBratYr1 <- ggplot(MCMC) +
    geom_density(aes(SSB_2023 / SSB_unfished)) +
    theme_bw(base_size = 20) +
    labs(x = '2023 female spawning biomass/unfished female spawning biomass') +
    xlim(0.10, 0.5) +
    geom_vline(xintercept = 0.2, color = "orange", linetype = 3, size = 1.0) +
    geom_vline(xintercept = 0.175, color = "red", linetype = 2, size = 1.0) +
    geom_vline(xintercept = sort(MCMC$SSB_2023 / MCMC$SSB_unfished)[245], color = "Blue", size = 2)
  
  # ggsave(here::here("Stock_Synthesis_files", Model_name, "MCMC", "MCMC-plots", "SSBratYr1.png"),
  #        plot = SSBratYr1)
  # 
  SSBratYr2 <- ggplot(MCMC) +
    geom_density(aes(SSB_2024 / SSB_unfished)) +
    theme_bw(base_size = 20) +
    labs(x = '2024 female spawning biomass/unfished female spawning biomass') +
    xlim(0.10, 0.5) +
    geom_vline(xintercept = 0.2, color = "orange", linetype = 3, size = 1.0) +
    geom_vline(xintercept = 0.175, color = "red", linetype = 2, size = 1.0) +
    geom_vline(xintercept = sort(MCMC$SSB_2024 / MCMC$SSB_unfished)[245], color = "Blue", size = 2)
  
  # ggsave(here::here("Stock_Synthesis_files", Model_name, "MCMC", "MCMC-plots", "SSBratYr2.png"),
  #        plot = SSBratYr2)
  # 
  output <- list(SSB, Rec, obj_trace, obj_dens, acf, SSBratYr1, SSBratYr2)
  
  output

}
