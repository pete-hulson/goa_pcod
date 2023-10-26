## Function to plot cumulative catch for species and subarea

plot_env_ind<-function(env_data){

  env_data %>% 
    filter(Month == 6) %>% 
    rename('0-20 cm' = '0_20',
           '40-60 cm' = '40_60') %>% 
    select(-'20_40', -'60_80', -'80plus', -Month) %>% 
    pivot_longer(c('0-20 cm'), names_to = 'size', values_to = 'Temp') -> plot_ind
  
  mu_20 <- mean(subset(plot_ind$Temp, plot_ind$size == "0-20 cm"))

  # Plot temperatures
  temps <- ggplot(data = plot_ind, 
         aes(x = Year, y = Temp, color = factor(size))) + 
    geom_point(size = 3, color = "darkblue") + 
    geom_path(aes(group = size), color = "darkblue") +
    geom_hline(yintercept = mu_20, linetype = "dashed", color = "darkblue") +
    # scale_color_nmfs("seagrass", name = "") +
    theme_bw(base_size = 21) +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.text.x = element_blank(),
          legend.position = c(0.3, 0.85)) +
    labs(y = "Bottom Temperature (C)", x = NULL)
  
  
  # Plot anomalies
  env_data %>% 
    filter(Month == 6) %>%
    select(Year, '0_20') %>% 
    rename(l20 = '0_20') %>% 
    mutate(sub = case_when(Year <= 2012 & Year >= 1982 ~ l20),
           reg_mu = mean(sub, na.rm = TRUE),
           anomaly = l20 - reg_mu) %>% 
    select(Year, anomaly) %>% 
    mutate(sign = case_when(anomaly > 0 ~ "Pos",
                            anomaly < 0 ~ "Neg")) -> plot_anom
  
  # Plot anomalies
  anoms <- ggplot(data = plot_anom, 
         aes(x = Year, y = anomaly, fill = sign)) + 
    geom_bar(stat="identity", width=0.777) + 
    geom_hline(yintercept = 0) +
    scale_fill_nmfs("seagrass", name = "") +
    theme_bw(base_size = 18) +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(vjust = 0.5, angle = 90),
          legend.position = 'none') +
    labs(y = "Temperature anomaly", x = "Year")
  
  
  env_plots <- list (temps, anoms)
  
  return(env_plots)
  
}


