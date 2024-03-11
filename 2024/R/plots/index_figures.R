
# Function to plot PCod indices
plot_indices <- function(styr,
                         endyr,
                         ss_datname){

  # Get the SS data file indices
  SS_DAT <- SS_readdat_3.30(here::here(endyr, "output", ss_datname))
  
  data.table(SS_DAT$CPUE) %>% 
    mutate(obs = as.numeric(obs),
           sd = as.numeric(se_log) * obs) %>% 
    mutate(name = case_when(index == 4 ~ "AFSC trawl survey numbers (1000s)",
                            index == 5 ~ "AFSC longline survey RPNs",
                            index == -6 ~ "IPHC longline survey RPNs",
                            index == -7 ~ "ADF&G trawl survey density")) %>% 
    mutate(name = factor(name, levels = c("AFSC trawl survey numbers (1000s)", 
                                          "AFSC longline survey RPNs", 
                                          "IPHC longline survey RPNs", 
                                          "ADF&G trawl survey density"))) -> CPUE
  
  # Fitted indices (AFSC trawl & LL)
  fitted <- ggplot(data = CPUE[index %in% c(4, 5)], 
                   aes(x = as.numeric(year), y = obs)) +
    geom_point() +
    theme_bw(base_size = 18) +
    geom_ribbon(aes(ymin = obs - 1.96 * sd, ymax = obs + 1.96 * sd, fill = name), alpha = 0.2, show.legend = FALSE) +
    geom_line() +
    geom_errorbar(aes(ymin = obs - 1.96 * sd, ymax = obs + 1.96 * sd), width = 0.25) +
    xlab("Year") +
    ylab(element_blank()) +
    theme(axis.text.x = element_text(vjust = 0.5, angle = 90),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          strip.background = element_blank(),
          strip.placement = "outside") +
    scale_x_continuous(limits = c(styr - 1, endyr + 1), breaks = seq(styr, endyr, by = 1)) + 
    facet_wrap(~name, 
               nrow = 2, 
               scales = "free_y",
               strip.position = "left") +
    scale_fill_manual(values=c("turquoise", "blue"))

  # Not fitted indices (IPHC LL & ADFG trawl)
  
  not_fitted <- ggplot(data = CPUE[index %in% c(-6, -7)], 
                       aes(x = as.numeric(year), y = obs)) +
    geom_point() +
    theme_bw(base_size = 18) +
    geom_ribbon(aes(ymin = obs - 1.96 * sd, ymax = obs + 1.96 * sd, fill = name), alpha = 0.2, show.legend = FALSE) +
    geom_line() +
    geom_errorbar(aes(ymin = obs - 1.96 * sd, ymax = obs + 1.96 * sd), width = 0.25) +
    xlab("Year") +
    ylab(element_blank()) +
    theme(axis.text.x = element_text(vjust = 0.5, angle = 90),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          strip.background = element_blank(),
          strip.placement = "outside") +
    scale_x_continuous(limits = c(styr - 2, endyr + 1), breaks = seq(styr - 2, endyr, by = 1)) + 
    facet_wrap(~name, 
               nrow = 2, 
               scales = "free_y",
               strip.position = "left") +
    scale_fill_manual(values=c("red", "orange"))
  
  # Age - 0 Beach seine index
  age0 <- ggplot(data = CPUE[index == -9 & year >= 2006], 
                 aes(x = as.numeric(year), y = obs)) +
    geom_line() +
    geom_point() +
    theme_bw(base_size = 18) +
    geom_ribbon(aes(ymin = obs - 1.96 * sd, ymax = obs + 1.96 * sd), alpha = 0.2, fill = "dark green") +
    geom_errorbar(aes(ymin = obs - 1.96 * sd, ymax = obs + 1.96 * sd), width = 0.25) +
    xlab("Year") +
    ylab("Age-0 Beach Seine numbers/haul") +
    theme(axis.text.x = element_text(vjust = 0.5, angle = 90),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank()) +
    scale_x_continuous(limits = c(2005, endyr + 1), breaks = seq(2006, endyr, by = 1))
  
  # Larval habitat index
  larv_hab <- ggplot(data = CPUE[index == 8], 
                     aes(x = as.numeric(year), y = obs)) +
    geom_line() +
    geom_point() +
    theme_bw(base_size = 18) +
    geom_ribbon(aes(ymin = obs - 1.96 * sd, ymax = obs + 1.96 * sd), alpha = 0.2, fill = "red") +
    geom_errorbar(aes(ymin = obs - 1.96 * sd, ymax = obs + 1.96 * sd), width = 0.25) +
    xlab("Year") +
    ylab("Larval habitat index") +
    theme(axis.text.x = element_text(vjust = 0.5, angle = 90),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank()) +
    scale_x_continuous(limits = c(1978, endyr + 1), breaks = seq(1978, endyr + 1, by = 1))
  
  
  plots <- list(fitted, not_fitted, age0, larv_hab)
  
  plots
}

