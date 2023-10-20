## Plot fish condition

plot_fish_cond <- function(CYR = 2022,
                           data_query = FALSE){

  # Length - weight residual fcn
  lw.resids <- function(length,
                        weight,
                        outlier.rm = FALSE){
    
    loglength <- log(length)
    logwt <- log(weight)
    lw.res <- lm(logwt ~ loglength)
    #Assessing Outliers using Bonferoni Outlier Test
    #Identify if there are any outliers in your data that exceed cutoff = 0.05 (default)
    if(outlier.rm == TRUE){
      #Produce a bonferoni value for each point in your data
      test1 <- outlierTest(lw.res, n.max = Inf, cutoff = Inf, order = FALSE)$bonf.p 
      remove <- which(test1 < .7,)
      print("Outlier rows removed")
      print(unname(remove))
      logwt[remove] <- NA
      lw.res <- lm(logwt ~ loglength, na.action = na.exclude)
      lw.res <- residuals(lw.res) 
    }
    
    if(outlier.rm == FALSE){lw.res <- residuals(lw.res)}
    return(lw.res)}
  
  #' A function to weight length-weight residuals by catch
  #'
  #' This function weights length-weight residuals by a catch column. This
  #' catch can be CPUE from the tow where the fish was caught (most common) or
  #' stratum CPUE or biomass. 
  #' @param year Year of sample must be the same length as the residuals
  #' @param residual Residual that will be weighted by catch
  #' @param catch Catch for weighting residual (default = 1) must be the same length as residuals
  #' @keywords length, weight, groundfish condition
  #' @export
  #' @examples
  #' weighted_resids()
  
  weighted_resids <- function(year, residuals, catch = 1){
    wtlw.res <- residuals
    if(length(catch) == 1){catch <- rep(1, length(residuals))}
    years1 <- unique(year)
    for(i in 1:length(years1)){
      d0 <- which(year == years1[i])
      d1 <- residuals[d0]
      d2 <- catch[d0]
      var1 <- d1 * d2
      var2 <- sum(d2)
      var3 <- var1 / var2 * length(d2)
      wtlw.res[d0] <- var3}
    return(wtlw.res)}
  
  if(data_query == TRUE){
    ## data pull for fishery data from AFSC database
    test <- paste0("SELECT
    obsint.debriefed_age.nmfs_area,
    obsint.debriefed_age.gear,
    obsint.debriefed_age.species,
    obsint.debriefed_age.length,
    obsint.debriefed_age.weight,
    obsint.debriefed_age.year,
    TO_CHAR(obsint.debriefed_age.haul_offload_date, 'mm') AS month,
    obsint.debriefed_spcomp.extrapolated_weight AS catch
    FROM obsint.debriefed_age
    INNER JOIN obsint.debriefed_spcomp 
    ON obsint.debriefed_age.haul_join = obsint.debriefed_spcomp.haul_join
    WHERE obsint.debriefed_age.nmfs_area > 600
    AND obsint.debriefed_age.nmfs_area < 640
    AND obsint.debriefed_age.species = 202
    AND obsint.debriefed_spcomp.species = 202")
    
    dataF <- data.table(sqlQuery(AFSC, test))
    
    # Save output
    save(dataF, file = here::here(CYR, "output", "fish_cond.RData"))}
  
  if(data_query == FALSE){
    load(here::here(CYR, "output", "fish_cond.RData"))}
  
  dataF %>% 
    rename_all(tolower) %>% 
    filter(length > 0, length < 150, weight > 0, year > 1991) %>% 
    mutate(trimester = case_when(month <= 4 ~ 1,
                                 month > 4 & month <= 8 ~ 2,
                                 month > 8 ~ 3)) %>%
    mutate(area = case_when(nmfs_area > 610 ~ "CG",
                            nmfs_area == 610 ~ "WG")) %>% 
    mutate(gear_desc = case_when(gear <= 2 ~ "Trawl",
                                 gear == 6 ~ "Pot",
                                 gear == 8 ~ "Longline")) %>% 
    group_by(gear, area, trimester, gear_desc) %>% 
    mutate(residuals = lw.resids(length, weight, outlier.rm = TRUE)) %>% 
    mutate(residuals.wt = weighted_resids(year, residuals, catch)) %>% 
    filter(is.na(residuals.wt) == FALSE) %>% 
    group_by(year, area, gear, trimester, gear_desc) %>% 
    summarise(ymeans = mean(residuals.wt),
              yn = length(residuals.wt),
              ysd = sd(residuals.wt),
              yse = ysd / sqrt(yn)) %>% 
    filter(year > 1997) -> plot_data

  # Plot WGOA
  WG_plot <- ggplot(plot_data %>% filter(gear %in% c(6,8), area == "WG", trimester == 1), 
                    aes(x = year, y = ymeans), cex = 2) +  
    geom_bar(position = position_dodge(), width = 1, stat = "identity", fill = "gray70", col = "black") + 
    geom_errorbar(aes(ymin = ymeans - yse, ymax = ymeans + yse), width = 0.30) +
    scale_x_continuous(breaks = c(1999:CYR), limits = c(1998.5, CYR + 0.5)) +
    ylim(-0.2, 0.2) +
    geom_hline(yintercept = 0, color = "black") +
    theme_bw(base_size = 20) +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(size = 18, vjust = 0.5, angle = 90),
          axis.text.y = element_text(size = 18)) +
    facet_wrap( ~ gear_desc, scale = "free_y", nrow = 2) +
    labs(y = "Length-weight residual 20cm-150cm", x = "Year", title = "Western GOA Jan-Apr")
  
  # Plot CGOA
  CG_plot <- ggplot(plot_data %>% filter(gear %in% c(6,8), area == "CG", trimester == 1), 
                    aes(x = year, y = ymeans), cex = 2) +  
    geom_bar(position = position_dodge(), width = 1, stat = "identity", fill = "gray70", col = "black") + 
    geom_errorbar(aes(ymin = ymeans - yse, ymax = ymeans + yse), width = 0.30) +
    scale_x_continuous(breaks = c(1999:CYR), limits = c(1998.5, CYR + 0.5)) +
    ylim(-0.2, 0.2) +
    geom_hline(yintercept = 0, color = "black") +
    theme_bw(base_size = 20) +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(size = 18, vjust = 0.5, angle = 90),
          axis.text.y = element_text(size = 18)) +
    facet_wrap( ~ gear_desc, scale = "free_y", nrow = 2) +
    labs(y = "Length-weight residual 20cm-150cm", x = "Year", title = "Central GOA Jan-Apr")
  

  cond_plots <- list(WG_plot, CG_plot)
  
  cond_plots

}
