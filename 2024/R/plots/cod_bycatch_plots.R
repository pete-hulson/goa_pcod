# Functions to plot PCod as bycatch in other fisheries

pollock_bycatch <- function(CYR,
                            data_query = FALSE){
  
  if(data_query == TRUE){
    
    test3 <- paste0("SELECT OBSINT.CURRENT_SPCOMP.SPECIES,
    OBSINT.CURRENT_HAUL.GEAR_TYPE,
    OBSINT.CURRENT_SPCOMP.YEAR,
    OBSINT.CURRENT_HAUL.NMFS_AREA,
    OBSINT.CURRENT_SPCOMP.SAMPLE_NUMBER,
    OBSINT.CURRENT_SPCOMP.SAMPLE_SIZE,
    OBSINT.CURRENT_SPCOMP.SAMPLE_WEIGHT,
    OBSINT.CURRENT_SPCOMP.WEIGHT,
    OBSINT.CURRENT_SPCOMP.COUNT,
    OBSINT.CURRENT_HAUL.DEPLOYMENT_DATE,
    OBSINT.CURRENT_HAUL.RETRIEVAL_DATE,
    to_char(OBSINT.CURRENT_HAUL.RETRIEVAL_DATE,'mm') as MONTH,
    OBSINT.CURRENT_HAUL.TOTAL_HOOKS_POTS,
    OBSINT.CURRENT_HAUL.LATDD_END,
    OBSINT.CURRENT_HAUL.LONDD_END,
    OBSINT.CURRENT_HAUL.BOTTOM_DEPTH_FATHOMS,
    OBSINT.CURRENT_HAUL.VESSEL_TYPE,
    OBSINT.CURRENT_HAUL.OFFICIAL_TOTAL_CATCH,
    OBSINT.CURRENT_SPCOMP.SAMPLE_TYPE,
    OBSINT.CURRENT_SPCOMP.VESSEL,
    OBSINT.CURRENT_HAUL.HAUL_JOIN,
    OBSINT.CURRENT_SPCOMP.CRUISE,
    OBSINT.CURRENT_SPCOMP.HAUL
    FROM OBSINT.CURRENT_HAUL
    INNER JOIN OBSINT.CURRENT_SPCOMP
    ON OBSINT.CURRENT_HAUL.CRUISE = OBSINT.CURRENT_SPCOMP.CRUISE
    AND OBSINT.CURRENT_HAUL.PERMIT = OBSINT.CURRENT_SPCOMP.PERMIT
    AND OBSINT.CURRENT_HAUL.HAUL_SEQ = OBSINT.CURRENT_SPCOMP.HAUL_SEQ
    AND OBSINT.CURRENT_SPCOMP.HAUL_JOIN = OBSINT.CURRENT_HAUL.HAUL_JOIN
    WHERE OBSINT.CURRENT_HAUL.GEAR_TYPE = 2
    AND OBSINT.CURRENT_SPCOMP.YEAR > 2007")
    
    data_P <- data.table(sqlQuery(AFSC, test3, as.is = T))
    
    # Save output
    save(data_P, file = here::here(CYR, "output", "poll_plotdat.RData"))}
  
  if(data_query == FALSE){
    load(here::here(CYR, "output", "poll_plotdat.RData"))}
  
  data_P %>% 
    rename_all(tolower) %>% 
    mutate(id = paste(cruise, vessel, haul, sep = "_")) %>% 
    filter(month %in% c("01", "02", "03"), nmfs_area %in% c(610, 620, 630)) %>%  
    group_by(year, nmfs_area) %>% 
    summarise(ncod = length(unique(id[species == 202])),
              nhauls = length(unique(id))) %>% 
    mutate(pcod = ncod / nhauls) -> data.p
  
  prop_hls <- ggplot(data.p, aes(year, pcod, group="")) +
    ylim(0, 1) +
    geom_line(size = 1, color = "gray20") +
    geom_point(size = 4, color = "red") +
    ylab("Proportion of hauls with Pcod") +
    facet_wrap(~nmfs_area) +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          strip.text = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          axis.title.y = element_text(size = 14))
  
  num_hls <- ggplot(data.p,aes(x = year, y = nhauls)) +
    geom_col() +
    ylab("Number of pelagic hauls") +
    xlab("Year") +
    facet_wrap(~nmfs_area) +
    theme(strip.text = element_blank(),
          axis.text.x = element_text(size = 14, vjust = 0.5, angle = 90),
          axis.text.y = element_text(size = 14),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14))
  
  poll_plots <- list(prop_hls, num_hls)
  
  poll_plots
  
}


swf_bycatch <- function(CYR,
                        data_query = FALSE){

  if(data_query == TRUE){
    test <- paste0("SELECT OBSINT.DEBRIEFED_SPCOMP.SPECIES,
  CAST(OBSINT.DEBRIEFED_HAUL.HAUL_JOIN AS VARCHAR(20)) AS HAUL_JOIN,
  OBSINT.DEBRIEFED_HAUL.CRUISE,
  OBSINT.DEBRIEFED_SPCOMP.YEAR,
  OBSINT.DEBRIEFED_SPCOMP.EXTRAPOLATED_WEIGHT,
  OBSINT.DEBRIEFED_SPCOMP.EXTRAPOLATED_NUMBER,
  OBSINT.DEBRIEFED_HAUL.OFFICIAL_TOTAL_CATCH,
  OBSINT.DEBRIEFED_HAUL.DEPLOYMENT_DATE,
  OBSINT.DEBRIEFED_HAUL.RETRIEVAL_DATE,
  OBSINT.DEBRIEFED_HAUL.NMFS_AREA,
  to_char(OBSINT.DEBRIEFED_HAUL.RETRIEVAL_DATE,'mm') as MONTH
  FROM OBSINT.DEBRIEFED_HAUL
  INNER JOIN OBSINT.DEBRIEFED_SPCOMP
  ON OBSINT.DEBRIEFED_HAUL.CRUISE = OBSINT.DEBRIEFED_SPCOMP.CRUISE
  AND OBSINT.DEBRIEFED_HAUL.PERMIT = OBSINT.DEBRIEFED_SPCOMP.PERMIT
  WHERE OBSINT.DEBRIEFED_SPCOMP.YEAR > 2007
  AND OBSINT.DEBRIEFED_HAUL.GEAR_TYPE = 1
  AND OBSINT.DEBRIEFED_HAUL.NMFS_AREA > 609 
  AND OBSINT.DEBRIEFED_HAUL.NMFS_AREA < 650")
    
    test2 <- paste0("SELECT COUNCIL.COMPREHENSIVE_OBS_HAUL.CRUISE,
  CAST(COUNCIL.COMPREHENSIVE_OBS_HAUL.HAUL_JOIN AS VARCHAR(20)) AS HAUL_JOIN,
  COUNCIL.COMPREHENSIVE_OBS_HAUL.TARGET_FISHERY_NAME,
  COUNCIL.COMPREHENSIVE_OBS_HAUL.TRIP_TARGET_NAME
  FROM COUNCIL.COMPREHENSIVE_OBS_HAUL
  WHERE COUNCIL.COMPREHENSIVE_OBS_HAUL.YEAR > 2007")
    
    data_CATCH <- data.table(sqlQuery(CHINA, test2, as.is = T))
    data_ALL <- data.table(sqlQuery(AFSC, test, as.is = T))
    
    data_ALL2 = merge(data_ALL,data_CATCH,by=c("CRUISE","HAUL_JOIN"))
    
    # Save output
    save(data_ALL2, file = here::here(CYR, "output", "swf_plotdat.RData"))}
  
  if(data_query == FALSE){
    load(here::here(CYR, "output", "swf_plotdat.RData"))}
  
  data_ALL2 %>% 
    rename_all(tolower) %>% 
    filter(trip_target_name == "Shallow Water Flatfish - GOA", month < 4) %>% 
    group_by(year) %>% 
    summarise(cod = sum(as.numeric(extrapolated_weight[species == 202]), na.rm = TRUE) / 1000,
              total_w = sum(as.numeric(extrapolated_weight), na.rm = TRUE) / 1000) %>% 
    mutate(prop = cod / total_w) -> swf_data
  
  swf_plot <- ggplot(swf_data,
                     aes(x = year, y = prop, group = "")) +
    geom_line(size = 1) +
    geom_point(size = 4, color = "red", fill = "yellow") +
    theme_bw(base_size = 16) +
    xlab("Year") +
    ylab("Pacific cod (t)/Total Catch(t)") +
    theme(axis.text.x = element_text(vjust = 0.5, angle = 90),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank()) +
    ggtitle(paste0("Pcod bycatch in GOA Shallow water flatfish fisheries 2008-", CYR))
  
  swf_plot
  
}

