
## number of vessels

num_fish_vess <- function(CYR,
                          data_query = FALSE){

  if(data_query == TRUE){
    ## data pull for fishery data from AFSC database
    test1<-paste0("SELECT council.comprehensive_blend_ca.week_end_date,
    TO_CHAR(council.comprehensive_blend_ca.week_end_date, 'mm') AS month,
    council.comprehensive_blend_ca.year,
    council.comprehensive_blend_ca.vessel_id,
    council.comprehensive_blend_ca.trip_target_code,
    council.comprehensive_blend_ca.fmp_subarea,
    council.comprehensive_blend_ca.fmp_gear,
    council.comprehensive_blend_ca.weight_posted,
    council.comprehensive_blend_ca.species_name,
    council.comprehensive_blend_ca.trip_target_name,
    council.comprehensive_blend_ca.reporting_area_code,
    council.comprehensive_blend_ca.fmp_area
    FROM council.comprehensive_blend_ca
    WHERE council.comprehensive_blend_ca.species_name = 'cod, Pacific (gray)'
    AND council.comprehensive_blend_ca.fmp_area = 'GOA'")
    
    dataC <- data.table(sqlQuery(CHINA, test1))
    
    # Save output
    save(dataC, file = here::here("output", "num_vess.RData"))}
  
  if(data_query == FALSE){
    load(here::here("output", "num_vess.RData"))}
  
  dataC %>% 
    filter(MONTH %in% c(1:6) & TRIP_TARGET_CODE == "C") %>% 
    group_by(YEAR, FMP_GEAR, FMP_SUBAREA) %>% 
    summarize(NUM_VES = length(unique(VESSEL_ID))) %>% 
    filter(FMP_SUBAREA %in% c("CG", "WG")) %>% 
    pivot_wider(names_from = FMP_GEAR, values_from = NUM_VES) %>% 
    mutate(HAL = replace_na(HAL, 0),
           POT = replace_na(POT, 0),
           TRW = replace_na(TRW, 0),
           JIG = replace_na(JIG, 0),
           OTH = replace_na(OTH, 0)) %>% 
    pivot_longer(cols = c(HAL, POT, TRW, JIG, OTH), names_to = "FMP_GEAR", values_to = "NUM_VES") -> Effort
  
  p <- ggplot(Effort, 
              aes(x = YEAR, y = NUM_VES, fill = FMP_GEAR)) +
    geom_area(stat = "identity", alpha = 0.6) +
    scale_x_continuous(breaks = c(2003:CYR), limits = c(2003, CYR)) + 
    ylim(0, 400) +  
    theme_bw(base_size = 18) +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(vjust = 0.5, angle = 90)) +
    labs(y = "Number of unique vessels", x = "Year", fill = "Gear Type") +
    facet_wrap( ~ FMP_SUBAREA, scale = "free_y", nrow = 2)
  p
  
}



