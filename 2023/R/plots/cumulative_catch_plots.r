## Function to plot cumulative catch for species and subarea

plot_cumulative<-function(data_query = FALSE,
                           species = "'PCOD'",
                           FMP_AREA = "'GOA'",
                           syear = 2015,
                           CYR = 2022,
                           curr_wk = 20){

  
  if(data_query == TRUE){
    test<-paste0("SELECT
    council.comprehensive_blend_ca.week_end_date,
    council.comprehensive_blend_ca.species_group_code,
    council.comprehensive_blend_ca.year,
    council.comprehensive_blend_ca.fmp_area,
    council.comprehensive_blend_ca.fmp_subarea,
    council.comprehensive_blend_ca.fmp_gear,
    council.comprehensive_blend_ca.weight_posted
    FROM council.comprehensive_blend_ca
    WHERE
    council.comprehensive_blend_ca.species_group_code = ", species,
                 " AND council.comprehensive_blend_ca.year > ", syear,
                 " AND council.comprehensive_blend_ca.fmp_area = ", FMP_AREA)
    
    WED_C <- data.table(sqlQuery(CHINA, test))
    
    # Save output
    save(WED_C, file = here::here("output", "cumul_c.RData"))}
  
  if(data_query == FALSE){
    load(here::here("output", "cumul_c.RData"))}

  # Get CG plot data together
  subarea <- 'CG'
   WED_C %>% 
     rename_all(tolower) %>% 
     mutate(week = as.numeric(as.character(format(as.Date(week_end_date), "%W")))) %>% 
     filter(fmp_subarea == subarea) %>% 
     group_by(week, fmp_gear, year) %>% 
     summarise(tons = sum(weight_posted)) -> w1
   
   expand.grid(year = unique(w1$year), fmp_gear = unique(w1$fmp_gear), week = 0:52) %>% 
     filter(year < CYR) %>% 
     bind_rows(expand.grid(year = CYR, fmp_gear = unique(w1$fmp_gear), week = 0:curr_wk)) %>% 
     left_join(w1) %>% 
     mutate(tons = replace_na(tons, 0),
            cum = tons) %>% 
     arrange(fmp_gear, year, week) -> cumul_c

   for(i in 2:nrow(cumul_c)){
     if(cumul_c$year[i] == cumul_c$year[i - 1] & cumul_c$fmp_gear[i] == cumul_c$fmp_gear[i - 1]) 
       cumul_c$cum[i] <- cumul_c$tons[i] + cumul_c$cum[i - 1]
   }
   
  cumul_CG <- ggplot(data = cumul_c, 
              aes(x = week, y = cum, color = factor(year))) + 
    geom_point() + 
    geom_path(aes(group = year)) +
    facet_wrap( ~ fmp_gear, scale = "free_y") +
    theme_bw() +
    theme(axis.text.x = element_text(vjust = 0.5, angle = 90)) +
    labs(title = paste0(species, " Area = ", FMP_AREA, " Subarea = ", subarea), y = "Cummulative Catch (t)", color = "Year")
  
  # Get WG plot data together
  subarea <- 'WG'
  WED_C %>% 
    rename_all(tolower) %>% 
    mutate(week = as.numeric(as.character(format(as.Date(week_end_date), "%W")))) %>% 
    filter(fmp_subarea == subarea) %>% 
    group_by(week, fmp_gear, year) %>% 
    summarise(tons = sum(weight_posted)) -> w1
  
  expand.grid(year = unique(w1$year), fmp_gear = unique(w1$fmp_gear), week = 0:52) %>% 
    filter(year < CYR) %>% 
    bind_rows(expand.grid(year = CYR, fmp_gear = unique(w1$fmp_gear), week = 0:curr_wk)) %>% 
    left_join(w1) %>% 
    mutate(tons = replace_na(tons, 0),
           cum = tons) %>% 
    arrange(fmp_gear, year, week) -> cumul_c
  
  for(i in 2:nrow(cumul_c)){
    if(cumul_c$year[i] == cumul_c$year[i - 1] & cumul_c$fmp_gear[i] == cumul_c$fmp_gear[i - 1]) 
      cumul_c$cum[i] <- cumul_c$tons[i] + cumul_c$cum[i - 1]
  }
  
  cumul_WG <- ggplot(data = cumul_c, 
                     aes(x = week, y = cum, color = factor(year))) + 
    geom_point() + 
    geom_path(aes(group = year)) +
    facet_wrap( ~ fmp_gear, scale = "free_y") +
    theme_bw() +
    theme(axis.text.x = element_text(vjust = 0.5, angle = 90)) +
    labs(title = paste0(species, " Area = ", FMP_AREA, " Subarea = ", subarea), y = "Cummulative Catch (t)", color = "Year")
  
  cumul_plots <- list (cumul_CG, cumul_WG)
  
  cumul_plots
  
}


