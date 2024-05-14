## Function to plot cumulative catch for species and subarea

plot_cumulative<-function(data_query = FALSE,
                          species = "PCOD",
                          fmp_area = "GOA",
                          cyr = 2024){

  # get data ----
  if(isTRUE(data_query)){
    # get connected to akfin
    db = 'akfin'
    conn = afscdata::connect(db)
    
    # pull akfin catch table
    syr = cyr - 5
    dplyr::tbl(conn, dplyr::sql('council.comprehensive_blend_ca')) %>% 
      dplyr::rename_all(tolower) %>% 
      dplyr::filter(species_group_code == species,
                    fmp_area == fmp_area,
                    year > syr) %>% 
      dplyr::select(year, species_group_code, fmp_area, fmp_subarea, fmp_gear, weight_posted, week_end_date) %>% 
      collect() -> wed_c
    
    # Save output
    save(wed_c, file = here::here(cyr, "output", "cumul_c.RData"))
  } else{
    load(here::here(cyr, "output", "cumul_c.RData"))}

  # fcns ----
  cumul_catch = function(data, subarea, cyr, curr_wk){
    data %>% 
      tidytable::mutate(week = as.numeric(as.character(format(as.Date(week_end_date), "%W")))) %>% 
      tidytable::filter(fmp_subarea == subarea) %>% 
      tidytable::summarise(tons = sum(weight_posted), .by = c(week, fmp_gear, year)) -> w1
    
    tidytable::expand_grid(year = unique(w1$year),
                           fmp_gear = unique(w1$fmp_gear), 
                           week = 0:52) %>% 
      tidytable::filter(year < cyr) %>% 
      tidytable::bind_rows(expand.grid(year = cyr, 
                                       fmp_gear = unique(w1$fmp_gear),
                                       week = 0:curr_wk)) %>% 
      tidytable::left_join(w1) %>% 
      tidytable::mutate(tons = replace_na(tons, 0),
                        cum = tons) %>% 
      tidytable::arrange(fmp_gear, year, week) %>% 
      tidytable::mutate(cum = cumsum(tons), .by = c(year, fmp_gear))
  }
  
  plot_cumul_catch <- function(data){
      ggplot(data = data, 
             aes(x = week, y = cum, color = factor(year))) + 
      geom_point() + 
      geom_path(aes(group = year)) +
      facet_wrap( ~ fmp_gear, scale = "free_y") +
      theme_bw() +
      theme(axis.text.x = element_text(vjust = 0.5, angle = 90)) +
      labs(title = paste0(species, " Area = ", fmp_area, " Subarea = ", subarea), y = "Cummulative Catch (t)", color = "Year")
  }
  
  # plot cumulative catch ----
  # current week
  curr_wk <- as.numeric(format(Sys.Date(), format = "%W"))
  
  ## central gulf ----
  subarea <- 'CG'

  cumul_CG <- cumul_catch(wed_c, subarea, cyr, curr_wk)
  cumul_CG_plot <- plot_cumul_catch(cumul_CG)
  
  ## western gulf ----
  subarea <- 'WG'
  
  cumul_WG <- cumul_catch(wed_c, subarea, cyr, curr_wk)
  cumul_WG_plot <- plot_cumul_catch(cumul_WG)

  # return ----
  list (cumul_CG_plot, cumul_WG_plot)
  
}


