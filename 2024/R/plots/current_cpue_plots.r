## Function to plot cpue

plot_cum_cpue <- function(data_query = FALSE,
                          species = 202,
                          cyr = 2024){

  # get data ----
  if(isTRUE(data_query)){
    # get connected to akfin
    db = 'akfin'
    conn = afscdata::connect(db)
    
    # pull akfin observer catch table
    syr = cyr - 10
    dplyr::tbl(conn, dplyr::sql('council.comprehensive_obs_haul')) %>% 
      dplyr::rename_all(tolower) %>% 
      dplyr::filter(year > syr) %>% 
      dplyr::select(cruise, haul_join, target_fishery_name, trip_target_name) %>% 
      dplyr::mutate(haul_join = as.character(haul_join)) %>% 
      collect() -> data_catch
    
    # get connected to afsc
    db = 'afsc'
    conn = afscdata::connect(db)

    # pull afsc observer catch table
    dplyr::tbl(conn, dplyr::sql('obsint.current_haul')) %>% 
      dplyr::inner_join(dplyr::tbl(conn, dplyr::sql('obsint.current_spcomp')),
                       by = c('CRUISE', 'PERMIT', 'HAUL_SEQ')) %>% 
      dplyr::rename_all(tolower) %>% 
      dplyr::select(species,
                    haul_join = haul_join.x,
                    cruise,
                    vessel = vessel.x,
                    gear_type,
                    vessel_type,
                    year = year.x,
                    nmfs_area,
                    sample_type,
                    sample_number,
                    sample_size,
                    sample_weight,
                    weight,
                    count,
                    official_total_catch,
                    deployment_date,
                    retrieval_date,
                    total_hooks_pots,
                    latdd_end,
                    londd_end,
                    bottom_depth_fathoms) %>% 
      dplyr::mutate(haul_join = as.character(haul_join),
                    month = lubridate::month(retrieval_date)) %>% 
      dplyr::filter(species == species,
                    nmfs_area %in% c(610, 620, 630),
                    year > syr) %>% 
      collect() -> data_all

    data_all %>% 
      tidytable::left_join(data_catch, .by = c(cruise, haul_join)) %>% 
      tidytable::filter(trip_target_name %in% c("Pacific Cod"),
                        gear_type %in% c(1,2,6,8),
                        !is.na(deployment_date),
                        !is.na(retrieval_date)) %>% 
      tidytable::mutate(gear = case_when(gear_type == 8 ~ "Longline",
                                         gear_type == 6 ~ "Pot",
                                         gear_type %in% c(1,2) ~ "Trawl"),
                        duration = as.numeric(as.POSIXct(retrieval_date, format = "%Y-%m-%d %H:%M:%S") - as.POSIXct(deployment_date, format = "%Y-%m-%d %H:%M:%S")) / 60,
                        cod_weight = case_when(gear == "Trawl" ~ (sample_weight / sample_size) * official_total_catch,
                                               gear %in% c("Longline", "Pot") ~ (weight / count) * (total_hooks_pots * sample_number / sample_size)),
                        cod_number = case_when(gear == "Trawl" ~ cod_weight / (sample_weight / sample_number),
                                               gear %in% c("Longline", "Pot") ~ count / sample_size * total_hooks_pots),
                        cpue_w = case_when(gear == "Trawl" ~ cod_weight / duration,
                                           gear %in% c("Longline", "Pot") ~ weight / total_hooks_pots),
                        cpue_n = case_when(gear == "Trawl" ~ cod_number / duration,
                                           gear %in% c("Longline", "Pot") ~ count / total_hooks_pots),
                        region = case_when(nmfs_area == 610 ~ "Western GOA",
                                           nmfs_area %in% c(620, 630) ~ "Central GOA")) -> cpue_goa
    
    # Save output
    save(cpue_goa, file = here::here(cyr, "output", "cumul_cpue.RData"))
  } else{
    load(here::here(cyr, "output", "cumul_cpue.RData"))}

  # plot weight cpue ----
  
  ggplot(data_goa %>% tidytable::filter(month %in% seq(1,4)),
         aes(as.factor(year), log(cpue_w), group = as.factor(year))) +
    geom_boxplot() +
    theme_bw() +
    facet_wrap(region ~ gear, scales = "free_y") +
    xlab("Year") +
    ylab("log(CPUE)") +
    ggtitle(paste0("Weight CPUE by Year for GOA Jan-Apr")) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  dev.print(png, file = here::here(new_SS_dat_year, "plots", "other", "cpue_w.png"), width = 700, height = 400)
  dev.off()
  
  ggplot(data_goa %>% tidytable::filter(month %in% seq(1,4)),
         aes(as.factor(year), log(cpue_n), group = as.factor(year))) +
    geom_boxplot() +
    theme_bw() +
    facet_wrap(region ~ gear, scales = "free_y") +
    xlab("Year") +
    ylab("log(CPUE)") +
    ggtitle(paste0("Number CPUE by Year for GOA Jan-Apr")) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  dev.print(png, file = here::here(new_SS_dat_year, "plots", "other", "cpue_n.png"), width = 700, height = 400)
  dev.off()
  

}


