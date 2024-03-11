## Function to plot cumulative catch for species and subarea

plot_mean_dl<-function(data_query = FALSE,
                       species = 202,
                       FMP_AREA = "'GOA'",
                       CYR){

  
  if(data_query == TRUE){
    
    # Get length comps
    test <- paste("SELECT \n ",
                  "CASE \n ",
                  "  WHEN OBSINT.DEBRIEFED_LENGTH.GEAR in (1,2,3,4) \n ",
                  "  THEN 0\n ",
                  "  WHEN OBSINT.DEBRIEFED_LENGTH.GEAR in 6 \n ",
                  "  THEN 2 \n ",
                  "  WHEN OBSINT.DEBRIEFED_LENGTH.GEAR in (5,7,9,10,11,68,8) \n ",
                  "  THEN 3 \n ",
                  "END                                              AS GEAR, \n ",
                  "CONCAT('H',TO_CHAR(OBSINT.DEBRIEFED_SPCOMP.HAUL_JOIN))       AS HAUL_JOIN, \n ",
                  "TO_CHAR(OBSINT.DEBRIEFED_SPCOMP.HAUL_DATE, 'MM') AS MONTH, \n ",
                  "CASE \n ",
                  "  WHEN TO_CHAR(OBSINT.DEBRIEFED_SPCOMP.HAUL_DATE, 'MM') <= 2 \n ",
                  "  THEN 1 \n ",
                  "  WHEN TO_CHAR(OBSINT.DEBRIEFED_SPCOMP.HAUL_DATE, 'MM') > 2 \n ",
                  "  AND TO_CHAR(OBSINT.DEBRIEFED_SPCOMP.HAUL_DATE, 'MM') <= 4 \n ",
                  "  THEN 2 \n ",
                  "  WHEN TO_CHAR(OBSINT.DEBRIEFED_SPCOMP.HAUL_DATE, 'MM') > 4 \n ",
                  "  AND TO_CHAR(OBSINT.DEBRIEFED_SPCOMP.HAUL_DATE, 'MM') <= 8 \n ",
                  "  THEN 3 \n ",
                  "  WHEN TO_CHAR(OBSINT.DEBRIEFED_SPCOMP.HAUL_DATE, 'MM') > 8 \n ",
                  "  AND TO_CHAR(OBSINT.DEBRIEFED_SPCOMP.HAUL_DATE, 'MM') <= 10 \n ",
                  "  THEN 4 \n ",
                  "  WHEN TO_CHAR(OBSINT.DEBRIEFED_SPCOMP.HAUL_DATE, 'MM') > 10 \n ",
                  "  THEN 5 \n ",
                  "END                                                AS SEASON, \n ",
                  "CASE \n ",
                  "  WHEN TO_CHAR(OBSINT.DEBRIEFED_SPCOMP.HAUL_DATE, 'MM') in (1,2,3) \n ",
                  "  THEN 1 \n ",
                  "  WHEN TO_CHAR(OBSINT.DEBRIEFED_SPCOMP.HAUL_DATE, 'MM') in (4,5,6) \n ",
                  "  THEN 2 \n ",
                  "  WHEN TO_CHAR(OBSINT.DEBRIEFED_SPCOMP.HAUL_DATE, 'MM') in (7,8,9) \n ",
                  "  THEN 3 \n ",
                  "  WHEN TO_CHAR(OBSINT.DEBRIEFED_SPCOMP.HAUL_DATE, 'MM') in (10,11,12) \n ",
                  "  THEN 4 \n ",
                  "END                                                AS QUARTER, \n ",
                  "CASE \n ",
                  "  WHEN TO_CHAR(OBSINT.DEBRIEFED_SPCOMP.HAUL_DATE, 'MM') in (1,2,3,4) \n ",
                  "  THEN 1 \n ",
                  "  WHEN TO_CHAR(OBSINT.DEBRIEFED_SPCOMP.HAUL_DATE, 'MM') in (5,6,7,8) \n ",
                  "  THEN 2 \n ",
                  "  WHEN TO_CHAR(OBSINT.DEBRIEFED_SPCOMP.HAUL_DATE, 'MM') in (9,10,11,12) \n ",
                  "  THEN 3 \n ",
                  "END                                                AS TRIMESTER, \n ",
                  "TO_CHAR(OBSINT.DEBRIEFED_SPCOMP.HAUL_DATE, 'YYYY') AS YEAR, \n ",
                  "OBSINT.DEBRIEFED_SPCOMP.EXTRAPOLATED_NUMBER        AS NUMB, \n ",
                  "OBSINT.DEBRIEFED_SPCOMP.CRUISE        AS CRUISE, \n ",
                  "OBSINT.DEBRIEFED_SPCOMP.PERMIT        AS PERMIT, \n ",
                  "OBSINT.DEBRIEFED_SPCOMP.HAUL        AS HAUL, \n ",
                  "OBSINT.DEBRIEFED_SPCOMP.EXTRAPOLATED_WEIGHT / 1000 AS WEIGHT, \n ",
                  "OBSINT.DEBRIEFED_LENGTH.LENGTH                     AS LENGTH, \n ",
                  "OBSINT.DEBRIEFED_LENGTH.FREQUENCY                  AS FREQ, \n ",
                  "OBSINT.DEBRIEFED_HAUL.LONDD_END AS LON, \n",
                  "OBSINT.DEBRIEFED_HAUL.LATDD_END AS LAT, \n",
                  "OBSINT.DEBRIEFED_SPCOMP.HAUL_DATE AS HDAY, \n",
                  "OBSINT.DEBRIEFED_HAUL.NMFS_AREA AS AREA \n",
                  "FROM OBSINT.DEBRIEFED_HAUL \n ",
                  "INNER JOIN OBSINT.DEBRIEFED_SPCOMP \n ",
                  "ON OBSINT.DEBRIEFED_HAUL.HAUL_JOIN = OBSINT.DEBRIEFED_SPCOMP.HAUL_JOIN \n ",
                  "INNER JOIN OBSINT.DEBRIEFED_LENGTH \n ",
                  "ON OBSINT.DEBRIEFED_HAUL.HAUL_JOIN = OBSINT.DEBRIEFED_LENGTH.HAUL_JOIN \n ",
                  "WHERE OBSINT.DEBRIEFED_HAUL.NMFS_AREA BETWEEN 600 AND 699 \n",
                  "AND OBSINT.DEBRIEFED_LENGTH.NMFS_AREA != 670 \n",
                  "AND OBSINT.DEBRIEFED_SPCOMP.SPECIES  in  (",species,")",
                  "AND OBSINT.DEBRIEFED_LENGTH.SPECIES    in  (",species,")",sep="")
    
    lencomp <- data.table(sqlQuery(AFSC,test))
    
    # Get catch
    test <- paste0("SELECT 
    NORPAC.DEBRIEFED_SPCOMP_MV.YEAR, 
    NORPAC.DEBRIEFED_SPCOMP_MV.CRUISE, 
    NORPAC.DEBRIEFED_SPCOMP_MV.PERMIT, 
    NORPAC.DEBRIEFED_SPCOMP_MV.HAUL_JOIN AS HAUL, 
    NORPAC.DEBRIEFED_SPCOMP_MV.EXTRAPOLATED_WEIGHT AS WT, 
    NORPAC.DEBRIEFED_HAUL_MV.BOTTOM_DEPTH_FATHOMS AS DEPTH, 
    NORPAC.DEBRIEFED_HAUL_MV.TRIP_TARGET_NAME AS TARGET,
    NORPAC.DEBRIEFED_HAUL_MV.GEAR_TYPE AS GEAR, 
    NORPAC.DEBRIEFED_HAUL_MV.NMFS_AREA AS AREA 
    FROM 
    NORPAC.DEBRIEFED_SPCOMP_MV INNER JOIN NORPAC.DEBRIEFED_HAUL_MV ON NORPAC.DEBRIEFED_SPCOMP_MV.JOIN_KEY = NORPAC.DEBRIEFED_HAUL_MV.JOIN_KEY 
    WHERE 
    NORPAC.DEBRIEFED_HAUL_MV.FMP_AREA = ", FMP_AREA, 
                   " AND NORPAC.DEBRIEFED_SPCOMP_MV.SPECIES = 202")
    
    obs_C <- data.table(sqlQuery(CHINA, test))
    
    # Save output
    save(lencomp, file = here::here(CYR, "output", "lencomp.RData"))
    save(obs_C, file = here::here(CYR, "output", "obs_C.RData"))}
  
  if(data_query == FALSE){
    load(here::here(CYR, "output", "lencomp.RData"))
    load(here::here(CYR, "output", "obs_C.RData"))}
  
  # compute and plot catch-weighted mean length
  lencomp %>% 
    dplyr::rename_all(tolower) %>% 
    tidytable::mutate(haul1 = as.character(paste(cruise, permit, haul, sep = "_")),
                      area = trunc(area/10)*10,
                      gear = case_when(gear == 0 ~ "TRAWL",
                                       gear == 2 ~ "POT",
                                       gear == 3 ~ "LONGLINE")) %>% 
    tidytable::filter(area <= 630) %>% 
    tidytable::mutate(region = case_when(area == 610 ~ "WG",
                                         area %in% c(620, 630) ~ "CG")) %>% 
    tidytable::select(year, gear, region, haul1, length, freq, numb) %>% 
    tidytable::uncount(freq) %>% 
    tidytable::summarise(mu_len = mean(length), .by = c(year, gear, region, haul1, numb)) %>% 
    tidytable::summarise(tot_c = sum(numb), .by = c(year, gear, region)) -> tot_c
  
  
  lencomp %>% 
    dplyr::rename_all(tolower) %>% 
    tidytable::mutate(haul1 = as.character(paste(cruise, permit, haul, sep = "_")),
                      area = trunc(area/10)*10,
                      gear = case_when(gear == 0 ~ "TRAWL",
                                       gear == 2 ~ "POT",
                                       gear == 3 ~ "LONGLINE")) %>% 
    tidytable::filter(area <= 630) %>% 
    tidytable::mutate(region = case_when(area == 610 ~ "WG",
                                         area %in% c(620, 630) ~ "CG")) %>% 
    tidytable::select(year, gear, region, haul1, length, freq, numb) %>% 
    tidytable::uncount(freq) %>% 
    tidytable::summarise(mu_len = mean(length),
                         uci_len = quantile(length, probs = 0.975),
                         lci_len = quantile(length, probs = 0.025),
                         n_len = length(length), .by = c(year, gear, region, haul1, numb)) %>% 
    tidytable::mutate(ct_wt = numb * mu_len,
                      ct_wt_uci = numb * uci_len,
                      ct_wt_lci = numb * lci_len) %>% 
    tidytable::summarise(ct_wt = sum(ct_wt),
                         ct_wt_uci = sum(ct_wt_uci),
                         ct_wt_lci = sum(ct_wt_lci),
                         n_len = sum(n_len),
                         n_hl = length(haul1), .by = c(year, gear, region)) %>% 
    tidytable::left_join(tot_c) %>% 
    tidytable::mutate(mean_len = ct_wt / tot_c,
                      uci_len = ct_wt_uci / tot_c,
                      lci_len = ct_wt_lci / tot_c) %>% 
    tidytable::select(year, gear, region, mean_len, uci_len, lci_len, n_len, n_hl) %>% 
    tidytable::filter(year >= 2010) -> mean_len
  
  data.table(expand.grid(
    year = sort(unique(mean_len$year)),
    region = unique(mean_len$region),
    gear = unique(mean_len$gear))) %>% 
    left_join(mean_len) -> mean_len
  
  CYR <- max(mean_len$year)
  
  mu_len <- ggplot(data = mean_len, 
                   aes(x = year, y = mean_len, color = factor(region))) + 
    geom_point(size = 3) + 
    geom_path(aes(group = region)) +
    scale_x_continuous(breaks = c(2010:CYR), limits = c(2009.5, CYR + 0.5)) +
    scale_color_nmfs("waves", name = "") +
    facet_wrap( ~ gear) +
    theme_bw(base_size = 18) +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.text.x = element_blank()) +
    labs(y = "Catch Wtd Mean Length (cm)", x = NULL) + 
    geom_ribbon(aes(ymin = lci_len, ymax = uci_len), alpha = 0.05)
  
  ss_len <- ggplot(data = mean_len, 
               aes(x = year, y = n_len / 1000, fill = factor(region))) + 
    geom_bar(stat="identity", width=0.5) + 
    scale_x_continuous(breaks = c(2010:CYR), limits = c(2009.5, CYR + 0.5)) +
    facet_wrap( ~ gear) +
    scale_fill_nmfs("waves", name = "") +
    theme_bw(base_size = 18) +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(vjust = 0.5, angle = 90)) +
    labs(y = "Sample size (1,000s)", x = "Year") 
  
  
    
  # compute and plot catch-weighted mean depth
  obs_C %>% 
    dplyr::rename_all(tolower) %>% 
    tidytable::filter(gear %in% c(1, 2, 6, 8),
                      target == "Pacific Cod") %>% 
    tidytable::mutate(haul1 = as.character(paste(cruise, permit, haul, sep = "_")),
                      area = trunc(area/10)*10,
                      gear = case_when(gear %in% c(1, 2) ~ "TRAWL",
                                       gear == 6 ~ "POT",
                                       gear == 8 ~ "LONGLINE")) %>% 
    tidytable::filter(area <= 630,
                      !is.na(depth)) %>% 
    tidytable::mutate(region = case_when(area == 610 ~ "WG",
                                         area %in% c(620, 630) ~ "CG")) %>% 
    tidytable::select(year, gear, region, haul1, depth, wt) %>% 
    tidytable::summarise(tot_wt = sum(wt), .by = c(year, gear, region)) -> tot_wt
  
  obs_C %>% 
    dplyr::rename_all(tolower) %>% 
    tidytable::filter(gear %in% c(1, 2, 6, 8),
                      target == "Pacific Cod") %>% 
    tidytable::mutate(haul1 = as.character(paste(cruise, permit, haul, sep = "_")),
                      area = trunc(area/10)*10,
                      gear = case_when(gear %in% c(1, 2) ~ "TRAWL",
                                       gear == 6 ~ "POT",
                                       gear == 8 ~ "LONGLINE")) %>% 
    tidytable::filter(area <= 630,
                      !is.na(depth)) %>% 
    tidytable::mutate(region = case_when(area == 610 ~ "WG",
                                         area %in% c(620, 630) ~ "CG")) %>% 
    tidytable::select(year, gear, region, haul1, depth, wt) %>% 
    tidytable::mutate(d_wtd = depth * wt) %>%  
    tidytable::summarise(wtd_depth = -1 * sum(d_wtd),
                         n_hl = length(d_wtd), .by = c(year, gear, region)) %>% 
    tidytable::left_join(tot_wt) %>% 
    tidytable::mutate(wtd_depth = wtd_depth / tot_wt) %>% 
    tidytable::select(-tot_wt) %>% 
    tidytable::filter(!(year == 2020 & gear == "POT" ),
                      !(year %in% c(2018, 2019, 2020) & gear == "TRAWL" )) -> wtd_depth
    
  data.table(expand.grid(
    year = sort(unique(mean_len$year)),
    region = unique(mean_len$region),
    gear = unique(mean_len$gear))) %>% 
    left_join(wtd_depth) -> wtd_depth
  
    
  mu_dep <- ggplot(data = wtd_depth, 
                   aes(x = year, y = wtd_depth, color = factor(region))) + 
    geom_point(size = 3) + 
    geom_path(aes(group = region)) +
    scale_x_continuous(breaks = c(2010:CYR), limits = c(2009.5, CYR + 0.5)) +
    scale_color_nmfs("waves", name = "") +
    facet_wrap( ~ gear) +
    theme_bw(base_size = 18) +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.text.x = element_blank()) +
    labs(y = "Catch Wtd Mean Depth (fathoms)", x = NULL)
  
  ss_dep <- ggplot(data = wtd_depth, 
                   aes(x = year, y = n_hl / 100, fill = factor(region))) + 
    geom_bar(stat="identity", width=0.5) + 
    scale_x_continuous(breaks = c(2010:CYR), limits = c(2009.5, CYR + 0.5)) +
    facet_wrap( ~ gear) +
    scale_fill_nmfs("waves", name = "") +
    theme_bw(base_size = 18) +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(vjust = 0.5, angle = 90)) +
    labs(y = "Number of observed hauls (100s)", x = "Year") 
  
  mu_dl_plots <- list (mu_len, ss_len, mu_dep, ss_dep)
  
  return(mu_dl_plots)
  
}


