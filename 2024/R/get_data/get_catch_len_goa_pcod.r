#' Get fishery catch length composition for goa pcod
#' Originally adapted/generalized from Steve Barbeaux' files for generating SS files for EBS/AI Greenland Turbot
#' Re-developed in 2024 by p. hulson (too many changes for short note)
#' 
#' @param new_year current assessment year
#' @param fsh_sp_code species code for observer/catch data (default = 202)
#' @param query switch for whether to run sql query for data (default = FALSE)
#' @param database switch for which database to pull data from (default = 'akfin')
#' @param fltr switch for whether to filter data to >30 lengths per haul (default = TRUE)
#' @param fill_st switch for whether to fill holes in fed data with state data (default = TRUE)
#' 

get_catch_len <- function(new_year = 9999,
                          fsh_sp_code = 202,
                          query = FALSE,
                          database = 'akfin',
                          fltr = TRUE,
                          fill_st = TRUE){
  
  # query length freq data ----
  # note that catch data is queried in 'get_catch_goa_pcod' fcn
  if(isTRUE(query)){

    if(database == 'afsc'){
      # get connected to afsc
      db = 'afsc'
      conn = afscdata::connect(db)

      # query fishery length data (note to self: not yet in afscdata - see if this query can get added to that package)
      dplyr::tbl(conn, dplyr::sql('obsint.debriefed_haul')) %>% 
        dplyr::inner_join(dplyr::tbl(conn, dplyr::sql('obsint.debriefed_spcomp')) %>% 
                            dplyr::filter(SPECIES == fsh_sp_code),
                          by = c('HAUL_JOIN')) %>% 
        dplyr::inner_join(dplyr::tbl(conn, dplyr::sql('obsint.debriefed_length')) %>% 
                            dplyr::filter(SPECIES == fsh_sp_code),
                          by = c('HAUL_JOIN')) %>% 
        dplyr::rename_all(tolower) %>% 
        dplyr::select(gear,
                      haul_join,
                      numb = extrapolated_number,
                      cruise = cruise.x,
                      permit = permit.x,
                      haul = haul.x,
                      weight = extrapolated_weight,
                      length,
                      freq = frequency,
                      lon = londd_end.x,
                      lat = latdd_end.x,
                      hday = haul_date.y,
                      area = nmfs_area.x) %>% 
        dplyr::filter(area >= 600,
                      area <= 699,
                      area != 670) %>% 
        dplyr::mutate(haul_join = paste0('H', haul_join)) -> afsc_len

      dplyr::collect(afsc_len) %>% 
        dplyr::mutate(hday = hday + lubridate::hours('8'),
                      weight = weight / 1000,
                      year = lubridate::year(hday),
                      month = lubridate::month(hday),
                      season = dplyr::case_when(month <= 2 ~ 1,
                                                month %in% c(3, 4) ~ 2,
                                                month %in% c(5, 6, 7, 8) ~ 3,
                                                month %in% c(9, 10) ~ 4,
                                                month >= 11 ~ 5),
                      quarter = dplyr::case_when(month <= 3 ~ 1,
                                                 month %in% c(4, 5, 6) ~ 2,
                                                 month %in% c(7, 8, 9) ~ 3,
                                                 month >= 10 ~ 4),
                      trimester = dplyr::case_when(month <= 4 ~ 1,
                                                   month %in% c(5, 6, 7, 8) ~ 2,
                                                   month >= 9 ~ 3),
                      gear = dplyr::case_when(gear %in% c(1, 2, 3, 4) ~ 'trawl',
                                              gear == 6 ~ 'pot',
                                              gear %in% c(5, 7, 9, 10, 11, 68, 8) ~ 'longline')) %>% 
        vroom::vroom_write(., here::here(new_year, 'data', 'raw', 'fish_lfreq.csv'), delim = ",")
      
      capture.output(dplyr::show_query(afsc_len), 
                     file = here::here(new_year, "data", "sql", "fsh_lfreq_afsc_sql.txt"))

    }
    
    if(database == 'akfin'){
      # get connected to akfin
      db = 'akfin'
      conn = afscdata::connect(db)
    
    # query fishery length data (note to self: not yet in afscdata - see if this query can get added to that package)
      dplyr::tbl(conn, dplyr::sql('norpac.debriefed_haul_mv')) %>% 
        dplyr::inner_join(dplyr::tbl(conn, dplyr::sql('norpac.debriefed_spcomp_mv')) %>% 
                            dplyr::filter(SPECIES == fsh_sp_code),
                          by = c('HAUL_JOIN')) %>% 
        dplyr::inner_join(dplyr::tbl(conn, dplyr::sql('norpac.debriefed_length_mv')) %>% 
                            dplyr::filter(SPECIES == fsh_sp_code),
                          by = c('HAUL_JOIN')) %>% 
        dplyr::rename_all(tolower) %>% 
        dplyr::select(gear,
                      haul_join,
                      numb = extrapolated_number,
                      cruise = cruise.x,
                      permit = permit.x,
                      haul = haul.x,
                      weight = extrapolated_weight,
                      length,
                      freq = frequency,
                      lon = londd_end.x,
                      lat = latdd_end.x,
                      hday = haul_date.x,
                      area = nmfs_area.x) %>% 
        dplyr::filter(area >= 600,
                      area <= 699,
                      area != 670) -> akfin_len 
      
      dplyr::collect(akfin_len) %>% 
        dplyr::mutate(haul_join = paste0('H', haul_join),
                      hday = hday + lubridate::hours('8'),
                      weight = weight / 1000,
                      year = lubridate::year(hday),
                      month = lubridate::month(hday),
                      season = dplyr::case_when(month <= 2 ~ 1,
                                                month %in% c(3, 4) ~ 2,
                                                month %in% c(5, 6, 7, 8) ~ 3,
                                                month %in% c(9, 10) ~ 4,
                                                month >= 11 ~ 5),
                      quarter = dplyr::case_when(month <= 3 ~ 1,
                                                 month %in% c(4, 5, 6) ~ 2,
                                                 month %in% c(7, 8, 9) ~ 3,
                                                 month >= 10 ~ 4),
                      trimester = dplyr::case_when(month <= 4 ~ 1,
                                                   month %in% c(5, 6, 7, 8) ~ 2,
                                                   month >= 9 ~ 3),
                      gear = dplyr::case_when(gear %in% c(1, 2, 3, 4) ~ 'trawl',
                                              gear == 6 ~ 'pot',
                                              gear %in% c(5, 7, 9, 10, 11, 68, 8) ~ 'longline')) %>%  
        dplyr::filter(year <= new_year) %>% 
        vroom::vroom_write(., here::here(new_year, 'data', 'raw', 'fish_lfreq.csv'), delim = ",")
      
      capture.output(dplyr::show_query(akfin_len), 
                     file = here::here(new_year, "data", "sql", "fsh_lfreq_akfin_sql.txt"))
    }
  }

  # read and prep data ----
  
  ## length freq data ----
  ### federal ----
  fsh_len_f <- vroom::vroom(here::here(new_year, 'data', 'raw', 'fish_lfreq.csv')) %>% 
    # filter to years post-1991
    tidytable::filter(year >= 1991) %>% 
    # unique cruise-permit-haul description
    tidytable::mutate(haul1 = paste(cruise, permit, haul, sep = "_")) %>% 
    # get correct week end date
    tidytable::mutate(weekday = weekdays(hday),
                      wed = lubridate::ceiling_date(hday, "week"),
                      plus = dplyr::case_when(weekdays(hday) == "Sunday" ~ 6,
                                              weekdays(hday) != "Sunday" ~ -1),
                      yr = lubridate::year(hday),
                      next_saturday = dplyr::case_when(yr >= 1993 ~ date(wed) + plus,
                                                       yr < 1993 ~ date(wed)),
                      yr2 = lubridate::year(next_saturday),
                      wed2 = dplyr::case_when(yr != yr2 ~ date(paste0(yr, '-12-31')),
                                              yr == yr2 ~ next_saturday)) %>% 
    tidytable::select(-weekday, - wed, -plus, -yr, -next_saturday, -yr2) %>% 
    tidytable::rename(wed = wed2) %>% 
    # truncate area to nearest 10 (e.g., 649 becomes 640), set plus length group
    tidytable::mutate(area = trunc(area / 10) * 10,
                      length = case_when(length > 116 ~ 117,
                                         length <= 116 ~ length))

  ### state ----
  fsh_len_s <- vroom::vroom(here::here(new_year, 'data', 'ALL_STATE_LENGTHS.csv')) %>% 
    dplyr::rename_all(tolower) %>% 
    #filter to positive lengths
    tidytable::filter(length > 0) %>% 
    # define area, gear, plus length, trimester
    tidytable::mutate(area = trunc(area / 10) * 10, # truncate area to nearest 10 (e.g., 649 becomes 640)
                      gear1 = 'trawl',
                      gear1 = case_when(gear == 91 ~ 'pot',
                                        gear %in% c(5, 26, 61) ~ 'longline',
                                        gear1 == 'trawl' ~ 'trawl'),
                      length = case_when(length > 116 ~ 117,
                                         length <= 116 ~ length),
                      trimester = case_when(month <= 4 ~ 1,
                                            month %in% seq(5, 8) ~ 2,
                                            month >= 9 ~ 3)) %>% 
    tidytable::select(year, area, gear = gear1, month, trimester, quarter, sex, length, freq)
  
  ## catch data ----
  vroom::vroom(here::here(new_year, 'data', 'raw', 'fsh_catch_data.csv')) %>% 
    tidytable::mutate(month = lubridate::month(week_end_date),
                      season = dplyr::case_when(month <= 2 ~ 1,
                                                month %in% c(3, 4) ~ 2,
                                                month %in% c(5, 6, 7, 8) ~ 3,
                                                month %in% c(9, 10) ~ 4,
                                                month >= 11 ~ 5),
                      quarter = dplyr::case_when(month <= 3 ~ 1,
                                                 month %in% c(4, 5, 6) ~ 2,
                                                 month %in% c(7, 8, 9) ~ 3,
                                                 month >= 10 ~ 4),
                      trimester = dplyr::case_when(month <= 4 ~ 1,
                                                   month %in% c(5, 6, 7, 8) ~ 2,
                                                   month >= 9 ~ 3)) %>% 
    tidytable::summarise(tons = sum(weight_posted),
                         .by = c(year, 
                                 month, 
                                 season, 
                                 quarter,
                                 trimester,
                                 fmp_gear,
                                 reporting_area_code, 
                                 retained_or_discarded, 
                                 week_end_date,
                                 akr_state_federal_waters_code)) %>% 
    tidytable::select(tons,
                      month,
                      season,
                      quarter,
                      trimester,
                      gear = fmp_gear,
                      year,
                      area = reporting_area_code,
                      wed = week_end_date,
                      state = akr_state_federal_waters_code) %>% 
    # define NAs in state flag, gear type, and truncate area to nearest 10
    tidytable::mutate(wed = date(wed),
                      state = case_when(is.na(state) ~ 'F',
                                        !is.na(state) ~ state),
                      gear = case_when(gear %in% c('TRW', 'GLN', 'OTH') ~ 'trawl',
                                       gear == 'POT' ~ 'pot',
                                       gear %in% c('HAL', 'JIG') ~ 'longline'),
                      area = trunc(area / 10) * 10) %>% 
    # compute proportion of annual catch by week-area-gear
    tidytable::summarise(tons = sum(tons), .by = c(year, wed, trimester, area, gear)) %>% 
    tidytable::mutate(total = sum(tons), .by = year) %>% 
    tidytable::mutate(catch_prop = tons / total) -> catch_p

  # filter length freq data ----
  if(isTRUE(fltr)){
    ## filtering data to hauls with greater than 10 (federal) and 30 (state) lengths (old way) ----
    fsh_len_f %>% 
      # filter hauls w/ >10 lengths observed
      tidytable::mutate(n_hl = sum(freq), .by = c(haul_join, numb)) %>% 
      tidytable::filter(n_hl >= 10) %>% 
      tidytable::select(-n_hl) -> fsh_len_full_f
    
    fsh_len_s %>% 
      # length freq at trimester-area-gear
      tidytable::summarise(freq = sum(freq), .by = c(year, month, trimester, area, gear, length)) %>% 
      tidytable::drop_na() %>% 
      # filter to >30 lengths per trimester, area, and gear
      tidytable::left_join(fsh_len_s %>% 
                             tidytable::summarise(sfreq = sum(freq), .by = c(year, trimester, area, gear)) %>% 
                             tidytable::drop_na()) %>% 
      tidytable::filter(sfreq >= 30) %>% 
      tidytable::select(-sfreq) -> fsh_len_full_s
  } else{
    ## not filtering data  ----
    fsh_len_full_f <- fsh_len_f
    
    fsh_len_s %>% 
      # length freq at trimester-area-gear
      tidytable::summarise(freq = sum(freq), .by = c(year, month, trimester, area, gear, length)) %>% 
      tidytable::drop_na() -> fsh_len_full_s
    
  }
  
  if(isTRUE(fill_st)){
    # fill-in with state data ----
    ## federal catch weighted length comp ----
    # get grid of all possible combos of federal year-gear-area-trimester-length
    tidytable::expand_grid(year = sort(unique(fsh_len_full_f$year)),
                           gear = unique(fsh_len_full_f$gear),
                           area = unique(fsh_len_full_f$area),
                           trimester = c(1:3),
                           length = seq(1,117,1)) %>% 
      # join weighted fish length comp
      tidytable::left_join(fsh_len_full_f %>% 
                             # haul-level length frequency
                             tidytable::summarise(freq = sum(freq),
                                                  .by = c(year, wed, trimester, area, haul1, gear, length)) %>% 
                             # join number of total fish sampled per haul
                             tidytable::left_join(fsh_len_full_f %>% 
                                                    tidytable::summarise(n1 = min(numb),
                                                                         hfreq = sum(freq),
                                                                         .by = c(year, wed, trimester, area, haul1, gear))) %>% 
                             # join number of fish by year, week, area, and gear
                             tidytable::left_join(fsh_len_full_f %>% 
                                                    tidytable::summarise(n1 = min(numb),
                                                                         hfreq = sum(freq),
                                                                         .by = c(year, wed, trimester, area, haul1, gear)) %>% 
                                                    tidytable::summarise(n2 = sum(n1),
                                                                         tfreq = sum(hfreq),
                                                                         .by = c(year, wed, trimester, area, gear))) %>% 
                             # proportion of haul catch-at-length by gear-area-week observed catch
                             # expand the haul length composition by haul catch, then divide by the gear-area-week total observed haul catches
                             tidytable::mutate(prop = ((freq / hfreq) * n1) / n2) %>%
                             # summarise to length composition by week-area-gear
                             tidytable::summarise(prop = sum(prop),
                                                  .by = c(year, wed, trimester, area, gear, length)) %>% 
                             # join length comp with catch proportion and compute catch weighted length comp by year, trimester, area, and gear
                             tidytable::left_join(catch_p) %>% 
                             tidytable::mutate(prop1 = prop * catch_prop) %>% 
                             tidytable::drop_na() %>% 
                             tidytable::summarise(prop1 = sum(prop1), .by = c(year, trimester, area, gear, length))) %>% 
      tidytable::mutate(prop = case_when(is.na(prop1) ~ 0,
                                         !is.na(prop1) ~ prop1)) %>% 
      tidytable::select(-prop1) -> fsh_len_comp_f
    
    ## state catch weighted length comp ----
    # get grid of all possible combos of state year-gear-area-trimester-length
    tidytable::expand_grid(year = sort(unique(fsh_len_full_s$year)),
                           gear = unique(fsh_len_full_s$gear),
                           area = unique(fsh_len_full_s$area),
                           trimester = c(1:3),
                           length = seq(1,117,1)) %>% 
      # join state length frequencies
      tidytable::left_join(fsh_len_full_s %>% 
                             # total lengths obs by trimester-area-gear
                             tidytable::mutate(total = sum(freq), .by = c(year, trimester, area, gear)) %>% 
                             # compute state length comp by trimester, area, and gear
                             tidytable::mutate(prop = freq / total) %>% 
                             tidytable::select(-total, -freq)) %>% 
      # join federal catch for trimester-area-gear
      tidytable::left_join(catch_p %>% 
                             tidytable::summarise(catch_prop = sum(catch_prop), .by = c(year, trimester, area, gear))) %>% 
      # compute catch weighted length comp
      tidytable::mutate(prop = tidytable::replace_na(prop, 0),
                        catch_prop = tidytable::replace_na(catch_prop, 0),
                        prop1 = prop * catch_prop) %>% 
      tidytable::summarise(prop = sum(prop1), .by = c(year, trimester, area, gear, length)) -> fsh_len_comp_s
    
    ## merge state and federal length frequencies ----
    # test which trimester-area-gear length freqs have more state than fed data
    fsh_len_f %>% 
      # get federal number of length obs by trimester-area-gear
      tidytable::summarise(tfreq = sum(freq), .by = c(year, trimester, area, gear)) %>% 
      # get state number of length obs by trimester-area-gear
      tidytable::full_join(fsh_len_s %>% 
                             tidytable::summarise(sfreq = sum(freq), .by = c(year, trimester, area, gear))) %>% 
      # set index for when state > federal lengths
      tidytable::mutate(tfreq = tidytable::replace_na(tfreq, 0),
                        sfreq = tidytable::replace_na(sfreq, 0),
                        state = case_when(sfreq > tfreq ~ 1,
                                          sfreq <= tfreq ~ 0)) %>% 
      # filter to state with greater than 30 lengths and fed with less
      tidytable::filter(tfreq < 30,
                        sfreq >= 30) %>% 
      tidytable::select(-sfreq, -tfreq) -> state_test 
    
    # join state test to fed lengths
    fsh_len_comp_f %>% 
      tidytable::left_join(state_test) %>% 
      tidytable::filter(is.na(state)) %>% 
      tidytable::left_join(fsh_len_f %>%
                             tidytable::summarise(nsamp = length(unique(haul_join)),
                                                  .by = c(year, trimester, area, gear))) %>% 
      tidytable::mutate(state = tidytable::replace_na(state, 0),
                        nsamp = tidytable::replace_na(nsamp, 0)) %>% 
      # bind state data where fed data doesn't exist
      tidytable::bind_rows(fsh_len_comp_s %>% 
                             tidytable::left_join(state_test) %>% 
                             tidytable::filter(!is.na(state)) %>% 
                             tidytable::left_join(fsh_len_s %>% 
                                                    tidytable::summarise(tot = sum(freq), .by = c(year, trimester, area, gear)) %>% 
                                                    tidytable::drop_na() %>% 
                                                    tidytable::mutate(nsamp = round(tot / 50)) %>% 
                                                    tidytable::select(-tot))) %>% 
      tidytable::summarise(prop = sum(prop),
                           nsamp = sum(nsamp),
                           .by = c(year, gear, length)) %>% 
      # format to ss3 data
      tidytable::pivot_wider(names_from = length, values_from = prop) -> fsh_len_comp
  } else{
    # append state data ----
    # get grid of all possible combos of federal year-gear-area-trimester-length
    tidytable::expand_grid(year = sort(unique(c(fsh_len_full_f$year, fsh_len_full_s$year))),
                           gear = unique(c(fsh_len_full_f$gear, fsh_len_full_s$gear)),
                           length = seq(1,117,1)) %>% 
      # join weighted fish length comp
      tidytable::left_join(fsh_len_full_f %>% 
                             # number of fish by trimester, area, gear and length
                             tidytable::summarise(freq = sum(freq),
                                                  .by = c(year, trimester, area, gear, length)) %>% 
                             # bind in state data
                             tidytable::bind_rows(fsh_len_full_s) %>% 
                             tidytable::summarise(freq = sum(freq),
                                                  .by = c(year, trimester, area, gear, length)) %>% 
                             # join observed catch
                             tidytable::left_join(fsh_len_full_f %>% 
                                                    # observed catch by trimester, area, gear
                                                    tidytable::summarise(numb = sum(numb),
                                                                         .by = c(year, trimester, area, gear))) %>% 
                             # compute total length obs by trimester, area, gear
                             tidytable::mutate(tfreq = sum(freq), .by = c(year, trimester, area, gear)) %>% 
                             # expand month length composition by observed catch
                             tidytable::mutate(comp = freq / tfreq * numb) %>% 
                             # join length comp with catch proportion
                             tidytable::left_join(catch_p %>% 
                                                    tidytable::summarise(catch_prop = sum(catch_prop), .by = c(year, trimester, area, gear))) %>%
                             # compute catch weighted length comp by year, trimester, area, and gear
                             tidytable::mutate(prop = comp * catch_prop) %>% 
                             # standardize length comp to year and gear
                             tidytable::summarise(prop = sum(prop), .by = c(year, gear, length)) %>% 
                             tidytable::mutate(prop_t = sum(prop), .by = c(year, gear)) %>% 
                             tidytable::mutate(len_comp = prop / prop_t) %>% 
                             tidytable::select(year, gear, length, prop = len_comp)) %>% 
      tidytable::mutate(prop = case_when(is.na(prop) ~ 0,
                                         !is.na(prop) ~ prop)) %>% 
      tidytable::left_join(fsh_len_f %>%
                             tidytable::summarise(nsamp_f = length(unique(haul_join)),
                                                  .by = c(year, gear)) %>% 
                             tidytable::full_join(fsh_len_s %>% 
                                                    tidytable::summarise(tot = sum(freq), .by = c(year, gear)) %>% 
                                                    tidytable::drop_na() %>% 
                                                    tidytable::mutate(nsamp_s = round(tot / 50)) %>% 
                                                    tidytable::select(-tot)) %>% 
                             tidytable::mutate(nsamp_f = tidytable::replace_na(nsamp_f, 0),
                                               nsamp_s = tidytable::replace_na(nsamp_s, 0)) %>% 
                             tidytable::mutate(nsamp = nsamp_f + nsamp_s) %>% 
                             tidytable::select(-nsamp_f, -nsamp_s)) %>% 
      # format to ss3 data
      tidytable::pivot_wider(names_from = length, values_from = prop) -> fsh_len_comp

  }

  fsh_len_comp
}
