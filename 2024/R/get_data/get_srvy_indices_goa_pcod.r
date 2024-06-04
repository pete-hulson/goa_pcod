#' Function to pull bottom trawl survey adbundance indices (either numbers or biomass)
#' adapted/generalized from Steve Barbeaux' files for generating SS files for EBS/AI Greenland Turbot
#' Re-developed in 2024 by p. hulson to link to gap_products tables
#' 
#' @param new_year current assessment year
#' @param twl_srvy gap survey id number (default = 47 for goa)
#' @param species gap species code (default = 21720)
#' @param query switch for whether to run sql query for data (default = FALSE)
#' @param indx get index for either numbers 'num', or biomass 'biom' (default = NULL)
#' 

get_twl_srvy_index <- function(new_year = 9999,
                               twl_srvy = 47,
                               species = 21720,
                               query = FALSE,
                               indx = NULL){
  
  # query data ----
  if(isTRUE(query)){
    
    # get connected to akfin
    db = 'akfin'
    conn = afscdata::connect(db)
    
    # query bottom trawl survey index data
    dplyr::tbl(conn, dplyr::sql('gap_products.akfin_biomass')) %>% 
      dplyr::rename_all(tolower) %>% 
      dplyr::select(year,
                    survey_definition_id,
                    area_id,
                    species_code,
                    biomass_mt,
                    biomass_var,
                    population_count,
                    population_var) %>% 
      dplyr::filter(year <= new_year,
                    survey_definition_id == twl_srvy,
                    species_code == species,
                    area_id %in% c(803, 804, 805, 99903)) %>% 
      dplyr::select(year,
                    survey = survey_definition_id, 
                    strata = area_id, 
                    species_code,
                    biom = biomass_mt, 
                    biom_var = biomass_var,
                    num = population_count,
                    num_var = population_var) %>%  
      dplyr::mutate(area = case_when(strata == 803 ~ 'central',
                                     strata == 804 ~ 'eastern',
                                     strata == 805 ~ 'western',
                                     strata == 99903 ~ 'goa')) -> twl_q
    
    dplyr::collect(twl_q) %>% 
      vroom::vroom_write(., here::here(new_year, 'data', 'raw', 'twl_srvy_index.csv'), delim = ",")
    capture.output(dplyr::show_query(twl_q), 
                   file = here::here(new_year, "data", "sql", "twl_srvy_index_sql.txt"))

  }
  
  # format for ss3 ----
  ts_indx <- vroom::vroom(here::here(new_year, 'data', 'raw', 'twl_srvy_index.csv'))
  
  # get pop'n numbers index
  if(indx == 'num'){
    ts_indx %>% 
      tidytable::expand(year = min(ts_indx$year):max(ts_indx$year)) %>% 
      tidytable::left_join(ts_indx %>% 
                             tidytable::filter(area == 'goa') %>% 
                             tidytable::mutate(obs = num / 1000,
                                               se_log = sqrt(log(1 + (sqrt(num_var) / num) ^ 2))) %>% 
                             tidytable::select(year, obs, se_log)) %>% 
      tidytable::mutate(seas = 7,
                        obs = replace_na(obs, 1),
                        se_log = replace_na(se_log, 1)) %>% 
      tidytable::mutate(index = case_when(obs == 1 ~ -4,
                                          obs > 1 ~ 4)) %>% 
      tidytable::select(year, seas, index, obs, se_log)
  } else{.
    # get biomass index
    ts_indx %>% 
      tidytable::expand(year = min(ts_indx$year):max(ts_indx$year)) %>% 
      tidytable::left_join(ts_indx %>% 
                             tidytable::filter(area == 'goa') %>% 
                             tidytable::mutate(obs = biom,
                                               se_log = sqrt(log(1 + (sqrt(biom_var) / biom) ^ 2))) %>% 
                             tidytable::select(year, obs, se_log)) %>% 
      tidytable::mutate(seas = 7,
                        obs = replace_na(obs, 1),
                        se_log = replace_na(se_log, 1)) %>% 
      tidytable::mutate(index = case_when(obs == 1 ~ -4,
                                          obs > 1 ~ 4)) %>% 
      tidytable::select(year, seas, index, obs, se_log)
  }

}

#' Function to pull longline survey abundance indices (either by number, rpn, or by weight, rpw)
#' adapted/generalized from Steve Barbeaux' files for generating SS files
#' Re-developed in 2024 by p. hulson to link to afscdata package
#' 
#' @param new_year current assessment year
#' @param area survey area (default = 'goa')
#' @param species gap species code (default = 21720)
#' @param query switch for whether to run sql query for data (default = FALSE)
#' @param indx get index for either numbers 'num', or weight 'wt' (default = NULL)
#' 

get_ll_srvy_index <- function(new_year = 9999,
                              area = 'goa',
                              species = 21720, 
                              query = FALSE,
                              indx = NULL){
  
  # query data ----
  if(isTRUE(query)){
    
    # get connected to akfin
    db = 'akfin'
    conn = afscdata::connect(db)
    
    # query longline survey data and write raw data to folder 
    afscdata::q_lls_rpn(year = new_year,
                        species = species,
                        area = area,
                        by = 'geoarea',
                        use_historical = FALSE,
                        db = conn)
  }
  
    # format for ss3 ----
    
  # read in longline survey data and filter to 1990 on
    lls_indx <- vroom::vroom(here::here(new_year, "data", "raw", "lls_rpn_geoarea_data.csv")) %>% 
      tidytable::filter(year >= 1990)
  
    # get rpn index
    if(indx == 'num'){
      lls_indx %>% 
        tidytable::expand(year = min(lls_indx$year):max(lls_indx$year)) %>% 
        tidytable::left_join(lls_indx %>% 
                               tidytable::summarise(obs = sum(rpn, na.rm = TRUE),
                                                    rpn_var = sum(rpn_var, na.rm = TRUE),
                                                    .by = c(year)) %>% 
                               tidytable::mutate(se_log = sqrt(log(1 + (sqrt(rpn_var) / obs) ^ 2))) %>% 
                               tidytable::select(year, obs, se_log)) %>% 
        tidytable::mutate(seas = 7,
                          obs = replace_na(obs, 1),
                          se_log = replace_na(se_log, 1)) %>% 
        tidytable::mutate(index = case_when(obs == 1 ~ -5,
                                            obs > 1 ~ 5)) %>% 
        tidytable::select(year, seas, index, obs, se_log)
    } else{
      # get rpw index
      lls_indx %>% 
        tidytable::expand(year = min(lls_indx$year):max(lls_indx$year)) %>% 
        tidytable::left_join(lls_indx %>% 
                               tidytable::summarise(obs = sum(rpw, na.rm = TRUE),
                                                    rpw_var = sum(rpw_var, na.rm = TRUE),
                                                    .by = c(year)) %>% 
                               tidytable::mutate(se_log = sqrt(log(1 + (sqrt(rpw_var) / obs) ^ 2))) %>% 
                               tidytable::select(year, obs, se_log)) %>% 
        tidytable::mutate(seas = 7,
                          obs = replace_na(obs, 1),
                          se_log = replace_na(se_log, 1)) %>% 
        tidytable::mutate(index = case_when(obs == 1 ~ -5,
                                            obs > 1 ~ 5)) %>% 
        tidytable::select(year, seas, index, obs, se_log)
    }

}


#' Function to pull iphc longline survey abundance index
#' adapted/generalized from Steve Barbeaux' files for generating SS files
#' Re-developed in 2024 by p. hulson
#' 
#' @param new_year current assessment year
#' @param query switch for whether to run sql query for data (default = FALSE)
#' 

get_iphc_srvy_index <- function(new_year = 9999,
                                query = FALSE){
  
  # query data ----
  if(isTRUE(query)){
    
    # get connected to akfin
    db = 'akfin'
    conn = afscdata::connect(db)
    
    # query iphc longline survey index and write raw data to folder 
    
    dplyr::tbl(conn, dplyr::sql('afsc_host.fiss_rpn')) %>% 
      dplyr::rename_all(tolower) %>% 
      dplyr::filter(species %in% c('Pacific cod'),
                    fmp_sub_area %in% c("CGOA", "EY/SE", "WGOA", "WY")) %>% 
      dplyr::select(year = survey_year, 
                    fmp_sub_area, 
                    species, 
                    strata = rpn_strata, 
                    strata_rpn, 
                    boot_sd, 
                    boot_bias,
                    n_stations,
                    n_pos_catch) -> iphc_q
    
    dplyr::collect(iphc_q) %>% 
      vroom::vroom_write(., here::here(new_year, 'data', 'raw', 'iphc_srvy_index.csv'), delim = ",")
    capture.output(dplyr::show_query(iphc_q), 
                   file = here::here(new_year, "data", "sql", "iphc_srvy_index_sql.txt"))

  }
  
  # format for ss3 ----
  
  # read in iphc longline survey data
  iphc_indx <- vroom::vroom(here::here(new_year, "data", "raw", "iphc_srvy_index.csv"))
  
  # get iphc rpn index
  # note: can not reproduce original cvs as obtained from s. barbeaux in 2021 by bootstrap
  iphc_indx %>% 
    tidytable::summarise(obs = sum(strata_rpn),
                         se_log = sqrt(log(1 + sqrt(sum(boot_sd ^ 2)) / sum(strata_rpn)) ^ 2),
                         .by = c(year)) %>%
    tidytable::mutate(seas = 7, index = -6) %>%
    tidytable::select(year, seas, index, obs, se_log)
  
}


#' Function to get adf&g trawl survey abundance index
#' adapted/generalized from Steve Barbeaux' files for generating SS files
#' Re-developed in 2024 by p. hulson
#' 
#' @param new_year current assessment year
#' @param run_glm switch for whether to run delta glm model (default = FALSE)
#' 

get_adfg_srvy_index <- function(new_year = 9999,
                                run_glm = FALSE){

  
  # run delta glm model ----
  if(isTRUE(run_glm)){
    # get model
    dglm = dget(here::here(new_year, 'data', "delta_glm_1-7-2.get"))
    
    # get model data together
    vroom::vroom(here::here(new_year, 'data', 'raw', 'adfg_srvy_index.csv')) %>% 
      tidytable::filter(district %in% c(1,2,6),
                        !is.na(avg_depth_fm),
                        !is.na(start_longitude)) %>% 
      tidytable::mutate(depth = case_when(avg_depth_fm <= 30 ~ 1,
                                          avg_depth_fm > 30 & avg_depth_fm <= 70 ~ 2,
                                          avg_depth_fm > 70 ~ 3),
                        density = total_weight_kg / area_km2) %>% 
      tidytable::select(density, year, district, depth) -> mydata
    
    # run model
    codout = dglm(mydata, dist = "lognormal", write = F, J = T)
    
    # write model results
    codout$deltaGLM.index %>% 
      mutate(year = as.numeric(rownames(.))) %>% 
      as_tibble(.) %>% 
      vroom::vroom_write(., here::here(new_year, 'data', 'raw', 'adfg_srvy_glm.csv'), delim = ",")
  }
  
  # format for ss3 ----
      
  # read in adf&g glm model results
  adfg_indx <- vroom::vroom(here::here(new_year, "data", "raw", "adfg_srvy_glm.csv"))
  
  # get adf&g index
  adfg_indx %>% 
    tidytable::rename(obs = index) %>% 
    tidytable::mutate(se_log = sqrt(log(1 + jack.se / obs) ^ 2),
                      seas = 7, 
                      index = -7) %>% 
    tidytable::select(year, seas, index, obs, se_log)

}
