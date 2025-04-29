#' Function to pull bottom trawl survey adbundance indices (either numbers or biomass)
#' adapted/generalized from Steve Barbeaux' files for generating SS files for EBS/AI Greenland Turbot
#' Re-developed in 2024 by p. hulson to link to gap_products tables
#' 
#' @param new_year current assessment year
#' @param indx get index for either numbers 'num', or biomass 'biom' (default = NULL)
#' 

get_twl_srvy_index <- function(new_year = 9999,
                               indx = NULL){
  
  # format for ss3 ----
  ts_indx <- vroom::vroom(here::here(new_year, 'data', 'raw', 'twl_srvy_index.csv'))
  
  # get pop'n numbers index
  if(indx == 'num'){
    data.frame(ts_indx %>% 
                 tidytable::filter(area == 'goa') %>% 
                 tidytable::mutate(obs = num / 1000,
                                   se_log = sqrt(log(1 + (sqrt(num_var) / num) ^ 2))) %>% 
                 tidytable::select(year, obs, se_log) %>% 
                 tidytable::mutate(seas = 7,
                                   index = 4) %>% 
                 tidytable::select(year, seas, index, obs, se_log))
  } else{
    # get biomass index
    data.frame(ts_indx %>% 
                 tidytable::filter(area == 'goa') %>% 
                 tidytable::mutate(obs = biom,
                                   se_log = sqrt(log(1 + (sqrt(biom_var) / biom) ^ 2))) %>% 
                 tidytable::select(year, obs, se_log) %>% 
                 tidytable::mutate(seas = 7,
                                   index = 4) %>% 
                 tidytable::select(year, seas, index, obs, se_log))
  }
}

#' Function to pull longline survey abundance indices (either by number, rpn, or by weight, rpw)
#' adapted/generalized from Steve Barbeaux' files for generating SS files
#' Re-developed in 2024 by p. hulson to link to afscdata package
#' 
#' @param new_year current assessment year
#' @param indx get index for either numbers 'num', or weight 'wt' (default = NULL)
#' 

get_ll_srvy_index <- function(new_year = 9999,
                              indx = NULL){
  
  # format for ss3 ----
  
  # read in longline survey data and filter to 1990 on
  lls_indx <- vroom::vroom(here::here(new_year, "data", "raw", "lls_rpn_geoarea_data.csv")) %>% 
    tidytable::filter(year >= 1990)
  
  # get rpn index
  if(indx == 'num'){
    data.frame(lls_indx %>% 
                 tidytable::summarise(obs = sum(rpn, na.rm = TRUE),
                                      rpn_var = sum(rpn_var, na.rm = TRUE),
                                      .by = c(year)) %>% 
                 tidytable::mutate(se_log = sqrt(log(1 + (sqrt(rpn_var) / obs) ^ 2))) %>% 
                 tidytable::select(year, obs, se_log) %>% 
                 tidytable::mutate(seas = 7,
                                   index = 5) %>% 
                 tidytable::select(year, seas, index, obs, se_log))
  } else{
    # get rpw index
    data.frame(lls_indx %>% 
                 tidytable::summarise(obs = sum(rpw, na.rm = TRUE),
                                      rpw_var = sum(rpw_var, na.rm = TRUE),
                                      .by = c(year)) %>% 
                 tidytable::mutate(se_log = sqrt(log(1 + (sqrt(rpw_var) / obs) ^ 2))) %>% 
                 tidytable::select(year, obs, se_log) %>% 
                 tidytable::mutate(seas = 7,
                                   index = 5) %>% 
                 tidytable::select(year, seas, index, obs, se_log))
  }
  
}


#' Function to pull iphc longline survey abundance index
#' adapted/generalized from Steve Barbeaux' files for generating SS files
#' Re-developed in 2024 by p. hulson
#' 
#' @param new_year current assessment year
#' 

get_iphc_srvy_index <- function(new_year = 9999){
  
  # format for ss3 ----
  
  # read in iphc longline survey data
  iphc_indx <- vroom::vroom(here::here(new_year, "data", "raw", "iphc_srvy_index.csv"))
  
  # get iphc rpn index
  # note: can not reproduce original cvs as obtained from s. barbeaux in 2021 by bootstrap
  data.frame(iphc_indx %>% 
               tidytable::summarise(obs = sum(strata_rpn),
                                    se_log = sqrt(log(1 + sqrt(sum(boot_sd ^ 2)) / sum(strata_rpn)) ^ 2),
                                    .by = c(year)) %>%
               tidytable::mutate(seas = 7, 
                                 index = -6) %>%
               tidytable::select(year, seas, index, obs, se_log))
  
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
    vroom::vroom(here::here(new_year, 'data', 'adfg_srvy_index.csv')) %>% 
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
  data.frame(adfg_indx %>% 
               tidytable::rename(obs = index) %>% 
               tidytable::mutate(se_log = sqrt(log(1 + jack.se / obs) ^ 2),
                                 seas = 7, 
                                 index = -7) %>% 
               tidytable::select(year, seas, index, obs, se_log))
  
}
