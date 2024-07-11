#' Function to pull bottom trawl survey pop'n numbers for length composition
#' adapted/generalized from Steve Barbeaux' files for generating SS files for EBS/AI Greenland Turbot
#' Re-developed in 2024 by p. hulson to link to gap_products tables with dplyr::tbl()
#' 
#' @param new_year current assessment year
#' @param twl_srvy gap survey id number (default = 47 for goa)
#' @param species gap species code (default = 21720)
#' @param query switch for whether to run sql query for data (default = FALSE)
#' @param bins user defined length bins (min/max for the length bin, default = NULL)
#' @param ss3_frmt whether to format comp data for ss3 data file (default = TRUE)
#' @param iss test for whether input sample size time-dependent (TRUE) or constant (FALSE)
#' @param nsamp value or vector for input sample size (default = 100)
#' 

get_twl_srvy_lcomp <- function(new_year = 9999,
                               twl_srvy = 47,
                               species = 21720,
                               query = FALSE,
                               bins = NULL,
                               ss3_frmt = TRUE,
                               iss = FALSE,
                               nsamp = 100){
  
  # query data ----
  if(isTRUE(query)){
    
    # get connected to akfin
    db = 'akfin'
    conn = afscdata::connect(db)
    
    # query bottom trawl survey length pop'n data
    dplyr::tbl(conn, dplyr::sql('gap_products.akfin_sizecomp')) %>% 
      dplyr::rename_all(tolower) %>% 
      dplyr::select(year,
                    survey_definition_id,
                    area_id,
                    species_code,
                    length_mm,
                    sex,
                    population_count) %>% 
      dplyr::filter(year <= new_year,
                    survey_definition_id == twl_srvy,
                    species_code == species,
                    area_id %in% c(99903)) %>% 
      dplyr::select(year,
                    survey = survey_definition_id, 
                    strata = area_id, 
                    species_code,
                    length = length_mm, 
                    sex,
                    num = population_count) -> twl_q
    
    dplyr::collect(twl_q) %>% 
      vroom::vroom_write(., here::here(new_year, 'data', 'raw', 'twl_srvy_lpop.csv'), delim = ",")
    capture.output(dplyr::show_query(twl_q), 
                   file = here::here(new_year, "data", "sql", "twl_srvy_lpop_sql.txt"))

  }
  
  # compute comps ----
  ts_lpop <- vroom::vroom(here::here(new_year, 'data', 'raw', 'twl_srvy_lpop.csv'))
  
  # bin length comps to desired bin width
  ts_lpop %>% 
    tidytable::filter(length > 0) %>% 
    tidytable::summarise(popn = sum(num), .by = c(year, length)) %>% 
    get_bin(., bins) %>% 
    tidytable::select(-bin) -> ts_lpop_bin  

  # compute comps
  ts_lpop_bin %>% 
    tidytable::mutate(lencomp = popn / sum(popn), .by = year) %>% 
    tidytable::select(-popn) -> ts_lcomp

  # format for ss3 if desired ----
  if(isTRUE(ss3_frmt)){
    # hard-wired in season, etc for ss3 in ss3_args
    ss3_args = c(7, 4, 0, 0)
    ts_lcomp <- ss3_len_com(ts_lcomp,
                            ss3_args,
                            iss,
                            nsamp)
  }

  ts_lcomp
}


#' Function to pull longline survey length composition
#' adapted/generalized from Steve Barbeaux' files for generating SS files
#' Re-developed in 2024 by p. hulson to link to afscdata package
#' 
#' @param new_year current assessment year
#' @param area survey area (default = 'goa')
#' @param species gap species code (default = 21720)
#' @param query switch for whether to run sql query for data (default = FALSE)
#' @param bins user defined length bins (min/max for the length bin, default = NULL)
#' @param ss3_frmt whether to format comp data for ss3 data file (default = TRUE)
#' @param iss test for whether input sample size time-dependent (TRUE) or constant (FALSE)
#' @param nsamp value or vector for input sample size (default = 100)
#' 

get_ll_srvy_lcomp <- function(new_year = 9999,
                              area = 'goa',
                              species = 21720, 
                              query = FALSE,
                              bins = NULL,
                              ss3_frmt = TRUE,
                              iss = FALSE,
                              nsamp = 100){
  
  # query data ----
  if(isTRUE(query)){
    
    # get connected to akfin
    db = 'akfin'
    conn = afscdata::connect(db)
    
    # query longline survey data and write raw data to folder 
    afscdata::q_lls_rpn_length(year = new_year,
                               species = species,
                               area = area,
                               # by = 'geoarea',
                               # use_historical = FALSE,
                               db = conn)
  }
  
  
  # compute comps ----
  
  # read in longline survey data and filter to 1990 on
  lls_lpop <- vroom::vroom(here::here(new_year, "data", "raw", "lls_rpn_length_data.csv")) %>% 
    tidytable::filter(year >= 1990)
  
  
  # bin length comps to desired bin width
  lls_lpop %>% 
    tidytable::filter(length > 0) %>% 
    tidytable::summarise(popn = sum(rpn), .by = c(year, length)) %>% 
    get_bin(., bins) %>% 
    tidytable::select(-bin) -> lls_lpop_bin
  
  # compute comps
  lls_lpop_bin %>% 
    tidytable::mutate(lencomp = popn / sum(popn), .by = year) %>% 
    tidytable::select(-popn) -> lls_lcomp
  
  # format for ss3 if desired ----
  if(isTRUE(ss3_frmt)){
    # hard-wired in season, etc for ss3 in ss3_args
    ss3_args = c(7, 5, 0, 0)
    lls_lcomp <- ss3_len_com(lls_lcomp,
                             ss3_args,
                             iss,
                             nsamp)
  }
  
  lls_lcomp
}

