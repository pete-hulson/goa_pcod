# Function to pull bottom trawl survey pop'n numbers for length composition
# adapted/generalized from Steve Barbeaux' files for generating SS files for EBS/AI Greenland Turbot
# Re-developed in 2024 by p. hulson to link to gap_products tables
#' @param new_year current assessment year
#' @param twl_srvy gap survey id number (default = 47 for goa)
#' @param species gap species code (default = 21720)
#' @param query switch for whether to run sql query for data (default = FALSE)
#' @param bins user defined length bins (min/max for the length bin, default = NULL)
#' @param iss test for whether input sample size time-dependent (TRUE) or constant (FALSE)
#' @param nsamp value or vector for input sample size (default = 100)
#' 
#' @return
#' @export get_twl_srvy_lcomp
#' 
#' @examples
#' 

get_twl_srvy_lcomp <- function(new_year = 9999,
                               twl_srvy = 47,
                               species = 21720,
                               query = FALSE,
                               bins = NULL,
                               iss = FALSE,
                               nsamp = 100){
  
  # query data ----
  if(isTRUE(query)){
    
    ## Open up data base connections
    db_specs <- vroom::vroom(here::here(new_year, "database_specs.csv"))
    akfin_user = db_specs$username[db_specs$database == "AKFIN"]
    akfin_pass = db_specs$password[db_specs$database == "AKFIN"]
    database = 'akfin'
    
    conn = DBI::dbConnect(odbc::odbc(),
                          database,
                          UID = akfin_user,
                          PWD = akfin_pass)
    
    # query bottom trawl survey index data
    q_twl_srvy = readLines(here::here(new_year, 'inst', 'sql', 'twl_srvy_lpop.sql'))
    q_twl_srvy = sql_filter(sql_precode = "IN", x = twl_srvy, sql_code = q_twl_srvy, flag = '-- insert survey')
    q_twl_srvy = sql_filter(sql_precode = "IN", x = species, sql_code = q_twl_srvy, flag = '-- insert species')
    
    sql_run(conn, q_twl_srvy) %>% 
      dplyr::rename_all(tolower) %>% 
      tidytable::filter(year <= new_year) %>% 
      vroom::vroom_write(., here::here(new_year, 'data', 'raw', 'twl_srvy_lpop.csv'), delim = ",")
    
  }
  
  # format for ss3 ----
  ts_lpop <- vroom::vroom(here::here(new_year, 'data', 'raw', 'twl_srvy_lpop.csv'))
  
  # bin length comps to desired bin width
  ts_lpop %>% 
    tidytable::filter(length > 0) %>% 
    tidytable::summarise(popn = sum(num), .by = c(year, length)) %>% 
    get_bin(., bins) %>% 
    tidytable::select(year, bin, popn) -> ts_lpop_bin
  
  # compute comps
  ts_lpop_bin %>% 
    tidytable::expand(bin = 1:max(bin),
                      year) %>% 
    tidytable::full_join(ts_lpop_bin) %>% 
    tidytable::mutate(popn = replace_na(popn, 0)) %>% 
    tidytable::summarise(popn = sum(popn), .by = c(year, bin)) %>% 
    tidytable::mutate(lencomp = popn / sum(popn), .by = year) %>% 
    tidytable::select(-popn) %>% 
    tidytable::arrange(bin) %>% 
    # hard-wired in season, etc for ss3
    tidytable::mutate(seas = 7,
                      fltsrv = 4,
                      gender = 0,
                      part = 0) -> lcomp_part
  
  # test if input sample size constant or read-in (e.g., from surveyISS package)
  if(isTRUE(iss)){
    lcomp_part %>% 
      tidytable::left_join(nsamp) %>% 
      tidytable::pivot_wider(names_from = bin, values_from = lencomp)
  } else{
    lcomp_part %>% 
      tidytable::mutate(nsamp = nsamp) %>% 
      tidytable::pivot_wider(names_from = bin, values_from = lencomp)
  }
  
}


# Function to pull longline survey length composition
# adapted/generalized from Steve Barbeaux' files for generating SS files
# Re-developed in 2024 by p. hulson to link to afscdata package
#' @param new_year current assessment year
#' @param area survey area (default = 'goa')
#' @param species gap species code (default = 21720)
#' @param query switch for whether to run sql query for data (default = FALSE)
#' @param bins user defined length bins (min/max for the length bin, default = NULL)
#' @param iss test for whether input sample size time-dependent (TRUE) or constant (FALSE)
#' @param nsamp value or vector for input sample size (default = 100)
#' 
#' @return
#' @export get_ll_srvy_lcomp
#' 
#' @examples
#' 

get_ll_srvy_lcomp <- function(new_year = 9999,
                              area = 'goa',
                              species = 21720, 
                              query = FALSE,
                              bins = NULL,
                              iss = FALSE,
                              nsamp = 100){
  
  # query data ----
  if(isTRUE(query)){
    
    ## Open up data base connections
    db_specs <- vroom::vroom(here::here(new_year, "database_specs.csv"))
    akfin_user = db_specs$username[db_specs$database == "AKFIN"]
    akfin_pass = db_specs$password[db_specs$database == "AKFIN"]
    database = 'akfin'
    
    conn = DBI::dbConnect(odbc::odbc(),
                          database,
                          UID = akfin_user,
                          PWD = akfin_pass)
    
    # query longline survey data and write raw data to folder 
    afscdata::q_lls_rpn_length(year = new_year,
                               species = species,
                               area = area,
                               # by = 'geoarea',
                               # use_historical = FALSE,
                               db = conn)
  }
  
  
  # format for ss3 ----
  
  # read in longline survey data and filter to 1990 on
  lls_lpop <- vroom::vroom(here::here(new_year, "data", "raw", "lls_rpn_length_data.csv")) %>% 
    tidytable::filter(year >= 1990)
  
  
  # bin length comps to desired bin width
  lls_lpop %>% 
    tidytable::filter(length > 0) %>% 
    tidytable::summarise(popn = sum(rpn), .by = c(year, length)) %>% 
    get_bin(., bins) %>% 
    tidytable::select(year, bin, popn) -> lls_lpop_bin
  
  # compute comps
  lls_lpop_bin %>% 
    tidytable::expand(bin = 1:max(bin),
                      year) %>% 
    tidytable::full_join(lls_lpop_bin) %>% 
    tidytable::mutate(popn = replace_na(popn, 0)) %>% 
    tidytable::summarise(popn = sum(popn), .by = c(year, bin)) %>% 
    tidytable::mutate(lencomp = popn / sum(popn), .by = year) %>% 
    tidytable::select(-popn) %>% 
    tidytable::arrange(bin) %>% 
    # hard-wired in season, etc for ss3
    tidytable::mutate(seas = 7,
                      fltsrv = 5,
                      gender = 0,
                      part = 0) -> lcomp_part
  
  # test if input sample size constant or read-in (e.g., from surveyISS package)
  if(isTRUE(iss)){
    lcomp_part %>% 
      tidytable::left_join(nsamp) %>% 
      tidytable::pivot_wider(names_from = bin, values_from = lencomp)
  } else{
    lcomp_part %>% 
      tidytable::mutate(nsamp = nsamp) %>% 
      tidytable::pivot_wider(names_from = bin, values_from = lencomp)
  }

}

