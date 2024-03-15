# adapted/generalized from Steve Barbeaux' files for generating SS files for EBS/AI Greenland Turbot
# Re-developed in 2024 by p. hulson to link to gap_products tables
#' @param new_year current assessment year
#' @param twl_srvy gap survey id number (default = 47 for goa)
#' @param species gap species code (default = 21720)
#' @param query switch for whether to run sql query for data (default = FALSE)
#' @param indx get index for either numbers 'num', or biomass 'biom' (default = NULL)
#' 
#' @return
#' @export get_twl_srvy_index
#' 
#' @examples
#' 

get_twl_srvy_index <- function(new_year = 9999,
                               twl_srvy = 47,
                               species = 21720,
                               query = FALSE,
                               indx = NULL){
  
  # query data ----
  if(isTRUE(query)){
    
    ## Open up data base connections
    db_specs <- vroom::vroom(here::here(new_dat_year, "database_specs.csv"))
    akfin_user = db_specs$username[db_specs$database == "AKFIN"]
    akfin_pass = db_specs$password[db_specs$database == "AKFIN"]
    database = 'akfin'
    
    conn = DBI::dbConnect(odbc::odbc(),
                          database,
                          UID = akfin_user,
                          PWD = akfin_pass)
    
    # query bottom trawl survey index data

    q_twl_srvy = readLines(here::here(new_year, 'inst', 'sql', 'twl_srvy_index.sql'))
    q_twl_srvy = sql_filter(sql_precode = "IN", x = twl_srvy, sql_code = q_twl_srvy, flag = '-- insert survey')
    q_twl_srvy = sql_filter(sql_precode = "IN", x = species, sql_code = q_twl_srvy, flag = '-- insert species')

    sql_run(conn, q_twl_srvy) %>% 
      dplyr::rename_all(tolower) %>% 
      tidytable::filter(year <= new_year) %>% 
      tidytable::mutate(area = case_when(strata == 803 ~ 'central',
                                         strata == 804 ~ 'eastern',
                                         strata == 805 ~ 'western',
                                         strata == 99903 ~ 'goa')) %>% 
      tidytable::select(year, area, biom, biom_var, num, num_var) %>% 
      vroom::vroom_write(., here::here(new_year, 'data', 'raw', 'twl_srvy_index.csv'), delim = ",")
    
  }
  
  # format for ss3 ----
  ts_indx <- vroom::vroom(here::here(new_year, 'data', 'raw', 'twl_srvy_index.csv'))
  
  # get index by type (either numbers or biomass)
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
  } else{
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


