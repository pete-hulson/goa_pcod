#' Get fishery catch age composition
#' Originally adapted/generalized from Steve Barbeaux' files for generating SS files for EBS/AI Greenland Turbot
#' Re-developed in 2024 by p. hulson from FISH_AGE_COMP.r file
#' 
#' @param new_year current assessment year
#' @param st_yr start year for age data (default = 2007)
#' @param max_age max age for age comps (i.e., plus group, default = 10)
#' @param fltr switch for whether to filter length samples (default = TRUE)
#' @param add_a1 option to define smallest lengths in data as age-1 (default = TRUE)
#' @param use_FSA switch for whether to use FSA package to compute age-length key or to do it manually (default = TRUE)
#' @param iters when using FSA, option to have replicates of age-length key (default = 1)
#' @param by_sex switch for whether to compute sex-specific comps (default = TRUE)
#' @param ss3_frmt whether to format comp data for ss3 data file (default = TRUE)
#' @param fit whether to fit age comps in model (default = FALSE)
#' 

get_fsh_age <- function(new_year = 9999,
                        st_yr = 2007,
                        max_age = 10,
                        fltr = TRUE,
                        add_a1 = TRUE,
                        use_FSA = TRUE,
                        iters = 1,
                        by_sex = TRUE,
                        ss3_frmt = TRUE,
                        fit = FALSE){
  
  # get data ----
  ## expanded length comps ----
  fsh_len_exp <- get_fsh_len4age(new_year,
                                  fltr,
                                  by_sex) %>% 
    tidytable::mutate(freq = prop * 10000) %>% 
    # filter to years post-2007 (as default)
    tidytable::filter(year > st_yr)
  
  ## age data ----
  fsh_age <- vroom::vroom(here::here(new_year, 'data', 'raw', 'fish_age_domestic.csv')) %>% 
    # filter to years post-2007 (as default)
    tidytable::filter(year > st_yr)

  # compute age comps ----
  ## using FSA package ----
  if(isTRUE(use_FSA)){
    ### sex-specific comps ----
    if(isTRUE(by_sex)){
      # prep age and length data
      fsh_age %>% 
        tidytable::mutate(age = tidytable::case_when(age > 20 ~ 20,
                                                     .default = age)) %>% 
        tidytable::select(year, gear, sex, age, tl = length) -> adata_alk
      
      fsh_len_exp %>% 
        tidytable::select(year, gear, sex, tl = length, freq) %>% 
        tidytable::mutate(freq = floor(freq)) %>% 
        tidytable::uncount(freq) %>% 
        tidytable::mutate(age = NA) %>% 
        tidytable::select(year, gear, sex, age, tl) -> ldata_alk
      
      # define smallest lengths as age 1, or not
      if(isTRUE(add_a1)){
        adata_alk %>% 
          tidytable::bind_rows(ldata_alk %>% 
                                 tidytable::summarise(tl = min(tl), .by = c(year, gear, sex)) %>% 
                                 tidytable::bind_rows(adata_alk %>% 
                                                        tidytable::summarise(tl = min(tl) - 1, .by = c(year, gear, sex))) %>% 
                                 tidytable::mutate(age = 1)) %>% 
          FSA::lencat(x = ~tl, data = ., startcat = 5, w = 5) -> adata_alk
      } else{
        adata_alk %>% 
          FSA::lencat(x = ~tl, data = ., startcat = 5, w = 5) -> adata_alk
      }
      
      # get ages for unobserved lengths with FSA package
      combos <- fsh_age %>% 
        tidytable::select(year, gear, sex) %>% 
        dplyr::distinct() %>% 
        tidytable::filter(sex != 'U')
      
      length_age_list <- purrr::map(1:iters, ~purrr::map(1:nrow(combos), ~FSA::alkIndivAge(prop.table(table(subset(adata_alk$LCat, 
                                                                                                              adata_alk$year == combos$year[.] & 
                                                                                                                adata_alk$gear == combos$gear[.] & 
                                                                                                                adata_alk$sex == combos$sex[.]),
                                                                                                       subset(adata_alk$age, 
                                                                                                              adata_alk$year == combos$year[.] & 
                                                                                                                adata_alk$gear == combos$gear[.] & 
                                                                                                                adata_alk$sex == combos$sex[.])), 
                                                                                                 margin = 1), 
                                                                                      formula = ~tl, 
                                                                                      data = subset(ldata_alk, 
                                                                                                    ldata_alk$year == combos$year[.] & 
                                                                                                      ldata_alk$gear == combos$gear[.] & 
                                                                                                      ldata_alk$sex == combos$sex[.]))),
                                    .progress = list(type = "iterator", 
                                                     format = "Resampling {cli::pb_bar} {cli::pb_percent}",
                                                     clear = TRUE))
      
      
      purrr::map(1:iters, ~tidytable::map_df(length_age_list[[.]], ~as.data.frame(.x), .id = "combo")) %>% 
        tidytable::map_df(., ~as.data.frame(.x), .id = "iter") %>% 
        select(iter, year, gear, sex, age, length = tl) %>% 
        tidytable::summarise(freq = .N, .by = c(iter, year, gear, age)) %>% 
        tidytable::summarise(freq = mean(freq), .by = c(year, gear, age)) -> length_age

      # get age comp for all combos of year-gear-age
      tidytable::expand_grid(year = sort(unique(fsh_age$year)),
                             gear = unique(fsh_age$gear),
                             age = seq(1, 20)) %>% 
        tidytable::left_join(length_age) %>% 
        tidytable::mutate(freq = tidytable::replace_na(freq, 0)) %>% 
        tidytable::mutate(total = sum(freq), .by = c(year, gear)) %>% 
        tidytable::mutate(agecomp = freq / total) %>% 
        tidytable::select(-total, -freq) %>% 
        tidytable::drop_na() -> fsh_acomp
    } else{
      ### sex-aggregated comps ----
      # prep age and length data
      fsh_age %>% 
        tidytable::mutate(age = tidytable::case_when(age > 20 ~ 20,
                                                     .default = age)) %>% 
        tidytable::select(year, gear, age, tl = length) -> adata_alk
      
      fsh_len_exp %>% 
        tidytable::select(year, gear, tl = length, freq) %>% 
        tidytable::mutate(freq = floor(freq)) %>% 
        tidytable::uncount(freq) %>% 
        tidytable::mutate(age = NA) %>% 
        tidytable::select(year, gear, age, tl) -> ldata_alk
      
      # define smallest lengths as age 1, or not
      if(isTRUE(add_a1)){
        adata_alk %>% 
          tidytable::bind_rows(ldata_alk %>% 
                                 tidytable::summarise(tl = min(tl), .by = c(year, gear)) %>% 
                                 tidytable::bind_rows(adata_alk %>% 
                                                        tidytable::summarise(tl = min(tl) - 1, .by = c(year, gear))) %>% 
                                 tidytable::mutate(age = 1)) %>% 
          FSA::lencat(x = ~tl, data = ., startcat = 5, w = 5) -> adata_alk
      } else{
        adata_alk %>% 
          FSA::lencat(x = ~tl, data = ., startcat = 5, w = 5) -> adata_alk
      }
      
      # get ages for unobserved lengths with FSA package
      combos <- fsh_age %>% 
        tidytable::select(year, gear) %>% 
        dplyr::distinct()
      
      length_age <- purrr::map(1:nrow(combos), ~FSA::alkIndivAge(prop.table(table(subset(adata_alk$LCat, 
                                                                                         adata_alk$year == combos$year[.] & 
                                                                                           adata_alk$gear == combos$gear[.]),
                                                                                  subset(adata_alk$age, 
                                                                                         adata_alk$year == combos$year[.] & 
                                                                                           adata_alk$gear == combos$gear[.])), 
                                                                            margin = 1), 
                                                                 formula = ~tl, 
                                                                 data = subset(ldata_alk, 
                                                                               ldata_alk$year == combos$year[.] & 
                                                                                 ldata_alk$gear == combos$gear[.]))) %>% 
        tidytable::map_df(., ~as.data.frame(.x), .id = "combo") %>% 
        select(year, gear, age, length = tl)
      
      # get age comp for all combos of year-gear-age
      tidytable::expand_grid(year = sort(unique(fsh_age$year)),
                             gear = unique(fsh_age$gear),
                             age = seq(1, 20)) %>% 
        tidytable::left_join(length_age %>% 
                               tidytable::summarise(freq = length(length), .by = c(year, gear, age))) %>% 
        tidytable::mutate(freq = tidytable::replace_na(freq, 0)) %>% 
        tidytable::mutate(total = sum(freq), .by = c(year, gear)) %>% 
        tidytable::mutate(agecomp = freq / total) %>% 
        tidytable::select(-total, -freq) %>% 
        tidytable::drop_na() -> fsh_acomp
    }
  } else{
    ## with age-length key (note: only for sex-combined comps) ----
    
    # get age comp for all combos of year-gear-age
    tidytable::expand_grid(year = sort(unique(fsh_age$year)),
                           gear = unique(fsh_age$gear),
                           age = seq(1, 20)) %>% 
      tidytable::left_join(fsh_age %>% 
                             tidytable::select(year, gear, length, age) %>% 
                             # compute age-length key
                             tidytable::summarise(count = .N, .by = c(year, gear, length, age)) %>% 
                             tidytable::mutate(tot = sum(count), .by = c(year, gear, length)) %>% 
                             tidytable::mutate(alk = count / tot) %>% 
                             tidytable::select(year, gear, length, age, alk) %>% 
                             # join length comps and compute expanded age comps
                             tidytable::left_join(fsh_len_exp %>% 
                                                    tidytable::select(year, gear, length, prop)) %>% 
                             tidytable::mutate(acomp = alk * prop) %>% 
                             tidytable::summarise(acomp1 = sum(acomp), .by = c(year, gear, age))) %>% 
      tidytable::mutate(acomp1 = tidytable::replace_na(acomp1, 0)) %>% 
      # standardize age comps
      tidytable::mutate(tot = sum(acomp1), .by = c(year, gear)) %>% 
      tidytable::mutate(agecomp = acomp1 / tot) %>% 
      tidytable::select(year, gear, age, agecomp) %>% 
      tidytable::drop_na() -> fsh_acomp
  }

  # format for ss3 if desired ----
  if(isTRUE(ss3_frmt)){
    # hard-wired in season, etc for ss3 in ss3_args c(seas, gender, part, ageerr, lgin_lo, lgin_hi)
    ss3_args = c(1, 0, 0, 1, -1, -1)
    # get input sample size as number of hauls or 200, whichever is smaller
    fsh_len_exp %>% 
      tidytable::summarise(nsamp = mean(nsamp), .by = c(year, gear)) %>% 
      tidytable::mutate(nsamp = case_when(nsamp > 200 ~ 200, .default = nsamp)) -> nsamp
    # format data
    fsh_acomp <- ss3_age_com_fsh(fsh_acomp, ss3_args, max_age, iss = TRUE, nsamp, fit)
  }

  fsh_acomp
  
}
