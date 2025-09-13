library(tidyverse)

# get data ----
gap_strata <- vroom::vroom(here::here('2025', 'rsch', 'll_srv', 'gap_strata.csv'))
ll_catch <- vroom::vroom(here::here('2025', 'rsch', 'll_srv', 'lls_pcod_catch.csv'))
ll_rpn <- vroom::vroom(here::here('2025', 'data', 'raw', 'lls_rpn_geoarea_data.csv'))

# combined strata areas for depths > 300 m ----
gap_strata %>% 
  tidytable::filter(depth_min_m < 301) %>% 
  tidytable::bind_rows(gap_strata %>% 
                        tidytable::filter(depth_min_m >= 301) %>% 
                        tidytable::summarise(area = sum(area), .by = subreg) %>% 
                        tidytable::mutate(depth_min_m = 301,
                                          depth_max_m = 1000)) %>% 
  tidytable::mutate(stratum_description = paste0(depth_min_m, "-", depth_max_m, "m"),
                    stratum_description = case_when(stratum_description == '1-100m' ~ '0-100m',
                                                    .default = stratum_description)) %>% 
  tidytable::select(subreg, stratum_description, area) -> area_size


# define functions ----

# define function to calc cpue
calc_cpue <- function(data){
  # get subregion cpue
  data %>% 
    tidytable::mutate(catch_freq = replace_na(catch_freq, 0),
                      cpue = catch_freq / (45 - ineffective),
                      subreg = case_when(subarea == 'Central Gulf of Alaska' ~ 'CGOA',
                                         subarea == 'Western Gulf of Alaska' ~ 'WGOA',
                                         subarea == 'West Yakutat' ~ 'EGOA',
                                         subarea == 'East Yakutat/Southeast' ~ 'EGOA')) %>% 
    tidytable::summarize(mean_cpue = mean(cpue, na.rm = TRUE),
                         n_skate = .N, 
                         .by = c(year, subreg)) %>% 
    # get total cpue
    tidytable::bind_rows(data %>% 
                           tidytable::mutate(catch_freq = replace_na(catch_freq, 0),
                                             cpue = catch_freq / (45 - ineffective)) %>% 
                           tidytable::summarize(mean_cpue = mean(cpue, na.rm = TRUE),
                                                n_skate = .N, 
                                                .by = year)  %>% 
                           tidytable::mutate(subreg = 'GOA')) -> cpue
  #output
  cpue
}

# define function to bootstrap cpue
boot_cpue <- function(data){
  # resample skates within subregion
  data %>% 
    tidytable::mutate(id = paste0(year, "-", vessel_number, "-", station_number, "-", hachi),
                      subreg = case_when(subarea == 'Central Gulf of Alaska' ~ 'CGOA',
                                         subarea == 'Western Gulf of Alaska' ~ 'WGOA',
                                         subarea == 'West Yakutat' ~ 'EGOA',
                                         subarea == 'East Yakutat/Southeast' ~ 'EGOA')) %>% 
    tidytable::select(year, subreg, id) %>% 
    tidytable::mutate(id = base::sample(id, .N, replace = TRUE), .by = c(year, subreg)) %>% 
    tidytable::separate(id, c('year', 'vessel_number', 'station_number', "hachi"), sep = '-', convert = TRUE) %>% 
    tidytable::left_join(data) -> data_resamp
  # compute the cpue
  cpue <- calc_cpue(data_resamp)
  # output
  list(cpue = cpue)
}

# define function to calc rpn
calc_rpn <- function(data, area_size){
  # compute strata cpue
  data %>% 
    tidytable::mutate(catch_freq = replace_na(catch_freq, 0),
                      cpue = catch_freq / (45 - ineffective),
                      subreg = case_when(subarea == 'Central Gulf of Alaska' ~ 'CGOA',
                                         subarea == 'Western Gulf of Alaska' ~ 'WGOA',
                                         subarea == 'West Yakutat' ~ 'EGOA',
                                         subarea == 'East Yakutat/Southeast' ~ 'EGOA'),
                      stratum_description = case_when(stratum >= 4 ~ '301-1000m',
                                                      .default = stratum_description)) %>% 
    tidytable::summarize(mean_cpue = mean(cpue, na.rm = TRUE),
                         n_skate = .N, 
                         .by = c(year, subreg, stratum_description)) -> cpue_stratum
  # get subregion rpn
  cpue_stratum %>% 
    tidytable::left_join(area_size) %>% 
    tidytable::mutate(rpn = mean_cpue * area) %>% 
    tidytable::summarise(rpn = sum(rpn), .by = c(year, subreg)) %>% 
    # get total rpn
    tidytable::bind_rows(cpue_stratum %>% 
                           tidytable::left_join(area_size) %>% 
                           tidytable::mutate(rpn = mean_cpue * area) %>% 
                           tidytable::summarise(rpn = sum(rpn), .by = year) %>% 
                           tidytable::mutate(subreg = 'GOA')) %>% 
    tidytable::rename(region = subreg) -> rpn
  rpn
}

# define function to bootstrap rpn
boot_rpn <- function(data, area_size){
  # resample skates within subregion and depth strata
  data %>% 
    tidytable::mutate(catch_freq = replace_na(catch_freq, 0),
                      subreg = case_when(subarea == 'Central Gulf of Alaska' ~ 'CGOA',
                                         subarea == 'Western Gulf of Alaska' ~ 'WGOA',
                                         subarea == 'West Yakutat' ~ 'EGOA',
                                         subarea == 'East Yakutat/Southeast' ~ 'EGOA'))  %>% 
    tidytable::mutate(id = paste0(year, "-", vessel_number, "-", station_number, "-", hachi)) %>% 
    tidytable::select(year, subreg, stratum, id) %>% 
    tidytable::mutate(id = base::sample(id, .N, replace = TRUE), .by = c(year, subreg, stratum)) %>% 
    tidytable::separate(id, c('year', 'vessel_number', 'station_number', "hachi"), sep = '-', convert = TRUE) %>% 
    tidytable::left_join(data) -> data_resamp
  # compute the rpn
  rpn <- calc_rpn(data_resamp, area_size)
  # output
  list(rpn = rpn)
}

# define number of iterations for bootstrap ----
iters = 10

# compute cpue index ----

# calc cpue
cpue <- calc_cpue(ll_catch) 

# bootstrap for uncertainty
rr <- purrr::map(1:iters, ~ boot_cpue(ll_catch),
                 .progress = list(type = "iterator", 
                                  format = "Resampling {cli::pb_bar} {cli::pb_percent}",
                                  clear = TRUE))
cpue_indx <- cpue %>% 
  tidytable::left_join(do.call(mapply, c(list, rr, SIMPLIFY = FALSE))$cpue %>% 
                         tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
                         tidytable::summarise(sd = sd(mean_cpue),
                                              lci = quantile(mean_cpue, probs = 0.025),
                                              uci = quantile(mean_cpue, probs = 0.975),
                                              .by = c(year, subreg))) %>% 
  vroom::vroom_write(., here::here('2025', 'rsch', 'll_srv', 'output', 'cpue_indx.csv'), delim = ',')



# compute rpn index ---

# calc rpn
rpn <- calc_rpn(ll_catch, area_size)

# bootstrap for uncertainty
rr <- purrr::map(1:iters, ~ boot_rpn(ll_catch, area_size),
                 .progress = list(type = "iterator", 
                                  format = "Resampling {cli::pb_bar} {cli::pb_percent}",
                                  clear = TRUE))
rpn_indx <- rpn %>% 
  tidytable::left_join(do.call(mapply, c(list, rr, SIMPLIFY = FALSE))$rpn %>% 
                         tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
                         tidytable::summarise(sd = sd(rpn),
                                              lci = quantile(rpn, probs = 0.025),
                                              uci = quantile(rpn, probs = 0.975),
                                              .by = c(year, region))) %>% 
  vroom::vroom_write(., here::here('2025', 'rsch', 'll_srv', 'output', 'rpn_indx.csv'), delim = ',')















# compute historical
rpn_hist <- ll_rpn %>% 
  tidytable::summarise(rpn = sum(rpn), .by = c(year, council_management_area)) %>% 
  tidytable::mutate(region = case_when(council_management_area == 'Central Gulf of Alaska' ~ 'CGOA',
                                       council_management_area == 'Western Gulf of Alaska' ~ 'WGOA',
                                       council_management_area == 'Eastern Gulf of Alaska' ~ 'EGOA')) %>% 
  tidytable::select(year, region, rpn) %>% 
  tidytable::bind_rows(ll_rpn %>% 
                         tidytable::summarise(rpn = sum(rpn), .by = c(year)) %>% 
                         tidytable::mutate(region = 'GOA'))

plot_dat <- rpn %>% 
  tidytable::mutate(type = 'new') %>% 
  tidytable::bind_rows(rpn_hist %>% 
                         tidytable::mutate(type = 'orig'))


ggplot(plot_dat, aes(x = year, y = rpn, col = type)) +
  geom_line() +
  geom_point() +
  facet_grid(type~region, scales = 'free_y')





ggplot(cpue, aes(x = year, y = mean_cpue, col = subreg)) +
  geom_line() +
  geom_point()

