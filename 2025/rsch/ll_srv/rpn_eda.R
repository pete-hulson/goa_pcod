library(tidyverse)

# get data ----
ll_cpue <- vroom::vroom(here::here('2025', 'rsch', 'll_srv', 'output', 'cpue_indx.csv'))
ll_rpn <- vroom::vroom(here::here('2025', 'rsch', 'll_srv', 'output', 'rpn_indx.csv'))
ll_rpn_25 <- vroom::vroom(here::here('2025', 'rsch', 'll_srv', 'output', 'rpn_indx_25.csv'))
ll_rpn_og <- vroom::vroom(here::here('2025', 'data', 'raw', 'lls_rpn_geoarea_data.csv'))

gap_cpue <- vroom::vroom(here::here('2025', 'rsch', 'll_srv', 'data', 'gap_cpue.csv'))
gap_index <- vroom::vroom(here::here('2025', 'data', 'raw', 'twl_srvy_index.csv'))
gap_strata <- vroom::vroom(here::here('2025', 'rsch', 'll_srv', 'data', 'gap_strata.csv'))

# compute historical
rpn_hist <- ll_rpn_og %>% 
  tidytable::summarise(rpn = sum(rpn),
                       sd = sqrt(sum(rpn_var)),
                       .by = c(year, council_management_area)) %>% 
  tidytable::mutate(region = case_when(council_management_area == 'Central Gulf of Alaska' ~ 'CGOA',
                                       council_management_area == 'Western Gulf of Alaska' ~ 'WGOA',
                                       council_management_area == 'Eastern Gulf of Alaska' ~ 'EGOA'),
                    lci = rpn - 1.96 * sd,
                    uci = rpn + 1.96 * sd) %>% 
  tidytable::select(year, region, rpn, sd, lci, uci) %>% 
  tidytable::bind_rows(ll_rpn_og %>% 
                         tidytable::summarise(rpn = sum(rpn),
                                              sd = sqrt(sum(rpn_var)),
                                              .by = c(year)) %>% 
                         tidytable::mutate(region = 'GOA',
                                           lci = rpn - 1.96 * sd,
                                           uci = rpn + 1.96 * sd))

# plot_dat <- ll_rpn %>% 
#   tidytable::mutate(type = 'new') %>% 
#   tidytable::bind_rows(rpn_hist %>% 
#                          tidytable::mutate(type = 'orig')) %>% 
#   tidytable::mutate(region = factor(region, levels = c('GOA', 'WGOA', 'CGOA', 'EGOA'))) %>% 
#   vroom::vroom_write(., here::here('2025', 'rsch', 'll_srv', 'output', 'rpn_indx_comparison.csv'), delim = ',')

# compare new vs old standardized rpns ----
ll_rpn %>% 
  tidytable::mutate(type = 'll_srv_new') %>% 
  tidytable::filter(stratum_description == '101-1000m') %>% 
  tidytable::select(-c(stratum_description, n_skate, n_pos_c)) %>% 
  tidytable::bind_rows(ll_rpn_25 %>% 
                         tidytable::mutate(type = 'll_srv_new_25only') %>% 
                         tidytable::filter(stratum_description == '101-1000m') %>% 
                         tidytable::select(-c(stratum_description, n_skate, n_pos_c))) %>% 
  tidytable::bind_rows(rpn_hist %>% 
                         tidytable::mutate(type = 'll_srv_orig')) %>% 
  tidytable::mutate(mean = mean(rpn), .by = c(region, type)) %>% 
  tidytable::mutate(z_index = (rpn - mean) / mean) %>% 
  tidytable::select(year, region, index = rpn, z_index, type) %>% 
  tidytable::bind_rows(gap_index %>% 
                         tidytable::select(year, num, area) %>% 
                         tidytable::mutate(mean = mean(num), .by = area) %>% 
                         tidytable::mutate(z_index = (num - mean) / mean) %>% 
                         tidytable::mutate(type = 'twl_srv',
                                           region = case_when(area == 'western' ~ 'WGOA',
                                                              area == 'central' ~ 'CGOA',
                                                              area == 'eastern' ~ 'EGOA',
                                                              area == 'goa' ~ 'GOA')) %>% 
                         tidytable::select(year, region, index = num, z_index, type)) %>% 
  tidytable::mutate(region = factor(region, levels = c('GOA', 'WGOA', 'CGOA', 'EGOA'))) -> plot_dat

ggplot(plot_dat, aes(x = year, y = z_index, col = type)) +
  geom_line() +
  geom_point() +
  facet_wrap(~region, scales = 'free_y', ncol = 1) +
  theme_bw(base_size = 14) +
  ylab('Standardized index') +
  xlab('Year')

# Save plot
ggsave(here::here("2025", "rsch", "ll_srv", 'figures', "index_plot.png"), width = 16, height = 12, dpi = 300, bg = 'transparent')

# plot on ly using 25 stations
ggplot(plot_dat %>% tidytable::filter(type %in% c('ll_srv_new', 'll_srv_new_25only')), aes(x = year, y = index, col = type)) +
  geom_line() +
  geom_point() +
  facet_wrap(~region, scales = 'free_y', ncol = 1) +
  theme_bw(base_size = 14) +
  ylab('RPN index') +
  xlab('Year')
# Save plot
ggsave(here::here("2025", "rsch", "ll_srv", 'figures', "index_plot_25only.png"), width = 16, height = 12, dpi = 300, bg = 'transparent')


# compare ll and trawl survey apportionment ----
ll_rpn %>% 
  tidytable::filter(region != 'GOA',
                    stratum_description == '101-1000m') %>% 
  tidytable::left_join(ll_rpn %>% 
                         tidytable::filter(region == 'GOA',
                                           stratum_description == '101-1000m') %>% 
                         tidytable::select(year, rpn) %>% 
                         tidytable::rename(rpn_goa = rpn)) %>% 
  tidytable::mutate(apport = rpn / rpn_goa,
                    type = 'll_srv_new') %>% 
  tidytable::select(year, region, apport, type) %>% 
  tidytable::bind_rows(ll_rpn_25 %>% 
                         tidytable::filter(region != 'GOA',
                                           stratum_description == '101-1000m') %>% 
                         tidytable::left_join(ll_rpn_25 %>% 
                                                tidytable::filter(region == 'GOA',
                                                                  stratum_description == '101-1000m') %>% 
                                                tidytable::select(year, rpn) %>% 
                                                tidytable::rename(rpn_goa = rpn)) %>% 
                         tidytable::mutate(apport = rpn / rpn_goa,
                                           type = 'll_srv_new_25only') %>% 
                         tidytable::select(year, region, apport, type)) %>% 
  tidytable::bind_rows(ll_rpn_og %>% 
                         tidytable::summarise(rpn = sum(rpn),
                                              .by = c(year, council_management_area)) %>% 
                         tidytable::mutate(region = case_when(council_management_area == 'Central Gulf of Alaska' ~ 'CGOA',
                                                              council_management_area == 'Western Gulf of Alaska' ~ 'WGOA',
                                                              council_management_area == 'Eastern Gulf of Alaska' ~ 'EGOA')) %>% 
                         tidytable::select(year, region, rpn) %>% 
                         tidytable::left_join(ll_rpn_og %>% 
                                                tidytable::summarise(rpn_goa = sum(rpn),
                                                                     .by = c(year))) %>% 
                         tidytable::mutate(apport = rpn / rpn_goa,
                                           type = 'll_srv_old') %>% 
                         tidytable::select(year, region, apport, type)) %>% 
  tidytable::bind_rows(gap_index %>% 
                         tidytable::filter(area != 'goa') %>% 
                         tidytable::select(year, num, area) %>% 
                         tidytable::left_join(gap_index %>% 
                                                tidytable::filter(area == 'goa') %>% 
                                                tidytable::select(year, num) %>% 
                                                tidytable::rename(num_goa = num)) %>% 
                         tidytable::mutate(apport = num / num_goa,
                                           type = 'twl_srv',
                                           region = case_when(area == 'western' ~ 'WGOA',
                                                              area == 'central' ~ 'CGOA',
                                                              area == 'eastern' ~ 'EGOA')) %>% 
                         tidytable::select(year, region, apport, type)) %>% 
  tidytable::mutate(region = factor(region, levels = c('WGOA', 'CGOA', 'EGOA'))) -> apport


ggplot(apport, aes(x = year, y = apport, col = type)) +
  geom_line() +
  geom_point() +
  facet_wrap(~region, scales = 'free_y', ncol = 1) +
  theme_bw(base_size = 14) +
  ylab('Apportionment') +
  xlab('Year')

# Save plot
ggsave(here::here("2025", "rsch", "ll_srv", 'figures', "apport_plot.png"), width = 16, height = 12, dpi = 300, bg = 'transparent')

# look at skates per depth w or w/o cod catch ----
ll_catch <- vroom::vroom(here::here('2025', 'rsch', 'll_srv', 'data', 'lls_pcod_catch.csv'))


cod_c_freq <- ll_catch %>% 
  tidytable::mutate(catch_freq = replace_na(catch_freq, 0)) %>% 
  tidytable::filter(stratum_description2 %in% c('101-150m', '151-200m', '201-300m')) %>% 
  tidytable::summarize(n_skates = .N, .by = c(year, stratum_description2)) %>% 
  tidytable::left_join(ll_catch %>% 
                         tidytable::mutate(catch_freq = replace_na(catch_freq, 0)) %>% 
                         tidytable::filter(stratum_description2 %in% c('101-150m', '151-200m', '201-300m'),
                                           catch_freq > 0) %>% 
                         tidytable::summarize(n_cod_skates = .N, .by = c(year, stratum_description2))) %>% 
  tidytable::mutate(prop_cod = n_cod_skates / n_skates) %>% 
  vroom::vroom_write(., here::here('2025', 'rsch', 'll_srv', 'output', 'cod_c_freq.csv'), delim = ',')


ggplot(cod_c_freq, aes(x = year, y = prop_cod, fill = stratum_description2)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~stratum_description2, scales = 'free_y', ncol = 1) +
  theme_bw(base_size = 14) +
  ylab('Proprtion of skates with cod') +
  xlab('Year') +
  theme(legend.position = "none")
  
# Save plot
ggsave(here::here("2025", "rsch", "ll_srv", 'figures', "cod_c_freq.png"), width = 16, height = 12, dpi = 300, bg = 'transparent')



