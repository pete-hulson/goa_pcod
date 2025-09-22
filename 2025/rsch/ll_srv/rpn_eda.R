library(tidyverse)

# get data ----
ll_cpue <- vroom::vroom(here::here('2025', 'rsch', 'll_srv', 'output', 'cpue_indx.csv'))
ll_rpn <- vroom::vroom(here::here('2025', 'rsch', 'll_srv', 'output', 'rpn_indx.csv'))
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
  tidytable::bind_rows(rpn_hist %>% 
                         tidytable::mutate(type = 'll_srv_orig')) %>% 
  tidytable::mutate(mean = mean(rpn), .by = c(region, type)) %>% 
  tidytable::mutate(z_index = (rpn - mean) / mean) %>% 
  tidytable::select(year, region, z_index, type) %>% 
  tidytable::bind_rows(gap_index %>% 
                         tidytable::select(year, num, area) %>% 
                         tidytable::mutate(mean = mean(num), .by = area) %>% 
                         tidytable::mutate(z_index = (num - mean) / mean) %>% 
                         tidytable::mutate(type = 'twl_srv',
                                           region = case_when(area == 'western' ~ 'WGOA',
                                                              area == 'central' ~ 'CGOA',
                                                              area == 'eastern' ~ 'EGOA',
                                                              area == 'goa' ~ 'GOA')) %>% 
                         tidytable::select(year, region, z_index, type)) %>% 
  tidytable::mutate(region = factor(region, levels = c('GOA', 'WGOA', 'CGOA', 'EGOA'))) -> plot_dat

ggplot(plot_dat, aes(x = year, y = z_index, col = type)) +
  geom_line() +
  geom_point() +
  facet_wrap(~region, scales = 'free_y', ncol = 1) +
  theme_bw() +
  ylab('Standardized index') +
  xlab('Year')

# Save plot
ggsave(here::here("2025", "rsch", "ll_srv", 'figures', "index_plot.png"), width = 16, height = 12, dpi = 300, bg = 'transparent')

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
  theme_bw() +
  ylab('Apportionment') +
  xlab('Year')

# Save plot
ggsave(here::here("2025", "rsch", "ll_srv", 'figures', "apport_plot.png"), width = 16, height = 12, dpi = 300, bg = 'transparent')

