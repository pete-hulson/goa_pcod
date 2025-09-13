library(tidyverse)

index <- vroom::vroom(here::here('2025', 'rsch', 'spatial', 'data', 'index.csv'))

index %>% 
  dplyr::rename_all(tolower) %>% 
  tidytable::select(-'...1') %>% 
  tidytable::filter(model == 'Without Russian data',
                    region %in% c('EBS', 'NBS')) %>% 
  tidytable::summarise(est = sum(est),
                       lwr = sum(lwr),
                       upr = sum(upr),
                       .by = year) %>% 
  tidytable::mutate(region = 'EBS|NBS') %>% 
  tidytable::bind_rows(index %>% 
                         dplyr::rename_all(tolower) %>% 
                         tidytable::select(-'...1') %>% 
                         tidytable::filter(model == 'Without Russian data',
                                           region %in% c('GOA')) %>% 
                         tidytable::summarise(est = sum(est),
                                              lwr = sum(lwr),
                                              upr = sum(upr),
                                              .by = year) %>% 
                         tidytable::mutate(region = 'WGOA')) %>% 
  tidytable::bind_rows(index %>% 
                         dplyr::rename_all(tolower) %>% 
                         tidytable::select(-'...1') %>% 
                         tidytable::filter(model == 'Without Russian data',
                                           region %in% c('EBS', 'NBS', 'GOA')) %>% 
                         tidytable::summarise(est = sum(est),
                                              lwr = sum(lwr),
                                              upr = sum(upr),
                                              .by = year) %>% 
                         tidytable::mutate(region = 'EBS|NBS|WGOA')) %>% 
  tidytable::mutate(region = factor(region, levels = c('EBS|NBS|WGOA', 'EBS|NBS', 'WGOA'))) -> plot_dat1



index %>% 
  dplyr::rename_all(tolower) %>% 
  tidytable::select(-'...1') %>% 
  tidytable::filter(model == 'Without Russian data',
                    region %in% c('EBS', 'NBS')) %>% 
  tidytable::summarise(est_bs = sum(est),
                       .by = year) %>% 
  tidytable::left_join(index %>% 
                         dplyr::rename_all(tolower) %>% 
                         tidytable::select(-'...1') %>% 
                         tidytable::filter(model == 'Without Russian data',
                                           region %in% c('GOA')) %>% 
                         tidytable::summarise(est_wg = sum(est),
                                              .by = year)) %>% 
  tidytable::filter(year %in% c(2001, 2003, 2005, 2007, 2009, 2011, 2013, 2015, 2017, 2019, 2021, 2023)) %>% 
  tidytable::mutate(prop_wg = est_wg / est_bs) -> plot_dat2

ggplot(data = plot_dat1, aes(x = year, y = est)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr, fill = region), alpha = 0.1) +
  geom_line(aes(col = region)) +
  theme_bw(base_size = 14) +
  labs(x = "Year", y = "Survey Biomass (mt)", col = "Region", fill = "Region") +
  theme(plot.background = element_rect(fill = "transparent", colour = NA),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.key.width = unit(0.25, 'cm'),
        legend.position = "top")

# Save plot
ggsave(here::here("2025", "rsch", "spatial", 'figures', "index_plot.png"), width = 8, height = 5, dpi = 300, bg = "transparent")



ggplot(data = plot_dat2, aes(x = factor(year), y = prop_wg)) +
  geom_bar(stat = "identity", fill = 'steelblue') +
  theme_bw(base_size = 14) +
  labs(x = "Year", y = "Western gulf proportion of Bering") +
  geom_text(aes(label = paste0(round(prop_wg * 100, digits = 0), "%")), vjust = -0.5) +
  theme(plot.background = element_rect(fill = "transparent", colour = NA))

# Save plot
ggsave(here::here("2025", "rsch", "spatial", 'figures', "prop_plot.png"), width = 8, height = 5, dpi = 300, bg = 'transparent')





# test table
plot_dat2 %>% 
  tidytable::filter(year >= 2019) %>% 
  tidytable::mutate(prop = 0.3 * est_wg / est_bs,
                    move = '30%') %>% 
  tidytable::bind_rows(plot_dat2 %>% 
                         tidytable::filter(year >= 2019) %>% 
                         tidytable::mutate(prop = 0.4 * est_wg / est_bs,
                                           move = '40%')) %>% 
  tidytable::bind_rows(plot_dat2 %>% 
                         tidytable::filter(year >= 2019) %>% 
                         tidytable::mutate(prop = 0.5 * est_wg / est_bs,
                                           move = '50%')) %>% 
  tidytable::select(year, prop, move) %>% 
  tidytable::pivot_wider(names_from = move, values_from = prop) %>% 
  vroom::vroom_write(., here::here('2025', 'rsch', 'spatial', 'data', 'wgoa_move.csv'), delim = ',')


