


# get connected
db = 'afsc'
conn = afscdata::connect(db)  

dplyr::tbl(conn, dplyr::sql('obsint.current_haul')) %>% 
  dplyr::inner_join(dplyr::tbl(conn, dplyr::sql('obsint.current_spcomp')) %>% 
                      dplyr::filter(SPECIES == 202),
                    by = c('HAUL_JOIN')) %>% 
  dplyr::rename_all(tolower) %>% 
  dplyr::select(year = year.x,
                gear = gear_type,
                area = nmfs_area,
                sample_number,
                sample_size,
                sample_weight,
                weight,
                count,
                catch = official_total_catch,
                deployment_date,
                retrieval_date,
                hk_pots = total_hooks_pots,
                hday = haul_date.x) %>% 
  dplyr::filter(area >= 600,
                area <= 699,
                area != 670) -> akfin_obsc2

dplyr::collect(akfin_obsc2) %>% 
  dplyr::mutate(month = lubridate::month(hday),
                gear = dplyr::case_when(gear %in% c(1, 2, 3, 4) ~ 'trawl',
                                        gear == 6 ~ 'pot',
                                        gear %in% c(5, 7, 9, 10, 11, 68, 8) ~ 'longline'),
                dur_min = as.numeric(retrieval_date - deployment_date) / 60,
                cpue = dplyr::case_when(gear == 'trawl' ~ (sample_weight / sample_size * catch) / dur_min,
                                        gear %in% c('pot', 'longline') ~ ((weight / count) * (hk_pots * sample_number / sample_size)) / hk_pots)) %>%  
  dplyr::select(-c(deployment_date, retrieval_date)) %>% 
  dplyr::filter(year <= new_year,
                year >= 2015,
                area <= 630) -> cpue_dat

vroom::vroom_write(cpue_dat, here::here(new_year, "output", "flc_plots", 'cpue_dat.csv'), delim = ",")


em_dat <- vroom::vroom(here::here(new_year, 'data', 'raw', 'em_catch.csv'))

vroom::vroom(here::here(new_year, 'data', 'raw', 'em_catch.csv')) %>% 
  tidytable::filter(subarea %in% c('CG', 'WG'),
                    gear == 'Pot') %>% 
  tidytable::mutate(cpue = weight_kg / trip_sampled_hauls_pots,
                    ln_cpue = log(cpue),
                    month = lubridate::month(haul_date),
                    area = case_when(subarea == 'WG' ~ 'Western gulf',
                                     .default = 'Central gulf'),
                    gear = 'pot') %>% 
  tidytable::filter(month <= 4)%>% 
  tidytable::select(year, month, gear, area, cpue, ln_cpue)



cpue_dat %>% 
  tidytable::select(year, gear, area, month, cpue) %>% 
  tidytable::filter(month <= 4,
                    area <= 630) %>% 
  tidytable::mutate(ln_cpue = log(cpue),
                    area = case_when(area == 610 ~ 'Western gulf',
                                     .default = 'Central gulf')) -> plot_dat
  # tidytable::bind_rows(vroom::vroom(here::here(new_year, 'data', 'raw', 'em_catch.csv')) %>% 
  #                        tidytable::filter(subarea %in% c('CG', 'WG'),
  #                                          gear == 'Pot') %>% 
  #                        tidytable::mutate(cpue = weight_kg / trip_sampled_hauls_pots,
  #                                          ln_cpue = log(cpue),
  #                                          month = lubridate::month(haul_date),
  #                                          area = case_when(subarea == 'WG' ~ 'Western gulf',
  #                                                           .default = 'Central gulf'),
  #                                          gear = 'pot') %>% 
  #                        tidytable::filter(month <= 4)%>% 
  #                        tidytable::select(year, month, gear, area, cpue, ln_cpue)) -> plot_dat


ggplot(data = plot_dat, 
       aes(x = factor(year), y = ln_cpue, fill = factor(year))) +
  geom_boxplot(width = 0.5, alpha = 0.7, outliers = FALSE) +
  facet_grid(gear ~ area, scales = 'free_y') +
  theme_bw() +
  theme(legend.position = "none") +
  xlab('Year') +
  ylab('ln(Weight CPUE)') +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  scico::scale_fill_scico_d(palette = 'roma')



ggsave(filename = "cpue.png",
       path = here::here(new_year, "output", "flc_plots"),
       width = 6.5,
       height = 7,
       units = "in")
