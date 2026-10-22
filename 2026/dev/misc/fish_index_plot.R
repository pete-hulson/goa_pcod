
# get connected
db = 'akfin'
conn = afscdata::connect(db)  

# Current assessment year
new_year <- as.numeric(format(Sys.Date(), format = "%Y"))

# query shallow water fishery data
dplyr::tbl(conn, dplyr::sql('norpac.debriefed_haul_mv')) %>% 
dplyr::filter(TRIP_TARGET_NAME != "Pacific Cod",
              FMP_AREA == "GOA",
              BOTTOM_DEPTH_FATHOMS < 165) %>% 
  dplyr::inner_join(dplyr::tbl(conn, dplyr::sql('norpac.debriefed_spcomp_mv')),
                    by = c('HAUL_JOIN')) %>% 
  dplyr::rename_all(tolower) %>% 
  dplyr::select(year = year.x,
                species,
                gear = gear_type,
                gear_desc = gear_description,
                weight_kg = extrapolated_weight,
                area = nmfs_area,
                target = trip_target_name,
                bottom_depth_f = bottom_depth_fathoms,
                haul_date = haul_date.x,
                permit = permit.x,
                haul = haul.x) %>% 
  dplyr::filter(area > 609,
                area < 650,
                year > 2007,
                gear_desc != "PELAGIC",
                target %in% c("Arrowtooth Flounder", "Halibut", "Pollock - bottom", "Rockfish", "Sablefish", "Shallow Water Flatfish - GOA")) %>% 
  dplyr::collect() -> swf_catch

# plot shallow water fishery catch of pcod
# pcod catch by target
swf_catch %>% 
  tidytable::summarise(cod = sum(weight_kg[species == 202], na.rm = TRUE),
                       tot = sum(weight_kg, na.rm = TRUE),
                       .by = c(year, target)) %>% 
  tidytable::mutate(pcod = cod / tot) %>% 
  tidytable::mutate(type = "Proportion of Pcod catch in shallow water fisheries") %>% 
  tidytable::select(year, target, pcod, type) %>% 
  tidytable::filter(year >= 2006) -> plot_dat

# number of observed hauls
swf_catch %>% 
  tidytable::mutate(test_haul = as.numeric(paste0(lubridate::month(haul_date), lubridate::day(haul_date), permit, haul))) %>% 
  tidytable::distinct(test_haul, .by = c(target, year)) %>% 
  tidytable::rename(target = .by1, year = .by2) %>% 
  tidytable::summarise(num_hauls = .N, .by = c(year, target)) -> trip_plot

# standardized pcod catch
plot_dat %>% 
  tidytable::mutate(mean = mean(pcod),
                    sd = sd(pcod), .by = c(target)) %>% 
  tidytable::mutate(index = (pcod - mean) / sd) -> index_plot
  
  # composite index (standardized weighted by number of trips)
index_plot %>% 
  tidytable::left_join(trip_plot) %>% 
  tidytable::summarise(index = sum(index * num_hauls) / sum(num_hauls), 
                       .by = c(year, type)) -> comp_plot


# plot by target
ggplot(plot_dat, aes(year, pcod, col = target)) +
  geom_line(linetype = 2) +
  geom_point() +
  facet_wrap( ~ target, ncol = 2, scale = "free_y", labeller = labeller(type = label_wrap_gen(30))) +
  scico::scale_color_scico_d(palette = 'roma') +
  labs(x = "Year", y = "Proportion of Pacific cod catch by target fishery") +
  theme_bw(base_size = 14) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(vjust = 0.5, angle = 90),
        legend.position = "none") +
  scale_x_continuous(breaks = c(2008:new_year), limits = c(2008, new_year))

ggsave(filename = "prop_by_fishery.png",
       path = here::here(new_year, "dev", "misc", "fish_indx_plots"),
       width = 6.5,
       height = 7,
       units = "in")

# plot number of hauls observed
ggplot(trip_plot, aes(year, num_hauls, fill = target)) +
  geom_bar(stat = "identity") +
  facet_wrap( ~ target, ncol = 2, labeller = labeller(type = label_wrap_gen(30))) +
  scico::scale_fill_scico_d(palette = 'roma') +
  labs(x = "Year", y = "Number of observed hauls", color = "Index") +
  theme_bw(base_size = 14) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(vjust = 0.5, angle = 90),
        legend.position = "none") +
  scale_x_continuous(breaks = 2008:new_year, limits = c(2007.5, new_year + 0.5))

ggsave(filename = "hauls_by_fishery.png",
       path = here::here(new_year, "dev", "misc", "fish_indx_plots"),
       width = 6.5,
       height = 7,
       units = "in")

# plot standardized index
ggplot(index_plot, aes(year, index, col = target)) +
  geom_line(linetype = 2) +
  geom_point() +
  geom_hline(yintercept = 0) +
  facet_wrap( ~ target, ncol = 2, scale = "free_y", labeller = labeller(type = label_wrap_gen(30))) +
  scico::scale_color_scico_d(palette = 'roma') +
  labs(x = "Year", y = "Pacific cod auxiliary indices", color = "Index") +
  theme_bw(base_size = 14) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(vjust = 0.5, angle = 90),
        legend.position = "none") +
  scale_x_continuous(breaks = c(2008:new_year), limits = c(2008, new_year))

ggsave(filename = "z_prop_by_fishery.png",
       path = here::here(new_year, "dev", "misc", "fish_indx_plots"),
       width = 6.5,
       height = 7,
       units = "in")

# composit index plot
ggplot(comp_plot, aes(year, index)) +
  geom_line(linetype = 2) +
  geom_point() +
  scico::scale_color_scico_d(palette = 'roma') +
  labs(x = "Year", y = "Index of proportion of Pcod catch in shallow water fisheries") +
  theme_bw(base_size = 14) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(vjust = 0.5, angle = 90),
        legend.position = "none") +
  scale_x_continuous(breaks = c(2008:new_year), limits = c(2008, new_year))


ggsave(filename = "comp_index.png",
       path = here::here(new_year, "dev", "misc", "fish_indx_plots"),
       width = 7,
       height = 7,
       units = "in")




