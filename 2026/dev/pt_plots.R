library(r4ss)
library(tidyverse)

# Current assessment year
new_year <- as.numeric(format(Sys.Date(), format = "%Y"))
# recommended model name
rec_mdl <- "24.0"



# plot recent fishery length comps ----

len_bins <- c(4.5, 9.5, 14.5, 19.5, 24.5, 29.5, 34.5, 39.5, 44.5, 49.5, 54.5, 59.5, 64.5, 69.5, 74.5, 79.5, 84.5, 89.5, 94.5, 99.5, 104.5)

# source functions ----
source_files <- list.files(here::here(new_year, "R", "get_data"), "*.r$")
purrr::map(here::here(new_year, "R", "get_data", source_files), source)
source(here::here(new_year, "R", "utils.r"))

len_comp <- get_fsh_len_post91(new_year,
                               bins = len_bins,
                               ss3_frmt = FALSE)

ggplot(data = len_comp %>% tidytable::filter(year >= new_year - 1), 
       aes(x = length, y= lencomp, color = gear)) +
  # geom_bar(stat = "identity", position = "dodge") +
  geom_line() +
  geom_point() +
  facet_wrap(~year, ncol = 1) +
  theme_bw(base_size = 14) +
  scico::scale_color_scico_d(palette = 'roma') +
  labs(x = "Length (cm)", y = "Composition", color = "Fleet") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "top")

# save
ggsave(filename = "fish_len.png",
       path = here::here(new_year, "output", "safe_plots"),
       width = 6.5,
       height = 7,
       units = "in")


# plot 2025 caal data ----

fsh_age <- vroom::vroom(here::here(new_year, 'data', 'raw', 'fish_age_domestic.csv')) %>% 
  # filter to years post-2007 (as default)
  tidytable::filter(year >= 2007)
ts_age <- vroom::vroom(here::here(new_year, 'data', 'raw', 'twl_srvy_age.csv'), 
                       progress = FALSE, 
                       show_col_types = FALSE)

tidytable::expand_grid(year = sort(unique(fsh_age$year)),
                       gear = sort(unique(fsh_age$gear)),
                       length = len_bins) %>% 
  tidytable::left_join(fsh_age %>% 
                         tidytable::select(year, gear, length, age) %>% 
                         tidytable::left_join(get_bin(fsh_age %>% 
                                                        tidytable::distinct(length), len_bins)) %>% 
                         tidytable::select(year, gear, age, length = new_length)) %>% 
  tidytable::drop_na() %>%
  tidytable::mutate(age = tidytable::case_when(age > 10 ~ 10,
                                               .default = age),
                    period = case_when(year <= 2024 ~ "Historical",
                                       year == 2025 ~ "2025")) %>% 
  tidytable::summarise(count = .N, .by = c(period, gear, length, age)) %>% 
  tidytable::mutate(tot = sum(count), .by = c(period, gear, length)) %>% 
  tidytable::mutate(caal = round(count / tot, digits = 5)) %>% 
  tidytable::select(period, gear, length, age, caal) %>% 
  tidytable::bind_rows(tidytable::expand_grid(year = sort(unique(ts_age$year)),
                                              length = len_bins) %>% 
                         tidytable::left_join(ts_age %>% 
                                                tidytable::select(year, length, age) %>% 
                                                tidytable::left_join(get_bin(ts_age %>% 
                                                                               tidytable::distinct(length), len_bins)) %>% 
                                                tidytable::select(year, age, length = new_length)) %>% 
                         tidytable::drop_na() %>%
                         tidytable::mutate(age = tidytable::case_when(age > 10 ~ 10,
                                                                      .default = age),
                                           period = case_when(year <= 2024 ~ "Historical",
                                                              year == 2025 ~ "2025")) %>% 
                         tidytable::summarise(count = .N, .by = c(period, length, age)) %>% 
                         tidytable::mutate(tot = sum(count), .by = c(period, length)) %>% 
                         tidytable::mutate(caal = round(count / tot, digits = 5),
                                           gear = "Trawl Survey") %>% 
                         tidytable::select(period, gear, length, age, caal)) -> dat


ggplot(data = dat, aes(x = age, y = length, size = caal, color = period))+
  geom_point(alpha = 0.25) +
  facet_wrap(~gear) +
  theme_bw(base_size = 14) +
  scico::scale_color_scico_d(palette = 'roma') +
  labs(x = "Age", y = "Length (cm)", color = "Period", size = "CAAL") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "right")
  

# save
ggsave(filename = "caal_comp.png",
       path = here::here(new_year, "output", "safe_plots"),
       width = 7,
       height = 5,
       units = "in")

# plot adf&g survey ----

glm_res <- vroom::vroom(here::here(new_year, 'data', 'raw', 'adfg_srvy_glm.csv'))
adfg_res <- vroom::vroom(here::here(new_year, 'data', 'adfg_srvy_index.csv'))

glm_res %>% 
  tidytable::select(-se_wt, -se_num) %>% 
  tidytable::pivot_longer(cols = c(index_wt, index_num)) %>% 
  tidytable::mutate(type = case_when(name == "index_wt" ~ "Weight",
                                     name == "index_num" ~ "Numbers"),
                    est = "Model-based") %>% 
  tidytable::bind_rows(adfg_res %>% 
  tidytable::mutate(wt_cpue = total_weight_kg / area_swept_km2,
                    num_cpue = total_number_caught / area_swept_km2) %>% 
  tidytable::summarise(wt_cpue = mean(wt_cpue),
                       num_cpue = mean(num_cpue), .by = year) %>% 
  tidytable::pivot_longer(cols = c(wt_cpue, num_cpue)) %>% 
  tidytable::mutate(type = case_when(name == "wt_cpue" ~ "Weight",
                                     name == "num_cpue" ~ "Numbers"),
                    est = "Design-based")) %>% 
  tidytable::filter(year >= 2000) -> dat

ggplot(data = dat, aes(x = year, y = value, color = est)) +
  geom_point() +
  geom_line() +
  facet_wrap(~type, scales = "free_y", ncol = 1) +
  theme_bw(base_size = 14) +
  scico::scale_color_scico_d(palette = 'roma') +
  labs(x = "Year", y = "CPUE (kg/num per km2)", color = "Estimator") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "top")

# save
ggsave(filename = "adfg_comp.png",
       path = here::here(new_year, "output", "safe_plots"),
       width = 7,
       height = 5,
       units = "in")

