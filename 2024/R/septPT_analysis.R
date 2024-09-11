
# TMB:::install.contrib("https://github.com/vtrijoulet/OSA_multivariate_dists/archive/main.zip")
# remotes::install_github("fishfollower/compResidual/compResidual", INSTALL_opts=c("--no-multiarch"), force = TRUE)

# test fishery length comps
libs <- c("r4ss",
          "RODBC",
          "DBI",
          "dplyr",
          "data.table",
          "FSA",
          "lubridate",
          "tidyr",
          "afscdata",
          "purrr",
          "tidyverse",
          "tidytable",
          "vroom",
          "here",
          "compResidual")

if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )]) > 0) {
  install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE)])
}

lapply(libs, library, character.only = TRUE)

# Current assessment year
new_year <- as.numeric(format(Sys.Date(), format = "%Y"))

# source functions ----
source_files <- list.files(here::here(new_year, "R", "get_data"), "*.r$")
purrr::map(here::here(new_year, "R", "get_data", source_files), source)
source_files <- list.files(here::here(new_year, "R", "assessment"), "*.r$")
purrr::map(here::here(new_year, "R", "assessment", source_files), source)
source(here::here(new_year, "R", "utils.r"))


# fishery comp filtering analysis ----

vroom::vroom(here::here(new_year, 'data', 'raw', 'fish_lfreq_domestic.csv')) %>% 
  # filter to years post-1991
  tidytable::filter(year >= 1991) %>% 
  # unique cruise-permit-haul description
  tidytable::mutate(haul1 = paste(cruise, permit, haul, sep = "_")) %>% 
  tidytable::summarise(tfreq = sum(freq), .by = c(year, gear, haul1)) %>% 
  tidytable::summarise(mufreq = mean(tfreq), .by = c(year, gear)) %>% 
  tidytable::pivot_wider(names_from = gear, values_from = mufreq) %>% 
  vroom::vroom_write(., here::here(new_year, 'output', 'fed_mean_hls.csv'), delim = ",")

## federal > 10 per haul ----
fsh_len_f <- vroom::vroom(here::here(new_year, 'data', 'raw', 'fish_lfreq_domestic.csv')) %>% 
  # filter to years post-1991
  tidytable::filter(year >= 1991) %>% 
  # unique cruise-permit-haul description
  tidytable::mutate(haul1 = paste(cruise, permit, haul, sep = "_"))

fsh_len_f %>%  
  tidytable::summarise(tot_hls = length(unique(haul1)), .by = c(year, gear)) %>% 
  tidytable::left_join(fsh_len_f %>% 
                         tidytable::summarise(tfreq = sum(freq), .by = c(year, gear, haul1)) %>% 
                         tidytable::filter(tfreq < 10) %>% 
                         tidytable::summarise(fltrd_hls = length(tfreq), .by = c(year, gear))) %>% 
  tidytable::mutate(prop_fltrd = round(100 * fltrd_hls / tot_hls, digits = 1)) %>% 
  tidytable::select(year, gear, prop_fltrd) %>% 
  tidytable::mutate(prop_fltrd = tidytable::replace_na(prop_fltrd, 0)) %>% 
  tidytable::pivot_wider(names_from = gear, values_from = prop_fltrd) %>% 
  vroom::vroom_write(., here::here(new_year, 'output', 'fed_fltrd_hls.csv'), delim = ",")

## state has > 30 (step 1) ----
fsh_len_s <- vroom::vroom(here::here(new_year, 'data', 'fish_lfreq_state.csv')) %>% 
  dplyr::rename_all(tolower) %>% 
  #filter to positive lengths
  tidytable::filter(length > 0) %>% 
  # define area, gear, plus length, trimester
  tidytable::mutate(area = trunc(area / 10) * 10, # truncate area to nearest 10 (e.g., 649 becomes 640)
                    gear1 = tidytable::case_when(gear == 91 ~ 'pot',
                                                 gear %in% c(5, 26, 61) ~ 'longline',
                                                 .default = 'trawl'), # define gears
                    length = tidytable::case_when(length > 116 ~ 117,
                                                  .default = length), # set plus length to 117 cm
                    trimester = tidytable::case_when(month %in% seq(5, 8) ~ 2,
                                                     month >= 9 ~ 3,
                                                     .default = 1)) %>% 
  tidytable::select(year, area, gear = gear1, month, trimester, quarter, sex, length, freq)

fsh_len_s %>% 
  tidytable::summarise(tfreq = sum(freq), .by = c(year, area, gear)) %>% 
  tidytable::mutate(fltrd = case_when(tfreq < 30 ~ 1,
                                      .default = 0)) %>% 
  tidytable::summarise(prop_fltrd = sum(fltrd) / length(tfreq), .by = c(year, gear)) %>% 
  tidytable::pivot_wider(names_from = gear, values_from = prop_fltrd) %>% 
  vroom::vroom_write(., here::here(new_year, 'output', 'st_fltrd_s1.csv'), delim = ",")

## state removed (step 2) ----
fsh_len_f %>% 
  # get federal number of length obs by trimester-area-gear
  tidytable::summarise(tfreq = sum(freq), .by = c(year, trimester, area, gear)) %>% 
  # get state number of length obs by trimester-area-gear
  tidytable::full_join(fsh_len_s %>% 
                         tidytable::summarise(sfreq = sum(freq), .by = c(year, trimester, area, gear))) %>% 
  tidytable::drop_na(sfreq) %>% 
  tidytable::mutate(fltrd = case_when(is.na(tfreq) ~ 0,
                                      .default = 1)) %>% 
  tidytable::summarise(prop_fltrd = sum(fltrd) / length(sfreq), .by = c(year, gear)) %>% 
  tidytable::pivot_wider(names_from = gear, values_from = prop_fltrd) %>% 
  vroom::vroom_write(., here::here(new_year, 'output', 'st_fltrd_s2.csv'), delim = ",")




# fishery length comps ----
# length bins to use for length comp data
  bin_width <- 1
  min_size <- 0.5
  max_size <- 105.5  # less than 1% of the fish in each year are 105 cm or larger (max less than 0.6%)
  len_bins <- seq(min_size, max_size, bin_width)
  len_bins2 <- seq(min_size, max_size, 2)
  len_bins5 <- seq(min_size, max_size, 5)
  

# get old way of doing comps
lcomp_old <- get_fsh_len_post91(new_year,
                                fltr = TRUE,
                                bins = len_bins,
                                ss3_frmt = FALSE)

# old way with no filter
lcomp_old_fltr <- get_fsh_len_post91(new_year,
                                     fltr = FALSE,
                                     bins = len_bins,
                                     ss3_frmt = FALSE)

# get new way of doing comps
lcomp_new <- get_fsh_len_post91_new(new_year,
                                    bins = len_bins,
                                    ss3_frmt = FALSE,
                                    time = 'month') %>% 
  tidytable::rename(new_lencomp = lencomp)


# new way with new bins
# 2 cm
lcomp_new_bin2 <- get_fsh_len_post91_new(new_year,
                                         bins = len_bins2,
                                         ss3_frmt = FALSE,
                                         time = 'month') %>% 
  tidytable::rename(bin_lencomp = lencomp)
# 5 cm
lcomp_new_bin5 <- get_fsh_len_post91_new(new_year,
                                         bins = len_bins5,
                                         ss3_frmt = FALSE,
                                         time = 'month') %>% 
  tidytable::rename(bin_lencomp = lencomp)

## compare old vs new ----
### aggregated plot ----
lcomp_old %>% 
  tidytable::left_join(lcomp_new) %>% 
  tidytable::mutate(length = ceiling(length)) %>% 
  tidytable::summarise(lencomp = sum(lencomp),
                       new_lencomp = sum(new_lencomp),
                       .by = c(gear, length)) %>% 
  tidytable::mutate(tot = sum(lencomp),
                    tot_new = sum(new_lencomp),
                    .by = gear) %>% 
  tidytable::mutate(lcomp_old = lencomp / tot,
                    lcomp_new = new_lencomp / tot_new) %>% 
  tidytable::select(gear, length, lcomp_old, lcomp_new) %>% 
  tidytable::pivot_longer(cols = c(lcomp_old, lcomp_new)) -> dat

ggplot(data = dat, aes(x = as.numeric(length), y = value, group = name)) +
  geom_line(aes(color = name))  +
  geom_point(aes(color = name)) +
  geom_area(aes(fill = name),
            alpha = 0.3777,
            position = 'identity') +
  theme_bw(base_size = 14) +
  theme(legend.position = "top") +
  facet_wrap(~ gear, nrow = 3,
             strip.position = 'top') +
  labs(y = "Aggregated length composition", x = "Length (cm)", color = "", fill = "") +
  scale_color_manual(values = c('blue', 'green')) +
  scale_fill_manual(values = c('blue', 'green')) -> agg_plot

suppressWarnings(ggplot2::ggsave(agg_plot,
                                 file = here::here(new_year, "plots", 'other','lcomp_compare_agg.png'),
                                 width = 12, height = 7, unit = 'in', dpi = 520))

### annual plot (trawl) ----
lcomp_old %>% 
  tidytable::left_join(lcomp_new) %>% 
  tidytable::mutate(length = ceiling(length)) %>% 
  tidytable::filter(gear == 'trawl') %>% 
  tidytable::select(year, gear, length, lcomp_old = lencomp, lcomp_new = new_lencomp) %>% 
  tidytable::pivot_longer(cols = c(lcomp_old, lcomp_new)) %>% 
  tidytable::filter(year <= 2023) -> dat

ggplot(data = dat, aes(x = as.numeric(length), y = value, group = name)) +
  geom_line(aes(color = name))  +
  geom_point(aes(color = name), size = 0.5) +
  geom_area(aes(fill = name),
            alpha = 0.3777,
            position = 'identity') +
  theme_bw(base_size = 14) +
  theme(legend.position = "top") +
  facet_wrap( ~ year) +
  labs(y = "Trawl length composition", x = "Length (cm)", color = "", fill = "") +
  scale_color_manual(values = c('blue', 'green')) +
  scale_fill_manual(values = c('blue', 'green')) -> trawl

suppressWarnings(ggplot2::ggsave(trawl,
                                 file = here::here(new_year, "plots", 'other','lcomp_compare_twl.png'),
                                 width = 7, height = 7, unit = 'in', dpi = 520))


### annual plot (longline) ----
lcomp_old %>% 
  tidytable::left_join(lcomp_new) %>% 
  tidytable::mutate(length = ceiling(length)) %>% 
  tidytable::filter(gear == 'longline') %>% 
  tidytable::select(year, gear, length, lcomp_old = lencomp, lcomp_new = new_lencomp) %>% 
  tidytable::pivot_longer(cols = c(lcomp_old, lcomp_new)) %>% 
  tidytable::filter(year <= 2023) -> dat

ggplot(data = dat, aes(x = as.numeric(length), y = value, group = name)) +
  geom_line(aes(color = name))  +
  geom_point(aes(color = name), size = 0.5) +
  geom_area(aes(fill = name),
            alpha = 0.3777,
            position = 'identity') +
  theme_bw(base_size = 14) +
  theme(legend.position = "top") +
  facet_wrap( ~ year) +
  labs(y = "Longline length composition", x = "Length (cm)", color = "", fill = "") +
  scale_color_manual(values = c('blue', 'green')) +
  scale_fill_manual(values = c('blue', 'green')) -> longline

suppressWarnings(ggplot2::ggsave(longline,
                                 file = here::here(new_year, "plots", 'other','lcomp_compare_ll.png'),
                                 width = 7, height = 7, unit = 'in', dpi = 520))

### annual plot (pot) ----
lcomp_old %>% 
  tidytable::left_join(lcomp_new) %>% 
  tidytable::mutate(length = ceiling(length)) %>% 
  tidytable::filter(gear == 'pot') %>% 
  tidytable::select(year, gear, length, lcomp_old = lencomp, lcomp_new = new_lencomp) %>% 
  tidytable::pivot_longer(cols = c(lcomp_old, lcomp_new)) %>% 
  tidytable::filter(year <= 2023) -> dat

ggplot(data = dat, aes(x = as.numeric(length), y = value, group = name)) +
  geom_line(aes(color = name))  +
  geom_point(aes(color = name), size = 0.5) +
  geom_area(aes(fill = name),
            alpha = 0.3777,
            position = 'identity') +
  theme_bw(base_size = 14) +
  theme(legend.position = "top") +
  facet_wrap( ~ year) +
  labs(y = "Pot length composition", x = "Length (cm)", color = "", fill = "") +
  scale_color_manual(values = c('blue', 'green')) +
  scale_fill_manual(values = c('blue', 'green')) -> pot

suppressWarnings(ggplot2::ggsave(pot,
                                 file = here::here(new_year, "plots", 'other','lcomp_compare_pot.png'),
                                 width = 7, height = 7, unit = 'in', dpi = 520))

### 2022 pot ----

lcomp_old %>% 
  tidytable::mutate(name = 'Original') %>% 
  tidytable::bind_rows(lcomp_old_fltr %>% 
                         tidytable::mutate(name = 'Original, no filter')) %>% 
  tidytable::bind_rows(lcomp_new %>% 
                         tidytable::mutate(name = 'New aggregated M-A-G, merged state, no filter') %>% 
                         tidytable::rename(lencomp = new_lencomp)) %>% 
  tidytable::mutate(length = ceiling(length)) %>% 
  tidytable::filter(year == 2022,
                    gear == 'pot') %>% 
  tidytable::mutate(name2 = factor(name, levels = c('Original', 'Original, no filter', 'New aggregated M-A-G, merged state, no filter'))) -> dat

ggplot(data = dat, aes(x = as.numeric(length), y = lencomp, group = name2)) +
  geom_line(aes(color = name2))  +
  geom_point(aes(color = name2), size = 0.5) +
  geom_area(aes(fill = name2),
            alpha = 0.3777,
            position = 'identity') +
  theme_bw(base_size = 14) +
  theme(legend.position = "none") +
  facet_wrap( ~ name2, ncol = 1) +
  labs(y = "Pot length composition", x = "Length (cm)", fill = "Data treatment:", color = "Data treatment:") +
  scale_color_manual(values = c('green', 'red', 'blue')) +
  scale_fill_manual(values = c('green', 'red', 'blue')) -> pot_22


suppressWarnings(ggplot2::ggsave(pot_22,
                                 file = here::here(new_year, "plots", 'other','lcomp_compare_pot_22.png'),
                                 width = 7, height = 7, unit = 'in', dpi = 520))

## compare bins ----
lcomp_new %>% 
  tidytable::mutate(name = 'lcomp_new',
                    length = ceiling(length)) %>% 
  tidytable::rename(lencomp = new_lencomp) %>% 
  tidytable::bind_rows(lcomp_new_bin2 %>% 
                         tidytable::mutate(name = 'lcomp_new_bin2',
                                           length = length + 1) %>% 
                         tidytable::rename(lencomp = bin_lencomp)) %>% 
  tidytable::bind_rows(lcomp_new_bin5 %>% 
                         tidytable::mutate(name = 'lcomp_new_bin5',
                                           length = length + 2.5) %>% 
                         tidytable::rename(lencomp = bin_lencomp)) %>% 
  tidytable::left_join(lcomp_new %>% 
                         tidytable::summarise(max_og = max(new_lencomp), .by = c(year, gear))) %>% 
  tidytable::left_join(lcomp_new_bin2 %>% 
                         tidytable::summarise(max_bin2 = max(bin_lencomp), .by = c(year, gear))) %>% 
  tidytable::left_join(lcomp_new_bin5 %>% 
                         tidytable::summarise(max_bin5 = max(bin_lencomp), .by = c(year, gear)))  %>% 
  tidytable::mutate(lencomp = tidytable::case_when(name %in% c('lcomp_old', 'lcomp_old_fltr', 'lcomp_new') ~ lencomp,
                                                   name == 'lcomp_new_bin2' ~ -1 * lencomp * max_og / max_bin2,
                                                   name == 'lcomp_new_bin5' ~ -1 * lencomp * max_og / max_bin5)) %>% 
  tidytable::filter(year <= 2023,
                    year >= 2021) -> dat

### pot ----
dat %>% 
  tidytable::filter(gear == 'pot') %>% 
  tidytable::select(year, gear, length, lencomp, name) %>% 
  tidytable::filter(name %in% c('lcomp_new', 'lcomp_new_bin2')) %>% 
  tidytable::mutate(comp = paste0(year, '_2cm')) %>% 
  tidytable::bind_rows(dat %>% 
                         tidytable::filter(gear == 'pot') %>% 
                         tidytable::select(year, gear, length, lencomp, name) %>% 
                         tidytable::filter(name %in% c('lcomp_new', 'lcomp_new_bin5')) %>% 
                         tidytable::mutate(comp = paste0(year, '_5cm'))) %>% 
  tidytable::mutate(name = case_when(name == 'lcomp_new' ~ '1cm',
                                     name == 'lcomp_new_bin2' ~ '2cm',
                                     name == 'lcomp_new_bin5' ~ '5cm')) -> plot_dat

ggplot(data = plot_dat, 
       aes(x = as.numeric(length), y = lencomp, group = name)) +
  geom_line(aes(color = name))  +
  geom_point(aes(color = name)) +
  geom_area(aes(fill = name),
            alpha = 0.3777,
            position = 'identity') +
  theme_bw(base_size = 14) +
  theme(legend.position = "top") +
  facet_wrap( ~ comp, ncol = 2) +
  labs(y = "Pot length composition", x = "Length (cm)", color = "", fill = "") +
  scale_color_manual(values = c('blue', 'red', 'green')) +
  scale_fill_manual(values = c('blue', 'red' , 'green')) -> bin_pot

suppressWarnings(ggplot2::ggsave(bin_pot,
                                 file = here::here(new_year, "plots", 'other','lcomp_compare_bin_pot.png'),
                                 width = 10, height = 12, unit = 'in', dpi = 520))

### trawl ----
dat %>% 
  tidytable::filter(gear == 'trawl') %>% 
  tidytable::select(year, gear, length, lencomp, name) %>% 
  tidytable::filter(name %in% c('lcomp_new', 'lcomp_new_bin2')) %>% 
  tidytable::mutate(comp = paste0(year, '_2cm')) %>% 
  tidytable::bind_rows(dat %>% 
                         tidytable::filter(gear == 'trawl') %>% 
                         tidytable::select(year, gear, length, lencomp, name) %>% 
                         tidytable::filter(name %in% c('lcomp_new', 'lcomp_new_bin5')) %>% 
                         tidytable::mutate(comp = paste0(year, '_5cm'))) %>% 
  tidytable::mutate(name = case_when(name == 'lcomp_new' ~ '1cm',
                                     name == 'lcomp_new_bin2' ~ '2cm',
                                     name == 'lcomp_new_bin5' ~ '5cm')) -> plot_dat

ggplot(data = plot_dat, 
       aes(x = as.numeric(length), y = lencomp, group = name)) +
  geom_line(aes(color = name))  +
  geom_point(aes(color = name)) +
  geom_area(aes(fill = name),
            alpha = 0.3777,
            position = 'identity') +
  theme_bw(base_size = 14) +
  theme(legend.position = "top") +
  facet_wrap( ~ comp, ncol = 2) +
  labs(y = "Trawl length composition", x = "Length (cm)", color = "", fill = "") +
  scale_color_manual(values = c('blue', 'red', 'green')) +
  scale_fill_manual(values = c('blue', 'red', 'green')) -> bin_trawl

suppressWarnings(ggplot2::ggsave(bin_trawl,
                                 file = here::here(new_year, "plots", 'other','lcomp_compare_bin_twl.png'),
                                 width = 10, height =12, unit = 'in', dpi = 520))

### longline ----
dat %>% 
  tidytable::filter(gear == 'longline') %>% 
  tidytable::select(year, gear, length, lencomp, name) %>% 
  tidytable::filter(name %in% c('lcomp_new', 'lcomp_new_bin2')) %>% 
  tidytable::mutate(comp = paste0(year, '_2cm')) %>% 
  tidytable::bind_rows(dat %>% 
                         tidytable::filter(gear == 'longline') %>% 
                         tidytable::select(year, gear, length, lencomp, name) %>% 
                         tidytable::filter(name %in% c('lcomp_new', 'lcomp_new_bin5')) %>% 
                         tidytable::mutate(comp = paste0(year, '_5cm'))) %>% 
  tidytable::mutate(name = case_when(name == 'lcomp_new' ~ '1cm',
                                     name == 'lcomp_new_bin2' ~ '2cm',
                                     name == 'lcomp_new_bin5' ~ '5cm')) -> plot_dat

ggplot(data = plot_dat, 
       aes(x = as.numeric(length), y = lencomp, group = name)) +
  geom_line(aes(color = name))  +
  geom_point(aes(color = name)) +
  geom_area(aes(fill = name),
            alpha = 0.3777,
            position = 'identity') +
  theme_bw(base_size = 14) +
  theme(legend.position = "top") +
  facet_wrap( ~ comp, ncol = 2) +
  labs(y = "Longline length composition", x = "Length (cm)", color = "", fill = "") +
  scale_color_manual(values = c('blue', 'red', 'green')) +
  scale_fill_manual(values = c('blue', 'red', 'green')) -> bin_ll

suppressWarnings(ggplot2::ggsave(bin_ll,
                                 file = here::here(new_year, "plots", 'other','lcomp_compare_bin_ll.png'),
                                 width = 10, height = 12, unit = 'in', dpi = 520))

### trawl survey ----
# get new way of doing comps
lcomp_srv <- get_twl_srvy_lcomp(new_year = new_year,
                                bins = len_bins,
                                ss3_frmt = FALSE)

lcomp_srv_bin2 <- get_twl_srvy_lcomp(new_year = new_year,
                                     bins = len_bins2,
                                     ss3_frmt = FALSE) %>% 
  tidytable::rename(bin_lencomp = lencomp)

lcomp_srv_bin5 <- get_twl_srvy_lcomp(new_year = new_year,
                                     bins = len_bins5,
                                     ss3_frmt = FALSE) %>% 
  tidytable::rename(bin_lencomp = lencomp)


lcomp_srv %>% 
  tidytable::mutate(name = 'lcomp_new',
                    length = ceiling(length)) %>% 
  tidytable::bind_rows(lcomp_srv_bin2 %>% 
                         tidytable::mutate(name = 'lcomp_new_bin2',
                                           length = length + 1) %>% 
                         tidytable::rename(lencomp = bin_lencomp)) %>% 
  tidytable::bind_rows(lcomp_srv_bin5 %>% 
                         tidytable::mutate(name = 'lcomp_new_bin5',
                                           length = length + 2.5) %>% 
                         tidytable::rename(lencomp = bin_lencomp)) %>% 
  tidytable::left_join(lcomp_srv %>% 
                         tidytable::summarise(max_og = max(lencomp), .by = c(year))) %>% 
  tidytable::left_join(lcomp_srv_bin2 %>% 
                         tidytable::summarise(max_bin2 = max(bin_lencomp), .by = c(year))) %>% 
  tidytable::left_join(lcomp_srv_bin5 %>% 
                         tidytable::summarise(max_bin5 = max(bin_lencomp), .by = c(year)))  %>% 
  tidytable::mutate(lencomp = tidytable::case_when(name %in% c('lcomp_old', 'lcomp_old_fltr', 'lcomp_new') ~ lencomp,
                                                   name == 'lcomp_new_bin2' ~ -1 * lencomp * max_og / max_bin2,
                                                   name == 'lcomp_new_bin5' ~ -1 * lencomp * max_og / max_bin5)) %>% 
  tidytable::filter(year <= 2023,
                    year >= 2019) -> dat

dat %>% 
  tidytable::select(year, length, lencomp, name) %>% 
  tidytable::filter(name %in% c('lcomp_new', 'lcomp_new_bin2')) %>% 
  tidytable::mutate(comp = paste0(year, '_2cm')) %>% 
  tidytable::bind_rows(dat %>% 
                         tidytable::select(year, length, lencomp, name) %>% 
                         tidytable::filter(name %in% c('lcomp_new', 'lcomp_new_bin5')) %>% 
                         tidytable::mutate(comp = paste0(year, '_5cm'))) %>% 
  tidytable::mutate(name = case_when(name == 'lcomp_new' ~ '1cm',
                                     name == 'lcomp_new_bin2' ~ '2cm',
                                     name == 'lcomp_new_bin5' ~ '5cm')) -> plot_dat


ggplot(data = plot_dat, 
       aes(x = as.numeric(length), y = lencomp, group = name)) +
  geom_line(aes(color = name))  +
  geom_point(aes(color = name)) +
  geom_area(aes(fill = name),
            alpha = 0.3777,
            position = 'identity') +
  theme_bw(base_size = 18) +
  theme(legend.position = "top") +
  facet_wrap( ~ comp, ncol = 2) +
  labs(y = "Trawl survey length composition", x = "Length (cm)", color = "", fill = "") +
  scale_color_manual(values = c('blue', 'red', 'green')) +
  scale_fill_manual(values = c('blue', 'red', 'green')) -> bin_twl_srv

suppressWarnings(ggplot2::ggsave(bin_twl_srv,
                                 file = here::here(new_year, "plots", 'other','lcomp_compare_bin_tsrv.png'),
                                 width = 10, height = 12, unit = 'in', dpi = 520))

### longline survey ----

# get new way of doing comps
lcomp_srv <- get_ll_srvy_lcomp(new_year = new_year,
                               bins = len_bins,
                               ss3_frmt = FALSE)

lcomp_srv_bin2 <- get_ll_srvy_lcomp(new_year = new_year,
                                    bins = len_bins2,
                                    ss3_frmt = FALSE)

lcomp_srv_bin5 <- get_ll_srvy_lcomp(new_year = new_year,
                                    bins = len_bins5,
                                    ss3_frmt = FALSE)

lcomp_srv %>% 
  tidytable::mutate(name = 'lcomp_new',
                    length = ceiling(length)) %>% 
  tidytable::bind_rows(lcomp_srv_bin2 %>% 
                         tidytable::mutate(name = 'lcomp_new_bin2',
                                           length = length + 1)) %>% 
  tidytable::bind_rows(lcomp_srv_bin5 %>% 
                         tidytable::mutate(name = 'lcomp_new_bin5',
                                           length = length + 2.5)) %>% 
  tidytable::left_join(lcomp_srv %>% 
                         tidytable::summarise(max_og = max(lencomp), .by = c(year))) %>% 
  tidytable::left_join(lcomp_srv_bin2 %>% 
                         tidytable::summarise(max_bin2 = max(lencomp), .by = c(year))) %>% 
  tidytable::left_join(lcomp_srv_bin5 %>% 
                         tidytable::summarise(max_bin5 = max(lencomp), .by = c(year)))  %>% 
  tidytable::mutate(lencomp = tidytable::case_when(name %in% c('lcomp_old', 'lcomp_old_fltr', 'lcomp_new') ~ lencomp,
                                                   name == 'lcomp_new_bin2' ~ -1 * lencomp * max_og / max_bin2,
                                                   name == 'lcomp_new_bin5' ~ -1 * lencomp * max_og / max_bin5)) %>% 
  tidytable::filter(year <= 2023,
                    year >= 2021) -> dat


dat %>% 
  tidytable::select(year, length, lencomp, name) %>% 
  tidytable::filter(name %in% c('lcomp_new', 'lcomp_new_bin2')) %>% 
  tidytable::mutate(comp = paste0(year, '_2cm')) %>% 
  tidytable::bind_rows(dat %>% 
                         tidytable::select(year, length, lencomp, name) %>% 
                         tidytable::filter(name %in% c('lcomp_new', 'lcomp_new_bin5')) %>% 
                         tidytable::mutate(comp = paste0(year, '_5cm'))) %>% 
  tidytable::mutate(name = case_when(name == 'lcomp_new' ~ '1cm',
                                     name == 'lcomp_new_bin2' ~ '2cm',
                                     name == 'lcomp_new_bin5' ~ '5cm')) -> plot_dat

ggplot(data = plot_dat, aes(x = as.numeric(length), y = lencomp, group = name)) +
  geom_line(aes(color = name))  +
  geom_point(aes(color = name)) +
  geom_area(aes(fill = name),
            alpha = 0.3777,
            position = 'identity') +
  theme_bw(base_size = 14) +
  theme(legend.position = "top") +
  facet_wrap( ~ comp, ncol = 2) +
  labs(y = "Longline survey length composition", x = "Length (cm)", color = "", fill = "") +
  scale_color_manual(values = c('blue', 'red', 'green')) +
  scale_fill_manual(values = c('blue', 'red', 'green')) -> bin_ll_srv

suppressWarnings(ggplot2::ggsave(bin_ll_srv,
                                 file = here::here(new_year, "plots", 'other','lcomp_compare_bin_llsrv.png'),
                                 width = 10, height = 12, unit = 'in', dpi = 520))



# fishery age comps ----
max_age = 10

# get old way of doing comps
acomp_old <- get_fsh_age(new_year,
                         st_yr = 2007,
                         max_age,
                         fltr = TRUE,
                         add_a1 = TRUE,
                         use_FSA = TRUE,
                         iters = 1,
                         by_sex = TRUE,
                         ss3_frmt = FALSE,
                         fit = FALSE)

# get new way of doing comps
acomp_new <- get_fsh_age_new(new_year,
                             st_yr = 2007,
                             max_age,
                             ss3_frmt = FALSE,
                             fit = FALSE)

## aggregated plot ----
acomp_old %>% 
  tidytable::filter(age <= max_age) %>% 
  tidytable::left_join(acomp_new %>% 
                         tidytable::filter(age <= max_age) %>% 
                         tidytable::rename(new_agecomp = agecomp)) %>% 
  tidytable::summarise(agecomp = sum(agecomp),
                       new_agecomp = sum(new_agecomp),
                       .by = c(gear, age)) %>% 
  tidytable::mutate(tot = sum(agecomp),
                    tot_new = sum(new_agecomp),
                    .by = gear) %>% 
  tidytable::mutate(acomp_old = agecomp / tot,
                    acomp_new = new_agecomp / tot_new) %>% 
  tidytable::select(gear, age, acomp_old, acomp_new) %>% 
  tidytable::pivot_longer(cols = c(acomp_old, acomp_new)) -> dat

ggplot(data = dat, aes(x = age, y = value, group = name)) +
  geom_line(aes(color = name))  +
  geom_point(aes(color = name)) +
  geom_area(aes(fill = name),
            alpha = 0.3777,
            position = 'identity') +
  theme(legend.position = "top") +
  facet_wrap( ~ gear, nrow = 3,
             strip.position = 'top') +
  labs(y = "Aggregated age composition", x = "Age") +
  scale_color_manual(values = c('blue', 'green')) +
  scale_fill_manual(values = c('blue', 'green')) -> agg_age

suppressWarnings(ggplot2::ggsave(agg_age,
                                 file = here::here(new_year, "plots", 'other','acomp_compare_agg.png'),
                                 width = 12, height = 7, unit = 'in', dpi = 520))

## annual plot (trawl) ----
acomp_old %>% 
  tidytable::filter(age <= max_age) %>% 
  tidytable::left_join(acomp_new %>% 
                         tidytable::filter(age <= max_age) %>% 
                         tidytable::rename(new_agecomp = agecomp)) %>% 
  tidytable::filter(gear == 'trawl') %>% 
  tidytable::select(year, gear, age, acomp_old = agecomp, acomp_new = new_agecomp) %>% 
  tidytable::pivot_longer(cols = c(acomp_old, acomp_new)) -> dat

ggplot(data = dat, aes(x = as.numeric(age), y = value, group = name)) +
  geom_line(aes(color = name))  +
  geom_point(aes(color = name)) +
  geom_area(aes(fill = name),
            alpha = 0.3777,
            position = 'identity') +
  theme(legend.position = "top") +
  labs(y = "Trawl age composition", x = "Age") +
  facet_wrap( ~ year) +
  scale_color_manual(values = c('blue', 'green')) +
  scale_fill_manual(values = c('blue', 'green')) -> twl_age

suppressWarnings(ggplot2::ggsave(twl_age,
                                 file = here::here(new_year, "plots", 'other','acomp_compare_twl.png'),
                                 width = 7, height = 7, unit = 'in', dpi = 520))

## annual plot (longline) ----
acomp_old %>% 
  tidytable::filter(age <= max_age) %>% 
  tidytable::left_join(acomp_new %>% 
                         tidytable::filter(age <= max_age) %>% 
                         tidytable::rename(new_agecomp = agecomp)) %>% 
  tidytable::filter(gear == 'longline') %>% 
  tidytable::select(year, gear, age, acomp_old = agecomp, acomp_new = new_agecomp) %>% 
  tidytable::pivot_longer(cols = c(acomp_old, acomp_new)) -> dat

ggplot(data = dat, aes(x = as.numeric(age), y = value, group = name)) +
  geom_line(aes(color = name))  +
  geom_point(aes(color = name)) +
  geom_area(aes(fill = name),
            alpha = 0.3777,
            position = 'identity') +
  theme(legend.position = "top") +
  labs(y = "Longline age composition", x = "Age") +
  facet_wrap( ~ year) +
  scale_color_manual(values = c('blue', 'green')) +
  scale_fill_manual(values = c('blue', 'green')) -> ll_age

suppressWarnings(ggplot2::ggsave(ll_age,
                                 file = here::here(new_year, "plots", 'other','acomp_compare_ll.png'),
                                 width = 7, height = 7, unit = 'in', dpi = 520))

## annual plot (pot) ----
acomp_old %>% 
  tidytable::filter(age <= max_age) %>% 
  tidytable::left_join(acomp_new %>% 
                         tidytable::filter(age <= max_age) %>% 
                         tidytable::rename(new_agecomp = agecomp)) %>% 
  tidytable::filter(gear == 'pot') %>% 
  tidytable::select(year, gear, age, acomp_old = agecomp, acomp_new = new_agecomp) %>% 
  tidytable::pivot_longer(cols = c(acomp_old, acomp_new)) -> dat

ggplot(data = dat, aes(x = as.numeric(age), y = value, group = name)) +
  geom_line(aes(color = name))  +
  geom_point(aes(color = name)) +
  geom_area(aes(fill = name),
            alpha = 0.3777,
            position = 'identity') +
  theme(legend.position = "top") +
  labs(y = "Pot age composition", x = "Age") +
  facet_wrap( ~ year) +
  scale_color_manual(values = c('blue', 'green')) +
  scale_fill_manual(values = c('blue', 'green')) -> pot_age

suppressWarnings(ggplot2::ggsave(ll_age,
                                 file = here::here(new_year, "plots", 'other','acomp_compare_pot.png'),
                                 width = 7, height = 7, unit = 'in', dpi = 520))



## osa plots ----

new_base <- "2019.1c-2024"
new_base_ae <- "2019.1d-2024"
new_base_lcomp <- "2019.1e-2024"
new_base_lcomp_bin2 <- "2019.1e.2cm-2024"
new_base_lcomp_bin5 <- "2019.1e.5cm-2024"
 
model = here::here(new_year, 'rsch', new_base)
fleet = 1
sx = 1
lengths = seq(0.5, 116.5, by = 1)
N = 100

# OSA_run_SS_length()
library(compResidual)
mods1 <- r4ss::SSgetoutput(dirvec = model)

age <- data.table::data.table(mods1[[1]]$lendbase[,c(1,6,13,16:19)])[Bin%in%lengths & Fleet == fleet & Sex == sx]
age <- data.table::data.table(melt(age, c('Yr', 'Fleet', 'Sex', 'Bin')))
o <- age[variable == 'Obs']
o <- maditr::dcast(o, Yr ~ Bin)
p <- age[variable == 'Exp']
p <- maditr::dcast(p, Yr ~ Bin)
pearson <- age[variable == 'Pearson']
pearson <- maditr::dcast(pearson, Yr ~ Bin)
yrs <- o$Yr
obs <- as.matrix(o[,-1])
pred <- as.matrix(p[,-1])
pearson <- as.matrix(pearson[,-1])
Neff <- round(data.table::data.table(mods1[[1]]$lendbase)[Bin == lengths[1] & Fleet == fleet & Sex == sx]$effN)

# plot_osa_comps2()

o1 <- round(Neff * obs / rowSums(obs), 0)
p <- pred / rowSums(pred)
## default output
res1 <- list()
sdnr1 <- list()
for(i in 1:N){
  res1[[i]] <- resMulti(t(o1), t(p))
  sdnr1[[i]] <- sd(res1[[i]])
}
sdnr <- data.table(do.call(rbind, sdnr1))
names(sdnr) = "sdnr"
sdnr$ID <- 1:nrow(sdnr)
sdnr <- sdnr[order(sdnr),]

HCI <- sqrt(qchisq(.975, (length(res1[[1]]) - 1)) / (length(res1[[1]]) - 1))
LCI <- sqrt(qchisq(.025, (length(res1[[1]]) - 1)) / (length(res1[[1]]) - 1))
n = 1
if(N > 1){n = trunc(N / 2)}

res = res1[[sdnr$ID[n]]]

# plot_res2()




plot_res2(x=res,o=obs,e=p,pr=pearson,yr=years,inde=index)
dev.off()

print(psdnr)
windows(width=20,height=12)
plot_res2(x=res,o=obs,e=p,pr=pearson,yr=years,inde=index)
  

library(compResidual) 
library(ggplot2)
library(ggpubr)

Neff <- colSums(o)
ehat <- Neff*e
V <- Neff*e*(1-e)

nbins <- nrow(o)
nyrs<-nrow(e)
pearson<-pr
if(!all(is.finite(x))){
  ind <- (!is.finite(x))
  message("the following  were not finite")
  message("observed counts= ", paste(o[-nbins,][ind], collapse=' '))
  message("expected counts= ", paste(round(ehat[-nbins,][ind],2), collapse=' '))
  message("Pearson resid= ", paste(round(pearson[-nbins,][ind],2), collapse=' '))
  message("OSA resid= ", paste(round(x[ind],2), collapse=' '))
  stop('non-finite residuals')
  pearson[2,17]
  pearson[!is.finite(x)]
  pearson[ind]
  x[ind]
  o[-nbins,][ind]
  ehat[-nbins,][ind]
  pearson[-nbins,][ind]
}
pearson=t(pearson)

o_tab<-data.table(t(matrix(x,nrow=length(inde)-1)))
names(o_tab)<-paste(inde[1:(length(inde)-1)])
o_tab$yr=yr

o_tab<-melt(o_tab,"yr")
names(o_tab)<-c("Year","Index","Value")
o_tab$Sign<-"<0"
o_tab[Value>0]$Sign<-">0"

bubble_osa<-ggplot(data=o_tab,aes(y=Index,x=Year,color=Sign,size=abs(Value),alpha=abs(Value)))+
  geom_point()+theme_bw(base_size=12)+scale_color_manual(values=c("blue","red"))+
  labs(color="resid",sign="resid",size="resid",alpha="resid", title="OSA residuals")

p_tab<-data.table(t(pearson))
p_tab$yr<-yr
p_tab<-melt(p_tab,"yr")
names(p_tab)<-c("Year","Index","Value")
p_tab$Sign<-"<0"
p_tab[Value>0]$Sign<-">0"
bubble_pear<-ggplot(data=p_tab,aes(y=Index,x=Year,color=Sign,size=abs(Value),alpha=abs(Value)))+
  geom_point()+theme_bw(base_size=12)+scale_color_manual(values=c("blue","red"))+
  labs(color="resid",sign="resid",size="resid",alpha="resid", title="Pearson residuals")

pfram<-data.frame(y=c(pearson))
o_tab1<-data.table(t(matrix(x,nrow=length(inde)-1)))
o_val<-melt(o_tab1)$value
ofram<-data.frame(y=c(o_val))
ofram$type="OSA"
pfram$type="Pearson"
fram<-rbind(ofram,pfram)

o_sdnr<-sd(subset(fram,type=="OSA")$y)
p_sdnr<-sd(subset(fram,type=="Pearson")$y)

qq_p<-ggplot(fram,aes(sample=y,shape=type,color=type))+stat_qq(size=2)+scale_color_manual(values=c("blue","red"))+
  theme_bw(base_size=12)+scale_shape_manual(values=c(16,1))+geom_abline()+
  labs(color="Residual type",shape='Residual type', title="QQ plots",x="",y="")+
  annotate("text",label=paste0("OSA SDNR = ",round(o_sdnr,3)),x=0,y=min(fram$y)+(1-(min(fram$y)/8)),hjust="left")+
  annotate("text",label=paste0("Pearson SDNR = ",round(p_sdnr,3)),x=0,y = min(fram$y)+1,hjust="left")


oagg <- colSums(o)/sum(o)
eagg <- colSums(e)/sum(e)
tab1<-data.table(Index=inde,Obs=oagg,Exp=eagg)

agg_plot<-ggplot(data=tab1)+geom_bar(aes(x=Index,y=Obs),stat='identity',color="blue",fill='blue')+
  geom_line(aes(x=Index,y=Exp),color='red')+theme_bw(base_size=16)+
  labs(title="Aggregated fits",y="Proportion",x="Index")



plotg<-ggpubr::ggarrange(bubble_pear, bubble_osa,qq_p,agg_plot,ncol=2,nrow=2,heights=c(1,1),widths=c(1,1),align = c("h"),
                         common.legend = F, legend = "bottom")
print(plotg)

## ageing error ----

both_lin <- vroom::vroom(here::here(new_year, 'data', 'ageing_error', 'ResultsBoth_Linear', 'Pcod SS3_format_Reader1.csv'), delim = ',')
both_spl <- vroom::vroom(here::here(new_year, 'data', 'ageing_error', 'ResultsBoth_Spline', 'Pcod SS3_format_Reader1.csv'), delim = ',')

ebs_lin <- vroom::vroom(here::here(new_year, 'data', 'ageing_error', 'ResultsBS10_Linear', 'Pcod SS3_format_Reader1.csv'), delim = ',')
ebs_spl <- vroom::vroom(here::here(new_year, 'data', 'ageing_error', 'ResultsBS10_Spline', 'Pcod SS3_format_Reader1.csv'), delim = ',')

goa_lin <- vroom::vroom(here::here(new_year, 'data', 'ageing_error', 'ResultsGOA_Linear', 'Pcod SS3_format_Reader1.csv'), delim = ',')
goa_spl <- vroom::vroom(here::here(new_year, 'data', 'ageing_error', 'ResultsGOA_Spline', 'Pcod SS3_format_Reader1.csv'), delim = ',')

both_lin %>% 
  tidytable::filter(...1 %in% c('True_Age')) %>% 
  tidytable::select(-...1, -'Age 0') %>% 
  tidytable::pivot_longer(values_to = 'age') %>% 
  tidytable::left_join(both_lin %>% 
                         tidytable::filter(...1 %in% c('SD')) %>% 
                         tidytable::select(-...1, -'Age 0') %>% 
                         tidytable::pivot_longer(values_to = 'sd')) %>% 
  tidytable::mutate(region = 'both',
                    type = 'linear') %>% 
  tidytable::bind_rows(both_spl %>% 
                         tidytable::filter(...1 %in% c('True_Age')) %>% 
                         tidytable::select(-...1, -'Age 0') %>% 
                         tidytable::pivot_longer(values_to = 'age') %>% 
                         tidytable::left_join(both_spl %>% 
                                                tidytable::filter(...1 %in% c('SD')) %>% 
                                                tidytable::select(-...1, -'Age 0') %>% 
                                                tidytable::pivot_longer(values_to = 'sd')) %>% 
                         tidytable::mutate(region = 'both',
                                           type = 'spline')) %>% 
  tidytable::bind_rows(ebs_lin %>% 
                         tidytable::filter(...1 %in% c('True_Age')) %>% 
                         tidytable::select(-...1, -'Age 0') %>% 
                         tidytable::pivot_longer(values_to = 'age') %>% 
                         tidytable::left_join(ebs_lin %>% 
                                                tidytable::filter(...1 %in% c('SD')) %>% 
                                                tidytable::select(-...1, -'Age 0') %>% 
                                                tidytable::pivot_longer(values_to = 'sd')) %>% 
                         tidytable::mutate(region = 'ebs',
                                           type = 'linear')) %>% 
  tidytable::bind_rows(ebs_spl %>% 
                         tidytable::filter(...1 %in% c('True_Age')) %>% 
                         tidytable::select(-...1, -'Age 0') %>% 
                         tidytable::pivot_longer(values_to = 'age') %>% 
                         tidytable::left_join(ebs_spl %>% 
                                                tidytable::filter(...1 %in% c('SD')) %>% 
                                                tidytable::select(-...1, -'Age 0') %>% 
                                                tidytable::pivot_longer(values_to = 'sd')) %>% 
                         tidytable::mutate(region = 'ebs',
                                           type = 'spline')) %>% 
  tidytable::bind_rows(goa_lin %>% 
                         tidytable::filter(...1 %in% c('True_Age')) %>% 
                         tidytable::select(-...1, -'Age 0') %>% 
                         tidytable::pivot_longer(values_to = 'age') %>% 
                         tidytable::left_join(goa_lin %>% 
                                                tidytable::filter(...1 %in% c('SD')) %>% 
                                                tidytable::select(-...1, -'Age 0') %>% 
                                                tidytable::pivot_longer(values_to = 'sd')) %>% 
                         tidytable::mutate(region = 'goa',
                                           type = 'linear')) %>% 
  tidytable::bind_rows(goa_spl %>% 
                         tidytable::filter(...1 %in% c('True_Age')) %>% 
                         tidytable::select(-...1, -'Age 0') %>% 
                         tidytable::pivot_longer(values_to = 'age') %>% 
                         tidytable::left_join(goa_spl %>% 
                                                tidytable::filter(...1 %in% c('SD')) %>% 
                                                tidytable::select(-...1, -'Age 0') %>% 
                                                tidytable::pivot_longer(values_to = 'sd')) %>% 
                         tidytable::mutate(region = 'goa',
                                           type = 'spline')) %>% 
  tidytable::select(-name) -> dat

ggplot(data = dat, aes(x = age, y = sd, colour = region, linetype = type)) +
  geom_line(linewidth = 1.25) +
  # geom_point(size = 2.5) +
  # facet_grid(~type) +
  theme_bw() +
  scale_x_continuous(breaks = seq(1,10), labels = seq(1,10)) +
  theme(panel.grid.minor = element_blank()) -> age_err

suppressWarnings(ggplot2::ggsave(age_err,
                                 file = here::here(new_year, "plots", 'other','age_err.png'),
                                 width = 7, height = 7, unit = 'in', dpi = 520))
