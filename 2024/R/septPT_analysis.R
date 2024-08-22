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
          "here")

if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )]) > 0) {
  install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE)])
}

lapply(libs, library, character.only = TRUE)

# Current assessment year
new_year <- as.numeric(format(Sys.Date(), format = "%Y"))

# source functions ----
source_files <- list.files(here::here(new_year, "R", "get_data"), "*.r$")
purrr::map(here::here(new_year, "R", "get_data", source_files), source)
source(here::here(new_year, "R", "utils.r"))


# ageing error plots ----

vroom::vroom(here::here(new_year, 'data', 'ageing_error', 'ResultsBoth_Spline', 'Pcod SS3_format_Reader1.csv')) %>% 
  tidytable::filter(...1 == 'SD') %>% 
  tidytable::pivot_longer() %>% 
  tidytable::select(value)



# fishery comp filtering analysis ----

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

## state great than fed (step 1), and has > 30 (step 2) ----
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

fsh_len_f %>% 
  # get federal number of length obs by trimester-area-gear
  tidytable::summarise(tfreq = sum(freq), .by = c(year, trimester, area, gear)) %>% 
  # get state number of length obs by trimester-area-gear
  tidytable::full_join(fsh_len_s %>% 
                         tidytable::summarise(sfreq = sum(freq), .by = c(year, trimester, area, gear))) %>% 
  # set index for when state > federal lengths
  tidytable::mutate(tfreq = tidytable::replace_na(tfreq, 0),
                    sfreq = tidytable::replace_na(sfreq, 0),
                    state = tidytable::case_when(sfreq > tfreq ~ 1,
                                                 .default = 0)) %>% 
  tidytable::filter(sfreq > 0) %>% 
  tidytable::summarise(tot_n = length(sfreq),
                       used_n = sum(state), .by = c(year, gear)) %>% 
  tidytable::mutate(prop_fltrd = 1 - used_n / tot_n) %>% 
  tidytable::select(year, gear, prop_fltrd) %>% 
  tidytable::pivot_wider(names_from = gear, values_from = prop_fltrd) %>% 
  vroom::vroom_write(., here::here(new_year, 'output', 'st_fltrd_s1.csv'), delim = ",")

fsh_len_f %>% 
  # get federal number of length obs by trimester-area-gear
  tidytable::summarise(tfreq = sum(freq), .by = c(year, trimester, area, gear)) %>% 
  # get state number of length obs by trimester-area-gear
  tidytable::full_join(fsh_len_s %>% 
                         tidytable::summarise(sfreq = sum(freq), .by = c(year, trimester, area, gear))) %>% 
  tidytable::filter(sfreq > 0) %>% 
  tidytable::summarise(tot_n = length(sfreq), .by = c(year, gear)) %>% 
  tidytable::left_join(fsh_len_f %>% 
                         # get federal number of length obs by trimester-area-gear
                         tidytable::summarise(tfreq = sum(freq), .by = c(year, trimester, area, gear)) %>% 
                         # get state number of length obs by trimester-area-gear
                         tidytable::full_join(fsh_len_s %>% 
                                                tidytable::summarise(sfreq = sum(freq), .by = c(year, trimester, area, gear))) %>% 
                         tidytable::filter(sfreq > 0) %>% 
                         tidytable::filter(tfreq < 30,
                                           sfreq >= 30) %>% 
                         tidytable::summarise(used_n = length(sfreq), .by = c(year, gear))) %>% 
  tidytable::mutate(prop_fltrd = 1 - used_n / tot_n) %>% 
  tidytable::select(year, gear, prop_fltrd) %>% 
  tidytable::pivot_wider(names_from = gear, values_from = prop_fltrd) %>% 
  vroom::vroom_write(., here::here(new_year, 'output', 'st_fltrd_s2.csv'), delim = ",")
  # filter to state with greater than 30 lengths and fed with less
  tidytable::filter(tfreq < 30,
                    sfreq >= 30) %>% 
  tidytable::select(-sfreq, -tfreq) -> state_test 





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
                                    ss3_frmt = FALSE) %>% 
  tidytable::rename(new_lencomp = lencomp)

# new way with new bins
# 2 cm
lcomp_new_bin2 <- get_fsh_len_post91_new(new_year,
                                         bins = len_bins2,
                                         ss3_frmt = FALSE) %>% 
  tidytable::rename(bin_lencomp = lencomp)
# 5 cm
lcomp_new_bin5 <- get_fsh_len_post91_new(new_year,
                                         bins = len_bins5,
                                         ss3_frmt = FALSE) %>% 
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
  theme(legend.position = "top") +
  facet_wrap(~ gear, nrow = 3,
             strip.position = 'top') +
  labs(y = "Aggregated length composition", x = "Length (cm)") +
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
  theme(legend.position = "top") +
  facet_wrap( ~ year) +
  labs(y = "Trawl length composition", x = "Length (cm)") +
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
  theme(legend.position = "top") +
  facet_wrap( ~ year) +
  labs(y = "Longline length composition", x = "Length (cm)") +
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
  theme(legend.position = "top") +
  facet_wrap( ~ year) +
  labs(y = "Pot length composition", x = "Length (cm)") +
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
                         tidytable::mutate(name = 'New aggregated T-A-G, merged state, no filter') %>% 
                         tidytable::rename(lencomp = new_lencomp)) %>% 
  tidytable::mutate(length = ceiling(length)) %>% 
  tidytable::filter(year == 2022,
                    gear == 'pot') %>% 
  tidytable::mutate(name2 = factor(name, levels = c('Original', 'Original, no filter', 'New aggregated T-A-G, merged state, no filter'))) -> dat


ggplot(data = dat, aes(x = as.numeric(length), y = lencomp, group = name2)) +
  geom_line(aes(color = name2))  +
  geom_point(aes(color = name2), size = 0.5) +
  geom_area(aes(fill = name2),
            alpha = 0.3777,
            position = 'identity') +
  theme(legend.position = 'none') +
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
                    year >= 2018) -> dat

### pot ----
ggplot(data = dat %>% tidytable::filter(gear == 'pot'), 
       aes(x = as.numeric(length), y = lencomp, group = name)) +
  geom_line(aes(color = name))  +
  geom_point(aes(color = name)) +
  geom_area(aes(fill = name),
            alpha = 0.3777,
            position = 'identity') +
  theme(legend.position = "top",
        axis.text.y = element_blank()) +
  facet_wrap( ~ year) +
  labs(y = "Pot length composition", x = "Length (cm)") +
  scale_color_manual(values = c('blue', 'red', 'green')) +
  scale_fill_manual(values = c('blue', 'red' , 'green')) -> bin_pot

suppressWarnings(ggplot2::ggsave(bin_pot,
                                 file = here::here(new_year, "plots", 'other','lcomp_compare_bin_pot.png'),
                                 width = 12, height = 7, unit = 'in', dpi = 520))

### trawl ----
ggplot(data = dat %>% tidytable::filter(gear == 'trawl'), 
       aes(x = as.numeric(length), y = lencomp, group = name)) +
  geom_line(aes(color = name))  +
  geom_point(aes(color = name)) +
  geom_area(aes(fill = name),
            alpha = 0.3777,
            position = 'identity') +
  theme(legend.position = "top",
        axis.text.y = element_blank()) +
  facet_wrap( ~ year) +
  labs(y = "Trawl length composition", x = "Length (cm)") +
  scale_color_manual(values = c('blue', 'red', 'green')) +
  scale_fill_manual(values = c('blue', 'red', 'green')) -> bin_trawl

suppressWarnings(ggplot2::ggsave(bin_trawl,
                                 file = here::here(new_year, "plots", 'other','lcomp_compare_bin_twl.png'),
                                 width = 12, height = 7, unit = 'in', dpi = 520))

### longline ----
ggplot(data = dat %>% tidytable::filter(gear == 'longline'), 
       aes(x = as.numeric(length), y = lencomp, group = name)) +
  geom_line(aes(color = name))  +
  geom_point(aes(color = name)) +
  geom_area(aes(fill = name),
            alpha = 0.3777,
            position = 'identity') +
  theme(legend.position = "top",
        axis.text.y = element_blank()) +
  facet_wrap( ~ year) +
  labs(y = "Longline length composition", x = "Length (cm)") +
  scale_color_manual(values = c('blue', 'red', 'green')) +
  scale_fill_manual(values = c('blue', 'red', 'green')) -> bin_ll

suppressWarnings(ggplot2::ggsave(bin_ll,
                                 file = here::here(new_year, "plots", 'other','lcomp_compare_bin_ll.png'),
                                 width = 12, height = 7, unit = 'in', dpi = 520))

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
                    year >= 2018) -> dat


ggplot(data = dat, 
       aes(x = as.numeric(length), y = lencomp, group = name)) +
  geom_line(aes(color = name))  +
  geom_point(aes(color = name)) +
  geom_area(aes(fill = name),
            alpha = 0.3777,
            position = 'identity') +
  theme(legend.position = "top",
        axis.text.y = element_blank()) +
  facet_wrap( ~ year) +
  labs(y = "Trawl survey length composition", x = "Length (cm)") +
  scale_color_manual(values = c('blue', 'red', 'green')) +
  scale_fill_manual(values = c('blue', 'red', 'green')) -> bin_twl_srv

suppressWarnings(ggplot2::ggsave(bin_twl_srv,
                                 file = here::here(new_year, "plots", 'other','lcomp_compare_bin_tsrv.png'),
                                 width = 12, height = 7, unit = 'in', dpi = 520))

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
                    year >= 2018) -> dat

ggplot(data = dat, aes(x = as.numeric(length), y = lencomp, group = name)) +
  geom_line(aes(color = name))  +
  geom_point(aes(color = name)) +
  geom_area(aes(fill = name),
            alpha = 0.3777,
            position = 'identity') +
  theme(legend.position = "top",
        axis.text.y = element_blank()) +
  facet_wrap( ~ year) +
  labs(y = "Longline survey length composition", x = "Length (cm)") +
  scale_color_manual(values = c('blue', 'red', 'green')) +
  scale_fill_manual(values = c('blue', 'red', 'green')) -> bin_ll_srv

suppressWarnings(ggplot2::ggsave(bin_ll_srv,
                                 file = here::here(new_year, "plots", 'other','lcomp_compare_bin_llsrv.png'),
                                 width = 12, height = 7, unit = 'in', dpi = 520))



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
