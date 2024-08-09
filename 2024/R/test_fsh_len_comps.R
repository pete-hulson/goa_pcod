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

# source functions ----
source_files <- list.files(here::here(new_dat_year, "R", "get_data"), "*.r$")
purrr::map(here::here(new_dat_year, "R", "get_data", source_files), source)
source(here::here(new_dat_year, "R", "utils.r"))

# Current assessment year
new_year <- as.numeric(format(Sys.Date(), format = "%Y"))

# length comps ----
# length bins to use for length comp data
bin_width <- 1
min_size <- 0.5
max_size <- 116.5  # less than 1% of the fish in each year are 105 cm or larger (max less than 0.6%)
len_bins <- seq(min_size, max_size, bin_width)

len_bins = c(4.5, 9.5, 14.5, 19.5, 24.5, 29.5, 34.5, 39.5, 44.5, 49.5, 54.5, 59.5, 64.5, 69.5, 74.5, 79.5, 84.5, 89.5, 94.5, 99.5, 104.5, 109.5, 114.5, 119.5)


# get old way of doing comps
lcomp_old <- get_fsh_len_post91(new_year,
                                fltr = TRUE,
                                bins = len_bins,
                                ss3_frmt = FALSE)


# get new way of doing comps
lcomp_new <- get_fsh_len_post91_new(new_year,
                                    bins = len_bins,
                                    ss3_frmt = FALSE) %>% 
  tidytable::rename(new_lencomp = lencomp)

## aggregated plot ----
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
  facet_grid(rows = vars(gear))

## annual plot (trawl) ----
lcomp_old %>% 
  tidytable::left_join(lcomp_new) %>% 
  tidytable::mutate(length = ceiling(length)) %>% 
  tidytable::filter(gear == 'trawl') %>% 
  tidytable::select(year, gear, length, lcomp_old = lencomp, lcomp_new = new_lencomp) %>% 
  tidytable::pivot_longer(cols = c(lcomp_old, lcomp_new)) -> dat

ggplot(data = dat, aes(x = as.numeric(length), y = value, group = name)) +
  geom_line(aes(color = name))  +
  geom_point(aes(color = name)) +
  geom_area(aes(fill = name),
            alpha = 0.3777,
            position = 'identity') +
  theme(legend.position = "top") +
  facet_wrap( ~ year)

## annual plot (longline) ----
lcomp_old %>% 
  tidytable::left_join(lcomp_new) %>% 
  tidytable::mutate(length = ceiling(length)) %>% 
  tidytable::filter(gear == 'longline') %>% 
  tidytable::select(year, gear, length, lcomp_old = lencomp, lcomp_new = new_lencomp) %>% 
  tidytable::pivot_longer(cols = c(lcomp_old, lcomp_new)) -> dat

ggplot(data = dat, aes(x = as.numeric(length), y = value, group = name)) +
  geom_line(aes(color = name))  +
  geom_point(aes(color = name)) +
  geom_area(aes(fill = name),
            alpha = 0.3777,
            position = 'identity') +
  theme(legend.position = "top") +
  facet_wrap( ~ year)

## annual plot (pot) ----
lcomp_old %>% 
  tidytable::left_join(lcomp_new) %>% 
  tidytable::mutate(length = ceiling(length)) %>% 
  tidytable::filter(gear == 'pot') %>% 
  tidytable::select(year, gear, length, lcomp_old = lencomp, lcomp_new = new_lencomp) %>% 
  tidytable::pivot_longer(cols = c(lcomp_old, lcomp_new)) -> dat

ggplot(data = dat, aes(x = as.numeric(length), y = value, group = name)) +
  geom_line(aes(color = name))  +
  geom_point(aes(color = name)) +
  geom_area(aes(fill = name),
            alpha = 0.3777,
            position = 'identity') +
  theme(legend.position = "top") +
  facet_wrap( ~ year)


# age comps ----
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

ggplot(data = dat, aes(x = as.numeric(age), y = value, group = name)) +
  geom_line(aes(color = name))  +
  geom_point(aes(color = name)) +
  geom_area(aes(fill = name),
            alpha = 0.3777,
            position = 'identity') +
  theme(legend.position = "top") +
  facet_grid(rows = vars(gear))

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
  facet_wrap( ~ year)

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
  facet_wrap( ~ year)

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
  facet_wrap( ~ year)
