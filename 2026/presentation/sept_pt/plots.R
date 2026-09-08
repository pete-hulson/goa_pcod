# plots for september 26 plan team

# load libraries
library(tidyverse)
# pak::pak("nmfs-ost/nmfspalette")
library(nmfspalette)

# load data
cod_data <- readRDS(file = here::here('2026', 'rsch', 'wgoa', 'sporc_data', 'cod_data.rds'))


names(cod_data)

# plot catch data ----
catch <- cod_data$ObsCatch %>% 
  tidytable::rename(Year = year, Catch = catch, Season = seas) %>% 
  tidytable::mutate(region = factor(region, levels = c('bs', 'wgoa', 'cgoa'))) %>% 
  tidytable::mutate(prop_seas = Catch / sum(Catch), .by = c(Year, region))

# catch by region/season
ggplot(catch, aes(x = Year, y = Catch, fill = Season)) +
  geom_col(color = "black") +
  facet_wrap(~region) +
  theme_bw() +
  ylab("Catch (t)") +
  scale_fill_nmfs(palette = "waves")

# proportion of catch within season
ggplot(catch, aes(x = Year, y = prop_seas, fill = Season, shape = region)) +
  geom_col(color = "black") +
  facet_wrap(~region) +
  theme_bw() +
  ylab("Proportion of seasonal catch") +
  scale_fill_nmfs(palette = "waves")

# plot index data ----
index <- cod_data$ObsSrvIdx %>% 
  tidytable::mutate(prop_indx = index / sum(index), .by = c(year)) %>% 
  tidytable::left_join(cod_data$ObsSrvIdx_SE) %>% 
  tidytable::mutate(uci = index + 1.96 * se,
                    lci = index - 1.96 * se,
                    region = factor(region, levels = c('bs', 'wgoa', 'cgoa')))


ggplot(index, aes(x = year, y = index, color = region, shape = region)) +
  geom_line(linetype = "dashed") +
  geom_linerange(aes(ymin = lci, ymax = uci)) +
  geom_point() +
  facet_wrap(~region) +
  theme_bw() +
  ylab("Survey index (#'s)") +
  xlab("Year") +
  scale_color_nmfs(palette = "waves")

ggplot(index %>% tidytable::filter(prop_indx < 1, year < 2024), 
       aes(x = year, y = prop_indx, fill = region, shape = region)) +
  geom_col(color = "black") +
  theme_bw() +
  ylab("Regional proportion of survey index") +
  xlab("Year") +
  scale_fill_nmfs(palette = "waves")

# plot comps ----

# aggregated fishery age comp
agg_fa_comp <- cod_data$ObsFishAgeComps %>% 
  tidytable::pivot_longer(cols = as.character(1:10), names_to = 'age', values_to = 'comp') %>% 
  tidytable::summarise(agg_comp = mean(comp), .by = c(age, region, seas)) %>% 
  tidytable::mutate(agg_comp = agg_comp / sum(agg_comp), .by = c(region, seas)) %>% 
  tidytable::mutate(region = factor(region, levels = c('bs', 'wgoa', 'cgoa')),
                    age = factor(age, levels = 1:10))

ggplot(agg_fa_comp, aes(x = age, y= agg_comp, fill = region)) +
  geom_col(color = 'black') +
  facet_grid(seas ~ region) +
  ylab("Aggregated fishery age composition") +
  xlab("Age") +
  theme_bw() +
  scale_fill_nmfs(palette = "waves")

# annual fishery age comp

ex_fa_comp <- cod_data$ObsFishAgeComps %>% 
  tidytable::pivot_longer(cols = as.character(1:10), names_to = 'age', values_to = 'comp') %>% 
  tidytable::mutate(region = factor(region, levels = c('bs', 'wgoa', 'cgoa')),
                    age = factor(age, levels = 1:10)) %>% 
  tidytable::filter(year > 2020)

# a season
ggplot(ex_fa_comp %>% tidytable::filter(seas == 'A'),
       aes(x = age, y= comp, fill = region)) +
  geom_col(color = 'black') +
  facet_grid(year ~ region) +
  ylab("A season fishery age composition") +
  xlab("Age") +
  theme_bw() +
  scale_fill_nmfs(palette = "waves")

# b season
ggplot(ex_fa_comp %>% tidytable::filter(seas == 'B'),
       aes(x = age, y= comp, fill = region)) +
  geom_col(color = 'black') +
  facet_grid(year ~ region) +
  ylab("B season fishery age composition") +
  xlab("Age") +
  theme_bw() +
  scale_fill_nmfs(palette = "waves")


# aggregated survey age comp
agg_sa_comp <- cod_data$ObsSrvAgeComps %>% 
  tidytable::pivot_longer(cols = as.character(1:10), names_to = 'age', values_to = 'comp') %>% 
  tidytable::summarise(agg_comp = mean(comp), .by = c(age, region)) %>% 
  tidytable::mutate(agg_comp = agg_comp / sum(agg_comp), .by = c(region)) %>% 
  tidytable::mutate(region = factor(region, levels = c('bs', 'wgoa', 'cgoa')),
                    age = factor(age, levels = 1:10))

ggplot(agg_sa_comp, aes(x = age, y= agg_comp, fill = region)) +
  geom_col(color = 'black') +
  facet_grid(~ region) +
  ylab("Aggregated survey age composition") +
  xlab("Age") +
  theme_bw() +
  scale_fill_nmfs(palette = "waves")


# annual survey age comp
ex_sa_comp <- cod_data$ObsSrvAgeComps %>% 
  tidytable::pivot_longer(cols = as.character(1:10), names_to = 'age', values_to = 'comp') %>% 
  tidytable::mutate(region = factor(region, levels = c('bs', 'wgoa', 'cgoa')),
                    age = factor(age, levels = 1:10)) %>% 
  tidytable::filter(year %in% c(2017, 2019, 2021, 2023))

ggplot(ex_sa_comp, aes(x = age, y= comp, fill = region)) +
  geom_col(color = 'black') +
  facet_grid(year ~ region) +
  ylab("Recent survey age composition") +
  xlab("Age") +
  theme_bw() +
  scale_fill_nmfs(palette = "waves")

names(cod_data)


# growth ----
waa <- cod_data$WAA %>% 
  tidytable::filter(seas == 'B') %>% 
  tidytable::mutate(age = case_when(region == 'bs' ~ as.numeric(age) - 0.2,
                                    region == 'wgoa' ~ as.numeric(age),
                                    region == 'cgoa' ~ as.numeric(age) + 0.2)) %>% 
  tidytable::mutate(region = factor(region, levels = c('bs', 'wgoa', 'cgoa')))


goa_spec_srvy <- vroom::vroom(here::here(2026, 'rsch', 'wgoa', 'data', 'srvy_data', 'goa', 'specimen_wt.csv'))
bs_spec_srvy <- vroom::vroom(here::here(2026, 'rsch', 'wgoa', 'data', 'srvy_data', 'nebs', 'specimen_wt.csv'))

spec_data <- goa_spec_srvy %>% 
    tidytable::left_join(vroom::vroom(here::here(2026, 'rsch', 'wgoa', 'data', 'srvy_data', 'goa', 'strata.csv')) %>%
                           tidytable::mutate(region = case_when(area_id == 805 ~ 'wgoa',
                                                                area_id < 805 ~ 'cgoa')) %>%
                           tidytable::select(stratum, region)) %>% 
  tidytable::select(year, region, age, weight, length) %>% 
  tidytable::bind_rows(bs_spec_srvy %>% 
                         tidytable::mutate(region = 'bs') %>% 
                         tidytable::select(year, region, age, weight, length)) %>% 
  tidytable::filter(region != 'egoa',
                    age <= 10) %>% 
  tidytable::drop_na() %>% 
  tidytable::mutate(age = case_when(region == 'bs' ~ age - 0.2,
                                    region == 'wgoa' ~ age,
                                    region == 'cgoa' ~ age + 0.2)) %>% 
  tidytable::mutate(region = factor(region, levels = c('bs', 'wgoa', 'cgoa')))


ggplot(waa, aes(x = age, y = waa, color = region, group = region, shape = region)) +
  geom_point(data = spec_data, aes(x = age, y = weight),
             position = position_jitter(width = 0.05, height = 0, seed = 123),
             alpha = 0.1) +
  geom_line() +
  geom_point(color = 'black') +
  theme_bw() +
  ylab("Weight (kg)") +
  xlab("Age") +
  scale_color_nmfs(palette = "waves")




szaa <- spec_data %>% 
  tidytable::summarise(szaa = mean(length), .by = c(region, age)) %>% 
  tidytable::mutate(region = factor(region, levels = c('bs', 'wgoa', 'cgoa')))

ggplot(szaa, aes(x = age, y = szaa, color = region, group = region, shape = region)) +
  geom_point(data = spec_data, aes(x = age, y = length),
             position = position_jitter(width = 0.05, height = 0, seed = 123),
             alpha = 0.1) +
  geom_line() +
  geom_point(color = 'black') +
  theme_bw() +
  ylab("Length (cm)") +
  xlab("Age") +
  scale_color_nmfs(palette = "waves")


