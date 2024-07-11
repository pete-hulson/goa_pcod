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

source(here::here(2024, "R", "get_data", "get_catch_len_goa_pcod.r"))


new_fsh_comp <- get_catch_len(new_year = 2024,
                              fsh_sp_code = 202,
                              query = TRUE,
                              database = 'afsc',
                              fltr = TRUE)

new_fsh_comp_wofltr <- get_catch_len(new_year = 2024,
                                     fsh_sp_code = 202,
                                     query = FALSE,
                                     database = 'afsc',
                                     fltr = FALSE)


new_fsh_comp %>% 
  pivot_longer(cols = as.character(seq(1, 117)), names_to = 'length', values_to = 'prop') %>% 
  mutate(prop_t = sum(prop), .by = c(year, gear)) %>% 
  mutate(prop_new = prop / prop_t,
         length = as.numeric(length)) %>% 
  select(year, gear, length, prop_new) %>% 
  left_join(new_fsh_comp_wofltr %>% 
              pivot_longer(cols = as.character(seq(1, 117)), names_to = 'length', values_to = 'prop') %>% 
              mutate(prop_t = sum(prop), .by = c(year, gear)) %>% 
              mutate(prop_new_wofltr = prop / prop_t,
                     length = as.numeric(length)) %>% 
              select(year, gear, length, prop_new_wofltr)) %>% 
  pivot_longer(cols = c(prop_new, prop_new_wofltr)) %>% 
  filter(year >= 2019, gear == 'trawl') -> dat1

ggplot(data = dat1, aes(x = as.numeric(length), y = value, group = name)) +
  geom_line(aes(color = name))  +
  geom_point() +
  facet_wrap(gear ~ year)


