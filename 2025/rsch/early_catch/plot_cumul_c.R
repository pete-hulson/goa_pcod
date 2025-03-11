# script to plot 2025 cumulative catch
library(afscdata)
library(tidyverse)

# query data
# get connected
db = 'akfin'
conn = afscdata::connect(db)  

# query catch data and write raw data to folder ----
afscdata::setup_folders(year = 2025)
afscdata::q_catch(year = 2025,
                  species = "PCOD",
                  area = c("CG","PWSI","SE","SEI","WG","WY"),
                  db = conn,
                  add_fields = c("akr_state_fishery_flag", "vessel_id"))


dat <- vroom::vroom(here::here(new_year, "data", "raw", "fish_catch_data.csv"), 
                    progress = FALSE, 
                    show_col_types = FALSE) %>% 
  tidytable::mutate(week = as.numeric(as.character(format(as.Date(week_end_date), "%W"))))
problems(dat)


# get plot data
new_year = 2025
tidytable::expand_grid(year = c(2019, seq(new_year - 3, new_year-1)),
                       gear = c("Longline", "Jig", "Pot", "Trawl"), 
                       area = c("Central gulf", "Western gulf"),
                       week = 0:52) %>% 
  tidytable::bind_rows(expand.grid(year = new_year, 
                                   gear = c("Longline", "Jig", "Pot", "Trawl"), 
                                   area = c("Central gulf", "Western gulf"),
                                   week = 0:as.numeric(format(Sys.Date(), format = "%W")))) %>% 
  tidytable::left_join(vroom::vroom(here::here(new_year, "data", "raw", "fish_catch_data.csv"), 
                                    progress = FALSE, 
                                    show_col_types = FALSE) %>% 
                         tidytable::mutate(week = as.numeric(as.character(format(as.Date(week_end_date), "%W")))) %>% 
                         tidytable::filter(year %in% c(2019, seq(new_year - 3, new_year)),
                                           fmp_subarea %in% c("CG", "WG")) %>% 
                         tidytable::mutate(gear = case_when(fmp_gear == "HAL" ~ "Longline",
                                                            fmp_gear == "JIG" ~ "Jig",
                                                            fmp_gear == "OTH" ~ "Other",
                                                            fmp_gear == "POT" ~ "Pot",
                                                            fmp_gear == "TRW" ~ "Trawl"),
                                           area = case_when(fmp_subarea == "CG" ~ "Central gulf",
                                                            fmp_subarea == "WG" ~ "Western gulf")) %>% 
                         tidytable::summarise(tons = sum(weight_posted), .by = c(week, gear, year, area))) %>% 
  tidytable::mutate(tons = replace_na(tons, 0)) %>% 
  tidytable::mutate(catch = cumsum(tons), .by = c(year, gear, area)) %>% 
  tidytable::filter(week <= 15) -> cumul_catch

ggplot(data = cumul_catch, 
                     aes(x = week, y = catch, color = factor(year))) + 
  geom_point() + 
  geom_path(aes(group = year)) +
  facet_grid(gear ~ area, scale = "free_y") +
  theme_bw(base_size = 14) +
  theme(axis.text.x = element_text(vjust = 0.5, angle = 90)) +
  scico::scale_color_scico_d(palette = 'roma') +
  labs(x = "Week", y = "Cummulative Catch (t)", color = "Year")

ggsave(filename = "cumul_catch.png",
       path = here::here(new_year, "rsch", "early_catch"),
       width = 6.5,
       height = 7,
       units = "in")
