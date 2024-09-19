library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(DBI)
library(keyring)

# I get survey data from AKFIN and my credentials are stored with keyring
db <- "akfin"
channel_akfin <- DBI::dbConnect (odbc::odbc(),
                                 dsn = db,
                                 uid = keyring::key_list(db)$username,
                                 pwd =  keyring::key_get(db, keyring::key_list(db)$username))

haul <- dbGetQuery(channel_akfin, 
                   "select    CRUISEJOIN, DEPTH_GEAR_M, HAULJOIN, PERFORMANCE
                    from      gap_products.akfin_haul
                   where      performance = 0
                    ") |>  
  rename_all(tolower) 

cruise <- dbGetQuery(channel_akfin, 
                     "select    CRUISEJOIN, YEAR, SURVEY_DEFINITION_ID
                      from      gap_products.akfin_cruise
                     where      survey_definition_id = 47
                      ") |>  
  rename_all(tolower) |> 
  mutate(region = "GOA")

haul_dat <- left_join(haul, cruise) 

lls_len <- dbGetQuery(channel_akfin, 
                      "select    *
                from      afsc.lls_length_summary_view
                where     species_code = 21720 and
                          year > 1995 and
                          council_sablefish_mgmt_area in ('Central Gulf of Alaska', 'Western Gulf of Alaska', 'East Yakutat/Southeast', 'West Yakutat')
                order by  year asc") %>% 
  rename_all(tolower) |> 
  filter(!length == 999) |> 
  mutate(region = "GOA")

bts_len <- dbGetQuery(channel_akfin, 
                      "select    *
                from      gap_products.akfin_length
                where     species_code = 21720 and
                          length_mm > 0") %>% 
  rename_all(tolower) |> 
  mutate(length = length_mm / 10)

dbDisconnect(channel_akfin)

bt_len <- left_join(haul_dat, bts_len , by = c("hauljoin")) |> 
  filter(!is.na(length), !is.na(depth_gear_m), depth_gear_m > 0, region == "GOA",
         length > 69, depth_gear_m > 100, depth_gear_m < 301) |> 
  mutate(stratum_description = cut(depth_gear_m, breaks = c(100, 200, 300), 
                                   labels = c("101-200m", "201-300m")),
         len_bin = cut(length, breaks = seq(69, 114, 5), labels = seq(70,110,5))) |> 
  summarize(freq = sum(frequency), .by = c(stratum_description, len_bin)) |> 
  mutate(tot_freq = max(freq), .by = stratum_description) |>
  mutate(prop = freq / tot_freq, survey = "BTS") %>% 
  select(-tot_freq)

ll_len <- lls_len |> 
  filter(region == "GOA", length > 69, stratum_description %in% c("101-200m", "201-300m")) |> 
  mutate(len_bin = cut(length, breaks = seq(69, 114, 5), labels = seq(70,110,5))) |> 
  summarize(freq = sum(frequency), .by = c(stratum_description, len_bin)) |> 
  mutate(tot_freq = max(freq), .by = stratum_description) |>
  mutate(prop = freq / tot_freq, survey = "LLS") %>% 
  select(-tot_freq)

len_all <- bind_rows(bt_len, ll_len)

ggplot(len_all) +
  geom_col(aes(len_bin, prop, fill = survey), position = position_dodge2(width = 0.9, preserve = "single")) + 
  facet_wrap(~factor(stratum_description, levels = c("101-200m", "201-300m")))  +
  theme_bw() +
  xlab("Length bin (cm)") +
  ylab("Proportion") +
  # scale_x_discrete(breaks = c("(60,65]", "(70,75]", "(80,85]", "(90,95]")) +
  # ggtitle(paste0(name[[1]], " - GOA")) +
  guides(fill=guide_legend(title="Survey")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        text = element_text(size = 20) )

ggsave(file = paste0("results/cod_4_pete.png"), height = 7, width = 14, dpi=300)



ll_len <- vroom::vroom(here::here(2024, 'data', 'raw', 'lls_rpn_length_data.csv'), delim = ',')

twl_len <- vroom::vroom(here::here(2024, 'data', 'raw', 'twl_srvy_lpop.csv'), delim = ',')

twl_len %>% 
  filter(length > 69) |> 
  # mutate(len_bin = cut(length, breaks = seq(69, 114, 1), labels = seq(70,110,1))) |> 
  summarize(num = sum(num), .by = len_bin) |>
  mutate(prop = num / max(num), survey = "BTS") |> 
  select(-num) %>% 
  bind_rows(ll_len %>% 
              filter(length > 69) |> 
              # mutate(len_bin = cut(length, breaks = seq(69, 114, 1), labels = seq(70,110,1))) |> 
              summarize(rpn = sum(rpn), .by = len_bin) |>
              mutate(prop = rpn / max(rpn), survey = "LLS") |> 
              select(-rpn)) -> dat


ggplot(dat) +
  geom_col(aes(length, prop, fill = survey), position = position_dodge2(width = 0.9, preserve = "single")) + 
  # facet_wrap(~factor(stratum_description, levels = c("101-200m", "201-300m")))  +
  
  theme_bw(base_size = 15) +
  xlab("Length bin (cm)") +
  ylab("Proportion") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        text = element_text(size = 20) )







