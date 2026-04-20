remotes::install_github("zandjyo/DR4SS")
# remotes::install_github("jabbamodel/ss3diags")


# DR4SS::launch_DR4SS()


library(DR4SS)
library(tidyverse)

# get connected
db = 'akfin'
con_akfin = afscdata::connect(db)  
species = 202
# species = "PCOD"
region_def = list(BS = 500:539, WGOA = c(610, 620), CGOA = c(630, 640))
season_def = list(A = 1:3, B = 4:12)
start_year = 1976
end_year = 2025
wgoa_cod = TRUE



season_q <- list(A = c(1:3), B = c(4:12))
region_q <- list(BS = c(500:539), WGOA = 610, CGOA = c(620:649))

catch <- get_blend_catch_region(con_akfin,
                                species = 202,
                                region_def = region_q,
                                season_def = season_q,
                                start_year = 1990,
                                end_year = 2025,
                                wgoa_cod = FALSE)


catch_seas <- build_catch_season_from_blend(blend_catch = catch,
                                            by_gear = FALSE,
                                            complete_grid = TRUE)





season_q <-list(A = c(1:3), B = c(4:12))

region_test <-list(WGOA = 610, CGOA = 620)

catch <- get_blend_catch_region(con_akfin,
                                species = 202,
                                region_def = region_test,
                                season_def = season_q,
                                start_year = 2003,
                                end_year = 2025,
                                wgoa_cod = FALSE)


catch_seas <- build_catch_season_from_blend(blend_catch = catch,
                                            by_gear = FALSE,
                                            complete_grid = TRUE)

catch2 <- get_blend_catch_region(con_akfin,
                                 species = 202,
                                 region_def = region_test,
                                 season_def = season_q,
                                 start_year = 2003,
                                 end_year = 2025,
                                 wgoa_cod = TRUE)


catch_seas2 <- build_catch_season_from_blend(blend_catch = catch2,
                                             by_gear = FALSE,
                                             complete_grid = TRUE)


catch_seas2 %>% 
  dplyr::rename_all(tolower) %>% 
  tidytable::filter(region_grp == 'CGOA') %>% 
  tidytable::rename(catch_cgoa = catch) %>% 
  tidytable::select(-c(p_q, region_grp)) %>%
  tidytable::left_join(catch_seas2 %>% 
                         dplyr::rename_all(tolower) %>% 
                         tidytable::filter(region_grp == 'WGOA') %>% 
                         tidytable::rename(catch_wgoa = catch) %>% 
                         tidytable::select(-c(p_q, region_grp))) %>% 
  tidytable::mutate(prop_cgoa = catch_cgoa / catch_wgoa) -> test2

ggplot(test2, aes(x = season, y = prop_cgoa, fill = season)) +
  geom_boxplot() +
  theme_bw() +
  labs(y="Proportion of catch from 620 east of -158") +
  ylim(0, 1)




catch_seas %>% 
  dplyr::rename_all(tolower) %>% 
  tidytable::rename(catch_sep = catch) %>% 
  tidytable::select(-p_q) %>% 
  tidytable::left_join(catch_seas2 %>% 
                         dplyr::rename_all(tolower) %>% 
                         tidytable::rename(catch_kludge = catch) %>% 
                         tidytable::select(-p_q)) %>% 
  tidytable::mutate(diff = catch_sep - catch_kludge) %>% 
  tidytable::filter(region_grp == 'CGOA') %>% 
  tidytable::mutate(p_q = diff / catch_sep) -> test


ggplot(test, aes(x = year, y = p_q, color = season, fill = season)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  geom_smooth(method = "lm", alpha = 0.15) +
  labs(y="Proportion of catch from 620 west of -158") +
  ylim(0, 1)

ggplot(test, aes(x = season, y = p_q, fill = season)) +
  geom_boxplot() +
  theme_bw() +
  labs(y="Proportion of catch from 620 west of -158") +
  ylim(0, 1)




CGOA2<-catch_seas2[REGION_GRP=='CGOA']
CGOA<-catch_seas[REGION_GRP=='CGOA']
CGOA2$DIF<-CGOA$CATCH-CGOA2$CATCH
CGOA2$PROP<-CGOA2$DIF/CGOA$CATCH

ggplot(CGOA2,aes(x=YEAR,y=PROP,color=SEASON,fill=SEASON))+geom_point()+geom_line()+theme_bw()+geom_smooth(method="lm",alpha=0.15)+labs(y="Proportion of catch from 620 west of -158")

ggplot(CGOA2,aes(x=YEAR,y=DIF,color=SEASON,fill=SEASON))+geom_point()+geom_line()+theme_bw()+geom_smooth(method="lm",alpha=0.15)+labs(y="Catch from 620 west of -158")









library(sf)
library(ggplot2)
library(dplyr)

# 1. Unzip the file if you haven't already
unzip("ADFG_areas.zip", exdir = "ADFG_data")

# 2. Load the shapefile
# Note: Point to the .shp file; ensure .dbf, .shx, and .prj are in the same folder
adfg_shapes <- st_read("ADFG_data/ADFG_areas.shp")

# 3. Transform to WGS84 (Longitude/Latitude) to use -155 and -170
adfg_wgs84 <- st_transform(adfg_shapes, crs = 4326)

# 4. Filter for the specific longitude range
# We use st_crop to "clip" the map to your window
map_subset <- st_crop(adfg_wgs84, xmin = -170, xmax = -155, ymin = 50, ymax = 58)

# 5. Plot with Area Numbers
ggplot(data = map_subset) +
  geom_sf(fill = "aliceblue", color = "darkgrey") +
  # geom_sf_text adds the STATAREA labels at the center of each polygon
  geom_sf_text(aes(label = STATAREA), size = 3, check_overlap = TRUE) +
  coord_sf(xlim = c(-170, -155)) +
  theme_minimal() +
  labs(
    title = "ADF&G Statistical Areas",
    subtitle = "Range: -170° to -155° West",
    x = "Longitude",
    y = "Latitude"
  )











surv_ages <- vroom::vroom(here::here('2025', 'data', 'raw', 'twl_srvy_age.csv'), delim = ',')



surv_ages %>% 
  tidytable::filter(long_mid <= -156,
                    long_mid >= -159) %>% 
  tidytable::summarise(ages = .N, .by = c(year)) %>% 
  tidytable::left_join(surv_ages %>% 
  tidytable::summarise(ages_tot = .N, .by = c(year))) %>% 
  tidytable::mutate(p_sliv = ages / ages_tot)











