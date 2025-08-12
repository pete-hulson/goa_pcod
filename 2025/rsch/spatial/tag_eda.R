# exploratory data analysis of pcod tagging data


# library(remotes)
# install_github("afsc-gap-products/akgfmaps", build_vignettes = TRUE)
# devtools::install_github("afsc-gap-products/akgfmaps")
# library(akgfmaps)


library(tidyverse)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(raster)
library(geodata)

# prep tag data ----

## steve data (this data came from steve in email on 1-16-25) ----
sb_data <- vroom::vroom(here::here('2025', 'rsch', 'spatial', 'raw_data_SB', 'ALL_TAG_DATA2025.csv'), delim = ',') %>% 
  dplyr::rename_all(tolower) %>% 
  tidytable::mutate(recdt = as.Date(recdt, format = '%m/%d/%Y'),
                    reldt = as.Date(reldt, format = '%m/%d/%Y'),
                    lib_time = as.numeric(recdt - reldt)) %>% 
  # filter out duplicated tags (these ones have same release location but different recovery locations)
  tidytable::filter(n() == 1, .by = c(tag_number, reldt)) %>% 
  tidytable::select(tag_number, rel_month, rel_year, reldt, rel_region, rel_lat = lat_rel, rel_lon = long_rel,
                    rec_month, rec_year, recdt, rec_region, rec_lat = lat_rec, rec_lon = long_rec, lib_time) 
## PACT data ----
# this data comes from PACTbase (date pulled is in the file name)
rel_sat <- vroom::vroom(here::here('2025', 'rsch', 'spatial', 'raw_data_PACT', 'rel_sat.csv'), delim = ',')
rec_sat <- vroom::vroom(here::here('2025', 'rsch', 'spatial', 'raw_data_PACT', 'rec_sat.csv'), delim = ',')

rel_sat %>% 
  dplyr::rename_all(tolower) %>% 
  tidytable::mutate(reldt = as.Date(rel_time_local, format = '%m/%d/%Y'),
                    rel_month = lubridate::month(reldt),
                    rel_year = lubridate::year(reldt)) %>% 
  tidytable::select(tag_number = tag_num, rel_month, rel_year, reldt, rel_region, rel_lat = rel_latdd, rel_lon = rel_longdd) %>% 
  tidytable::left_join(rec_sat %>% 
                         dplyr::rename_all(tolower) %>% 
                         tidytable::mutate(recdt = as.Date(rec_time_utc, format = '%m/%d/%Y'),
                                           rec_month = lubridate::month(recdt),
                                           rec_year = lubridate::year(recdt)) %>% 
                         tidytable::select(tag_number = tag_num, rec_month, rec_year, recdt, rec_region, rec_lat, rec_lon = rec_lng)) %>% 
  tidytable::drop_na() %>% 
  tidytable::mutate(lib_time = as.numeric(recdt - reldt)) -> pact_data
  
# remove the duplicated sat tags and keep ones from PACT data
sb_data %>%
  tidytable::bind_rows(pact_data) %>% 
  tidytable::filter(n() > 1, .by = c(tag_number, reldt)) %>% 
  pull(tag_number) %>% 
  unique() -> dup_tags

## combine tag data ----
sb_data %>% 
  tidytable::filter(!(tag_number %in% dup_tags)) %>%
  tidytable::bind_rows(pact_data) %>% 
  # filter time @ liberty > 30 days
  tidytable::filter(lib_time > 30) -> tag_data

# add subregion info when release location is 'GOA'
tag_data %>% 
  tidytable::filter(rel_region != 'GOA') %>% 
  tidytable::bind_rows(tag_data %>% 
                         tidytable::filter(rel_region == 'GOA') %>% 
                         tidytable::mutate(rel_region = case_when(rel_lon >= -170 & rel_lon <= -159 ~ 'WGOA',
                                                                  rel_lon > -159 & rel_lon <= -147 ~ 'CGOA',
                                                                  .default = 'EGOA'))) -> .tag_data
.tag_data %>% 
  tidytable::filter(rec_region != 'GOA') %>% 
  tidytable::bind_rows(.tag_data %>% 
                         tidytable::filter(rec_region == 'GOA') %>% 
                         tidytable::mutate(rec_region = case_when(rec_lon >= -170 & rec_lon <= -159 ~ 'WGOA',
                                                                  rec_lon > -159 & rec_lon <= -147 ~ 'CGOA',
                                                                  .default = 'EGOA'))) -> tag_data
# movement cases ----

# overall
tag_data %>% 
  count(rel_region, rec_region) -> all

# released during spawning season
tag_data %>% 
  tidytable::filter(rel_month <= 3) %>% 
  count(rel_region, rec_region) -> spawn

# released outside spawning season
tag_data %>% 
  tidytable::filter(rel_month > 3) %>%
  count(rel_region, rec_region) -> not_spawn





# testing below here ----


# play around with maps ----
# plot
lon = c(-168, -145)
lat = c(53, 60)

geodata::country_codes() %>% 
  tidytable::filter(continent == 'North America') %>% 
  print(n=100)


terr <- geodata::elevation_global(res = 5, path = here::here('2025', 'rsch', 'spatial'))
# terr <- geodata::elevation_30s(country = "USA", path = here::here('2025', 'rsch', 'spatial'))
# 
# 
# geodata::elevation_3s(lon = lon, lat = lat, path = here::here('2025', 'rsch', 'spatial'))

inpfc_areas <- akgfmaps::get_inpfc_strata(select.region = "goa",
                                          set.crs = "EPSG:3338")
nmfs_areas <- akgfmaps::get_nmfs_areas(set.crs = "EPSG:3338")
goa_layers <- akgfmaps::get_base_layers(select.region = "goa",
                                        set.crs = "EPSG:3338")

terr_df <- terra::as.data.frame(terr, xy = TRUE)
str(terr_df)

ggplot() +
  # geom_sf(data = nmfs_areas) +
  # geom_sf(data = rnaturalearth::ne_countries(scale = "large", returnclass = "sf")) +
  geom_tile(data = terr_df, aes(x = x, y = y, fill = wc2.1_5m_elev)) +
  # geom_sf(data = nmfs_areas) +
  # geom_sf(data = goa_layers$survey.strata[goa_layers$survey.strata$STRATUM %in% wgoa_strat,],
  #         aes(fill = as.factor(STRATUM))) +
  coord_sf(xlim = c(-175, -145), ylim = c(50, 62), expand = FALSE)
# scico::scale_fill_scico_d(palette = 'roma') +
# geom_sf_text(data = goa_layers$survey.strata[goa_layers$survey.strata$STRATUM %in% wgoa_strat,],
#              mapping = aes(label = STRATUM)) +
# labs(fill = 'stratum')




# plot the tag data using steve's code ----
library(sf)
library(ggplot2)

DATA4<-read.csv("ALL_TAG_DATA2025.csv")

shapefile <- sf::read_sf(getwd(), "ALASKA_LAND")

shapefile <- sf::st_transform(shapefile, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))


map <- ggplot() +
  geom_sf(data = shapefile)+xlim(-180,-145)+ylim(50,65)

d <- map+geom_segment(data=DATA4, aes(y=LAT_REL,x=LONG_REL,yend=LAT_REC,xend=LONG_REC, group =INDEX,color=as.numeric(DAL)),arrow = arrow(length=unit(0.2,"cm"),type = "closed"))  

d<- d+ggtitle("All recovered Pacific cod tags")+theme1(base_size=20)+xlab("Longitude")+ylab("Latitude")+scale_colour_gradient(name="Days at liberty",breaks=round(seq(0,2500,length=10),-2),limits=c(0,2500),low="light blue",high="dark red")

print(d)

