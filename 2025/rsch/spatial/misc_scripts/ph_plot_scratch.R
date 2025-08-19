
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

