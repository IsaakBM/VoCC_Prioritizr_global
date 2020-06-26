library(raster)
library(dplyr)
library(sf)
library(ggplot2)

long <- st_read("shapefiles_rasters/LonghurstProvinces/Longhurst_world_v4_2010.shp") %>% 
  st_transform(crs = CRS("+proj=moll +lon_0=0 +datum=WGS84 +units=m +no_defs"))

glasgow_prov <- st_read("shapefiles_rasters/GlasgowProvinces/GlasgowMesopelagicProvinces_v1_2017.shp") %>% 
  st_transform(crs = CRS("+proj=moll +lon_0=0 +datum=WGS84 +units=m +no_defs"))

goods_prov <- st_read("shapefiles_rasters/GOODSprovinces/GOODSprovinces_abyssal.shp") %>% 
  st_transform(crs = CRS("+proj=moll +lon_0=0 +datum=WGS84 +units=m +no_defs"))

ggplot() +
  geom_sf(data = long, size = 0.05) +
  ggtitle("Longhurst Provinces") +
  ggsave("ypdfs/LonghurstProvinces.pdf", width = 20, height = 15, dpi = 300)

ggplot() +
  geom_sf(data = glasgow_prov, size = 0.05) +
  ggtitle("Glasgow Provinces") +
  ggsave("ypdfs/GlasgowProvinces.pdf", width = 20, height = 15, dpi = 300)

ggplot() +
  geom_sf(data = goods_prov, size = 0.05) +
  ggtitle("Glasgow Provinces") +
  ggsave("ypdfs/GOODSprovinces.pdf", width = 20, height = 15, dpi = 300)



sf_pu <- st_read("shapefiles_rasters/abnj_01-epipelagic_global_moll_05deg/abnj_01-epipelagic_global_moll_05deg.shp")
prov_code <- as.character(long$ProvCode)

long_list <- list()
for(i in 1:length(prov_code)) {
  single <- long %>% 
    filter(ProvCode == prov_code[i])
  dt1 <- st_crop(sf_pu, single)
  long_list[[i]] <- dt1 %>% 
    mutate(long_prov = prov_code[i])
}

# plot(st_geometry(long_list[[53]]))
pus_long <- do.call(rbind, long_list) %>% 
  arrange(layer)
sf_pu$long_prov <- pus_long$long_prov[match(sf_pu$layer, pus_long$layer)]
sf_pu$long_prov <- ifelse(is.na(sf_pu$long_prov), "non-categ", sf_pu$long_prov)

length(unique(sf_pu$layer))
length(unique(pus_long$layer))


# ggplot() +
#   geom_sf(data = sf_pu, size = 0.05) +
#   ggtitle("Longhurst Provinces by pu 0.5° ABNJ") +
#   theme_opts3 +
#   ggsave("ypdfs/abnj_LonghurstProvinces_05deg_all-categ.pdf", width = 20, height = 15, dpi = 300)



