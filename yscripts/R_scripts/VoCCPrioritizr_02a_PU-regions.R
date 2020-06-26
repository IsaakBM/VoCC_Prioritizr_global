library(raster)
library(dplyr)
library(sf)
library(ggplot2)

long <- st_read("shapefiles_rasters/LonghurstProvinces/Longhurst_world_v4_2010.shp") %>% 
  st_transform(crs = CRS("+proj=moll +lon_0=0 +datum=WGS84 +units=m +no_defs"))
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

length(unique(sf_pu$layer))
length(unique(pus_long$layer)) # less provinces because st_crop CUT things...



ggplot() +
  geom_sf(data = pus_long, size = 0.05) +
  ggtitle("Longhurst Provinces by pu 0.5° ABNJ") +
  theme_opts3 +
  ggsave("ypdfs/abnj_LonghurstProvinces_05deg.pdf", width = 20, height = 15, dpi = 300)


ggplot() +
  geom_sf(data = dt1, size = 0.05) +
  ggtitle("Longhurst Provinces by pu 0.5° ABNJ") +
  theme_opts3 +
  ggsave("ypdfs/abnj_LonghurstProvinces_BPLR_05deg.pdf", width = 20, height = 15, dpi = 300)


# not categorized by provinces: give those polygons a name... like "not category?" [sounds like a good idea!]
