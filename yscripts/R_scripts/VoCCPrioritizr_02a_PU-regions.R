library(raster)
library(dplyr)
library(sf)

long <- st_read("shapefiles_rasters/LonghurstProvinces/Longhurst_world_v4_2010.shp")
sf_pu <- st_read("shapefiles_rasters/abnj_01-epipelagic_global_moll_05deg/abnj_01-epipelagic_global_moll_05deg.shp")

prov_code <- as.character(long$ProvCode)
