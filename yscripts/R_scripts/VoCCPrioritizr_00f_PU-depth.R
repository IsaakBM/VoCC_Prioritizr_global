library(raster)
library(sf)
library(exactextractr)
library(dplyr)

proj.geo = "+proj=moll +lon_0=0 +datum=WGS84 +units=m +no_defs"
etopo <- readAll(raster("shapefiles_rasters/ETOPO1/ETOPO1_ocean.grd"))
crs(etopo) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
weight_rs <- readAll(raster::area(etopo))
crs(weight_rs) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

etopo_1 <- projectRaster(etopo, crs = CRS(proj.geo), method = "ngb", over = FALSE)
names(etopo_1) <- "layer"
writeRaster(etopo_1, "shapefiles_rasters/ETOPO1/ETOPO1_moll_ocean.tif")
weight_rs <- projectRaster(weight_rs, crs = CRS(proj.geo), method = "ngb", over = FALSE)
writeRaster(weight_rs, "shapefiles_rasters/ETOPO1/ETOPO1_weights-moll_ocean.tif")

ep <- st_read("shapefiles_rasters/abnj_02-epipelagic_global_moll_05deg/abnj_02-epipelagic_global_moll_05deg.shp")
mp <- st_read("shapefiles_rasters/abnj_03-mesopelagic_global_moll_05deg/abnj_03-mesopelagic_global_moll_05deg.shp")
bap <- st_read("shapefiles_rasters/abnj_04-bathyabysso_global_moll_05deg/abnj_04-bathyabysso_global_moll_05deg.shp")


etopo_1 <- readAll(raster("shapefiles_rasters/ETOPO1/ETOPO1_moll_ocean.tif"))
crs(etopo_1) <- CRS("+proj=moll +lon_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

ep_etopo <- exact_extract(etopo_1, ep, "weighted_mean", weights = weight_rs)

ep_final <- ep %>% 
  arrange(layer) %>% 
  mutate(depth = ep_etopo) %>% 
  mutate(depth = ifelse(is.na(depth), 0, depth))
st_write(ep_final, dsn = "shapefiles_rasters/", layer = "abnj_02-epipelagic_global_moll_05deg_depth", driver = "ESRI Shapefile")

mp_final <- mp %>%
  arrange(layer) %>% 
  mutate(depth = ep_etopo) %>% 
  mutate(depth = ifelse(is.na(depth), 0, depth)) %>% 
  filter(depth < -200)
range(mp_final$depth)  
plot(st_geometry(mp_final))
st_write(mp_final, dsn = "shapefiles_rasters/", layer = "abnj_03-mesopelagic_global_moll_05deg_depth", driver = "ESRI Shapefile")

bap_final <- bap %>%
  arrange(layer) %>% 
  mutate(depth = ep_etopo) %>% 
  mutate(depth = ifelse(is.na(depth), 0, depth)) %>% 
  filter(depth < -1000)
st_write(bap_final, dsn = "shapefiles_rasters/", layer = "abnj_04-bathyabysso_global_moll_05deg_depth", driver = "ESRI Shapefile")
