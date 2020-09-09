library(raster)
library(sf)
library(dplyr)
library(rnaturalearth)
library(rnaturalearthdata)
library(fasterize)
library(ggplot2)

##### Create a global polygon for ABNJ 
# global <- st_read("files/abnj_02/abnj_02.shp") %>% 
#   summarise(total_layer = sum(layer, do_union = TRUE))
# st_write(global, dsn = "files/global-poly_abnj_01", driver = "ESRI Shapefile")

##### indian ocean for Rosa
# etopo <- readAll(raster("files/ETOPO1_05deg/ETOPO1_ocean.grd"))
# plot(etopo)
# drawExtent()

proj_indian <- "+proj=laea +lon_0=85.78 +lat_0=-1.63 +datum=WGS84 +units=m +no_defs"
box_indian <- c(xmin = 20.00262, ymin = -60, xmax = 146.8982, ymax = 31.18586) %>%
  st_bbox()
indian <- st_read("files/global-poly_abnj_01/global-poly_abnj_01.shp") %>% 
  st_crop(box_indian) %>% 
  st_transform(crs = CRS(proj_indian))

grid_spacing <- 50000  # size of squares, in units of the CRS (i.e. meters for 5514)
pus_indian <- st_make_grid(indian, square = F, cellsize = c(grid_spacing, grid_spacing)) %>% # the grid, covering bounding box
  st_sf() # not really required, but makes the grid nicer to work with later
pus_indian <- pus_indian %>% 
  mutate(layer = as.numeric(seq(1,nrow(pus_indian)))) %>% 
  arrange(layer)
  
# plot(st_geometry(pus_indian))
nrow(pus_indian) # 25143 polygons... aprox 13ks area of 2.2km2
st_write(pus_indian, dsn = "files/indian-poly_abnj_02", driver = "ESRI Shapefile")

# pus_indian2 <- pus_indian %>% 
#   st_transform(crs = CRS(geo.prj))
pus_area <- round(st_area(pus_indian)/1e+06)
range(pus_area)
hist(pus_area)


# plotting to see what we have :-)
theme_opts3 <- list(theme(# panel.grid.minor = element_blank(),
                          # panel.grid.major = element_blank(),
                          # panel.background = element_rect(fill = "white", colour = "black"),
                          # plot.background = element_rect(fill = "white", colour = "black"),
                          panel.border = element_blank(),
                          axis.line = element_line(size = 1),
                          axis.text.x = element_text(size = rel(2), angle = 0),
                          axis.text.y = element_text(size = rel(2), angle = 0),
                          axis.ticks = element_line(size = 1.5),
                          axis.ticks.length = unit(.25, "cm"), 
                          axis.title.x = element_blank(),
                          axis.title.y = element_blank(),
                          plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
                          legend.title = element_text(colour = "black", face = "bold", size = 15),
                          legend.text = element_text(colour = "black", face = "bold", size = 10), 
                          legend.key.height = unit(1, "cm"),
                          legend.key.width = unit(0.8, "cm")))

geo.prj <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" 
world_sf <- ne_countries(scale = "medium", returnclass = "sf") %>%
  st_transform(crs = CRS(geo.prj))

world_indian <- ne_countries(scale = "medium", returnclass = "sf") %>%
  st_transform(crs = CRS(proj_indian))

ggplot() +
  geom_sf(data = pus_indian2, size = 0.05) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  ggtitle("Planning unit region Indian Ocean ABNJ") +
  theme_opts3 +
  ggsave("pdfs/abnj_indian_latlon_02.pdf", width = 20, height = 15, dpi = 300)

ggplot() +
  geom_sf(data = pus_indian, size = 0.05) +
  geom_sf(data = world_indian, size = 0.05, fill = "grey20") +
  ggtitle("Planning unit region Indian Ocean ABNJ") +
  theme_opts3 +
  ggsave("pdfs/abnj_indian_laea_02.pdf", width = 20, height = 15, dpi = 300)

#######

moll_global <- "+proj=moll +lon_0=0 +datum=WGS84 +units=m +no_defs"
global <- st_read("shapefiles_rasters/global-poly_abnj_01/global-poly_abnj_01.shp") %>% 
  st_transform(crs = CRS(moll_global))
# plot(st_geometry(global))

grid_spacing <- 55000  # size of squares, in units of the CRS (115473.4 for 1deg) (50000 for 0.5deg)
pus_global <- st_make_grid(global, square = F, cellsize = c(grid_spacing, grid_spacing)) %>% # the grid, covering bounding box
  st_sf() # not really required, but makes the grid nicer to work with later
  # plot(st_geometry(pus_global))
# nrow(pus_global)

# Epipelagic PUs
  pus_global_epi <- pus_global %>%
    mutate(layer = as.numeric(seq(1,nrow(pus_global)))) %>% 
    arrange(layer)
    st_write(sf_sf, dsn = "shapefiles_rasters/", layer = "abnj_02-epipelagic_global_moll_05deg", driver = "ESRI Shapefile")
# Mesopelagic PUs
  pus_global_meso <- pus_global_epi %>%
    mutate(layer = as.numeric(seq(90065, length.out = nrow(sf_sf)))) %>% 
    arrange(layer)
  st_write(pus_global_meso, dsn = "shapefiles_rasters/", layer = "abnj_03-mesopelagic_global_moll_05deg", driver = "ESRI Shapefile")
# Bathyabyssopelagic PUs
  pus_global_bathy <- pus_global_meso %>%
    mutate(layer = as.numeric(seq(180131,length.out = nrow(pus_global_meso)))) %>% 
    arrange(layer)
  st_write(pus_global_bathy, dsn = "shapefiles_rasters/", layer = "abnj_04-bathyabysso_global_moll_05deg", driver = "ESRI Shapefile")

pus_area <- round(st_area(pus_global)/1e+06)
range(pus_area)
hist(pus_area)


world_moll <- ne_countries(scale = "medium", returnclass = "sf") %>%
  st_transform(crs = CRS(moll_global))

ggplot() +
  geom_sf(data = pus_global, size = 0.05) +
  geom_sf(data = world_moll, size = 0.05, fill = "grey20") +
  ggtitle("Planning unit region 1° ABNJ") +
  theme_opts3 +
  ggsave("pdfs/abnj_global_moll_05deg.pdf", width = 20, height = 15, dpi = 300)

# seq(90065,length.out = nrow(pus_global))