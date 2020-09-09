# This code was written by Isaac Brito-Morales (i.britomorales@uq.edu.au)
# Please do not distribute this code without permission.
# NO GUARANTEES THAT CODE IS CORRECT
# Caveat Emptor!

# moll_global <- "+proj=moll +lon_0=0 +datum=WGS84 +units=m +no_defs"
# robin_global <- "+proj=robin +lon_0=0 +datum=WGS84 +units=m +no_defs"
# size of squares, in units of the CRS (115473.4 for 1deg) (55000 for ~0.5deg)

pu_grid <- function(path, outdir, proj_type, pu_shape, resolution, name) {
  
  library(dplyr)
  library(raster)
  library(sf)
  # Files location
    dir.shp <- list.files(path = path, pattern = "*.shp$", full.names = TRUE)
  # Reading the file
    shp <- st_read(dir.shp) %>% 
      st_transform(crs = CRS(proj_type))
  
  # Creating the grid 
    # Size of the grid
      grid_spacing <- resolution  
    # the grid, covering bounding box
      pus_shp <- st_make_grid(shp, square = ifelse(pu_shape == "square", TRUE, ifelse(pu_shape == "hexagon", FALSE)), cellsize = c(grid_spacing, grid_spacing)) %>% # the grid, covering bounding box
        st_sf() # not really required, but makes the grid nicer to work with later
      pus_final <- pus_shp %>%
        dplyr::mutate(layer = as.numeric(seq(1,nrow(pus_shp)))) %>% 
        dplyr::arrange(layer)
  # Writing the final grid[PU] object
    st_write(pus_final, dsn = outdir, layer = name, driver = "ESRI Shapefile")
  
}

# system.time(pu_grid(path = "features_shapefiles/SAEEZ_shapefile", 
#                     outdir = "features_shapefiles/SAEEZ_shapefile/", 
#                     proj_type = "+proj=robin +lon_0=0 +datum=WGS84 +units=m +no_defs", 
#                     pu_shape = "hexagon", 
#                     resolution = 55000, 
#                     name = "PUs_SA_moll_05deg"))

system.time(pu_grid(path = "/QRISdata/Q1216/BritoMorales/Project05a_Rafaela/features_shapefiles/SAEEZ_shapefile", 
                    outdir = "/QRISdata/Q1216/BritoMorales/Project05a_Rafaela/features_shapefiles/SAEEZ_pus/", 
                    proj_type = "+proj=robin +lon_0=0 +datum=WGS84 +units=m +no_defs", 
                    pu_shape = "hexagon", 
                    resolution = 55000, 
                    name ="PUs_SA_moll_05deg"))

