# This code was written by Isaac Brito-Morales (i.britomorales@uq.edu.au)
# Please do not distribute this code without permission.
# NO GUARANTEES THAT CODE IS CORRECT
# Caveat Emptor!

# AIM: Create a general dataframe that would be the core for generate input file for prioritizr analyses (conventional MARXAN)
# path: folder's name where species conservation feature files are located
# outdir: where to put the final sf-.rds object
# pu_shp: 

features_pus <- function(path, outdir, pu_shp, olayer) { 

####################################################################################
####### Defining the main packages (tryining to auto this)
####################################################################################
  library(raster)
  library(rgdal)
  library(rgeos)
  library(sf)
  library(dplyr)
  library(doParallel)
  library(stringr)
  library(sf)
  library(lwgeom)
  library(data.table)
  
####################################################################################
####### 
####################################################################################
  #
    if(stringr::str_detect(string = pu_shp, pattern = ".rds") == TRUE) {
      shp_PU_sf <- readRDS(pu_shp)
    } else if (stringr::str_detect(string = pu_shp, pattern = ".shp") == TRUE) {
      shp_PU_sf <- st_read(pu_shp)
    }
    # If no cellsID values were assinged to the original
      shp_PU_sf <- shp_PU_sf %>%
        dplyr::rename(cellsID = layer) %>% 
        dplyr::mutate(area_km2 = as.numeric(st_area(shp_PU_sf)/1e+06))
      pu_min_area <- min(shp_PU_sf$area_km2)
  # Reading conservation features .rds files (AquaMaps)
    dir <- path
      pattern1 <-  c(paste0("*", ".*.rds$"), paste0("*", ".*shp$"))
      files <- list.files(path = dir, pattern = paste0(pattern1, collapse = "|"), full.names = TRUE)

####################################################################################
####### 
####################################################################################
  # Loop through each file
    files_list <- vector("list", length = length(files)) # to allocate results
  # Begin the parallel structure
    ncores <- 3
    cl <- makeCluster(ncores)
    registerDoParallel(cl)
    # A parallel Loop
      PU_list <- foreach(i = 1:length(files), .packages = c("raster", "sf", "dplyr", "stringr", "lwgeom", "data.table")) %dopar% {
        # Reading conservation features
        if(stringr::str_detect(string = files[i], pattern = ".rds") == TRUE) {
          single <- readRDS(files[i])
        } else if (stringr::str_detect(string = files[i], pattern = ".shp") == TRUE) {
          single <- st_read(files[i])}
        # Intersects every conservation feature with planning unit region
          pu_int <- st_intersection(shp_PU_sf, single) %>% 
            filter(st_geometry_type(.) %in% c("POLYGON", "MULTIPOLYGON")) # we want just the polygons/multi not extra geometries
        # Filter the intersection with the planning unit sf object to get the exact distristribution per planning units
          if(nrow(pu_int) > 0) { # to avoid empty sf objects 
            files_list[[i]] <- st_join(x = shp_PU_sf, y = pu_int,  by = "cellsID") %>% 
              na.omit() %>% 
              dplyr::group_by(cellsID.x) %>% 
              dplyr::summarise(cellsID = unique(cellsID.x)) %>% 
              dplyr::select(cellsID, geometry) %>% 
              dplyr::mutate(area_km2 = as.numeric(st_area(geometry)/1e+06),
                            feature_names = paste(unlist(strsplit(basename(files[i]), "_"))[1], olayer, sep = "_")) %>% 
              ungroup()
                # dplyr::filter(area_km2 >= pu_min_area) %>% 
              }
          }
      stopCluster(cl)

####################################################################################
####### 
####################################################################################
  # Final sf with all species information and write that object (main object to develop marxan input files)
    PU_list_b <- do.call(rbind, PU_list)
    # Write the object
      pu_rds <- paste(olayer, ".rds", sep = "")
      saveRDS(PU_list_b, paste(outdir, pu_rds, sep = ""))
  return(PU_list_b)
}

system.time(features_pus(path = "Inputs/FeaturesSeafloor2",
                         outdir = "Output/",
                         pu_shp = "Output/02_abnjs_filterdepth/abnj_02-epipelagic_global_moll_05deg_depth/abnj_02-epipelagic_global_moll_05deg_depth.shp",
                         olayer = "epipelagic"))
