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
  # Begin the parallel structure
    ncores <- 3
    cl <- makeCluster(ncores)
    registerDoParallel(cl)
    # A parallel Loop
      PU_list <- foreach(i = 1:length(files), .packages = c("raster", "sf", "dplyr", "stringr", "lwgeom", "data.table")) %dopar% {
        # Reading conservation features
        if(stringr::str_detect(string = files[i], pattern = ".rds") == TRUE) {
          single <- readRDS(files[i]) %>% 
            st_make_valid()
        } else if (stringr::str_detect(string = files[i], pattern = ".shp") == TRUE) {
          single <- st_read(files[i])}
        # Intersects every conservation feature with planning unit region
          pu_int <- st_intersection(shp_PU_sf, single) %>% 
            filter(st_geometry_type(.) %in% c("POLYGON", "MULTIPOLYGON")) # we want just the polygons/multi not extra geometries
        # Filter the intersection with the planning unit sf object to get the exact distristribution per planning units
          if(nrow(pu_int) > 0) { # to avoid empty sf objects 
            out <- st_join(x = shp_PU_sf, y = pu_int,  by = "cellsID") %>% 
              na.omit() %>% 
              dplyr::group_by(cellsID.x) %>% 
              dplyr::summarise(cellsID = unique(cellsID.x)) %>% 
              dplyr::select(cellsID, geometry) %>% 
              dplyr::mutate(area_km2 = as.numeric(st_area(geometry)/1e+06),
                            feature_names = ifelse(str_detect(basename(files[i]), pattern = ".rds"), 
                                                   paste(unlist(strsplit(basename(files[i]), "[.]"))[1], olayer, sep = "_"),
                                                   paste(unlist(strsplit(basename(files[i]), "_"))[1], olayer, sep = "_"))) %>% 
              ungroup()
              # Write the .rds object
                pu_rds <- paste(unique(out$feature_names), ".rds", sep = "")
                saveRDS(out, paste(outdir, pu_rds, sep = ""))
              # Write .csv object
                outCSV <- out %>% 
                  as_tibble() %>% 
                  dplyr::select(-geometry)
                pu_csv <- paste(unique(outCSV$feature_names), ".csv", sep = "")
                write.csv(outCSV, paste(outdir, pu_csv, sep = ""), row.names = FALSE)
              }
          }
      stopCluster(cl)
}

system.time(features_pus(path = "Inputs/FeaturesSeafloor3",
                         outdir = "Output/",
                         pu_shp = "Output/02_abnjs_filterdepth/abnj_05-seafloor_global_moll_05deg_depth/abnj_05-seafloor_global_moll_05deg_depth.shp",
                         olayer = "seafloor"))
