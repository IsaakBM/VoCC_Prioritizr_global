# This code was written by Isaac Brito-Morales (i.britomorales@uq.edu.au)
# Please do not distribute this code without permission.
# NO GUARANTEES THAT CODE IS CORRECT
# Caveat Emptor!

# AIM: Create a general dataframe that would be the core for generate input file for MARXAN analyses (conventional MARXAN)
# path: folder's name where .grd species rasters files are located
# outdir: where to put the .csv species files
# region: a raster (or shapefile) of the region of interest. TRUE or FALSE. if it is FALSE the function will created a new one.
# shapefile: a "generic" file that will be use to create planning units (REGION = FALSE) / or a specific shapefile with the planning units  (REGION = TRUE)
# ncol_pu, nrow_pu: lon and lat resolution of the planning units
  # raster(ncol = 720, nrow = 360) for 0.5° spatial resolution
  # raster(ncol = 1440, nrow = 720) for 0.25° spatial resolution
# raster(ncol = 3600, nrow = 1800) for 0.10° spatial resolution

# raster_region = 

  marxan_inputs <- function(path, outdir, region, shapefile, proj.geo) { 
    
   # # List of pacakges that we will use
   #   list.of.packages <- c("raster", "fasterize", "rgdal", "rgeos", "sf", "dplyr", "doParallel", "foreach", "stringr", "lwgeom")
   #  # If is not installed, install the pacakge
   #    new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])] # is the package in MY list of packages
   #    if(length(new.packages)) install.packages(new.packages) # if not, installed
   #    # Load packages
   #      lapply(list.of.packages, library, character.only = TRUE)
        
        library(raster)
        library(fasterize)
        library(rgdal)
        library(rgeos)
        library(sf)
        library(dplyr)
        library(doParallel)
        library(foreach)
        library(stringr)
        library(lwgeom)
    
    # Creating planning unit based on region
    if(region == FALSE) {
      # reading files
      shp <- st_read(shapefile)
      # create an empty raster
      rs <- raster(raster_region) #%>% aggregate(2)
        rs <- crop(rs, shp) # in case you have a smaller study region
      shp_PU_sp <- as(rs,  "SpatialPolygonsDataFrame") # creating a polygon dataframe from raster
      shp_PU_sp$layer <- seq(1, length(shp_PU_sp)) # all polygons have the same name. Change it for a seq names
      geo.prj <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" 
      shp_PU_sp <- spTransform(shp_PU_sp, CRS(geo.prj)) 
      shp_PU_sf <- st_as_sf(shp_PU_sp) %>% select(layer)
      # Write the planning unit element created
        pu_name <- paste("pu_shapefile", sep = "_")
        st_write(shp_PU_sf, dsn = paste(outdir, pu_name, sep = ""), driver = "ESRI Shapefile")
      
    } else {
      geo.prj <- proj.geo
      shp_PU_sf <- st_read(shapefile)
        # try this with dplyr?
          col_ns <- colnames(shp_PU_sf)
          col_ns[1] <- ifelse(col_ns[1] != "layer", "layer", col_ns[1])
          colnames(shp_PU_sf) <- col_ns
          shp_PU_sf <- shp_PU_sf %>%
            dplyr::mutate (area_km2 = as.numeric(st_area(shp_PU_sf)/1e+06))
          pu_min_area <- min(shp_PU_sf$area_km2)
    }

    # Reading features raster files (AquaMaps | Trajectory classes)
      dir <- path
      pattern1 <-  c(paste0("*", ".*.grd$"), paste0("*", ".*.tif$")) # include tif rasters and stack rasters
      files <- list.files(path = dir, pattern = paste0(pattern1, collapse = "|"), full.names = TRUE)

    # Loop through each file
      files_list <- list() # to allocate results
    # Begin the parallel structure      
      ncores <- 24
      cl <- makeCluster(ncores)
      registerDoParallel(cl)
      # Loop
        PU_list <- foreach(i = 1:length(files), .packages = c("raster", "sf", "dplyr", "stringr", "lwgeom")) %dopar% {
        # Reading features. If the raster is not projected, do it 
          single <- stack(files[i])
            single <- readAll(subset(single, 1))
            if(is.na(single@crs) == TRUE) {crs(single) <- CRS(geo.prj)} else {single <- single}
          # Matching resolution of AquaMaps with the desirable cost layer (and also planning units)
            rs1 <- single
          # Transform AquaMaps species raster into a sf spatial polygon dataframe
            sd_rs1 <- as(rs1, "SpatialPolygonsDataFrame")
              sd_rs1 <- spTransform(sd_rs1, CRS(geo.prj))
              sd_rs1 <- st_as_sf(sd_rs1)
          # get the polygons from the world raster data
            pu_int <- st_intersection(shp_PU_sf, sd_rs1) %>% 
              filter(st_geometry_type(.) %in% c("POLYGON", "MULTIPOLYGON")) # we want just the polygons/multi not extra geometries
          # filter the intersection with the world waster data to get the exact layer names
            pu_int_b <- pu_int[pu_int$layer %in% shp_PU_sf$layer, ] %>% 
              group_by(layer) %>% 
              summarise(layer2 = unique(layer))
              # Calculating area info + type of feature
                pu_int_b <- pu_int_b %>%
                  dplyr::mutate (area_km2 = as.numeric(st_area(pu_int_b)/1e+06),
                                 feature_names = unlist(lapply(basename(files[i]), FUN = function(x) strsplit(x, "_")))[1]) %>%
                  dplyr::rename(pu = layer) %>% 
                  dplyr::select(-layer2) %>% 
                  dplyr::filter(area_km2 >= pu_min_area) 
                
            files_list[[i]] <- pu_int_b 
          }
      stopCluster(cl)
      
      # Final sf dataframe with all species information and write that object (main object to develop marxan input files)
        PU_list_b <- do.call(rbind, PU_list)
        # Write the object
          si_pu <- paste("sps_by_pu", sep = "_")
          st_write(PU_list_b, dsn = paste(outdir, si_pu, sep = ""), driver = "ESRI Shapefile")
  
  return(PU_list_b)
  }
  
  system.time(marxan_inputs(path = "/QRISdata/Q1216/BritoMorales/Project04b/aquamaps_outputs/02_EpipelagicLayer_rs",
                            outdir = "/QRISdata/Q1216/BritoMorales/Project04b/shapefiles_rasters/",
                            region = TRUE,
                            shapefile = "/QRISdata/Q1216/BritoMorales/Project04b/shapefiles_rasters/abnj_01-epipelagic_global_moll_05deg/abnj_01-epipelagic_global_moll_05deg.shp",
                            proj.geo = "+proj=moll +lon_0=0 +datum=WGS84 +units=m +no_defs"))
  