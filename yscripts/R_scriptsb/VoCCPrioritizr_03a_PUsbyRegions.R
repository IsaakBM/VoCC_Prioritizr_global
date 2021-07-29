# This code was written by Isaac Brito-Morales (i.britomorales@uq.edu.au)
# Please do not distribute this code without permission.
# NO GUARANTEES THAT CODE IS CORRECT
# Caveat Emptor!


pu_by_provinces <- function(pu_file, province_file, prov_name, olayer, proj.geo, outdir) {
  
  library(raster)
  library(dplyr)
  library(sf)
  library(rgeos)
  library(rgdal)
  library(data.table)
  library(foreach)
  library(doParallel)
  library(stringr)
  
  # Reading Planning Unit Region Shapefile
    pu_file <- pu_file
      pu_region <- st_read(pu_file) %>% 
        st_transform(crs = CRS(proj.geo))
  # Reading Marine Province Shapefile
    province <- province_file
    if(stringr::str_detect(string = province, pattern = ".rds") == TRUE) {
      bioprovince <- readRDS(province) %>% 
        st_transform(crs = CRS(proj.geo))
    } else if (stringr::str_detect(string = province, pattern = ".shp") == TRUE) {
      bioprovince <- st_read(province) %>% 
        st_transform(crs = CRS(proj.geo)) %>% 
        st_make_valid()
    }
  # Match 
    if(prov_name == "Longhurst") {
      nr <- st_nearest_feature(pu_region, bioprovince)
      pu_region <- pu_region %>% 
        dplyr::mutate(province = paste(as.vector(bioprovince$ProvCode[nr]), prov_name, sep = "_")) %>% 
        dplyr::arrange(layer)
        
        } else if (prov_name == "Glasgow") {
          nr <- st_nearest_feature(pu_region, bioprovince)
          pu_region <- pu_region %>% 
            dplyr::mutate(province = paste(as.vector(bioprovince$ProvId[nr]), prov_name, sep = "_")) %>% 
            dplyr::arrange(layer)
            
        } else if (prov_name == "GOODS") { # this is for the seafloor
          nr <- st_nearest_feature(pu_region, bioprovince)
          pu_region <- pu_region %>% 
            dplyr::mutate(province = paste(as.vector(bioprovince$Province[nr]), prov_name, sep = "_")) %>% 
            dplyr::arrange(layer)
          
        } else if (prov_name == "mpas") {
        # Set up parallel structure
          ncores <- 3
          cl <- makeCluster(ncores)
          registerDoParallel(cl)
        # Get the indicator for the provinces
          prov_code <- as.character(bioprovince$wdpaid)
          prov_list <- list() # to allocate results
          prov_par <- foreach(i = 1:length(prov_code), .packages = c("raster", "sf", "data.table", "dplyr", "rgeos", "rgdal")) %dopar% {
            single <- bioprovince %>% 
              filter(wdpaid == prov_code[i])
            single_sfc  <-  st_geometry(single)
            single_centroid_sfc  <-  st_centroid(single_sfc)
          # each object is firstly shifted in a way that its center has coordinates of 0, 0 (single_sfc - single_centroid_sfc)
          # the sizes of the geometries are reduced by half (* 0.5)
          # each object’s centroid is moved back to the input data coordinates (+ single_centroid_sfc).
          single_scale  <- (single_sfc - single_centroid_sfc) * 0.5 + single_centroid_sfc
          st_crs(single_scale) <-  proj.geo
          dt1 <- st_intersection(pu_region, single_scale) %>% 
            filter(st_geometry_type(.) %in% c("POLYGON", "MULTIPOLYGON"))
          if(nrow(dt1) > 0) { 
            prov_list[[i]] <- dt1 %>% 
              mutate(province = prov_code[i]) # save the output            
          }
        }
          stopCluster(cl)
        # Merge all the output
          pus_prov <- do.call(rbind, prov_par) %>% 
            arrange(layer)
        # Match and establish categories
          pu_region$province <- pus_prov$province[match(pu_region$layer, pus_prov$layer)]
          pu_region$province <- ifelse(is.na(pu_region$province), 
                                       paste("non-categ", prov_name, sep = "_"), 
                                       paste(pu_region$province, prov_name, sep = "_"))
          
        } else if (prov_name == "VMEs") {
          # Set up parallel structure
            ncores <- 3
            cl <- makeCluster(ncores)
            registerDoParallel(cl)
          # Get the indicator for the provinces
            prov_code <- as.character(bioprovince$VME_ID)
            prov_list <- list() # to allocate results
            prov_par <- foreach(i = 1:length(prov_code), .packages = c("raster", "sf", "data.table", "dplyr", "lwgeom")) %dopar% {
              single <- bioprovince %>% filter(VME_ID == prov_code[i])
              dt1 <- st_intersection(pu_region, single) %>% 
                filter(st_geometry_type(.) %in% c("POLYGON", "MULTIPOLYGON"))
              if(nrow(dt1) > 0) { 
                prov_list[[i]] <- dt1 %>% mutate(province = prov_code[i]) # save the output    
              }
            }
            stopCluster(cl)
          # Merge all the output
            pus_prov <- do.call(rbind, prov_par) %>% arrange(layer)
          # Match and establish categories
            pu_region$province <- pus_prov$province[match(pu_region$layer, pus_prov$layer)]
            pu_region$province <- ifelse(is.na(pu_region$province), 
                                         paste("non-categ", prov_name, sep = "_"), 
                                         paste(pu_region$province, prov_name, sep = "_"))
            
        } else if (prov_name == "ecoregions") {
          # Set up parallel structure
            bioprovince <- st_read(province) %>% 
              st_crop(mbp, xmin = -180, ymin = -90, xmax = 180, ymax = 90) %>% 
              st_transform(crs = CRS(proj.geo)) %>% 
              group_by(PROV_CODE) %>% 
              summarise(ecoregion = sum(PROV_CODE, do_union = TRUE)) %>% 
              select(PROV_CODE)
            ncores <- 3
            cl <- makeCluster(ncores)
            registerDoParallel(cl)
          # Get the indicator for the provinces
            prov_code <- as.character(bioprovince$PROV_CODE)
            prov_list <- list() # to allocate results
            prov_par <- foreach(i = 1:length(prov_code), .packages = c("raster", "sf", "data.table", "dplyr", "lwgeom")) %dopar% { 
              single <- bioprovince %>% filter(PROV_CODE == prov_code[i])
              dt1 <- st_intersection(pu_region, single) %>% 
                filter(st_geometry_type(.) %in% c("POLYGON", "MULTIPOLYGON"))
              if(nrow(dt1) > 0) { 
                prov_list[[i]] <- dt1 %>% mutate(province = prov_code[i]) # save the output    
              }
            }
            stopCluster(cl)
          # Merge all the output
            pus_prov <- do.call(rbind, prov_par) %>% arrange(layer)
          # Match and establish categories
            pu_region$province <- pus_prov$province[match(pu_region$layer, pus_prov$layer)]
            pu_region$province <- ifelse(is.na(pu_region$province), 
                                         paste("non-categ", prov_name, sep = "_"), 
                                         paste(pu_region$province, prov_name, sep = "_"))
        }
  # 
    pu_regionCSV <- as.data.frame(pu_region)
    pu_csv <- paste(paste("pus", olayer, sep = "-"), prov_name, ".csv", sep = "_")
    fwrite(dplyr::select(pu_regionCSV, -geometry), paste(outdir, pu_csv, sep = "")) # add , -depth
  # 
    pu_rds <- paste(paste("pus", olayer, sep = "-"), prov_name, ".rds", sep = "_")
    saveRDS(pu_region, paste(outdir, pu_rds, sep = ""))
  
}

# system.time(pu_by_provinces(pu_file = "/scratch/user/uqibrito/Project04c/Output/02_abnjs_filterdepth/abnj_05-seafloor_global_moll_05deg_depth/abnj_05-seafloor_global_moll_05deg_depth.shp",
#                             province_file = "/QRISdata/Q1216/BritoMorales/Project04b/shapefiles_rasters/mpas_v2018/mpas_v2018.shp",
#                             prov_name = "mpas",
#                             olayer = "seafloor",
#                             proj.geo = "+proj=moll +lon_0=0 +datum=WGS84 +units=m +no_defs",
#                             outdir = "/scratch/user/uqibrito/Project04c/Output/PlanningUnitsLocks/"))

system.time(pu_by_provinces(pu_file = "Output/02_abnjs_filterdepth/abnj_05-seafloor_global_moll_05deg_depth/abnj_05-seafloor_global_moll_05deg_depth.shp",
                            province_file = "Inputs/Boundaries/VMEs/VMEs.shp",
                            prov_name = "VMEs",
                            olayer = "seafloor",
                            proj.geo = "+proj=moll +lon_0=0 +datum=WGS84 +units=m +no_defs",
                            outdir = "Output/PlanningUnitsLocks/"))



