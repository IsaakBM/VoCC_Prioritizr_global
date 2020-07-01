# This code was written by Isaac Brito-Morales (i.britomorales@uq.edu.au)
# Please do not distribute this code without permission.
# NO GUARANTEES THAT CODE IS CORRECT
# Caveat Emptor!


pu_by_provinces <- function(pu_file, province_file, prov_name, olayer, proj.geo, outdir) {
  
  library(raster)
  library(dplyr)
  library(sf)
  library(data.table)
  library(foreach)
  library(doParallel)

  
  # Reading Planning Unit Region Shapefile
    pu_file <- pu_file
      pu_region <- st_read(pu_file) %>% st_transform(crs = CRS(proj.geo))
  # Reading Marine Province Shapefile
    province <- province_file
      bioprovince <- st_read(province) %>% st_transform(crs = CRS(proj.geo)) 
  # Match 
  if(prov_name == "Longhurst") {
    # Get the indicator for the provinces
      prov_code <- as.character(bioprovince$ProvCode)
      prov_list <- list() # to allocate results
      for(i in 1:length(prov_code)) { # this could be set on parallel
        single <- bioprovince %>% filter(ProvCode == prov_code[i])
        dt1 <- st_crop(pu_region, single) # crop each polygon with the bioprovince
        prov_list[[i]] <- dt1 %>% mutate(province = prov_code[i]) # save the output
      }
    # Merge all the output
      pus_prov <- do.call(rbind, prov_list) %>% arrange(layer)
    # Match and establish categories
      pu_region$province <- pus_prov$province[match(pu_region$layer, pus_prov$layer)]
      pu_region$province <- ifelse(is.na(pu_region$province), 
                                   paste("non-categ", prov_name, sep = "_"), 
                                   paste(pu_region$province, prov_name, sep = "_"))
    
  } else if (prov_name == "Glasgow") {
    # Get the indicator for the provinces
      prov_code <- as.character(bioprovince$ProvId)
      prov_list <- list() # to allocate results
      for(i in 1:length(prov_code)) { # this could be set on parallel
        single <- bioprovince %>% filter(ProvId == prov_code[i])
        dt1 <- st_crop(pu_region, single) # crop each polygon with the bioprovince
        prov_list[[i]] <- dt1 %>% mutate(province = prov_code[i]) # save the output
      }
    # Merge all the output
      pus_prov <- do.call(rbind, prov_list) %>% arrange(layer)
    # Match and establish categories
      pu_region$province <- pus_prov$province[match(pu_region$layer, pus_prov$layer)]
      pu_region$province <- ifelse(is.na(pu_region$province), 
                                   paste("non-categ", prov_name, sep = "_"), 
                                   paste(pu_region$province, prov_name, sep = "_"))
      
  } else if (prov_name == "GOODS") { # this is for the seafloor
    # Set up parallel structure
      ncores <- 3 
      cl <- makeCluster(ncores)
      registerDoParallel(cl)
    # Get the indicator for the provinces
      prov_code <- as.character(bioprovince$Province)
      prov_list <- list() # to allocate results
      prov_par <- foreach(i = 1:length(prov_code), .packages = c("raster", "sf", "data.table", "dplyr")) %dopar% {
        single <- bioprovince %>% filter(Province == prov_code[i])
        dt1 <- st_crop(pu_region, single) # crop each polygon with the bioprovince
        prov_list[[i]] <- dt1 %>% mutate(province = prov_code[i]) # save the output
      }
      stopCluster(cl)
    # Merge all the output
      pus_prov <- do.call(rbind, prov_par) %>% arrange(layer)
    # Match and establish categories
      pu_region$province <- pus_prov$province[match(pu_region$layer, pus_prov$layer)]
      pu_region$province <- ifelse(is.na(pu_region$province), 
                                   paste("non-categ", prov_name, sep = "_"), 
                                   paste(pu_region$province, prov_name, sep = "_"))
    
  } else if (prov_name == "mpas") {
    # Set up parallel structure
      ncores <- 21 
      cl <- makeCluster(ncores)
      registerDoParallel(cl)
    # Get the indicator for the provinces
      prov_code <- as.character(bioprovince$wdpaid)
      prov_list <- list() # to allocate results
      prov_par <- foreach(i = 1:length(prov_code), .packages = c("raster", "sf", "data.table", "dplyr")) %dopar% {
        single <- bioprovince %>% filter(wdpaid == prov_code[i])
        dt1 <- st_crop(pu_region, single) # crop each polygon with the bioprovince
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
  pu_region <- as.data.frame(pu_region)
  pu_csv <- paste(paste("pus", olayer, sep = "-"), prov_name, ".csv", sep = "_")
  fwrite(dplyr::select(pu_region, -geometry), paste(outdir, pu_csv, sep = ""))
}



# system.time(pu_by_provinces(pu_file = "/QRISdata/Q1216/BritoMorales/Project04b/shapefiles_rasters/XXXXX",
#                             province_file = "/data/Q1216/BritoMorales/Project04b/shapefiles_rasters/GOODSprovinces/GOODSprovinces_bathyal.shp",
#                             prov_name = "GOODS",
#                             olayer = "seafloor",
#                             proj.geo = "+proj=moll +lon_0=0 +datum=WGS84 +units=m +no_defs",
#                             outdir = "/QRISdata/Q1216/BritoMorales/Project04b/shapefiles_rasters/"))

system.time(pu_by_provinces(pu_file = "/QRISdata/Q1216/BritoMorales/Project04b/shapefiles_rasters/abnj_02-epipelagic_global_moll_05deg/abnj_02-epipelagic_global_moll_05deg.shp",
                            province_file = "/QRISdata/Q1216/BritoMorales/Project04b/shapefiles_rasters/mpas_v2018/mpas_v2018.shp", 
                            prov_name = "mpas",
                            olayer = "epipelagic",
                            proj.geo = "+proj=moll +lon_0=0 +datum=WGS84 +units=m +no_defs", 
                            outdir = "/QRISdata/Q1216/BritoMorales/Project04b/shapefiles_rasters/"))



