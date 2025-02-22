# This code was written by Isaac Brito-Morales (i.britomorales@uq.edu.au)
# Please do not distribute this code without permission.
# NO GUARANTEES THAT CODE IS CORRECT
# Caveat Emptor!

climatevar_feature <- function(rs_path, shp_path, outdir, var_name, proj.geo, ...) {

  library(raster)
  library(sf)
  library(exactextractr)
  library(dplyr)
  library(foreach)
  library(doParallel) 
  library(nngeo)
  
  # Function to replace NAs with nearest neighbor. Function wrtitten by Jason Everett
    fCheckNAs <- function(df, vari) {
      if (sum(is.na(pull(df, !!sym(vari))))>0){ # Check if there are NAs
        
        gp <- df %>%
          mutate(isna = is.finite(!!sym(vari))) %>%
          group_by(isna) %>%
          group_split()
        
        out_na <- gp[[1]] # DF with NAs
        out_finite <- gp[[2]] # DF without NAs
        
        d <- st_nn(out_na, out_finite) %>% # Get nearest neighbour
          unlist()
        
        out_na <- out_na %>%
          mutate(!!sym(vari) := pull(out_finite, !!sym(vari))[d])
        
        df <- rbind(out_finite, out_na)
        
      }
      return(df)
    }
  
  # Folder's structure
    dir.scenarios <- paste(list.dirs(path = rs_path, full.names = TRUE, recursive = FALSE), sep = "/") # Climate Models Directory
    dir.olayers <- paste(list.dirs(path = dir.scenarios, full.names = TRUE, recursive = FALSE), sep = "/") # Climate Models Directory per ocean layer
    dir.shpfile <- paste(list.dirs(path = shp_path, full.names = TRUE, recursive = FALSE), sep = "/") # Planning unit realm per depth
      dir.shpfile <- c(dir.shpfile, dir.shpfile, dir.shpfile)
  # Begin the parallel structure      
    cores  <-  3
    cl <- makeCluster(cores)
    registerDoParallel(cl)    
    foreach(i = 1:length(dir.olayers), .packages = c("raster", "dplyr", "sf", "exactextractr", "nngeo")) %dopar% {
      # Reading marxan_input file
        shp_file <- st_read(list.files(path = dir.shpfile[i] , pattern = ".shp", full.names = TRUE)) %>% 
          st_transform(crs = CRS(proj.geo))
        # playing with names
          col_shp <- colnames(shp_file)
          col_shp[1] <- ifelse(col_shp[1] == "layer", "id", col_shp[1])
          colnames(shp_file) <- col_shp
      # Read raster object
        rs_file <- readAll(raster(list.files(path = dir.olayers[i] , pattern = ".tif", full.names = TRUE)))
        crs(rs_file) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
        weight_rs <- raster::area(rs_file)
        rs_file <- projectRaster(rs_file, crs = CRS(proj.geo), method = "ngb", over = FALSE)
        weight_rs <- projectRaster(weight_rs, crs = CRS(proj.geo), method = "ngb", over = FALSE)
        names(rs_file) <- "layer"
      # Getting cost value by planning unit
        rs_bypu <- exact_extract(rs_file, shp_file, "weighted_mean", weights = weight_rs)
        rs_shp <- shp_file %>%
          dplyr::mutate(climate_feature = rs_bypu) %>%
          dplyr::relocate(id, climate_feature)
        # Replace NAs with nearest neighbor
          rs_sfInt <- fCheckNAs(df = rs_shp, vari = names(rs_shp)[2]) %>%
            dplyr::select(-isna) %>% 
            dplyr::mutate(area_km2 = as.numeric(st_area(rs_shp)/1e+06)) %>%
            dplyr::mutate(feature_names = var_name) %>% 
            dplyr::rename(pu = id) %>% 
            dplyr::select(pu, area_km2, feature_names, climate_feature)
          rs_dfInt <- rs_sfInt %>% 
            as.data.frame() %>% 
            dplyr::select(pu, area_km2, feature_names, climate_feature)
      # Files' name
        ns <- basename(list.files(path = dir.olayers[i] , pattern = ".tif", full.names = TRUE))
          var <- unlist(strsplit(x = ns, split = "_"))[2]
          olayer <- unlist(strsplit(x = ns, split = "_"))[1]
          model <- unlist(strsplit(x = ns, split = "_"))[3]
          ssp <- unlist(strsplit(x = ns, split = "_"))[4]
          period <- unlist(strsplit(unlist(strsplit(x = ns, split = "[.]"))[1], split = "_"))[5]
        name.csv <- paste(olayer, var, model, ssp, period, sep = "_")
        # Write the outputs
          saveRDS(rs_sfInt, paste(outdir, name.csv, ".rds", sep = ""))
          write.csv(rs_dfInt, paste(outdir, name.csv, ".csv", sep = ""))
    }  
    stopCluster(cl)  
}


# climatevar_feature(rs_path = "/QRISdata/Q1216/BritoMorales/Project04b/climate-change_inputs/vocc_mag_nobottom",
#                    shp_path = "/QRISdata/Q1216/BritoMorales/Project04b/shapefiles_rasters/02_abnjs_filterdepth",
#                    outdir = "/QRISdata/Q1216/BritoMorales/Project04b/features_CSVs/",
#                    var_name = "VoCC",
#                    proj.geo = "+proj=moll +lon_0=0 +datum=WGS84 +units=m +no_defs")

climatevar_feature(rs_path = "Inputs/ClimateChange/RCE",
                   shp_path = "Output/02_abnjs_filterdepth",
                   outdir = "Inputs/General/",
                   var_name = "RCE",
                   proj.geo = "+proj=moll +lon_0=0 +datum=WGS84 +units=m +no_defs")

