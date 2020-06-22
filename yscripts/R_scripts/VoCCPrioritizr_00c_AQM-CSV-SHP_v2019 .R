# This code was written by Isaac Brito-Morales (i.britomorales@uq.edu.au)
# Please do not distribute this code without permission.
# NO GUARANTEES THAT CODE IS CORRECT
# Caveat Emptor!

# AIM: Function that reads a AquaMaps .csv files and returns rasters? by species
# path: directory of AquaMaps species .csv files
# outdir: where to put the .csv species files
# olayer: species from which ocean layer to overlap with bathymetry shapefile
# bathymetry_shp: a bathymetry shapefile to constrain species cells (ETOPO dataset)

# ADD DATA (SPECIES OR RICHNESS) ARGUMENT (THIS WILL NOT WORK FOR RICHNESS BECAUSE YOU WILL NOT HAVE IN .CVS TEM, SALI JUST RICHNESS)

aqua_rs <- function(path, outdir, olayer, geo.prj) { # kill the cells that are not according with the bathymetry... 
  
  library(raster)
  library(sf)
  library(dplyr)
  library(doParallel)
  library(foreach)
  library(stringr)
  library(lwgeom)
  
  # 1) .csv files by species
  files_csv <- list.files(path = path, pattern = ".csv", full.names = TRUE)
  
   # 3) Creating raster distributions maps per species
    rs_final <- vector("list", length(files_csv))
    # Creating a projected 0.5 deg raster
      rs <- raster(ncol = 720, nrow = 360)
      rs[] <- 1:ncell(rs)
    # Set up parallel structure
      cores  <-  detectCores()
      ncores <- 24
      cl <- makeCluster(ncores)
      registerDoParallel(cl)
      # Parallel Loop
        rs_final <- foreach(j  = 1:length(files_csv), .packages = c("raster", "sf", "dplyr", "stringr", "lwgeom")) %dopar% {
          single <- read.csv(files_csv[j])
          ns <- basename(files_csv[j])
            code <- unlist(lapply(ns, function(x) strsplit(x, "_")[[1]][[1]]))
          if(nrow(single) >= 10 & mean(single$CenterLat) != (single$CenterLat[1])) { 
            rs1 <- rasterFromXYZ(as.data.frame(single) 
                                 [, c("CenterLong", "CenterLat", "Probability", "TempPrefMin","TempPrefMax", "SalinityPrefMin","SalinityPrefMax", "OxyPrefMin", "OxyPrefMax")])
            rs_final <- resample(rs1, rs, resample = "ngb") # projecting raster 0.5 deg

            # From Raster to Shapefile
              rs_final <- subset(rs_final, 1)
              if(is.na(rs_final@crs) == TRUE) {crs(rs_final) <- CRS(geo.prj)} else {rs_final <- rs_final}
            # Transform AquaMaps species raster into a sf spatial polygon dataframe
              sd_rs1 <- as(rs_final, "SpatialPolygonsDataFrame")
              sd_rs1 <- spTransform(sd_rs1, CRS(geo.prj))
              sd_rs1 <- st_as_sf(sd_rs1)
      # 
        name.rs <- paste(code, olayer, sep = "_")
        st_write(sd_rs1, dsn = outdir, layer = name.rs, driver = "ESRI Shapefile")
          }
      }
        stopCluster(cl)
}

# system.time(aqua_rs(path = "/QRISdata/Q1216/BritoMorales/Project04b/aquamaps_outputs/02_EpipelagicLayer_csv",
#                     outdir = "/QRISdata/Q1216/BritoMorales/Project04b/aquamaps_outputs/02_EpipelagicLayer_shp/",
#                     olayer = "surface",
#                     geo.prj = "+proj=moll +lon_0=0 +datum=WGS84 +units=m +no_defs"))

system.time(aqua_rs(path = "csvs/01_SurfaceLayer",
                    outdir = "csvs/",
                    olayer = "surface",
                    geo.prj = "+proj=moll +lon_0=0 +datum=WGS84 +units=m +no_defs"))
