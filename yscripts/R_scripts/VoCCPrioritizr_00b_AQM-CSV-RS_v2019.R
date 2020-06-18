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

aqua_rs <- function(path, outdir, olayer) { # kill the cells that are not according with the bathymetry... 
  
  library(dplyr)
  library(raster)
  library(foreach)
  library(doParallel)
  
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
        rs_final <- foreach(j  = 1:length(files_csv), .packages = c("raster", "dplyr")) %dopar% {
          single <- read.csv(files_csv[j])
          ns <- basename(files_csv[j])
            code <- unlist(lapply(ns, function(x) strsplit(x, "_")[[1]][[1]]))
          if(nrow(single) >= 10 & mean(single$CenterLat) != (single$CenterLat[1])) { 
            rs1 <- rasterFromXYZ(as.data.frame(single) 
                                 [, c("CenterLong", "CenterLat", "Probability", "TempPrefMin","TempPrefMax", "SalinityPrefMin","SalinityPrefMax", "OxyPrefMin", "OxyPrefMax")])
            rs_final <- resample(rs1, rs, resample = "ngb") # projecting raster 0.5 deg
            
            name.rs <- paste(code, olayer, sep = "_")
            writeRaster(rs_final, paste(outdir, name.rs, ".grd", sep = ""), overwrite = TRUE)
          }
        }
        stopCluster(cl)
}

system.time(aqua_rs(path = "/QRISdata/Q1216/BritoMorales/Project04b/aquamaps_outputs/02_EpipelagicLayer_csv",
                    outdir = "/QRISdata/Q1216/BritoMorales/Project04b/aquamaps_outputs/02_EpipelagicLayer_rs/",
                    olayer = "surface"))
