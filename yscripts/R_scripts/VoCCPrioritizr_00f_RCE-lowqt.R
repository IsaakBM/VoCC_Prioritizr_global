# This code was written by Isaac Brito-Morales (i.britomorales@uq.edu.au)
# Please do not distribute this code without permission.
# NO GUARANTEES THAT CODE IS CORRECT
# Caveat Emptor!

rs_lower_qt <- function(path, outdir) {
  
  library(raster)
  library(dplyr)
  library(foreach)
  library(doParallel)  
  
  # Folder's structure
    dir.scenarios <- paste(list.dirs(path = path, full.names = TRUE, recursive = FALSE), sep = "/") # Climate Models Directory
    dir.olayers <- paste(list.dirs(path = dir.scenarios, full.names = TRUE, recursive = FALSE), sep = "/") # Climate Models Directory per ocean layer
  # Begin the parallel structure      
    cores  <-  detectCores()
    cl <- makeCluster(cores - 1)
    registerDoParallel(cl)
    foreach(i = 1:length(dir.olayers), .packages = c("raster", "dplyr")) %dopar% {
      # Reading files and get the lower quantile
        single.dir  <- paste(dir.olayers[i], list.files(path = paste(dir.olayers[i], sep = "/"), pattern = ".tif"), sep = "/")
        rs <- readAll(stack(single.dir)) %>% subset(1)
        qts <- quantile(rs)
        rs[] <- ifelse(rs[] > as.vector(qts[2]), NA, rs[])
      # Write raster file
        ns <- basename(single.dir) # get the names to write the rasters
          var <- unlist(strsplit(x = ns, split = "_"))[2]
          olayer <- unlist(strsplit(x = ns, split = "_"))[1]
          model <- unlist(strsplit(x = ns, split = "_"))[3]
          ssp <- unlist(strsplit(x = ns, split = "_"))[4]
          period <- unlist(strsplit(unlist(strsplit(x = ns, split = "[.]"))[1], split = "_"))[5]
        name.rs <- paste(olayer, var, model, ssp, period, sep = "_")
        # Write the rasters
          writeRaster(rs, paste(outdir, name.rs, ".tif", sep = ""), overwrite = TRUE)
    }
    stopCluster(cl)
}


system.time(rs_lower_qt(path = "climate-change_inputs/vocc_mag", 
                        outdir = "climate-change_inputs/vocc_mag_25qt/"))
