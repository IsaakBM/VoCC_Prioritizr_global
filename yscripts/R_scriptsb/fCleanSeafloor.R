# This code was written by Isaac Brito-Morales (i.britomorales@uq.edu.au)
# Please do not distribute this code without permission.
# NO GUARANTEES THAT CODE IS CORRECT
# Caveat Emptor!

fCleanGeo <- function(path, outdir, proj.geo) {
  library(sf)
  library(lwgeom)
  library(dplyr)
  library(stringr)
  library(data.table)
  library(doParallel)
  
  files.shp <- list.files(path = path, pattern = paste0((paste0(".*.shp$")), collapse = "|"), full.names = TRUE)
  ncores <- 5
  cl <- makeCluster(ncores)
  registerDoParallel(cl)
  foreach(i = 1:length(files.shp), .packages = c("sf", "dplyr", "stringr", "lwgeom", "data.table")) %dopar% {
    st_read(files.shp[i]) %>%    
      st_make_valid() %>% 
      st_crop(xmin = -180, ymin = -90, xmax = 180, ymax = 90) %>% 
      st_transform(crs = CRS(proj.geo)) %>% 
      saveRDS(paste0(outdir, str_remove(basename(files.shp[i]), pattern = ".shp"), ".rds"))}
  stopCluster(cl)
}

system.time(fCleanGeo(path = "/scratch/user/uqibrito/Project04c/Inputs/Global_Seafloor_Geomorphic_Features", 
                      outdir = "/scratch/user/uqibrito/Project04c/Inputs/GeomorphicFeatures_rds/", 
                      proj.geo = "+proj=moll +lon_0=0 +datum=WGS84 +units=m +no_defs"))