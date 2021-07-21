# This code was written by Isaac Brito-Morales (i.britomorales@uq.edu.au)
# Please do not distribute this code without permission.
# NO GUARANTEES THAT CODE IS CORRECT
# Caveat Emptor!


features_merge <- function(path, outdir) { 
  
  library(sf)
  library(dplyr)
  library(doParallel)
  library(stringr)
  library(lwgeom)
  library(data.table)
  library(readr)

# Reading conservation features .rds files (AquaMaps)
  pattern1 <-  c(paste0("*", ".*.rds$"), paste0("*", ".*shp$"))
  files <- list.files(path = path, pattern = paste0(pattern1, collapse = "|"), full.names = TRUE)
# Loop through each file
  ncores <- 5
  cl <- makeCluster(ncores)
  registerDoParallel(cl)
  ftr_list <- vector("list", length = length(files))
  ftrs <- foreach(i = 1:length(files), .packages = c("sf", "dplyr", "stringr", "lwgeom", "data.table", "readr")) %dopar% {
    ftr_list[[i]] <- readRDS(files[i]) %>% 
      as_tibble() %>% 
      dplyr::select(-geometry) %>% 
      dplyr::rename(pu = cellsID)}
  stopCluster(cl)
  ftrs_df <- data.table::rbindlist(ftrs, use.names = TRUE)
# Write .csv object
  nsm <- paste(str_extract(basename(files[1]), pattern = paste0(c("epipelagic", "mesopelagic", "BathyAbyssopelagic", "seafloor"), collapse = "|")), ".csv", sep = "")
  write_csv(ftrs_df, paste(outdir, nsm, sep = ""))
}

system.time(features_merge(path = "/scratch/user/uqibrito/Project04c/Output/FeaturesPUs/04_BathyAbyssopelagicLayer", 
                           outdir = "/scratch/user/uqibrito/Project04c/Output/FeaturesOLayer"))