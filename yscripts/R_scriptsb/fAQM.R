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

plg <- readRDS("Inputs/AquaMaps_SpeciesInfoFB_Pelagic.rds") # 2572
bth <- readRDS("Inputs/AquaMaps_SpeciesInfoFB_Benthic.rds") # 21018

aqm_move <- function(dirF, plg, outdir, from, to) {
  
  files.shp <- list.files(path = dirF, pattern = paste0((paste0(".*.shp$")), collapse = "|"), full.names = TRUE)
  
  d1 <- files.shp %>% 
    as_tibble() %>% 
    rename(dir = value)
  d2 <- d1 %>% 
    mutate(sps = str_remove_all(basename(dir), pattern = paste0(c("_", from, ".shp"), collapse = "|")))
  d3 <- d2 %>% 
    dplyr::filter(sps %in% str_replace_all(bth$SpeciesID, pattern = "_", replacement = "-"))
  dff <- as.vector(d3$dir)
  
  ncores <- 3
  cl <- makeCluster(ncores)
  registerDoParallel(cl)
  sps_ls <- foreach(i = 1:length(dff), .packages = c("raster", "sf", "dplyr", "stringr", "lwgeom", "data.table")) %dopar% {
    f1 <- st_read(dff[i])
    saveRDS(f1, paste(outdir, paste0(basename(str_remove_all(basename(dff[i]), pattern = paste0(c("_", from, ".shp"), collapse = "|"))), to, ".rds"), sep = ""))
  }
  stopCluster(cl)
}

aqm_move(dirF = "Inputs/Aqm/02_EpipelagicLayer_shp", bth, outdir = "Inputs/Aqm/05_Seafloor_rds/", from = "epipelagic", to = "_Seafloor")

# epi <- list.files(path = "Inputs/Aqm/02_EpipelagicLayer_rds", pattern = paste0((paste0(".*.rds$")), collapse = "|"), full.names = TRUE) # 1567
# length(epi)
# meso <- list.files(path = "Inputs/Aqm/03_MesopelagicLayer_rds", pattern = paste0((paste0(".*.rds$")), collapse = "|"), full.names = TRUE) # 1335
# length(meso)
# bap <- list.files(path = "Inputs/Aqm/04_BathyAbyssopelagicLayer_rds", pattern = paste0((paste0(".*.rds$")), collapse = "|"), full.names = TRUE) # 524
# length(bap)
# sfl <- list.files(path = "Inputs/Aqm/05_Seafloor_rds", pattern = paste0((paste0(".*.rds$")), collapse = "|"), full.names = TRUE) # 21014
# length(sfl)
