# This code was written by Isaac Brito-Morales (i.britomorales@uq.edu.au)
# Please do not distribute this code without permission.
# NO GUARANTEES THAT CODE IS CORRECT
# Caveat Emptor!

library(sf)
library(raster)
library(ggplot2)
library(prioritizr)
library(gurobi)
library(patchwork)
library(rnaturalearth)
library(rnaturalearthdata)
library(RColorBrewer)
library(dplyr)
library(readr)
library(ggtext)
library(stringr)
library(data.table)
library(kader)

####################################################################################
####### 0.- General shapefiles by planning domain
####################################################################################
# Robinson Projection
moll <- "+proj=moll +lon_0=0 +datum=WGS84 +units=m +no_defs"
dir.climatic <- list.dirs(path = "Inputs", full.names = TRUE, recursive = FALSE)[6]
files.shp <- list.files(path = dir.climatic, pattern = paste0((paste0(".*.rds$")), collapse = "|"), full.names = TRUE)

for(i in seq_along(sf_list)) {
  st_read(files.shp[i]) %>%    
    st_transform(crs = CRS(moll)) %>% 
    saveRDS(paste0("Inputs/", str_remove(basename(files.shp[i]), pattern = ".shp"), ".rds"))}

####################################################################################
####### 0.- General shapefiles by planning domain
####################################################################################
files.rds <- list.files(path = dir.climatic, pattern = paste0((paste0(".*.rds$")), collapse = "|"), full.names = TRUE)
geoF <- lapply(files.rds, function(x) readRDS(x))
names(geoF) <- str_remove_all(basename(files.rds), pattern = ".rds")

sfL <- vector("list", length = length(geoF))
for(j in seq_along(geoF)){
  if(sum(str_detect(colnames(geoF[[j]]), c("Class", "class", "Type", "type"))) != 0) {
    clm <- paste0(c("Class", "class", "Type", "type"), collapse = "|")
    d1 <- geoF[[j]]
    sfL[[j]] <- d1 %>% 
      dplyr::select(Geomorphic, colnames(d1)[str_detect(colnames(d1), string = clm)]) %>% 
      dplyr::relocate(Geomorphic, geometry)
  }
}
names(sfL) <- str_remove_all(files.rds, pattern = ".rds")
sfL <- sfL[lengths(sfL) != 0]

# Abyss
abys <- sfL[[1]]
abysType <- as.vector(unique(abys$Class))
for(i in seq_along(abysType)){
    dplyr::filter(Class == abysType[i]) %>% 
    saveRDS(paste0("Inputs/", paste("Abyss", abysType[i], sep = "_"), ".rds"))}
# Canyon
canyon <- sfL[[2]]
canyonType <- as.vector(unique(canyon$type))
for(i in seq_along(canyonType)){
  canyon %>% 
    dplyr::filter(type == canyonType[i]) %>% 
    saveRDS(paste0("Inputs/", paste("Canyon", canyonType[i], sep = "_"), ".rds"))}
# Shelf 
shelf <- sfL[[3]]
shelfType <- as.vector(unique(shelf$Class))
for(i in seq_along(shelfType)){
  shelf %>% 
    dplyr::filter(Class == shelfType[i]) %>% 
    saveRDS(paste0("Inputs/", paste("Shelf", shelfType[i], sep = "_"), ".rds"))}
# Shelf 
ShelfValley  <- sfL[[4]]
ShelfValleyType <- as.vector(unique(ShelfValley$Type))
for(i in seq_along(ShelfValleyType)){
  ShelfValley %>% 
    dplyr::filter(Type == ShelfValleyType[i]) %>% 
    saveRDS(paste0("Inputs/", paste("ShelfValley", ShelfValleyType[i], sep = "_"), ".rds"))}





