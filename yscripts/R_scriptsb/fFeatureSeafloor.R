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
moll <- "+proj=moll +lon_0=0 +datum=WGS84 +units=m +no_defs"
dir.climatic <- list.dirs(path = "Inputs", full.names = TRUE, recursive = FALSE)[6]
files.shp <- list.files(path = dir.climatic, pattern = paste0((paste0(".*.shp$")), collapse = "|"), full.names = TRUE)

for(i in seq_along(files.shp)) {
  st_read(files.shp[i]) %>%    
    st_make_valid() %>% 
    st_crop(xmin = -180, ymin = -90, xmax = 180, ymax = 90) %>% 
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
  abys %>% 
    dplyr::filter(Class == abysType[i]) %>%
    st_make_valid() %>% 
    saveRDS(paste0("Inputs/", paste("Abyss", abysType[i], sep = "_"), ".rds"))}
# Canyon
canyon <- sfL[[2]]
canyonType <- as.vector(unique(canyon$type))
for(i in seq_along(canyonType)){
  canyon %>% 
    dplyr::filter(type == canyonType[i]) %>% 
    st_make_valid() %>% 
    saveRDS(paste0("Inputs/", paste("Canyon", canyonType[i], sep = "_"), ".rds"))}
# Shelf 
shelf <- sfL[[3]]
shelfType <- as.vector(unique(shelf$Class))
for(i in seq_along(shelfType)){
  shelf %>% 
    dplyr::filter(Class == shelfType[i]) %>% 
    st_make_valid() %>% 
    saveRDS(paste0("Inputs/", paste("Shelf", shelfType[i], sep = "_"), ".rds"))}
# Shelf 
ShelfValley  <- sfL[[4]]
ShelfValleyType <- as.vector(unique(ShelfValley$Type))
for(i in seq_along(ShelfValleyType)){
  ShelfValley %>% 
    dplyr::filter(Type == ShelfValleyType[i]) %>% 
    st_make_valid() %>% 
    saveRDS(paste0("Inputs/", paste("ShelfValley", ShelfValleyType[i], sep = "_"), ".rds"))}

####################################################################################
####### 0.- General shapefiles by planning domain
####################################################################################
moll <- "+proj=moll +lon_0=0 +datum=WGS84 +units=m +no_defs"
files.rds <- list.files(path = "Output/FeaturesPUs/06_GeomorphicFeatures", pattern = paste0((paste0(".*.rds$")), collapse = "|"), full.names = TRUE)
imp <- paste0(c("Basins", "Sills", "Escarpments", "Seamounts", "Guyots", "Canyons", "Ridges", "Troughs", "Fans", "Plateaus", "Trenches", "Bridges"), collapse = "|")
imp <- files.rds[str_detect(files.rds, pattern = imp)]
sfGeo <- lapply(imp, function(x) readRDS(x))
names(sfGeo) <- str_remove_all(basename(imp), pattern = ".rds")

theme_opts3 <- list(theme(panel.grid.minor = element_blank(),
                          panel.grid.major = element_blank(),
                          panel.background = element_blank(),
                          plot.background = element_rect(fill = "white"),
                          panel.border = element_blank(),
                          axis.line = element_blank(),
                          axis.text.x = element_blank(),
                          axis.text.y = element_blank(),
                          axis.ticks = element_blank(),
                          axis.ticks.length = unit(.25, "cm"),
                          axis.title.x = element_blank(),
                          axis.title.y = element_text(face = "plain", size = 25, angle = 90),
                          plot.title = element_text(face = "plain", size = 25, hjust = 0.5),
                          legend.title = element_text(colour = "black", face = "bold", size = 25),
                          legend.text = element_text(colour = "black", face = "bold", size = 20),
                          legend.key.height = unit(2.5, "cm"),
                          legend.key.width = unit(1.4, "cm"),
                          plot.tag = element_text(size = 25, face = "bold")))
sfL <- vector("list", length = length(sfGeo))
for(i in seq_along(sfGeo)){
  sfL[[i]] <- ggplot() +
    geom_sf(data = sfGeo[[i]], fill = "#31a354", color = NA) +
    geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
    theme_opts3 +
    ggtitle(str_remove_all(unique(sfGeo[[i]]$feature_names), pattern = "_seafloor"))}

p1.1 <- patchwork::wrap_plots(sfL, ncol = 4, byrow = TRUE)
ggsave("Figures/Exploratory/GeomFeatures03.pdf", plot = p1.1, width = 28, height = 12, dpi = 300, limitsize = FALSE)
ggsave("Figures/Exploratory/GeomFeatures03.png", plot = p1.1, width = 28, height = 12, dpi = 300, limitsize = FALSE)

  




