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
# Marine ecoregions
  lg <- readRDS("Output/PlanningUnitsProvinces/pus-epipelagic_Longhurst_.rds") %>% 
    st_transform(crs = CRS(moll)) %>% 
    group_by(province) %>% 
    summarise(ecoregion = sum(as.numeric(factor(province)), do_union = TRUE))
  glw <- readRDS("Output/PlanningUnitsProvinces/pus-mesopelagic_Glasgow_.rds") %>% 
    st_transform(crs = CRS(moll)) %>% 
    group_by(province) %>% 
    summarise(ecoregion = sum(as.numeric(factor(province)), do_union = TRUE))
  sflr <- readRDS("Output/PlanningUnitsProvinces/pus-seafloor_GOODS_.rds") %>% 
    st_transform(crs = CRS(moll)) %>% 
    group_by(province) %>% 
    summarise(ecoregion = sum(as.numeric(factor(province)), do_union = TRUE))
  pldom <- list(lg, lg, lg, glw, glw, glw, glw, glw, glw, sflr, sflr, sflr)
