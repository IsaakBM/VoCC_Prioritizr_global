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
# Moll Projection
  moll <- "+proj=moll +lon_0=0 +datum=WGS84 +units=m +no_defs"
# Planning domains
  pld_ep <- st_read("Output/02_abnjs_filterdepth/abnj_02-epipelagic_global_moll_05deg_depth/abnj_02-epipelagic_global_moll_05deg_depth.shp") %>% 
    dplyr::rename(pu = layer) %>% 
    dplyr::select(pu)
  pld_mp <- st_read("Output/02_abnjs_filterdepth/abnj_03-mesopelagic_global_moll_05deg_depth/abnj_03-mesopelagic_global_moll_05deg_depth.shp") %>% 
    dplyr::rename(pu = layer) %>% 
    dplyr::select(pu)
  pld_bap <- st_read("Output/02_abnjs_filterdepth/abnj_04-bathyabysso_global_moll_05deg_depth/abnj_04-bathyabysso_global_moll_05deg_depth.shp") %>% 
    dplyr::rename(pu = layer) %>% 
    dplyr::select(pu)
  pld_sflr <- st_read("Output/02_abnjs_filterdepth/abnj_05-seafloor_global_moll_05deg_depth/abnj_05-seafloor_global_moll_05deg_depth.shp") %>% 
    dplyr::rename(pu = layer) %>% 
    dplyr::select(pu)
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
# MPAs
  mpas_ep <- read.csv("Output/PlanningUnitsLocks/pus-epipelagic_mpas_.csv") %>% 
    dplyr::rename(id = layer) %>% 
    dplyr::filter(province != "non-categ_mpas")
  mpas_mp <- read.csv("Output/PlanningUnitsLocks/pus-mesopelagic_mpas_.csv") %>% 
    dplyr::rename(id = layer) %>% 
    dplyr::filter(province != "non-categ_mpas")
  mpas_bap <- read.csv("Output/PlanningUnitsLocks/pus-bathyabyssopelagic_mpas_.csv") %>% 
    dplyr::rename(id = layer) %>% 
    dplyr::filter(province != "non-categ_mpas")
  mpas_sflr <- read.csv("Output/PlanningUnitsLocks/pus-seafloor_mpas_.csv") %>% 
    dplyr::rename(id = layer) %>% 
    dplyr::filter(province != "non-categ_mpas")
# VMEs
  vmes_ep <- read.csv("Output/PlanningUnitsLocks/pus-epipelagic_VMEs_.csv") %>% 
    dplyr::rename(id = layer) %>% 
    dplyr::filter(province != "non-categ_VMEs")
  vmes_mp <- read.csv("Output/PlanningUnitsLocks/pus-mesopelagic_VMEs_.csv") %>% 
    dplyr::rename(id = layer) %>% 
    dplyr::filter(province != "non-categ_VMEs")
  vmes_bap <- read.csv("Output/PlanningUnitsLocks/pus-bathyabyssopelagic_VMEs_.csv") %>% 
    dplyr::rename(id = layer) %>% 
    dplyr::filter(province != "non-categ_VMEs")
  vmes_sflr <- read.csv("Output/PlanningUnitsLocks/pus-seafloor_VMEs_.csv") %>% 
    dplyr::rename(id = layer) %>% 
    dplyr::filter(province != "non-categ_VMEs")
  
  
  
