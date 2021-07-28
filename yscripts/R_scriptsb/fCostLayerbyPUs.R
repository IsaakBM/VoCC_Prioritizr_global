# This code was written by Isaac Brito-Morales (i.britomorales@uq.edu.au)
# Please do not distribute this code without permission.
# NO GUARANTEES THAT CODE IS CORRECT
# Caveat Emptor!

library(sf)
library(raster)
library(dplyr)
library(stringr)
library(data.table)
library(kader)
library(exactextractr)
library(nngeo)

source("yscripts/R_scriptsb/VoCCPrioritizr_Help.R")
moll <- "+proj=moll +lon_0=0 +datum=WGS84 +units=m +no_defs"

# Function to replace NAs with nearest neighbor. Function wrtitten by Jason Everett (jason.everett@uq.edu.au)
  fCheckNAs <- function(df, vari) {
    if (sum(is.na(pull(df, !!sym(vari)))) > 0){ # Check if there are NAs
      gp <- df %>%
        mutate(isna = is.finite(!!sym(vari))) %>%
        group_by(isna) %>%
        group_split()
      
      out_na <- gp[[1]] # DF with NAs
      out_finite <- gp[[2]] # DF without NAs
      d <- st_nn(out_na, out_finite) %>% # Get nearest neighbour
        unlist()
      
      out_na <- out_na %>%
        mutate(!!sym(vari) := pull(out_finite, !!sym(vari))[d])
      df <- rbind(out_finite, out_na)
    }
    return(df)
  }

# Function to get a mean weighted interpolated cost by Planning Units using as input a Raster object
  CostbyPUs <- function(data, sfdom, moll) {
    # Read raster object
      rs_file <- readAll(raster(data))
      crs(rs_file) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
      weight_rs <- raster::area(rs_file)
      rs_file <- projectRaster(rs_file, crs = CRS(moll), method = "ngb", over = FALSE)
      weight_rs <- projectRaster(weight_rs, crs = CRS(moll), method = "ngb", over = FALSE)
      names(rs_file) <- "layer"
    # Getting cost value by planning unit
      rs_bypu <- exact_extract(rs_file, sfdom, "weighted_mean", weights = weight_rs)
      rs_shp <- sfdom %>%
        dplyr::mutate(cost = rs_bypu) %>%
        dplyr::relocate(pu, cost)
    # Replace NAs with nearest neighbor
      rs_sfInt <- fCheckNAs(df = rs_shp, vari = names(rs_shp)[2]) %>% 
        dplyr::select(-isna)
      return(rs_sfInt)
  }
  
# Run function for every planning domain 
  epCostSF <- CostbyPUs(data = "Inputs/Cost/02-epipelagic_CostRasterTotal.tif", sfdom = pld_ep, moll = moll)
  mpCostSF <- CostbyPUs(data = "Inputs/Cost/03-mesopelagic_CostRasterTotal.tif", sfdom = pld_mp, moll = moll)
  # saveRDS(mpCostSF, "Output/CostOLayer/03-mesopelagic_CostSFTotal.rds")
  bapCostSF <- CostbyPUs(data = "Inputs/Cost/04-bathyabyssopelagic_CostRasterTotal.tif", sfdom = pld_bap, moll = moll)
  # saveRDS(bapCostSF, "Output/CostOLayer/04-bathyabyssopelagic_CostSFTotal.rds")
  sflrCostSF <- CostbyPUs(data = "Inputs/Cost/05-seafloor_CostRasterTotal.tif", sfdom = pld_sflr, moll = moll)
  # saveRDS(sflrCostSF, "Output/CostOLayer/05-seafloorCostSFTotal.rds")