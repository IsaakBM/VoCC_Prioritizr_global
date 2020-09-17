# This code was written by Isaac Brito-Morales (i.britomorales@uq.edu.au)
# Please do not distribute this code without permission.
# NO GUARANTEES THAT CODE IS CORRECT
# Caveat Emptor!

# AIM: Function that reads a AquaMaps files and returns .csv species files
# path: folder's name where aquampas.csv files are located
# outdir: where to put the .csv species files
# olayer: for what ocean layer do you want the species
# prob_threshold: 
# sp_env: Species envelope. >= 10 good cells (1) or 3-9 good cells (2). If you want all, write 1|2
# data: by species or richness?
# region: a raster (or shapefile) of your region of interest. If you don't know how, just load a global raster or shapefile and then use 
  # the interactive drawExtent() function to get a new crop raster (or shapefile). The use this object in region argument to rin the function

# Input Files
# 1. species information (e.g. species ID, taxonomy, "reviewed" or not, but without spatial information)  = hcaf.csv
# 2. information for each half-degree cell (e.g. max/min depth, salinity, temperature, etc) = hpen.csv
# 3. a lookup table of which species are found in each half-degree cell (and the "probability of occurrence" of that species in that cell)   = cell.csv


aqua_start <- function(path, outdir, olayer, prob_threshold, sp_env, data, region, ...) { # add species by species or richness?
  
  library(raster)
  library(data.table)
  library(dplyr)
  library(tidyselect)
  library(foreach)
  library(doParallel)
  
  # List of pacakges that we will use
    # list.of.packages <- c("raster", "data.table", "dplyr", "foreach", "doParallel")
    # Load packages
      # lapply(list.of.packages, library, character.only = TRUE)
      # If is not installed, install the package
        # new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])] # is the package in MY list of packages
        # if(length(new.packages)) install.packages(new.packages) # if not, installed
  
  # file's names
    dir <- path
      first_csv <- list.files(path = dir, pattern = "*hcaf.*.csv$", full.names = TRUE)
      second_csv <- list.files(path = dir, pattern = "*hspen.*.csv$", full.names = TRUE)
      third_csv <- list.files(path = dir, pattern = "*occursum.*.csv$", full.names = TRUE)
  
  # Reading input files
    # Create a buffer zone to crop species distribution if the projection is not a world map
    region <- raster(region) # reading the region file (in this case a raster...). maybe a good idea a shapefile
    ext <- extent(region)
    if (ext@xmin > -180 & ext@xmax < 180 & ext@ymin > -90 & ext@ymax < 90) {
      buff <- c(ext@xmin-2, ext@xmax+2, ext@ymin-2, ext@ymax+2) # create a buffer zone (just in case!)
      hcaf <- fread(first_csv) %>% 
        dplyr::select(SpeciesID, CenterLat, CenterLong, Probability) %>% 
        dplyr::filter(Probability >= prob_threshold) %>% 
        dplyr::filter(CenterLat >= buff[3] & CenterLat <= buff[4] & CenterLong >= buff[1] & CenterLong <= buff[2])
    } else {
      hcaf <- fread(first_csv) %>% 
        dplyr::select(SpeciesID, CenterLat, CenterLong, Probability) %>% 
        dplyr::filter(Probability >= prob_threshold)
        # [1] "SpeciesID"   "CsquareCode"
        # [3] "Probability" "CenterLat"  
        # [5] "CenterLong"  "LOICZID"
    }
  
    hspen <- fread(second_csv) %>% 
      dplyr::select(tidyselect::matches("Species|Pelagic|Depth|Oxy|Temp|Salinity|Rank")) %>% 
      dplyr::filter(Rank == sp_env)
      # "SpeciesID"  
      # "DepthMin" "DepthPrefMin" "DepthPrefMax" "DepthMax" "MeanDepth"
      # "Pelagic"
      # "TempMin" "TempPrefMin" "TempPrefMax" "TempMax"
      # "SalinityMin" "SalinityPrefMin" "SalinityPrefMax" "SalinityMax"
      # "OxyMin" "OxyMinPrefMin" "OxyMinPrefMax" "OxyMinMax"
    
    speciesInfo <- fread(third_csv, fill = TRUE)
    
  # Filtering by layers before loops
  if(olayer == "surface") {
      hspen_v2 <- hspen %>% filter(DepthPrefMin <= 200 | DepthPrefMax <= 200)
    } else if (olayer == "mesopelagic") {
      hspen_v2 <- hspen %>% filter(DepthPrefMin > 200 & DepthPrefMin <= 1000 | DepthPrefMax > 200 & DepthPrefMax <= 1000)
    } else if (olayer == "bathyabyssopelagic") {
      hspen_v2 <- hspen %>% filter(DepthPrefMin > 1000 | DepthPrefMax > 1000)
    } else if (olayer == "all") {
      hspen_v2 <- hspen %>% filter(DepthPrefMin >= 0 | DepthPrefMax >= 0)
    } else {
      hspen_v2 <- hspen
    }
    speciesID <- hspen_v2$SpeciesID # how many species?
    IDs_df <- vector("list", length(speciesID))
  # Set up parallel structure
    ncores <- 20 
    cl <- makeCluster(ncores)
    registerDoParallel(cl)
    # Loops
      IDs_df <- foreach(i = 1:length(speciesID), .packages = c("data.table", "dplyr")) %dopar% { # if you dont add any combine argument it will return a list
        x <- hcaf[hcaf$SpeciesID == speciesID[i],]
        y <- hspen_v2[hspen_v2$SpeciesID == speciesID[i],]
        z <- left_join(x = x, y = y, by = "SpeciesID")
        IDs_df[[i]] <- z
      }
      stopCluster(cl)
      IDs_df <- IDs_df[lapply(IDs_df, nrow) > 0] # removing empty species from previous list
  # Defining outcome
    if(data == "species") { # write list elements (speciesID)
      for(j in 1:length(IDs_df)) {
        name.csv <- paste(IDs_df[[j]][1,1], olayer, sep = "_")
        write.csv(IDs_df[[j]], paste(outdir, name.csv, ".csv", sep = ""), row.names = FALSE)
        print(paste0(j, " of ", length(IDs_df)))
        }
      } else if (data == "richness") { # write a unique data.frame for total richness
        sp_richness <- do.call(rbind, IDs_df)
          sp_richness <- sp_richness %>% group_by(CenterLat, CenterLong) %>% summarise(richness = n()) %>% data.frame()
        name.csv <- paste("spp_richness", olayer, sep = "_")
        write.csv(sp_richness, paste(outdir, name.csv, ".csv", sep = ""), row.names = FALSE)
        return(sp_richness)
      } else {
        return(IDs_df)
      }
  
  # Summ table with species taxonomic info per ocean layer []
    spp_all <- do.call(rbind, IDs_df)
    speciesInfo <- speciesInfo[speciesInfo$speciesID %in% spp_all$SpeciesID,]
    name.sum <- paste("01_speciesInfo", olayer, sep = "_")
    write.csv(speciesInfo, paste(outdir, name.sum, ".csv", sep = ""), row.names = FALSE)
}

system.time(aqua_start(path = "/QRISdata/Q1216/BritoMorales/AquaMaps_wflow/AquaMaps/v2019a",
                       outdir = "/QRISdata/Q1216/BritoMorales/Project04b/aquamaps_outputs/",
                       olayer = "all",
                       prob_threshold = 0.5,
                       sp_env = 1,
                       data = "richness",
                       region = "/QRISdata/Q1216/BritoMorales/Project04b/ETOPO1_05deg/ETOPO1_ocean.grd"))

# system.time(aqua_start(path = "/Users/bri273/Desktop/AquaMaps_wflow/AquaMaps/v2019a",
#                        outdir = "/Users/bri273/Desktop/AquaMaps_wflow/CSVs/01_surface_mediterranean/",
#                        olayer = "surface",
#                        prob_threshold = 0.5,
#                        sp_env = 1,
#                        data = "richness",
#                        region = "/Users/bri273/Desktop/AquaMaps_wflow/ETOPO1_05deg/etopos_mediterranean.grd"))

