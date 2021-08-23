# This code was written by Isaac Brito-Morales (i.britomorales@uq.edu.au)
# Please do not distribute this code without permission.
# NO GUARANTEES THAT CODE IS CORRECT
# Caveat Emptor!

source("yscripts/R_scriptsb/VoCCPrioritizr_Help.R")

####################################################################################
####### 1.- Summary Stast Individual Solutions
####################################################################################
# 
  mpaList <- list(mpas_ep, mpas_ep, mpas_ep, mpas_mp, mpas_mp, mpas_mp, mpas_bap, mpas_bap, mpas_bap, mpas_sflr, mpas_sflr, mpas_sflr)
  vmeList <- list(vmes_ep, vmes_ep, vmes_ep, vmes_mp, vmes_mp, vmes_mp, vmes_bap, vmes_bap, vmes_bap, vmes_sflr, vmes_sflr, vmes_sflr)
# 
  SummSol <- function(data, mpas, vmes) {
    d1 <- readRDS(data)
    d2 <- d1[[2]] %>% 
      dplyr::filter(!(id %in% mpas$id)) %>% 
      dplyr::filter(!(id %in% vmes$id)) %>% 
      dplyr::rename(pu = id) %>% 
      as_tibble() %>% 
      dplyr::select(pu, solution_1) %>% 
      dplyr::filter(solution_1 == 1)
    
    ff <- tibble(Ocean_layer = str_remove(unlist(str_split(basename(data), "_"))[2], "Layer"), 
           Scenario = unlist(str_split(basename(data), "_"))[3], 
           Protection = round(length(unique(d2$pu))/length(unique(d1[[2]]$id))*100, 1))
    
    return(ff)
  }
# 
  dir.sol <- list.dirs(path = "Prioritisation/PrioritizrSolutionsCost", full.names = TRUE, recursive = FALSE)
  solCostF <- list.files(path = dir.sol, pattern = ".rds", full.names = TRUE)
# 
  DFList <- vector("list", length = length(solCostF))
  for(i in seq_along(solCostF)){ DFList[[i]] <- SummSol(data = solCostF[i], mpas = mpaList[[i]], vmes = vmeList[[i]])}
  DFList <- do.call(rbind, DFList) %>% 
    as_tibble()
  # write_csv(DFList, "SummStats/Solutions_SummStats_features_10100.csv")

####################################################################################
####### 2.- Summary Stast No Regret Networks
####################################################################################
# 
  mpaList2 <- list(mpas_ep, mpas_mp, mpas_bap, mpas_sflr, mpas_ep, mpas_ep)
  vmeList2 <- list(vmes_ep, vmes_mp, vmes_bap, vmes_sflr, vmes_ep, vmes_ep)
#
  SummSol_NR <- function(data, mpas, vmes) {
    d1 <- readRDS(data)
    d2 <- d1 %>% 
      dplyr::filter(!(id %in% mpas$id)) %>% 
      dplyr::filter(!(id %in% vmes$id)) %>% 
      dplyr::rename(pu = id) %>% 
      as_tibble() %>% 
      dplyr::filter(.[[2]] != 0)
    
    ff <- tibble(Ocean_layer = str_remove(paste0(unlist(str_split(basename(data), "_"))[1:2], collapse = "_"), ".rds"), 
                 Protection = round(length(unique(d2$pu))/length(unique(d1$id))*100, 1))
    
    return(ff)
  }
  
# 
  dir.NR <- list.dirs(path = "SummStats/PrioritizrSolutionsCost", full.names = TRUE, recursive = FALSE)
  NReg <- list.files(path = dir.NR, pattern = ".rds", full.names = TRUE)
  NReg <- c(NReg[2], NReg[3], NReg[1], NReg[6], NReg[4], NReg[5])
# 
  DFList2 <- vector("list", length = length(NReg))
  for(i in seq_along(NReg)){ DFList2[[i]] <- SummSol_NR(data = NReg[i], mpas = mpaList2[[i]], vmes = vmeList2[[i]])}
  DFList2 <- do.call(rbind, DFList2) %>% 
    as_tibble()
  # write_csv(DFList2, "SummStats/NoRegret_SummStats_features_10100.csv")


####################################################################################
####### 3.- Number of Species, Conservation Features, etc.
####################################################################################
# Number of Species 
  ep_sps <- fread("Prioritisation/PrioritizrFiles/features_10100/02_EpipelagicLayer_ssp126/spec_02_EpipelagicLayer_ssp126.dat") %>% 
    dplyr::mutate(speciesID = str_split(string = name, pattern = "_", simplify = TRUE)[,1]) %>%
    dplyr::arrange(id) %>% 
    dplyr::select(id, speciesID)
  length(unique(ep_sps$speciesID)) # 1081 species
  
  mp_sps <- fread("Prioritisation/PrioritizrFiles/features_10100/03_MesopelagicLayer_ssp126/spec_03_MesopelagicLayer_ssp126.dat") %>% 
    dplyr::mutate(speciesID = str_split(string = name, pattern = "_", simplify = TRUE)[,1]) %>%
    dplyr::arrange(id) %>% 
    dplyr::select(id, speciesID)
  length(unique(mp_sps$speciesID)) # 1300 species
  
  bap_sps <- fread("Prioritisation/PrioritizrFiles/features_10100/04_BathyAbyssopelagicLayer_ssp126/spec_04_BathyAbyssopelagicLayer_ssp126.dat") %>% 
    dplyr::mutate(speciesID = str_split(string = name, pattern = "_", simplify = TRUE)[,1]) %>%
    dplyr::arrange(id) %>% 
    dplyr::select(id, speciesID)
  length(unique(bap_sps$speciesID)) # 519 species

  sflr_sps <- fread("Prioritisation/PrioritizrFiles/features_10100/05_Seafloor_ssp126/spec_05_Seafloor_ssp126.dat") %>% 
    dplyr::mutate(speciesID = str_split(string = name, pattern = "_", simplify = TRUE)[,1]) %>%
    dplyr::arrange(id) %>% 
    dplyr::select(id, speciesID) %>% 
    dplyr::filter(str_detect(string = speciesID, 
                             pattern = paste0(c("Basins", "Bridges", "Canyons", "Escarpments", 
                                                "Fans", "Guyots", "Plateaus", "Ridges", 
                                                "Seamounts", "Sills", "Trenches", "Troughs"), 
                                              collapse = "|")) == FALSE)
  length(unique(sflr_sps$speciesID)) # 10860 species

  all_sps <- rbind(ep_sps, mp_sps, bap_sps, sflr_sps)  
  length(unique(all_sps$speciesID)) # 10872 species in TOTAL
  
# Number of Conservation Features
  ep_cf <- fread("Prioritisation/PrioritizrFiles/features_10100/02_EpipelagicLayer_ssp126/spec_02_EpipelagicLayer_ssp126.dat") %>% 
    dplyr::mutate(speciesID = str_remove_all(string = name, pattern = "_RCE|_VoCC")) %>% 
    dplyr::arrange(id) %>% 
    dplyr::select(id, speciesID)
  length(unique(ep_cf$speciesID)) # 12791 conservation features
  
  mp_cf <- fread("Prioritisation/PrioritizrFiles/features_10100/03_MesopelagicLayer_ssp126/spec_03_MesopelagicLayer_ssp126.dat") %>% 
    dplyr::mutate(speciesID = str_remove_all(string = name, pattern = "_RCE|_VoCC")) %>% 
    dplyr::arrange(id) %>% 
    dplyr::select(id, speciesID)
  length(unique(mp_cf$speciesID)) # 15141 conservation features
  
  bap_cf <- fread("Prioritisation/PrioritizrFiles/features_10100/04_BathyAbyssopelagicLayer_ssp126/spec_04_BathyAbyssopelagicLayer_ssp126.dat") %>% 
    dplyr::mutate(speciesID = str_remove_all(string = name, pattern = "_RCE|_VoCC")) %>% 
    dplyr::arrange(id) %>% 
    dplyr::select(id, speciesID)
  length(unique(bap_cf$speciesID)) # 7083 conservation features
  
  sflr_cf <- fread("Prioritisation/PrioritizrFiles/features_10100/05_Seafloor_ssp126/spec_05_Seafloor_ssp126.dat") %>% 
    dplyr::mutate(speciesID = str_remove_all(string = name, pattern = "_RCE|_VoCC")) %>% 
    dplyr::arrange(id) %>% 
    dplyr::select(id, speciesID)
  length(unique(sflr_cf$speciesID)) # 31078 conservation features
  
  all_cf <- rbind(ep_cf, mp_cf, bap_cf, sflr_cf)  
  length(unique(all_cf$speciesID)) # 66,093 conservation features in TOTAL
  
####################################################################################
####### 4.- COST Number of Species
####################################################################################
# 
  epCost <- readRDS("Inputs/Cost/02-epipelagic_CostbySpecies.rds")
  epCost <- lapply(epCost, function(x) colnames(x)[3])
  epCost <- do.call(rbind, epCost) %>% 
    as_tibble() # 221 prices
  
  mpCost <- readRDS("Inputs/Cost/03-mesopelagic_CostbySpecies.rds")
  mpCost <- lapply(mpCost, function(x) colnames(x)[3])
  mpCost <- do.call(rbind, mpCost) %>% 
    as_tibble() # 232 prices
  
  bapCost <- readRDS("Inputs/Cost/04-bathyabyssopelagic_CostbySpecies.rds")
  bapCost <- lapply(bapCost, function(x) colnames(x)[3])
  bapCost <- do.call(rbind, bapCost) %>% 
    as_tibble() # 55 prices
  
  sflrCost <- readRDS("Inputs/Cost/05-seafloor_CostbySpecies.rds")
  sflrCost <- lapply(sflrCost, function(x) colnames(x)[3])
  sflrCost <- do.call(rbind, sflrCost) %>% 
    as_tibble() # 834 prices
  
  all_cost <- rbind(epCost, mpCost, bapCost, sflrCost)
  length(unique(all_cost$V1))

####################################################################################
####### 
####################################################################################
library(sf)
library(dplyr)
  
  ep1 <- readRDS("Prioritisation/PrioritizrSolutionsCost/features_10100/02_EpipelagicLayer_ssp126_0_1.rds")
  ep1 <- ep1[[2]] %>% 
    dplyr::filter(solution_1 == 1)
  sum(ep1$cost) # 1,034,244
  nrow(ep1)
  
  ep2 <- readRDS("Prioritisation/PrioritizrSolutionsNCostA/features_10100/02_EpipelagicLayer_ssp126_0_1.rds")
  ep2 <- ep2[[2]] %>% 
    dplyr::filter(solution_1 == 1)
  sum(ep2$cost) # 117,148,060
  nrow(ep2)
  # thinking about doing a face-face plot?  
  
  
####################################################################################
####### 
####################################################################################
  library(sf)
  library(dplyr)
  library(readr)
  
  sol_rds <- list.files(path = "Prioritisation/PrioritizrSolutionsCost/features_10100", pattern = ".rds", full.names = TRUE)
  summStat <- lapply(sol_rds, function(x){
    d1 <- readRDS(x)
    dff <- tibble(mean = mean(d1[[1]]$data$features$prop, na.rm = TRUE), 
                  sd = sd(d1[[1]]$data$features$prop, na.rm = TRUE), 
                  median = median(d1[[1]]$data$features$prop, na.rm = TRUE), 
                  q1 = quantile(d1[[1]]$data$features$prop, probs = 0.25),
                  q3 = quantile(d1[[1]]$data$features$prop, probs = 0.75))})
  
  ff <- do.call(rbind, summStat)
  mean(ff$mean) # 0.79 (0.79 +- )
  sd(ff$mean) # 0.12
  median(ff$mean) # 0.74
  quantile(ff$mean, probs = 0.25) # 0.70
  quantile(ff$mean, probs = 0.75) # 0.82
  
  # mean: 0.79 +- 0.12 (n = 12)
  # median: 0.74 (0.70, 0.82)
  
####################################################################################
####### AQMs Richness Phyla Supp Table
####################################################################################
library(sf)
library(dplyr)
library(data.table)
library(readr)
library(stringr)
  
# 
  aqm <- fread("Inputs/Aqm/speciesoccursum.csv", stringsAsFactors = FALSE, fill = TRUE) %>% 
    dplyr::select(speciesID, phylum)

# Epipelagic
  ep <- fread("Prioritisation/PrioritizrFiles/features_10100/02_EpipelagicLayer_ssp126/spec_02_EpipelagicLayer_ssp126.dat") %>% 
    dplyr::mutate(speciesID = str_split(string = name, pattern = "_", simplify = TRUE)[,1]) %>% 
    dplyr::select(speciesID)
  ep_species_main <- dplyr::left_join(x = ep, y = aqm,  by = "speciesID") %>% 
    distinct(.keep_all = TRUE) %>% 
    group_by(phylum) %>% 
    summarise(epipelagic = n())
# Mesopelagic
  mp <- fread("Prioritisation/PrioritizrFiles/features_10100/03_MesopelagicLayer_ssp126/spec_03_MesopelagicLayer_ssp126.dat") %>% 
    dplyr::mutate(speciesID = str_split(string = name, pattern = "_", simplify = TRUE)[,1]) %>%
    dplyr::select(speciesID)
  mp_species_main <- dplyr::left_join(x = mp, y = aqm,  by = "speciesID") %>% 
    distinct(.keep_all = TRUE) %>% 
    group_by(phylum) %>% 
    summarise(mesopelagic = n())
# BathyAbyssopelagic
  bap <- fread("Prioritisation/PrioritizrFiles/features_10100/04_BathyAbyssopelagicLayer_ssp126/spec_04_BathyAbyssopelagicLayer_ssp126.dat") %>% 
    dplyr::mutate(speciesID = str_split(string = name, pattern = "_", simplify = TRUE)[,1]) %>%
    dplyr::select(speciesID)
  bap_species_main <- dplyr::left_join(x = bap, y = aqm,  by = "speciesID") %>% 
    distinct(.keep_all = TRUE) %>% 
    group_by(phylum) %>% 
    summarise(bathyabyssopelagic = n())
# Seafloor
  sflr <- fread("Prioritisation/PrioritizrFiles/features_10100/05_Seafloor_ssp126/spec_05_Seafloor_ssp126.dat") %>% 
    dplyr::mutate(speciesID = str_split(string = name, pattern = "_", simplify = TRUE)[,1]) %>%
    dplyr::filter(str_detect(string = speciesID, 
                             pattern = paste0(c("Basins", "Bridges", "Canyons", "Escarpments", 
                                                "Fans", "Guyots", "Plateaus", "Ridges", 
                                                "Seamounts", "Sills", "Trenches", "Troughs"), 
                                              collapse = "|")) == FALSE) %>% 
    dplyr::select(speciesID)
  sflr_species_main <- dplyr::left_join(x = sflr, y = aqm,  by = "speciesID") %>% 
    distinct(.keep_all = TRUE) %>% 
    group_by(phylum) %>% 
    summarise(seafloor = n())
  
# Creating the final Extended Table
  df_list <- list(ep_species_main, mp_species_main, bap_species_main, sflr_species_main)
  data <- reshape::merge_recurse(df_list) %>% 
    arrange(phylum)
  write.csv(data, "SummStats/zrichness_phyla.csv")
  
  # data <- read_csv("SummStats/zrichness_phyla.csv")
  # sum(data$epipelagic, na.rm = T)
  # sum(data$mesopelagic, na.rm = T)
  # sum(data$bathyabyssopelagic, na.rm = T)
  # sum(data$seafloor, na.rm = T)
  
  