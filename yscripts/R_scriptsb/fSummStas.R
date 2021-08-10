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



