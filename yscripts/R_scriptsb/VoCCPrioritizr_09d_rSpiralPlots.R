# This code was written by Isaac Brito-Morales (i.britomorales@uq.edu.au)
# Please do not distribute this code without permission.
# NO GUARANTEES THAT CODE IS CORRECT
# Caveat Emptor!

source("yscripts/R_scriptsb/VoCCPrioritizr_08d_fSpiralPlots.R")

####################################################################################
####### 1.- Spiral Low-Regret per Layer
####################################################################################
# 
  ep <- spiralDF(aqm = "Inputs/Aqm/speciesoccursum.csv", 
                 features = "Output/FeaturesOLayer/epipelagic.csv", 
                 solution = "SummStats/PrioritizrSolutionsCost/features_10100/NoRegret_Epipelagic_ssp126ssp245ssp585.rds")
  mp <- spiralDF(aqm = "Inputs/Aqm/speciesoccursum.csv", 
                 features = "Output/FeaturesOLayer/mesopelagic.csv", 
                 solution = "SummStats/PrioritizrSolutionsCost/features_10100/NoRegret_Mesopelagic_ssp126ssp245ssp585.rds")
  bap <- spiralDF(aqm = "Inputs/Aqm/speciesoccursum.csv", 
                 features = "Output/FeaturesOLayer/BathyAbyssopelagic.csv", 
                 solution = "SummStats/PrioritizrSolutionsCost/features_10100/NoRegret_BathyAbyssopelagic_ssp126ssp245ssp585.rds")
  sflr <- spiralDF(aqm = "Inputs/Aqm/speciesoccursum.csv", 
                  features = "Output/FeaturesOLayer/seafloor_spsgf.csv", 
                  solution = "SummStats/PrioritizrSolutionsCost/features_10100/NoRegret_Seafloor_ssp126ssp245ssp585.rds")
  no_regret <- PlotSpiral(rbind(ep, mp, bap, sflr))
  
####################################################################################
####### 2.- Spiral Low-Regret Vertical Layer
####################################################################################
# 
  plg <- spiralDF_V(aqm = "Inputs/Aqm/speciesoccursum.csv", 
                    features = "Output/FeaturesOLayer/zPelagicSeafloor.csv", 
                    solution = "SummStats/PrioritizrSolutionsCost/features_10100/NoRegret_Pelagic.rds", 
                    pu_plg)
  plg_slfr <- spiralDF_V(aqm = "Inputs/Aqm/speciesoccursum.csv", 
                         features = "Output/FeaturesOLayer/zPelagicSeafloor.csv", 
                         solution = "SummStats/PrioritizrSolutionsCost/features_10100/NoRegret_PelagicSeafloor.rds", 
                         pu_all)
  no_regret_V <- PlotSpiral_V(rbind(plg, plg_slfr))

####################################################################################
####### 2.- Spiral Low-Regret Vertical Layer
####################################################################################
  
  