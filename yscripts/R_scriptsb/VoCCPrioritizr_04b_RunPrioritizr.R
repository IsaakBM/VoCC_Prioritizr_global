# This code was written by Isaac Brito-Morales (i.britomorales@uq.edu.au)
# Please do not distribute this code without permission.
# NO GUARANTEES THAT CODE IS CORRECT
# Caveat Emptor!

pzr_function <- function(path, outdir, cost, blm_df, sol) {
  
  library(raster)
  library(sf)
  library(data.table)
  library(dplyr)
  library(prioritizr)
  library(gurobi)
  library(spatstat)
  library(reldist)
  library(doParallel)
  library(foreach)
  
  # Reading features raster files (AquaMaps | Trajectory classes)
    dir.layers <- paste(list.dirs(path = path, full.names = TRUE, recursive = FALSE), sep = "/")
    
  # Begin the parallel structure      
    ncores <- 3
    cl <- makeCluster(ncores)
    registerDoParallel(cl)
    problem_list <- list()
    problems <-  foreach(kk = 1:length(dir.layers), .packages = c("prioritizr", "gurobi", "dplyr", "reldist")) %dopar% {
      
      # Identifying files per directories
        file_pu <- paste(dir.layers[1], list.files(path = paste(dir.layers[1], sep = "/"), pattern = "*pu_.*.dat$"), sep = "/")
        file_features <- paste(dir.layers[1], list.files(path = paste(dir.layers[1], sep = "/"), pattern = "*spec_.*.dat$"), sep = "/")
        file_bound <- paste(dir.layers[1], list.files(path = paste(dir.layers[1], sep = "/"), pattern = "*bound.*._.*.dat$"), sep = "/")
        file_rij <- paste(dir.layers[1], list.files(path = paste(dir.layers[1], sep = "/"), pattern = "*puvsp_0.*.dat$"), sep = "/")
        # Reading files per directories
          pu <- read.table(file_pu, sep = ",", header = TRUE) %>% 
            mutate(cost = ifelse(is.na(cost), 0, cost)) %>%
            mutate(status = ifelse(status == 3, 0, status))
          if(cost == "calibration") {pu$cost <- 0}
          if(cost == "area") {pu$cost <- 2619726846}
          features <- read.table(file_features, sep = ",", header = TRUE)
          bound <- read.table(file_bound, sep = ",", header = TRUE)
          rij <- read.table(file_rij, sep = ",", header = TRUE)
        # Reading BLM file
          blm_df <- read.csv(blm_df)
      # Establish the Problem
        mp1 <- marxan_problem(x = pu, spec = features, puvspr = rij, bound = bound, blm = blm_df[1, 2]) %>%
          add_gap_portfolio(number_solutions = sol, pool_gap = 0)
        # Solve the problem
          mp3_solution <- mp1 %>% 
            add_gurobi_solver(gap = 0, presolve = 2, time_limit = 10800, threads = 3, first_feasible = FALSE) %>% 
            solve(force = TRUE)
        # Write the object
          mp3_final  <- list(mp1, mp3_solution)
          ns <- basename(dir.layers[kk])
          saveRDS(mp3_final, paste(outdir, paste(ns, blm_df[kk, 2], sol, sep = "_"), ".rds", sep = ""))
    }
    stopCluster(cl)
}

# system.time(pzr_function(path = "/QRISdata/Q1216/BritoMorales/Project04b/tdatfiles_lowCC_03_sametargets/tdatfiles_10100rce-vocc10100_targets-mix_noduplicates_iucn", 
#                          outdir = "/QRISdata/Q1216/BritoMorales/Project04b/ublm_lowCC_03_sametargets/ublm-cal_10100rce-vocc10100_targets-mix_rawcost_noduplicates_iucn/",
#                          cost = "general",
#                          blm_df = "/QRISdata/Q1216/BritoMorales/Project04b/ublm_lowCC_03_sametargets/ublm-cal_10100rce-vocc10100_targets-mix_rawcost_noduplicates_iucn/BLM_0.csv", 
#                          sol = 1))

system.time(pzr_function(path = "Prioritisation/PrioritizrFiles/features_10100", 
                         outdir = "Prioritisation/PrioritizrSolutions/",
                         cost = "general",
                         blm_df = "Prioritisation/PrioritizrSolutions/BLM_0.csv", 
                         sol = 1))

