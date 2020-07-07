# This code was written by Isaac Brito-Morales (i.britomorales@uq.edu.au)
# Please do not distribute this code without permission.
# NO GUARANTEES THAT CODE IS CORRECT
# Caveat Emptor!

pzr_function <- function(path, outdir, n_blm, sol) {
  
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
    
    for(kk in 1:length(dir.layers)) {
      # Identifying files per directories
        file_pu <- paste(dir.layers[kk], list.files(path = paste(dir.layers[kk], sep = "/"), pattern = "pu.dat$"), sep = "/")
        file_features <- paste(dir.layers[kk], list.files(path = paste(dir.layers[kk], sep = "/"), pattern = "spec.dat$"), sep = "/")
        file_bound <- paste(dir.layers[kk], list.files(path = paste(dir.layers[kk], sep = "/"), pattern = "*bound.*.dat$"), sep = "/")
        file_rij <- paste(dir.layers[kk], list.files(path = paste(dir.layers[kk], sep = "/"), pattern = "puvsp.dat$"), sep = "/")
        # Reading files per directories
          pu <- read.table(file_pu, sep = ",", header = TRUE) %>% 
            mutate(cost = ifelse(is.na(cost), 0, cost)) %>% 
            mutate(cost = round(cost, digits = 2))
          features <- read.table(file_features, sep = ",", header = TRUE)
          bound <- read.table(file_bound, sep = ",", header = TRUE)
          rij <- read.table(file_rij, sep = ",", header = TRUE)
        # Begin the parallel structure      
          ncores <- 10
          cl <- makeCluster(ncores)
          registerDoParallel(cl)
          problem_list <- list()
        # Calibration BLM
          blm_cal <- round(seq(0, 1, length.out = n_blm), digits = 4)
          problems <- foreach(i  = 1:length(blm_cal), .packages = c("prioritizr", "gurobi", "dplyr", "reldist")) %dopar% {
            # Establish the Problem      
              mp1 <- marxan_problem(x = pu, spec = features, puvspr = rij, bound = bound, blm = blm_cal[i]) %>%
                add_gap_portfolio(number_solutions = sol, pool_gap = 0.1)
            # Solve the problem
              mp3_solution <- prioritizr::solve(mp1)    
            # Write the object
              ns <- basename(dir.layers[kk])
              write.csv(mp3_solution, paste(outdir, paste(ns, blm_cal[i], sep = "_"), ".csv", sep = ""), row.names = FALSE)
          }
          stopCluster(cl)
    }
}


system.time(pzr_function(path = "/QRISdata/Q1216/BritoMorales/Project04b/output_datfiles",
                         outdir = "/QRISdata/Q1216/BritoMorales/Project04b/output_datfiles/",
                         n_blm = 10,
                         sol = 100))

# system.time(pzr_function(path = "output_datfiles", 
#                          outdir = "output_datfiles/", 
#                          n_blm = 3, 
#                          sol = 2))
