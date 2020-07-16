# This code was written by Isaac Brito-Morales (i.britomorales@uq.edu.au)
# Please do not distribute this code without permission.
# NO GUARANTEES THAT CODE IS CORRECT
# Caveat Emptor!

pzr_function <- function(path, outdir, cost, blm, sol) {
  
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
    ncores <- 10
    cl <- makeCluster(ncores)
    registerDoParallel(cl)
    problem_list <- list()
    problems <-  foreach(kk = 1:length(dir.layers), .packages = c("prioritizr", "gurobi", "dplyr", "reldist")) %dopar% {
      
      # Identifying files per directories
        file_pu <- paste(dir.layers[kk], list.files(path = paste(dir.layers[kk], sep = "/"), pattern = "*pu_.*.dat$"), sep = "/")
        file_features <- paste(dir.layers[kk], list.files(path = paste(dir.layers[kk], sep = "/"), pattern = "*spec_.*.dat$"), sep = "/")
        file_bound <- paste(dir.layers[kk], list.files(path = paste(dir.layers[kk], sep = "/"), pattern = "*bound_.*.dat$"), sep = "/")
        file_rij <- paste(dir.layers[kk], list.files(path = paste(dir.layers[kk], sep = "/"), pattern = "*puvsp_0.*.dat$"), sep = "/")
      
        # Reading files per directories
          pu <- read.table(file_pu, sep = ",", header = TRUE) %>% 
            mutate(cost = ifelse(is.na(cost), 0, cost)) %>% 
            mutate(cost = round(cost)) %>% 
            mutate(status = ifelse(status == 3, 0, status))
            pu$cost <- ifelse(pu$cost == 0, median(filter(pu, pu$cost != 0)$cost), pu$cost)
            if(cost == "calibration") {pu$cost <- 0}
          features <- read.table(file_features, sep = ",", header = TRUE)
          bound <- read.table(file_bound, sep = ",", header = TRUE)
          rij <- read.table(file_rij, sep = ",", header = TRUE)
      
      # Establish the Problem
        mp1 <- marxan_problem(x = pu, spec = features, puvspr = rij, bound = bound, blm = blm) %>%
          add_gap_portfolio(number_solutions = sol, pool_gap = 0.2)
        # Solve the problem
          mp3_solution <- mp1 %>% 
            add_gurobi_solver(gap = 0, presolve = 2, time_limit = 3600, threads = 14, first_feasible = FALSE) %>% 
            solve()
        # Write the object
          ns <- basename(dir.layers[kk])
          write.csv(mp3_solution, paste(outdir, paste(ns, blm, sol, sep = "_"), ".csv", sep = ""), row.names = FALSE)
    }
    stopCluster(cl)
}


system.time(pzr_function(path = "/QRISdata/Q1216/BritoMorales/Project04b/output_prioritizr_blm-cal", 
                         outdir = "/QRISdata/Q1216/BritoMorales/Project04b/output_prioritizr_blm-cal/",
                         cost = "general",
                         blm = 0.002294, 
                         sol = 10))
