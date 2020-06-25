

pzr_function <- function(path, outdir, blm, sol) {
  
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
      
      #
        file_pu <- paste(dir.layers[kk], list.files(path = paste(dir.layers[kk], sep = "/"), pattern = "pu.dat$"), sep = "/")
        file_features <- paste(dir.layers[kk], list.files(path = paste(dir.layers[kk], sep = "/"), pattern = "spec.dat$"), sep = "/")
        file_bound <- paste(dir.layers[kk], list.files(path = paste(dir.layers[kk], sep = "/"), pattern = "*bound.*.dat$"), sep = "/")
        file_rij <- paste(dir.layers[kk], list.files(path = paste(dir.layers[kk], sep = "/"), pattern = "puvsp.dat$"), sep = "/")
        # 
          pu <- read.table(file_pu, sep = ",", header = TRUE)
          features <- read.table(file_features, sep = ",", header = TRUE)
          bound <- read.table(file_bound, sep = ",", header = TRUE)
          rij <- read.table(file_rij, sep = ",", header = TRUE)
      
      # How many interation per problem?
        interations <- seq(1, sol, length.out = sol)
        problem_list <- list()
      # Begin the parallel structure      
        ncores <- 3
        cl <- makeCluster(ncores)
        registerDoParallel(cl)
      # Loop
        problems <- foreach(i = 1:length(interations), .packages = c("prioritizr", "gurobi", "dplyr", "reldist")) %dopar% {
          
          # Establish the Problem
            mp1 <- marxan_problem(x = pu, spec = features, puvspr = rij, bound = bound, blm = blm) %>%
              add_pool_portfolio(method = 2, number_solutions = interations[i])
          # Solve the problem
            mp3_solution <- prioritizr::solve(mp1) # needs gurobi R package
            #  
              problem_list[[i]] <- mp3_solution
        }
        stopCluster(cl)
  
      # 
        test <- do.call(cbind, problems)
        # Write the object
          ns <- basename(dir.layers[kk])
          write.csv(test, paste(outdir, ns, ".csv", sep = ""), row.names = FALSE)
    }
}


system.time(pzr_function(path = "/Users/bri273/Desktop/VoCC_Marxan/output_datfiles", 
                         outdir = "/Users/bri273/Desktop/VoCC_Prioritizr_global/", 
                         blm = 0, 
                         sol = 2))
