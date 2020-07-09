# This code was written by Isaac Brito-Morales (i.britomorales@uq.edu.au)
# Please do not distribute this code without permission.
# NO GUARANTEES THAT CODE IS CORRECT
# Caveat Emptor!

# AIM: Parallelize function that makes spatial interpolation by neasrest distance method
# path = 
# outdir = 
# type = a character: "trajectory" or "general"

near.dist <- function(path, outdir, type) { # add type of analysis interpolation: velocity or trajectories? 
  
  # # List of pacakges that we will use
  #   list.of.packages <- c("raster", "future.apply", "dplyr", "foreach", "doParallel")
  #   # Load packages
  #     lapply(list.of.packages, library, character.only = TRUE)
  #     # If is not installed, install the package
  #       new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])] # is the package in MY list of packages
  #       if(length(new.packages)) install.packages(new.packages) # if not, install it
  
  # Packages
    library(raster)
    library(future.apply)
    library(dplyr)
    library(foreach)
    library(doParallel)
  
  # Folder's structure
    dir.scenarios <- paste(list.dirs(path = path, full.names = TRUE, recursive = FALSE), sep = "/") # Climate Models Directory
    dir.olayers <- paste(list.dirs(path = dir.scenarios, full.names = TRUE, recursive = FALSE), sep = "/") # Climate Models Directory per ocean layer
    
    # dir.olayers <- paste(list.dirs(path = path, full.names = TRUE, recursive = FALSE), sep = "/") # Climate Models Directory
    
  # Do an interpolation parallel analysis for every directory in your path climatic folder
    for(i in 1:length(dir.olayers)) { # what about doing a parallel here? nested parallel?
      if (type == "trajectory") { # if the path are trajectory classes .tif files do this
        single.dir  <- paste(dir.olayers[i], list.files(path = paste(dir.olayers[i], sep = "/"), pattern = ".tif"), sep = "/")
        rs_list <- lapply(single.dir, function(x) {files <- readAll(stack(x)) %>% subset(1)})
          rs <- do.call(merge, rs_list)
        # A parallel structure to replace empty cells for the neasrest cell value
          xy <- data.frame(xyFromCell(rs, 1:ncell(rs))) # get the coordinates
          # declare the arguments of your session
            cores  <-  detectCores()
            ncores <- 23 # set 23 for cluster
            plan(multisession, workers = ncores)
            sampled2 <- future_apply(X = xy, MARGIN = 1, FUN = function(xy) 
              rs@data@values[which.min(replace(distanceFromPoints(rs, xy), is.na(rs), NA))]) # make the replacement here
            # some data maniputaltion here
              b <- lapply(sampled2, mean) %>% unlist() %>% data.frame() # usually mean but check this argument in any case
              c <- b %>% mutate(x = xy[,1], y = xy[,2], variable = trunc(b[,1])) %>% select(-.)
              rs_inter <- rasterFromXYZ(c)
              crs(rs_inter) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
        # Define some arguments for trajectories and to generate individual interpolated trajectory raster files
          traj_names <- c("non-moving", 
                          "slow-moving", 
                          "internal-sinks", 
                          "boundary-sinks", 
                          "sources", 
                          "relative-sinks", 
                          "corridors",
                          "divergence",
                          "convergence")
          rs_classes <- sort(unique(rs_inter[])) # how many unique trajectories we have
          rs_trajnames <- traj_names[rs_classes] # get the names
          
          # Isolate traj classes and then write individual files
            traj_list <- list()
            for(j in 1:length(rs_classes)) {
              single <- rs_inter
              single[] <- ifelse(single[] == rs_classes[j], single[], NA)
              traj_list[[j]] <- single
              }
              names(traj_list) <- rs_trajnames
              # List of files's names  
                names.rs <- lapply(single.dir, function(x) {files <- basename(x)})
                for(k in 1:length(names.rs)) {
                  ns <- basename(names.rs[[k]])
                    olayer <- unlist(strsplit(x = ns, split = "_"))[2]
                    model <- unlist(strsplit(x = ns, split = "_"))[3]
                    ssp <- unlist(strsplit(x = ns, split = "_"))[4]
                    period <- unlist(strsplit(x = ns, split = "_"))[6]
                  name.traj <- paste(names(traj_list[k]), olayer, model, ssp, period, sep = "_")
                  # Write the rasters
                    writeRaster(traj_list[[k]], paste(outdir, name.traj, ".tif", sep = ""), overwrite = TRUE)
                    }
                } else {
                  
                  single.dir  <- paste(dir.olayers[i], list.files(path = paste(dir.olayers[i], sep = "/"), pattern = ".tif"), sep = "/") # what about more models?
                  rs <- readAll(stack(single.dir)) %>% subset(1)
                  # A parallel structure to replace empty cells for the neasrest cell value
                    xy <- data.frame(xyFromCell(rs, 1:ncell(rs))) # get the coordinates
                    # declare the arguments of your session
                      cores  <-  detectCores()
                      ncores <- 23 # set 23 for cluster
                      plan(multisession, workers = ncores)
                      sampled2 <- future_apply(X = xy, MARGIN = 1, FUN = function(xy) 
                        rs@data@values[which.min(replace(distanceFromPoints(rs, xy), is.na(rs), NA))]) # make the replacement here
                      # some data maniputaltion here
                        b <- lapply(sampled2, mean) %>% unlist() %>% data.frame() # usually mean but check this argument in any case
                        c <- b %>% mutate(x = xy[,1], y = xy[,2], variable = b[,1]) %>% select(-.)
                        rs_inter <- rasterFromXYZ(c)
                        crs(rs_inter) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
                        
                  # Write raster file
                    ns <- basename(single.dir) # get the names to write the rasters
                      var <- unlist(strsplit(x = ns, split = "_"))[1]
                      olayer <- unlist(strsplit(x = ns, split = "_"))[2]
                      model <- unlist(strsplit(x = ns, split = "_"))[3]
                      ssp <- unlist(strsplit(x = ns, split = "_"))[4]
                      period <- unlist(strsplit(x = ns, split = "_"))[6]
                    name.rs <- paste(var, olayer, model, ssp, period, sep = "_")
                    # Write the rasters
                      writeRaster(rs_inter, paste(outdir, name.rs, ".tif", sep = ""), overwrite = TRUE)
        }
    }
}

  system.time(near.dist(path = "/QRISdata/Q1216/BritoMorales/Project04b/vocc_a_nointer/mag", 
                        outdir = "/QRISdata/Q1216/BritoMorales/Project04b/vocc_b_inter/mag/ssp126/", 
                        type = "general"))
  
