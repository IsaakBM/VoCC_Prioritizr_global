# This code was written by Isaac Brito-Morales (i.britomorales@uq.edu.au)
# Please do not distribute this code without permission.
# NO GUARANTEES THAT CODE IS CORRECT
# Caveat Emptor!

# AIM: 
# path: 


posthoc_marxan <- function(path, outdir, pu_shpfile, proj.geo) { 
  
  # List of pacakges that we will use
    list.of.packages <- c("raster", "sf", "dplyr", "readr", "lwgeom", "doParallel", "parallel", "stringr", "magrittr")
    # If is not installed, install the pacakge
      new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])] # is the package in MY list of packages?
      if(length(new.packages)) install.packages(new.packages) # if not -> install it
  # Load packages
    lapply(list.of.packages, require, character.only = TRUE)
  
  # Define all the directories 
    dir.scenarios <- paste(list.dirs(path = path, full.names = TRUE, recursive = FALSE), sep = "/")

  # Loop through every scenario-solutionBLM
    # To allocate iterations dataframes
      scenario_list <- list()
      for(i in 1:length(dir.scenarios)) {
        # Files location
          out_files <- list.files(path = dir.scenarios[i], pattern = "*.csv$", full.names = TRUE)
        # Read shapefile just one time
          dt_shp <- st_read(pu_shpfile) %>% 
            st_transform(crs = CRS(proj.geo))
          var.names <- colnames(dt_shp)
          dt_shp <- dt_shp %>% 
            magrittr::set_colnames(ifelse(str_detect(var.names, "(?i).*id*"), "id", 
                                          ifelse(str_detect(var.names, "(?i)cost"), "cost", var.names)))

        # Begin the parallel structure
          UseCores <- detectCores() -1
          cl <- makeCluster(UseCores)  
          registerDoParallel(cl)
          ls_geom <- list() # empty list to allocate results
          # A parallel structure to search the different solutions per iterations and calculates boundaries and perimeters 
            geom_list <- foreach(j = 1:length(out_files), .packages = c("sf", "raster", "dplyr", "tidyr", "readr", "lwgeom", "stringr", "magrittr")) %dopar% {
              # Read outfiles
                dt <- read.csv(out_files[j], sep = ",", header = TRUE)
                # Solution's names
                  name <- basename(out_files[j])
                  name <- sub(pattern = "*.csv", "", name)
                # Split dataframe in solutions and ids/cost
                  dt_idcost <- dt[,1:2]
                  dt_solutions <- dt %>% 
                    dplyr::select(matches("solution"))
                # Extract from the shapefile the object dt
                  solutions_list <- list()
                  for(k in 1:ncol(dt_solutions)) {
                    single <- cbind(dt_idcost, dt_solutions[k])
                    dt1 <- single %>% 
                      filter(single[,3] == "1")
                    dt2 <- dt_shp[dt_shp$id %in% dt1$id,]
                    dt3 <- dt2 %>% 
                      dplyr::summarise(total_cost = sum(cost, do_union = TRUE), median_cost = median(cost, do_union = TRUE))
                    dt_final <- dt3 %>% mutate(area = st_area(dt3), 
                                               perimeter = st_perimeter(dt3), 
                                               solution = colnames(dt1)[3],
                                               scenario = name)
                    solutions_list[[k]] <- dt_final
                  }
                solutions_final <- do.call(rbind, solutions_list)  
                ls_geom[[j]] <- solutions_final # to allocate results
                } 
            stopCluster(cl)
        
    # Combine to a dataframe per iteration-scenario
      geom_list <- do.call(rbind, geom_list)
      # Cleane the object
        geom_list <- geom_list %>% 
          data.frame %>% 
          dplyr::select(-geometry) %>% 
          mutate(area = as.numeric(area), 
                 perimeter = as.numeric(perimeter)) %>% 
          rename(area_m2 = area,
                 perimeter_m = perimeter)
    # Final list object
      scenario_list[[i]] <- geom_list
      }
  # Final dataframe for all iterations-scenarios in directory path
    iteration_df <- do.call(rbind, scenario_list)
    name.df <- paste("PostHoc_Calibration")
    write.csv(iteration_df, paste(outdir, name.df, ".csv", sep = ""), row.names = FALSE)  
  
}

  system.time(posthoc_marxan(path = "output_prioritizr_blm-cal3",
                             outdir = "output_prioritizr_blm-cal3/",
                             pu_shpfile = "output_prioritizr_blm-cal3/02_EpipelagicLayer_BLM_cal/pu.shp",
                             proj.geo = "+proj=moll +lon_0=0 +datum=WGS84 +units=m +no_defs"))

