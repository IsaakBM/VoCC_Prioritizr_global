# This code was written by Isaac Brito-Morales (i.britomorales@uq.edu.au)
# Please do not distribute this code without permission.
# NO GUARANTEES THAT CODE IS CORRECT
# Caveat Emptor!

# AIM: 
# path: 


posthoc_marxan <- function(path, outdir, geo.proj) { 
  
  # List of pacakges that we will use
    list.of.packages <- c("raster", "sf", "dplyr", "tidyr", "readr", "lwgeom", "doParallel", "parallel", "stringr", "magrittr")
    # If is not installed, install the pacakge
      new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])] # is the package in MY list of packages?
      if(length(new.packages)) install.packages(new.packages) # if not -> install it
  # Load packages
    lapply(list.of.packages, require, character.only = TRUE)
  
  # Define all the interation directories 
    dir.scenarios <- list.dirs(paste(list.dirs(path = path, full.names = TRUE, recursive = FALSE)), full.names = TRUE, recursive = FALSE)

  # Loop through every Iteration-scenario
    # To allocate iterations dataframes
      scenario_list <- list()
      for(i in 1:length(dir.scenarios)) {
        # Define directories per scenario
          folders <- list.dirs(dir.scenarios[i], full.names = TRUE, recursive = FALSE)
        # Files location
          out_files <- list.files(path = folders[2], pattern = "*_r.*.csv$", full.names = TRUE)
          out_log <- list.files(path = folders[2], pattern = "*_log.*.dat$", full.names = TRUE)
          shp_files <- list.files(path = folders[3], pattern = "*.shp$", full.names = TRUE)
        # Read shapefile just one time
          dt_shp <- st_read(shp_files) %>% st_transform(crs = geo.proj)
          var.names <- colnames(dt_shp)
          dt_shp <- dt_shp %>% 
            magrittr::set_colnames(ifelse(str_detect(var.names, "(?i).*id*"), "id", 
                                          ifelse(str_detect(var.names, "(?i)cost"), "cost", var.names))) %>% 
            dplyr::select(id, cost, geometry) %>% arrange(id)

        # Begin the parallel structure
          UseCores <- detectCores() -1
          cl <- makeCluster(UseCores)  
          registerDoParallel(cl)
          ls_geom <- list() # empty list to allocate results
          # A parallel structure to search the different solutions per iterations and calculates boundaries and perimeters 
            geom_list <- foreach(j = 1:length(out_files), .packages = c("sf", "raster", "dplyr", "tidyr", "readr", "lwgeom", "stringr", "magrittr")) %dopar% {
              # Read outfiles
                dt <- read.table(out_files[j], sep = ",", header = TRUE) # read every element from the out_files object
                dt <- dt[(dt[,2] == 1),] %>% rename(id = names(dt[1]), solution = names(dt[2]))
                # Marxan's solution names
                  name <- unlist(lapply(basename(out_files[j]), FUN = function(x) strsplit(x, "_")))[2]
                  name <- sub(pattern = "*.csv", "", name)
                # Best solution find 
                  dt_outlog <- readLines(out_log)
                  name_outlog <- dt_outlog[grep("Best solution*", x = dt_outlog)]
                  name_outlog <- unlist(strsplit(name_outlog, " "))[5]
                # Extract from the shapefile the object dt
                  dt2 <- dt_shp[dt_shp$id %in% dt$id,]
                  dt3 <- dt2 %>% dplyr::summarise(total_cost = sum(cost, do_union = TRUE), median_cost = median(cost, do_union = TRUE)) # testing...
                  dt_final <- dt3 %>% mutate(area = st_area(dt3), 
                                             perimeter = st_perimeter(dt3), 
                                             solution = name,
                                             iteration = readr::parse_number(unlist(str_extract_all(folders[1], "(?i)iteration*\\d+"))),
                                             scenario = readr::parse_number(unlist(str_extract_all(folders[1], "(?i)scenario*\\d+"))),
                                             best_solution = ifelse(as.character(readr::parse_number(name)) == name_outlog, "YES", "NO"))
                # Fragmentation process
                  dt_final <- dt_final %>% mutate(per_circle = (sqrt(area/pi))*2*pi)
                  dt_final <- dt_final %>% mutate(fragmentation = perimeter/per_circle)
              
                ls_geom[[j]] <- dt_final # to allocate results
                } 
            stopCluster(cl)
        
    # Combine to a dataframe per iteration-scenario
      geom_list <- do.call(rbind, geom_list)
      # Cleane the object
        geom_list <- geom_list %>% data.frame %>% dplyr::select(-geometry) %>% 
          mutate(area = as.numeric(area), 
                 perimeter = as.numeric(perimeter),
                 per_circle = as.numeric(per_circle),
                 fragmentation = as.numeric(fragmentation)) %>% 
          rename(area_m2 = area,
                 perimeter_m = perimeter,
                 per_circle_m = per_circle,
                 fragmentation_m = fragmentation)
    # Final list object
      scenario_list[[i]] <- geom_list
      }
  # Final dataframe for all iterations-scenarios in directory path
    iteration_df <- do.call(rbind, scenario_list)
    name.df <- paste("PostHoc_Calibration")
    write.csv(iteration_df, paste(outdir, name.df, ".csv", sep = ""), row.names = FALSE)  
  
}

  system.time(posthoc_marxan(path = "BLM-Velocity",
                             outdir = "BLM-Velocity/",
                             geo.proj = "+proj=aea +lat_1=60 +lat_2=60 +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"))


# for PNG
# "+proj=cea +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

# for med
# "+proj=aea +lat_1=60 +lat_2=60 +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
  
