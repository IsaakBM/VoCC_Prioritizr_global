# This code was written by Isaac Brito-Morales (i.britomorales@uq.edu.au)
# Please do not distribute this code without permission.
# NO GUARANTEES THAT CODE IS CORRECT
# Caveat Emptor!

# AIM: 
# path: 

summary_priotitizr <- function(path, outdir, proj.geo, spot) {
  
  library(raster)
  library(sf)
  library(dplyr)
  library(lwgeom)
  library(doParallel)
  library(parallel)
  library(foreach)
  library(stringr)
  library(magrittr)
  
  # Define all the directories 
    dir.scenarios <- paste(list.dirs(path = path, full.names = TRUE, recursive = FALSE), sep = "/")
  # Begin the parallel structure
    UseCores <- detectCores() -1 #24
    cl <- makeCluster(UseCores)  
    registerDoParallel(cl)
    sum_list <- list()
    # Loop
      summary_list <- foreach(j = 1:length(dir.scenarios), .packages = c("sf", "raster", "dplyr", "lwgeom", "stringr", "magrittr")) %dopar% { 
        # Files location
          out_files <- list.files(path = dir.scenarios[j], pattern = "*cost.*.csv$", full.names = TRUE)
          rce_csv <- list.files(path = dir.scenarios[j], pattern = "_RCE_.*.csv$", full.names = TRUE) # RCE file
          vocc_csv <- list.files(path = dir.scenarios[j], pattern = "voccMag_.*.csv$", full.names = TRUE) # VoCC file
          pu_shpfile <- list.files(path = dir.scenarios[j], pattern = "*.shp$", full.names = TRUE)
        # Read shapefile just one time
          dt_shp <- st_read(pu_shpfile) %>% 
            st_transform(crs = CRS(proj.geo))
          var.names <- colnames(dt_shp)
          dt_shp <- dt_shp %>% 
            magrittr::set_colnames(ifelse(str_detect(var.names, "(?i).*id*"), "id", 
                                          ifelse(str_detect(var.names, "(?i)cost"), "cost", var.names)))
        
        if(length(rce_csv) != 0 | length(vocc_csv) != 0) {
          output_list <- list() #vector("numeric", length = length(out_files))
          for(i in 1:length(out_files)) {
            # Read outfiles
              dt <- read.csv(out_files[i], sep = ",", header = TRUE)
              rce <- read.csv(rce_csv[i], sep = ",", header = TRUE)
              vocc <- read.csv(vocc_csv[i], sep = ",", header = TRUE)
            # Solution's names
              name <- basename(out_files[i])
              name <- sub(pattern = "*.csv", "", name)
              ns <- unlist(strsplit(x = name, split = "_"))
              ns_scenario <- paste(ns[1:(length(ns)-2)], collapse = "_")
              ns_blm <- unlist(strsplit(x = name, split = "_"))[length(ns)-1]
            # Split dataframe in solutions and ids/cost
              dt_idcost <- dt[,1:2]
              dt_idcost$vocc <- vocc$climate_feature[match(dt_idcost$id, vocc$pu)]
              dt_idcost$rce <- rce$climate_feature[match(dt_idcost$id, rce$pu)]
              dt_solutions <- dt %>% 
                dplyr::select(matches("solution"))
            # Extract from the shapefile the object dt
              solutions_list <- list()
              for(k in 1:ncol(dt_solutions)) {
                single <- cbind(dt_idcost, dt_solutions[k])
                dt1 <- single %>% 
                  filter(single[,5] == "1")
                dt2 <- dt1[dt1$id %in% dt_shp$id,]
                dt3 <- left_join(x = dt_shp, y = dt2, by = "id") %>% 
                  na.omit()
                dt4 <- dt3 %>% 
                  dplyr::summarise(total_cost = sum(cost.x, na.rm = TRUE, do_union = TRUE), 
                                   rce_median = median(rce, na.rm = TRUE, do_union = TRUE),
                                   rce_lower_qt = quantile(rce, prob = 0.25, na.rm = TRUE, do_union = TRUE),
                                   rce_upper_qt = quantile(rce, prob = 0.75, na.rm = TRUE, do_union = TRUE),
                                   vocc_median = median(vocc*10, na.rm = TRUE, do_union = TRUE), 
                                   vocc_lower_qt = quantile(vocc*10, prob = 0.25, na.rm = TRUE, do_union = TRUE),
                                   vocc_upper_qt = quantile(vocc*10, prob = 0.75, na.rm = TRUE, do_union = TRUE),)
                dt_final <- dt4 %>% mutate(area = st_area(dt4), 
                                           perimeter = st_perimeter(dt4), 
                                           solution = colnames(dt1)[5],
                                           scenario = ns_scenario,
                                           BLM = ns_blm)
                solutions_list[[k]] <- dt_final
            }
            solutions_final <- do.call(rbind, solutions_list)  
            output_list[[i]] <- solutions_final %>% data.frame
          }
        }
        sum_list[[j]] <- output_list %>% 
          data.frame %>% 
          dplyr::mutate(protection = area/sum(st_area(dt_shp)))
      }
    stopCluster(cl)
    # Combine to a dataframe per iteration-scenario
      geom_list <- do.call(rbind, summary_list)
    # Cleane the object
      geom_list <- geom_list %>% 
        data.frame %>% 
        dplyr::select(-geometry) %>% 
        dplyr::mutate(area = as.numeric(area), perimeter = as.numeric(perimeter)) %>% 
        dplyr::rename(area_m2 = area, perimeter_m = perimeter)
    # Final dataframe
      iteration_df<- geom_list %>% 
        dplyr::mutate(trade_off = ifelse(BLM == 0, "X", ifelse(BLM == 1, "Y", spot)))
      name.df <- paste("Summ-Stats", basename(path), sep = "_")
      write.csv(iteration_df, paste(outdir, name.df, ".csv", sep = ""), row.names = FALSE)  
}

system.time(summary_priotitizr(path = "vfinal-sol_figs_test/ublm-cal_0520rce-vocc040_targets-mix", 
                               outdir = "vfinal-sol_figs_test/ublm-cal_0520rce-vocc040_targets-mix/", 
                               proj.geo = "+proj=moll +lon_0=0 +datum=WGS84 +units=m +no_defs", 
                               spot = ""))
