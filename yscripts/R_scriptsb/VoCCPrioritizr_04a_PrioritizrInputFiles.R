# This code was written by Isaac Brito-Morales (i.britomorales@uq.edu.au)
# Please do not distribute this code without permission.
# NO GUARANTEES THAT CODE IS CORRECT
# Caveat Emptor!

# AIM: 
# marxan_input: directory of the outcome from marxan_inputs.R script
# pu_shpfile:
# outdir: 
# cost_file: climate velocity raster (or any raster?)
# cost_type

# pu: information planning units id, cost and status (available locked in and locked out). status can be associated with a MPAs or a status??? put that in the argument function
# spec: id (species ID); prop (how proportion for this species); spf (species penalty factor. missing target..).[this file is not associated with planning units]
# puvsp.dat: species information by planning units
# puvsp_sporder.dat: same as puvsp.dat by ordered by species
# bound.dat: boundary shared by planning units

marxan_dat_files <- function(path, outdir, proj.geo) {

### Libraries to call
    library(raster)
    library(sf)
    library(dplyr)
    library(prioritizr)
    library(lwgeom)
    library(stringr)
    library(data.table)
    library(exactextractr)
    library(doParallel)
    library(foreach)
  
### Define all the interation directories 
    dir.scenarios <- paste(list.dirs(path = path, full.names = TRUE, recursive = FALSE), sep = "/") # Climate Models Directory
### Begin the parallel structure      
    cores  <-  5
    cl <- makeCluster(cores)
    registerDoParallel(cl)
    foreach(i = 1:length(dir.scenarios), .packages = c("raster", "sf", "dplyr", "prioritizr", "lwgeom", "stringr", "data.table", "exactextractr")) %dopar% {
      ### Files location [shapefile, sps info by province, targets by sps, mpas locked-in, vmes locked-in]
          pu_shpfile <- list.files(path = dir.scenarios[i], pattern = "*.rds$", full.names = TRUE)
          marxan_input_csv <- list.files(path = dir.scenarios[i], pattern = "*_provinces.*.csv$", full.names = TRUE)
          targets_csv <- list.files(path = dir.scenarios[i], pattern = "*_targets.*.csv$", full.names = TRUE)
          mpas_csv <- list.files(path = dir.scenarios[i], pattern = "*_mpas.*.csv$", full.names = TRUE)
          vmes_csv <- list.files(path = dir.scenarios[i], pattern = "*_VMEs.*.csv$", full.names = TRUE)     
          cost_file <- list.files(path = dir.scenarios[i], pattern = "*CostSFTotal*.rds$", full.names = TRUE)
      ### Reading planning unit shapefile AND species by provinces for every prioritization scenario 
          shp_file <- readRDS(pu_shpfile) %>% 
            st_transform(crs = CRS(proj.geo))
          shp_csv <- fread(marxan_input_csv)
          
      ### bound.dat FILE
          length_mtx <- prioritizr::boundary_matrix(shp_file) # a spare matrix ::: deleting the TRUE argument due crashed 
          length_data <- as(length_mtx, "dgTMatrix")
          length_data <- data.frame(id1 = length_data@i + 1, id2 = length_data@j + 1, boundary = length_data@x)
            # keep same name of original pus by converting into factor
              length_data$id2 <- as.factor(length_data$id2)
              length_data$id1 <- as.factor(length_data$id1)
              # replace those name with original shapefile pu layer
                levels(length_data$id2) <- shp_file$pu
                levels(length_data$id1) <- shp_file$pu
              # Writing the object
                bound_name <- paste("bound", basename(dir.scenarios[i]), sep = "_")
                write.table(length_data, file = paste(outdir, bound_name, ".dat", sep = ""), row.names = FALSE, sep = ",", quote = FALSE)
                
      ### puvsp FILE (species[a different number that the species' code], pu[planning unit], amount[area])
          shp_df <- shp_csv %>% 
            dplyr::select(pu, area_km2, feature_names_prov) %>% 
            base::transform(id = as.numeric(factor(feature_names_prov)))
                
          puvsp <- shp_df %>% 
            dplyr::select(id, pu, area_km2) %>% 
            dplyr::rename(species = id, amount = area_km2) %>%
            arrange(pu)
          # Write the file puvsp
            puvsp_name <- paste("puvsp", basename(dir.scenarios[i]), sep = "_")
            write.table(puvsp, file = paste(outdir, puvsp_name, ".dat", sep = ""), row.names = FALSE, sep = ",", quote = FALSE)
            # Write the file puvsp_sporder (orderer by species column)
              puvsp_order <- puvsp %>% arrange(species)
              puvsp_name_order <- paste("puvsp_sporder", basename(dir.scenarios[i]), sep = "_")
              write.table(puvsp_order, file = paste(outdir, puvsp_name_order, ".dat", sep = ""), row.names = FALSE, sep = ",", quote = FALSE)
              
      ### spec FILE (id[species ID], prop[proportion for protection of these species... 0.3 in general], spf[species penalty factor], name[species' name/code])  
          spec <- shp_df %>% 
            dplyr::group_by(id, feature_names_prov) %>% 
            summarize(total_area = sum(area_km2)) %>% 
            dplyr::select(id, feature_names_prov) %>% 
            dplyr::rename(id = id, name = feature_names_prov) %>% 
            mutate(prop = 0.2, spf = 1.1) %>% 
            dplyr::select(id, prop, spf, name) %>% 
            data.frame()
          # Reading target files
            df_targets <- fread(targets_csv) %>% 
              dplyr::rename(prop = targets, 
                            name = feature_names_prov)
            spec <- left_join(spec, df_targets, "name") %>% 
              dplyr::select(id, prop.y, spf, name) %>% 
              dplyr::rename(prop = prop.y)
	          # spec <- spec %>% 
	          #   mutate(spf = ifelse(str_detect(string = name, pattern = paste0(c(paste0("RCE"), paste0("VoCC")), collapse = "|")), 1.2, round((prop)/(max(0.30)), digits = 2)))
            # Write the file
              spec_name <- paste("spec", basename(dir.scenarios[i]), sep = "_")
              write.table(spec, file = paste(outdir, spec_name, ".dat", sep = ""), row.names = FALSE, sep = ",", quote = FALSE)
              
      ### pu FILE (id[planning units id], cost[velocity], status[available locked in and locked out])
          col_shp <- colnames(shp_file)
          col_shp[1] <- ifelse(col_shp[1] == "pu", "id", col_shp[1])
          colnames(shp_file) <- col_shp
          pu_file_df <- shp_file %>%
            mutate(cost = abs(cost), status = 0) %>% 
            data.frame() %>% 
            dplyr::select(id, cost, status)
          # Status: available = 0; locked in = 2; locked out = 3)
            # MPAs
              pu_mpas <- read.csv(mpas_csv) %>% 
                filter(province != "non-categ_mpas") %>% 
                mutate(status = 2) # 2 locked in
            # VMEs
              pu_vmes <- read.csv(vmes_csv) %>% 
                filter(province != "non-categ_VMEs") %>% 
                mutate(status = 2) # 2 locked in
            # Datas to locked-in/locked-out
              pus_lock <- rbind(pu_mpas, pu_vmes)
              pu_file_df <- pu_file_df %>% 
                mutate(status =  pus_lock$status[match(pu_file_df$id, pus_lock$layer)]) %>% 
                mutate(status = ifelse(is.na(status), 0, status)) %>% 
                mutate(cost = ifelse(is.na(cost), 0, cost)) %>% 
                mutate(status = ifelse(cost == 0, 3, status)) # if cost ZERO == 3 locked out
              # Write the pu.dat FILE
                pu_name <- paste("pu", basename(dir.scenarios[i]), sep = "_")
                write.table(pu_file_df, file = paste(outdir, pu_name, ".dat", sep = ""), row.names = FALSE, sep = ",", quote = FALSE)
    }
    stopCluster(cl)
}


system.time(marxan_dat_files(path = "/scratch/user/uqibrito/Project04c/Prioritisation/PrioritizrCleanBegin/features_10100",
                             outdir = "/scratch/user/uqibrito/Project04c/Prioritisation/PrioritizrFiles/features_10100/",
                             proj.geo = "+proj=moll +lon_0=0 +datum=WGS84 +units=m +no_defs"))

# system.time(marxan_dat_files(path = "Prioritisation/PrioritizrCleanBegin/features_10100",
#                              outdir = "Prioritisation/PrioritizrFiles/",
#                              proj.geo = "+proj=moll +lon_0=0 +datum=WGS84 +units=m +no_defs"))



