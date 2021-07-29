# This code was written by Isaac Brito-Morales (i.britomorales@uq.edu.au)
# Please do not distribute this code without permission.
# NO GUARANTEES THAT CODE IS CORRECT
# Caveat Emptor!


iucn_targets <- function(path, aquamaps_data, iucn_data, iucn_noclim_target, iucn_clim_target) { 
  
  library(data.table)
  library(dplyr)
  library(doParallel)
  library(foreach)
  library(stringr)
  
  # List of directories
    dir.scenarios <- paste(list.dirs(path = path, full.names = TRUE, recursive = FALSE), sep = "/") # Climate Models Directory
    pattern1 <-  paste0("*targets*", ".*.csv$")
    
      # Begin the parallel structure      
      cores  <-  5
      cl <- makeCluster(cores)
      registerDoParallel(cl)
      foreach(j = 1:length(dir.scenarios), .packages = c("dplyr", "stringr", "data.table")) %dopar% {
        
        if(str_detect(string = dir.scenarios[j], pattern = "ssp") == FALSE) {
          # Read files
          csv_targets <- fread(list.files(path = dir.scenarios[j], pattern = pattern1, full.names = TRUE)) # all (species + clim + species&clim layers)
          csv_targets_noclim <- fread(list.files(path = dir.scenarios[j], pattern = pattern1, full.names = TRUE)) %>% 
            dplyr::filter(str_detect(string = feature_names_prov, pattern = "VoCC|RCE") == FALSE) # just species
          csv_iucn <- fread(iucn_data)
          
          # Getting species' code from targets' file
          if(nrow(csv_targets_noclim) == 0) {
            df1 <- strsplit(as.character(csv_targets$feature_names_prov), split = "_")
          } else {
            df1 <- strsplit(as.character(csv_targets_noclim$feature_names_prov), split = "_")
          }
          df2 <- lapply(df1, function(x) {x[1]})
          speciesID <- do.call(rbind, df2)
          # Read the AquaMaps data (species occur .csv) and extract species' name according to the code
          aqm <- fread(aquamaps_data, stringsAsFactors = FALSE) %>% 
            dplyr::select(speciesID, genus, species) %>% 
            dplyr::mutate(scientific_name = paste(genus, species, sep = " ")) %>% 
            dplyr::select(speciesID, scientific_name)
          # Merging two data frames
          species_targets <- dplyr::left_join(x = data.frame(speciesID, stringsAsFactors = FALSE), y = aqm,  by = "speciesID")
          
          # Reading IUCN .csv and filtering by Threatened categories (Vulberable[VU], Endangered[EN], Critically Endangered[CR])
          iucn <- fread(iucn_data, stringsAsFactors = FALSE, fill = TRUE) %>% 
            dplyr::select(scientific_name, category) %>% 
            dplyr::filter(category %in%  c("CR", "EN", "VU")) # VU????
          
          # Merging species_targets df and iucn df
          final <- left_join(x = species_targets, y = iucn,  by = "scientific_name") %>% 
            na.omit() %>% 
            dplyr::mutate(targets = iucn_noclim_target) %>% 
            dplyr::select(speciesID, targets)
          
          # Loop every row to match species ID with the "original target df" and changing the target
          df2 <- csv_targets %>% 
            dplyr::group_by(feature_names_prov) %>% 
            dplyr::mutate(speciesID = unlist(strsplit(as.character(feature_names_prov), split = "_"))[1]) %>% 
            dplyr::mutate(targets2 = ifelse(is.na(final$targets[match(speciesID, final$speciesID)]), targets, final$targets[match(speciesID, final$speciesID)])) %>%
            dplyr::select(feature_names_prov, cells, targets2) %>%
            dplyr::rename(targets = targets2)
          # Writing the final object
          ns2.name <- sub(pattern = "*.csv", "", basename(list.files(path = dir.scenarios[j], pattern = pattern1, full.names = TRUE)))
          fwrite(df2, paste(paste(dir.scenarios[j], "/", sep = ""), paste(ns2.name, "iucn", sep = "-"), ".csv", sep = ""), row.names = FALSE)
          
        } else if (str_detect(string = dir.scenarios[j], pattern = "ssp") == TRUE) {
          
          # Read files
          csv_targets <- fread(list.files(path = dir.scenarios[j], pattern = pattern1, full.names = TRUE)) # all (species + clim + species&clim layers)
          csv_targets_noclim <- fread(list.files(path = dir.scenarios[j], pattern = pattern1, full.names = TRUE)) %>% 
            dplyr::filter(str_detect(string = feature_names_prov, pattern = "VoCC|RCE") == FALSE) # just species
          csv_iucn <- fread(iucn_data)
          
          # Getting species' code from targets' file
          if(nrow(csv_targets_noclim) == 0) {
            df1 <- strsplit(as.character(csv_targets$feature_names_prov), split = "_")
          } else {
            df1 <- strsplit(as.character(csv_targets_noclim$feature_names_prov), split = "_")
          }
          df2 <- lapply(df1, function(x) {x[1]})
          speciesID <- do.call(rbind, df2)
          # Read the AquaMaps data (species occur .csv) and extract species' name according to the code
          aqm <- fread(aquamaps_data, stringsAsFactors = FALSE) %>% 
            dplyr::select(speciesID, genus, species) %>% 
            dplyr::mutate(scientific_name = paste(genus, species, sep = " ")) %>% 
            dplyr::select(speciesID, scientific_name)
          # Merging two data frames
          species_targets <- dplyr::left_join(x = data.frame(speciesID, stringsAsFactors = FALSE), y = aqm,  by = "speciesID")
          
          # Reading IUCN .csv and filtering by Threatened categories (Vulberable[VU], Endangered[EN], Critically Endangered[CR])
          iucn <- fread(iucn_data, stringsAsFactors = FALSE, fill = TRUE) %>% 
            dplyr::select(scientific_name, category) %>% 
            dplyr::filter(category %in%  c("CR", "EN", "VU")) # VU????
          
          # Merging species_targets df and iucn df
          final <- left_join(x = species_targets, y = iucn,  by = "scientific_name") %>% 
            na.omit() %>% 
            dplyr::mutate(targets = iucn_clim_target) %>% 
            dplyr::select(speciesID, targets)
          
          # Loop every row to match species ID with the "original target df" and changing the target
          df2 <- csv_targets %>% 
            dplyr::group_by(feature_names_prov) %>% 
            dplyr::mutate(speciesID = unlist(strsplit(as.character(feature_names_prov), split = "_"))[1]) %>% 
            dplyr::mutate(targets2 = ifelse(is.na(final$targets[match(speciesID, final$speciesID)]), targets, final$targets[match(speciesID, final$speciesID)])) %>%
            dplyr::select(feature_names_prov, cells, targets2) %>%
            dplyr::rename(targets = targets2)
          # Writing the final object
          ns2.name <- sub(pattern = "*.csv", "", basename(list.files(path = dir.scenarios[j], pattern = pattern1, full.names = TRUE)))
          fwrite(df2, paste(paste(dir.scenarios[j], "/", sep = ""), paste(ns2.name, "iucn", sep = "-"), ".csv", sep = ""), row.names = FALSE)
        }
    }
}

system.time(iucn_targets(path = "/scratch/user/uqibrito/Project04c/Prioritisation/PrioritizrBegin/features_10100",
                         aquamaps_data = "/scratch/user/uqibrito/Project04c/Inputs/Aqm/speciesoccursum.csv",
                         iucn_data = "/scratch/user/uqibrito/Project04c/Inputs/Aqm/IUCN_REDLIST_2020.csv",
                         iucn_noclim_target = 0,
                         iucn_clim_target = 1))

# system.time(iucn_targets(path = "Prioritisation/PrioritizrBegin/features_10100",
#                          aquamaps_data = "Inputs/Aqm/speciesoccursum.csv",
#                          iucn_data = "Inputs/Aqm/IUCN_REDLIST_2020.csv",
#                          iucn_noclim_target = 0, 
#                          iucn_clim_target = 1))






