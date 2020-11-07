# This code was written by Isaac Brito-Morales (i.britomorales@uq.edu.au)
# Please do not distribute this code without permission.
# NO GUARANTEES THAT CODE IS CORRECT
# Caveat Emptor!

# MaxTarget% - PUs(i)/MaxPUs(all spp) * (MaxTarget%-MinTarget%)
# 0.50 - (5/100) * (0.50 - 0.10)

csvs_pus_provinces <- function(path, min_target, max_target, clim_min_target, clim_max_target) {
  library(data.table)
  library(dplyr)
  library(doParallel)
  library(foreach)
  library(stringr)
  
  # List of directories
    dir.scenarios <- paste(list.dirs(path = path, full.names = TRUE, recursive = FALSE), sep = "/") # Climate Models Directory
    pattern1 <-  c(paste0("*Longhurst*", ".*.csv$"), paste0("*Glasgow*", ".*.csv$"), paste0("*GOODS*", ".*.csv$")) # the different provinces
  # Begin the parallel structure      
    cores  <-  3
    cl <- makeCluster(cores)
    registerDoParallel(cl)
    foreach(j = 1:length(dir.scenarios), .packages = c("dplyr", "stringr", "data.table")) %dopar% {
      # Files location 
        csv_olayer_prov <- list.files(path = dir.scenarios[j], pattern = paste0(pattern1, collapse = "|"), full.names = TRUE) # provinces file 
        csv_olayer_species <- list.files(path = dir.scenarios[j], pattern = "*lagic*.csv", full.names = TRUE) # species file
        rce_csv <- list.files(path = dir.scenarios[j], pattern = "_RCE_.*.csv$", full.names = TRUE) # RCE file
        vocc_csv <- list.files(path = dir.scenarios[j], pattern = "voccMag_.*.csv$", full.names = TRUE) # VoCC file
      # Climate variable conditional 
        if(length(rce_csv) == 0 & length(vocc_csv) == 0) { # if you have a base scenario just do it without climatic information
          # Calculating Species by Provinces and Setting targets for each prov-species
            file_olayer_pus <- fread(csv_olayer_prov) %>% 
              arrange(layer)
            file_olayer_species <- fread(csv_olayer_species) %>% 
              arrange(pu)
            # Match province name with species name
              file_olayer_species$province <- file_olayer_pus$province[match(file_olayer_species$pu, file_olayer_pus$layer)] # maybe with dplyr?
              file_olayer_species <- file_olayer_species %>% 
                mutate(feature_names_prov = ifelse(is.na(province), paste("non-categ", prov_name, sep = "_"),
                                                   paste(feature_names, province, sep = "_"))) %>% 
                dplyr::group_by(feature_names) %>%
                dplyr::mutate(cells = n()) %>% # global cells for that species %>% 
                dplyr::filter(cells >= 10) %>%  # more than 10 records for species in ABNJs
                dplyr::ungroup() %>% 
                data.frame() %>% 
                dplyr::select(-cells)
            # Writing the object [which is going to be used for 1b .dat function]
              ns1 <- unlist(strsplit(x = basename(dir.scenarios[j]), split = "_"))[2]
              ns1.name <- paste("sps", ns1, "provinces", sep = "_")
              fwrite(file_olayer_species, paste(paste(dir.scenarios[j], "/", sep = ""), ns1.name, ".csv", sep = ""), row.names = FALSE)
          
          # Defining targets and creating .csv species targets file
            trg <- file_olayer_species %>%
              dplyr::group_by(feature_names) %>%
              dplyr::summarise(cells = n()) %>% # global cells for that species
              dplyr::mutate(targets = (max_target - ((cells/max(cells)) * (max_target - min_target)))) %>% # relative to global planning region
              dplyr::ungroup()
            global_trg <- left_join(x = file_olayer_species, y = trg, by = "feature_names") %>% 
              dplyr::select(feature_names_prov, cells, targets)
            # Writing the object [which is going to be used for 1b .dat function]
              ns2.name <- paste("sps", ns1, "targets", sep = "_")
              fwrite(global_trg, paste(paste(dir.scenarios[j], "/", sep = ""), ns2.name, ".csv", sep = ""), row.names = FALSE)
          
        } else if (length(rce_csv) != 0 & length(vocc_csv) != 0) { # for VoCC and RCE
          # Calculating Species by Provinces
            file_olayer_pus <- fread(csv_olayer_prov) %>% 
              arrange(layer)
            file_olayer_species <- fread(csv_olayer_species) %>% 
              arrange(pu)
            # Match province name with species name
              file_olayer_species$province <- file_olayer_pus$province[match(file_olayer_species$pu, file_olayer_pus$layer)] # maybe with dplyr?
              file_olayer_species <- file_olayer_species %>% 
                mutate(feature_names_prov = ifelse(is.na(province), paste("non-categ", prov_name, sep = "_"),
                                                   paste(feature_names, province, sep = "_"))) %>% 
                dplyr::group_by(feature_names) %>%
                dplyr::mutate(cells = n()) %>% # global cells for that species %>% 
                dplyr::filter(cells >= 10) %>% 
                dplyr::ungroup() %>% 
                data.frame() %>% 
                dplyr::select(-cells)
          
          # Calculating VoCC-RCE by Provinces and Setting targets for each of those 
            rce <- fread(rce_csv) %>% 
              dplyr::select(-V1)
            vocc <- fread(vocc_csv) %>% 
              dplyr::select(-V1)
            # Creating the final .csv file
              rce_vocc <- rbind(rce, vocc) %>% 
                dplyr::arrange(pu)
            # Match province name with species name
              rce_vocc$province <- file_olayer_pus$province[match(rce_vocc$pu, file_olayer_pus$layer)] # maybe with dplyr?
              rce_vocc <- rce_vocc %>% 
                mutate(feature_names_prov = ifelse(is.na(province), paste("non-categ", prov_name, sep = "_"),
                                                   paste(feature_names, province, sep = "_")))

          # Creating Species LOW-VoCC/RCE Feature data
            # VoCC
              file_olayer_species_vocc <- rce_vocc %>% 
                dplyr::filter(str_detect(string = feature_names, pattern = "VoCC") == TRUE) %>% 
                dplyr::left_join(x = file_olayer_species,  by = "pu") %>% 
                dplyr::arrange(pu) %>% 
                dplyr::group_by(feature_names_prov.x) %>% # low velocity ares within species per province or target
                dplyr::mutate(low_climate_feature = ifelse(climate_feature <= as.vector(quantile(climate_feature, na.rm = TRUE)[2]), climate_feature, NA)) %>% 
                dplyr::ungroup() %>% 
                data.frame() %>% 
                dplyr::filter(!is.na(low_climate_feature)) %>% 
                dplyr::select(pu, area_km2.x, feature_names.x,  province.x, feature_names_prov.x, feature_names.y) %>% 
                dplyr::mutate(feature_names_prov = paste(feature_names_prov.x, feature_names.y, sep = "_")) %>% 
                dplyr::select(pu, area_km2.x, feature_names.x,  province.x, feature_names_prov) %>% 
                dplyr::rename(area_km2 = area_km2.x, feature_names = feature_names.x,  province = province.x)
            # RCE
              file_olayer_species_rce <- rce_vocc %>% 
                dplyr::filter(str_detect(string = feature_names, pattern = "RCE") == TRUE) %>% 
                dplyr::left_join(x = file_olayer_species,  by = "pu") %>% 
                dplyr::arrange(pu) %>% 
                dplyr::group_by(feature_names_prov.x) %>% # low RCE ares within species per province or target
                dplyr::mutate(low_climate_feature = ifelse(climate_feature <= as.vector(quantile(climate_feature, na.rm = TRUE)[2]), climate_feature, NA)) %>% 
                dplyr::ungroup() %>% 
                data.frame() %>% 
                dplyr::filter(!is.na(low_climate_feature)) %>% 
                dplyr::select(pu, area_km2.x, feature_names.x,  province.x, feature_names_prov.x, feature_names.y) %>% 
                dplyr::mutate(feature_names_prov = paste(feature_names_prov.x, feature_names.y, sep = "_")) %>% 
                dplyr::select(pu, area_km2.x, feature_names.x,  province.x, feature_names_prov) %>% 
                dplyr::rename(area_km2 = area_km2.x, feature_names = feature_names.x,  province = province.x)
              
          # Writing the FINAL FEATURES LOW-VELOCITY-RCE SPECIES DISTRIBUTION DATA BY PROVINCES [DO NOT INCLUDE SPECIES OR CLIMATE HERE THEY ARE DUPLICATES]
            features_final <- rbind(file_olayer_species_vocc, file_olayer_species_rce)
              ns1 <- unlist(strsplit(x = basename(dir.scenarios[j]), split = "_"))[2]
              ns1.name <- paste("sps-rce-vocc-mixall", ns1, "provinces", sep = "_")
            fwrite(features_final, paste(paste(dir.scenarios[j], "/", sep = ""), ns1.name, ".csv", sep = ""), row.names = FALSE)
              
            # Targets Low VoCC-Species
              target_vocc_species <- file_olayer_species_vocc %>%
                dplyr::group_by(feature_names) %>%
                dplyr::summarise(cells = n()) %>% # global cells for that species
                dplyr::mutate(targets = (clim_max_target - ((cells/max(cells)) * (clim_max_target - clim_min_target)))) %>% # relative to global low cells
                dplyr::ungroup()
              target_vocc_species_final <- left_join(x = file_olayer_species_vocc, y = target_vocc_species, by = "feature_names") %>% 
                dplyr::select(feature_names_prov, cells, targets)
            # Targets Low RCE-Species
              target_rce_species <- file_olayer_species_rce %>%
                dplyr::group_by(feature_names) %>%
                dplyr::summarise(cells = n()) %>% # global cells for that species
                dplyr::mutate(targets = (clim_max_target - ((cells/max(cells)) * (clim_max_target - clim_min_target)))) %>% # relative to global low cells
                dplyr::ungroup()
              target_rce_species_final <- left_join(x = file_olayer_species_rce, y = target_vocc_species, by = "feature_names") %>% 
                dplyr::select(feature_names_prov, cells, targets)
          
          # Writing the FINAL TARGETS LOW-VELOCITY-RCE SPECIES DISTRIBUTION DATA BY PROVINCES [DO NOT INCLUDE SPECIES OR CLIMATE HERE THEY ARE DUPLICATES]
            targets_final <- rbind(target_vocc_species_final, target_rce_species_final)
            ns2.name <- paste("sps-rce-vocc-mixall", ns1, "targets", sep = "_")
            fwrite(targets_final, paste(paste(dir.scenarios[j], "/", sep = ""), ns2.name, ".csv", sep = ""), row.names = FALSE)
            
        } else if(length(rce_csv) == 0 & length(vocc_csv) != 0) { # just VoCC
          # Calculating Species by Provinces
            file_olayer_pus <- fread(csv_olayer_prov) %>% 
              arrange(layer)
            file_olayer_species <- fread(csv_olayer_species) %>% 
              arrange(pu)
          # Match province name with species name
            file_olayer_species$province <- file_olayer_pus$province[match(file_olayer_species$pu, file_olayer_pus$layer)] # maybe with dplyr?
            file_olayer_species <- file_olayer_species %>% 
              mutate(feature_names_prov = ifelse(is.na(province), paste("non-categ", prov_name, sep = "_"),
                                                 paste(feature_names, province, sep = "_"))) %>% 
              dplyr::group_by(feature_names) %>%
              dplyr::mutate(cells = n()) %>% # global cells for that species %>% 
              dplyr::filter(cells >= 10) %>% 
              dplyr::ungroup() %>% 
              data.frame() %>% 
              dplyr::select(-cells)
            
          # Calculating VoCC-RCE by Provinces
            vocc <- fread(vocc_csv) %>% 
              dplyr::select(-V1) %>% 
              dplyr::arrange(pu)
            # Match province name with species name
              vocc$province <- file_olayer_pus$province[match(vocc$pu, file_olayer_pus$layer)] # maybe with dplyr?
              vocc <- vocc %>% 
                mutate(feature_names_prov = ifelse(is.na(province), paste("non-categ", prov_name, sep = "_"),
                                                   paste(feature_names, province, sep = "_")))
          
          # Creating Species LOW-VoCC Feature data
              file_olayer_species_vocc <- rce_vocc %>% 
                dplyr::filter(str_detect(string = feature_names, pattern = "VoCC") == TRUE) %>% 
                dplyr::left_join(x = file_olayer_species,  by = "pu") %>% 
                dplyr::arrange(pu) %>% 
                dplyr::group_by(feature_names_prov.x) %>% # low velocity ares within species per province or target
                dplyr::mutate(low_climate_feature = ifelse(climate_feature <= as.vector(quantile(climate_feature, na.rm = TRUE)[2]), climate_feature, NA)) %>% 
                dplyr::ungroup() %>% 
                data.frame() %>% 
                dplyr::filter(!is.na(low_climate_feature)) %>% 
                dplyr::select(pu, area_km2.x, feature_names.x,  province.x, feature_names_prov.x, feature_names.y) %>% 
                dplyr::mutate(feature_names_prov = paste(feature_names_prov.x, feature_names.y, sep = "_")) %>% 
                dplyr::select(pu, area_km2.x, feature_names.x,  province.x, feature_names_prov) %>% 
                dplyr::rename(area_km2 = area_km2.x, feature_names = feature_names.x,  province = province.x)
            
              # Writing the FINAL FEATURES LOW-VELOCITY SPECIES DISTRIBUTION DATA BY PROVINCES [DO NOT INCLUDE SPECIES OR CLIMATE HERE THEY ARE DUPLICATES]
              features_final <- rbind(file_olayer_species_vocc)
              ns1 <- unlist(strsplit(x = basename(dir.scenarios[j]), split = "_"))[2]
              ns1.name <- paste("sps-vocc-mixall", ns1, "provinces", sep = "_")
              fwrite(features_final, paste(paste(dir.scenarios[j], "/", sep = ""), ns1.name, ".csv", sep = ""), row.names = FALSE)
              
          # Targets Low VoCC-Species
              target_vocc_species <- file_olayer_species_vocc %>%
                dplyr::group_by(feature_names) %>%
                dplyr::summarise(cells = n()) %>% # global cells for that species
                dplyr::mutate(targets = (clim_max_target - ((cells/max(cells)) * (clim_max_target - clim_min_target)))) %>% # relative to global low cells
                dplyr::ungroup()
              target_vocc_species_final <- left_join(x = file_olayer_species_vocc, y = target_vocc_species, by = "feature_names") %>% 
                dplyr::select(feature_names_prov, cells, targets)
            
              # Writing the FINAL TARGETS LOW-VELOCITY SPECIES DISTRIBUTION DATA BY PROVINCES [DO NOT INCLUDE SPECIES OR CLIMATE HERE THEY ARE DUPLICATES]
              targets_final <- rbind(target_vocc_species_final)
              ns2.name <- paste("sps-vocc-mixall", ns1, "targets", sep = "_")
              fwrite(targets_final, paste(paste(dir.scenarios[j], "/", sep = ""), ns2.name, ".csv", sep = ""), row.names = FALSE)
            
        } else if(length(rce_csv) != 0 & length(vocc_csv) == 0) { # just RCE
          # Calculating Species by Provinces
            file_olayer_pus <- fread(csv_olayer_prov) %>% 
              arrange(layer)
            file_olayer_species <- fread(csv_olayer_species) %>% 
              arrange(pu)
          # Match province name with species name
            file_olayer_species$province <- file_olayer_pus$province[match(file_olayer_species$pu, file_olayer_pus$layer)] # maybe with dplyr?
            file_olayer_species <- file_olayer_species %>% 
              mutate(feature_names_prov = ifelse(is.na(province), paste("non-categ", prov_name, sep = "_"),
                                                 paste(feature_names, province, sep = "_"))) %>% 
              dplyr::group_by(feature_names) %>%
              dplyr::mutate(cells = n()) %>% # global cells for that species %>% 
              dplyr::filter(cells >= 10) %>% 
              dplyr::ungroup() %>% 
              data.frame() %>% 
              dplyr::select(-cells)
          
          # Calculating RCE by Provinces and Setting targets for each of those 
            rce <- fread(rce_csv) %>% 
              dplyr::select(-V1)
            # Match province name with species name
              rce$province <- file_olayer_pus$province[match(rce$pu, file_olayer_pus$layer)] # maybe with dplyr?
              rce <- rce %>% 
                mutate(feature_names_prov = ifelse(is.na(province), paste("non-categ", prov_name, sep = "_"),
                                                   paste(feature_names, province, sep = "_")))
            
          # Creating Species LOW-RCE Feature data
              file_olayer_species_rce <- rce_vocc %>% 
                dplyr::filter(str_detect(string = feature_names, pattern = "RCE") == TRUE) %>% 
                dplyr::left_join(x = file_olayer_species,  by = "pu") %>% 
                dplyr::arrange(pu) %>% 
                dplyr::group_by(feature_names_prov.x) %>% # low RCE ares within species per province or target
                dplyr::mutate(low_climate_feature = ifelse(climate_feature <= as.vector(quantile(climate_feature, na.rm = TRUE)[2]), climate_feature, NA)) %>% 
                dplyr::ungroup() %>% 
                data.frame() %>% 
                dplyr::filter(!is.na(low_climate_feature)) %>% 
                dplyr::select(pu, area_km2.x, feature_names.x,  province.x, feature_names_prov.x, feature_names.y) %>% 
                dplyr::mutate(feature_names_prov = paste(feature_names_prov.x, feature_names.y, sep = "_")) %>% 
                dplyr::select(pu, area_km2.x, feature_names.x,  province.x, feature_names_prov) %>% 
                dplyr::rename(area_km2 = area_km2.x, feature_names = feature_names.x,  province = province.x)
            
              # Writing the FINAL FEATURES LOW-RCE SPECIES DISTRIBUTION DATA BY PROVINCES [DO NOT INCLUDE SPECIES OR CLIMATE HERE THEY ARE DUPLICATES]
              features_final <- rbind(file_olayer_species_rce)
              ns1 <- unlist(strsplit(x = basename(dir.scenarios[j]), split = "_"))[2]
              ns1.name <- paste("sps-rce-mixall", ns1, "provinces", sep = "_")
              fwrite(features_final, paste(paste(dir.scenarios[j], "/", sep = ""), ns1.name, ".csv", sep = ""), row.names = FALSE)
              
          # Targets Low RCE-Species
            target_rce_species <- file_olayer_species_rce %>%
                dplyr::group_by(feature_names) %>%
                dplyr::summarise(cells = n()) %>% # global cells for that species
                dplyr::mutate(targets = (clim_max_target - ((cells/max(cells)) * (clim_max_target - clim_min_target)))) %>% # relative to global low cells
                dplyr::ungroup()
              target_rce_species_final <- left_join(x = file_olayer_species_rce, y = target_vocc_species, by = "feature_names") %>% 
                dplyr::select(feature_names_prov, cells, targets)
            
              # Writing the FINAL TARGETS LOW-RCE SPECIES DISTRIBUTION DATA BY PROVINCES [DO NOT INCLUDE SPECIES OR CLIMATE HERE THEY ARE DUPLICATES]
              targets_final <- rbind(target_rce_species_final)
              ns2.name <- paste("sps-rce-mixall", ns1, "targets", sep = "_")
              fwrite(targets_final, paste(paste(dir.scenarios[j], "/", sep = ""), ns2.name, ".csv", sep = ""), row.names = FALSE) 
        }
    }
    stopCluster(cl)
}

csvs_pus_provinces(path = "features_0520CSV050_targets-mix_test",
                   min_target = 0.05,
                   max_target = 0.20,
                   clim_min_target = 0.05,
                   clim_max_target = 0.20)
