# This code was written by Isaac Brito-Morales (i.britomorales@uq.edu.au)
# Please do not distribute this code without permission.
# NO GUARANTEES THAT CODE IS CORRECT
# Caveat Emptor!

# MaxTarget% - PUs(i)/MaxPUs(all spp) * (MaxTarget%-MinTarget%)
# 0.50 - (5/100) * (0.50 - 0.10)

csvs_pus_provinces <- function(path, min_target, max_target, clim_target) {
  library(data.table)
  library(dplyr)
  library(doParallel)
  library(foreach)
  library(stringr)
  
  # List of directories
    dir.scenarios <- paste(list.dirs(path = path, full.names = TRUE, recursive = FALSE), sep = "/") # Climate Models Directory
    pattern1 <-  c(paste0("*Longhurst*", ".*.csv$"), paste0("*Glasgow*", ".*.csv$"), paste0("*GOODS*", ".*.csv$")) # the different provinces
  # Begin the parallel structure      
    cores  <-  12
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
                                                   paste(feature_names, province, sep = "_")))
            # Writing the object [which is going to be used for 1b .dat function]
              ns1 <- unlist(strsplit(x = basename(dir.scenarios[j]), split = "_"))[2]
              ns1.name <- paste("sps", ns1, "provinces", sep = "_")
              fwrite(file_olayer_species, paste(paste(dir.scenarios[j], "/", sep = ""), ns1.name, ".csv", sep = ""), row.names = FALSE)
          
          # Defining targets and creating .csv species targets file
            provinces_bypu <- unique(file_olayer_pus$province) # provinces for loop
            dt_final <- list()
            for(i in 1:length(provinces_bypu)) {
              dt1 <- file_olayer_pus %>% 
                filter(province == provinces_bypu[i])
              max_pus <- length(unique(dt1$layer))
              dt2 <- file_olayer_species %>% 
                filter(province == provinces_bypu[i]) %>%  
                group_by(feature_names_prov) %>% 
                summarise(cells = n()) %>% 
                mutate(targets = (max_target - ((cells/max_pus) * (max_target - min_target))))
              dt_final[[i]] <- dt2
              }
            dt_testing <- do.call(rbind, dt_final)
            # Writing the object [which is going to be used for 1b .dat function]
              ns2.name <- paste("sps", ns1, "targets", sep = "_")
              fwrite(dt_testing, paste(paste(dir.scenarios[j], "/", sep = ""), ns2.name, ".csv", sep = ""), row.names = FALSE)
          
        } else if (length(rce_csv) == 0 & length(vocc_csv) == 0) {
          # Calculating Species by Provinces and Setting targets for each prov-species
            file_olayer_pus <- fread(csv_olayer_prov) %>% 
              arrange(layer)
            file_olayer_species <- fread(csv_olayer_species) %>% 
              arrange(pu)
            # Match province name with species name [BUT NOT WRITING UNTIL THE END]
              file_olayer_species$province <- file_olayer_pus$province[match(file_olayer_species$pu, file_olayer_pus$layer)] # maybe with dplyr?
              file_olayer_species <- file_olayer_species %>% 
                mutate(feature_names_prov = ifelse(is.na(province), paste("non-categ", prov_name, sep = "_"),
                                                   paste(feature_names, province, sep = "_")))
          
          # Defining targets and creating .csv species targets file [BUT NOT WRITING UNTIL THE END]
            provinces_bypu <- unique(file_olayer_pus$province) # provinces for loop
            dt_final <- list()
            for(i in 1:length(provinces_bypu)) {
              dt1 <- file_olayer_pus %>% 
                filter(province == provinces_bypu[i])
              max_pus <- length(unique(dt1$layer))
              dt2 <- file_olayer_species %>% 
                filter(province == provinces_bypu[i]) %>%  
                group_by(feature_names_prov) %>% 
                summarise(cells = n()) %>% 
                mutate(targets = (max_target - ((cells/max_pus) * (max_target - min_target))))
              dt_final[[i]] <- dt2
              }
            dt_testing <- do.call(rbind, dt_final)
          
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

          # Estimating lower quantiles for RCE and VoCC per Province
            # VoCC
              vocc_prov <- rce_vocc %>% 
                dplyr::filter(str_detect(string = feature_names, pattern = "VoCC") == TRUE) %>% 
                data.frame()
              province_vocc <- unique(vocc_prov$feature_names_prov)
              province_vocc_list <- vector("list", length = length(province_vocc))
              for(kk in 1:length(province_vocc)) {
                single <- vocc_prov %>% 
                  dplyr::filter(feature_names_prov == unique(feature_names_prov)[kk]) %>% 
                  na.omit()
                vocc_qt <- quantile(single$climate_feature)
                province_vocc_list[[kk]] <- single %>% 
                  dplyr::filter(climate_feature <= as.vector(vocc_qt[2]))
                }
              vocc_prov_final <- do.call(rbind, province_vocc_list) %>% 
                dplyr::arrange(pu) %>% 
                dplyr::select(-climate_feature)
            # RCE
              rce_prov <- rce_vocc %>% 
                dplyr::filter(str_detect(string = feature_names, pattern = "RCE") == TRUE) %>% 
                data.frame()
              province_rce <- unique(rce_prov$feature_names_prov)
              province_rce_list <- vector("list", length = length(province_rce))
              for(l in 1:length(province_rce)) {
                single <- rce_prov %>% 
                  dplyr::filter(feature_names_prov == unique(feature_names_prov)[l]) %>% 
                  na.omit()
                vocc_qt <- quantile(single$climate_feature)
                province_rce_list[[l]] <- single %>% 
                  dplyr::filter(climate_feature <= as.vector(vocc_qt[2]))
                }
              rce_prov_final <- do.call(rbind, province_rce_list) %>% 
                dplyr::arrange(pu) %>% 
                dplyr::select(-climate_feature)
          
          # Creating Species LOW-VoCC/RCE Feature data
            # VoCC
              file_olayer_species_vocc <- vocc_prov_final
              file_olayer_species_vocc$species <- file_olayer_species$feature_names_prov[match(file_olayer_species_vocc$pu, file_olayer_species$pu)] # maybe with dplyr?
              file_olayer_species_vocc <- file_olayer_species_vocc %>% 
                na.omit() %>% 
                dplyr::mutate(feature_names_prov = paste(species, feature_names, sep = "_")) %>% 
                dplyr::select(pu, area_km2, feature_names, province, feature_names_prov)
            # RCE
              file_olayer_species_rce <- rce_prov_final
              file_olayer_species_rce$species <- file_olayer_species$feature_names_prov[match(file_olayer_species_rce$pu, file_olayer_species$pu)] # maybe with dplyr?
              file_olayer_species_rce <- file_olayer_species_rce %>% 
                na.omit() %>% 
                dplyr::mutate(feature_names_prov = paste(species, feature_names, sep = "_")) %>% 
                dplyr::select(pu, area_km2, feature_names, province, feature_names_prov)
              
          # Writing the FINAL FEATURES FILES WITH SPECIES, VOCC25QT, RCE25QT ALL BY PROVINCES
            features_final <- rbind(file_olayer_species, vocc_prov_final, rce_prov_final, file_olayer_species_vocc, file_olayer_species_rce)
              ns1 <- unlist(strsplit(x = basename(dir.scenarios[j]), split = "_"))[2]
              ns1.name <- paste("sps-rce-vocc-mixall", ns1, "provinces", sep = "_")
            fwrite(features_final, paste(paste(dir.scenarios[j], "/", sep = ""), ns1.name, ".csv", sep = ""), row.names = FALSE)
          
          # Defining Targets for RCE and VoCC
            # Targets VoCC
              provinces_bypu_vocc <- unique(vocc_prov_final$province) # provinces for loop
              target_vocc <- vector("list", length = length(provinces_bypu_vocc))
              for(m in 1:length(provinces_bypu_vocc)) {
                dt2 <- vocc_prov_final %>% 
                  filter(province == provinces_bypu_vocc[m]) %>%  
                  group_by(feature_names_prov) %>% 
                  summarise(cells = n()) %>% 
                  mutate(targets = clim_target)
                target_vocc[[m]] <- dt2
                }
              target_vocc_final <- do.call(rbind, target_vocc)
            # Targets RCE
              provinces_bypu_rce <- unique(rce_prov_final$province) # provinces for loop
              target_rce <- vector("list", length = length(provinces_bypu_rce))
              for(a in 1:length(provinces_bypu_rce)) {
                dt2 <- rce_prov_final %>% 
                  filter(province == provinces_bypu_rce[a]) %>%  
                  group_by(feature_names_prov) %>% 
                  summarise(cells = n()) %>% 
                  mutate(targets = clim_target)
                target_rce[[a]] <- dt2
                }
              target_rce_final <- do.call(rbind, target_rce)
              
            # Targets Low VoCC-Species
              provinces_bypu_vocc_species <- unique(file_olayer_species_vocc$province) # provinces for loop
              target_vocc_species <- vector("list", length = length(provinces_bypu_vocc_species))
              for(n in 1:length(provinces_bypu_vocc_species)) {
                dt2 <- file_olayer_species_vocc %>% 
                  filter(province == provinces_bypu_vocc_species[n]) %>%  
                  group_by(feature_names_prov) %>% 
                  summarise(cells = n()) %>% 
                  mutate(targets = clim_target)
                target_vocc_species[[n]] <- dt2
              }
              target_vocc_species_final <- do.call(rbind, target_vocc_species)
            # Targets Low RCE-Species
              provinces_bypu_rce_species <- unique(file_olayer_species_rce$province) # provinces for loop
              target_rce_species <- vector("list", length = length(provinces_bypu_rce_species))
              for(ll in 1:length(provinces_bypu_rce_species)) {
                dt2 <- file_olayer_species_rce %>% 
                  filter(province == provinces_bypu_rce_species[ll]) %>%  
                  group_by(feature_names_prov) %>% 
                  summarise(cells = n()) %>% 
                  mutate(targets = clim_target)
                target_rce_species[[ll]] <- dt2
              }
              target_rce_species_final <- do.call(rbind, target_rce_species)
          
          # Writing the FINAL TARGET FILE WITH SPECIES, VOCC25QT, RCE25QT ALL BY PROVINCES 
            targets_final <- rbind(dt_testing, target_vocc_final, target_rce_final, target_vocc_species_final, target_rce_species_final)
            ns2.name <- paste("sps-rce-vocc-mixall", ns1, "targets", sep = "_")
            fwrite(targets_final, paste(paste(dir.scenarios[j], "/", sep = ""), ns2.name, ".csv", sep = ""), row.names = FALSE)
            
        } else if(length(rce_csv) == 0 & length(vocc_csv) != 0) { 
          # Calculating Species by Provinces and Setting targets for each prov-species
            file_olayer_pus <- fread(csv_olayer_prov) %>% 
              arrange(layer)
            file_olayer_species <- fread(csv_olayer_species) %>% 
              arrange(pu)
          # Match province name with species name [BUT NOT WRITING UNTIL THE END]
            file_olayer_species$province <- file_olayer_pus$province[match(file_olayer_species$pu, file_olayer_pus$layer)] # maybe with dplyr?
            file_olayer_species <- file_olayer_species %>% 
              mutate(feature_names_prov = ifelse(is.na(province), paste("non-categ", prov_name, sep = "_"),
                                                 paste(feature_names, province, sep = "_")))
          # Defining targets and creating .csv species targets file [BUT NOT WRITING UNTIL THE END]
            provinces_bypu <- unique(file_olayer_pus$province) # provinces for loop
            dt_final <- list()
            for(i in 1:length(provinces_bypu)) {
              dt1 <- file_olayer_pus %>% 
                filter(province == provinces_bypu[i])
              max_pus <- length(unique(dt1$layer))
              dt2 <- file_olayer_species %>% 
                filter(province == provinces_bypu[i]) %>%  
                group_by(feature_names_prov) %>% 
                summarise(cells = n()) %>% 
                mutate(targets = (max_target - ((cells/max_pus) * (max_target - min_target))))
              dt_final[[i]] <- dt2
            }
            dt_testing <- do.call(rbind, dt_final)
            
          # Calculating VoCC-RCE by Provinces and Setting targets for each of those 
            vocc <- fread(vocc_csv) %>% 
              dplyr::select(-V1) %>% 
              dplyr::arrange(pu)
            # Match province name with species name
              vocc$province <- file_olayer_pus$province[match(vocc$pu, file_olayer_pus$layer)] # maybe with dplyr?
              vocc <- vocc %>% 
                mutate(feature_names_prov = ifelse(is.na(province), paste("non-categ", prov_name, sep = "_"),
                                                   paste(feature_names, province, sep = "_")))
          # Estimating lower quantiles for VoCC per Province
            vocc_prov <- vocc %>% 
              dplyr::filter(str_detect(string = feature_names, pattern = "VoCC") == TRUE) %>% 
              data.frame()
            province_vocc <- unique(vocc_prov$feature_names_prov)
            province_vocc_list <- vector("list", length = length(province_vocc))
            for(kk in 1:length(province_vocc)) {
              single <- vocc_prov %>% 
                dplyr::filter(feature_names_prov == unique(feature_names_prov)[kk]) %>% 
                na.omit()
              vocc_qt <- quantile(single$climate_feature)
              province_vocc_list[[kk]] <- single %>% 
                dplyr::filter(climate_feature <= as.vector(vocc_qt[2]))
              }
              vocc_prov_final <- do.call(rbind, province_vocc_list) %>% 
                dplyr::arrange(pu) %>% 
                dplyr::select(-climate_feature)
          # Creating Species LOW-VoCC Feature data
            file_olayer_species_vocc <- vocc_prov_final
            file_olayer_species_vocc$species <- file_olayer_species$feature_names_prov[match(file_olayer_species_vocc$pu, file_olayer_species$pu)] # maybe with dplyr?
            file_olayer_species_vocc <- file_olayer_species_vocc %>% 
              na.omit() %>% 
              dplyr::mutate(feature_names_prov = paste(species, feature_names, sep = "_")) %>% 
              dplyr::select(pu, area_km2, feature_names, province, feature_names_prov)
            
            # Writing the FINAL FEATURES FILES WITH SPECIES, VOCC25QT, ALL BY PROVINCES
              features_final <- rbind(file_olayer_species, vocc_prov_final, file_olayer_species_vocc)
              ns1 <- unlist(strsplit(x = basename(dir.scenarios[j]), split = "_"))[2]
              ns1.name <- paste("sps-vocc-mixall", ns1, "provinces", sep = "_")
              fwrite(features_final, paste(paste(dir.scenarios[j], "/", sep = ""), ns1.name, ".csv", sep = ""), row.names = FALSE)
              
          # Defining Targets for VoCC
            provinces_bypu_vocc <- unique(vocc_prov_final$province) # provinces for loop
            target_vocc <- vector("list", length = length(provinces_bypu_vocc))
            for(m in 1:length(provinces_bypu_vocc)) {
              dt2 <- vocc_prov_final %>% 
                filter(province == provinces_bypu_vocc[m]) %>%  
                group_by(feature_names_prov) %>% 
                summarise(cells = n()) %>% 
                mutate(targets = clim_target)
              target_vocc[[m]] <- dt2
              }
            target_vocc_final <- do.call(rbind, target_vocc)
          # Targets Low VoCC-Species
            provinces_bypu_vocc_species <- unique(file_olayer_species_vocc$province) # provinces for loop
            target_vocc_species <- vector("list", length = length(provinces_bypu_vocc_species))
            for(n in 1:length(provinces_bypu_vocc_species)) {
              dt2 <- file_olayer_species_vocc %>% 
                filter(province == provinces_bypu_vocc_species[n]) %>%  
                group_by(feature_names_prov) %>% 
                summarise(cells = n()) %>% 
                mutate(targets = clim_target)
              target_vocc_species[[n]] <- dt2
            }
            target_vocc_species_final <- do.call(rbind, target_vocc_species)
            
            # Writing the FINAL TARGET FILE WITH SPECIES, VOCC25QT, RCE25QT ALL BY PROVINCES 
              targets_final <- rbind(dt_testing, target_vocc_final, target_vocc_species_final)
              ns2.name <- paste("sps-vocc-mixall", ns1, "targets", sep = "_")
              fwrite(targets_final, paste(paste(dir.scenarios[j], "/", sep = ""), ns2.name, ".csv", sep = ""), row.names = FALSE)
            
        } else if(length(rce_csv) != 0 & length(vocc_csv) == 0) {
          # Calculating Species by Provinces and Setting targets for each prov-species
            file_olayer_pus <- fread(csv_olayer_prov) %>% 
              arrange(layer)
            file_olayer_species <- fread(csv_olayer_species) %>% 
              arrange(pu)
          # Match province name with species name [BUT NOT WRITING UNTIL THE END]
            file_olayer_species$province <- file_olayer_pus$province[match(file_olayer_species$pu, file_olayer_pus$layer)] # maybe with dplyr?
            file_olayer_species <- file_olayer_species %>% 
              mutate(feature_names_prov = ifelse(is.na(province), paste("non-categ", prov_name, sep = "_"),
                                                 paste(feature_names, province, sep = "_")))
          # Defining targets and creating .csv species targets file [BUT NOT WRITING UNTIL THE END]
            provinces_bypu <- unique(file_olayer_pus$province) # provinces for loop
            dt_final <- list()
            for(i in 1:length(provinces_bypu)) {
              dt1 <- file_olayer_pus %>% 
                filter(province == provinces_bypu[i])
              max_pus <- length(unique(dt1$layer))
              dt2 <- file_olayer_species %>% 
                filter(province == provinces_bypu[i]) %>%  
                group_by(feature_names_prov) %>% 
                summarise(cells = n()) %>% 
                mutate(targets = (max_target - ((cells/max_pus) * (max_target - min_target))))
              dt_final[[i]] <- dt2
            }
            dt_testing <- do.call(rbind, dt_final)
          
          # Calculating RCE by Provinces and Setting targets for each of those 
            rce <- fread(rce_csv) %>% 
              dplyr::select(-V1)
            # Match province name with species name
              rce$province <- file_olayer_pus$province[match(rce$pu, file_olayer_pus$layer)] # maybe with dplyr?
              rce <- rce %>% 
                mutate(feature_names_prov = ifelse(is.na(province), paste("non-categ", prov_name, sep = "_"),
                                                   paste(feature_names, province, sep = "_")))
          # Estimating lower quantiles for RCE per Province
            rce_prov <- rce %>% 
              dplyr::filter(str_detect(string = feature_names, pattern = "RCE") == TRUE) %>% 
              data.frame()
            province_rce <- unique(rce_prov$feature_names_prov)
            province_rce_list <- vector("list", length = length(province_rce))
            for(l in 1:length(province_rce)) {
              single <- rce_prov %>% 
                dplyr::filter(feature_names_prov == unique(feature_names_prov)[l]) %>% 
                na.omit()
              vocc_qt <- quantile(single$climate_feature)
              province_rce_list[[l]] <- single %>% 
                dplyr::filter(climate_feature <= as.vector(vocc_qt[2]))
              }
            rce_prov_final <- do.call(rbind, province_rce_list) %>% 
              dplyr::arrange(pu) %>% 
              dplyr::select(-climate_feature)
            
          # Creating Species LOW-RCE Feature data
            file_olayer_species_rce <- rce_prov_final
            file_olayer_species_rce$species <- file_olayer_species$feature_names_prov[match(file_olayer_species_rce$pu, file_olayer_species$pu)] # maybe with dplyr?
            file_olayer_species_rce <- file_olayer_species_rce %>% 
              na.omit() %>% 
              dplyr::mutate(feature_names_prov = paste(species, feature_names, sep = "_")) %>% 
              dplyr::select(pu, area_km2, feature_names, province, feature_names_prov)
            
            # Writing the FINAL FEATURES FILES WITH SPECIES, RCE25QT ALL BY PROVINCES
              features_final <- rbind(file_olayer_species, rce_prov_final, file_olayer_species_rce)
              ns1 <- unlist(strsplit(x = basename(dir.scenarios[j]), split = "_"))[2]
              ns1.name <- paste("sps-rce-mixall", ns1, "provinces", sep = "_")
              fwrite(features_final, paste(paste(dir.scenarios[j], "/", sep = ""), ns1.name, ".csv", sep = ""), row.names = FALSE)
              
          # Defining Targets for RCE
            provinces_bypu_rce <- unique(rce_prov_final$province) # provinces for loop
            target_rce <- vector("list", length = length(provinces_bypu_rce))
            for(a in 1:length(provinces_bypu_rce)) {
              dt2 <- rce_prov_final %>% 
                filter(province == provinces_bypu_rce[a]) %>%  
                group_by(feature_names_prov) %>% 
                summarise(cells = n()) %>% 
                mutate(targets = clim_target)
              target_rce[[a]] <- dt2
              }
            target_rce_final <- do.call(rbind, target_rce)
          # Targets Low RCE-Species
            provinces_bypu_rce_species <- unique(file_olayer_species_rce$province) # provinces for loop
            target_rce_species <- vector("list", length = length(provinces_bypu_rce_species))
            for(ll in 1:length(provinces_bypu_rce_species)) {
              dt2 <- file_olayer_species_rce %>% 
                filter(province == provinces_bypu_rce_species[ll]) %>%  
                group_by(feature_names_prov) %>% 
                summarise(cells = n()) %>% 
                mutate(targets = clim_target)
              target_rce_species[[ll]] <- dt2
              }
            target_rce_species_final <- do.call(rbind, target_rce_species)
            
            # Writing the FINAL TARGET FILE WITH SPECIES, RCE25QT ALL BY PROVINCES 
              targets_final <- rbind(dt_testing, target_rce_final, target_rce_species_final)
              ns2.name <- paste("sps-rce-mixall", ns1, "targets", sep = "_")
              fwrite(targets_final, paste(paste(dir.scenarios[j], "/", sep = ""), ns2.name, ".csv", sep = ""), row.names = FALSE) 
        }
    }
    stopCluster(cl)
}

csvs_pus_provinces(path = "features_0520CSV040_targets-mix_vocc",
                   min_target = 0.10, 
                   max_target = 0.20,
                   clim_target = 0.40)

# csvs_pus_provinces(path = "/QRISdata/Q1216/BritoMorales/Project04b/features_0520CSV040_targets-mix_vocc",
#                    min_target = 0.05, 
#                    max_target = 0.20,
#                    clim_target = 0.40)
