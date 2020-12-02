

################
aqm <- fread("/Users/bri273/Desktop/AquaMaps_wflow/AquaMaps/v2019a/speciesoccursum.csv", fill = TRUE) %>% 
  dplyr::filter(rank == 1) %>% 
  dplyr::select(speciesID, phylum)

ep <- fread("epipelagic.csv") %>% 
  dplyr::mutate(speciesID = str_remove_all(string = feature_names, pattern = "_epipelagic")) %>%
  dplyr::arrange(pu) %>% 
  dplyr::select(pu, speciesID)

species_pu <- dplyr::left_join(x = ep, y = aqm,  by = "speciesID") %>% 
  dplyr::rename(id = pu)

final <- dplyr::left_join(x = species_pu, y = no_regrets02,  by = "id") %>% 
  na.omit() %>% 
  dplyr::arrange(id) %>% 
  dplyr::group_by(speciesID, phylum) %>% 
  dplyr::summarise(value = n()) %>% 
  ungroup() %>% 
  dplyr::group_by(phylum) %>% 
  dplyr::summarise(value = n()) %>% 
  ungroup() %>% 
  dplyr::mutate(group = "Epipelagic") %>% 
  dplyr::relocate(phylum, group, value)
  
  
  
  
# individual, group, value, id



