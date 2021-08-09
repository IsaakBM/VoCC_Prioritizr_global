# This code was written by Isaac Brito-Morales (i.britomorales@uq.edu.au)
# Please do not distribute this code without permission.
# NO GUARANTEES THAT CODE IS CORRECT
# Caveat Emptor!

library(data.table)
library(dplyr)
library(stringr)
library(sf)

pu_ep <- st_read("Output/01_abnjs_nofilterdepth/abnj_02-epipelagic_global_moll_05deg/abnj_02-epipelagic_global_moll_05deg.shp") %>% 
  as_tibble() %>% 
  select(-geometry) %>% 
  rename(pu1 = layer) %>% 
  arrange(pu1)
pu_mp <- st_read("Output/01_abnjs_nofilterdepth/abnj_03-mesopelagic_global_moll_05deg/abnj_03-mesopelagic_global_moll_05deg.shp") %>% 
  as_tibble() %>% 
  select(-geometry) %>% 
  rename(pu2 = layer) %>% 
  arrange(pu2)
pu_bap <- st_read("Output/01_abnjs_nofilterdepth/abnj_04-bathyabysso_global_moll_05deg/abnj_04-bathyabysso_global_moll_05deg.shp") %>% 
  as_tibble() %>% 
  select(-geometry) %>% 
  rename(pu3 = layer) %>% 
  arrange(pu3)
pu_sflr <- st_read("Output/01_abnjs_nofilterdepth/abnj_05-seafloor_global_moll_05deg/abnj_05-seafloor_global_moll_05deg.shp") %>% 
  as_tibble() %>% 
  select(-geometry) %>% 
  rename(pu4 = layer) %>% 
  arrange(pu4)
pu_plg <- cbind(pu_ep, pu_mp, pu_bap)
pu_all <- cbind(pu_ep, pu_mp, pu_bap, pu_sflr)

####################################################################################
####### 1.- Spiral Input Df per Layer
####################################################################################
# 
  spiralDF <- function(aqm, features, solution) {
    
    library(data.table)
    library(dplyr)
    library(stringr)
    
  # Read the AquaMaps data (species occur .csv) and extract species' name according to the code
    aqm <- fread(aqm, stringsAsFactors = FALSE, fill = TRUE) %>% 
      dplyr::filter(rank == 1)
    aqm_groups <- aqm %>% 
      mutate(groups_01 = ifelse(class == "Bivalvia", "Bivalvia",
                         ifelse(class == "Cephalopoda", "Cephalopoda", 
                         ifelse(class == "Polyplacophora", "Polyplacophora", 
                         ifelse(order == "Cetacea", "Cetacea", 
                         ifelse(phylum == "Echinodermata", "Echinoderms", 
                         ifelse(order == "Euphausiacea", "Euphausiids", 
                         ifelse(phylum == "Foraminifera", "Foraminifera", 
                         ifelse(class == "Gastropoda", "Gastropods", 
                         ifelse(class == "Hydrozoa", "Hydrozoans", 
                         ifelse(class == "Cubozoa", "Jellyfish", 
                         ifelse(class == "Scyphozoa", "Jellyfish", 
                         ifelse(class == "Anthozoa", "Corals", 
                         ifelse(family == "Odobenidae", "Pinnipeds", 
                         ifelse(family == "Otariidae", "Pinnipeds", 
                         ifelse(family == "Phocidae", "Pinnipeds", 
                         ifelse(class == "Reptilia", "Reptiles", 
                         ifelse(class == "Elasmobranchii", "Sharks - Rays", 
                         ifelse(phylum == "Porifera", "Sponges", 
                         ifelse(class == "Appendicularia", "Tunicates", 
                         ifelse(class == "Ascidiacea", "Tunicates", 
                         ifelse(class == "Thaliacea", "Tunicates", 
                         ifelse(phylum == "Acanthocephala", "Sea worms", 
                         ifelse(phylum == "Annelida", "Sea worms", 
                         ifelse(class == "Aplacophora", "Sea worms", 
                         ifelse(phylum == "Chaetognatha", "Sea worms", 
                         ifelse(phylum == "Cycliophora", "Sea worms", 
                         ifelse(phylum == "Kamptozoa", "Sea worms", 
                         ifelse(phylum == "Gastrotricha", "Sea worms", 
                         ifelse(phylum == "Gnathostomulida", "Sea worms", 
                         ifelse(phylum == "Hemichordata", "Sea worms", 
                         ifelse(phylum == "Kinorhyncha", "Sea worms", 
                         ifelse(phylum == "Loricifera", "Sea worms", 
                         ifelse(phylum == "Nematoda", "Sea worms", 
                         ifelse(phylum == "Nemertea", "Sea worms", 
                         ifelse(phylum == "Phoronida", "Sea worms", 
                         ifelse(phylum == "Platyhelminthes", "Sea worms", 
                         ifelse(phylum == "Priapulida", "Sea worms", 
                         ifelse(phylum == "Sipuncula", "Sea worms", 
                         ifelse(family == "Teredinidae", "Sea worms", 
                         ifelse(phylum == "Xenacoelomorpha", "Sea worms", 
                         ifelse(order == "Pantopoda", "Sea spiders", 
                         ifelse(phylum == "Chlorophyta", "Green algae",
                         ifelse(phylum == "Ochrophyta", "Brown algae", 
                         ifelse(phylum == "Rhodophyta", "Red algae", 
                         ifelse(family == "Xiphiidae", "Tunas - Billfishes", 
                         ifelse(order == "Perciformes" & family == "Pristiophoridae", "Tunas - Billfishes", 
                         ifelse(order == "Perciformes" & family == "Scombridae", "Tunas - Billfishes", "Other"))))))))))))))))))))))))))))))))))))))))))))))))
    aqm_groups <- aqm_groups %>% 
      dplyr::mutate(groups_01 = ifelse(class == "Actinopterygii" & groups_01 == "Other", "Bony fish", 
                                ifelse(class == "Sarcopterygii" & groups_01 == "Other", "Bony fish", 
                                ifelse(class == "Branchiopoda" & groups_01 == "Other", "Crustaceans", 
                                ifelse(class == "Cephalocarida" & groups_01 == "Other", "Crustaceans",
                                ifelse(class == "Maxillopoda" & groups_01 == "Other", "Crustaceans",
                                ifelse(class == "Ostracoda" & groups_01 == "Other", "Crustaceans",
                                ifelse(class == "Malacostraca" & groups_01 == "Other", "Crustaceans", groups_01)))))))) %>% 
      dplyr::select(speciesID, groups_01) # add the line with geom features here to create a new "bio_groups"
    geomF <- tibble(speciesID = c("Basins", "Bridges", "Canyons", "Escarpments", "Fans", "Guyots",
                                  "Plateaus", "Ridges", "Seamounts", "Sills", "Trenches", "Troughs"), 
                    groups_01 = "Geomorphic")
    
    aqm_groups <- rbind(aqm_groups, geomF)
    
  # Reading no regret df solution
    sol <- readRDS(solution) %>% 
      dplyr::rename(pu = id)
  # Species taxonomic information among planning units
    sps_df <- fread(features) %>% 
      dplyr::mutate(speciesID = str_split(string = feature_names, pattern = "_", simplify = TRUE)[,1]) %>%
      dplyr::arrange(pu) %>% 
      dplyr::select(pu, speciesID)
  # Merging "Species taxonomic information among planning units" with "Planning units climate-smart network"
    species <- dplyr::left_join(x = sol, y = sps_df,  by = "pu") %>% 
      dplyr::rename(id = pu) %>% 
      dplyr::filter(solution != 0) %>% 
      dplyr::group_by(speciesID) %>%
      dplyr::summarise(cells = n()) %>% 
      dplyr::mutate(rep_target = round((cells/nrow(dplyr::filter(sol, solution != 0)))*100, digits = 4)) %>% 
      dplyr::arrange(-rep_target) %>% 
      ungroup() %>% 
      na.omit()
  # 
    ff <- dplyr::left_join(x = species, y = aqm_groups,  by = "speciesID") %>% # on average this group is at least in XXX planning unit among all the pl
      dplyr::group_by(groups_01) %>% 
      dplyr::summarise(value = mean(rep_target)) %>% 
      ungroup() %>% 
      dplyr::arrange(-value) %>% 
      dplyr::mutate(group = unlist(str_split(basename(solution), "_"))[2]) %>% 
      dplyr::relocate(groups_01, group, value) %>% 
      dplyr::rename(individual = groups_01)
    
    return(ff)
  }

####################################################################################
####### 2.- Spiral Input Df Vertical Layer
####################################################################################
# 
  spiralDF_V <- function(aqm, features, solution, pu_all) {
    
  library(data.table)
  library(dplyr)
  library(stringr)
  
  # Read the AquaMaps data (species occur .csv) and extract species' name according to the code
  aqm <- fread(aqm, stringsAsFactors = FALSE, fill = TRUE) %>% 
    dplyr::filter(rank == 1)
  aqm_groups <- aqm %>% 
    mutate(groups_01 = ifelse(class == "Bivalvia", "Bivalvia",
                       ifelse(class == "Cephalopoda", "Cephalopoda", 
                       ifelse(class == "Polyplacophora", "Polyplacophora", 
                       ifelse(order == "Cetacea", "Cetacea", 
                       ifelse(phylum == "Echinodermata", "Echinoderms", 
                       ifelse(order == "Euphausiacea", "Euphausiids", 
                       ifelse(phylum == "Foraminifera", "Foraminifera", 
                       ifelse(class == "Gastropoda", "Gastropods", 
                       ifelse(class == "Hydrozoa", "Hydrozoans", 
                       ifelse(class == "Cubozoa", "Jellyfish", 
                       ifelse(class == "Scyphozoa", "Jellyfish", 
                       ifelse(class == "Anthozoa", "Corals", 
                       ifelse(family == "Odobenidae", "Pinnipeds", 
                       ifelse(family == "Otariidae", "Pinnipeds", 
                       ifelse(family == "Phocidae", "Pinnipeds", 
                       ifelse(class == "Reptilia", "Reptiles", 
                       ifelse(class == "Elasmobranchii", "Sharks - Rays", 
                       ifelse(phylum == "Porifera", "Sponges", 
                       ifelse(class == "Appendicularia", "Tunicates", 
                       ifelse(class == "Ascidiacea", "Tunicates", 
                       ifelse(class == "Thaliacea", "Tunicates", 
                       ifelse(phylum == "Acanthocephala", "Sea worms", 
                       ifelse(phylum == "Annelida", "Sea worms", 
                       ifelse(class == "Aplacophora", "Sea worms", 
                       ifelse(phylum == "Chaetognatha", "Sea worms", 
                       ifelse(phylum == "Cycliophora", "Sea worms", 
                       ifelse(phylum == "Kamptozoa", "Sea worms", 
                       ifelse(phylum == "Gastrotricha", "Sea worms", 
                       ifelse(phylum == "Gnathostomulida", "Sea worms", 
                       ifelse(phylum == "Hemichordata", "Sea worms", 
                       ifelse(phylum == "Kinorhyncha", "Sea worms", 
                       ifelse(phylum == "Loricifera", "Sea worms", 
                       ifelse(phylum == "Nematoda", "Sea worms", 
                       ifelse(phylum == "Nemertea", "Sea worms", 
                       ifelse(phylum == "Phoronida", "Sea worms", 
                       ifelse(phylum == "Platyhelminthes", "Sea worms", 
                       ifelse(phylum == "Priapulida", "Sea worms", 
                       ifelse(phylum == "Sipuncula", "Sea worms", 
                       ifelse(family == "Teredinidae", "Sea worms", 
                       ifelse(phylum == "Xenacoelomorpha", "Sea worms", 
                       ifelse(order == "Pantopoda", "Sea spiders", 
                       ifelse(phylum == "Chlorophyta", "Green algae",
                       ifelse(phylum == "Ochrophyta", "Brown algae", 
                       ifelse(phylum == "Rhodophyta", "Red algae", 
                       ifelse(family == "Xiphiidae", "Tunas - Billfishes", 
                       ifelse(order == "Perciformes" & family == "Pristiophoridae", "Tunas - Billfishes", 
                       ifelse(order == "Perciformes" & family == "Scombridae", "Tunas - Billfishes", "Other"))))))))))))))))))))))))))))))))))))))))))))))))
  aqm_groups <- aqm_groups %>% 
    dplyr::mutate(groups_01 = ifelse(class == "Actinopterygii" & groups_01 == "Other", "Bony fish", 
                              ifelse(class == "Sarcopterygii" & groups_01 == "Other", "Bony fish", 
                              ifelse(class == "Branchiopoda" & groups_01 == "Other", "Crustaceans", 
                              ifelse(class == "Cephalocarida" & groups_01 == "Other", "Crustaceans",
                              ifelse(class == "Maxillopoda" & groups_01 == "Other", "Crustaceans",
                              ifelse(class == "Ostracoda" & groups_01 == "Other", "Crustaceans",
                              ifelse(class == "Malacostraca" & groups_01 == "Other", "Crustaceans", groups_01)))))))) %>% 
    dplyr::select(speciesID, groups_01) # add the line with geom features here to create a new "bio_groups"
  geomF <- tibble(speciesID = c("Basins", "Bridges", "Canyons", "Escarpments", "Fans", "Guyots",
                                "Plateaus", "Ridges", "Seamounts", "Sills", "Trenches", "Troughs"), 
                  groups_01 = "Geomorphic")
  
  aqm_groups <- rbind(aqm_groups, geomF)
  
  # Reading no regret df solution
    sol <- readRDS(solution) %>% 
      dplyr::rename(pu = id) %>% 
      dplyr::rename(solution = solutionAll)
  # Species taxonomic information among planning units
    sps_df <- fread(features) %>% 
      dplyr::mutate(speciesID = str_split(string = feature_names, pattern = "_", simplify = TRUE)[,1]) %>%
      dplyr::arrange(pu) %>% 
      dplyr::select(pu, speciesID)
  # 
    gL <- vector("list", length = length(pu_all))
    for(i in 1:ncol(pu_all)) {
      d1 <- pu_all %>% 
        dplyr::select(colnames(pu_all)[i]) %>% 
        dplyr::mutate(solution  = sol$solution) %>% 
        dplyr::rename(pu = colnames(pu_all)[i]) %>% 
        dplyr::mutate(puF = sol$pu)
      d2 <- dplyr::left_join(x = d1, y = sps_df,  by = "pu")
      gL[[i]] <- d2 %>% 
        dplyr::select(puF, solution, speciesID) %>% 
        dplyr::rename(pu = puF) %>% 
        dplyr::filter(solution != 0)
      }
      
    species <- data.table::rbindlist(gL, use.names = TRUE) %>% 
      dplyr::group_by(speciesID) %>% 
      dplyr::summarise(cells = n()) %>% 
      dplyr::mutate(rep_target = cells/(length(unique(gL[[1]]$pu))*ncol(pu_all))*100) %>% 
      dplyr::arrange(-rep_target) %>% 
      ungroup() %>% 
      na.omit()
    
    ff <- dplyr::left_join(x = species, y = aqm_groups,  by = "speciesID") %>% # on average this group is at least in XXX planning unit among all the pl
      dplyr::group_by(groups_01) %>% 
      dplyr::summarise(value = mean(rep_target)) %>% 
      ungroup() %>% 
      dplyr::arrange(-value) %>% 
      dplyr::mutate(group = unlist(str_split(basename(solution), "_"))[2]) %>% 
      dplyr::relocate(groups_01, group, value) %>% 
      dplyr::rename(individual = groups_01)
    
    return(ff)
  }
  
####################################################################################
####### 3.- Spiral Plot Per Ocean Layer
####################################################################################
# 
  PlotSpiral <- function(data) {
    
    library(data.table)
    library(ggplot2)
    library(patchwork)
    library(dplyr)
    library(stringr)
    
    data2 <- data %>% 
      data.frame() %>% 
      mutate(individual = factor(individual), group = factor(group))    
    
  # Set a number of 'empty bar' to add at the end of each group
    empty_bar <- 3
    to_add <- data.frame(matrix(NA, empty_bar*nlevels(data2$group), ncol(data2)))
    colnames(to_add) <- colnames(data2)
    to_add$group <- rep(levels(data2$group), each = empty_bar)
    data2 <- rbind(data2, to_add)
    data2 <- data2 %>% 
      arrange(group)
    data2$id <- seq(1, nrow(data2))
  # Get the name and the y position of each label
    label_data <- data2
    number_of_bar <- nrow(label_data)
    angle <- 90 - 360 * (label_data$id - 0.5)/number_of_bar # substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
    label_data$hjust <- ifelse(angle < -90, 1, 0)
    label_data$angle <- ifelse(angle < -90, angle + 180, angle)
  # Prepare a data frame for base lines
    base_data <- data2 %>% 
      group_by(group) %>% 
      summarize(start = min(id), end = max(id) - empty_bar) %>% 
      rowwise() %>% 
      mutate(title = mean(c(start, end)))
  # Prepare a data frame for grid (scales)
    grid_data <- base_data
    grid_data$end <- grid_data$end[c(nrow(grid_data), 1:nrow(grid_data)-1)] + 1
    grid_data$start <- grid_data$start - 1
    grid_data <- grid_data[-1,]
  # Final plot for NO-Regret networks of MPAs
    p2 <- ggplot(data2, aes(x = as.factor(id), y = value, fill = group)) +
      geom_bar(aes(x = as.factor(id), y = value, fill = group), stat = "identity", alpha = 0.5) +
      # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
      geom_segment(data = grid_data, aes(x = end, y = 50, xend = start, yend = 50), colour = "grey", alpha = 1, size = 0.3 , inherit.aes = FALSE ) +
      geom_segment(data = grid_data, aes(x = end, y = 40, xend = start, yend = 40), colour = "grey", alpha = 1, size = 0.3 , inherit.aes = FALSE ) +
      geom_segment(data = grid_data, aes(x = end, y = 30, xend = start, yend = 30), colour = "grey", alpha = 1, size = 0.3 , inherit.aes = FALSE ) +
      geom_segment(data = grid_data, aes(x = end, y = 20, xend = start, yend = 20), colour = "grey", alpha = 1, size = 0.3 , inherit.aes = FALSE ) +
      geom_segment(data = grid_data, aes(x = end, y = 10, xend = start, yend = 10), colour = "grey", alpha = 1, size = 0.3 , inherit.aes = FALSE ) +
      # Add text showing the value of each 100/75/50/25 lines
      annotate("text", x = rep(max(data2$id),5), y = c(10, 20, 30, 40, 50), label = c("10", "20", "30", "40", 50) , color = "grey", size = 5 , angle = 0, fontface = "bold", hjust = 1) +
      geom_bar(aes(x = as.factor(id), y = value, fill = group), stat = "identity", alpha = 0.5) +
      ylim(-100, 120) +
      theme_minimal() +
      scale_fill_manual(values = c("#1a9850", "#de2d26", "#fdae61", "#2c7fb8")) +
      theme(legend.position = "none",
            axis.text = element_blank(),
            axis.title = element_blank(),
            panel.grid = element_blank(),
            plot.margin = unit(rep(-1,4), "cm")) +
      coord_polar() + 
      geom_text(data = label_data, aes(x = id, y = value + 10, label = individual, hjust = hjust), color = "black", fontface = "bold", alpha = 0.6, size = 3.5, angle = label_data$angle, inherit.aes = FALSE ) +
      # Add base line information
      geom_segment(data = base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha = 0.8, size = 0.6 , inherit.aes = FALSE)  +
      geom_text(data = base_data, aes(x = title, y = -18, label = group), hjust = c(1, 1, 1, 0), colour = "black", alpha = 0.8, size = 4, fontface = "bold", inherit.aes = FALSE)
    
    return(p2)
  }

####################################################################################
####### 4.- Spiral Plot Vertical
####################################################################################
# 
  PlotSpiral_V <- function(data) {
    
    library(data.table)
    library(ggplot2)
    library(patchwork)
    library(dplyr)
    library(stringr)

    data2 <- data %>% 
      data.frame() %>% 
      mutate(individual = factor(individual), group = factor(group))    
    
  # Set a number of 'empty bar' to add at the end of each group
    empty_bar <- 3
    to_add <- data.frame(matrix(NA, empty_bar*nlevels(data2$group), ncol(data2)))
    colnames(to_add) <- colnames(data2)
    to_add$group <- rep(levels(data2$group), each = empty_bar)
    data2 <- rbind(data2, to_add)
    data2 <- data2 %>% 
      arrange(group)
    data2$id <- seq(1, nrow(data2))
  # Get the name and the y position of each label
    label_data <- data2
    number_of_bar <- nrow(label_data)
    angle <- 90 - 360 * (label_data$id - 0.5)/number_of_bar # substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
    label_data$hjust <- ifelse(angle < -90, 1, 0)
    label_data$angle <- ifelse(angle < -90, angle + 180, angle)
  # Prepare a data frame for base lines
    base_data <- data2 %>% 
      group_by(group) %>% 
      summarize(start = min(id), end = max(id) - empty_bar) %>% 
      rowwise() %>% 
      mutate(title = mean(c(start, end)))
  # Prepare a data frame for grid (scales)
    grid_data <- base_data
    grid_data$end <- grid_data$end[c(nrow(grid_data), 1:nrow(grid_data)-1)] + 1
    grid_data$start <- grid_data$start - 1
    grid_data <- grid_data[-1,]
    # Final plot for NO-Regret networks of MPAs
    p2 <- ggplot(data2, aes(x = as.factor(id), y = value, fill = group)) +
      geom_bar(aes(x = as.factor(id), y = value, fill = group), stat = "identity", alpha = 0.5) +
      # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
      geom_segment(data = grid_data, aes(x = end, y = 40, xend = start, yend = 40), colour = "grey", alpha = 1, size = 0.3 , inherit.aes = FALSE ) +
      geom_segment(data = grid_data, aes(x = end, y = 30, xend = start, yend = 30), colour = "grey", alpha = 1, size = 0.3 , inherit.aes = FALSE ) +
      geom_segment(data = grid_data, aes(x = end, y = 20, xend = start, yend = 20), colour = "grey", alpha = 1, size = 0.3 , inherit.aes = FALSE ) +
      geom_segment(data = grid_data, aes(x = end, y = 10, xend = start, yend = 10), colour = "grey", alpha = 1, size = 0.3 , inherit.aes = FALSE ) +
      # Add text showing the value of each 100/75/50/25 lines
      annotate("text", x = rep(max(data2$id),4), y = c(10, 20, 30, 40), label = c("10", "20", "30", "40") , color = "grey", size = 5 , angle = 0, fontface = "bold", hjust = 1) +
      geom_bar(aes(x = as.factor(id), y = value, fill = group), stat = "identity", alpha = 0.5) +
      ylim(-100, 120) +
      theme_minimal() +
      scale_fill_manual(values = c("#ec7014", "#0570b0")) +
      theme(legend.position = "none",
            axis.text = element_blank(),
            axis.title = element_blank(),
            panel.grid = element_blank(),
            plot.margin = unit(rep(-1,4), "cm")) +
      coord_polar() + 
      geom_text(data = label_data, aes(x = id, y = value + 10, label = individual, hjust = hjust), color = "black", fontface = "bold", alpha = 0.6, size = 3.5, angle = label_data$angle, inherit.aes = FALSE ) +
      # Add base line information
      geom_segment(data = base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha = 0.8, size = 0.6 , inherit.aes = FALSE)  +
      geom_text(data = base_data, aes(x = title, y = -18, label = group), hjust = c(1, 0), colour = "black", alpha = 0.8, size = 4, fontface = "bold", inherit.aes = FALSE)# +
    
    return(p2)
  }
