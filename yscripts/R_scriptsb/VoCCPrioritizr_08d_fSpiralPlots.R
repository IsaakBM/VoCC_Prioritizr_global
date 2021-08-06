# This code was written by Isaac Brito-Morales (i.britomorales@uq.edu.au)
# Please do not distribute this code without permission.
# NO GUARANTEES THAT CODE IS CORRECT
# Caveat Emptor!

plot_targets <- function(path, aquamaps_data, outdir) {
  
  library(data.table)
  library(ggplot2)
  library(patchwork)
  library(dplyr)
  library(stringr)
  library(doParallel)
  library(foreach)
  
  # Which directories?
    dir.layers <- paste(list.dirs(path = path, full.names = TRUE, recursive = FALSE), sep = "/")
    y_axis <- c("epipelagic", "mesopelagic", "bathyabyssopelagic")
  # No regret ocean layers directory
    ep_ssp <- dir.layers[str_detect(string = basename(dir.layers), pattern = "Epi") == TRUE & str_detect(string = basename(dir.layers), pattern = "ssp") == TRUE]
    mp_ssp <- dir.layers[str_detect(string = basename(dir.layers), pattern = "Meso") == TRUE & str_detect(string = basename(dir.layers), pattern = "ssp") == TRUE]
    bap_ssp <- dir.layers[str_detect(string = basename(dir.layers), pattern = "Bathy") == TRUE & str_detect(string = basename(dir.layers), pattern = "ssp") == TRUE]
    vertical_ssp <- dir.layers[str_detect(string = basename(dir.layers), pattern = "Verti") == TRUE]
    olayers_ssp_list <- list(ep_ssp, mp_ssp, bap_ssp)
  # No CC ocean layers directory
    ep_nocc <- dir.layers[str_detect(string = basename(dir.layers), pattern = "Epi") == TRUE & str_detect(string = basename(dir.layers), pattern = "ssp") == FALSE]
    mp_nocc <- dir.layers[str_detect(string = basename(dir.layers), pattern = "Meso") == TRUE & str_detect(string = basename(dir.layers), pattern = "ssp") == FALSE]
    bap_nocc <- dir.layers[str_detect(string = basename(dir.layers), pattern = "Bathy") == TRUE & str_detect(string = basename(dir.layers), pattern = "ssp") == FALSE]
    olayers_nocc_list <- list(ep_nocc, mp_nocc, bap_nocc)
  # Read the AquaMaps data (species occur .csv) and extract species' name according to the code
    aqm <- fread(aquamaps_data, stringsAsFactors = FALSE, fill = TRUE) %>% 
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
      dplyr::select(speciesID, groups_01)
    
    
    # No regret ocean layers directory
      # Begin the parallel structure      
      cores  <-  3
      cl <- makeCluster(cores)
      registerDoParallel(cl)
      dflist01 <- vector("list", length = length(olayers_ssp_list))
      dflist_01 <- foreach(i = 1:length(olayers_ssp_list), .packages = c("dplyr", "stringr", "data.table")) %dopar% {
        # List of files
          files_sps <- list.files(path = olayers_ssp_list[[i]], pattern = "*lagic.csv$", full.names = TRUE)
          files_no_regret <- list.files(path = olayers_ssp_list[[i]], pattern = "*sps.*.csv$", full.names = TRUE)
        # Species taxonomic information among planning units
          sps_df <- fread(files_sps) %>% 
            dplyr::mutate(speciesID = str_split(string = feature_names, pattern = "_", simplify = TRUE)[,1]) %>%
            dplyr::arrange(pu) %>% 
            dplyr::select(pu, speciesID)
        # Planning units climate-smart network
          no_regrets_df <- fread(files_no_regret) %>% 
            dplyr::select(-V1)
        # Merging "Species taxonomic information among planning units" with "Planning units climate-smart network"
          species <- dplyr::left_join(x = sps_df, y = aqm_groups,  by = "speciesID") %>% 
            dplyr::rename(id = pu)
        #
          final_df <- dplyr::left_join(x = species, y = no_regrets_df,  by = "id") %>% 
            na.omit() %>% 
            dplyr::group_by(speciesID) %>%
            dplyr::summarise(cells = n()) %>% 
            dplyr::mutate(rep_target = round((cells/length(unique(no_regrets_df$id)))*100, digits = 4)) %>% 
            dplyr::arrange(-rep_target) %>% 
            ungroup()
            final_df <- dplyr::left_join(x = final_df, y = aqm_groups,  by = "speciesID") %>%
              dplyr::group_by(groups_01) %>% 
              dplyr::summarise(value = mean(rep_target)) %>% 
              ungroup() %>% 
              dplyr::arrange(-value) %>% 
              dplyr::mutate(group = y_axis[i]) %>% 
              dplyr::relocate(groups_01, group, value) %>% 
              dplyr::rename(individual = groups_01)
            dflist01[[i]] <- final_df
      }
      stopCluster(cl)
      # No regret ocean Vertical
        sps <- lapply(olayers_ssp_list, function(x){
          # List of files
            files_sps <- list.files(path = x, pattern = "*lagic.csv$", full.names = TRUE)
            files_no_regret <- list.files(path = x, pattern = "*sps.*.csv$", full.names = TRUE)
          # Species taxonomic information among planning units
            sps_df <- fread(files_sps) %>% 
              dplyr::mutate(speciesID = str_split(string = feature_names, pattern = "_", simplify = TRUE)[,1]) %>%
              dplyr::arrange(pu) %>% 
              dplyr::select(pu, speciesID)
          # Merging "Species taxonomic information among planning units" with "Planning units climate-smart network"
            species <- dplyr::left_join(x = sps_df, y = aqm_groups,  by = "speciesID") %>% 
              dplyr::rename(id = pu)})
        #
        vertical_df <- fread(list.files(path = vertical_ssp, pattern = "*sps.*.csv$", full.names = TRUE)) %>% 
          dplyr::select(-V1) %>% 
          dplyr::arrange()
          # Getting epipelagic
            final_all_ep <- vertical_df %>% 
              dplyr::select(ep_id, olayer) %>% 
              dplyr::rename(id = ep_id) %>% 
              dplyr::arrange(id) %>% 
              dplyr::left_join(x = sps[[1]], by = "id") %>% 
              na.omit()
          # Getting mesopelagic
            final_all_mp <- vertical_df %>% 
              dplyr::select(mp_id, olayer) %>% 
              dplyr::rename(id = mp_id) %>% 
              dplyr::arrange(id) %>% 
              dplyr::left_join(x = sps[[2]], by = "id") %>% 
              na.omit()
          # Getting bathy
            final_all_bap <- vertical_df %>% 
              dplyr::select(bap_id , olayer) %>% 
              dplyr::rename(id = bap_id ) %>% 
              dplyr::arrange(id) %>% 
              dplyr::left_join(x = sps[[3]], by = "id") %>% 
              na.omit()
            # Mergin all three layers species information within planning units
              vertical_final <- rbind(final_all_ep, final_all_mp, final_all_bap)
          # Creating the final % targets for vertical
            final_vertical <- vertical_final %>% 
              na.omit() %>%
              dplyr::group_by(speciesID) %>%
              dplyr::summarise(cells = n()) %>% 
              dplyr::mutate(rep_target = round((cells/(length(unique(final_all_ep$id))*3))*100, digits = 4)) %>% 
              dplyr::arrange(-rep_target) %>% 
              ungroup()
              final_vertical <- dplyr::left_join(x = final_vertical, y = aqm_groups,  by = "speciesID") %>%
                dplyr::group_by(groups_01) %>% 
                dplyr::summarise(value = mean(rep_target)) %>% 
                ungroup() %>% 
                dplyr::arrange(-value) %>% 
                dplyr::mutate(group = "Vertical") %>% 
                dplyr::relocate(groups_01, group, value) %>% 
                dplyr::rename(individual = groups_01)
      # Final Data frame for plotting
        data <- rbind(do.call(rbind, dflist_01), final_vertical) %>% 
          data.frame() %>% 
          mutate(individual = factor(individual), group = factor(group))
        
      # PLOTTING CIRCULAR HISTOGRAM FOR NO REGRET INFORMATION NETWORK % TARGET (source code from https://www.r-graph-gallery.com/297-circular-barplot-with-groups.html)
        # Set a number of 'empty bar' to add at the end of each group
          empty_bar <- 3
          to_add <- data.frame(matrix(NA, empty_bar*nlevels(data$group), ncol(data)))
          colnames(to_add) <- colnames(data)
          to_add$group <- rep(levels(data$group), each = empty_bar)
          data <- rbind(data, to_add)
          data <- data %>% 
            arrange(group)
          data$id <- seq(1, nrow(data))
        
        # Get the name and the y position of each label
          label_data <- data
          number_of_bar <- nrow(label_data)
          angle <- 90 - 360 * (label_data$id - 0.5)/number_of_bar # substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
          label_data$hjust <- ifelse(angle < -90, 1, 0)
          label_data$angle <- ifelse(angle < -90, angle + 180, angle)
        
        # Prepare a data frame for base lines
          base_data <- data %>% 
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
          p1 <- ggplot(data, aes(x = as.factor(id), y = value, fill = group)) +
            geom_bar(aes(x = as.factor(id), y = value, fill = group), stat = "identity", alpha = 0.5) +
            # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
            geom_segment(data = grid_data, aes(x = end, y = 40, xend = start, yend = 40), colour = "grey", alpha = 1, size = 0.3 , inherit.aes = FALSE ) +
            geom_segment(data = grid_data, aes(x = end, y = 30, xend = start, yend = 30), colour = "grey", alpha = 1, size = 0.3 , inherit.aes = FALSE ) +
            geom_segment(data = grid_data, aes(x = end, y = 20, xend = start, yend = 20), colour = "grey", alpha = 1, size = 0.3 , inherit.aes = FALSE ) +
            geom_segment(data = grid_data, aes(x = end, y = 10, xend = start, yend = 10), colour = "grey", alpha = 1, size = 0.3 , inherit.aes = FALSE ) +
            # Add text showing the value of each 100/75/50/25 lines
            annotate("text", x = rep(max(data$id),4), y = c(10, 20, 30, 40), label = c("10", "20", "30", "40") , color = "grey", size = 3 , angle = 0, fontface = "bold", hjust = 1) +
            geom_bar(aes(x = as.factor(id), y = value, fill = group), stat = "identity", alpha = 0.5) +
            ylim(-100, 120) +
            theme_minimal() +
            scale_fill_manual(values = c("#1a9850", "#de2d26", "#fdae61", "#2c7fb8")) + # bathy, epi, meso, all?
            theme(legend.position = "none",
                  axis.text = element_blank(),
                  axis.title = element_blank(),
                  panel.grid = element_blank(),
                  plot.margin = unit(rep(-1,4), "cm")) +
            coord_polar() + 
            geom_text(data = label_data, aes(x = id, y = value + 10, label = individual, hjust = hjust), color = "black", fontface = "bold", alpha = 0.6, size = 2.5, angle = label_data$angle, inherit.aes = FALSE ) +
            # Add base line information
            geom_segment(data = base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha = 0.8, size = 0.6 , inherit.aes = FALSE)  +
            geom_text(data = base_data, aes(x = title, y = -18, label = group), hjust = c(1, 1, 0, 0), colour = "black", alpha = 0.8, size = 4, fontface = "bold", inherit.aes = FALSE) +
            ggsave(paste(outdir, paste("spiral-diagram-noregret", ".pdf", sep = ""), sep = ""), width = 8, height = 8, dpi = 300)
        

    # No regret ocean layers directory
      # Begin the parallel structure      
        cores  <-  3
        cl <- makeCluster(cores)
        registerDoParallel(cl)
        dflist02 <- vector("list", length = length(olayers_nocc_list))
        dflist_02 <- foreach(j = 1:length(olayers_nocc_list), .packages = c("dplyr", "stringr", "data.table")) %dopar% {
          # List of files
            files_sps <- list.files(path = olayers_nocc_list[[j]], pattern = "*lagic.csv$", full.names = TRUE)
            files_no_regret <- list.files(path = olayers_nocc_list[[j]], pattern = "*sps.*.csv$", full.names = TRUE)
          # Species taxonomic information among planning units
            sps_df <- fread(files_sps) %>% 
              dplyr::mutate(speciesID = str_split(string = feature_names, pattern = "_", simplify = TRUE)[,1]) %>%
              dplyr::arrange(pu) %>% 
              dplyr::select(pu, speciesID)
          # Planning units no climate smart network
            no_regrets_df <- fread(files_no_regret) %>% 
              dplyr::select(-V1)
          # Merging "Species taxonomic information among planning units" with "Planning units no climate smart network"
            species <- dplyr::left_join(x = sps_df, y = aqm_groups,  by = "speciesID") %>% 
              dplyr::rename(id = pu)
          #
            final_df <- dplyr::left_join(x = species, y = no_regrets_df,  by = "id") %>% 
              na.omit() %>% 
              dplyr::group_by(speciesID) %>%
              dplyr::summarise(cells = n()) %>% 
              dplyr::mutate(rep_target = round((cells/length(unique(no_regrets_df$id)))*100, digits = 4)) %>% 
              dplyr::arrange(-rep_target) %>% 
              ungroup()
            final_df <- dplyr::left_join(x = final_df, y = aqm_groups,  by = "speciesID") %>%
              dplyr::group_by(groups_01) %>% 
              dplyr::summarise(value = mean(rep_target)) %>% 
              ungroup() %>% 
              dplyr::arrange(-value) %>% 
              dplyr::mutate(group = y_axis[j]) %>% 
              dplyr::relocate(groups_01, group, value) %>% 
              dplyr::rename(individual = groups_01)
            dflist02[[j]] <- final_df
          }
          stopCluster(cl)
          data2 <- do.call(rbind, dflist_02) %>% 
            data.frame() %>% 
            mutate(individual = factor(individual), group = factor(group))
          
          # PLOTTING CIRCULAR HISTOGRAM FOR NO CC INFORMATION NETWORK % TARGET
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
                    annotate("text", x = rep(max(data2$id),4), y = c(10, 20, 30, 40), label = c("10", "20", "30", "40") , color = "grey", size = 3 , angle = 0, fontface = "bold", hjust = 1) +
                    geom_bar(aes(x = as.factor(id), y = value, fill = group), stat = "identity", alpha = 0.5) +
                    ylim(-100, 120) +
                    theme_minimal() +
                    scale_fill_manual(values = c("#1a9850", "#de2d26", "#fdae61")) + # bathy, epi, meso
                    theme(legend.position = "none",
                          axis.text = element_blank(),
                          axis.title = element_blank(),
                          panel.grid = element_blank(),
                          plot.margin = unit(rep(-1,4), "cm")) +
                    coord_polar() + 
                    geom_text(data = label_data, aes(x = id, y = value + 10, label = individual, hjust = hjust), color = "black", fontface = "bold", alpha = 0.6, size = 2.5, angle = label_data$angle, inherit.aes = FALSE ) +
                  # Add base line information
                    geom_segment(data = base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha = 0.8, size = 0.6 , inherit.aes = FALSE)  +
                    geom_text(data = base_data, aes(x = title, y = -18, label = group), hjust = c(1, 1, 0), colour = "black", alpha = 0.8, size = 4, fontface = "bold", inherit.aes = FALSE) +
                    ggsave(paste(outdir, paste("spiral-diagram-noCC", ".pdf", sep = ""), sep = ""), width = 8, height = 8, dpi = 300)
          
        # PATCHWORK Figure 
          p3 <-   (p2 | p1) +
            plot_annotation(tag_prefix = "",
                            tag_levels = "a",
                            tag_suffix = ".",) +
            ggsave(paste(outdir, paste("spiral-diagram-patch", ".pdf", sep = ""), sep = ""), width = 16, height = 8, dpi = 300)
}

system.time(plot_targets(path = "znoregret-network_sps", 
                         aquamaps_data = "/Users/bri273/Desktop/AquaMaps_wflow/AquaMaps/v2019a/speciesoccursum.csv", 
                         outdir = "znoregret-network_sps/"))
