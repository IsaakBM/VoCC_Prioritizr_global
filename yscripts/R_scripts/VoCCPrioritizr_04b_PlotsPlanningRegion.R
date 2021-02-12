# This code was written by Isaac Brito-Morales (i.britomorales@uq.edu.au)
# Please do not distribute this code without permission.
# NO GUARANTEES THAT CODE IS CORRECT
# Caveat Emptor!

plot_solutions <- function(path, outdir) {
  
  library(sf)
  library(raster)
  library(dplyr)
  library(ggplot2)
  library(patchwork)
  library(rnaturalearth)
  library(rnaturalearthdata)
  library(RColorBrewer)
  library(foreach)
  library(doParallel)
  
  # Which directories for climate prioritization scenarios?
    dir.layers <- paste(list.dirs(path = path, full.names = TRUE, recursive = FALSE), sep = "/")
  # Loop for every directory
    # Begin the parallel structure
      UseCores <- 3 #24
      cl <- makeCluster(UseCores)  
      registerDoParallel(cl)
      gglits <- vector("list", length = length(dir.layers))
      # Parallel Loop
        plots_list <- foreach(i = 1:length(dir.layers), .packages = c("sf", "raster", "dplyr", "ggplot2", "rnaturalearth", "rnaturalearthdata", "RColorBrewer")) %dopar% {
          # 
            files_solution <- list.files(path = dir.layers[i], pattern = "*Layer_.*.csv$", full.names = TRUE)
            provinces_csv <- read.csv(list.files(path = dir.layers[i], pattern = "*pus-.*.csv$", full.names = TRUE)) %>% 
              dplyr::arrange(layer)
            pu_shpfile <- st_read(list.files(path = dir.layers[i], pattern = ".shp", full.names = TRUE))
          # Some manipulation
            single <- read.csv(files_solution)
            if(ncol(single) > 6) { # more than 1 solution == freq selection
            sol_csv <- single %>% 
              dplyr::mutate(freq_sel = rowSums(single[, 6:ncol(single)])) %>% 
              dplyr::select(id, cost, freq_sel) %>% 
              dplyr::mutate(freq_cat = (freq_sel)/(length(6:ncol(single)))*100)
            freq_base <- sol_csv %>% 
              mutate(freq_cat2 = ifelse(freq_cat == 0, 1, 
                                       ifelse(freq_cat > 0 & freq_cat <= 25, 2,
                                              ifelse(freq_cat > 25 & freq_cat <= 50, 3, 
                                                     ifelse(freq_cat > 50 & freq_cat <= 75, 4, 5)))))
            
          # Get the freq solutions from the corresponding planning unit shapefile
            best_freq_sol <- pu_shpfile[pu_shpfile$id %in% freq_base$id, ] %>% 
              mutate(freq_cat = freq_base$freq_cat2)
          # Creating the PROVINCES shapefile based on ocean layer
            provinces_shp <- pu_shpfile %>%
              dplyr::mutate(provinces = provinces_csv$province) %>% 
              base::transform(id = as.numeric(factor(provinces))) %>% 
              dplyr::group_by(id) %>% 
              dplyr::summarise(prov = sum(id, do_union = TRUE))
            
          # Define themes to plot 
            theme_opts3 <- list(theme(panel.grid.minor = element_blank(),
                                      panel.grid.major = element_blank(),
                                      panel.background = element_blank(),
                                      plot.background = element_rect(fill = "white"),
                                      panel.border = element_blank(),
                                      axis.line = element_blank(),
                                      axis.text.x = element_blank(),
                                      axis.text.y = element_blank(),
                                      axis.ticks = element_blank(),
                                      axis.ticks.length = unit(.25, "cm"), 
                                      axis.title.x = element_blank(),
                                      axis.title.y = element_blank(),
                                      plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
                                      legend.title = element_text(colour = "black", face = "bold", size = 15),
                                      legend.text = element_text(colour = "black", face = "bold", size = 10), 
                                      legend.key.height = unit(2, "cm"),
                                      legend.key.width = unit(0.9, "cm"),
                                      plot.tag = element_text(size = 25, face = "bold")))
          # Color Palette, World borders and Legend
            pal0 <- brewer.pal(length(unique(best_freq_sol$freq_cat)) - 1, "Greens")
            pal <- c("#deebf7", pal0)
            world_sf <- ne_countries(scale = "medium", returnclass = "sf") 
            ranges <- c("0", "< 25", "25 - 50", "50 - 75", "> 75")
          # Plot
            gglits[[i]] <- ggplot() + 
              geom_sf(data = best_freq_sol, aes(group = as.factor(freq_cat), fill = as.factor(freq_cat)), color = NA) +
              geom_sf(data = provinces_shp, fill = NA) +
              geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
              scale_fill_manual(values = pal,
                                name = "Selection Frequency (%)",
                                labels = ranges) +
              ggtitle(basename(sub(pattern = "*.csv", "", files_solution[i]))) +
              theme_opts3
            
            } else { # one solution == yes or no
              
                sol_csv <- single %>% 
                  dplyr::mutate(freq_sel = single[, 6]) %>% 
                  dplyr::select(id, cost, freq_sel)
                freq_base <- sol_csv %>% 
                  dplyr::mutate(freq_cat2 = ifelse(freq_sel == 0, 1, 2)) %>% 
                  dplyr::arrange(id)
              
              # Get the freq solutions from the corresponding planning unit shapefile
                best_freq_sol <- pu_shpfile[pu_shpfile$id %in% freq_base$id, ] %>% 
                  mutate(freq_cat = freq_base$freq_cat2)
              # Creating the PROVINCES shapefile based on ocean layer
                provinces_shp <- pu_shpfile %>%
                  dplyr::mutate(provinces = provinces_csv$province) %>% 
                  base::transform(id = as.numeric(factor(provinces))) %>% 
                  dplyr::group_by(id) %>% 
                  dplyr::summarise(prov = sum(id, do_union = TRUE))
              
              # Define themes to plot 
                # Defining themes
                theme_opts3 <- list(theme(panel.grid.minor = element_blank(),
                                          panel.grid.major = element_blank(),
                                          panel.background = element_blank(),
                                          plot.background = element_rect(fill = "white"),
                                          panel.border = element_blank(),
                                          axis.line = element_blank(),
                                          axis.text.x = element_blank(),
                                          axis.text.y = element_blank(),
                                          axis.ticks = element_blank(),
                                          axis.ticks.length = unit(.25, "cm"), 
                                          axis.title.x = element_blank(),
                                          axis.title.y = element_blank(),
                                          plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
                                          legend.title = element_text(colour = "black", face = "bold", size = 15),
                                          legend.text = element_text(colour = "black", face = "bold", size = 10), 
                                          legend.key.height = unit(2, "cm"),
                                          legend.key.width = unit(0.9, "cm"),
                                          plot.tag = element_text(size = 25, face = "bold")))
              # Color Palette, World borders and Legend
                pal <- c("#deebf7", "#31a354")
                world_sf <- ne_countries(scale = "medium", returnclass = "sf") 
                ranges <- c("0", "1")
              # Plot
                gglits[[i]] <- ggplot() + 
                  geom_sf(data = best_freq_sol, aes(group = as.factor(freq_cat), fill = as.factor(freq_cat)), color = NA) +
                  geom_sf(data = provinces_shp, fill = NA) +
                  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
                  scale_fill_manual(values = pal,
                                    name = "Selection",
                                    labels = ranges) +
                  ggtitle(basename(sub(pattern = "*.csv", "", files_solution[i]))) +
                  theme_opts3
            }
        }
        stopCluster(cl)
        # Plotting the FINAL FIGURES
          # Defining themes
          theme_opts3 <- list(theme(panel.grid.minor = element_blank(),
                                    panel.grid.major = element_blank(),
                                    panel.background = element_blank(),
                                    plot.background = element_rect(fill = "white"),
                                    panel.border = element_blank(),
                                    axis.line = element_blank(),
                                    axis.text.x = element_blank(),
                                    axis.text.y = element_blank(),
                                    axis.ticks = element_blank(),
                                    axis.ticks.length = unit(.25, "cm"),
                                    axis.title.x = element_blank(),
                                    axis.title.y = element_blank(),
                                    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
                                    legend.title = element_text(colour = "black", face = "bold", size = 15),
                                    legend.text = element_text(colour = "black", face = "bold", size = 10),
                                    legend.key.height = unit(2, "cm"),
                                    legend.key.width = unit(0.9, "cm"),
                                    plot.tag = element_text(size = 25, face = "bold")))
          # CALIBRATION PLOTS
            p3 <-   (plots_list[[1]] + plots_list[[2]] + plots_list[[3]] + plots_list[[4]]) +
              plot_layout(guides = "collect") +
              plot_annotation(tag_prefix = "",
                              tag_levels = "A",
                              tag_suffix = ".",) +
              theme_opts3 +
              ggsave(paste(outdir, paste("calibration-solutions", ".pdf", sep = ""), sep = ""), width = 40, height = 20, dpi = 300)
}

system.time(test <- plot_solutions(path = "prioritization_zblm-cal_rce-vocc040",
                                   outdir = "prioritization_zblm-cal_rce-vocc040/"))

# system.time(plot_solutions(path = "/QRISdata/Q1216/BritoMorales/Project04b/prioritization_zblm-cal_rce-vocc050_logcost", 
#                            outdir = "/QRISdata/Q1216/BritoMorales/Project04b/prioritization_zblm-cal_rce-vocc050_logcost/"))



