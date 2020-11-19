# This code was written by Isaac Brito-Morales (i.britomorales@uq.edu.au)
# Please do not distribute this code without permission.
# NO GUARANTEES THAT CODE IS CORRECT
# Caveat Emptor!

no_regret_plots <- function(path, outdir, shp) {
  
  library(sf)
  library(raster)
  library(dplyr)
  library(ggplot2)
  library(stringr)
  library(patchwork)
  library(rnaturalearth)
  library(rnaturalearthdata)
  library(RColorBrewer)
  library(foreach)
  library(doParallel)  
  
  # Which directories for climate prioritization scenarios?
    dir.layers <- paste(list.dirs(path = path, full.names = TRUE, recursive = FALSE), sep = "/")
    dir.shp <- paste(list.dirs(path = shp, full.names = TRUE, recursive = FALSE), sep = "/")
    # 
      ep_dir <- dir.layers[str_detect(string = basename(dir.layers), pattern = "Epi") == TRUE]
      mp_dir <- dir.layers[str_detect(string = basename(dir.layers), pattern = "Meso") == TRUE]
      bap_dir <- dir.layers[str_detect(string = basename(dir.layers), pattern = "Bathy") == TRUE]
      shps <- list.files(path = dir.shp, pattern = "*.shp$", full.names = TRUE)
      olayers_list <- list(ep_dir, mp_dir, bap_dir)
    # 
      y_axis <- c("Epipelagic", "Mesopelagic", "Bathyabyssopelagic")
      
  # Loop for every directory for CLIMATE SMART AMONG SOLUTIONS FIGURE BY DEPTH LAYER [original figure 2]
    # Begin the parallel structure
      UseCores <- 5
      cl <- makeCluster(UseCores)  
      registerDoParallel(cl)
      gglist01 <- vector("list", length = length(olayers_list))
      plots_list01 <- foreach(i = 1:length(olayers_list), .packages = c("sf", "raster", "dplyr", "ggplot2", "rnaturalearth", "rnaturalearthdata", "RColorBrewer")) %dopar% {
        #
          files_solution <- list.files(path = olayers_list[[i]], pattern = "*Layer_.*.csv$", full.names = TRUE)
          provinces_csv <- read.csv(list.files(path = olayers_list[[i]], pattern = "*pus-.*.csv$", full.names = TRUE)[1]) %>% 
            dplyr::arrange(layer)
          mpas_csv <- read.csv(list.files(path = olayers_list[[i]], pattern = "*_mpas.*.csv$", full.names = TRUE)[1]) %>% 
            dplyr::filter(province != "non-categ_mpas")
          vmes_csv <- read.csv(list.files(path = olayers_list[[i]], pattern = "*_VMEs.*.csv$", full.names = TRUE)[1]) %>% 
            dplyr::filter(province != "non-categ_VMEs")
          pu_shpfile <- st_read(list.files(path = olayers_list[[i]], pattern = ".shp", full.names = TRUE)[1]) # the same for every ocean layer so that's why [1]
          
        # 
          solutions_csv <- lapply(files_solution, function(x) {
            single <- read.csv(x)
            sol_csv <- single %>% 
              dplyr::mutate(freq_sel = single[, 6]) %>% 
              dplyr::select(id, cost, freq_sel) %>% 
              dplyr::arrange(id)})
          
        # 
          ssp126 <- dplyr::left_join(x = pu_shpfile, y = solutions_csv[[1]],  by = "id") %>% 
            dplyr::mutate(solution1 = ifelse(is.na(freq_sel), 0, ifelse(freq_sel == 1, 4, freq_sel)))
          ssp245 <- dplyr::left_join(x = pu_shpfile, y = solutions_csv[[2]],  by = "id") %>% 
            dplyr::mutate(solution1 = ifelse(is.na(freq_sel), 0, ifelse(freq_sel == 1, 5, freq_sel)))
          ssp585 <- dplyr::left_join(x = pu_shpfile, y = solutions_csv[[3]],  by = "id") %>% 
            dplyr::mutate(solution1 = ifelse(is.na(freq_sel), 0, ifelse(freq_sel == 1, 6, freq_sel)))
        # 
          no_regrets01 <- pu_shpfile %>% 
            dplyr::mutate(no_regret_all = ssp126$solution1+ssp245$solution1+ssp585$solution1) %>% 
            dplyr::mutate(no_regret_all = ifelse(no_regret_all == 4, 0, 
                                                 ifelse(no_regret_all == 5, 0, 
                                                        ifelse(no_regret_all == 6, 0, no_regret_all)))) %>% 
            dplyr::filter(!id %in% unique(c(mpas_csv$layer, vmes_csv$layer)))
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
                                      axis.title.y = element_text(face = "plain", size = 25, angle = 90),
                                      plot.title = element_text(face = "plain", size = 25, hjust = 0.5),
                                      legend.title = element_text(colour = "black", face = "bold", size = 16),
                                      legend.text = element_text(colour = "black", face = "plain", size = 15), 
                                      legend.key.height = unit(1.5, "cm"),
                                      legend.key.width = unit(1.5, "cm"),
                                      plot.tag = element_text(size = 25, face = "bold")))
          # Color Palette, World borders and Legend
            pal <- c("#deebf7", "#984ea3", "#1b9e77", "#377eb8", "#e41a1c")
            world_sf <- ne_countries(scale = "medium", returnclass = "sf") 
            ranges <- c("Not selected", "SSP1-2.6 and SSP2-4.5", "SSP1-2.6 and SSP5-8.5", "SSP2-4.5 and SSP5-8.5", "All")
          # Plot
            gglist01[[i]] <- ggplot() + 
              geom_sf(data = no_regrets01, aes(group = as.factor(no_regret_all), fill = as.factor(no_regret_all)), color = NA) +
              geom_sf(data = provinces_shp, fill = NA) +
              geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
              scale_fill_manual(values = pal,
                                name = "Coherence\n across climate scenarios",
                                labels = ranges) +
              # ggtitle(main_tittles[i]) +
              # labs(y = y_axis[i]) +
              theme_opts3
      }
      stopCluster(cl)
      
  # Loop for every directory for NON CLIMATE SOLUTIONS FIGURE BY DEPTH LAYER
    # Begin the parallel structure
      UseCores <- 5
      cl <- makeCluster(UseCores)  
      registerDoParallel(cl)
      gglist02 <- vector("list", length = length(olayers_list))
      plots_list02 <- foreach(kk = 1:length(olayers_list), .packages = c("sf", "raster", "dplyr", "ggplot2", "rnaturalearth", "rnaturalearthdata", "RColorBrewer")) %dopar% {
        #
          files_solution <- list.files(path = olayers_list[[kk]], pattern = "*Layer_.*.csv$", full.names = TRUE)
          provinces_csv <- read.csv(list.files(path = olayers_list[[kk]], pattern = "*pus-.*.csv$", full.names = TRUE)[1]) %>% 
            dplyr::arrange(layer)
          mpas_csv <- read.csv(list.files(path = olayers_list[[kk]], pattern = "*_mpas.*.csv$", full.names = TRUE)[1]) %>% 
            dplyr::filter(province != "non-categ_mpas")
          vmes_csv <- read.csv(list.files(path = olayers_list[[kk]], pattern = "*_VMEs.*.csv$", full.names = TRUE)[1]) %>% 
            dplyr::filter(province != "non-categ_VMEs")
          pu_shpfile <- st_read(list.files(path = olayers_list[[kk]], pattern = ".shp", full.names = TRUE)[1]) # the same for every ocean layer so that's why [1]
        
        # 
          solutions_csv <- lapply(files_solution, function(x) {
            single <- read.csv(x)
            sol_csv <- single %>% 
              dplyr::mutate(freq_sel = single[, 6]) %>% 
              dplyr::select(id, cost, freq_sel) %>% 
              dplyr::arrange(id)}) 
        # 
          no_cc <- dplyr::left_join(x = pu_shpfile, y = solutions_csv[[4]],  by = "id") %>% 
            dplyr::filter(!id %in% unique(c(mpas_csv$layer, vmes_csv$layer)))
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
                                      axis.title.y = element_text(face = "plain", size = 25, angle = 90),
                                      plot.title = element_text(face = "plain", size = 25, hjust = 0.5),
                                      legend.title = element_text(colour = "black", face = "bold", size = 16),
                                      legend.text = element_text(colour = "black", face = "plain", size = 15), 
                                      legend.key.height = unit(1.5, "cm"),
                                      legend.key.width = unit(1.5, "cm"),
                                      legend.position = "none", 
                                      plot.tag = element_text(size = 25, face = "bold")))
          # Color Palette, World borders and Legend
            pal <- c("#deebf7", "#31a354")
            world_sf <- ne_countries(scale = "medium", returnclass = "sf") 
            ranges <- c("0", "1")
          # Plot
            gglist02[[kk]] <- ggplot() + 
              geom_sf(data = no_cc, aes(group = as.factor(freq_sel), fill = as.factor(freq_sel)), color = NA) +
              geom_sf(data = provinces_shp, fill = NA) +
              geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
              scale_fill_manual(values = pal,
                                name = "Selection",
                                labels = ranges) +
              # ggtitle(main_tittles[kk]) +
              labs(y = y_axis[kk]) +
              theme_opts3 +
              theme(legend.position = "none")
      }
      stopCluster(cl)
      
  # Plotting the FINAL FIGURE AMONG SCENARIOS
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
                                axis.title.y = element_text(face = "plain", size = 25, angle = 90),
                                plot.title = element_text(face = "plain", size = 25, hjust = 0.5),
                                legend.title = element_text(colour = "black", face = "bold", size = 16),
                                legend.text = element_text(colour = "black", face = "plain", size = 15),
                                legend.key.height = unit(1.5, "cm"),
                                legend.key.width = unit(1.5, "cm"),
                                plot.tag = element_text(size = 25, face = "bold")))
    # CALIBRATION PLOTS
      p3 <-   ((plots_list02[[1]] / plots_list02[[2]] / plots_list02[[3]]) | ((plots_list01[[1]] / plots_list01[[2]] / plots_list01[[3]]) + theme_opts3)) +
        # plot_layout(guides = "collect") +
        plot_annotation(tag_prefix = "",
                        tag_levels = "A",
                        tag_suffix = ".",) +
        ggsave(paste(outdir, paste("no-regrets-scenarios", ".pdf", sep = ""), sep = ""), width = 30, height = 20, dpi = 300)

  # NO REGRETS ALL [original figure 3]
    # Begin the parallel structure
      UseCores <- 5
      cl <- makeCluster(UseCores)  
      registerDoParallel(cl)
      gglits <- vector("list", length = length(olayers_list))
      plots_list <- foreach(l = 1:length(olayers_list), .packages = c("sf", "raster", "dplyr", "ggplot2", "rnaturalearth", "rnaturalearthdata", "RColorBrewer")) %dopar% {
        # 
          files_solution <- list.files(path = olayers_list[[l]], pattern = "*Layer_.*.csv$", full.names = TRUE)
          provinces_csv <- read.csv(list.files(path = olayers_list[[l]], pattern = "*pus-.*.csv$", full.names = TRUE)[1]) %>% 
            dplyr::arrange(layer) # the same for every ocean layer so that's why [1]
          mpas_csv <- read.csv(list.files(path = olayers_list[[kk]], pattern = "*_mpas.*.csv$", full.names = TRUE)[1]) %>% 
            dplyr::filter(province != "non-categ_mpas")
          vmes_csv <- read.csv(list.files(path = olayers_list[[kk]], pattern = "*_VMEs.*.csv$", full.names = TRUE)[1]) %>% 
            dplyr::filter(province != "non-categ_VMEs")
          pu_shpfile <- st_read(list.files(path = olayers_list[[l]], pattern = ".shp", full.names = TRUE)[1]) # # the same for every ocean layer so that's why [1]
        
        # 
          solutions_csv <- lapply(files_solution, function(x) {
            single <- read.csv(x)
            sol_csv <- single %>% 
              dplyr::mutate(freq_sel = single[, 6]) %>% 
              dplyr::select(id, cost, freq_sel) %>% 
              dplyr::arrange(id)})
        # 
          # no_regret <- solutions_csv[[1]][,3]*solutions_csv[[2]][,3]*solutions_csv[[3]][,3]*solutions_csv[[4]][,3]
          no_regret <- solutions_csv[[1]][,3]*solutions_csv[[2]][,3]*solutions_csv[[3]][,3]
          no_regret_csv <- solutions_csv[[4]] %>% # [[4]] is the "base" dataframe solution but does not matter in this case
            dplyr::mutate(no_regret = no_regret) %>% 
            dplyr::select(id, no_regret) %>% 
            dplyr::arrange(id)
        
        # Get the solutions from the corresponding planning unit shapefile (not sure if this is useful but worth to check later)
          best_freq_sol <- pu_shpfile[pu_shpfile$id %in% no_regret_csv$id, ] %>% 
            dplyr::mutate(no_regret = no_regret_csv$no_regret) %>% 
            dplyr::filter(!id %in% unique(c(mpas_csv$layer, vmes_csv$layer)))
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
                                      axis.title.y = element_text(face = "plain", size = 25, angle = 90),
                                      plot.title = element_text(face = "plain", size = 25, hjust = 0.5),
                                      legend.title = element_text(colour = "black", face = "bold", size = 16),
                                      legend.text = element_text(colour = "black", face = "plain", size = 15), 
                                      legend.key.height = unit(1.5, "cm"),
                                      legend.key.width = unit(1.5, "cm"),
                                      legend.position = "none", 
                                      plot.tag = element_text(size = 25, face = "bold")))
          # Color Palette, World borders and Legend
            pal <- c("#deebf7", "#31a354")
            world_sf <- ne_countries(scale = "medium", returnclass = "sf") 
            ranges <- c("0", "1")
          # Plot
            gglits[[l]] <- ggplot() + 
              geom_sf(data = best_freq_sol, aes(group = as.factor(no_regret), fill = as.factor(no_regret)), color = NA) +
              geom_sf(data = provinces_shp, fill = NA) +
              geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
              scale_fill_manual(values = pal,
                                name = "Selection",
                                labels = ranges) +
              # ggtitle(main_tittles[l]) +
              labs(y = y_axis[l]) +
              theme_opts3 +
              theme(legend.position = "none")
      }
      stopCluster(cl)
      
  # NO REGRET ACROSS DEPTHS
    UseCores <- 5
    cl <- makeCluster(UseCores)  
    registerDoParallel(cl)
    sflits <- vector("list", length = length(olayers_list))
    sf_list <- foreach(j = 1:length(olayers_list), .packages = c("sf", "raster", "dplyr", "ggplot2", "rnaturalearth", "rnaturalearthdata", "RColorBrewer")) %dopar% {
      # 
        files_solution <- list.files(path = olayers_list[[j]], pattern = "*Layer_.*.csv$", full.names = TRUE)
        provinces_csv <- read.csv(list.files(path = olayers_list[[j]], pattern = "*pus-.*.csv$", full.names = TRUE)[1]) %>% 
          dplyr::arrange(layer) # the same for every ocean layer so that's why [1]
        mpas_csv <- read.csv(list.files(path = olayers_list[[j]], pattern = "*_mpas.*.csv$", full.names = TRUE)[1]) %>% 
          dplyr::filter(province != "non-categ_mpas")
        vmes_csv <- read.csv(list.files(path = olayers_list[[j]], pattern = "*_VMEs.*.csv$", full.names = TRUE)[1]) %>% 
          dplyr::filter(province != "non-categ_VMEs")
        pu_shpfile <- st_read(list.files(path = olayers_list[[j]], pattern = ".shp", full.names = TRUE)[1]) # # the same for every ocean layer so that's why [1]
      # 
        solutions_csv <- lapply(files_solution, function(x) {
          single <- read.csv(x)
          sol_csv <- single %>% 
            dplyr::mutate(freq_sel = single[, 6]) %>% 
            dplyr::select(id, cost, freq_sel) %>% 
            dplyr::arrange(id)})
      # 
        no_regret <- solutions_csv[[1]][,3]*solutions_csv[[2]][,3]*solutions_csv[[3]][,3]
        no_regret_csv <- solutions_csv[[4]] %>% # [[4]] is the "base" dataframe solution but does not matter in this case
          dplyr::mutate(no_regret = no_regret) %>% 
          dplyr::select(id, no_regret) %>% 
          dplyr::arrange(id)
        
      # Get the solutions from the corresponding planning unit shapefile (not sure if this is useful but worth to check later)
        best_freq_sol <- pu_shpfile[pu_shpfile$id %in% no_regret_csv$id, ] %>% 
          mutate(no_regret = no_regret_csv$no_regret)
        sflits[[j]] <- best_freq_sol
      }
      stopCluster(cl)
  # Getting no regrets from original shapefiles
      single_shp <- lapply(shps, function(x) {final <- st_read(x)})
      first <- sf_list[[1]]$no_regret[match(single_shp[[1]]$layer, sf_list[[1]]$id)]
        first <- ifelse(is.na(first), 0, ifelse(first == 1, 4, first)) # some NAs due depth
      second <- sf_list[[2]]$no_regret[match(single_shp[[2]]$layer, sf_list[[2]]$id)]
        second <- ifelse(is.na(second), 0, ifelse(second == 1, 5, second)) # some NAs due depth
      third <- sf_list[[3]]$no_regret[match(single_shp[[3]]$layer, sf_list[[3]]$id)]
        third <- ifelse(is.na(third), 0, ifelse(third == 1, 6, third)) # some NAs due depth
      # Adding elements to get values
        sf_list[[1]]$no_regret_all <- first+second+third
      # Leaving all important layers
        no_regrets01 <- sf_list[[1]] %>% 
          mutate(no_regret_all = ifelse(no_regret_all == 4, 0, 
                                        ifelse(no_regret_all == 5, 0, 
                                               ifelse(no_regret_all == 6, 0, no_regret_all)))) %>% 
          dplyr::filter(!id %in% unique(c(mpas_csv$layer, vmes_csv$layer)))
  
    # Creating the final data frame with % per depth among all depth + climate scenarios
        df_sum1 <- data.frame(Epipelagic = round(sum(first == 4)/90065, digits = 2), 
                              Mesopelagic = round(sum(second == 5)/88528, digits = 2),
                              Bathy = round(sum(third == 6)/87170, digits = 2),
                              EpiMeso = round((sum(no_regrets01$no_regret_all == 9))/(90065+88528+87170), digits = 3), 
                              EpiBathy = round((sum(no_regrets01$no_regret_all == 10))/(90065+88528+87170), digits = 3), 
                              MesoBathy = round((sum(no_regrets01$no_regret_all == 11))/(90065+88528+87170), digits = 3), 
                              All = round((sum(no_regrets01$no_regret_all == 15))/(90065+88528+87170), digits = 3))
        df_sum2 <- data.frame(Epipelagic = round(sum(first == 4)/90065, digits = 2), 
                              Mesopelagic = round(sum(second == 5)/88528, digits = 2),
                              Bathy = round(sum(third == 6)/87170, digits = 2),
                              EpiMeso = round((sum(no_regrets01$no_regret_all == 9))/(90065+88528), digits = 3), 
                              EpiBathy = round((sum(no_regrets01$no_regret_all == 10))/(90065+87170), digits = 3), 
                              MesoBathy = round((sum(no_regrets01$no_regret_all == 11))/(88528+87170), digits = 3), 
                              All = round((sum(no_regrets01$no_regret_all == 15))/(90065+88528+87170), digits = 3))
        
        df_final <- rbind(df_sum1, df_sum2)
        write.csv(df_final, paste(outdir, paste("no-regrets-all", ".csv", sep = ""), sep = ""))  
        
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
                                    axis.title.y = element_text(face = "plain", size = 25, angle = 90),
                                    plot.title = element_text(face = "plain", size = 25, hjust = 0.5),
                                    legend.title = element_text(colour = "black", face = "bold", size = 16),
                                    legend.text = element_text(colour = "black", face = "plain", size = 15), 
                                    legend.key.height = unit(1.5, "cm"),
                                    legend.key.width = unit(1.5, "cm"),
                                    plot.tag = element_text(size = 25, face = "bold")))
        # Color Palette, World borders and Legend
          pal <- c("#deebf7", "#984ea3", "#1b9e77", "#377eb8", "#e41a1c")
          world_sf <- ne_countries(scale = "medium", returnclass = "sf") 
          ranges <- c("Not selected", "Epipelagic and Mesopelagic", "Epipelagic and Bathyabyssopelagic", "Mesopelagic and Bathyabyssopelagic", "All")
        # Plot
          no_regret_all <- ggplot() + 
            geom_sf(data = no_regrets01, aes(group = as.factor(no_regret_all), fill = as.factor(no_regret_all)), color = NA) +
            geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
            scale_fill_manual(values = pal,
                              name = "Coherence\n across layers",
                              labels = ranges) +
            theme_opts3

  # Plotting the FINAL FIGURE
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
                                axis.title.y = element_text(face = "plain", size = 25, angle = 90),
                                plot.title = element_text(face = "plain", size = 25, hjust = 0.5),
                                legend.title = element_text(colour = "black", face = "bold", size = 16),
                                legend.text = element_text(colour = "black", face = "plain", size = 15),
                                legend.key.height = unit(1.5, "cm"),
                                legend.key.width = unit(1.5, "cm"),
                                plot.tag = element_text(size = 25, face = "bold")))
      # CALIBRATION PLOTS
      p3 <-   ((plots_list[[1]] / plots_list[[2]] / plots_list[[3]]) | ((no_regret_all / no_regret_all / no_regret_all) + theme_opts3)) +
        # plot_layout(guides = "collect") +
        plot_annotation(tag_prefix = "",
                        tag_levels = "A",
                        tag_suffix = ".",) +
        # theme_opts3 +
        ggsave(paste(outdir, paste("no-regrets-all", ".pdf", sep = ""), sep = ""), width = 30, height = 20, dpi = 300)
}

system.time(no_regret_plots(path = "/QRISdata/Q1216/BritoMorales/Project04b/vfinal-sol_figs_03/ublm-cal_0520rce-vocc0520_targets-mix_rawcost_noduplicates",
                            outdir = "/QRISdata/Q1216/BritoMorales/Project04b/vfinal-sol_figs_03/ublm-cal_0520rce-vocc0520_targets-mix_rawcost_noduplicates/",
                            shp = "/QRISdata/Q1216/BritoMorales/Project04b/shapefiles_rasters/01_abnjs_nofilterdepth"))

# system.time(no_regret_plots(path = "ublm-cal_1020rce-vocc1050_targets-mix_rawcost_noduplicates",
#                             outdir = "ublm-cal_1020rce-vocc1050_targets-mix_rawcost_noduplicates/",
#                             shp = "shapefiles_rasters/01_abnjs_nofilterdepth"))

