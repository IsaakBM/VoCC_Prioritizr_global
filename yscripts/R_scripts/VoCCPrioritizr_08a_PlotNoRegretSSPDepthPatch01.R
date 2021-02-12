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
      main_tittles2 <- c("Climate-smart network", "", "")
      
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
                                      legend.title = element_text(colour = "black", face = "bold", size = 18),
                                      legend.text = element_text(colour = "black", face = "plain", size = 16), 
                                      legend.key.height = unit(2.7, "cm"),
                                      legend.key.width = unit(1.6, "cm"),
                                      plot.tag = element_text(size = 35, face = "bold")))
          # Color Palette, World borders and Legend
            pal <- c("#deebf7", "#984ea3", "#1b9e77", "#377eb8", "#e41a1c")
            pal2 <- c("#e5f5f9", "#41b6c4", "#8856a7", "#fa9fb5", "#2c7fb8")
            world_sf <- ne_countries(scale = "medium", returnclass = "sf") 
            ranges <- c("Not selected", "SSP1-2.6 and SSP2-4.5", "SSP1-2.6 and SSP5-8.5", "SSP2-4.5 and SSP5-8.5", "All")
          # Plot
            gglist01[[i]] <- ggplot() + 
              geom_sf(data = no_regrets01, aes(group = as.factor(no_regret_all), fill = as.factor(no_regret_all)), color = NA) +
              geom_sf(data = provinces_shp, fill = NA) +
              geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
              scale_fill_manual(values = pal2,
                                name = "Coherence\n across climate scenarios",
                                labels = ranges) +
              ggtitle(main_tittles2[i]) +
              # labs(y = y_axis[i]) +
              theme_opts3
      }
      stopCluster(cl)
      
      # Getting data frame information for CLIMATE SCENARIOS
        UseCores <- 5
        cl <- makeCluster(UseCores)  
        registerDoParallel(cl)
        dflist01 <- vector("list", length = length(olayers_list))
        df_list01 <- foreach(a = 1:length(olayers_list), .packages = c("sf", "raster", "dplyr", "ggplot2", "rnaturalearth", "rnaturalearthdata", "RColorBrewer")) %dopar% {
          #
            files_solution <- list.files(path = olayers_list[[a]], pattern = "*Layer_.*.csv$", full.names = TRUE)
            provinces_csv <- read.csv(list.files(path = olayers_list[[a]], pattern = "*pus-.*.csv$", full.names = TRUE)[1]) %>% 
              dplyr::arrange(layer)
            mpas_csv <- read.csv(list.files(path = olayers_list[[a]], pattern = "*_mpas.*.csv$", full.names = TRUE)[1]) %>% 
              dplyr::filter(province != "non-categ_mpas")
            vmes_csv <- read.csv(list.files(path = olayers_list[[a]], pattern = "*_VMEs.*.csv$", full.names = TRUE)[1]) %>% 
              dplyr::filter(province != "non-categ_VMEs")
            pu_shpfile <- st_read(list.files(path = olayers_list[[a]], pattern = ".shp", full.names = TRUE)[1]) # the same for every ocean layer so that's why [1]
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
          
          no_regrets02 <- pu_shpfile %>% 
            dplyr::mutate(no_regret_all = ssp126$solution1+ssp245$solution1+ssp585$solution1) %>% 
            dplyr::filter(!id %in% unique(c(mpas_csv$layer, vmes_csv$layer)))
          # 
          dflist01[[a]] <- data.frame(ssp126 = round(sum(ssp126$solution1 == 4)/length(unique(pu_shpfile$id)), digits = 4)*100, 
                                     ssp245 = round(sum(ssp245$solution1 == 5)/length(unique(pu_shpfile$id)), digits = 4)*100,
                                     ssp585 = round(sum(ssp585$solution1 == 6)/length(unique(pu_shpfile$id)), digits = 4)*100,
                                     ssp126_ssp245 = round((sum(no_regrets02$no_regret_all == 9))/(90065), digits = 4)*100, 
                                     ssp126_ssp585 = round((sum(no_regrets02$no_regret_all == 10))/(90065), digits = 4)*100, 
                                     ssp245_ssp585 = round((sum(no_regrets02$no_regret_all == 11))/(90065), digits = 4)*100, 
                                     All_ssp = round((sum(no_regrets02$no_regret_all == 15))/(90065), digits = 4)*100)
        }
        stopCluster(cl)
        dflist01_final <- do.call(rbind, df_list01)
        rownames(dflist01_final) <- y_axis
      

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
                                legend.title = element_text(colour = "black", face = "bold", size = 18),
                                legend.text = element_text(colour = "black", face = "plain", size = 16),
                                legend.key.height = unit(2.7, "cm"),
                                legend.key.width = unit(1.6, "cm"),
                                plot.tag = element_text(size = 35, face = "bold")))
    # CALIBRATION PLOTS
      p3 <-   (((plots_list01[[1]] / plots_list01[[2]] / plots_list01[[3]]) + theme_opts3)) +
        plot_layout(guides = "collect") +
        plot_annotation(tag_prefix = "",
                        tag_levels = "a",
                        tag_suffix = ".",) +
        ggsave(paste(outdir, paste("no-regret-scenarios", ".pdf", sep = ""), sep = ""), width = 30, height = 20, dpi = 300)

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
          mpas_csv <- read.csv(list.files(path = olayers_list[[l]], pattern = "*_mpas.*.csv$", full.names = TRUE)[1]) %>% 
            dplyr::filter(province != "non-categ_mpas")
          vmes_csv <- read.csv(list.files(path = olayers_list[[l]], pattern = "*_VMEs.*.csv$", full.names = TRUE)[1]) %>% 
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
          no_regret_csv <- solutions_csv[[1]] %>%
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
                                      legend.title = element_text(colour = "black", face = "bold", size = 18),
                                      legend.text = element_text(colour = "black", face = "plain", size = 16), 
                                      legend.key.height = unit(2.7, "cm"),
                                      legend.key.width = unit(1.6, "cm"),
                                      legend.position = "none", 
                                      plot.tag = element_text(size = 35, face = "bold")))
          # Color Palette, World borders and Legend
            pal <- c("#deebf7", "#31a354")
            pal2 <- c("#e5f5f9", "#31a354")
            world_sf <- ne_countries(scale = "medium", returnclass = "sf") 
            ranges <- c("0", "1")
          # Plot
            gglits[[l]] <- ggplot() + 
              geom_sf(data = best_freq_sol, aes(group = as.factor(no_regret), fill = as.factor(no_regret)), color = NA) +
              geom_sf(data = provinces_shp, fill = NA) +
              geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
              scale_fill_manual(values = pal2,
                                name = "Selection",
                                labels = ranges) +
              ggtitle(main_tittles2[l]) +
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
        ssp126 <- dplyr::left_join(x = pu_shpfile, y = solutions_csv[[1]],  by = "id") %>% 
          dplyr::mutate(solution1 = ifelse(is.na(freq_sel), 0, ifelse(freq_sel == 1, 4, freq_sel)))
        ssp245 <- dplyr::left_join(x = pu_shpfile, y = solutions_csv[[2]],  by = "id") %>% 
          dplyr::mutate(solution1 = ifelse(is.na(freq_sel), 0, ifelse(freq_sel == 1, 5, freq_sel)))
        ssp585 <- dplyr::left_join(x = pu_shpfile, y = solutions_csv[[3]],  by = "id") %>% 
          dplyr::mutate(solution1 = ifelse(is.na(freq_sel), 0, ifelse(freq_sel == 1, 6, freq_sel)))
      # 
        no_regrets02 <- pu_shpfile %>% 
          dplyr::mutate(no_regret_all = ssp126$solution1+ssp245$solution1+ssp585$solution1) %>% 
          dplyr::filter(!id %in% unique(c(mpas_csv$layer, vmes_csv$layer))) %>% 
          data.frame %>% 
          dplyr::select(id, depth, cost, no_regret_all)
      }
      stopCluster(cl)
  # Getting no regrets from original shapefiles
    single_shp <- lapply(shps, function(x) {
      single <- st_read(x) %>% 
        dplyr::rename(id = layer)})
    
    ep <- dplyr::left_join(x = single_shp[[1]], y = sf_list[[1]],  by = "id") %>% 
      dplyr::mutate(no_regret_all2 = ifelse(no_regret_all != 15, 0, no_regret_all)) %>% 
      dplyr::mutate(no_regret_all2 = ifelse(no_regret_all == 15, 4, no_regret_all2))
    mp <- dplyr::left_join(x = single_shp[[2]], y = sf_list[[2]],  by = "id") %>% 
      dplyr::mutate(no_regret_all2 = ifelse(no_regret_all != 15, 0, no_regret_all)) %>% 
      dplyr::mutate(no_regret_all2 = ifelse(no_regret_all == 15, 5, no_regret_all2))
    bap <- dplyr::left_join(x = single_shp[[3]], y = sf_list[[3]],  by = "id") %>% 
      dplyr::mutate(no_regret_all2 = ifelse(no_regret_all != 15, 0, no_regret_all)) %>% 
      dplyr::mutate(no_regret_all2 = ifelse(no_regret_all == 15, 6, no_regret_all2))
      
  # Leaving all important layers
    mpas_csv <- read.csv(list.files(path = olayers_list[[1]], pattern = "*_mpas.*.csv$", full.names = TRUE)[1]) %>% 
      dplyr::filter(province != "non-categ_mpas")
    vmes_csv <- read.csv(list.files(path = olayers_list[[1]], pattern = "*_VMEs.*.csv$", full.names = TRUE)[1]) %>% 
      dplyr::filter(province != "non-categ_VMEs")
        
  # Adding elements to get values
    no_regrets01 <- single_shp[[1]] %>% 
      dplyr::mutate(no_regret_all = ep$no_regret_all2 + mp$no_regret_all2 + bap$no_regret_all2) %>% 
      dplyr::filter(!id %in% unique(c(mpas_csv$layer, vmes_csv$layer)))
    
    df_sum1 <- data.frame(Epipelagic = round((sum(ep$no_regret_all2 == 4, na.rm = TRUE))/(90065), digits = 4)*100, 
                          Mesopelagic = round((sum(mp$no_regret_all2 == 5, na.rm = TRUE))/(90065), digits = 4)*100,
                          Bathy = round((sum(bap$no_regret_all2 == 6, na.rm = TRUE))/(90065), digits = 4)*100,
                          EpiMeso = round((sum(no_regrets01$no_regret_all == 9, na.rm = TRUE))/(90065), digits = 4)*100, 
                          EpiBathy = round((sum(no_regrets01$no_regret_all == 10, na.rm = TRUE))/(90065), digits = 4)*100, 
                          MesoBathy = round((sum(no_regrets01$no_regret_all == 11, na.rm = TRUE))/(90065), digits = 4)*100, 
                          All = round((sum(no_regrets01$no_regret_all == 15, na.rm = TRUE))/(90065), digits = 4)*100)
    write.csv(df_sum1, paste(outdir, paste("no-regrets-all", ".csv", sep = ""), sep = ""))  
    
    no_regrets02 <- no_regrets01 %>% 
      dplyr::mutate(no_regret_all2 = ifelse(is.na(no_regret_all), 0, 
                                            ifelse(no_regret_all == 4, 0, 
                                                   ifelse(no_regret_all == 5, 0, 
                                                          ifelse(no_regret_all == 6, 0, no_regret_all))))) %>% 
      dplyr::filter(!id %in% unique(c(mpas_csv$layer, vmes_csv$layer)))
    
      # Getting planning units information for no-regret areas to later get Species Information 
        no_regrets03 <- data.frame(cbind(ep$id, ep$no_regret_all2, mp$id, mp$no_regret_all2, bap$id, bap$no_regret_all2))
        no_regrets03 <- no_regrets03 %>% 
          dplyr::mutate(vertical_all = X2+X6+X4) %>% 
          dplyr::filter(vertical_all == 15) %>% 
          dplyr::mutate(olayer = "all") %>% 
          dplyr::rename(ep_id = X1, ep_value = X2, mp_id = X3, mp_value = X4, bap_id = X5, bap_value = X6)
        write.csv(no_regrets03, paste(outdir, paste("Vertical_sps", ".csv", sep = ""), sep = ""))  
        
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
                                    legend.title = element_text(colour = "black", face = "bold", size = 18),
                                    legend.text = element_text(colour = "black", face = "plain", size = 16), 
                                    legend.key.height = unit(2.7, "cm"),
                                    legend.key.width = unit(1.6, "cm"),
                                    plot.tag = element_text(size = 35, face = "bold")))
        # Color Palette, World borders and Legend
          pal <- c("#deebf7", "#984ea3", "#1b9e77", "#377eb8", "#e41a1c")
          pal2 <- c("#deebf7", "#fc8d59", "#df65b0", "#bae4bc", "#2c7fb8")
          world_sf <- ne_countries(scale = "medium", returnclass = "sf") 
          ranges <- c("Not selected", "Epipelagic and Mesopelagic", "Epipelagic and Bathyabyssopelagic", "Mesopelagic and Bathyabyssopelagic", "All")
        # Plot
          no_regret_all <- ggplot() + 
            geom_sf(data = no_regrets02, aes(group = as.factor(no_regret_all2), fill = as.factor(no_regret_all2)), color = NA) +
            geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
            scale_fill_manual(values = pal2,
                              name = "Coherence\n across layers",
                              labels = ranges) +
            theme_opts3 +
            ggtitle("Vertical climate-smart network")

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
                                legend.title = element_text(colour = "black", face = "bold", size = 18),
                                legend.text = element_text(colour = "black", face = "plain", size = 16),
                                legend.key.height = unit(1.5, "cm"),
                                legend.key.width = unit(1.5, "cm"),
                                plot.tag = element_text(size = 35, face = "bold")))
      # CALIBRATION PLOTS
      p4 <-   (((plots_list01[[1]] / plots_list01[[2]] / plots_list01[[3]]) + theme_opts3) |
                 ((no_regret_all / no_regret_all / no_regret_all) + theme_opts3)) +
        # plot_layout(guides = "collect") +
        plot_annotation(tag_prefix = "",
                        tag_levels = "a",
                        tag_suffix = ".",) +
        ggsave(paste(outdir, paste("no-regret-FinalAll", ".pdf", sep = ""), sep = ""), width = 48, height = 20, dpi = 300)
}

system.time(no_regret_plots(path = "/QRISdata/Q1216/BritoMorales/Project04b/vfinal-sol_figs_03_noBase/ublm-cal_1030rce-vocc10100_targets-mix_rawcost_noduplicates_iucn",
                            outdir = "/QRISdata/Q1216/BritoMorales/Project04b/vfinal-sol_figs_03_noBase/ublm-cal_1030rce-vocc10100_targets-mix_rawcost_noduplicates_iucn/",
                            shp = "/QRISdata/Q1216/BritoMorales/Project04b/shapefiles_rasters/01_abnjs_nofilterdepth"))

# system.time(no_regret_plots(path = "vfinal-sol_figs_03/ublm-cal_1030rce-vocc10100_targets-mix_rawcost_noduplicates_iucn copy",
#                             outdir = "vfinal-sol_figs_03/ublm-cal_1030rce-vocc10100_targets-mix_rawcost_noduplicates_iucn copy/",
#                             shp = "shapefiles_rasters/01_abnjs_nofilterdepth"))

