# This code was written by Isaac Brito-Morales (i.britomorales@uq.edu.au)
# Please do not distribute this code without permission.
# NO GUARANTEES THAT CODE IS CORRECT
# Caveat Emptor!

no_regret_plots <- function(path, outdir) {
  
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
    # 
      ep_dir <- dir.layers[str_detect(string = basename(dir.layers), pattern = "Epi") == TRUE]
      mp_dir <- dir.layers[str_detect(string = basename(dir.layers), pattern = "Meso") == TRUE]
      bap_dir <- dir.layers[str_detect(string = basename(dir.layers), pattern = "Bathy") == TRUE]
      olayers_list <- list(ep_dir, mp_dir, bap_dir)
    # 
      y_axis <- c("Epipelagic", "Mesopelagic", "Bathyabyssopelagic")
      
  # Loop for every directory
    # Begin the parallel structure
      UseCores <- 5
      cl <- makeCluster(UseCores)  
      registerDoParallel(cl)
      gglits <- vector("list", length = length(olayers_list))
      plots_list <- foreach(i = 1:length(olayers_list), .packages = c("sf", "raster", "dplyr", "ggplot2", "rnaturalearth", "rnaturalearthdata", "RColorBrewer")) %dopar% {
        
        # 
          files_solution <- list.files(path = olayers_list[[i]], pattern = "*Layer_.*.csv$", full.names = TRUE)
          provinces_csv <- read.csv(list.files(path = olayers_list[[i]], pattern = "*pus-.*.csv$", full.names = TRUE)[1]) %>% 
            dplyr::arrange(layer) # the same for every ocean layer so that's why [1]
          pu_shpfile <- st_read(list.files(path = olayers_list[[i]], pattern = ".shp", full.names = TRUE)[1]) # # the same for every ocean layer so that's why [1]
        
        # 
          solutions_csv <- lapply(files_solution, function(x) {
            single <- read.csv(x)
            sol_csv <- single %>% 
              dplyr::mutate(freq_sel = single[, 6]) %>% 
              dplyr::select(id, cost, freq_sel) %>% 
              dplyr::arrange(id)})
        # 
          no_regret <- solutions_csv[[1]][,3]*solutions_csv[[2]][,3]*solutions_csv[[3]][,3]*solutions_csv[[4]][,3]
          no_regret_csv <- solutions_csv[[4]] %>% # [[4]] is the "base" dataframe solution but does not matter in this case
            dplyr::mutate(no_regret = no_regret) %>% 
            dplyr::select(id, no_regret) %>% 
            dplyr::arrange(id)
        
        # Get the solutions from the corresponding planning unit shapefile (not sure if this is useful but worth to check later)
          best_freq_sol <- pu_shpfile[pu_shpfile$id %in% no_regret_csv$id, ] %>% 
            mutate(no_regret = no_regret_csv$no_regret)
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
              geom_sf(data = best_freq_sol, aes(group = as.factor(no_regret), fill = as.factor(no_regret)), color = NA) +
              geom_sf(data = provinces_shp, fill = NA) +
              geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
              scale_fill_manual(values = pal,
                                name = "Selection",
                                labels = ranges) +
              # ggtitle(main_tittles[i]) +
              labs(y = y_axis[i]) +
              theme_opts3
        
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
                                axis.title.y = element_text(face = "plain", size = 25, angle = 90),
                                plot.title = element_text(face = "plain", size = 25, hjust = 0.5),
                                legend.title = element_text(colour = "black", face = "bold", size = 15),
                                legend.text = element_text(colour = "black", face = "bold", size = 10),
                                legend.key.height = unit(2, "cm"),
                                legend.key.width = unit(0.9, "cm"),
                                plot.tag = element_text(size = 25, face = "bold")))
      # CALIBRATION PLOTS
      p3 <-   ((plots_list[[1]] / plots_list[[2]] / plots_list[[3]])) +
        plot_layout(guides = "collect") +
        plot_annotation(tag_prefix = "",
                        tag_levels = "A",
                        tag_suffix = ".",) +
        theme_opts3 +
        ggsave(paste(outdir, paste("no-regrets", ".pdf", sep = ""), sep = ""), width = 20, height = 20, dpi = 300)
}

system.time(no_regret_plots(path = "/QRISdata/Q1216/BritoMorales/Project04b/vfinal-sol_figs/ublm-cal_0520rce-vocc050_targets-mix_rawcost",
                           outdir = "/QRISdata/Q1216/BritoMorales/Project04b/vfinal-sol_figs/ublm-cal_0520rce-vocc050_targets-mix_rawcost/"))


###############################


# TABLE with the area of protection of the no regret area, cost and vocc comparing with the different climate scenarios 
# epi total area of protection; total opportunity cost; vocc [vocc126-vocc245-vocc585] ; RCE index [rce126-rce245-rce585]
# meso
# bathy

# nrow(no_regret_csv) # 90065
# nrow(no_regret_csv[no_regret_csv$no_regret == 1,]) # 7899
# 
# (7899*100)/90065
# 
# nrow(solutions_csv[[1]])
# nrow(no_regret_csv[solutions_csv[[1]]$freq_sel == 1,]) # 21627
# (21627*100)/90065
