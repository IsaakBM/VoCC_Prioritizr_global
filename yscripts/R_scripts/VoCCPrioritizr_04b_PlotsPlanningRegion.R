# This code was written by Isaac Brito-Morales (i.britomorales@uq.edu.au)
# Please do not distribute this code without permission.
# NO GUARANTEES THAT CODE IS CORRECT
# Caveat Emptor!

plot_solutions <- function(path, outdir) {
  
  library(sf)
  library(raster)
  library(dplyr)
  library(ggplot2)
  library(rnaturalearth)
  library(rnaturalearthdata)
  library(RColorBrewer)
  library(foreach)
  library(doParallel)
  
  # Which directories for climate prioritization scenarios?
    dir.layers <- paste(list.dirs(path = path, full.names = TRUE, recursive = FALSE), sep = "/")
  # Loop for every directory
    for(j in 1:length(dir.layers)) {
      # reading how many SOLUTIONS .csv files are per BLM calibration
        files_solution <- list.files(path = dir.layers[j], pattern = "*Layer_.*.csv$", full.names = TRUE)
        provinces_csv <- read.csv(list.files(path = dir.layers[j], pattern = "*pus-.*.csv$", full.names = TRUE)) %>% 
          dplyr::arrange(layer)
        pu_shpfile <- st_read(list.files(path = dir.layers[j], pattern = ".shp", full.names = TRUE))
      # Begin the parallel structure
      UseCores <- 24
      cl <- makeCluster(UseCores)  
      registerDoParallel(cl)
      # Parallel Loop
        foreach(i = 1:length(files_solution), .packages = c("sf", "raster", "dplyr", "ggplot2", "rnaturalearth", "rnaturalearthdata", "RColorBrewer")) %dopar% {
          # Some manipulation
            single <- read.csv(files_solution[i])
            if(ncol(single) > 6) {
            sol_csv <- single %>% 
              dplyr::mutate(freq_sel = rowSums(single[, 6:ncol(single)])) %>% 
              dplyr::select(id, cost, freq_sel) %>% 
              dplyr::mutate(freq_cat = (freq_sel)/(length(6:ncol(single)))*10)
            freq_base <- sol_csv %>% 
              mutate(freq_cat2 = ifelse(freq_cat == 0, 1, 
                                       ifelse(freq_cat > 0 & freq_cat <= 2, 2,
                                              ifelse(freq_cat > 2 & freq_cat <= 5, 3, 
                                                     ifelse(freq_cat > 5 & freq_cat <= 7, 4, 5))))) # categories needs to be based on max length columns
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
          theme_opts2 <- list(theme(panel.grid.minor = element_blank(),
                                    panel.grid.major = element_blank(),
                                    panel.background = element_blank(),
                                    plot.background = element_rect(fill = "white"),
                                    panel.border = element_blank(),
                                    axis.line = element_line(size = 1),
                                    axis.text.x = element_text(size = rel(2), angle = 0),
                                    axis.text.y = element_text(size = rel(2), angle = 0),
                                    axis.ticks = element_line(size = 1.5),
                                    axis.ticks.length = unit(.25, "cm"), 
                                    axis.title.x = element_blank(),
                                    axis.title.y = element_blank(),
                                    plot.title = element_text(face = "bold", size = 22, hjust = 0.5),
                                    legend.title = element_text(colour = "black", face = "bold", size = 20),
                                    legend.text = element_text(colour = "black", face = "bold", size = 20), 
                                    legend.key.height = unit(1.5, "cm"),
                                    legend.key.width = unit(1, "cm"),
                                    plot.tag = element_text(size = 30, face = "bold")))
          
      # Color Palette, World borders and Legend
        #pal0 <- brewer.pal(length(unique(best_freq_sol$freq_cat)) - 1, "YlOrRd")
        pal0 <- brewer.pal(length(unique(best_freq_sol$freq_cat)) - 1, "Greens")
        pal <- c("#deebf7", pal0)
        world_sf <- ne_countries(scale = "medium", returnclass = "sf") 
        # ranges <- c("0", "< 25", "25 - 50", "50 - 75", "> 75")
        ranges <- as.character(sort(unique(best_freq_sol$freq_cat)))
        
      # Plot
        ggplot() + 
          geom_sf(data = best_freq_sol, aes(group = as.factor(freq_cat), fill = as.factor(freq_cat)), color = NA) +
          geom_sf(data = provinces_shp) +
          geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
          scale_fill_manual(values = pal,
                            name = "Selection Frequency (%)",
                            labels = ranges) +
          ggtitle(basename(sub(pattern = "*.csv", "", files_solution[i]))) +
          theme_opts2 +
          ggsave(paste(outdir, basename(sub(pattern = "*.csv", "", files_solution[i])), ".pdf", sep = ""), width = 22, height = 10, dpi = 300)
        }
    }
    stopCluster(cl)
  }
  
}

# system.time(plot_solutions(path = "/QRISdata/Q1216/BritoMorales/Project04b/prioritization_zblm-cal", 
#                            outdir = "/QRISdata/Q1216/BritoMorales/Project04b/prioritization_zblm-cal"))

system.time(plot_solutions(path = "prioritization_zblm-cal",
                           outdir = "prioritization_zblm-cal/"))


