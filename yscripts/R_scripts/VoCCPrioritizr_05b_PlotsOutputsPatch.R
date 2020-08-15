# This code was written by Isaac Brito-Morales (i.britomorales@uq.edu.au)
# Please do not distribute this code without permission.
# NO GUARANTEES THAT CODE IS CORRECT
# Caveat Emptor!

library(sf)
library(raster)
library(dplyr)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(RColorBrewer)
library(patchwork)
library(stringr)
library(irr)
library(psych)

# path = "Project05b_Rosa/w_results-outputs_figures"
path = "Project05b_Rosa/w_results-outputs_figures _final-g-blm"
# Identifing different files
  sol_csv <- list.files(path = path, pattern = paste0(c(paste0("*vocc_*", ".*.csv$")), collapse = "|"), full.names = TRUE)
  prov_csv <- list.files(path = path, pattern = paste0(c(paste0("*Longhurst*", ".*.csv$"), paste0("*Glasgow*", ".*.csv$"), paste0("*GOODS*", ".*.csv$")) , collapse = "|"), full.names = TRUE)
  shp <- list.files(path = path, pattern = paste0(c(paste0("*lagic*", ".*.shp$")), collapse = "|"), full.names = TRUE)
# Reading cost shapefile in drectory
  cost_shp <- lapply(shp, function(x) {
    single <- st_read(x)
    final <- single %>% 
      dplyr::mutate(cost = ifelse(is.na(cost), 0, cost)) %>% 
      dplyr::mutate(cost = round(cost))
    final$cost <- ifelse(final$cost == 0, median(filter(final, final$cost != 0)$cost), final$cost)
    final <- final %>% 
      dplyr::mutate(cost_log = log10(cost))
    final <- final})
# Reading Solutions and merge it to a shapefile for each climatic scenario
  sol_shp <- lapply(sol_csv, function(x) {
    single <- read.csv(x)
    if(ncol(single) > 6) {
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
      pu_shpfile <- cost_shp[[1]]
      best_freq_sol <- pu_shpfile[pu_shpfile$id %in% freq_base$id, ] %>% 
        mutate(freq_cat = freq_base$freq_cat2)
    # Creating the PROVINCES shapefile 
      provinces_csv <- read.csv(prov_csv) %>% 
        dplyr::arrange(layer)
      provinces_shp <- pu_shpfile %>%
        dplyr::mutate(provinces = provinces_csv$province) %>% 
        base::transform(id = as.numeric(factor(provinces))) %>% 
        dplyr::group_by(id) %>% 
        dplyr::summarise(prov = sum(id, do_union = TRUE))
      
    # Defining themes
      theme_opts3 <- list(theme(panel.grid.minor = element_blank(),
                                panel.grid.major = element_blank(),
                                panel.background = element_rect(fill = "white", colour = "black"),
                                plot.background = element_rect(fill = "white"),
                                panel.border = element_blank(),
                                axis.line = element_line(size = 1),
                                axis.text.x = element_text(size = rel(2), angle = 0),
                                axis.text.y = element_text(size = rel(2), angle = 0),
                                axis.ticks = element_line(size = 1.5),
                                axis.ticks.length = unit(.25, "cm"), 
                                axis.title.x = element_blank(),
                                axis.title.y = element_blank(),
                                plot.title = element_text(face = "bold", size = 30, hjust = 0.5),
                                legend.title = element_text(colour = "black", face = "bold", size = 20),
                                legend.text = element_text(colour = "black", face = "bold", size = 15), 
                                legend.key.height = unit(1.9, "cm"),
                                legend.key.width = unit(1, "cm"),
                                plot.tag = element_text(size = 25, face = "bold")))
    # Color Palette, World borders and Legend
      pal0 <- brewer.pal(length(unique(best_freq_sol$freq_cat)) - 1, "Greens")
      pal <- c("#deebf7", pal0)
      world_sf <- ne_countries(scale = "medium", returnclass = "sf") 
      ranges <- c("0", "< 25", "25 - 50", "50 - 75", "> 75")
    # Plot
      p <- ggplot() + 
        geom_sf(data = best_freq_sol, aes(group = as.factor(freq_cat), fill = as.factor(freq_cat)), color = NA) +
        geom_sf(data = provinces_shp, fill = NA) +
        geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
        coord_sf(xlim = c(1228192, 14748730), ylim = c(-7600965, 3826233)) +
        scale_fill_manual(values = pal,
                          name = "Selection Frequency (%)",
                          labels = ranges) +
        ggtitle(ifelse(str_detect(string = basename(x), pattern = "ssp"), unlist(strsplit(basename(x), split = "_"))[5], "Base")) +
        theme_opts3}})
# Plotting the FINAL figure
  p0_final2 <- (sol_shp[[1]] + sol_shp[[2]] + sol_shp[[3]] + sol_shp[[4]]) +
    plot_layout(guides = "collect") +
    plot_annotation(tag_prefix = "",
                    tag_levels = "A",
                    tag_suffix = ".",) +
    theme_opts3 +
    # ggsave("Project05b_Rosa/w_results-outputs_figures/test5.pdf", width = 25, height = 20, dpi = 300)
    ggsave("Project05b_Rosa/w_results-outputs_figures _final-g-blm/BLM-sol-30_final.png", width = 25, height = 20, dpi = 300)
  
# Getting the frequency to calculate Kappa
  sol_freq <- lapply(sol_csv, function(x) {
    single <- read.csv(x)
    if(ncol(single) > 6) {
      sol_csv <- single %>% 
        dplyr::mutate(freq_sel = rowSums(single[, 6:ncol(single)])) %>% 
        dplyr::select(id, cost, freq_sel) %>% 
        dplyr::mutate(freq_cat = (freq_sel)/(length(6:ncol(single)))*100)}
    freq_base <- sol_csv %>% 
      mutate(freq_cat2 = ifelse(freq_cat == 0, 1, 
                                ifelse(freq_cat > 0 & freq_cat <= 25, 2,
                                       ifelse(freq_cat > 25 & freq_cat <= 50, 3, 
                                              ifelse(freq_cat > 50 & freq_cat <= 75, 4, 5)))))})
  
  # Calculating Kappa
    df3 <- cbind(sol_freq[[3]]$freq_cat, sol_freq[[4]]$freq_cat)
    # irr::kappa2(df3)
    unlist(cohen.kappa(x = df3))[1]

      
# Getting the frequency to calculate "NO-REGRET" areas
  noregret <- lapply(sol_csv, function(x) {
    single <- read.csv(x)
    if(ncol(single) > 6) {
      sol_csv <- single %>% 
        dplyr::mutate(freq_sel = rowSums(single[, 6:ncol(single)])) %>% 
        dplyr::select(id, cost, freq_sel) %>% 
        dplyr::mutate(freq_cat = (freq_sel)/(length(6:ncol(single)))*100)}
    freq_base <- sol_csv %>% 
        mutate(freq_cat2 = ifelse(freq_cat == 0, 0, 
                                  ifelse(freq_cat > 0 & freq_cat <= 25, 2,
                                         ifelse(freq_cat > 25 & freq_cat <= 50, 3, 
                                                ifelse(freq_cat > 50 & freq_cat <= 75, 4, 5))))) %>% 
      dplyr::select(id, cost, freq_cat2) %>% 
      dplyr::arrange(id)
    })
  # Creating data frame for no-regret pus among SSPs
    final_noregret <- noregret[[1]] %>% 
      dplyr::select(id, cost) %>% 
      dplyr::arrange(id)
    final_noregret <- final_noregret %>% 
      dplyr::mutate(noregret  = (noregret[[2]]$freq_cat2 * noregret[[3]]$freq_cat2 * noregret[[4]]$freq_cat2)) %>% 
      dplyr::mutate(cat_noregret = ifelse(noregret == 0, 1, 2))
  
  # Get the PUs from the corresponding planning unit shapefile
    pu_shpfile <- cost_shp[[1]]
    noregret_sol <- pu_shpfile[pu_shpfile$id %in% final_noregret$id, ] %>% 
      mutate(cat_noregret = final_noregret$cat_noregret)
  # Creating the PROVINCES shapefile 
    provinces_csv <- read.csv(prov_csv) %>% 
      dplyr::arrange(layer)
    provinces_shp <- pu_shpfile %>%
      dplyr::mutate(provinces = provinces_csv$province) %>% 
      base::transform(id = as.numeric(factor(provinces))) %>% 
      dplyr::group_by(id) %>% 
      dplyr::summarise(prov = sum(id, do_union = TRUE))
  
  # Defining themes
    theme_opts3 <- list(theme(panel.grid.minor = element_blank(),
                              panel.grid.major = element_blank(),
                              panel.background = element_rect(fill = "white", colour = "black"),
                              plot.background = element_rect(fill = "white"),
                              panel.border = element_blank(),
                              axis.line = element_line(size = 1),
                              axis.text.x = element_text(size = rel(2), angle = 0),
                              axis.text.y = element_text(size = rel(2), angle = 0),
                              axis.ticks = element_line(size = 1.5),
                              axis.ticks.length = unit(.25, "cm"), 
                              axis.title.x = element_blank(),
                              axis.title.y = element_blank(),
                              plot.title = element_text(face = "bold", size = 30, hjust = 0.5),
                              legend.title = element_text(colour = "black", face = "bold", size = 20),
                              legend.text = element_text(colour = "black", face = "bold", size = 15), 
                              legend.key.height = unit(1.9, "cm"),
                              legend.key.width = unit(1, "cm"),
                              plot.tag = element_text(size = 25, face = "bold")))
  # Color Palette, World borders and Legend
    pal <- c("#deebf7", "#e34a33")
    world_sf <- ne_countries(scale = "medium", returnclass = "sf") 
    ranges <- c("0", "1")
  # Plot
    p <- ggplot() + 
      geom_sf(data = noregret_sol, aes(group = as.factor(cat_noregret), fill = as.factor(cat_noregret)), color = NA) +
      geom_sf(data = provinces_shp, fill = NA) +
      geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
      coord_sf(xlim = c(1228192, 14748730), ylim = c(-7600965, 3826233)) +
      scale_fill_manual(values = pal,
                        name = "selection",
                        labels = ranges) +
      ggtitle("no-regret") +
      theme_opts3
    # Plotting the FINAL figure
      p0_final2 <- p +
        plot_layout(guides = "collect") +
        plot_annotation(tag_prefix = "",
                        tag_levels = "A",
                        tag_suffix = ".",) +
        theme_opts3 +
        # ggsave("Project05b_Rosa/w_results-outputs_figures/noregret_01.pdf", width = 20, height = 15, dpi = 300)
        ggsave("Project05b_Rosa/w_results-outputs_figures _final-g-blm/noregret_01.png", width = 20, height = 15, dpi = 300)
      
  
  
  
  
    
    
    
    

  