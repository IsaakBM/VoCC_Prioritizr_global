# This code was written by Isaac Brito-Morales (i.britomorales@uq.edu.au)
# Please do not distribute this code without permission.
# NO GUARANTEES THAT CODE IS CORRECT
# Caveat Emptor!

library(sf)
library(raster)
library(dplyr)
library(data.table)
library(future.apply)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(RColorBrewer)
library(patchwork)
library(stringr)


general_plots <- function(path, outdir){ }

path = "wgeneral_figs"
# Climatic directory
dir.climatic <- list.dirs(path = path, full.names = TRUE, recursive = FALSE) [1:3]
# Different data per directory
sps <- list.files(path = dir.climatic, pattern = paste0(c(paste0("*lagic.csv")), collapse = "|"), full.names = TRUE) # species' number
sps_prov <- list.files(path = dir.climatic, pattern = paste0(c(paste0("*provinces*", ".*.csv$")), collapse = "|"), full.names = TRUE) # features[species by prov] number
shp <- list.files(path = dir.climatic, pattern = paste0(c(paste0("*lagic*", ".*.shp$")), collapse = "|"), full.names = TRUE) # shapefile with the cost
prov_csv <- list.files(path = dir.climatic, pattern = paste0(c(paste0("*Longhurst*", ".*.csv$"), paste0("*Glasgow*", ".*.csv$"), paste0("*GOODS*", ".*.csv$")) , collapse = "|"), full.names = TRUE) # province data
rce_csv <- list.files(path = dir.climatic, pattern = paste0(c(paste0("*RCE*", ".*.csv$")), collapse = "|"), full.names = TRUE) # RCE by layer/SSP
vocc_csv <- list.files(path = dir.climatic, pattern = paste0((paste0("*vocc*", ".*.csv$")), collapse = "|"), full.names = TRUE) # VoCC by layer/SSP

# Getting th plan to use lappy future parallel package
plan(multiprocess)
  # Reading the planning unit [PU] file that contains COST
    cost_shp <- future_lapply(shp, function(x) {
      single <- st_read(x)
      final <- single %>% 
        dplyr::mutate(cost = ifelse(is.na(cost), 0, cost)) %>% 
        dplyr::mutate(cost_log = log10(cost + 1))
      final$cost <- ifelse(final$cost_log == 0, median(filter(final, final$cost_log != 0)$cost), final$cost_log)
      final <- final})
  
  # Creating PLOTS Species Provinces
    sps_prov_plots <- future_lapply(sps_prov, function(x) {
      single <- fread(x) %>% 
        dplyr::arrange(pu) %>%
        dplyr::group_by(pu) %>% 
        dplyr::summarise(richness = n()) %>% 
        dplyr::mutate(richness_log = log10(richness))
      final <- single %>% 
        mutate(rich_categ = ifelse(richness_log == 0, 1,
                                   ifelse(richness_log > 0 & richness_log <= 1, 2, 
                                          ifelse(richness_log > 1 & richness_log <= 2, 3,
                                                 ifelse(richness_log > 2 & richness_log <= 3, 4, 5)))))
      
    indexing <- ifelse(stringr::str_detect(string = basename(x), pattern = "Epi"), 1,
                       ifelse(stringr::str_detect(string = basename(x), pattern = "Meso"), 2, 3))
    # Get the variable from the corresponding planning unit shapefile
      pu_shpfile <- cost_shp[[indexing]]
      rich_shpfile <- pu_shpfile[pu_shpfile$id %in% final$pu, ] %>% 
        mutate(rich_categ = final$rich_categ)
    # Creating the PROVINCES shapefile 
      provinces_csv <- read.csv(prov_csv[indexing]) %>% 
        dplyr::arrange(layer)
      provinces_shp <- pu_shpfile %>%
        dplyr::mutate(provinces = provinces_csv$province) %>% 
        base::transform(id = as.numeric(factor(provinces))) %>% 
        dplyr::group_by(id) %>% 
        dplyr::summarise(prov = sum(id, do_union = TRUE))
    
    # Defining generalities
      pal_rich <- rev(brewer.pal(5, "RdYlBu"))
      cv_rich <- c("1", "1 - 10", "10 - 100", "100 - 1000", "> 1000")
      world_sf <- ne_countries(scale = "medium", returnclass = "sf")
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
    # Plotting the figures
      p <- ggplot() + 
        geom_sf(data = rich_shpfile, aes(fill = rich_categ), color = NA) +
        geom_sf(data = provinces_shp, fill = NA) +
        geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
        ggtitle(str_remove(unlist(strsplit(basename(x), split = "_"))[2], pattern = "Layer")) +
        scale_fill_gradientn(name = "Richness",
                             colours = pal_rich,
                             limits = c(1, 5),
                             breaks = seq(1, 5, 1),
                             labels = cv_rich) +
        theme_opts3})
  
    
    
    # Creating COST's PLOTS
      cost_plots <- future_lapply(shp, function(x) {
        single <- st_read(x)
        final <- single %>% 
          dplyr::mutate(cost = ifelse(is.na(cost), 0, cost)) %>% 
          dplyr::mutate(cost_log = log10(cost + 1))
        final$cost <- ifelse(final$cost_log == 0, median(filter(final, final$cost_log != 0)$cost), final$cost_log)
        
        cost_shpfile <- final %>% 
          mutate(cost_categ = ifelse(cost_log == 0, 1,
                                     ifelse(cost_log > 0 & cost_log <= 1, 2, 
                                            ifelse(cost_log > 1 & cost_log <= 2, 3,
                                                   ifelse(cost_log > 2 & cost_log <= 3, 4, 
                                                          ifelse(cost_log > 3 & cost_log <= 4, 5, 6))))))
        
        indexing <- ifelse(stringr::str_detect(string = basename(x), pattern = "Epi"), 1,
                           ifelse(stringr::str_detect(string = basename(x), pattern = "Meso"), 2, 3))
        # Creating the PROVINCES shapefile 
          pu_shpfile <- cost_shp[[indexing]]
          provinces_csv <- read.csv(prov_csv[indexing]) %>% 
            dplyr::arrange(layer)
          provinces_shp <- pu_shpfile %>%
            dplyr::mutate(provinces = provinces_csv$province) %>% 
            base::transform(id = as.numeric(factor(provinces))) %>% 
            dplyr::group_by(id) %>% 
            dplyr::summarise(prov = sum(id, do_union = TRUE))
        
        # Defining generalities
          # pal_cost <- c("#a1d99b", "#74c476", "#ffffcc", "#ffeda0", "#d0d1e6", "#0570b0")
          pal_cost <- brewer.pal(6, "RdPu")
          cv_cost <- c("1", "1 - 10", "10 - 100", "100 - 1000", "1000 - 10000", "> 10000")
          world_sf <- ne_countries(scale = "medium", returnclass = "sf")
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
        # Plotting the figures
          p <- ggplot() + 
            geom_sf(data = cost_shpfile, aes(fill = cost_categ), color = NA) +
            geom_sf(data = provinces_shp, fill = NA) +
            geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
            ggtitle(str_remove(unlist(strsplit(basename(x), split = "_"))[3], pattern = "Layer")) +
            scale_fill_gradientn(name = "USD",
                                 colours = pal_cost,
                                 limits = c(1, 6),
                                 breaks = seq(1, 6, 1),
                                 labels = cv_cost) +
            theme_opts3})
      
  # Plotting the FINAL figure
    p0_final2 <- ((cost_plots[[1]] / cost_plots[[2]] / cost_plots[[3]]) + plot_layout(guides = "collect") | (sps_prov_plots[[1]] / sps_prov_plots[[2]] / sps_prov_plots[[3]]) + plot_layout(guides = "collect")) +
      # plot_layout(guides = "collect") +
      plot_annotation(tag_prefix = "",
                      tag_levels = "A",
                      tag_suffix = ".",) +
      theme_opts3 +
      ggsave("wgeneral_figs/cost-richness_test02.png", width = 25, height = 20, dpi = 300)



  
  
  # Relative Cumulative Exposure Index
    vocc_shp <- future_lapply(rce_csv, function(x) {
      single <- read.csv(x)
      final <- single %>% 
        dplyr::select(-X) %>% 
        dplyr::arrange(pu)
      final$climate_feature <- ifelse(is.na(final$climate_feature), 
                                      median(filter(final, final$climate_feature != 0)$climate_feature), 
                                      final$climate_feature)
      final <- final %>% 
        dplyr::mutate(croot_rce = kader:::cuberoot(final$climate_feature)) %>% 
        dplyr::mutate(rce_categ = ifelse(croot_rce <= 0.2, 1, 
                                         ifelse(croot_rce > 0.2 & croot_rce <= 0.4, 2, 
                                                ifelse(croot_rce > 0.4 & croot_rce <= 0.6, 3, 
                                                       ifelse(croot_rce > 0.6 & croot_rce <= 0.8, 4, 
                                                              ifelse(croot_rce > 0.8 & croot_rce <= 1.1, 5, 
                                                                     ifelse(croot_rce > 1.1 & croot_rce <= 1.2, 6, 
                                                                            ifelse(croot_rce > 1.2 & croot_rce <= 1.5, 7, 
                                                                                   ifelse(croot_rce > 1.5 & croot_rce <= 2, 8, 
                                                                                          ifelse(croot_rce > 2 & croot_rce <= 4, 9,
                                                                                                 ifelse(croot_rce > 4 & croot_rce <= 6, 10, 11)))))))))))})

    
    
# Reading VOCC and merge it to a shapefile for each SSP
vocc_shp <- future_lapply(vocc_csv, function(x) {
  # Reading file
    single <- read.csv(x)
    final <- single %>% 
      dplyr::select(-X) %>% 
      dplyr::arrange(pu)
    final$climate_feature <- ifelse(is.na(final$climate_feature), 
                                    median(filter(final, final$climate_feature != 0)$climate_feature), 
                                    final$climate_feature)
  # Merging VoCC values with the appropiate shapefile
    shp <- cost_shp[[1]]
    final <- shp %>%
      dplyr::mutate(vocc_dec = final$climate_feature*10) %>% 
      dplyr::mutate(vocc_categ = ifelse(vocc_dec <= -50, 1, 
                                        ifelse(vocc_dec > -50 & vocc_dec <= -20, 2, 
                                               ifelse(vocc_dec > -20 & vocc_dec <= -10, 3,
                                                      ifelse(vocc_dec > -10 & vocc_dec <= -5, 4, 
                                                             ifelse(vocc_dec > -5 & vocc_dec <= 5, 5, 
                                                                    ifelse(vocc_dec > 5 & vocc_dec <= 10, 6,
                                                                           ifelse(vocc_dec > 10 & vocc_dec <= 20, 7, 
                                                                                  ifelse(vocc_dec > 20 & vocc_dec <= 50, 8,
                                                                                         ifelse(vocc_dec > 50 & vocc_dec <= 100, 9, 
                                                                                                ifelse(vocc_dec > 100 & vocc_dec <= 200, 10, 11)))))))))))
  # Creating the PROVINCES shapefile 
    provinces_csv <- read.csv(prov_csv) %>% 
      dplyr::arrange(layer)
    provinces_shp <- shp %>%
      dplyr::mutate(provinces = provinces_csv$province) %>% 
      base::transform(id = as.numeric(factor(provinces))) %>% 
      dplyr::group_by(id) %>% 
      dplyr::summarise(prov = sum(id, do_union = TRUE))
  
  # Defining generalities
    bls <- rev(brewer.pal(6, "Blues"))[1:5]
    rds <- brewer.pal(6, "OrRd")
    pal_vocc <- c(bls, rds)
    cv_vocc <- c("< -50", "-50 - -20", "-20 - -10", "-10 - -5", "-5 - 5",
                 "5 - 10", "10 - 20", "20 - 50", "50 - 100", "100 - 200", "> 200")
    world_sf <- ne_countries(scale = "medium", returnclass = "sf")
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
                              plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
                              legend.title = element_text(colour = "black", face = "bold", size = 20),
                              legend.text = element_text(colour = "black", face = "bold", size = 15), 
                              legend.key.height = unit(1.9, "cm"),
                              legend.key.width = unit(1, "cm"),
                              plot.tag = element_text(size = 25, face = "bold")))
  # Plotting the figures
    p <- ggplot() + 
      geom_sf(data = final, aes(fill = vocc_categ), color = NA) +
      geom_sf(data = provinces_shp, fill = NA) +
      geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
      coord_sf(xlim = c(1228192, 14748730), ylim = c(-7600965, 3826233)) +
      ggtitle(unlist(strsplit(basename(x), split = "_"))[4]) +
      scale_fill_gradientn(name = expression(km~dec^-1),
                           colours = pal_vocc,
                           limits = c(1, 11),
                           breaks = seq(1, 11, 1),
                           labels = cv_vocc) +
      theme_opts3})

# Reading RCE and merge it to a shapefile for each SSP
rce_shp <- lapply(rce_csv, function(x) {
  # 
    single <- read.csv(x)
    final <- single %>% 
      dplyr::select(-X) %>% 
      dplyr::arrange(pu)
    final$climate_feature <- ifelse(is.na(final$climate_feature), median(filter(final, final$climate_feature != 0)$climate_feature), final$climate_feature)
  # Merging RCE values with the appropiate shapefile
    shp <- cost_shp[[1]]
    final <- shp %>%
      dplyr::mutate(croot_rce = kader:::cuberoot(final$climate_feature)) %>% 
      dplyr::mutate(rce_categ = ifelse(croot_rce <= 0.2, 1,
                                       ifelse(croot_rce > 0.2 & croot_rce <= 0.4, 2,
                                              ifelse(croot_rce > 0.4 & croot_rce <= 0.6, 3,
                                                     ifelse(croot_rce > 0.6 & croot_rce <= 0.8, 4,
                                                            ifelse(croot_rce > 0.8 & croot_rce <= 1.1, 5,
                                                                   ifelse(croot_rce > 1.1 & croot_rce <= 1.2, 6,
                                                                          ifelse(croot_rce > 1.2 & croot_rce <= 1.5, 7,
                                                                                 ifelse(croot_rce > 1.5 & croot_rce <= 2, 8,
                                                                                        ifelse(croot_rce > 2 & croot_rce <= 4, 9,
                                                                                               ifelse(croot_rce > 4 & croot_rce <= 6, 10, 11)))))))))))
    # Creating the PROVINCES shapefile 
      provinces_csv <- read.csv(prov_csv) %>% 
        dplyr::arrange(layer)
      provinces_shp <- shp %>%
        dplyr::mutate(provinces = provinces_csv$province) %>% 
        base::transform(id = as.numeric(factor(provinces))) %>% 
        dplyr::group_by(id) %>% 
        dplyr::summarise(prov = sum(id, do_union = TRUE))
    
    # Defining generalities
      pal_rce <- rev(brewer.pal(9, "Spectral"))
      cv_rce <- c("min", "", "", "", "", "", "", "", "max")
      world_sf <- ne_countries(scale = "medium", returnclass = "sf")
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
                                plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
                                legend.title = element_text(colour = "black", face = "bold", size = 20),
                                legend.text = element_text(colour = "black", face = "bold", size = 15), 
                                legend.key.height = unit(1.9, "cm"),
                                legend.key.width = unit(1, "cm"),
                                plot.tag = element_text(size = 25, face = "bold")))
    # Plotting the figures
      p <- ggplot() + 
        geom_sf(data = final, aes(fill = rce_categ), color = NA) +
        geom_sf(data = provinces_shp, fill = NA) +
        geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
        coord_sf(xlim = c(1228192, 14748730), ylim = c(-7600965, 3826233)) +
        ggtitle(unlist(strsplit(basename(x), split = "_"))[4]) +
        scale_fill_gradientn(name = "RCE index",
                             colours = pal_rce,
                             limits = c(1, 9),
                             breaks = seq(1, 9, 1),
                             labels = cv_rce) +
        theme_opts3})


slp_shp <- lapply(slp_csv, function(x) {
  # 
    single <- read.csv(x)
    final <- single %>% 
      dplyr::select(-X) %>% 
      dplyr::arrange(pu)
    final$climate_feature <- ifelse(is.na(final$climate_feature), median(filter(final, final$climate_feature != 0)$climate_feature), final$climate_feature)
    
  # Merging SLOPE values with the appropiate shapefile
    shp <- cost_shp[[1]]
    final <- shp %>%
      dplyr::mutate(slp_dec = final$climate_feature*10) %>% 
      dplyr::mutate(croot_slp = kader:::cuberoot(slp_dec)) %>% 
      dplyr::mutate(slp_categ = ifelse(croot_slp <= -0.06, 1, 
                                       ifelse(croot_slp > -0.06 & croot_slp <= 0.18, 2, 
                                              ifelse(croot_slp > 0.18 & croot_slp <= 0.42, 3,
                                                     ifelse(croot_slp > 0.42 & croot_slp <= 0.54, 4,
                                                            ifelse(croot_slp > 0.54 & croot_slp <= 0.66, 5, 
                                                                   ifelse(croot_slp > 0.66 & croot_slp <= 0.78, 6, 7)))))))
  # Creating the PROVINCES shapefile 
    provinces_csv <- read.csv(prov_csv) %>% 
      dplyr::arrange(layer)
    provinces_shp <- shp %>%
      dplyr::mutate(provinces = provinces_csv$province) %>% 
      base::transform(id = as.numeric(factor(provinces))) %>% 
      dplyr::group_by(id) %>% 
      dplyr::summarise(prov = sum(id, do_union = TRUE))
    
  # Defining generalities
    pal_slp <- brewer.pal(7, "RdPu")
    cv_slp <- c("-0.03 - 0", "0 - 0.01", "0.01 - 0.07", "0.07 - 0.16", "0.16 - 0.29", "0.29 - 0.47" , "> 0.47")
    world_sf <- ne_countries(scale = "medium", returnclass = "sf")
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
                              plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
                              legend.title = element_text(colour = "black", face = "bold", size = 20),
                              legend.text = element_text(colour = "black", face = "bold", size = 15), 
                              legend.key.height = unit(1.9, "cm"),
                              legend.key.width = unit(1, "cm"),
                              plot.tag = element_text(size = 25, face = "bold")))
  # Plotting the figures
    p <- ggplot() + 
      geom_sf(data = final, aes(fill = slp_categ), color = NA) +
      geom_sf(data = provinces_shp, fill = NA) +
      geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
      coord_sf(xlim = c(1228192, 14748730), ylim = c(-7600965, 3826233)) +
      ggtitle(unlist(strsplit(basename(x), split = "_"))[4]) +
      scale_fill_gradientn(name = expression(~degree~C~dec^-1),
                           colours = pal_slp,
                           limits = c(1, 7),
                           breaks = seq(1, 7, 1),
                           labels = cv_slp) +
      theme_opts3})

# unique(slp_shp[[1]]$slp_categ)
# unique(slp_shp[[2]]$slp_categ)
# unique(slp_shp[[3]]$slp_categ)
# 
# range(slp_shp[[1]]$croot_slp)
# range(slp_shp[[2]]$croot_slp)
# range(slp_shp[[3]]$croot_slp)
# 
# 
# range(kader:::cuberoot(slp_shp[[1]]$climate_feature*10))
# range(kader:::cuberoot(slp_shp[[2]]$climate_feature*10))
# range(kader:::cuberoot(slp_shp[[3]]$climate_feature*10))
# 
# a <- seq(-0.3, 0.9, length.out = 11)
# round(a^(3), 2)

p0_final2 <- ((slp_shp[[1]] + slp_shp[[2]] + slp_shp[[3]])/(rce_shp[[1]] + rce_shp[[2]] + rce_shp[[3]])/(vocc_shp[[1]] + vocc_shp[[2]] + vocc_shp[[3]])) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_prefix = "",
                  tag_levels = "A",
                  tag_suffix = ".",) +
  theme_opts3 +
  ggsave("Project05b_Rosa/w_climatic-plots_figures/Figure_1_climatic-variables.png", width = 25, height = 20, dpi = 300)

