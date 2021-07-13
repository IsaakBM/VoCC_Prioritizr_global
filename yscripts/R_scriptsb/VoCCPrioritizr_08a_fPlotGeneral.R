# This code was written by Isaac Brito-Morales (i.britomorales@uq.edu.au)
# Please do not distribute this code without permission.
# NO GUARANTEES THAT CODE IS CORRECT
# Caveat Emptor!

library(sf)
library(raster)
library(ggplot2)
library(prioritizr)
library(gurobi)
library(patchwork)
library(rnaturalearth)
library(rnaturalearthdata)
library(RColorBrewer)
library(dplyr)
library(readr)
library(ggtext)
library(stringr)
library(data.table)

####################################################################################
####### 0.- Calling general objects for plots
####################################################################################
# Robinson Projection
moll <- "+proj=moll +lon_0=0 +datum=WGS84 +units=m +no_defs"
# Marine regions

# Planning area
sf_PlanningArea <- st_read("Output/PlanningUnits/PUs_SA_robin_025deg/PUs_SA_robin_025deg.shp") %>% 
  dplyr::rename(id = layer)
# Land
world_sf <- ne_countries(scale = "medium", returnclass = "sf") %>% 
  st_transform(crs = CRS(moll))
# # MPAs .csv
# mpas <- read.csv("Output/PlanningbyRegions/pus-surface_mpas_.csv") %>% 
#   dplyr::rename(id = layer) %>% 
#   dplyr::filter(province != "non-categ_mpas")
# # MPAs sf
# mpas_sf <- left_join(sf_PlanningArea, mpas, "id") %>% 
#   na.omit() %>% 
#   dplyr::select(id, geometry)


####################################################################################
####### 3.- Plot Climate velocity
####################################################################################
#  
  plot_VoCC <- function(path, world_sf) { 
    # Directory planning domain
      dir.climatic <- list.dirs(path = "Figures/General", full.names = TRUE, recursive = FALSE)
    # SSPs files
      files.nc <- list.files(path = dir.climatic, pattern = paste0((paste0("*vocc*", ".*.rds$")), collapse = "|"), full.names = TRUE)
    # Loop
      gg_list <- vector("list", length = length(files.nc))
    # Each SSP file
      for(j in seq_along(files.nc)) {
       # Reading files
          dt1 <- readRDS(files.nc[j]) %>% 
            dplyr::rename(id = pu) %>% 
            dplyr::select(id, climate_feature) %>% 
            dplyr::mutate(vocc_dec = climate_feature*10) %>%
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
        # Defining generalities to plot
          bls <- rev(brewer.pal(6, "Blues"))[1:5]
          rds <- brewer.pal(6, "OrRd")
          pal_vocc <- c(bls, rds)
          cv_vocc <- c("< -50", "-50 - -20", "-20 - -10", "-10 - -5", "-5 - 5",
                       "5 - 10", "10 - 20", "20 - 50", "50 - 100", "100 - 200", "> 200")
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
                                    legend.title = element_text(colour = "black", face = "bold", size = 25),
                                    legend.text = element_text(colour = "black", face = "bold", size = 20),
                                    legend.key.height = unit(2.5, "cm"),
                                    legend.key.width = unit(1.4, "cm"),
                                    plot.tag = element_text(size = 25, face = "bold")))
        # Create the ggplot
          gg_list[[j]] <- ggplot() +
            geom_sf(data = dt1, aes(fill = vocc_categ), color = NA) +
            # geom_sf(data = provinces_shp, fill = NA) +
            geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
            scale_fill_gradientn(name = expression(km~dec^-1),
                                 colours = pal_vocc,
                                 limits = c(1, 11),
                                 breaks = seq(1, 11, 1),
                                 labels = cv_vocc) +
            ggtitle(ifelse(stringr::str_detect(string = files.nc[j], pattern = "Epi"), 
                           ifelse(stringr::str_detect(basename(files.nc[j]), pattern = "ssp126"), "SSP1-2.6", 
                                  ifelse(stringr::str_detect(basename(files.nc[j]), pattern = "ssp245"), "SSP2-4.5", "SSP5-8.5")), "")) +
            labs(y = str_remove(ifelse(stringr::str_detect(string = basename(files.nc[j]), pattern = "ssp126"),
                                       unlist(strsplit(unlist(str_split(files.nc[j], pattern = "/"))[3], split = "_"))[2], ""), pattern = "Layer")) +
            theme_opts3
      }
      return(gg_list)
  }


####################################################################################
####### 4.- RCE
####################################################################################
# 
  plot_RCE <- function(path, world_sf) { 
    # Directory planning domain
      dir.climatic <- list.dirs(path = "Figures/General", full.names = TRUE, recursive = FALSE)
    # SSPs files
      files.nc <- list.files(path = dir.climatic, pattern = paste0((paste0("*RCE*", ".*.rds$")), collapse = "|"), full.names = TRUE)
    # Loop
      gg_list <- vector("list", length = length(files.nc))
    # Each SSP file
      for(j in seq_along(files.nc)) {
        # Reading files
        dt1 <- readRDS(files.nc[j]) %>% 
          dplyr::rename(id = pu) %>% 
          dplyr::select(id, climate_feature) %>% 
          dplyr::mutate(croot_rce = kader:::cuberoot(climate_feature)) %>%
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
        # Defining generalities to plot
          pal_rce <- rev(brewer.pal(11, "Spectral"))
          cv_rce <- c("min", "", "", "", "", "", "", "", "", "", "max")
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
                                    legend.title = element_text(colour = "black", face = "bold", size = 25),
                                    legend.text = element_text(colour = "black", face = "bold", size = 20),
                                    legend.key.height = unit(2.5, "cm"),
                                    legend.key.width = unit(1.4, "cm"),
                                    plot.tag = element_text(size = 25, face = "bold")))
        # Create the ggplot
          gg_list[[j]] <- ggplot() +
            geom_sf(data = dt1, aes(fill = rce_categ), color = NA) +
            # geom_sf(data = provinces_shp, fill = NA) +
            geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
            scale_fill_gradientn(name = "RCE index",
                                 colours = pal_rce,
                                 limits = c(1, 11),
                                 breaks = seq(1, 11, 1),
                                 labels = cv_rce) +
            ggtitle(ifelse(stringr::str_detect(string = files.nc[j], pattern = "Epi"), 
                           ifelse(stringr::str_detect(basename(files.nc[j]), pattern = "ssp126"), "SSP1-2.6", 
                                  ifelse(stringr::str_detect(basename(files.nc[j]), pattern = "ssp245"), "SSP2-4.5", "SSP5-8.5")), "")) +
            labs(y = str_remove(ifelse(stringr::str_detect(string = basename(files.nc[j]), pattern = "ssp126"),
                                       unlist(strsplit(unlist(str_split(files.nc[j], pattern = "/"))[3], split = "_"))[2], ""), pattern = "Layer")) +
            theme_opts3
      }
    return(gg_list)
  }


























p1 <- patchwork::wrap_plots(gg_list, ncol = 3, byrow = TRUE) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_prefix = "(",
                  tag_levels = "a", 
                  tag_suffix = ")",)
ggsave("Figures/test07.pdf", plot = p1, width = 35, height = 25, dpi = 300, limitsize = FALSE)


p1 <- patchwork::wrap_plots(gg_list, ncol = 3, byrow = TRUE) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_prefix = "(",
                  tag_levels = "a", 
                  tag_suffix = ")",)
ggsave("Figures/test08.pdf", plot = p1, width = 35, height = 25, dpi = 300, limitsize = FALSE)









