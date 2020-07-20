library(sf)
library(raster)
library(dplyr)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(RColorBrewer)
library(patchwork)
library(foreach)
library(doParallel)

path = "zztestingplots"
outdir = "zztestingplots/"
dir.layers <- paste(list.dirs(path = path, full.names = TRUE, recursive = FALSE), sep = "/")
# Begin the parallel structure
UseCores <- detectCores() -1
cl <- makeCluster(UseCores)  
registerDoParallel(cl)

df_list <- vector(mode = "list", length = length(dir.layers))
foreach(i = 1:length(dir.layers), .packages = c("sf", "raster", "dplyr", "ggplot2", "rnaturalearth", "rnaturalearthdata", "RColorBrewer", "patchwork")) %dopar% {
  
  single <- list.files(path = dir.layers[i], pattern = "*.csv$", full.names = TRUE) %>% 
    read.csv()
  pu_shpfile <- list.files(path = dir.layers[i], pattern = "*.shp$", full.names = TRUE) %>% 
    st_read()
  sol_csv <- single %>% 
    dplyr::mutate(freq_sel = rowSums(single[, 6:15])) %>% 
    dplyr::select(id, cost, freq_sel)
  freq_base <- sol_csv %>% 
    mutate(freq_cat = ifelse(freq_sel == 0, 1, 
                             ifelse(freq_sel > 0 & freq_sel <= 2, 2,
                                    ifelse(freq_sel > 2 & freq_sel <= 5, 3, 
                                           ifelse(freq_sel > 5 & freq_sel <= 7, 4, 5)))))
  
  best_freq_sol <- pu_shpfile[pu_shpfile$id %in% freq_base$id, ] %>% 
    mutate(freq_cat = freq_base$freq_cat)
  
  
  theme_opts2 <- list(theme(panel.grid.minor = element_blank(),
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
                            plot.title = element_text(face = "bold", size = 22, hjust = 0.5),
                            legend.title = element_text(colour = "black", face = "bold", size = 20),
                            legend.text = element_text(colour = "black", face = "bold", size = 20), 
                            legend.key.height = unit(1.5, "cm"),
                            legend.key.width = unit(1, "cm"),
                            plot.tag = element_text(size = 30, face = "bold")))
  
  rds <- brewer.pal(4, "YlOrRd")
  pal <- c("#deebf7", rds)
  world_sf <- ne_countries(scale = "medium", returnclass = "sf") 
  ranges <- c("0", "< 25", "25 - 50", "50 - 75", "> 75")
  
  ggplot() + 
    geom_sf(data = best_freq_sol, aes(group = as.factor(freq_cat), fill = as.factor(freq_cat)), color = NA) +
    geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
    scale_fill_manual(values = pal,
                      name = "Selection Frequency (%)",
                      labels = ranges) +
    ggtitle(basename(dir.layers[i])) +
    theme_opts2 +
    ggsave(paste(outdir, basename(dir.layers[i]), ".pdf", sep = ""), width = 22, height = 10, dpi = 300)
}
stopCluster(cl)










# 
# single <- list.files(path = dir.layers[1], pattern = "*.csv$", full.names = TRUE) %>% 
#   read.csv()
# pu_shpfile <- list.files(path = dir.layers[1], pattern = "*.shp$", full.names = TRUE) %>% 
#   st_read()
# sol_csv <- single %>% 
#   dplyr::mutate(freq_sel = rowSums(single[, 6:15])) %>% 
#   dplyr::select(id, cost, freq_sel)
# freq_base <- sol_csv %>% 
#   mutate(freq_cat = ifelse(freq_sel == 0, 1, 
#                            ifelse(freq_sel > 0 & freq_sel <= 2, 2,
#                                   ifelse(freq_sel > 2 & freq_sel <= 5, 3, 
#                                          ifelse(freq_sel > 5 & freq_sel <= 7, 4, 5)))))
# 
# best_freq_sol <- pu_shpfile[pu_shpfile$id %in% freq_base$id, ] %>% 
#   mutate(freq_cat = freq_base$freq_cat)
# 
# 
# 
# 
# 
# sol_base <- sol_base %>% 
#   mutate(freq_sel = rowSums(sol_base[, 6:15])) %>% 
#   select(id, cost, freq_sel)
# freq_base <- sol_base %>% mutate(freq_cat = ifelse(freq_sel == 0, 1, 
#                                                    ifelse(freq_sel > 0 & freq_sel <= 2, 2,
#                                                           ifelse(freq_sel > 2 & freq_sel <= 5, 3, 
#                                                                  ifelse(freq_sel > 5 & freq_sel <= 7, 4, 5)))))
# 
# shp <- st_read("shapefiles_rasters/02_abnjs_filterdepth/abnj_02-epipelagic_global_moll_05deg_depth/abnj_02-epipelagic_global_moll_05deg_depth.shp")
# best_freq_sol <- shp[shp$layer %in% freq_base$id, ] %>% 
#   mutate(freq_cat = freq_base$freq_cat)
# 
# theme_opts2 <- list(theme(panel.grid.minor = element_blank(),
#                           panel.grid.major = element_blank(),
#                           panel.background = element_rect(fill = "white", colour = "black"),
#                           plot.background = element_rect(fill = "white"),
#                           panel.border = element_blank(),
#                           axis.line = element_line(size = 1),
#                           axis.text.x = element_text(size = rel(2), angle = 0),
#                           axis.text.y = element_text(size = rel(2), angle = 0),
#                           axis.ticks = element_line(size = 1.5),
#                           axis.ticks.length = unit(.25, "cm"), 
#                           axis.title.x = element_blank(),
#                           axis.title.y = element_blank(),
#                           plot.title = element_text(face = "bold", size = 22, hjust = 0.5),
#                           legend.title = element_text(colour = "black", face = "bold", size = 20),
#                           legend.text = element_text(colour = "black", face = "bold", size = 20), 
#                           legend.key.height = unit(1.5, "cm"),
#                           legend.key.width = unit(1, "cm"),
#                           plot.tag = element_text(size = 30, face = "bold")))
# 
# 
# 
# rds <- brewer.pal(4, "YlOrRd")
# pal <- c("#deebf7", rds)
# world_sf <- ne_countries(scale = "medium", returnclass = "sf") 
# ranges <- c("0", "< 25", "25 - 50", "50 - 75", "> 75")
# 
# ggplot() + 
#   geom_sf(data = best_freq_sol, aes(group = as.factor(freq_cat), fill = as.factor(freq_cat)), color = NA) +
#   geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
#   scale_fill_manual(values = pal,
#                     name = "Selection Frequency (%)",
#                     labels = ranges) +
#   ggtitle("Base BLM 0.00022") +
#   theme_opts2 +
#   ggsave("ypdfs/02_EpipelagicLayer_BASE_BLM-0.00022.pdf", width = 22, height = 10, dpi = 300)
# 
# 
# 
# 
# # 
# # 
# # p4 <- ggplot() + 
# #   geom_sf(data = test[[4]], aes(group = as.factor(freq_cat), fill = as.factor(freq_cat)), color = NA) +
# #   geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
# #   coord_sf(xlim = c(-8, 35), ylim = c(30.5, 46)) +
# #   scale_fill_manual(values = pal,
# #                     name = "Selection Frequency (%)",
# #                     labels = ranges) +
# #   ggtitle("Represent VoCC trajectory classes") +
# #   theme_opts2
# # 
# # p4.1 <- ggplot() + 
# #   geom_sf(data = test[[4]], aes(group = as.factor(freq_cat), fill = as.factor(freq_cat)), color = NA) +
# #   geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
# #   coord_sf(xlim = c(5, 20), ylim = c(33, 46)) +
# #   scale_x_continuous(breaks = seq(5, 20, 5), limits = c(-5, 20)) +
# #   scale_y_continuous(breaks = seq(33, 46, 3), limits = c(33, 46)) +
# #   scale_fill_manual(values = pal,
# #                     name = "Selection Frequency (%)",
# #                     labels = ranges) +
# #   theme_opts2
# # 
# # p0_final1 <- ((p1/p4) | (p1.1/p4.1)) +
# #   plot_layout(guides = "collect") +
# #   plot_annotation(tag_levels = 'A', 
# #                   tag_suffix = ')',
# #                   theme = theme(text = element_text(size = 30))) +
# #   theme_opts2
# # 
# # ggsave("zfigs/02_freq-select_adaptation_01.pdf", width = 30, height = 15, dpi = 300)
# # 
# 
# 
# 
# trial <- test[[1]]
# 
# quantile(trial$freq_sel)
