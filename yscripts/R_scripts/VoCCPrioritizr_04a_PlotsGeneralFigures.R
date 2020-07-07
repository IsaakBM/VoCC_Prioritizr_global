library(sf)
library(raster)
library(dplyr)
library(ggplot2)
library(tmap)
library(rnaturalearth)
library(rnaturalearthdata)
library(RColorBrewer)
library(patchwork)


# sf objects
  richness <- st_read("features_shapefiles/richness_by_pu/richness_by_pu.shp") %>% 
    mutate(richness_log = log10(richness))
  cost <- st_read("output_datfiles/03-cost-fish_feat-sps-trajclass_ssp245/03-cost-fish_feat-sps-trajclass_ssp245.shp") %>% 
    mutate(cost_log = log10(cost))
  vocc <- st_read("output_datfiles/02-cost-vocc_feat-sps_ssp245/02-cost-vocc_feat-sps_ssp245.shp") %>%
    mutate(vocc = ifelse(cost <= 1, 1, 
                         ifelse(cost > 1 & cost <= 2, 2, 
                                ifelse(cost > 2 & cost <= 3, 3, 
                                       ifelse(cost > 3 & cost <= 4, 4, 
                                              ifelse(cost > 4 & cost <= 5, 5, 
                                                     ifelse(cost > 5 & cost <= 10, 6,
                                                            ifelse(cost > 10 & cost <= 15, 7, 8))))))))
  traj <- st_read("features_shapefiles/trajectories_by_pu/traj_by_pu.shp")
  slow_vocc <- vocc %>% filter(cost <= 1.874664) %>% 
    mutate(ftr_nms = 1)
  
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
                            legend.title = element_text(colour = "black", face = "bold", size = 15),
                            legend.text = element_text(colour = "black", face = "bold", size = 10), 
                            legend.key.height = unit(1, "cm"),
                            legend.key.width = unit(0.8, "cm"),
                            plot.tag = element_text(size = 25, face = "bold")))

# 
  dat2 <- list(cost, richness, vocc, traj, slow_vocc)
  names(dat2) <- c("cost", "richness", "vocc", "traj", "slow")
  world_sf <- ne_countries(scale = "medium", returnclass = "sf")

# color palette
  # pal_vocc <- rev(brewer.pal(4, "RdYlGn"))
  pal_rich <- rev(brewer.pal(9, "RdYlBu"))
  pal_cost <- c("#a1d99b", "#74c476", "#41ab5d",
                "#ffffcc", "#ffeda0", "#fed976", 
                "#d0d1e6", "#0570b0", "#023858")
  pal_vocc <- rev(brewer.pal(8, "Spectral"))

# legend 
  cv_cost <- c("10", "", "100", "", expression(1~x~10^4), "", expression(1~x~10^6), "", expression(1~x~10^8), "")
  cv_vocc <- c("0 - 1", "1 - 2", "2 - 3", "3 - 4",
           "4 - 5", "5 - 10", "10 - 15", "> 15")
  cv_rich <- c("1", "", "", "10", "", "100", "", "", "1000")


p1 <- ggplot() + 
  geom_sf(data = dat2[[1]], aes(fill = cost_log), color = NA) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(-8, 35), ylim = c(30.5, 46)) +
  ggtitle("Opportunity cost") +
  scale_fill_gradientn(name = "Euros",
                       colours = pal_cost,
                       limits = c(0, 9),
                       breaks = seq(0, 9, 1),
                       labels = cv_cost) +
  theme_opts3

p2 <- ggplot() +
  geom_sf(data = dat2[[2]], aes(fill = richness_log), color = NA) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(-8, 35), ylim = c(30.5, 46)) +
  ggtitle("Species richness") +
  scale_fill_gradientn(name = "richness",
                       colours = pal_rich,
                       limits = c(0, 3.2),
                       breaks = seq(0, 3.2, length.out = 9), 
                       labels = cv_rich) +
  theme_opts3

p3 <- ggplot() + 
  geom_sf(data = dat2[[3]], aes(fill = vocc), color = NA) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(-8, 35), ylim = c(30.5, 46)) +
  ggtitle("Climate velocity") +
  scale_fill_gradientn(name = expression(km~yr^-1),
                       colours = pal_vocc,
                       limits = c(1, 8),
                       breaks = seq(1, 8, 1),
                       labels = cv_vocc) +
  theme_opts3

p4 <- ggplot() + 
  geom_sf(data = dat2[[4]], aes(fill = ftr_nms), color = NA) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(-8, 35), ylim = c(30.5, 46)) +
  ggtitle("Climate-velocity trajectory classes") +
  scale_fill_manual(values = c("#fdae61", "#41b6c4", "#a6611a",
                               "#cccccc", "#cb181d", "#bae4bc", "#0570b0"), 
                    name = "Classes",
                    labels = c("Sinks", "Divergence", "Internal sinks",
                               "Non-moving", "Relative sinks", "Slow-moving", "Sources")) +
  theme_opts3

p5 <- ggplot() + 
  geom_sf(data = dat2[[5]], aes(fill = as.factor(ftr_nms)), color = NA) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(-8, 35), ylim = c(30.5, 46)) +
  ggtitle("Slow climate-velocity areas") +
  scale_fill_manual(values = "#31a354", 
                    name = "") +
  theme_opts3 +
  theme(legend.position = "none")


# p0_final1 <- ((p1+p2+p3+p4) | p5) +
#   plot_annotation(tag_levels = 'A', 
#                   tag_suffix = ')',) +
#   theme_opts3

p0_final2 <- ((p3+p4+p2+p1)) +
  plot_annotation(tag_prefix = "(",
                  tag_levels = "a", 
                  tag_suffix = ")",) +
  theme_opts3

ggsave("zfigs/00_general-plots_02c.pdf", width = 20, height = 10, dpi = 300) # original
# ggsave("zfigs/00_general-plots_01d.pdf", width = 40, height = 20, dpi = 300)


cost <- st_read("output_datfiles/02_EpipelagicLayer/pu.shp") 
cost$cost <- ifelse(is.na(cost$cost), 0, cost$cost)
cost <- cost %>% 
  mutate(cost_log = log10(cost + 1))

# cost$cost_log2 <- ifelse(is.infinite(cost$cost_log), cost$cost, cost$cost_log)

cv_cost <- c("1", "10", "100", "1000", "10000")
# pal_cost <- c("#a1d99b", "#74c476", "#41ab5d",
#               "#ffffcc", "#ffeda0", "#fed976", 
#               "#d0d1e6", "#0570b0", "#023858")

pal_cost <- rev(brewer.pal(5, "RdYlBu"))

ggplot() + 
  geom_sf(data = cost, aes(fill = cost_log), color = NA) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  # coord_sf(xlim = c(-8, 35), ylim = c(30.5, 46)) +
  ggtitle("Cost") +
  scale_fill_gradientn(name = "Euros",
                       colours = pal_cost,
                       limits = c(0, 5),
                       breaks = seq(0, 5, length.out = 5),
                       labels = cv_cost,
                       trans = "log") +
  theme_opts3 +
  ggsave("ypdfs/02-epipelagic_Cost_Raster_Sum_01a.pdf", width = 20, height = 10, dpi = 300) # original
