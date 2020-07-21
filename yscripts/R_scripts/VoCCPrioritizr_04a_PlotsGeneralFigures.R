library(sf)
library(raster)
library(dplyr)
library(ggplot2)
library(tmap)
library(rnaturalearth)
library(rnaturalearthdata)
library(RColorBrewer)
library(patchwork)
library(foreach)
library(doParallel)
library(data.table)

path = "wgeneral_figs"
outdir = "wgeneral_figs/"
#### Cost 
#### Reading features shapefiles
dir.layers <- paste(list.dirs(path = path, full.names = TRUE, recursive = FALSE), sep = "/")
files_shp <- list.files(paste(list.dirs(path = dir.layers[3], full.names = TRUE, recursive = FALSE), sep = "/"), pattern = ".shp", full.names = TRUE)
cost_shp_files <- lapply(files_shp, function(x) {
  single <- st_read(x)
  final <- single %>% 
    dplyr::mutate(cost = ifelse(is.na(cost), 0, cost)) %>% 
    mutate(cost = round(cost))
  final$cost <- ifelse(final$cost == 0, median(filter(final, final$cost != 0)$cost), final$cost)
  final <- final %>% 
    dplyr::mutate(cost_log = log10(cost))
  final <- final})

# Begin the parallel structure
UseCores <- detectCores() -1
cl <- makeCluster(UseCores)  
registerDoParallel(cl)
foreach(i = 1:length(cost_shp_files), .packages = c("sf", "raster", "dplyr", "ggplot2", "rnaturalearth", "rnaturalearthdata", "RColorBrewer", "patchwork")) %dopar% { 
  
  # Defining generalities
    # pal_cost <- rev(brewer.pal(6, "YlGn"))
    pal_cost <- c("#a1d99b", "#74c476",
                  "#ffffcc", "#ffeda0", 
                  "#d0d1e6", "#0570b0")
    cv_cost <- c("1", "10", "100", "1000", expression(1~x~10^4), expression(1~x~10^5))
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
                              legend.title = element_text(colour = "black", face = "bold", size = 15),
                              legend.text = element_text(colour = "black", face = "bold", size = 10), 
                              legend.key.height = unit(1, "cm"),
                              legend.key.width = unit(0.8, "cm"),
                              plot.tag = element_text(size = 25, face = "bold")))
  # Plotting the figures
    ggplot() + 
      geom_sf(data = cost_shp_files[[i]], aes(fill = cost_log), color = NA) +
      geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
      ggtitle("Cost") +
      scale_fill_gradientn(name = "USD",
                           colours = pal_cost,
                           limits = c(0, 5),
                           breaks = seq(0, 5, length.out = 6),
                           labels = cv_cost) +
      ggtitle(basename(files_shp[i])) +
      theme_opts3 +
      ggsave(paste(outdir, basename(files_shp[i]), ".pdf", sep = ""), width = 22, height = 10, dpi = 300)
}
stopCluster(cl)

#### VoCC
#### Reading features shapefiles
dir.layers <- paste(list.dirs(path = path, full.names = TRUE, recursive = FALSE), sep = "/")
files_vocc <- list.files(paste(list.dirs(path = dir.layers[2], full.names = TRUE, recursive = FALSE), sep = "/"), pattern = ".csv", full.names = TRUE)
vocc_csv_files <- lapply(files_vocc, function(x) {
  single <- read.csv(x)
  final <- single %>% 
    dplyr::select(-X) %>% 
    dplyr::arrange(pu)
  final$climate_feature <- ifelse(is.na(final$climate_feature), median(filter(final, final$climate_feature != 0)$climate_feature), final$climate_feature)
  final <- final})
# Creating general files
vocc_shp_ep_ssp126 <- vocc_shp_ep_ssp245 <- cost_shp_files[[1]]
vocc_shp_mp_ssp126 <- vocc_shp_mp_ssp245 <- cost_shp_files[[2]]
vocc_shp_bap_ssp126 <- vocc_shp_bap_ssp245 <- cost_shp_files[[3]]
# Epipelagic
vocc_shp_ep_ssp126$vocc <- vocc_csv_files[[1]]$climate_feature
vocc_shp_ep_ssp245$vocc <- vocc_csv_files[[2]]$climate_feature
# Mesopelagic
vocc_shp_mp_ssp126$vocc <- vocc_csv_files[[3]]$climate_feature
vocc_shp_mp_ssp245$vocc <- vocc_csv_files[[4]]$climate_feature
# Bathyabyssopelagic
vocc_shp_bap_ssp126$vocc <- vocc_csv_files[[5]]$climate_feature
vocc_shp_bap_ssp245$vocc <- vocc_csv_files[[6]]$climate_feature

vocc_shp_files <- list(vocc_shp_ep_ssp126, vocc_shp_ep_ssp245,
                       vocc_shp_mp_ssp126, vocc_shp_mp_ssp245,
                       vocc_shp_bap_ssp126, vocc_shp_bap_ssp245)

# ranges_vocc <- lapply(vocc_shp_files, function(x) {
#   single <- x 
#   ranges_vocc <- range(single$vocc)})


vocc_shp_files2 <- lapply(vocc_shp_files, function(x) { 
  single <- x %>% 
    dplyr::mutate(vocc_dec = vocc*10)
  vocc <- single %>% 
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
  })


# Begin the parallel structure
UseCores <- detectCores() -1
cl <- makeCluster(UseCores)  
registerDoParallel(cl)
foreach(i = 1:length(vocc_shp_files2), .packages = c("sf", "raster", "dplyr", "ggplot2", "rnaturalearth", "rnaturalearthdata", "RColorBrewer", "patchwork")) %dopar% { 
  
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
                            legend.title = element_text(colour = "black", face = "bold", size = 15),
                            legend.text = element_text(colour = "black", face = "bold", size = 10), 
                            legend.key.height = unit(1, "cm"),
                            legend.key.width = unit(0.8, "cm"),
                            plot.tag = element_text(size = 25, face = "bold")))
  # Plotting the figures
  ggplot() + 
    geom_sf(data = vocc_shp_files2[[i]], aes(fill = vocc_categ), color = NA) +
    geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
    ggtitle("Climate velocity") +
    scale_fill_gradientn(name = expression(km~dec^-1),
                         colours = pal_vocc,
                         limits = c(1, 11),
                         breaks = seq(1, 11, 1),
                         labels = cv_vocc) +
    ggtitle(basename(files_vocc[i])) +
    theme_opts3 +
    ggsave(paste(outdir, basename(files_vocc[i]), ".pdf", sep = ""), width = 22, height = 10, dpi = 300)
}
stopCluster(cl)

#### RCE
#### Reading features shapefiles
dir.layers <- paste(list.dirs(path = path, full.names = TRUE, recursive = FALSE), sep = "/")
files_rce <- list.files(paste(list.dirs(path = dir.layers[1], full.names = TRUE, recursive = FALSE), sep = "/"), pattern = ".csv", full.names = TRUE)
rce_csv_files <- lapply(files_rce, function(x) {
  single <- read.csv(x)
  final <- single %>% 
    dplyr::select(-X) %>% 
    dplyr::arrange(pu)
  final$climate_feature <- ifelse(is.na(final$climate_feature), median(filter(final, final$climate_feature != 0)$climate_feature), final$climate_feature)
  final <- final})

# Creating general files
rce_shp_ep_ssp126 <- rce_shp_ep_ssp245 <- cost_shp_files[[1]]
rce_shp_mp_ssp126 <- rce_shp_mp_ssp245 <- cost_shp_files[[2]]
rce_shp_bap_ssp126 <- rce_shp_bap_ssp245 <- cost_shp_files[[3]]
# Epipelagic
rce_shp_ep_ssp126$rce <- rce_csv_files[[1]]$climate_feature
rce_shp_ep_ssp245$rce <- rce_csv_files[[2]]$climate_feature
# Mesopelagic
rce_shp_mp_ssp126$rce <- rce_csv_files[[3]]$climate_feature
rce_shp_mp_ssp245$rce <- rce_csv_files[[4]]$climate_feature
# Bathyabyssopelagic
rce_shp_bap_ssp126$rce <- rce_csv_files[[5]]$climate_feature
rce_shp_bap_ssp245$rce <- rce_csv_files[[6]]$climate_feature

rce_shp_files <- list(rce_shp_ep_ssp126, rce_shp_ep_ssp245,
                       rce_shp_mp_ssp126, rce_shp_mp_ssp245,
                       rce_shp_bap_ssp126, rce_shp_bap_ssp245)

ranges_rce <- lapply(rce_shp_files, function(x) {
  single <- x
  ranges_rce <- range(single$rce)})

kader:::cuberoot(min(unlist(ranges_rce)))
kader:::cuberoot(max(unlist(ranges_rce)))

rce_shp_files2 <- lapply(rce_shp_files, function(x) { 
  single <- x %>% 
    dplyr::mutate(croot_rce = kader:::cuberoot(rce))
  vocc <- single %>% 
    dplyr::mutate(rce_categ = ifelse(croot_rce <= 0.2, 1, 
                                     ifelse(croot_rce > 0.2 & croot_rce <= 0.4, 2, 
                                            ifelse(croot_rce > 0.4 & croot_rce <= 0.6, 3, 
                                                   ifelse(croot_rce > 0.6 & croot_rce <= 0.8, 4, 
                                                          ifelse(croot_rce > 0.8 & croot_rce <= 1.1, 5, 
                                                                 ifelse(croot_rce > 1.1 & croot_rce <= 1.2, 6, 
                                                                        ifelse(croot_rce > 1.2 & croot_rce <= 1.5, 7, 8)))))))) # add 11 category
})

# Begin the parallel structure
UseCores <- detectCores() -1
cl <- makeCluster(UseCores)  
registerDoParallel(cl)
foreach(i = 1:length(rce_shp_files2), .packages = c("sf", "raster", "dplyr", "ggplot2", "rnaturalearth", "rnaturalearthdata", "RColorBrewer", "patchwork")) %dopar% { 
  
  # Defining generalities
  pal_rce <- rev(brewer.pal(11, "Spectral"))
  cv_rce <- c("0", "", "", "", "", "", "", "> 1.5")
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
                            legend.title = element_text(colour = "black", face = "bold", size = 15),
                            legend.text = element_text(colour = "black", face = "bold", size = 10), 
                            legend.key.height = unit(1, "cm"),
                            legend.key.width = unit(0.8, "cm"),
                            plot.tag = element_text(size = 25, face = "bold")))
  # Plotting the figures
  ggplot() + 
    geom_sf(data = rce_shp_files2[[i]], aes(fill = rce_categ), color = NA) +
    geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
    ggtitle("RCE index") +
    scale_fill_gradientn(name = "RCE index",
                         colours = pal_rce,
                         limits = c(1, 8),
                         breaks = seq(1, 8, 1),
                         labels = cv_rce) +
    ggtitle(basename(files_rce[i])) +
    theme_opts3 +
    ggsave(paste(outdir, basename(files_rce[i]), ".pdf", sep = ""), width = 22, height = 10, dpi = 300)
}
stopCluster(cl)


