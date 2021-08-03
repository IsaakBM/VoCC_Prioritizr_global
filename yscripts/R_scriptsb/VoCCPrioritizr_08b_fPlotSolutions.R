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
library(kader)
library(exactextractr)
library(nngeo)

source("yscripts/R_scriptsb/VoCCPrioritizr_Help.R")
# Land
world_sf <- ne_countries(scale = "medium", returnclass = "sf") %>% 
  st_transform(crs = CRS(moll))

####################################################################################
####### 1.- Plot Individual Solutions
####################################################################################
# 
  plot_sol <- function(data, sfdom, sfprov, mpas, vmes) {
    d1 <- readRDS(data)
    d2 <- d1[[2]] %>% 
      dplyr::filter(!(id %in% mpas$id)) %>% 
      dplyr::filter(!(id %in% vmes$id)) %>% 
      dplyr::rename(pu = id) %>% 
      as_tibble()
    dff <- left_join(sfdom, d2, "pu") %>% 
      na.omit() %>% 
      dplyr::select(pu, cost, solution_1)
    
    # Defining generalities
    pal <- c("#deebf7", "#31a354")
    ranges <- c("Not Selected", "Selected")
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
                              legend.key.height = unit(2.2, "cm"),
                              legend.key.width = unit(1.4, "cm"),
                              plot.tag = element_text(size = 25, face = "bold")))
    # Create the ggplot
    gg_list <- ggplot() +
      geom_sf(data = dff, aes(group = as.factor(solution_1), fill = as.factor(solution_1)), color = NA) +
      geom_sf(data = sfprov, fill = NA, color = "black", lwd = 1) +
      geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
      scale_fill_manual(values = pal,
                        name = "Selection",
                        labels = ranges) +
      ggtitle(ifelse(stringr::str_detect(string = basename(data), pattern = "Epi"),
              ifelse(stringr::str_detect(basename(data), pattern = "ssp126"), "SSP1-2.6", 
              ifelse(stringr::str_detect(basename(data), pattern = "ssp245"), "SSP2-4.5", "SSP5-8.5")), "")) +
      labs(y = str_remove(ifelse(stringr::str_detect(string = basename(data), pattern = "ssp126"),
                                 unlist(str_split(basename(data), pattern = "_"))[2], ""), pattern = "Layer")) +
      theme_opts3
    return(gg_list)
  }

####################################################################################
####### 2.- Plot Combined CLimatic Solutions
####################################################################################
# 
  plot_CombSol <- function(data1, data2, sfdom, sfprov, mpas, vmes) {
    d1 <- readRDS(data1)
    d2 <- readRDS(data2)
    d1.1 <- d1[[2]] %>% 
      dplyr::filter(!(id %in% mpas$id)) %>% 
      dplyr::filter(!(id %in% vmes$id)) %>% 
      dplyr::rename(pu = id) %>% 
      as_tibble() %>% 
      dplyr::select(pu, cost, solution_1)
    d1.2 <- d2[[2]] %>% 
      dplyr::filter(!(id %in% mpas$id)) %>% 
      dplyr::filter(!(id %in% vmes$id)) %>% 
      dplyr::rename(pu = id) %>% 
      as_tibble() %>% 
      dplyr::select(pu, cost, solution_1)
    d3 <- left_join(d1.1, d1.2, "pu") %>% 
      dplyr::mutate(solution_f = solution_1.x*solution_1.y) %>% 
      dplyr::rename(cost = cost.x) %>% 
      dplyr::select(pu, cost, solution_f)
    dff <- left_join(sfdom, d3, "pu") %>% 
      na.omit() %>% 
      dplyr::select(pu, cost, solution_f)
    
    if(str_detect(basename(data1), "ssp126") & str_detect(basename(data2), "ssp245")) {
      pal <- c("#deebf7", "#41b6c4")
      ranges <- c("Not Selected", "SSP1-2.6 and SSP2-4.5")
      MT <- "SSP1-2.6 and SSP2-4.5"
    } else if(str_detect(basename(data1), "ssp126") & str_detect(basename(data2), "ssp585")) {
      pal <- c("#deebf7", "#88419d")
      ranges <- c("Not Selected", "SSP1-2.6 and SSP5-8.5")
      MT <- "SSP1-2.6 and SSP5-8.5"
    } else if (str_detect(basename(data1), "ssp245") & str_detect(basename(data2), "ssp585")) {
      pal <- c("#deebf7", "#fa9fb5")
      ranges <- c("Not Selected", "SSP2-4.5 and SSP5-8.5")
      MT <- "SSP2-4.5 and SSP5-8.5"
    }
    
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
                              legend.key.height = unit(2.2, "cm"),
                              legend.key.width = unit(1.4, "cm"),
                              plot.tag = element_text(size = 25, face = "bold")))
    
    gg_list <- ggplot() +
      geom_sf(data = dff, aes(group = as.factor(solution_f), fill = as.factor(solution_f)), color = NA) +
      geom_sf(data = sfprov, fill = NA, color = "black", lwd = 1) +
      geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
      scale_fill_manual(values = pal,
                        name = "",
                        labels = ranges) +
      ggtitle(ifelse(stringr::str_detect(string = basename(data1), pattern = "Epi"), MT, "")) +
      labs(y = str_remove(ifelse(str_detect(basename(data1), "ssp126") & str_detect(basename(data2), "ssp245"), 
                                 unlist(str_split(basename(data1), pattern = "_"))[2], ""), pattern = "Layer")) +
      theme_opts3
    return(gg_list)
    
  }


####################################################################################
####### 3.- Plot No Regret climatic 
####################################################################################
# 
  plot_NoReg <- function(data1, data2, data3, sfdom, sfprov, mpas, vmes) {
    d1 <- readRDS(data1)
    d2 <- readRDS(data2)
    d3 <- readRDS(data2)
    d1.1 <- d1[[2]] %>% 
      dplyr::filter(!(id %in% mpas$id)) %>% 
      dplyr::filter(!(id %in% vmes$id)) %>% 
      dplyr::rename(pu = id) %>% 
      as_tibble() %>% 
      dplyr::select(pu, cost, solution_1)
    d1.2 <- d2[[2]] %>% 
      dplyr::filter(!(id %in% mpas$id)) %>% 
      dplyr::filter(!(id %in% vmes$id)) %>% 
      dplyr::rename(pu = id) %>% 
      as_tibble() %>% 
      dplyr::select(pu, cost, solution_1)
    d1.3 <- d3[[2]] %>% 
      dplyr::filter(!(id %in% mpas$id)) %>% 
      dplyr::filter(!(id %in% vmes$id)) %>% 
      dplyr::rename(pu = id) %>% 
      as_tibble() %>% 
      dplyr::select(pu, cost, solution_1)
    
    d4 <- left_join(d1.1, d1.2, "pu")
    d4 <- left_join(d4, d1.3, "pu") %>% 
      dplyr::mutate(solution_f = solution_1.x*solution_1.y*solution_1) %>% 
      dplyr::select(pu, cost, solution_f)
    dff <- left_join(sfdom, d4, "pu") %>% 
      na.omit() %>% 
      dplyr::select(pu, cost, solution_f)
    
    # Defining generalities
    pal <- c("#deebf7", "#0570b0")
    ranges <- c("Not Selected", "Low Regret")
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
                              legend.key.height = unit(2.2, "cm"),
                              legend.key.width = unit(1.4, "cm"),
                              plot.tag = element_text(size = 25, face = "bold")))
    # Create the ggplot
    gg_list <- ggplot() +
      geom_sf(data = dff, aes(group = as.factor(solution_f ), fill = as.factor(solution_f )), color = NA) +
      geom_sf(data = sfprov, fill = NA, color = "black", lwd = 1) +
      geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
      scale_fill_manual(values = pal,
                        name = "Selection",
                        labels = ranges) +
      ggtitle(ifelse(stringr::str_detect(string = basename(data1), pattern = "Epi"), "Low Regret", "")) +
      labs(y = "") +
      theme_opts3
    return(gg_list)
  }


# 
# data <- "Prioritisation/PrioritizrSolutionsNCost/features_10100/02_EpipelagicLayer_ssp126_0_1.rds"
# sfdom <- pld_ep
# sfprov <- lg
# mpas <- mpas_ep
# vmes <- vmes_ep
# 
# data <- "Prioritisation/PrioritizrSolutionsNCost/features_10100/03_MesopelagicLayer_ssp126_0_1.rds"
# sfdom <- pld_mp
# sfprov <- glw
# mpas <- mpas_mp
# vmes <- vmes_mp
# 
# data <- "Prioritisation/PrioritizrSolutionsNCost/02_EpipelagicLayer_ssp126_0_1.rds"
# sfdom <- pld_mp
# sfprov <- glw
# mpas <- mpas_mp
# vmes <- vmes_mp
# 
# data <- "Prioritisation/PrioritizrSolutionsCost/features_10100/02_EpipelagicLayer_ssp126_0_1.rds"
# sfdom <- pld_mp
# sfprov <- glw
# mpas <- mpas_mp
# vmes <- vmes_mp
# 
# 
# 
# 
# data1 <- "Prioritisation/PrioritizrSolutionsCost/features_10100/02_EpipelagicLayer_ssp126_0_1.rds"
# sfdom <- pld_ep
# sfprov <- lg
# mpas <- mpas_ep
# vmes <- vmes_ep
# 
# data2 <- "Prioritisation/PrioritizrSolutionsCost/features_10100/02_EpipelagicLayer_ssp245_0_1.rds"
# sfdom <- pld_ep
# sfprov <- lg
# mpas <- mpas_ep
# vmes <- vmes_ep
# 
# data3 <- "Prioritisation/PrioritizrSolutionsCost/features_10100/02_EpipelagicLayer_ssp585_0_1.rds"
# sfdom <- pld_ep
# sfprov <- lg
# mpas <- mpas_ep
# vmes <- vmes_ep















