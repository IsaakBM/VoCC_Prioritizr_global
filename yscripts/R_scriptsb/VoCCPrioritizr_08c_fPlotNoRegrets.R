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

####################################################################################
####### 3.- Data No Regret climatic by Ocean Layer
####################################################################################
# 
  data_NoReg <- function(data1, data2, data3, sfdom, outdir) {
    d1 <- readRDS(data1)
    d2 <- readRDS(data2)
    d3 <- readRDS(data2)
    
    sfdom <- sfdom %>% 
      dplyr::rename(id = pu)
      
    ssp126 <- dplyr::left_join(x = sfdom, y = d1[[2]],  by = "id") %>% 
      dplyr::mutate(solution = ifelse(is.na(solution_1), 0, solution_1)) %>% 
      as_tibble() %>% 
      dplyr::rename(pu = id) %>% 
      dplyr::select(pu, cost, solution) %>% 
      dplyr::arrange(pu)
    ssp245 <- dplyr::left_join(x = sfdom, y = d2[[2]],  by = "id") %>% 
      dplyr::mutate(solution = ifelse(is.na(solution_1), 0, solution_1)) %>% 
      as_tibble() %>% 
      dplyr::rename(pu = id) %>% 
      dplyr::select(pu, cost, solution) %>% 
      dplyr::arrange(pu)
    ssp585 <- dplyr::left_join(x = sfdom, y = d3[[2]],  by = "id") %>% 
      dplyr::mutate(solution = ifelse(is.na(solution_1), 0, solution_1)) %>% 
      as_tibble() %>% 
      dplyr::rename(pu = id) %>% 
      dplyr::select(pu, cost, solution) %>% 
      dplyr::arrange(pu)
    
    dff <- sfdom %>% 
      dplyr::mutate(solution = ssp126$solution*ssp245$solution*ssp585$solution) %>% 
      as_tibble() %>% 
      dplyr::select(-geometry) %>%
      dplyr::arrange(id)
    
  # Write file for later
    ns <- paste("NoRegret", 
                str_remove_all(unlist(str_split(basename(data1), "_"))[2], "Layer"), 
                paste0(unlist(str_split(basename(data1), "_"))[3], unlist(str_split(basename(data2), "_"))[3], unlist(str_split(basename(data3), "_"))[3]), 
                sep = "_")
    saveRDS(dff, paste(outdir, paste0(ns, ".rds")))
    return(dff)
  }

####################################################################################
####### 4.- Data No Regret Vertical
####################################################################################
# 
  NoRegVer <- function(data1, data2, data3, data4, sfdom, mpas, vmes, outdir) {
    
    sfdom <- sfdom %>% 
      dplyr::rename(id = pu)
    
    plg <- sfdom %>% 
      dplyr::mutate(solutionAll = data1$solution*data2$solution*data3$solution)
    ver <- sfdom %>% 
      dplyr::mutate(solutionAll = data1$solution*data2$solution*data3$solution*data4$solution) %>% 
      dplyr::mutate(solutionAll = ifelse(solutionAll == 1, 3, solutionAll))
  # Write file for later
    saveRDS(plg %>% 
              as_tibble() %>% 
              dplyr::select(-geometry), paste0(outdir, paste0("NoRegretPelagic", ".rds")))
    saveRDS(ver %>% 
              as_tibble() %>% 
              dplyr::select(-geometry), paste0(outdir, paste0("NoRegret_PelagicSeafloor", ".rds")))
    
    ff <- sfdom %>% 
      dplyr::mutate(solution = plg$solutionAll+ver$solutionAll) %>% 
      dplyr::filter(!(id %in% mpas$id)) %>% 
      dplyr::filter(!(id %in% vmes$id))
    
    # Defining generalities
    brd <- sfdom %>% 
      summarise(borders = sum(id, do_union = TRUE))
    pal <- c("#deebf7", "#ec7014", "#0570b0")
    ranges <- c("Not Selected", "Pelagic (12%)", "Pelagic + Seafloor (6%)")
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
                              plot.title = element_text(face = "plain", size = 18, hjust = 0.5),
                              legend.title = element_text(colour = "black", face = "bold", size = 25),
                              legend.text = element_text(colour = "black", face = "plain", size = 10),
                              legend.key.height = unit(0.8, "cm"),
                              legend.key.width = unit(0.7, "cm"),
                              plot.tag = element_text(size = 15, face = "bold")))
    
    # Create the ggplot
    gg_list <- ggplot() +
      geom_sf(data = ff, aes(group = as.factor(solution), fill = as.factor(solution)), color = NA) +
      geom_sf(data = brd, fill = NA, color = "black", lwd = 0.3) +
      geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
      scale_fill_manual(values = pal,
                        name = "",
                        labels = ranges) +
      ggtitle("Low regret areas for climate-smart networks") +
      labs(y = "") +
      theme_opts3
    
    return(gg_list)
  }
  
####################################################################################
####### 5.- Lat distribution
####################################################################################
# 
  lat_plot <- function(data, sfdom, mpas, vmes) {
    
    d1 <- readRDS(data) %>% 
      dplyr::mutate(solution = ifelse(solutionAll != 0, 1, 0)) %>% 
      dplyr::select(id, solution) %>% 
      dplyr::rename(pu = id)
    
    d2 <- left_join(d1, sfdom, "pu") %>% 
      cbind(st_coordinates(st_centroid(sfdom))) %>% 
      as_tibble() %>% 
      dplyr::select(pu, solution, X, Y) %>% 
      dplyr::rename(id = pu) %>% 
      dplyr::filter(!(id %in% mpas$id)) %>% 
      dplyr::filter(!(id %in% vmes$id)) %>% 
      dplyr::rename(pu = id)
    
    d3 <- d2 %>% 
      dplyr::group_by(Y) %>% 
      dplyr::summarise(cells = length(solution), counts = sum(solution)) %>% 
      dplyr::arrange(Y) %>% 
      as_tibble() %>% 
      dplyr::select(Y, cells, counts) %>% 
      dplyr::mutate(p_lat = (counts/nrow(dplyr::filter(d2, solution == 1)))*100)
    
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
                              plot.title = element_text(face = "plain", size = 18, hjust = 0.5),
                              legend.title = element_text(colour = "black", face = "bold", size = 25),
                              legend.text = element_text(colour = "black", face = "plain", size = 10),
                              legend.key.height = unit(0.8, "cm"),
                              legend.key.width = unit(0.7, "cm"),
                              plot.tag = element_text(size = 15, face = "bold")))
    
    if(str_detect(basename(data), "Pelagic.rds") == FALSE) {
      gg_lat <- ggplot(data = d3, aes(x = as.factor(Y), y = (p_lat))) +
        geom_bar(aes(x = as.factor(Y), y = p_lat, fill = "#bdbdbd"), colour = "#0570b0", stat = "identity", position = position_dodge()) +
        coord_flip() +
        scale_y_continuous(breaks = seq(0, 1.5, 0.5), limits = c(0, 1.5), expand = c(0, 0), labels = function(x) abs(x)) +
        labs(y = "") +
        theme_minimal() +
        scale_fill_manual(values = c("#0570b0")) +
        theme_opts3
      
    } else {
      gg_lat <- ggplot(data = d3, aes(x = as.factor(Y), y = (p_lat)*-1)) +
        geom_bar(aes(x = as.factor(Y), y = p_lat*-1, fill = "#bdbdbd"), colour = "#ec7014", stat = "identity", position = position_dodge()) +
        coord_flip() +
        scale_y_continuous(breaks = seq(-1.5, 0, 0.5), limits = c(-1.5,0), expand = c(0, 0), labels = function(x) abs(x)) +
        labs(y = "") +
        theme_minimal() +
        scale_fill_manual(values = c("#ec7014")) +
        theme_opts3
    }
    return(gg_lat)
  }
  