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
  data_NoReg <- function(data1, data2, data3, sfdom) {
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
    
    return(dff)
  }

####################################################################################
####### 4.- Data No Regret Vertical
####################################################################################
# 
  NoRegVer <- function(data1, data2, data3, data4, sfdom, mpas, vmes) {
    
    sfdom <- sfdom %>% 
      dplyr::rename(id = pu)
    
    plg <- sfdom %>% 
      dplyr::mutate(solutionAll = data1$solution*data2$solution*data3$solution)
    ver <- sfdom %>% 
      dplyr::mutate(solutionAll = data1$solution*data2$solution*data3$solution*data4$solution) %>% 
      dplyr::mutate(solutionAll = ifelse(solutionAll == 1, 3, solutionAll))
    
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
                              plot.tag = element_text(size = 25, face = "bold")))
    
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
  
  
  # round((nrow(testNAll[testNAll$solutionAll == 1,])/90065)*100)  # just the epi
  # round((nrow(testAll[testAll$solutionAll == 1,])/90065)*100) # All
  

