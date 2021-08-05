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

data1 <- "Prioritisation/PrioritizrSolutionsCost/features_10100/03_MesopelagicLayer_ssp126_0_1.rds"
sfdom <- pld_mp1
sfprov <- lg
mpas <- mpas_ep
vmes <- vmes_ep

data2 <- "Prioritisation/PrioritizrSolutionsCost/features_10100/03_MesopelagicLayer_ssp245_0_1.rds"
sfdom <- pld_mp
sfprov <- lg
mpas <- mpas_ep
vmes <- vmes_ep

data3 <- "Prioritisation/PrioritizrSolutionsCost/features_10100/03_MesopelagicLayer_ssp585_0_1.rds"
sfdom <- pld_mp
sfprov <- lg
mpas <- mpas_ep
vmes <- vmes_ep

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
####### 4.- Data No Regret climatic by Ocean Layer
####################################################################################
# 
  data_NoReg <- function(data1, data2, data3, data4, sfdom, mpas, vmes) {
    sfdom <- sfdom %>% 
      dplyr::rename(id = pu)
    dff1 <- paste(data1$solution_f, data2$solution_f, data3$solution_f, data4$solution_f, sep = "_")
    sel <- unlist(lapply(dff1, function(x){f <- ifelse(sum(unlist(strsplit(x, "_")) == "0") <= 2, x, 0)})) # individual laer solution == 0
    
    epSF <- sfdom %>% 
      dplyr::mutate(solutions = dff1) %>% 
      dplyr::filter(solutions %in%  c("Epi_Meso_Bathy_0", "Epi_Meso_Bathy_Sea")) %>% 
      as_tibble() %>% 
      dplyr::select(-geometry)
    epSF <- left_join(sfdom, epSF, "id") %>% 
      dplyr::mutate_all(~replace(., is.na(.), 0)) #%>% 
      # dplyr::filter(!(id %in% mpas$id)) %>% 
      # dplyr::filter(!(id %in% vmes$id))
    # round((length(epSF$solutions[epSF$solutions == "Epi_Meso_Bathy_Sea"])/nrow(epSF))*100, 2)
    # round((length(epSF$solutions[epSF$solutions == "Epi_Meso_Bathy_0"])/nrow(epSF))*100, 2)
    
  }

  sfdom <- pld_ep1
  data1 <- epF
  data2 <- mpF
  data3 <- bapF
  data4 <- sflrF
  mpas <- mpas_ep
  vmes <- vmes_ep
  
  sfdom <- sfdom %>% 
    dplyr::rename(id = pu)
  
  plg <- sfdom %>% 
    dplyr::mutate(solutionAll = data1$solution*data2$solution*data3$solution)
  ver <- sfdom %>% 
    dplyr::mutate(solutionAll = data1$solution*data2$solution*data3$solution*data4$solution) %>% 
    dplyr::mutate(solutionAll = ifelse(solutionAll == 1, 3, solutionAll))
    
  final <- sfdom %>% 
    dplyr::mutate(solution = plg$solutionAll+ver$solutionAll) %>% 
    dplyr::filter(!(id %in% mpas$id)) %>% 
    dplyr::filter(!(id %in% vmes$id))
  
  
  round((nrow(testNAll[testNAll$solutionAll == 1,])/90065)*100)  # just the epi
  round((nrow(testAll[testAll$solutionAll == 1,])/90065)*100) # All
  
  
  
  # unique(epF$solution_f)
  # unique(mpF$solution_f)
  # unique(bapF$solution_f)
  # unique(sflrF$solution_f)
  # 
  # epSF <- sfdom %>% 
  #   dplyr::mutate(solutions = sel) %>% 
  #   dplyr::filter(solutions %in%  c("Epi_Meso_Bathy_0", "Epi_Meso_Bathy_Sea")) %>% 
  #   as_tibble() %>% 
  #   dplyr::select(-geometry)
  # epSF <- left_join(sfdom, epSF, "id") %>% 
  #   dplyr::mutate_all(~replace(., is.na(.), 0)) %>% 
  #   dplyr::filter(!(id %in% mpas$id)) %>% 
  #   dplyr::filter(!(id %in% vmes$id))
  # 
  # 
  # length(epSF$solutions[epSF$solutions == "Epi_Meso_Bathy_0"])
  # length(epSF$solutions[epSF$solutions == "Epi_Meso_Bathy_Sea"])
  # 
  # round((length(epSF$solutions[epSF$solutions == "Epi_Meso_Bathy_Sea"])/nrow(epSF))*100, 1)
  # round((length(epSF$solutions[epSF$solutions == "Epi_Meso_Bathy_0"])/nrow(epSF))*100, 1)
  # 
  # unique(epSF$solutions)
  # 
  # a <- epSF %>% 
  #   dplyr::filter(solutions == "Epi_Meso_Bathy_0")
  # b <- epSF %>% 
  #   dplyr::filter(solutions == "Epi_Meso_Bathy_Sea")
  
  
  # Defining generalities
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
    geom_sf(data = final, aes(group = as.factor(solution), fill = as.factor(solution)), color = NA) +
    geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
    scale_fill_manual(values = pal,
                      name = "",
                      labels = ranges) +
    ggtitle("Low regret areas for climate-smart networks") +
    labs(y = "") +
    theme_opts3
  
  ggsave("Figures/MS_v1/BritoMorales_Fi_3.pdf", plot = gg_list, width = 12, height = 6, dpi = 300, limitsize = FALSE)
  ggsave("Figures/MS_v1/BritoMorales_Fi_3.png", plot = gg_list, width = 12, height = 6, dpi = 300, limitsize = FALSE)

