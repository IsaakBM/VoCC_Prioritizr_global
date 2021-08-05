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

data1 <- "Prioritisation/PrioritizrSolutionsCost/features_10100/02_EpipelagicLayer_ssp126_0_1.rds"
sfdom <- pld_ep
sfprov <- lg
mpas <- mpas_ep
vmes <- vmes_ep

data2 <- "Prioritisation/PrioritizrSolutionsCost/features_10100/02_EpipelagicLayer_ssp245_0_1.rds"
sfdom <- pld_ep
sfprov <- lg
mpas <- mpas_ep
vmes <- vmes_ep

data3 <- "Prioritisation/PrioritizrSolutionsCost/features_10100/02_EpipelagicLayer_ssp585_0_1.rds"
sfdom <- pld_ep
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
    d1.1 <- d1[[2]] %>%
      dplyr::rename(pu = id) %>% 
      as_tibble() %>% 
      dplyr::select(pu, cost, solution_1)
    d1.2 <- d2[[2]] %>%
      dplyr::rename(pu = id) %>% 
      as_tibble() %>% 
      dplyr::select(pu, cost, solution_1)
    d1.3 <- d3[[2]] %>% 
      dplyr::rename(pu = id) %>% 
      as_tibble() %>% 
      dplyr::select(pu, cost, solution_1)
    
    d4 <- left_join(d1.1, d1.2, "pu")
    d4 <- left_join(d4, d1.3, "pu") %>% 
      dplyr::mutate(solution_f = solution_1.x*solution_1.y*solution_1) %>% 
      dplyr::select(pu, cost, solution_f)
    dff <- left_join(sfdom, d4, "pu") %>% 
      dplyr::mutate_all(~replace(., is.na(.), 0)) %>% 
      dplyr::select(pu, cost, solution_f) %>% 
      as_tibble() %>% 
      dplyr::select(-geometry) %>% 
      dplyr::mutate(solution_f = ifelse(solution_f == 1 & str_detect(basename(data1), "Epi"), "Epi",
                                 ifelse(solution_f == 1 & str_detect(basename(data1), "Meso"), "Meso", 
                                 ifelse(solution_f == 1 & str_detect(basename(data1), "Bathy"), "Bathy", 
                                 ifelse(solution_f == 1 & str_detect(basename(data1), "Sea"), "Sea", 0)))))
    return(dff)
  }

####################################################################################
####### 4.- Data No Regret climatic by Ocean Layer
####################################################################################
# 
  data_NoReg <- function(data1, data2, data3, data4, sfdom) {
    sfdom <- pld_ep1 %>% 
      dplyr::rename(id = pu)
    dff1 <- paste(data1$solution_f, data2$solution_f, data3$solution_f, data4$solution_f, sep = "_")
    sel <- unlist(lapply(dff1, function(x){f <- ifelse(sum(unlist(strsplit(x, "_")) == "0") <= 2, x, 0)})) # individual laer solution == 0
    
    epSF <- sfdom %>% 
      dplyr::mutate(solutions = sel) %>% 
      dplyr::filter(solutions %in%  c("Epi_Meso_Bathy_0", "Epi_Meso_Bathy_Sea")) %>% 
      as_tibble() %>% 
      dplyr::select(-geometry)
    epSF <- left_join(sfdom, epSF, "id") %>% 
      dplyr::mutate_all(~replace(., is.na(.), 0)) %>% 
      dplyr::filter(!(id %in% mpas$id)) %>% 
      dplyr::filter(!(id %in% vmes$id))
    # round((length(epSF$solutions[epSF$solutions == "Epi_Meso_Bathy_Sea"])/nrow(epSF))*100, 2)
    # round((length(epSF$solutions[epSF$solutions == "Epi_Meso_Bathy_0"])/nrow(epSF))*100, 2)
    
  }


  data1 <- epF
  data2 <- mpF
  data3 <- bapF
  data4 <- sflrF
  
  
  
  epSF <- sfdom %>% 
    dplyr::mutate(solutions = sel) %>% 
    dplyr::filter(solutions %in%  c("Epi_Meso_Bathy_0", "Epi_Meso_Bathy_Sea")) %>% 
    as_tibble() %>% 
    dplyr::select(-geometry)
  epSF <- left_join(sfdom, epSF, "id") %>% 
    dplyr::mutate_all(~replace(., is.na(.), 0)) %>% 
    dplyr::filter(!(id %in% mpas$id)) %>% 
    dplyr::filter(!(id %in% vmes$id))
  
  round((length(epSF$solutions[epSF$solutions == "Epi_Meso_Bathy_Sea"])/nrow(epSF))*100, 2)
  round((length(epSF$solutions[epSF$solutions == "Epi_Meso_Bathy_0"])/nrow(epSF))*100, 2)
  
  # Defining generalities
  pal <- c("#deebf7", "#ec7014", "#0570b0")
  ranges <- c("Not Selected", "Pelagic Low Regret", "Vertical Low Regret")
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
    geom_sf(data = epSF, aes(group = as.factor(solutions), fill = as.factor(solutions)), color = NA) +
    geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
    scale_fill_manual(values = pal,
                      name = "",
                      labels = ranges) +
    ggtitle("Climate-smart network ") +
    labs(y = "") +
    theme_opts3
  
  ggsave("Figures/MS_v1/BritoMorales_Fi_3.pdf", plot = gg_list, width = 12, height = 6, dpi = 300, limitsize = FALSE)
  ggsave("Figures/MS_v1/BritoMorales_Fi_3.png", plot = gg_list, width = 12, height = 6, dpi = 300, limitsize = FALSE)

