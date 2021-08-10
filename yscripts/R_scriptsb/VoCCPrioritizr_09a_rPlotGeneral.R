# This code was written by Isaac Brito-Morales (i.britomorales@uq.edu.au)
# Please do not distribute this code without permission.
# NO GUARANTEES THAT CODE IS CORRECT
# Caveat Emptor!

source("yscripts/R_scriptsb/VoCCPrioritizr_08a_fPlotGeneral.R")

####################################################################################
####### 0.- BritoMorales_ED_Fi_1
####################################################################################
# Richness
  ep_rich <- plot_rich(data = "Output/FeaturesOLayer/epipelagic.csv", sfdom = pld_ep, sfprov = lg) + theme(legend.position = "none")
  mp_rich <- plot_rich(data = "Output/FeaturesOLayer/mesopelagic.csv", sfdom = pld_mp, sfprov = glw)
  bap_rich <- plot_rich(data = "Output/FeaturesOLayer/BathyAbyssopelagic.csv", sfdom = pld_bap, sfprov = glw) + theme(legend.position = "none")
  sflr_rich <- plot_rich(data = "Output/FeaturesOLayer/seafloor_sps.csv", sfdom = pld_sflr, sfprov = sflr) + theme(legend.position = "none")
# Cost
  ep_cost <- plot_cost(data = "Output/CostOLayer/02-epipelagic_CostSFTotal.rds", sfprov = lg) + theme(legend.position = "none")
  mp_cost <- plot_cost(data = "Output/CostOLayer/03-mesopelagic_CostSFTotal.rds", sfprov = glw)
  bap_cost <- plot_cost(data = "Output/CostOLayer/04-bathyabyssopelagic_CostSFTotal.rds", sfprov = glw) + theme(legend.position = "none")
  sflr_cost <- plot_cost(data = "Output/CostOLayer/05-seafloorCostSFTotal.rds", sfprov = sflr) + theme(legend.position = "none")
# 
  pED01 <- patchwork::wrap_plots(ep_cost, mp_cost, bap_cost, sflr_cost, 
                                 ep_rich, mp_rich, bap_rich, sflr_rich, ncol = 2, byrow = FALSE) +
    plot_annotation(tag_prefix = "",
                    tag_levels = "a", 
                    tag_suffix = "",)
  ggsave("Figures/MS_v1/BritoMorales_ED_Fi_1.pdf", plot = pED01, width = 30, height = 25, dpi = 300, limitsize = FALSE)
  ggsave("Figures/MS_v1/BritoMorales_ED_Fi_1.png", plot = pED01, width = 30, height = 25, dpi = 300, limitsize = FALSE)

####################################################################################
####### 0.- BritoMorales_ED_Fi_2
####################################################################################
# Plot Geomorphic Features
  geom_sflr <- plot_GeomF(path = "Output/FeaturesPUs/06_GeomorphicFeatures", sfprov = sflr)
  pED02 <- patchwork::wrap_plots(geom_sflr, ncol = 4, byrow = TRUE) +
    plot_annotation(tag_prefix = "",
                    tag_levels = "a", 
                    tag_suffix = "",)
  ggsave("Figures/MS_v1/BritoMorales_ED_Fi_2.pdf", plot = pED02, width = 28, height = 12, dpi = 300, limitsize = FALSE)
  ggsave("Figures/MS_v1/BritoMorales_ED_Fi_2.png", plot = pED02, width = 28, height = 12, dpi = 300, limitsize = FALSE)

####################################################################################
####### 0.- BritoMorales_ED_Fi_3
####################################################################################
# VoCC
  p1 <- plot_VoCC(path = "Output/ClimateChange", world_sf, pldom)
  p1.1 <- patchwork::wrap_plots(p1, ncol = 3, byrow = TRUE) +
    plot_layout(guides = "collect") +
    plot_annotation(tag_prefix = "",
                    tag_levels = "a", 
                    tag_suffix = "",)
  ggsave("Figures/MS_v1/BritoMorales_ED_Fi_3.pdf", plot = p1.1, width = 35, height = 25, dpi = 300, limitsize = FALSE)
  ggsave("Figures/MS_v1/BritoMorales_ED_Fi_3.png", plot = p1.1, width = 35, height = 25, dpi = 300, limitsize = FALSE)

####################################################################################
####### 0.- BritoMorales_ED_Fi_4
####################################################################################  
# RCE
  p2 <- plot_RCE(path = "Output/ClimateChange", world_sf, pldom)
  p1.2 <- patchwork::wrap_plots(p2, ncol = 3, byrow = TRUE) +
    plot_layout(guides = "collect") +
    plot_annotation(tag_prefix = "",
                    tag_levels = "a", 
                    tag_suffix = "",)
  ggsave("Figures/MS_v1/BritoMorales_ED_Fi_4.pdf", plot = p1.2, width = 35, height = 25, dpi = 300, limitsize = FALSE)
  ggsave("Figures/MS_v1/BritoMorales_ED_Fi_4.png", plot = p1.2, width = 35, height = 25, dpi = 300, limitsize = FALSE)





