



source("yscripts/R_scriptsb/VoCCPrioritizr_08a_fPlotGeneral.R")


p1 <- plot_VoCC(path = "Inputs/General", world_sf, pldom)
p2 <- plot_RCE(path = "Inputs/General", world_sf, pldom)

p1.1 <- patchwork::wrap_plots(p1, ncol = 3, byrow = TRUE) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_prefix = "(",
                  tag_levels = "a", 
                  tag_suffix = ")",)
ggsave("Figures/MS_v1/BritoMorales_ED_Fi_2.pdf", plot = p1.1, width = 35, height = 25, dpi = 300, limitsize = FALSE)
ggsave("Figures/MS_v1/BritoMorales_ED_Fi_2.png", plot = p1.1, width = 35, height = 25, dpi = 300, limitsize = FALSE)


p1.2 <- patchwork::wrap_plots(p2, ncol = 3, byrow = TRUE) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_prefix = "(",
                  tag_levels = "a", 
                  tag_suffix = ")",)
ggsave("Figures/MS_v1/BritoMorales_ED_Fi_3.pdf", plot = p1.2, width = 35, height = 25, dpi = 300, limitsize = FALSE)
ggsave("Figures/MS_v1/BritoMorales_ED_Fi_3.png", plot = p1.2, width = 35, height = 25, dpi = 300, limitsize = FALSE)




ep <- plot_rich(data = "Output/FeaturesOLayer/epipelagic.csv", sfdom = pld_ep, sfprov = lg)
mp <- plot_rich(data = "Output/FeaturesOLayer/mesopelagic.csv", sfdom = pld_mp, sfprov = glw)
bap <- plot_rich(data = "Output/FeaturesOLayer/BathyAbyssopelagic.csv", sfdom = pld_bap, sfprov = glw)
sflr <- plot_rich(data = "Output/FeaturesOLayer/seafloor.csv", sfdom = pld_sflr, sfprov = sflr)
p1.3 <- patchwork::wrap_plots(ep, mp, bap, sflr, ncol = 1, byrow = FALSE) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_prefix = "(",
                  tag_levels = "a", 
                  tag_suffix = ")",)
ggsave("Figures/MS_v1/BritoMorales_ED_Fi_1.pdf", plot = p1.3, width = 15, height = 25, dpi = 300, limitsize = FALSE)
ggsave("Figures/MS_v1/BritoMorales_ED_Fi_1.png", plot = p1.3, width = 15, height = 25, dpi = 300, limitsize = FALSE)

