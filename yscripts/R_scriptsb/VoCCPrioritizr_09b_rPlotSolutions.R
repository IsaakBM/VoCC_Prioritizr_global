# This code was written by Isaac Brito-Morales (i.britomorales@uq.edu.au)
# Please do not distribute this code without permission.
# NO GUARANTEES THAT CODE IS CORRECT
# Caveat Emptor!

source("yscripts/R_scriptsb/VoCCPrioritizr_08b_fPlotSolutions.R")

pldList <- list(pld_ep, pld_ep, pld_ep, pld_mp, pld_mp, pld_mp, pld_bap, pld_bap, pld_bap, pld_sflr, pld_sflr, pld_sflr)
provList <- list(lg, lg, lg, glw, glw, glw, glw, glw, glw, sflr, sflr, sflr)
mpaList <- list(mpas_ep, mpas_ep, mpas_ep, mpas_mp, mpas_mp, mpas_mp, mpas_bap, mpas_bap, mpas_bap, mpas_sflr, mpas_sflr, mpas_sflr)
vmeList <- list(vmes_ep, vmes_ep, vmes_ep, vmes_mp, vmes_mp, vmes_mp, vmes_bap, vmes_bap, vmes_bap, vmes_sflr, vmes_sflr, vmes_sflr)

####################################################################################
####### 1A.- Plot Individual Solutions WITH COST
####################################################################################
dir.sol <- list.dirs(path = "Prioritisation/PrioritizrSolutionsCost", full.names = TRUE, recursive = FALSE)
solCostF <- list.files(path = dir.sol, pattern = ".rds", full.names = TRUE)

plotList <- vector("list", length = length(solCostF))
for(i in seq_along(solCostF)){
  plotList[[i]] <- plot_sol(data = solCostF[i], 
                            sfdom = pldList[[i]], 
                            sfprov = provList[[i]], 
                            mpas = mpaList[[i]], 
                            vmes = vmeList[[i]])  
}

p1 <- patchwork::wrap_plots(plotList, ncol = 3, byrow = TRUE) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_prefix = "",
                  tag_levels = "a", 
                  tag_suffix = "",)
ggsave("Figures/MS_v1/BritoMorales_ED_Fi_5.pdf", plot = p1, width = 35, height = 25, dpi = 300, limitsize = FALSE)
ggsave("Figures/MS_v1/BritoMorales_ED_Fi_5.png", plot = p1, width = 35, height = 25, dpi = 300, limitsize = FALSE)

####################################################################################
####### 1B.- Plot Individual Solutions WITH NO COST
####################################################################################
dir.sol <- list.dirs(path = "Prioritisation/PrioritizrSolutionsNCost", full.names = TRUE, recursive = FALSE)
solCostF <- list.files(path = dir.sol, pattern = ".rds", full.names = TRUE)

plotList <- vector("list", length = length(solCostF))
for(i in seq_along(solCostF)){
  plotList[[i]] <- plot_sol(data = solCostF[i], 
                            sfdom = pldList[[i]], 
                            sfprov = provList[[i]], 
                            mpas = mpaList[[i]], 
                            vmes = vmeList[[i]])  
}

p2 <- patchwork::wrap_plots(plotList, ncol = 3, byrow = TRUE) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_prefix = "(",
                  tag_levels = "a", 
                  tag_suffix = ")",)
ggsave("Figures/MS_v1/BritoMorales_ED_Fi_6.pdf", plot = p2, width = 35, height = 25, dpi = 300, limitsize = FALSE)
ggsave("Figures/MS_v1/BritoMorales_ED_Fi_6.png", plot = p2, width = 35, height = 25, dpi = 300, limitsize = FALSE)







