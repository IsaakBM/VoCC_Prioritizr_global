# This code was written by Isaac Brito-Morales (i.britomorales@uq.edu.au)
# Please do not distribute this code without permission.
# NO GUARANTEES THAT CODE IS CORRECT
# Caveat Emptor!

source("yscripts/R_scriptsb/VoCCPrioritizr_08c_fPlotNoRegrets.R")

####################################################################################
####### 2.- Plot COMBINED Solutions and No regret by Ocean Layer
####################################################################################

dir.sol <- list.dirs(path = "Prioritisation/PrioritizrSolutionsCost", full.names = TRUE, recursive = FALSE)
solCostF <- list.files(path = dir.sol, pattern = ".rds", full.names = TRUE)

# # Epipelagic
# ep1 <- plot_CombSol(data1 = solCostF[1], data2 = solCostF[2], sfdom = pld_ep, sfprov = lg, mpas = mpas_ep, vmes = vmes_ep)
# ep2 <- plot_CombSol(data1 = solCostF[1], data2 = solCostF[3], sfdom = pld_ep, sfprov = lg, mpas = mpas_ep, vmes = vmes_ep)
# ep3 <- plot_CombSol(data1 = solCostF[2], data2 = solCostF[3], sfdom = pld_ep, sfprov = lg, mpas = mpas_ep, vmes = vmes_ep)
# epF <- plot_NoReg(data1 = solCostF[1], data2 = solCostF[2], data3 = solCostF[3], sfdom = pld_ep, sfprov = lg, mpas = mpas_ep, vmes = vmes_ep)
# epList <- list(ep1, ep2, ep3, epF)
# saveRDS(epList, "Figures/MS_v1_rds/ep_plotList.rds")

# # Mesopelagic
# mp1 <- plot_CombSol(data1 = solCostF[4], data2 = solCostF[5], sfdom = pld_mp, sfprov = glw, mpas = mpas_mp, vmes = vmes_mp)
# mp2 <- plot_CombSol(data1 = solCostF[4], data2 = solCostF[6], sfdom = pld_mp, sfprov = glw, mpas = mpas_mp, vmes = vmes_mp)
# mp3 <- plot_CombSol(data1 = solCostF[5], data2 = solCostF[6], sfdom = pld_mp, sfprov = glw, mpas = mpas_mp, vmes = vmes_mp)
# mpF <- plot_NoReg(data1 = solCostF[4], data2 = solCostF[5], data3 = solCostF[6], sfdom = pld_mp, sfprov = glw, mpas = mpas_mp, vmes = vmes_mp)
# mpList <- list(mp1, mp2, mp3, mpF)
# saveRDS(mpList, "Figures/MS_v1_rds/mp_plotList.rds")

# # Bathy
# bap1 <- plot_CombSol(data1 = solCostF[7], data2 = solCostF[8], sfdom = pld_bap, sfprov = glw, mpas = mpas_bap, vmes = vmes_bap)
# bap2 <- plot_CombSol(data1 = solCostF[7], data2 = solCostF[9], sfdom = pld_bap, sfprov = glw, mpas = mpas_bap, vmes = vmes_bap)
# bap3 <- plot_CombSol(data1 = solCostF[8], data2 = solCostF[9], sfdom = pld_bap, sfprov = glw, mpas = mpas_bap, vmes = vmes_bap)
# bapF <- plot_NoReg(data1 = solCostF[7], data2 = solCostF[8], data3 = solCostF[9], sfdom = pld_bap, sfprov = glw, mpas = mpas_bap, vmes = vmes_bap)
# bapList <- list(bap1, bap2, bap3, bapF)
# saveRDS(bapList, "Figures/MS_v1_rds/bap_plotList.rds")

# Seafloor
# sflr1 <- plot_CombSol(data1 = solCostF[10], data2 = solCostF[11], sfdom = pld_sflr, sfprov = sflr, mpas = mpas_sflr, vmes = vmes_sflr)
# sflr2 <- plot_CombSol(data1 = solCostF[10], data2 = solCostF[12], sfdom = pld_sflr, sfprov = sflr, mpas = mpas_sflr, vmes = vmes_sflr)
# sflr3 <- plot_CombSol(data1 = solCostF[11], data2 = solCostF[12], sfdom = pld_sflr, sfprov = sflr, mpas = mpas_sflr, vmes = vmes_sflr)
# sflrF <- plot_NoReg(data1 = solCostF[10], data2 = solCostF[11], data3 = solCostF[12], sfdom = pld_sflr, sfprov = sflr, mpas = mpas_sflr, vmes = vmes_sflr)
# sflrList <- list(sflr1, sflr2, sflr3, sflrF)
# saveRDS(sflrList, "Figures/MS_v1_rds/sflr_plotList.rds")

epL <- readRDS("Figures/MS_v1_rds/ep_plotList.rds")
mpL <- readRDS("Figures/MS_v1_rds/mp_plotList.rds")
bapL <- readRDS("Figures/MS_v1_rds/bap_plotList.rds")
sflrL <- readRDS("Figures/MS_v1_rds/sflr_plotList.rds")
all_layers <- c(epL, mpL, bapL, sflrL)

p3 <- patchwork::wrap_plots(all_layers, ncol = 4, byrow = TRUE) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_prefix = "(",
                  tag_levels = "a", 
                  tag_suffix = ")",)

ggsave("Figures/MS_v1/BritoMorales_Fi_2.pdf", plot = p3, width = 43, height = 23, dpi = 300, limitsize = FALSE)
ggsave("Figures/MS_v1/BritoMorales_Fi_2.png", plot = p3, width = 43, height = 23, dpi = 300, limitsize = FALSE)

####################################################################################
####### 3.- Plot No Regret Across Climate and Ocean Layer
####################################################################################
dir.sol <- list.dirs(path = "Prioritisation/PrioritizrSolutionsCost", full.names = TRUE, recursive = FALSE)
solCostF <- list.files(path = dir.sol, pattern = ".rds", full.names = TRUE)

epF <- data_NoReg(data1 = solCostF[1], data2 = solCostF[2], data3 = solCostF[3], sfdom = pld_ep1, 
                  outdir = "SummStats/PrioritizrSolutionsCost/features_10100/")
mpF <- data_NoReg(data1 = solCostF[4], data2 = solCostF[5], data3 = solCostF[6], sfdom = pld_mp1, 
                  outdir = "SummStats/PrioritizrSolutionsCost/features_10100/")
bapF <- data_NoReg(data1 = solCostF[7], data2 = solCostF[8], data3 = solCostF[9], sfdom = pld_bap1, 
                   outdir = "SummStats/PrioritizrSolutionsCost/features_10100/")
sflrF <- data_NoReg(data1 = solCostF[10], data2 = solCostF[11], data3 = solCostF[12], sfdom = pld_sflr1, 
                    outdir = "SummStats/PrioritizrSolutionsCost/features_10100/")

all <- NoRegVer(data1 = epF, data2 = mpF, data3 = bapF, data4 = sflrF, sfdom = pld_ep1, mpas = mpas_ep, vmes = vmes_ep, 
                outdir = "SummStats/PrioritizrSolutionsCost/features_10100/") 

ggsave("Figures/MS_v1/BritoMorales_Fi_3.pdf", plot = all, width = 12, height = 6, dpi = 300, limitsize = FALSE)
ggsave("Figures/MS_v1/BritoMorales_Fi_3.png", plot = all, width = 12, height = 6, dpi = 300, limitsize = FALSE)






