library(raster)
library(sf)
library(data.table)
library(dplyr)
library(prioritizr)
library(gurobi)
library(spatstat)
library(reldist)

pu <- read.table("output_datfiles/pu.dat", sep = ",", header = TRUE)
features <- read.table("output_datfiles/spec.dat", sep = ",", header = TRUE)
bound <- read.table("output_datfiles/bound.dat", sep = ",", header = TRUE)
rij <- read.table("output_datfiles/puvsp.dat", sep = ",", header = TRUE)

pu <- read.table("output_prioritizr_blm-cal/02_EpipelagicLayer_01BLM-0_cost-normal/pu.dat", sep = ",", header = TRUE)
features <- read.table("output_prioritizr_blm-cal/02_EpipelagicLayer_01BLM-0_cost-normal/spec.dat", sep = ",", header = TRUE)
bound <- read.table("output_prioritizr_blm-cal/02_EpipelagicLayer_01BLM-0_cost-normal/bound.dat", sep = ",", header = TRUE)
rij <- read.table("output_prioritizr_blm-cal/02_EpipelagicLayer_01BLM-0_cost-normal/puvsp.dat", sep = ",", header = TRUE)



pu$cost <- ifelse(is.na(pu$cost), 0, pu$cost)
pu$cost <- round(pu$cost)
pu$status <- ifelse(pu$status == 3, 0, pu$status)
pu$cost <- ifelse(pu$cost == 0, median(filter(pu, pu$cost != 0)$cost), pu$cost)
range(pu$cost)
sum(pu$cost)

mp1 <- marxan_problem(x = pu, spec = features, puvspr = rij, bound = bound, blm = 0.002294) # does not like negative cost values
presolve_check(mp1)
mp1 <- marxan_problem(x = pu, spec = features, puvspr = rij, bound = bound, blm = 0.002294) %>%
  add_gap_portfolio(number_solutions = 100, pool_gap = 0.2)

mp3_solution <- prioritizr::solve(mp1) # needs gurobi R package
mp3_solution <- mp1 %>% add_gurobi_solver(gap = 0.2, presolve = 2, time_limit = 10) %>% solve()
# class(mp3_solution)
write.csv(mp3_solution, "output_prioritizr_blm-cal/mp3_solution_BLM-0_cost-normal.csv")
# calculate representation

pu$cost <- 0
mp2 <- marxan_problem(x = pu, spec = features, puvspr = rij, bound = bound, blm = 1) # does not like negative cost values
mp2_solution <- prioritizr::solve(mp2) # needs gurobi R package
write.csv(mp2_solution, "output_prioritizr_blm-cal/mp3_solution_BLM-0_cost-0.csv")

  mp3_solution <- read.csv("output_prioritizr_blm-cal/02_EpipelagicLayer_0.001173349_10.csv")
  sol_1 <- mp3_solution %>% filter(solution_1 == "1")
  shp <- st_read("shapefiles_rasters/abnj_02-epipelagic_global_moll_05deg/abnj_02-epipelagic_global_moll_05deg.shp")
  # rs <- raster("annualvocc_b_inter/mag/ssp245/01_SurfaceLayer_ssp245/voccMag-thetao_01-Surface_MPI-ESM1-2-HR_ssp245-05deg_2020-2100.tif") %>% disaggregate(2)
  best_sol <- shp[shp$layer %in% sol_1$id, ] # extrat the best solution polygons from the planning units sf file
  # rs <- crop(rs, best_sol)
  
  # b <- "shapefiles/WorldBorders/TM_WORLD_BORDERS-0.3.shp"
  # wb <- st_read(b)
  # wb_sp <- as(wb, "Spatial")
  
  
  pdf("ypdfs/02-eppipelagic_best-sol_01d_BLM-0.001173349-costMedian.pdf", width = 40, height = 20)
  # plot(st_geometry(shp))
  plot(st_geometry(best_sol))
  # plot(wb_sp, add = TRUE)
  dev.off()

  best_sol_sp <- as(best_sol, "Spatial")
  test <- rasterize(best_sol_sp, rs, mask = TRUE)
  
  pdf("output_datfiles/best_sol_01c.pdf", width = 38, height = 20)
  # plot(test)
  plot(best_sol_sp)
  dev.off()
  
  weights_mag <- raster::area(test$voccMag.thetao_01.Surface_MPI.ESM1.2.HR_ssp126.05deg_2050.2100)
  Mag_median = weighted.median(test, w = weights_mag, na.rm = TRUE)
  # median(test[], na.rm = T)
  Mag_lower_qt = wtd.quantile(test[], q = 0.25, weight = weights_mag[], na.rm = TRUE)
  Mag_upper_qt = wtd.quantile(test[], q = 0.75, weight = weights_mag[], na.rm = TRUE)

  
  x <- st_read("output_datfiles/02-cost-vocc_feat-sps_ssp126/02-cost-vocc_feat-sps_ssp126.shp")
  # area <- st_area(x)
  best_sol2 <- x[x$id %in% sol_1$id, ]
  weights_area <- st_area(best_sol2)/1e+06
  
  Mag_median <- weighted.median(best_sol2$cost, weights_area)
  Mag_lower_qt <-  wtd.quantile(best_sol2$cost, q = 0.25, weight = as.numeric(weights_area))
  Mag_upper_qt <-  wtd.quantile(best_sol2$cost, q = 0.75, weight = as.numeric(weights_area))

  
p1 <- mp1 %>% add_min_set_objective() # same as default
p2 <- mp1 %>% add_max_cover_objective(500)
p3 <- mp1 %>% add_max_features_objective(1900)
p4 <- mp1 %>% add_min_shortfall_objective(1900)
p7 <- mp1 %>% add_max_utility_objective(1900)

s <- list(solve(p1), solve(p2), solve(p3), solve(p4), solve(p7))
ps <- c("min_set_objective", "max_cover_objective", "max_features_objective", "min_shortfall_objective", "max_utility_objective")
shp <- st_read("shapefiles/01_surface_mediterranean/pu_shapefile_surface/pu_shapefile_surface.shp")

b <- "shapefiles/WorldBorders/TM_WORLD_BORDERS-0.3.shp"
wb <- st_read(b)
wb_sp <- as(wb, "Spatial")

for(i in 1:length(s)) {
  single <- s[[i]]
  sol_1 <- single %>% filter(solution_1 == "1")
  best_sol <- shp[shp$layer %in% sol_1$id, ]
  
  name.pdf <- paste(ps[i], ".pdf", sep = "_")
  pdf(file = paste("pdfs/", name.pdf, sep = ""), width = 38, height = 20)
  plot(st_geometry(shp), main = paste(ps[i]), cex.main = 3)
  plot(st_geometry(best_sol), add = TRUE, col = "green", main = paste(ps[i]), cex.main = 3)
  plot(wb_sp, add = TRUE, main = paste(ps[i]), cex.main = 3)
  dev.off()
}





