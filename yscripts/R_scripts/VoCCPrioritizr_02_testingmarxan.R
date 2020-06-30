library(raster)
library(sf)
library(data.table)
library(dplyr)
library(prioritizr)
library(gurobi)
library(spatstat)
library(reldist)

pu <- read.table("output_datfiles/abnj_04-bathyabysso_global_moll_05deg/pu.dat", sep = ",", header = TRUE)
features <- read.table("output_datfiles/abnj_04-bathyabysso_global_moll_05deg/spec.dat", sep = ",", header = TRUE)
bound <- read.table("output_datfiles/abnj_04-bathyabysso_global_moll_05deg/bound.dat", sep = ",", header = TRUE)
rij <- read.table("output_datfiles/abnj_04-bathyabysso_global_moll_05deg/puvsp.dat", sep = ",", header = TRUE)

mp1 <- marxan_problem(x = pu, spec = features, puvspr = rij, bound = bound, blm = 1) # does not like negative cost values
mp1 <- marxan_problem(x = pu, spec = features, puvspr = rij2, bound = bound2, blm = 0) %>%
  add_pool_portfolio(method = 2, number_solutions = 2)

mp3_solution <- prioritizr::solve(mp1) # needs gurobi R package
# class(mp3_solution)
# write.csv(mp3_solution, "CSVs/mp3_solution.csv")

  sol_1 <- mp3_solution %>% filter(solution_1 == "1")
  shp <- st_read("shapefiles_rasters/abnj_04-bathyabysso_global_moll_05deg/abnj_04-bathyabysso_global_moll_05deg.shp")
  # rs <- raster("annualvocc_b_inter/mag/ssp245/01_SurfaceLayer_ssp245/voccMag-thetao_01-Surface_MPI-ESM1-2-HR_ssp245-05deg_2020-2100.tif") %>% disaggregate(2)
  best_sol <- shp[shp$layer %in% sol_1$id, ] # extrat the best solution polygons from the planning units sf file
  # rs <- crop(rs, best_sol)
  
  # b <- "shapefiles/WorldBorders/TM_WORLD_BORDERS-0.3.shp"
  # wb <- st_read(b)
  # wb_sp <- as(wb, "Spatial")
  
  
  pdf("ypdfs/best_sol_01d_BLM.pdf", width = 40, height = 20)
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





