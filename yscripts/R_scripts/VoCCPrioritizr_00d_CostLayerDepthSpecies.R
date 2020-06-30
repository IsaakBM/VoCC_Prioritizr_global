library(raster)
library(rfishbase)
library(dplyr)
library(readr)
library(stringr)

cost <- readRDS("Cost_Layers/Cost_Raster_Sum.rds")
species <- read_rds("Cost_Layers/Cost_RasterStack_bySpecies.rds")
ns_species <- names(species)
length(ns_species)
ns_species2 <- stringr::str_replace_all(string = ns_species, pattern = "[.]", replacement = " ")
length(ns_species2)


depth <- tibble(Species = ns_species2)
depth2 <- list()



for (s in 1:length(depth$Species)){
  depth2[[s]] <- rfishbase::species(ns_species2[s], fields = c("DepthRangeShallow", "DepthRangeDeep"))
  # depth$MinDepth[s] <- eco$depthRangeShallow
  # depth$MaxDepth[s] <- eco$depthRangeDeep
}

names(depth2) <- ns_species2
depth3 <- do.call(rbind, depth2)
depth3 <- depth3 %>% 
  mutate(Species = ns_species2) %>% 
  select(Species, DepthRangeShallow, DepthRangeDeep)

plot(species$Acanthistius.brasilianus)

cost_all <- stackApply(species, indices = nlayers(species), fun = sum)
plot(cost_all)

writeRaster(cost_all, "Cost_Layers/Cost_Raster_Sum.tif")
