library(raster)
library(rfishbase)
library(dplyr)
library(readr)
library(stringr)

species <- read_rds("Cost_Layers/Cost_RasterStack_bySpecies.rds")
ns_species <- names(species)
length(ns_species)
ns_species2 <- stringr::str_replace_all(string = ns_species, pattern = "[.]", replacement = " ")
length(ns_species2)


depth <- tibble(Species = ns_species2)
for (s in 1:length(depth$Species)){
  eco <- rfishbase::species(ns_species2[s], fields = c("DepthRangeShallow", "DepthRangeDeep"))
  depth$MinDepth[s] <- eco$depthRangeShallow
  depth$MaxDepth[s] <- eco$depthRangeDeep
}
