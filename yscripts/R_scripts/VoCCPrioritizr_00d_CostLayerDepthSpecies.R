library(raster)
library(rfishbase)
library(dplyr)
library(readr)
library(stringr)



species <- read_rds("Cost_Layers/Cost_RasterStack_bySpecies.rds")
ns_species <- names(species)
ns_species2 <- stringr::str_replace_all(string = ns_species, pattern = "[.]", replacement = " ")
head(ns_species2)
length(ns_species2)
ns_species3 <- validate_names(ns_species2)
length(ns_species3)

depth2 <- list()
for (s in 1:length(ns_species3)){
  depth2[[s]] <- rfishbase::species(ns_species3[s], fields = c("DepthRangeShallow", "DepthRangeDeep"))}

names(depth2) <- ns_species3 # better to 
depth3 <- do.call(rbind, depth2)
depth3 <- depth3 %>% 
  mutate(Species = ns_species) %>% 
  select(Species, DepthRangeShallow, DepthRangeDeep)

write.csv(depth3, "Cost_Layers/Cost_bySpecies_all.csv")
depth4 <- na.omit(depth3)
write.csv(depth4, "Cost_Layers/Cost_bySpecies_NOTall.csv")


ep <- depth4 %>% 
  filter(DepthRangeDeep >= 0 & DepthRangeDeep < 200)
nrow(ep) #219 species
mp <- depth4 %>% 
  filter(DepthRangeDeep >= 200 & DepthRangeDeep < 1000)
nrow(mp) #277 species
bap <- depth4 %>% 
  filter(DepthRangeDeep >= 1000)
nrow(bap)#115

species_ep <- subset(species, ep$Species)
species_ep_rs <- stackApply(species_ep, nlayers(species_ep), fun = sum)

plot(log10(species_ep_rs))
range(species_ep_rs[])

mpas <- st_read("shapefiles_rasters/mpas_v2018/mpas_v2018.shp")
plot(st_geometry(mpas))
