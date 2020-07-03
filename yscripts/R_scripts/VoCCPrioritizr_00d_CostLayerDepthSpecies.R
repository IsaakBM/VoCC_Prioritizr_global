library(raster)
library(rfishbase)
library(dplyr)
library(readr)
library(stringr)


species <- read_rds("Cost_Layers/Cost_RasterStack_bySpecies.rds")
ns_species <- names(species)
ns_species2 <- stringr::str_replace_all(string = ns_species, pattern = "[.]", replacement = " ")

depth2 <- list()
for (s in 1:length(ns_species2)){
  depth2[[s]] <- rfishbase::species(ns_species2[s], fields = c("DepthRangeShallow", "DepthRangeDeep"))}

names(depth2) <- ns_species2 # better to 
depth3 <- do.call(rbind, depth2)
depth3 <- depth3 %>% 
  mutate(Species = ns_species2) %>% 
  select(Species, DepthRangeShallow, DepthRangeDeep)

write.csv(depth3, "Cost_Layers/Cost_bySpecies_all.csv")
depth4 <- na.omit(depth3)
write.csv(depth4, "Cost_Layers/Cost_bySpecies_NOTall.csv")

Species_new <- list()
for(j in 1:nrow(depth3)) {
  single <- as.character(depth3[j, 1])
  # Species_new[[j]] <- ifelse(validate_names(single$Species) != single$Species, 
  #                              validate_names(single$Species), single$Species)
  Species_new[[j]] <- ifelse(length(validate_names(single)) == 0, single, 
                             ifelse(validate_names(single) != single, validate_names(single), single))
}

species_new <- as.vector(do.call(rbind, Species_new))
depth4 <- list()
for (s in 1:length(species_new)){
  depth4[[s]] <- rfishbase::species(species_new[s], fields = c("DepthRangeShallow", "DepthRangeDeep"))}

depth4 <- do.call(rbind, depth4)
depth4 <- depth4 %>% 
  mutate(Species = species_new) %>% 
  select(Species, DepthRangeShallow, DepthRangeDeep)
write.csv(depth4, "Cost_Layers/Cost_bySpecies_all_validate-names.csv")



ep <- depth4 %>% 
  filter(DepthRangeShallow >= 0 & DepthRangeShallow < 200 | DepthRangeDeep >= 0 & DepthRangeDeep < 200)
nrow(ep) #669 species
mp <- depth4 %>% 
  filter(DepthRangeShallow >= 200 & DepthRangeShallow < 1000 | DepthRangeDeep >= 200 & DepthRangeDeep < 1000)
nrow(mp) #322 species
bap <- depth4 %>% 
  filter(DepthRangeShallow >= 1000 | DepthRangeDeep >= 1000)
nrow(bap)#108

names(species) <- str_replace_all(string = depth4$Species, pattern = " ", replacement = ".")

species_ep <- subset(species, str_replace_all(string = ep$Species, pattern = " ", replacement = "."))
species_ep_rs <- stackApply(species_ep, nlayers(species_ep), fun = sum)
writeRaster(species_ep_rs, "Cost_Layers/02-epipelagic_Cost_Raster_Sum.tif")

species_mp <- subset(species, str_replace_all(string = mp$Species, pattern = " ", replacement = "."))
species_mp_rs <- stackApply(species_mp, nlayers(species_mp), fun = sum)
writeRaster(species_mp_rs, "Cost_Layers/03-mesopelagic_Cost_Raster_Sum.tif")

species_bap <- subset(species, str_replace_all(string = bap$Species, pattern = " ", replacement = "."))
species_bap_rs <- stackApply(species_bap, nlayers(species_bap), fun = sum)
writeRaster(species_bap_rs, "Cost_Layers/04-bathyabyssopelagic_Cost_Raster_Sum.tif")

plot(log10(species_bap_rs))
range(species_ep_rs[])
