
plg <- readRDS("Inputs/Cost/Cost_SpeciesInfoFB_Pelagic.rds") %>% 
  mutate(OldName2 = stringr::str_replace_all(string = OldName, pattern = " ", replacement = "."))
species <- read_rds("Inputs/Cost/Cost_RasterStack_bySpecies.rds")

dfLS <- vector("list", length = length(names(species)))
for(i in seq_along(dfLS)) {
  d1 <- subset(species, i) %>% 
    raster::rasterToPoints() %>% 
    as_tibble()
  d2 <- plg %>% 
    dplyr::filter(OldName2 == names(d1)[3])
  if(length(nrow(d2)) != 0) {
    dfLS[[i]] <- d1 %>% 
      dplyr::mutate(DepthMin = ifelse(length(d2$DepthMin) == 0, NA, d2$DepthMin), 
                    DepthMax = ifelse(length(d2$DepthMax) == 0, NA, d2$DepthMax), 
                    DepthRange = DepthMax - DepthMin)
  }
}

t2 <- lapply(dfLS, function(x){
  d1 <- x
  if(!is.na(d1[,4])){
  ff <- d1  
  }
})
t2 <- t2[lengths(t2) != 0]


ep <- lapply(t2, function(x){
  if(unique(x$DepthMin) <= 200 | unique(x$DepthMax) <= 200){
    ff <- x}
})
ep <- ep[lengths(ep) != 0] #221
saveRDS(ep, "Inputs/Cost/02-epipelagic_CostbySpecies.rds")

mp <- lapply(t2, function(x){
  if(unique(x$DepthMin) >= 200 & unique(x$DepthMin) < 1000 | unique(x$DepthMax) >= 200 & unique(x$DepthMax) < 1000) {
    ff <- x}
})
mp <- mp[lengths(mp) != 0] # 102
saveRDS(mp, "Inputs/Cost/03-mesopelagic_CostbySpecies.rds")

bap <- lapply(t2, function(x){
  if(unique(x$DepthMin) >= 1000 | unique(x$DepthMax) >= 1000) {
    ff <- x}
})
bap <- bap[lengths(bap) != 0] # 55
saveRDS(bap, "Inputs/Cost/04-bathyabyssopelagic_CostbySpecies.rds")



(200/(300-150))

50*(200/(300-150))
150*(200/(300-150))
