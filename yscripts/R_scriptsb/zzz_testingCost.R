
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
  if(unique(x$DepthMin) >= 200 | unique(x$DepthMin) < 1000 | unique(x$DepthMax) >= 200 | unique(x$DepthMax) < 1000) {
    ff <- x}
})
test01 <- mp[lengths(mp) != 0] # 102 ...  ==>> #232
saveRDS(mp, "Inputs/Cost/03-mesopelagic_CostbySpecies.rds")

bap <- lapply(t2, function(x){
  if(unique(x$DepthMin) >= 1000 | unique(x$DepthMax) >= 1000) {
    ff <- x}
})
bap <- bap[lengths(bap) != 0] # 55
saveRDS(bap, "Inputs/Cost/04-bathyabyssopelagic_CostbySpecies.rds")



bap2 <- lapply(bap, function(x){
  ff <- x %>% 
    dplyr::mutate(cost = .[[3]], 
                  MinLayer = 1000,
                  MaxLayer = 4000, 
                  DepthRangeLayer = DepthMax - MinLayer, 
                  cost_depth = cost/DepthRange, 
                  cost_layer = cost_depth*DepthRangeLayer) %>% 
  dplyr::select(x, y, cost_layer)})
bapDF <- data.table::rbindlist(bap2, use.names = TRUE) %>% 
  dplyr::group_by(x, y) %>% 
  dplyr::summarise(total_cost = sum(cost_layer))

test <- raster::rasterFromXYZ(bapDF)


ep2 <- lapply(ep, function(x){
  ff <- x %>% 
    dplyr::mutate(cost = .[[3]], 
                  MinLayer = 0,
                  MaxLayer = 200, 
                  DepthRangeLayer = MaxLayer - DepthMin, 
                  cost_depth = cost/DepthRange, 
                  cost_layer = cost_depth*DepthRangeLayer) %>% 
    dplyr::select(x, y, cost_layer)})
epDF <- data.table::rbindlist(ep2, use.names = TRUE) %>% 
  dplyr::group_by(x, y) %>% 
  dplyr::summarise(total_cost = sum(cost_layer))


mp2 <- lapply(ep, function(x){
  ff <- x %>% 
    dplyr::mutate(cost = .[[3]], 
                  MinLayer = 200,
                  MaxLayer = 1000, 
                  DepthRangeLayer = (MaxLayer- MinLayer) - ifelse(DepthMin >= 200, DepthMin, 0), 
                  cost_depth = cost/DepthRange, 
                  cost_layer = cost_depth*DepthRangeLayer) %>% 
    dplyr::select(x, y, cost_layer)})
mpDF <- data.table::rbindlist(mp2, use.names = TRUE) %>% 
  dplyr::group_by(x, y) %>% 
  dplyr::summarise(total_cost = sum(cost_layer))




test1 <- raster::rasterFromXYZ(bapDF)
plot(kader:::cuberoot(test1))
saveRDS(test1, "Inputs/Cost/04-bathyabyssopelagic_CostRasterTotal.rds")
test3 <- raster::rasterFromXYZ(mpDF)
saveRDS(test3, "Inputs/Cost/03-mesopelagic_CostRasterTotal.rds")
plot(kader:::cuberoot(test3))
test2 <- raster::rasterFromXYZ(epDF)
plot(kader:::cuberoot(test2))
saveRDS(test2, "Inputs/Cost/02-epipelagic_CostRasterTotal.rds")


