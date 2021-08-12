
plg <- readRDS("Inputs/Cost/Cost_SpeciesInfoFB_Pelagic.rds") %>% 
  mutate(OldName2 = stringr::str_replace_all(string = OldName, pattern = " ", replacement = "."))
bth <- readRDS("Inputs/Cost/Cost_SpeciesInfoFB_Benthic.rds") %>% 
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
ep <- ep[lengths(ep) != 0] # 221
saveRDS(ep, "Inputs/Cost/02-epipelagic_CostbySpecies.rds")

mp <- lapply(t2, function(x){
  if(unique(x$DepthMin) >= 200 | unique(x$DepthMin) < 1000 | unique(x$DepthMax) >= 200 | unique(x$DepthMax) < 1000) {
    ff <- x}
})
mp <- mp[lengths(mp) != 0] # 232
saveRDS(mp, "Inputs/Cost/03-mesopelagic_CostbySpecies.rds")

bap <- lapply(t2, function(x){
  if(unique(x$DepthMin) >= 1000 | unique(x$DepthMax) >= 1000) {
    ff <- x}
})
bap <- bap[lengths(bap) != 0] # 55
saveRDS(bap, "Inputs/Cost/04-bathyabyssopelagic_CostbySpecies.rds")

####################################################################################
####### 
####################################################################################
# 
  ep2 <- lapply(ep, function(x){
    ff <- x %>% 
      dplyr::mutate(cost = .[[3]], 
                    MinLayer = 0,
                    MaxLayer = 200, 
                    DepthRangeLayer = MaxLayer - DepthMin, 
                    cost_depth = cost/DepthRange, 
                    cost_layer = cost_depth*DepthRangeLayer)})
  saveRDS(ep2, "Inputs/Cost/02-epipelagic_CostbySpecies.rds")
# 
  epDF <- lapply(ep2, function(x) x %>% dplyr::select(x, y, cost_layer))
  epDF <- epDF %>% 
    data.table::rbindlist(use.names = TRUE) %>% 
    dplyr::group_by(x, y) %>% 
    dplyr::summarise(total_cost = sum(cost_layer))
  epRS <- raster::rasterFromXYZ(epDF)
  saveRDS(epRS, "Inputs/Cost/02-epipelagic_CostRasterTotal.rds")
  writeRaster(epRS, "Inputs/Cost/02-epipelagic_CostRasterTotal.tif", overwrite = TRUE)
  
##### 
  mp2 <- lapply(mp, function(x){
    ff <- x %>% 
      dplyr::mutate(cost = .[[3]], 
                    MinLayer = 200,
                    MaxLayer = 1000, 
                    DepthRangeLayer = (MaxLayer- MinLayer) - ifelse(DepthMin >= 200, DepthMin, 0), 
                    cost_depth = cost/DepthRange, 
                    cost_layer = cost_depth*DepthRangeLayer)})
  saveRDS(mp2, "Inputs/Cost/03-mesopelagic_CostbySpecies.rds")
# 
  mpDF <- lapply(mp2, function(x) x %>% dplyr::select(x, y, cost_layer))
  mpDF <- mpDF %>% 
    data.table::rbindlist(use.names = TRUE) %>% 
    dplyr::group_by(x, y) %>% 
    dplyr::summarise(total_cost = sum(cost_layer))
  mpRS <- raster::rasterFromXYZ(mpDF)
  saveRDS(mpRS, "Inputs/Cost/03-mesopelagic_CostRasterTotal.rds")
  writeRaster(mpRS, "Inputs/Cost/03-mesopelagic_CostRasterTotal.tif", overwrite = TRUE)
    
#####
# 
  bap2 <- lapply(bap, function(x){
    ff <- x %>% 
      dplyr::mutate(cost = .[[3]], 
                    MinLayer = 1000,
                    MaxLayer = 4000, 
                    DepthRangeLayer = DepthMax - MinLayer, 
                    cost_depth = cost/DepthRange, 
                    cost_layer = cost_depth*DepthRangeLayer)})
  saveRDS(bap2, "Inputs/Cost/04-bathyabyssopelagic_CostbySpecies.rds")
#
  bapDF <- lapply(bap2, function(x) x %>% dplyr::select(x, y, cost_layer))
  bapDF <- bapDF %>% 
    data.table::rbindlist(use.names = TRUE) %>% 
    dplyr::group_by(x, y) %>% 
    dplyr::summarise(total_cost = sum(cost_layer))
  bapRS <- raster::rasterFromXYZ(bapDF)
  saveRDS(bapRS, "Inputs/Cost/04-bathyabyssopelagic_CostRasterTotal.rds")
  writeRaster(bapRS, "Inputs/Cost/04-bathyabyssopelagic_CostRasterTotal.tif", overwrite = TRUE)
  
####################################################################################
####### 
####################################################################################
# 
  bthLS <- vector("list", length = length(names(species)))
  for(i in seq_along(bthLS)) {
    d1 <- subset(species, i) %>% 
      raster::rasterToPoints() %>% 
      as_tibble()
    d2 <- bth %>% 
      dplyr::filter(OldName2 == names(d1)[3])
    if(length(nrow(d2)) != 0) {
      bthLS[[i]] <- d1 %>% 
        dplyr::mutate(DepthMin = ifelse(length(d2$DepthMin) == 0, NA, d2$DepthMin), 
                      DepthMax = ifelse(length(d2$DepthMax) == 0, NA, d2$DepthMax), 
                      DepthRange = DepthMax - DepthMin)
    }
  }

  t3 <- lapply(bthLS, function(x){
    d1 <- x
    if(!is.na(d1[,4])){
      ff <- d1  
    }
  })
  t3 <- t3[lengths(t3) != 0] # 844 Benthic species (associated with seafloor)
#
  bth2 <- lapply(t3, function(x){
    ff <- x %>% 
      dplyr::mutate(cost_layer = .[[3]])})
  saveRDS(bth2, "Inputs/Cost/05-seafloor_CostbySpecies.rds")
  bthDF <- lapply(bth2, function(x) x %>% dplyr::select(x, y, cost_layer))
  bthDF <- bthDF %>% 
    data.table::rbindlist(use.names = TRUE) %>% 
    dplyr::group_by(x, y) %>% 
    dplyr::summarise(total_cost = sum(cost_layer))
  bthRS <- raster::rasterFromXYZ(bthDF)
  saveRDS(bthRS, "Inputs/Cost/05-seafloor_CostRasterTotal.rds")
  writeRaster(bthRS, "Inputs/Cost/05-seafloor_CostRasterTotal.tif")
  
  
  