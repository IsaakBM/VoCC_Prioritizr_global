# This code was written by Isaac Brito-Morales (i.britomorales@uq.edu.au)
# Please do not distribute this code without permission.
# NO GUARANTEES THAT CODE IS CORRECT
# Caveat Emptor!


pu_by_provinces <- function(pu_file, province_file, prov_name, proj.geo, outdir) {
  
  library(raster)
  library(dplyr)
  library(sf)
  library(data.table)
  library(foreach)
  library(doParallel)

  
  # Reading Planning Unit Region Shapefile
    pu_file <- pu_file
      pu_region <- st_read(pu_file) %>% st_transform(crs = CRS(proj.geo))
  # Reading Marine Province Shapefile
    province <- province_file
      bioprovince <- st_read(province) %>% st_transform(crs = CRS(proj.geo)) 
  # Match 
  if(prov_name == "Longhurst") {
    # Get the indicator for the provinces
      prov_code <- as.character(bioprovince$ProvCode)
      prov_list <- list() # to allocate results
      for(i in 1:length(prov_code)) { # this could be set on parallel
        single <- bioprovince %>% filter(ProvCode == prov_code[i])
        dt1 <- st_crop(pu_region, single) # crop each polygon with the bioprovince
        prov_list[[i]] <- dt1 %>% mutate(province = prov_code[i]) # save the output
      }
    # Merge all the output
      pus_prov <- do.call(rbind, prov_list) %>% arrange(layer)
    # Match and establish categories
      pu_region$province <- pus_prov$province[match(pu_region$layer, pus_prov$layer)]
      pu_region$province <- ifelse(is.na(pu_region$province), 
                                   paste("non-categ", prov_name, sep = "_"), 
                                   paste(pu_region$province, prov_name, sep = "_"))
    
  } else if (prov_name == "Glasgow") {
    # Get the indicator for the provinces
      prov_code <- as.character(bioprovince$ProvId)
      prov_list <- list() # to allocate results
      for(i in 1:length(prov_code)) { # this could be set on parallel
        single <- bioprovince %>% filter(ProvId == prov_code[i])
        dt1 <- st_crop(pu_region, single) # crop each polygon with the bioprovince
        prov_list[[i]] <- dt1 %>% mutate(province = prov_code[i]) # save the output
      }
    # Merge all the output
      pus_prov <- do.call(rbind, prov_list) %>% arrange(layer)
    # Match and establish categories
      pu_region$province <- pus_prov$province[match(pu_region$layer, pus_prov$layer)]
      pu_region$province <- ifelse(is.na(pu_region$province), 
                                   paste("non-categ", prov_name, sep = "_"), 
                                   paste(pu_region$province, prov_name, sep = "_"))
      
  } else if (prov_name == "GOODS") { # this is for the seafloor
    # Set up parallel structure
      ncores <- 3 
      cl <- makeCluster(ncores)
      registerDoParallel(cl)
    # Get the indicator for the provinces
      prov_code <- as.character(bioprovince$Province)
      prov_list <- list() # to allocate results
      prov_par <- foreach(i = 1:length(prov_code), .packages = c("raster", "sf", "data.table", "dplyr")) %dopar% {
        single <- bioprovince %>% filter(Province == prov_code[i])
        dt1 <- st_crop(pu_region, single) # crop each polygon with the bioprovince
        prov_list[[i]] <- dt1 %>% mutate(province = prov_code[i]) # save the output
      }
      stopCluster(cl)
    # Merge all the output
      pus_prov <- do.call(rbind, prov_par) %>% arrange(layer)
    # Match and establish categories
      pu_region$province <- pus_prov$province[match(pu_region$layer, pus_prov$layer)]
      pu_region$province <- ifelse(is.na(pu_region$province), 
                                   paste("non-categ", prov_name, sep = "_"), 
                                   paste(pu_region$province, prov_name, sep = "_"))
    
  } else if (prov_name == "mpas") {
    # Set up parallel structure
      ncores <- 3 
      cl <- makeCluster(ncores)
      registerDoParallel(cl)
    # Get the indicator for the provinces
      prov_code <- as.character(bioprovince$wdpaid)
      prov_list <- list() # to allocate results
      prov_par <- foreach(i = 1:length(prov_code), .packages = c("raster", "sf", "data.table", "dplyr")) %dopar% {
        single <- bioprovince %>% filter(wdpaid == prov_code[i])
        dt1 <- st_crop(pu_region, single) # crop each polygon with the bioprovince
        if(nrow(dt1) > 0) { 
          prov_list[[i]] <- dt1 %>% mutate(province = prov_code[i]) # save the output    
        }
      }
      stopCluster(cl)
    # Merge all the output
      pus_prov <- do.call(rbind, prov_par) %>% arrange(layer)
    # Match and establish categories
      pu_region$province <- pus_prov$province[match(pu_region$layer, pus_prov$layer)]
      pu_region$province <- ifelse(is.na(pu_region$province), 
                                   paste("non-categ", prov_name, sep = "_"), 
                                   paste(pu_region$province, prov_name, sep = "_"))
  }
  return(pu_region)
}


test_longhurst <- pu_by_provinces(pu_file = "shapefiles_rasters/abnj_02-epipelagic_global_moll_05deg/abnj_02-epipelagic_global_moll_05deg.shp", 
                                province_file = "shapefiles_rasters/LonghurstProvinces/Longhurst_world_v4_2010.shp", 
                                prov_name = "Longhurst", 
                                proj.geo = "+proj=moll +lon_0=0 +datum=WGS84 +units=m +no_defs", 
                                outdir = "")

test_glasgow <- pu_by_provinces(pu_file = "shapefiles_rasters/abnj_04-bathyabysso_global_moll_05deg/abnj_04-bathyabysso_global_moll_05deg.shp", 
                        province_file = "shapefiles_rasters/GlasgowProvinces/GlasgowMesopelagicProvinces_v1_2017.shp", 
                        prov_name = "Glasgow", 
                        proj.geo = "+proj=moll +lon_0=0 +datum=WGS84 +units=m +no_defs", 
                        outdir = "")

test_good <- pu_by_provinces(pu_file = "shapefiles_rasters/abnj_04-bathyabysso_global_moll_05deg/abnj_04-bathyabysso_global_moll_05deg.shp", 
                                province_file = "shapefiles_rasters/GOODSprovinces/GOODSprovinces_abyssal.shp", 
                                prov_name = "GOODS", 
                                proj.geo = "+proj=moll +lon_0=0 +datum=WGS84 +units=m +no_defs", 
                                outdir = "")

test_good <- pu_by_provinces(pu_file = "shapefiles_rasters/abnj_02-epipelagic_global_moll_05deg/abnj_02-epipelagic_global_moll_05deg.shp", 
                             province_file = "shapefiles_rasters/mpas_v2018/mpas_v2018.shp", 
                             prov_name = "mpas", 
                             proj.geo = "+proj=moll +lon_0=0 +datum=WGS84 +units=m +no_defs", 
                             outdir = "")



unique(test_longhurst$province)
length(unique(test_longhurst$province))


long <- st_read("shapefiles_rasters/LonghurstProvinces/Longhurst_world_v4_2010.shp") %>% 
  st_transform(crs = CRS("+proj=moll +lon_0=0 +datum=WGS84 +units=m +no_defs"))

glasgow_prov <- st_read("shapefiles_rasters/GlasgowProvinces/GlasgowMesopelagicProvinces_v1_2017.shp") %>% 
  st_transform(crs = CRS("+proj=moll +lon_0=0 +datum=WGS84 +units=m +no_defs"))

goods_prov <- st_read("shapefiles_rasters/GOODSprovinces/GOODSprovinces_abyssal.shp") %>% 
  st_transform(crs = CRS("+proj=moll +lon_0=0 +datum=WGS84 +units=m +no_defs"))



# reading csv to match species

csv_olayer <- "shapefiles_rasters/bathyabyssopelagic.csv"
file_olayer <- fread(csv_olayer)
head(file_olayer)
length(unique(file_olayer$feature_names))
# match province name with species name
file_olayer$province <- pu_region$province[match(file_olayer$pu, pu_region$layer)]
head(file_olayer)
unique(file_olayer$province)

file_olayer <- file_olayer %>% 
  mutate(feature_names_prov = ifelse(is.na(province), paste("non-categ", prov_name, sep = "_"), 
                                     paste(feature_names, province, sep = "_")))

head(file_olayer)
nrow(file_olayer)
length(unique(file_olayer$feature_names_prov)) # increase the number of unique species features, which is OK and predictable








fwrite(file_olayer, "shapefiles_rasters/bathyabyssopelagic_provinces.csv")

bap_pu <- st_read("shapefiles_rasters/abnj_04-bathyabysso_global_moll_05deg/abnj_04-bathyabysso_global_moll_05deg.shp")
bap <- fread("shapefiles_rasters/bathyabyssopelagic.csv")
bap2 <- bap %>% 
  group_by(pu) %>% 
  summarise(richness = n()) %>% 
  data.frame()
head(bap2)
range(bap2$richness, na.rm = TRUE)

bap_pu$richness <- bap2$richness[match(bap_pu$layer, bap2$pu)]

nrow(bap_pu)
range(bap_pu$richness, na.rm = TRUE)
length(unique(bap$pu))

bap_pu <- bap_pu %>% 
  mutate(richness_log = log10(richness))

pal_rich <- rev(brewer.pal(9, "RdYlBu"))
cv_rich <- c("1", "", "", "10", "", "", "100", "", "1000")
world_sf <- ne_countries(scale = "medium", returnclass = "sf")
# Defining themes
theme_opts3 <- list(theme(panel.grid.minor = element_blank(),
                          panel.grid.major = element_blank(),
                          panel.background = element_rect(fill = "white", colour = "black"),
                          plot.background = element_rect(fill = "white"),
                          panel.border = element_blank(),
                          axis.line = element_line(size = 1),
                          axis.text.x = element_text(size = rel(2), angle = 0),
                          axis.text.y = element_text(size = rel(2), angle = 0),
                          axis.ticks = element_line(size = 1.5),
                          axis.ticks.length = unit(.25, "cm"), 
                          axis.title.x = element_blank(),
                          axis.title.y = element_blank(),
                          plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
                          legend.title = element_text(colour = "black", face = "bold", size = 15),
                          legend.text = element_text(colour = "black", face = "bold", size = 10), 
                          legend.key.height = unit(1, "cm"),
                          legend.key.width = unit(0.8, "cm"),
                          plot.tag = element_text(size = 25, face = "bold")))

ggplot() +
  geom_sf(data = bap_pu, aes(fill = richness_log), color = NA) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  ggtitle("Species richness") +
  scale_fill_gradientn(name = "richness",
                       colours = pal_rich,
                       limits = c(0, 3),
                       breaks = seq(0, 3, length.out = 9), 
                       labels = cv_rich) +
  theme_opts3 +
  ggsave("ypdfs/abnj_04-bathyabysso_richness_moll_05deg.pdf", width = 40, height = 20, dpi = 300)
