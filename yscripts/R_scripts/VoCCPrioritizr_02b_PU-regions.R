# This code was written by Isaac Brito-Morales (i.britomorales@uq.edu.au)
# Please do not distribute this code without permission.
# NO GUARANTEES THAT CODE IS CORRECT
# Caveat Emptor!

# MaxTarget% - PUs(i)/MaxPUs(all spp) * (MaxTarget%-MinTarget%)
0.50 - (5/100) * (0.50 - 0.10)

csvs_pus_provinces <- function(csv_olayer_prov, csv_olayer_species, olayer) {
  library(data.table)
  library(dplyr)
  
  
  
}

# reading csv to match species
library(data.table)
library(dplyr)

csv_olayer_prov <- "CSVs/02_EpipelagicLayer/pus-epipelagic_Longhurst_.csv"
csv_olayer_species <- "CSVs/02_EpipelagicLayer/epipelagic.csv"

file_olayer_pus <- fread(csv_olayer_prov) %>% 
  arrange(layer)
file_olayer_species <- fread(csv_olayer_species) %>% 
  arrange(pu)
# match province name with species name
file_olayer_species$province <- file_olayer_pus$province[match(file_olayer_species$pu, file_olayer_pus$layer)]
file_olayer_species <- file_olayer_species %>% 
  mutate(feature_names_prov = ifelse(is.na(province), paste("non-categ", prov_name, sep = "_"),
                                     paste(feature_names, province, sep = "_")))

# head(file_olayer_species)
# nrow(file_olayer_species)
# length(unique(file_olayer_species$feature_names)) # increase the number of unique species features, which is OK and predictable
fwrite(file_olayer_species, "shapefiles_rasters/mesopelagic_provinces.csv")


# defining targets
provinces_bypu <- unique(file_olayer_pus$province)
# species <- unique(file_olayer_species$feature_names_prov)
min_target <- 0.2
max_target <- 1

dt_final <- list()
for(i in 1:length(provinces_bypu)) {
  
  dt1 <- file_olayer_pus %>% 
    filter(province == provinces_bypu[i])
  max_pus <- length(unique(dt1$layer))
  
  dt2 <- file_olayer_species %>% 
    filter(province == provinces_bypu[i]) %>%  
    group_by(feature_names_prov) %>% 
    summarise(cells = n()) %>% 
    mutate(targets = (max_target - ((cells/max_pus) * (max_target - min_target))))
  
  dt_final[[i]] <- dt2
  
}

dt_testing <- do.call(rbind, dt_final)


dt1 <- file_olayer_pus %>% 
  filter(province == provinces_bypu[2])
max_pus <- length(unique(dt1$layer))

dt2 <- file_olayer_species %>% 
  filter(province == provinces_bypu[2]) %>%  
  group_by(feature_names_prov) %>% 
  summarise(cells = n()) %>% 
  mutate(targets = (max_target - ((cells/max_pus) * (max_target - min_target))))
  
dt3 <- dt2 %>% 
  group_by(feature_names_prov) %>% 
  summarise(cells = n()) %>% 
  mutate(targets = (max_target - ((cells/max_pus) * (max_target - min_target))))
  
head(dt3, 50)
nrow(dt3)
range(dt3$cells)
range(dt3$target)

filter(dt3, target >= 0.5)


species_dt2 <- unique(dt2$feature_names_prov)
length(species_dt2)


(max_target - ((cells/max_pus) * (max_target - min_target)))

# MaxTarget% - PUs(i)/MaxPUs(all spp) * (MaxTarget%-MinTarget%)
1 - (838/838) * (1 - 0.2)



# bap_pu <- st_read("shapefiles_rasters/abnj_04-bathyabysso_global_moll_05deg/abnj_04-bathyabysso_global_moll_05deg.shp")
# bap <- fread("shapefiles_rasters/bathyabyssopelagic.csv")
# bap2 <- bap %>% 
#   group_by(pu) %>% 
#   summarise(richness = n()) %>% 
#   data.frame()
# head(bap2)
# range(bap2$richness, na.rm = TRUE)
# 
# bap_pu$richness <- bap2$richness[match(bap_pu$layer, bap2$pu)]
# 
# nrow(bap_pu)
# range(bap_pu$richness, na.rm = TRUE)
# length(unique(bap$pu))
# 
# bap_pu <- bap_pu %>% 
#   mutate(richness_log = log10(richness))
# 
# pal_rich <- rev(brewer.pal(9, "RdYlBu"))
# cv_rich <- c("1", "", "", "10", "", "", "100", "", "1000")
# world_sf <- ne_countries(scale = "medium", returnclass = "sf")
# # Defining themes
# theme_opts3 <- list(theme(panel.grid.minor = element_blank(),
#                           panel.grid.major = element_blank(),
#                           panel.background = element_rect(fill = "white", colour = "black"),
#                           plot.background = element_rect(fill = "white"),
#                           panel.border = element_blank(),
#                           axis.line = element_line(size = 1),
#                           axis.text.x = element_text(size = rel(2), angle = 0),
#                           axis.text.y = element_text(size = rel(2), angle = 0),
#                           axis.ticks = element_line(size = 1.5),
#                           axis.ticks.length = unit(.25, "cm"), 
#                           axis.title.x = element_blank(),
#                           axis.title.y = element_blank(),
#                           plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
#                           legend.title = element_text(colour = "black", face = "bold", size = 15),
#                           legend.text = element_text(colour = "black", face = "bold", size = 10), 
#                           legend.key.height = unit(1, "cm"),
#                           legend.key.width = unit(0.8, "cm"),
#                           plot.tag = element_text(size = 25, face = "bold")))
# 
# ggplot() +
#   geom_sf(data = bap_pu, aes(fill = richness_log), color = NA) +
#   geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
#   ggtitle("Species richness") +
#   scale_fill_gradientn(name = "richness",
#                        colours = pal_rich,
#                        limits = c(0, 3),
#                        breaks = seq(0, 3, length.out = 9), 
#                        labels = cv_rich) +
#   theme_opts3 +
#   ggsave("ypdfs/abnj_04-bathyabysso_richness_moll_05deg.pdf", width = 40, height = 20, dpi = 300)
