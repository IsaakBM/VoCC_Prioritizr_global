library(data.table)
library(ggplot2)
library(dplyr)
library(stringr)

################
aqm <- fread("/Users/bri273/Desktop/AquaMaps_wflow/AquaMaps/v2019a/speciesoccursum.csv", fill = TRUE) %>% 
  dplyr::filter(rank == 1)

# miss: bryozoans
aqm_groups <- aqm %>% 
  mutate(groups_01 = ifelse(class == "Bivalvia", "Bivalvia",
                            ifelse(class == "Cephalopoda", "Cephalopoda", 
                                   ifelse(class == "Polyplacophora", "Polyplacophora", 
                                          ifelse(order == "Cetacea", "Cetacea", 
                                                 ifelse(phylum == "Echinodermata", "Echinoderms", 
                                                        ifelse(order == "Euphausiacea", "Euphausiids", 
                                                               ifelse(phylum == "Foraminifera", "Foraminifera", 
                                                                      ifelse(class == "Gastropoda", "Gastropods", 
                                                                             ifelse(class == "Hydrozoa", "Hydrozoans", 
                                                                                    ifelse(class == "Cubozoa", "Jellyfish", 
                                                                                           ifelse(class == "Scyphozoa", "Jellyfish", 
                                                                                                  ifelse(class == "Anthozoa", "Corals", 
                                                                                                         ifelse(family == "Odobenidae", "Pinnipeds", 
                                                                                                                ifelse(family == "Otariidae", "Pinnipeds", 
                                                                                                                       ifelse(family == "Phocidae", "Pinnipeds", 
                                                                                                                              ifelse(class == "Reptilia", "Reptiles", 
                                                                                                                                     ifelse(class == "Elasmobranchii", "Sharks - Rays", 
                                                                                                                                            ifelse(phylum == "Porifera", "Sponges", 
                                                                                                                                                   ifelse(class == "Appendicularia", "Tunicates", 
                                                                                                                                                          ifelse(class == "Ascidiacea", "Tunicates", 
                                                                                                                                                                 ifelse(class == "Thaliacea", "Tunicates", 
                                                                                                                                                                        ifelse(phylum == "Acanthocephala", "Sea worms", 
                                                                                                                                                                               ifelse(phylum == "Annelida", "Sea worms", 
                                                                                                                                                                                      ifelse(class == "Aplacophora", "Sea worms", 
                                                                                                                                                                                             ifelse(phylum == "Chaetognatha", "Sea worms", 
                                                                                                                                                                                                    ifelse(phylum == "Cycliophora", "Sea worms", 
                                                                                                                                                                                                           ifelse(phylum == "Kamptozoa", "Sea worms", 
                                                                                                                                                                                                                  ifelse(phylum == "Gastrotricha", "Sea worms", 
                                                                                                                                                                                                                         ifelse(phylum == "Gnathostomulida", "Sea worms", 
                                                                                                                                                                                                                                ifelse(phylum == "Hemichordata", "Sea worms", 
                                                                                                                                                                                                                                       ifelse(phylum == "Kinorhyncha", "Sea worms", 
                                                                                                                                                                                                                                              ifelse(phylum == "Loricifera", "Sea worms", 
                                                                                                                                                                                                                                                     ifelse(phylum == "Nematoda", "Sea worms", 
                                                                                                                                                                                                                                                            ifelse(phylum == "Nemertea", "Sea worms", 
                                                                                                                                                                                                                                                                    ifelse(phylum == "Phoronida", "Sea worms", 
                                                                                                                                                                                                                                                                           ifelse(phylum == "Platyhelminthes", "Sea worms", 
                                                                                                                                                                                                                                                                                  ifelse(phylum == "Priapulida", "Sea worms", 
                                                                                                                                                                                                                                                                                         ifelse(phylum == "Sipuncula", "Sea worms", 
                                                                                                                                                                                                                                                                                                ifelse(family == "Teredinidae", "Sea worms", 
                                                                                                                                                                                                                                                                                                       ifelse(phylum == "Xenacoelomorpha", "Sea worms", 
                                                                                                                                                                                                                                                                                                              ifelse(order == "Pantopoda", "Sea spiders", 
                                                                                                                                                                                                                                                                                                                     ifelse(phylum == "Chlorophyta", "Green algae",
                                                                                                                                                                                                                                                                                                                            ifelse(phylum == "Ochrophyta", "Brown algae", 
                                                                                                                                                                                                                                                                                                                                   ifelse(phylum == "Rhodophyta", "Red algae", 
                                                                                                                                                                                                                                                                                                                                          ifelse(family == "Xiphiidae", "Tunas - Billfishes", 
                                                                                                                                                                                                                                                                                                                                                 ifelse(order == "Perciformes" & family == "Pristiophoridae", "Tunas - Billfishes", 
                                                                                                                                                                                                                                                                                                                                                        ifelse(order == "Perciformes" & family == "Scombridae", "Tunas - Billfishes", "Other"))))))))))))))))))))))))))))))))))))))))))))))))

aqm_groups <- aqm_groups %>% 
  dplyr::mutate(groups_01 = ifelse(class == "Actinopterygii" & groups_01 == "Other", "Bony fish", 
                                   ifelse(class == "Sarcopterygii" & groups_01 == "Other", "Bony fish", 
                                          ifelse(class == "Branchiopoda" & groups_01 == "Other", "Crustaceans", 
                                                 ifelse(class == "Cephalocarida" & groups_01 == "Other", "Crustaceans",
                                                        ifelse(class == "Maxillopoda" & groups_01 == "Other", "Crustaceans",
                                                               ifelse(class == "Ostracoda" & groups_01 == "Other", "Crustaceans",
                                                                      ifelse(class == "Malacostraca" & groups_01 == "Other", "Crustaceans", groups_01))))))))

################
aqm3 <- aqm_groups %>% 
  dplyr::select(speciesID, groups_01)

ep <- fread("wgeneral_figs5/noregret-network_sps/epipelagic.csv") %>% 
  dplyr::mutate(speciesID = str_remove_all(string = feature_names, pattern = "_epipelagic")) %>%
  dplyr::arrange(pu) %>% 
  dplyr::select(pu, speciesID)

mp <- fread("wgeneral_figs5/noregret-network_sps/mesopelagic.csv") %>% 
  dplyr::mutate(speciesID = str_remove_all(string = feature_names, pattern = "_mesopelagic")) %>%
  dplyr::arrange(pu) %>% 
  dplyr::select(pu, speciesID)

bap <- fread("wgeneral_figs5/noregret-network_sps/bathyabyssopelagic.csv") %>% 
  dplyr::mutate(speciesID = str_remove_all(string = feature_names, pattern = "_bathyabyssopelagic")) %>%
  dplyr::arrange(pu) %>% 
  dplyr::select(pu, speciesID)


species_ep <- dplyr::left_join(x = ep, y = aqm3,  by = "speciesID") %>% 
  dplyr::rename(id = pu)
# length(unique(species_ep$speciesID)) # 8538 seems OK
species_mp <- dplyr::left_join(x = mp, y = aqm3,  by = "speciesID") %>% 
  dplyr::rename(id = pu)
# length(unique(species_mp$speciesID)) # 4329 seems OK
species_bap <- dplyr::left_join(x = bap, y = aqm3,  by = "speciesID") %>% 
  dplyr::rename(id = pu)
# length(unique(species_bap$speciesID)) # 1472 seems OK
species_vertical <- rbind(species_ep, species_mp, species_bap)
# length(unique(species_vertical$speciesID)) # 11701 seems OK

no_regrets_ep <- fread("wgeneral_figs5/noregret-network_sps/Epipelagic_sps.csv") %>% 
  dplyr::select(-V1)
no_regrets_mp <- fread("wgeneral_figs5/noregret-network_sps/Mesopelagic_sps.csv") %>% 
  dplyr::select(-V1)
no_regrets_bap <- fread("wgeneral_figs5/noregret-network_sps/Bathyabyssopelagic_sps.csv") %>% 
  dplyr::select(-V1)
no_regrets_vert <- fread("wgeneral_figs5/noregret-network_sps/Vertical_sps.csv") %>% 
  dplyr::select(ep_id, mp_id, bap_id, olayer)
  
# final_ep <- dplyr::left_join(x = species_ep, y = no_regrets_ep,  by = "id") %>% 
#   na.omit() %>% 
#   dplyr::arrange(id) %>% 
#   dplyr::group_by(speciesID, phylum) %>% 
#   dplyr::summarise(value = n()) %>% 
#   ungroup() %>% 
#   dplyr::group_by(phylum) %>% 
#   dplyr::summarise(value = n()) %>% 
#   ungroup() %>% 
#   dplyr::mutate(group = "Epipelagic") %>% 
#   dplyr::relocate(phylum, group, value) %>% 
#   dplyr::rename(individual = phylum) %>% 
#   dplyr::mutate(value = (value/sum(value))*100)

final_ep <- dplyr::left_join(x = species_ep, y = no_regrets_ep,  by = "id") %>% 
  # length(unique(final_ep$id)) # 30428 pu seems OK
  # length(unique(no_regrets_ep$id)) # 30428 pu seems OK
  na.omit() %>% 
  dplyr::group_by(speciesID) %>%
  dplyr::summarise(cells = n()) %>% 
  dplyr::mutate(rep_target = round((cells/length(unique(no_regrets_ep$id)))*100, digits = 4)) %>% 
  dplyr::arrange(-rep_target) %>% 
  ungroup()
  final_ep <- dplyr::left_join(x = final_ep, y = aqm3,  by = "speciesID") %>%
    dplyr::group_by(groups_01) %>% 
    dplyr::summarise(value = mean(rep_target)) %>% 
    ungroup() %>% 
    dplyr::arrange(-value) %>% 
    dplyr::mutate(group = "Epipelagic") %>% 
    dplyr::relocate(groups_01, group, value) %>% 
    dplyr::rename(individual = groups_01)
  
# final_mp <- dplyr::left_join(x = species_mp, y = no_regrets_mp,  by = "id") %>% 
#   na.omit() %>% 
#   dplyr::arrange(id) %>% 
#   dplyr::group_by(speciesID, phylum) %>% 
#   dplyr::summarise(value = n()) %>% 
#   ungroup() %>% 
#   dplyr::group_by(phylum) %>% 
#   dplyr::summarise(value = n()) %>% 
#   ungroup() %>% 
#   dplyr::mutate(group = "Mesopelagic") %>% 
#   dplyr::relocate(phylum, group, value) %>% 
#   dplyr::rename(individual = phylum) %>% 
#   dplyr::mutate(value = (value/sum(value))*100)
  
  final_mp <- dplyr::left_join(x = species_mp, y = no_regrets_mp,  by = "id") %>% 
    na.omit() %>% 
    dplyr::group_by(speciesID) %>%
    dplyr::summarise(cells = n()) %>% 
    dplyr::mutate(rep_target = round((cells/length(unique(no_regrets_mp$id)))*100, digits = 4)) %>% 
    dplyr::arrange(-rep_target) %>% 
    ungroup()
  final_mp <- dplyr::left_join(x = final_mp, y = aqm3,  by = "speciesID") %>%
    dplyr::group_by(groups_01) %>% 
    dplyr::summarise(value = mean(rep_target)) %>% 
    ungroup() %>% 
    dplyr::arrange(-value) %>% 
    dplyr::mutate(group = "Mesopelagic") %>% 
    dplyr::relocate(groups_01, group, value) %>% 
    dplyr::rename(individual = groups_01)

# final_bap <- dplyr::left_join(x = species_bap, y = no_regrets_bap,  by = "id") %>% 
#   na.omit() %>% 
#   dplyr::arrange(id) %>% 
#   dplyr::group_by(speciesID, phylum) %>% 
#   dplyr::summarise(value = n()) %>% 
#   ungroup() %>% 
#   dplyr::group_by(phylum) %>% 
#   dplyr::summarise(value = n()) %>% 
#   ungroup() %>% 
#   dplyr::mutate(group = "Bathyabyssopelagic") %>% 
#   dplyr::relocate(phylum, group, value) %>% 
#   dplyr::rename(individual = phylum) %>% 
#   dplyr::mutate(value = (value/sum(value))*100)
  
  final_bap <- dplyr::left_join(x = species_bap, y = no_regrets_bap,  by = "id") %>% 
    na.omit() %>% 
    dplyr::group_by(speciesID) %>%
    dplyr::summarise(cells = n()) %>% 
    dplyr::mutate(rep_target = round((cells/length(unique(no_regrets_bap$id)))*100, digits = 4)) %>% 
    dplyr::arrange(-rep_target) %>% 
    ungroup()
  final_bap <- dplyr::left_join(x = final_bap, y = aqm3,  by = "speciesID") %>%
    dplyr::group_by(groups_01) %>% 
    dplyr::summarise(value = mean(rep_target)) %>% 
    ungroup() %>% 
    dplyr::arrange(-value) %>% 
    dplyr::mutate(group = "Bathyabyssopelagic") %>% 
    dplyr::relocate(groups_01, group, value) %>% 
    dplyr::rename(individual = groups_01)

all_ep <- no_regrets_vert[,c(1,4)] %>% 
  dplyr::rename(id = ep_id)
all_mp <- no_regrets_vert[,c(2,4)] %>% 
  dplyr::rename(id = mp_id)
all_bap <- no_regrets_vert[,c(3,4)] %>% 
  dplyr::rename(id = bap_id)
  
final_all_ep <- dplyr::left_join(x = species_ep, y = all_ep,  by = "id") %>% 
  na.omit()
final_all_mp <- dplyr::left_join(x = species_mp, y = all_mp,  by = "id") %>% 
  na.omit()
final_all_bap <- dplyr::left_join(x = species_bap, y = all_bap,  by = "id") %>% 
  na.omit()

final_final <- rbind(final_all_ep, final_all_mp, final_all_bap)
# final_vertical <- final_final %>% 
#   na.omit() %>% 
#   dplyr::arrange(id) %>% 
#   dplyr::group_by(speciesID, phylum) %>% 
#   dplyr::summarise(value = n()) %>% 
#   ungroup() %>% 
#   dplyr::group_by(phylum) %>% 
#   dplyr::summarise(value = n()) %>% 
#   ungroup() %>% 
#   dplyr::mutate(group = "Vertical") %>% 
#   dplyr::relocate(phylum, group, value) %>% 
#   dplyr::rename(individual = phylum) %>% 
#   dplyr::mutate(value = (value/sum(value))*100)

# length(unique(species_vertical$id)); length(unique(final_final$id)) 
final_vertical <- final_final %>% 
  na.omit() %>%
  dplyr::group_by(speciesID) %>%
  dplyr::summarise(cells = n()) %>% 
  dplyr::mutate(rep_target = round((cells/(length(unique(final_all_ep$id))*3))*100, digits = 4)) %>% 
  dplyr::arrange(-rep_target) %>% 
  ungroup()
  final_vertical <- dplyr::left_join(x = final_vertical, y = aqm3,  by = "speciesID") %>%
    dplyr::group_by(groups_01) %>% 
    dplyr::summarise(value = mean(rep_target)) %>% 
    ungroup() %>% 
    dplyr::arrange(-value) %>% 
    dplyr::mutate(group = "Vertical") %>% 
    dplyr::relocate(groups_01, group, value) %>% 
    dplyr::rename(individual = groups_01)

# final_ep; final_mp; final_bap; final_vertical
# sum(final_ep$value); sum(final_mp$value); sum(final_bap$value); sum(final_vertical$value)  

final_df <- rbind(final_ep, final_mp, final_bap, final_vertical) %>% 
  data.frame() %>% 
  mutate(individual = factor(individual), group = factor(group))
data <- final_df

# Set a number of 'empty bar' to add at the end of each group
empty_bar <- 3
to_add <- data.frame( matrix(NA, empty_bar*nlevels(data$group), ncol(data)) )
colnames(to_add) <- colnames(data)
to_add$group <- rep(levels(data$group), each=empty_bar)
data <- rbind(data, to_add)
data <- data %>% arrange(group)
data$id <- seq(1, nrow(data))

# Get the name and the y position of each label
label_data <- data
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

# prepare a data frame for base lines
base_data <- data %>% 
  group_by(group) %>% 
  summarize(start=min(id), end=max(id) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))

# prepare a data frame for grid (scales)
grid_data <- base_data
grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start <- grid_data$start - 1
grid_data <- grid_data[-1,]

p <- ggplot(data, aes(x=as.factor(id), y=value, fill=group)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  geom_bar(aes(x=as.factor(id), y=value, fill=group), stat="identity", alpha=0.5) +
  # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
  # geom_segment(data=grid_data, aes(x = end, y = 80, xend = start, yend = 80), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  # geom_segment(data=grid_data, aes(x = end, y = 60, xend = start, yend = 60), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 40, xend = start, yend = 40), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 30, xend = start, yend = 30), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 20, xend = start, yend = 20), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 10, xend = start, yend = 10), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  # Add text showing the value of each 100/75/50/25 lines
  # annotate("text", x = rep(max(data$id),4), y = c(20, 40, 60, 80), label = c("20", "40", "60", "80") , color="grey", size=3 , angle=0, fontface="bold", hjust=1) +
  annotate("text", x = rep(max(data$id),4), y = c(10, 20, 30, 40), label = c("10", "20", "30", "40") , color="grey", size=3 , angle=0, fontface="bold", hjust=1) +
  geom_bar(aes(x=as.factor(id), y=value, fill=group), stat="identity", alpha=0.5) +
  ylim(-100,120) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm") 
  ) +
  coord_polar() + 
  geom_text(data=label_data, aes(x=id, y=value+10, label=individual, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE ) +
  # Add base line information
  geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  +
  geom_text(data=base_data, aes(x = title, y = -18, label=group), hjust=c(1,1,0,0), colour = "black", alpha=0.8, size=4, fontface="bold", inherit.aes = FALSE)

ggsave("wgeneral_figs5/BritoMorales_Fi_4c.pdf", width = 8, height = 8, dpi = 300)



#############
# Read the AquaMaps data (species occur .csv) and extract species' name according to the code
aqm2 <- fread("/Users/bri273/Desktop/AquaMaps_wflow/AquaMaps/v2019a/speciesoccursum.csv", stringsAsFactors = FALSE, fill = TRUE) %>% 
  dplyr::select(speciesID, genus, species) %>% 
  dplyr::mutate(scientific_name = paste(genus, species, sep = " ")) %>% 
  dplyr::select(speciesID, scientific_name)

# Reading IUCN .csv and filtering by Threatened categories (Vulberable[VU], Endangered[EN], Critically Endangered[CR])
iucn <- fread("features_1020CSV10100_targets-mix_noduplicates/IUCN_REDLIST_2020.csv", stringsAsFactors = FALSE, fill = TRUE) %>% 
  dplyr::select(scientific_name, category) %>% 
  dplyr::filter(category %in%  c("CR", "EN", "VU")) # VU????

# Merging species_targets df and iucn df
iucn_th <- left_join(x = aqm2, y = iucn,  by = "scientific_name") %>% 
  na.omit()

aqm <- fread("/Users/bri273/Desktop/AquaMaps_wflow/AquaMaps/v2019a/speciesoccursum.csv", fill = TRUE) %>% 
  dplyr::filter(rank == 1) %>% 
  dplyr::select(speciesID, phylum)

aqm_iucn <- left_join(x = aqm, y = iucn_th,  by = "speciesID") %>% 
  na.omit() %>% 
  dplyr::select(speciesID, phylum)

ep <- fread("wgeneral_figs5/noregret-network_sps/epipelagic.csv") %>% 
  dplyr::mutate(speciesID = str_remove_all(string = feature_names, pattern = "_epipelagic")) %>%
  dplyr::arrange(pu) %>% 
  dplyr::select(pu, speciesID)

mp <- fread("wgeneral_figs5/noregret-network_sps/mesopelagic.csv") %>% 
  dplyr::mutate(speciesID = str_remove_all(string = feature_names, pattern = "_mesopelagic")) %>%
  dplyr::arrange(pu) %>% 
  dplyr::select(pu, speciesID)

bap <- fread("wgeneral_figs5/noregret-network_sps/bathyabyssopelagic.csv") %>% 
  dplyr::mutate(speciesID = str_remove_all(string = feature_names, pattern = "_bathyabyssopelagic")) %>%
  dplyr::arrange(pu) %>% 
  dplyr::select(pu, speciesID)

species_ep <- dplyr::left_join(x = ep, y = aqm_iucn,  by = "speciesID") %>% 
  dplyr::rename(id = pu) %>% 
  na.omit()
species_mp <- dplyr::left_join(x = mp, y = aqm_iucn,  by = "speciesID") %>% 
  dplyr::rename(id = pu) %>% 
  na.omit()
species_bap <- dplyr::left_join(x = bap, y = aqm_iucn,  by = "speciesID") %>% 
  dplyr::rename(id = pu) %>% 
  na.omit()
species_vertical <- rbind(species_ep, species_mp, species_bap)

no_regrets_ep <- fread("wgeneral_figs5/noregret-network_sps/Epipelagic_sps.csv") %>% 
  dplyr::select(-V1)
no_regrets_mp <- fread("wgeneral_figs5/noregret-network_sps/Mesopelagic_sps.csv") %>% 
  dplyr::select(-V1)
no_regrets_bap <- fread("wgeneral_figs5/noregret-network_sps/Bathyabyssopelagic_sps.csv") %>% 
  dplyr::select(-V1)
no_regrets_vert <- fread("wgeneral_figs5/noregret-network_sps/Vertical_sps.csv") %>% 
  dplyr::select(ep_id, mp_id, bap_id, olayer)


final_ep <- dplyr::left_join(x = species_ep, y = no_regrets_ep,  by = "id") %>% 
  na.omit() %>% 
  dplyr::group_by(speciesID) %>%
  dplyr::summarise(cells = n()) %>% 
  dplyr::mutate(rep_target = round((cells/length(unique(no_regrets_ep$id)))*100, digits = 4)) %>% 
  dplyr::arrange(-rep_target) %>% 
  ungroup()
final_ep <- dplyr::left_join(x = final_ep, y = aqm,  by = "speciesID") %>%
  dplyr::group_by(phylum) %>% 
  dplyr::summarise(value = mean(rep_target)) %>% 
  ungroup() %>% 
  dplyr::arrange(-value) %>% 
  dplyr::mutate(group = "Epipelagic") %>% 
  dplyr::relocate(phylum, group, value) %>% 
  dplyr::rename(individual = phylum)

#################

ep_targets <- fread("wgeneral_figs5/noregret-network_sps/sps-rce-vocc-mixall_EpipelagicLayer_targets-iucn.csv") %>% 
  dplyr::mutate(speciesID = str_split(string = feature_names_prov , pattern = "_", simplify = TRUE)[,1]) %>% 
  dplyr::select(targets, speciesID)
ep_targets_range <- dplyr::left_join(x = ep_targets, y = aqm3,  by = "speciesID") %>% 
  na.omit() %>% 
  dplyr::select(targets, groups_01) %>% 
  dplyr::group_by(groups_01) %>% 
  dplyr::summarise(min_target = min(targets), max_target = max(targets), average = mean(targets), sd = sd(targets)) %>% 
  data.frame()



