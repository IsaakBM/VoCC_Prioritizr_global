

################
aqm <- fread("/Users/bri273/Desktop/AquaMaps_wflow/AquaMaps/v2019a/speciesoccursum.csv", fill = TRUE) %>% 
  dplyr::filter(rank == 1) %>% 
  dplyr::select(speciesID, phylum)

ep <- fread("epipelagic.csv") %>% 
  dplyr::mutate(speciesID = str_remove_all(string = feature_names, pattern = "_epipelagic")) %>%
  dplyr::arrange(pu) %>% 
  dplyr::select(pu, speciesID)

mp <- fread("mesopelagic.csv") %>% 
  dplyr::mutate(speciesID = str_remove_all(string = feature_names, pattern = "_mesopelagic")) %>%
  dplyr::arrange(pu) %>% 
  dplyr::select(pu, speciesID)

bap <- fread("bathyabyssopelagic.csv") %>% 
  dplyr::mutate(speciesID = str_remove_all(string = feature_names, pattern = "_bathyabyssopelagic")) %>%
  dplyr::arrange(pu) %>% 
  dplyr::select(pu, speciesID)

species_ep <- dplyr::left_join(x = ep, y = aqm,  by = "speciesID") %>% 
  dplyr::rename(id = pu)
species_mp <- dplyr::left_join(x = mp, y = aqm,  by = "speciesID") %>% 
  dplyr::rename(id = pu)
species_bap <- dplyr::left_join(x = bap, y = aqm,  by = "speciesID") %>% 
  dplyr::rename(id = pu)

no_regrets_ep <- fread("ublm-cal_1020rce-vocc1050_targets-mix_rawcost_noduplicates/Epipelagic_sps.csv") %>% 
  dplyr::select(-V1)
no_regrets_mp <- fread("ublm-cal_1020rce-vocc1050_targets-mix_rawcost_noduplicates/Mesopelagic_sps.csv") %>% 
  dplyr::select(-V1)
no_regrets_bap <- fread("ublm-cal_1020rce-vocc1050_targets-mix_rawcost_noduplicates/Bathyabyssopelagic_sps.csv") %>% 
  dplyr::select(-V1)
  
final_ep <- dplyr::left_join(x = species_ep, y = no_regrets_ep,  by = "id") %>% 
  na.omit() %>% 
  dplyr::arrange(id) %>% 
  dplyr::group_by(speciesID, phylum) %>% 
  dplyr::summarise(value = n()) %>% 
  ungroup() %>% 
  dplyr::group_by(phylum) %>% 
  dplyr::summarise(value = n()) %>% 
  ungroup() %>% 
  dplyr::mutate(group = "Epipelagic") %>% 
  dplyr::relocate(phylum, group, value) %>% 
  dplyr::rename(individual = phylum) %>% 
  dplyr::mutate(value = (value/sum(value))*100)

final_mp <- dplyr::left_join(x = species_mp, y = no_regrets_mp,  by = "id") %>% 
  na.omit() %>% 
  dplyr::arrange(id) %>% 
  dplyr::group_by(speciesID, phylum) %>% 
  dplyr::summarise(value = n()) %>% 
  ungroup() %>% 
  dplyr::group_by(phylum) %>% 
  dplyr::summarise(value = n()) %>% 
  ungroup() %>% 
  dplyr::mutate(group = "Mesopelagic") %>% 
  dplyr::relocate(phylum, group, value) %>% 
  dplyr::rename(individual = phylum) %>% 
  dplyr::mutate(value = (value/sum(value))*100)

final_bap <- dplyr::left_join(x = species_bap, y = no_regrets_bap,  by = "id") %>% 
  na.omit() %>% 
  dplyr::arrange(id) %>% 
  dplyr::group_by(speciesID, phylum) %>% 
  dplyr::summarise(value = n()) %>% 
  ungroup() %>% 
  dplyr::group_by(phylum) %>% 
  dplyr::summarise(value = n()) %>% 
  ungroup() %>% 
  dplyr::mutate(group = "Bathyabyssopelagic") %>% 
  dplyr::relocate(phylum, group, value) %>% 
  dplyr::rename(individual = phylum) %>% 
  dplyr::mutate(value = (value/sum(value))*100)

final_test <- dplyr::left_join(x = species_bap, y = no_regrets_bap,  by = "id") %>% 
  na.omit() %>% 
  dplyr::arrange(id) %>% 
  dplyr::group_by(speciesID, phylum) %>% 
  dplyr::summarise(value = n()) %>% 
  ungroup() %>% 
  dplyr::group_by(phylum) %>% 
  dplyr::summarise(value = n()) %>% 
  ungroup() %>% 
  dplyr::mutate(group = "test") %>% 
  dplyr::relocate(phylum, group, value) %>% 
  dplyr::rename(individual = phylum) %>% 
  dplyr::mutate(value = (value/sum(value))*100)
  
final_ep; final_mp; final_bap; 
sum(final_ep$value); sum(final_mp$value); sum(final_bap$value) 

final <- rbind(final_ep, final_mp, final_bap, final_test) %>% 
  data.frame() %>% 
  mutate(individual = factor(individual), group = factor(group))
data <- final

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
  geom_segment(data=grid_data, aes(x = end, y = 80, xend = start, yend = 80), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 60, xend = start, yend = 60), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 40, xend = start, yend = 40), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 20, xend = start, yend = 20), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  # Add text showing the value of each 100/75/50/25 lines
  annotate("text", x = rep(max(data$id),4), y = c(20, 40, 60, 80), label = c("20", "40", "60", "80") , color="grey", size=3 , angle=0, fontface="bold", hjust=1) +
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

ggsave("wgeneral_figs5/BritoMorales_Fi_4-test.pdf", width = 8, height = 8, dpi = 300)




