
ep <- st_read("Output/01_abnjs_nofilterdepth/abnj_02-epipelagic_global_moll_05deg/abnj_02-epipelagic_global_moll_05deg.shp") %>% 
  dplyr::rename(id = layer)
a1 <- readRDS("SummStats/PrioritizrSolutionsCost/features_10100/NoRegret_PelagicSeafloor.rds") %>% 
  dplyr::mutate(solution = ifelse(solutionAll != 0, 1, 0)) %>% 
  dplyr::select(id, solution)

range(a1$solution)
length(a1$solution[a1$solution == "1"])

d1 <- left_join(ep, a1, "id")
d2 <- cbind(d1, st_coordinates(st_centroid(d1)))

d3 <- d2 %>% 
  dplyr::group_by(Y) %>% 
  dplyr::summarise(cells = length(solution), counts = sum(solution)) %>% 
  dplyr::arrange(Y) %>% 
  as_tibble() %>% 
  dplyr::select(Y, cells, counts)
  
d4 <- d3 %>% 
  dplyr::mutate(p_lat = (counts/cells)*100)


ggplot(data = d4, aes(x = p_lat, y = as.factor(Y))) +
  geom_bar(aes(x = p_lat, y = as.factor(Y), fill = "#bdbdbd"), colour = "#ec7014", stat = "identity", position = position_dodge()) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(y = "") +
  theme_minimal() +
  scale_fill_manual(values = c("#ec7014"), 
                    name = "") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_blank(),
        plot.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill = NA, size = 1),
        plot.title = element_text(face = "plain", size = 20, hjust = 0.5),
        plot.tag = element_text(colour = "black", face = "bold", size = 23),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = rel(1.5), angle = 0),
        axis.text.x = element_text(size = rel(1.5), angle = 0),
        axis.text.y = element_blank(),
        legend.title = element_text(colour = "black", face = "bold", size = 15),
        legend.text = element_text(colour = "black", face = "bold", size = 13),
        legend.key.height = unit(1.5, "cm"),
        legend.key.width = unit(1.5, "cm")) +
  theme(legend.position = "none")
