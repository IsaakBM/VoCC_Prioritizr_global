
library(sf)
library(raster)
library(dplyr)
library(data.table)
library(future.apply)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(RColorBrewer)
library(patchwork)
library(stringr)

df <- read.csv("vfinal-sol_figs_test/Summ-Stats_ublm-cal_0520rce-vocc040_targets-mix.csv")
names(df)
str(df)

df2 <- df %>%
  mutate(vocc_log = log10(vocc_median + 1),
         vocc_log = ifelse(scenario == "Base", vocc_log*-1, vocc_log))


theme_bar <- list(theme(panel.grid.minor = element_blank(),
                        panel.grid.major = element_blank(),
                        panel.background = element_blank(),
                        plot.background = element_rect(fill =  "white"),
                        panel.border = element_blank(),
                        axis.line = element_blank(),
                        axis.text.x = element_text(size = rel(1.8), angle = 0),
                        axis.text.y = element_text(size = rel(2), angle = 0),
                        axis.ticks = element_line(size = 2),
                        axis.ticks.length = unit(.15, "cm"), 
                        axis.title.x = element_blank(),
                        axis.title.y = element_text(size = rel(2), angle = 90),
                        plot.title = element_text(size = 4),
                        legend.position = "none",
                        legend.text = element_text(size = 5),
                        legend.title = element_blank(),
                        legend.key.size = unit(2, "cm"),
                        legend.spacing.x = unit(1.0, 'cm'),
                        plot.tag = element_text(size = 25, face = "bold")))

ggplot(data = df2, aes(x = layer, y = vocc_log, fill = scenario)) +
  geom_bar(data = subset(df2, scenario == "Base"), stat = "identity", mapping = aes(y = vocc_log)) +
  geom_bar(data = subset(df2, scenario == "SSP126"), position = "identity", stat = "identity") +
  geom_bar(data = subset(df2, scenario == "SSP245"), position = "identity", stat = "identity") +
  geom_bar(data = subset(df2, scenario == "SSP585"), position = "identity", stat = "identity") +
  geom_hline(yintercept = 0, colour = "black") +
  geom_hline(yintercept = 1, colour = "black", linetype ="dashed") +
  geom_hline(yintercept = 1.5, colour = "black", linetype ="dashed") +
  geom_hline(yintercept = 2, colour = "black", linetype ="dashed") +
  geom_hline(yintercept = -1, colour = "black", linetype ="dashed") +
  geom_hline(yintercept = -1.5, colour = "black", linetype ="dashed") +
  geom_hline(yintercept = -2, colour = "black", linetype ="dashed") +
  coord_flip() +
  guides(fill = guide_legend(reverse = T)) +
  scale_y_continuous(breaks = seq(-3, 3, 1), limits = c(-3, 3), expand = c(0, 0)) + # remove white space of x axis
  theme_minimal() #+
  theme_bar
           
test <- read.csv("wgeneral_figs2/Summ-Stats_ublm-cal_0520rce-vocc050_targets-mix_rawcost-iucn.csv")
test2 <- test %>% 
  dplyr::mutate(scenario2 = ifelse(str_detect(as.character(scenario), pattern = "base"), "Base",
                                   ifelse(str_detect(as.character(scenario), pattern = "ssp126"), "SSP126", 
                                          ifelse(str_detect(as.character(scenario), pattern = "ssp245"), "SSP245", "SSP585")))) %>% 
  dplyr::mutate(olayer = ifelse(str_detect(as.character(scenario), pattern = "EpipelagicLayer") & str_detect(as.character(scenario), pattern = "base126"), "Epipelagic SSP126", 
                                ifelse(str_detect(as.character(scenario), pattern = "EpipelagicLayer") & str_detect(as.character(scenario), pattern = "base245"), "Epipelagic SSP245", 
                                       ifelse(str_detect(as.character(scenario), pattern = "EpipelagicLayer") & str_detect(as.character(scenario), pattern = "base585"), "Epipelagic SSP585", 
                                              ifelse(str_detect(as.character(scenario), pattern = "EpipelagicLayer") & str_detect(as.character(scenario), pattern = "ssp126"), "Epipelagic SSP126", 
                                                     ifelse(str_detect(as.character(scenario), pattern = "EpipelagicLayer") & str_detect(as.character(scenario), pattern = "ssp245"), "Epipelagic SSP245", 
                                                            ifelse(str_detect(as.character(scenario), pattern = "EpipelagicLayer") & str_detect(as.character(scenario), pattern = "ssp585"), "Epipelagic SSP585", 
                                                                   ifelse(str_detect(as.character(scenario), pattern = "MesopelagicLayer") & str_detect(as.character(scenario), pattern = "base126"), "Mesopelagic SSP126", 
                                                                          ifelse(str_detect(as.character(scenario), pattern = "MesopelagicLayer") & str_detect(as.character(scenario), pattern = "base245"), "Mesopelagic SSP245", 
                                                                                 ifelse(str_detect(as.character(scenario), pattern = "MesopelagicLayer") & str_detect(as.character(scenario), pattern = "base585"), "Mesopelagic SSP585", 
                                                                                        ifelse(str_detect(as.character(scenario), pattern = "MesopelagicLayer") & str_detect(as.character(scenario), pattern = "ssp126"), "Mesopelagic SSP126", 
                                                                                               ifelse(str_detect(as.character(scenario), pattern = "MesopelagicLayer") & str_detect(as.character(scenario), pattern = "ssp245"), "Mesopelagic SSP245", 
                                                                                                      ifelse(str_detect(as.character(scenario), pattern = "MesopelagicLayer") & str_detect(as.character(scenario), pattern = "ssp585"), "Mesopelagic SSP585", 
                                                                                                             ifelse(str_detect(as.character(scenario), pattern = "BathyAbyssopelagicLayer") & str_detect(as.character(scenario), pattern = "base126"), "BathyAbyssopelagic SSP126", 
                                                                                                                    ifelse(str_detect(as.character(scenario), pattern = "BathyAbyssopelagicLayer") & str_detect(as.character(scenario), pattern = "base245"), "BathyAbyssopelagic SSP245", 
                                                                                                                           ifelse(str_detect(as.character(scenario), pattern = "BathyAbyssopelagicLayer") & str_detect(as.character(scenario), pattern = "base585"), "BathyAbyssopelagic SSP585", 
                                                                                                                                  ifelse(str_detect(as.character(scenario), pattern = "BathyAbyssopelagicLayer") & str_detect(as.character(scenario), pattern = "ssp126"), "BathyAbyssopelagic SSP126", 
                                                                                                                                         ifelse(str_detect(as.character(scenario), pattern = "BathyAbyssopelagicLayer") & str_detect(as.character(scenario), pattern = "ssp245"), "BathyAbyssopelagic SSP245", "BathyAbyssopelagic SSP585")))))))))))))))))) %>% 
  dplyr::mutate(vocc_log = log10(vocc_median + 1),
                vocc_log = ifelse(scenario2 == "Base", vocc_log*-1, vocc_log)) %>% 
  dplyr::mutate(rce_log = log10(rce_median + 1),
                rce_log = ifelse(scenario2 == "Base", rce_log*-1, rce_log)) %>% 
  dplyr::mutate(cost_log = log10(total_cost + 1),
                cost_log = ifelse(scenario2 == "Base", cost_log*-1, cost_log))


p_vocc <- ggplot(data = test2, aes(x = olayer, y = vocc_log, fill = scenario2)) +
  geom_bar(data = subset(test2, scenario2 == "Base"), stat = "identity", mapping = aes(y = vocc_log)) +
  geom_bar(data = subset(test2, scenario2 == "SSP126"), position = "identity", stat = "identity") +
  geom_bar(data = subset(test2, scenario2 == "SSP245"), position = "identity", stat = "identity") +
  geom_bar(data = subset(test2, scenario2 == "SSP585"), position = "identity", stat = "identity") +
  geom_hline(yintercept = 0, colour = "black") +
  geom_hline(yintercept = 1, colour = "black", linetype ="dashed") +
  geom_hline(yintercept = 1.5, colour = "black", linetype ="dashed") +
  geom_hline(yintercept = 2, colour = "black", linetype ="dashed") +
  geom_hline(yintercept = -1, colour = "black", linetype ="dashed") +
  geom_hline(yintercept = -1.5, colour = "black", linetype ="dashed") +
  geom_hline(yintercept = -2, colour = "black", linetype ="dashed") +
  scale_x_discrete(limits = rev(c("Epipelagic SSP126", 
                              "Epipelagic SSP245",
                              "Epipelagic SSP585",
                              "Mesopelagic SSP126", 
                              "Mesopelagic SSP245",
                              "Mesopelagic SSP585", 
                              "BathyAbyssopelagic SSP126", 
                              "BathyAbyssopelagic SSP245",
                              "BathyAbyssopelagic SSP585")), 
                   labels = rev(c("", 
                                  "Epipelagic",
                                  "",
                                  "", 
                                  "Mesopelagic ",
                                  "", 
                                  "", 
                                  "BathyAbyssopelagic",
                                  ""))) +
  coord_flip() +
  guides(fill = guide_legend(reverse = T)) +
  scale_y_continuous(breaks = seq(-3, 3, 1), limits = c(-3, 3), expand = c(0, 0)) + # remove white space of x axis
  theme_minimal() +
  ggtitle(expression(Climate~velocity~(km~dec^-1)))


p_rce <- ggplot(data = test2, aes(x = olayer, y = rce_log, fill = scenario2)) +
  geom_bar(data = subset(test2, scenario2 == "Base"), stat = "identity", mapping = aes(y = rce_log)) +
  geom_bar(data = subset(test2, scenario2 == "SSP126"), position = "identity", stat = "identity") +
  geom_bar(data = subset(test2, scenario2 == "SSP245"), position = "identity", stat = "identity") +
  geom_bar(data = subset(test2, scenario2 == "SSP585"), position = "identity", stat = "identity") +
  geom_hline(yintercept = 0, colour = "black") +
  geom_hline(yintercept = 0.5, colour = "black", linetype ="dashed") +
  geom_hline(yintercept = 1.5, colour = "black", linetype ="dashed") +
  geom_hline(yintercept = -0.5, colour = "black", linetype ="dashed") +
  geom_hline(yintercept = -1.5, colour = "black", linetype ="dashed") +
  scale_x_discrete(limits = rev(c("Epipelagic SSP126", 
                                  "Epipelagic SSP245",
                                  "Epipelagic SSP585",
                                  "Mesopelagic SSP126", 
                                  "Mesopelagic SSP245",
                                  "Mesopelagic SSP585", 
                                  "BathyAbyssopelagic SSP126", 
                                  "BathyAbyssopelagic SSP245",
                                  "BathyAbyssopelagic SSP585")), 
                   labels = rev(c("", 
                                  "Epipelagic",
                                  "",
                                  "", 
                                  "Mesopelagic ",
                                  "", 
                                  "", 
                                  "BathyAbyssopelagic",
                                  ""))) +
  coord_flip() +
  guides(fill = guide_legend(reverse = T)) +
  scale_y_continuous(breaks = seq(-2, 2, 1), limits = c(-2, 2), expand = c(0, 0)) + # remove white space of x axis
  theme_minimal() +
  ggtitle("Relative Climate Exposure (RCE)")
  
p_cost <- ggplot(data = test2, aes(x = olayer, y =  cost_log , fill = scenario2)) +
  geom_bar(data = subset(test2, scenario2 == "Base"), stat = "identity", mapping = aes(y = cost_log)) +
  geom_bar(data = subset(test2, scenario2 == "SSP126"), position = "identity", stat = "identity") +
  geom_bar(data = subset(test2, scenario2 == "SSP245"), position = "identity", stat = "identity") +
  geom_bar(data = subset(test2, scenario2 == "SSP585"), position = "identity", stat = "identity") +
  geom_hline(yintercept = 0, colour = "black") +
  geom_hline(yintercept = 4, colour = "black", linetype ="dashed") +
  geom_hline(yintercept = 5, colour = "black", linetype ="dashed") +
  geom_hline(yintercept = -4, colour = "black", linetype ="dashed") +
  geom_hline(yintercept = -5, colour = "black", linetype ="dashed") +
  scale_x_discrete(limits = rev(c("Epipelagic SSP126", 
                                  "Epipelagic SSP245",
                                  "Epipelagic SSP585",
                                  "Mesopelagic SSP126", 
                                  "Mesopelagic SSP245",
                                  "Mesopelagic SSP585", 
                                  "BathyAbyssopelagic SSP126", 
                                  "BathyAbyssopelagic SSP245",
                                  "BathyAbyssopelagic SSP585")), 
                   labels = rev(c("", 
                                  "Epipelagic",
                                  "",
                                  "", 
                                  "Mesopelagic ",
                                  "", 
                                  "", 
                                  "BathyAbyssopelagic",
                                  ""))) +
  coord_flip() +
  guides(fill = guide_legend(reverse = T)) +
  scale_y_continuous(breaks = seq(-8, 8, 1), limits = c(-8, 8), expand = c(0, 0)) + # remove white space of x axis
  theme_minimal() +
  ggtitle("Opportunity Cost (USD)")


p0_final2 <- (p_cost | p_vocc | p_rce) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_prefix = "",
                  tag_levels = "A",
                  tag_suffix = ".",) +
  # theme_minimal() +
  ggsave("wgeneral_figs2/Summ-Stats_ublm-cal_0520rce-vocc050_targets-mix_rawcost-iucn.pdf", width = 25, height = 8, dpi = 300)






























