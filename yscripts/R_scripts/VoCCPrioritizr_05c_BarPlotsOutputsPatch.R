

path = "Project05b_Rosa/w_results-outputs_figures _final-g-blm"
data <- list.files(path = path, pattern = paste0(c(paste0("*Calibration_*", ".*.csv$")), collapse = "|"), full.names = TRUE)
  df <- read.csv(data) %>% 
  dplyr::filter(trade_off == "g") %>% 
  dplyr::select(-BLM) %>% 
  dplyr::group_by(scenario) %>% 
  dplyr::summarise(mean_cost = mean(total_cost), sd_cost = sd(total_cost), mean_area = mean(log10(area_m2)), sd_area = sd(log10(area_m2)))


theme_bar <- list(theme(panel.grid.minor = element_blank(),
                        panel.grid.major = element_blank(),
                        panel.background = element_blank(),
                        plot.background = element_rect(fill="white"),
                        panel.border = element_blank(),
                        axis.line = element_line(size = 1),
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

p1 <- ggplot(data = df, aes(x = scenario, y = mean_cost)) +
  geom_bar(aes(x = scenario, y = mean_cost, fill = "#bdbdbd"), colour = "black", stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(x = scenario, ymin = mean_cost, ymax = mean_cost + sd_cost), width = 0.1, position = position_dodge(0.9)) +
  geom_hline(yintercept = as.numeric(df[1,2]), linetype = "dashed", color = "#f03b20", size = 1.5) +
  scale_x_discrete(limits = c("02_EpipelagicLayer_cost-fish_feat-sps-rce-vocc", 
                              "02_EpipelagicLayer_cost-fish_feat-sps-rce-vocc_ssp126",
                              "02_EpipelagicLayer_cost-fish_feat-sps-rce-vocc_ssp245",
                              "02_EpipelagicLayer_cost-fish_feat-sps-rce-vocc_ssp585"), 
                   labels = c("Base", 
                              "SSP126",
                              "SSP245",
                              "SSP585")) +
  scale_y_continuous(breaks = seq(0, 5000, 1000), limits = c(0, 5000), expand = c(0, 0)) + # remove white space of x axis
  labs(y = expression(Total~average~cost~(log[10]~USD))) +
  scale_fill_manual(values = c("#bdbdbd")) +
  theme_bar

# p2 <- ggplot(data = df, aes(x = scenario, y = mean_area)) +
#   geom_bar(aes(x = scenario, y = mean_area, fill = "#bdbdbd"), colour = "black", stat = "identity", position = position_dodge()) +
#   geom_errorbar(aes(x = scenario, ymin = mean_area, ymax = mean_area + sd_area), width = 0.1, position = position_dodge(0.9)) +
#   geom_hline(yintercept = as.numeric(df[1,4]), linetype = "dashed", color = "#f03b20", size = 1.5) +
#   scale_x_discrete(limits = c("02_EpipelagicLayer_cost-fish_feat-sps-rce-vocc", 
#                               "02_EpipelagicLayer_cost-fish_feat-sps-rce-vocc_ssp126",
#                               "02_EpipelagicLayer_cost-fish_feat-sps-rce-vocc_ssp245",
#                               "02_EpipelagicLayer_cost-fish_feat-sps-rce-vocc_ssp585"), 
#                    labels = c("Base", 
#                               "SSP126",
#                               "SSP245",
#                               "SSP585")) +
#   # scale_y_continuous(breaks = seq(0, 5000, 1000), limits = c(0, 5000), expand = c(0, 0)) + # remove white space of x axis
#   labs(y = expression(Total~average~cost~(log[10]~USD~kg^-1))) +
#   scale_fill_manual(values = c("#bdbdbd")) +
#   theme_bar

p0_final2 <- p1 +
  plot_annotation(tag_prefix = "",
                  tag_levels = "A", 
                  tag_suffix = ".",) +
  theme_bar +
  ggsave("Project05b_Rosa/w_results-outputs_figures _final-g-blm/barplot_cost.png", width = 8, height = 6, dpi = 300)

