
library(ggplot2)
library(dplyr)
library(patchwork)
library(inflection)


rs_final <- read.csv("prioritization_zblm-cal/PostHoc_Calibration_01.csv")
# rs_final <- rs_final %>% 
#   dplyr::filter(scenario != "02_EpipelagicLayer_1_10")
rs_final$scenario <- as.factor(rs_final$scenario)
rs_final$solution <- as.factor(rs_final$solution)
rs_final$BLM <- as.factor(rs_final$BLM)

rs_final2 <- rs_final %>%
  filter(scenario != "02_EpipelagicLayer_0_10")
rs_final2 <- rs_final2 %>%
  filter(scenario != "02_EpipelagicLayer_1_10")

rs_final2 <- rs_final %>%
  filter(scenario != "02_EpipelagicLayer_0_10" | scenario != "02_EpipelagicLayer_1_10")

cost_x <- mean(rs_final2$total_cost[rs_final2$scenario == "02_EpipelagicLayer_0_10"], na.rm = TRUE)
cost_y <- mean(rs_final2$total_cost[rs_final2$scenario == "02_EpipelagicLayer_1_10"], na.rm = TRUE)

bound_x <- mean(rs_final2$perimeter_m[rs_final2$scenario == "02_EpipelagicLayer_0_10"], na.rm = TRUE)
bound_y <- mean(rs_final2$perimeter_m[rs_final2$scenario == "02_EpipelagicLayer_1_10"], na.rm = TRUE)

blm_final <- abs((cost_x - cost_y)/(bound_x - bound_y))

rs_final3 <- rs_final %>%
  filter(scenario == "02_EpipelagicLayer_0.00015_10" | scenario == "02_EpipelagicLayer_0.00022_10" | scenario == "02_EpipelagicLayer_0.00029_10" | scenario == "02_EpipelagicLayer_0.00036_10" | scenario == "02_EpipelagicLayer_0.00051_10")



p2 <- ggplot() +
  geom_point(data = rs_final, aes(x = perimeter_m, y = total_cost, colour = BLM), size = 3) +
  ggtitle("Perimeter vc Total Cost") +
  facet_wrap(~ scenario) +
  ggsave("ypdfs/BLM_calibration_test01.pdf", width = 10, height = 10) +
  theme_bw(base_size = 15)

