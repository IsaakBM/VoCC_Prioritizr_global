
library(ggplot2)
library(dplyr)
library(patchwork)
library(inflection)


rs_final <- read.csv("output_prioritizr_blm-cal3/PostHoc_Calibration.csv")
# rs_final <- rs_final %>% 
#   dplyr::filter(scenario != "02_EpipelagicLayer_1_10")
rs_final$scenario <- as.factor(rs_final$scenario)
rs_final$solution <- as.factor(rs_final$solution)

rs_final2 <- rs_final %>% 
  filter(scenario == "02_EpipelagicLayer_0_10" | scenario == "02_EpipelagicLayer_1_10")

cost_x <- mean(rs_final2$total_cost[rs_final2$scenario == "02_EpipelagicLayer_0_10"], na.rm = TRUE)
cost_y <- mean(rs_final2$total_cost[rs_final2$scenario == "02_EpipelagicLayer_1_10"], na.rm = TRUE)

bound_x <- mean(rs_final2$perimeter_m[rs_final2$scenario == "02_EpipelagicLayer_0_10"], na.rm = TRUE)
bound_y <- mean(rs_final2$perimeter_m[rs_final2$scenario == "02_EpipelagicLayer_1_10"], na.rm = TRUE)

blm_final <- abs((cost_x - cost_y)/(bound_x - bound_y))

# p1 <- ggplot() +
#   geom_point(data = rs_final, aes(x = median_vocc, y = total_cost, colour = scenario, shape = best_solution), size = 3) +
#   ggtitle("Median VoCC vc Total Cost")
p2 <- ggplot() +
  geom_point(data = rs_final, aes(x = perimeter_m, y = total_cost, colour = scenario), size = 3) +
  ggtitle("Perimeter vc Total Cost")
# p3 <- ggplot() +
#   geom_point(data = rs_final, aes(x = fragmentation_m, y = total_cost, colour = scenario, shape = best_solution), size = 3) +
#   ggtitle("Fragmentation vc Total Cost")


pf <- p1/p2/p3 + 
  plot_annotation(tag_levels = 'A', tag_suffix = ')') +
  plot_layout(guides = 'collect') & 
  theme_bw(base_size = 15)

# pf <- set_last_plot(pf)
ggsave('BLM-Velocity/03_BLM_Calibration_test01.pdf', pf, width = 15, height = 15)



# ggplot() +
#   geom_point(data = rs_final, aes(x = perimeter_m, y = new_cost, colour = iteration, shape = best_solution), size = 3) +
#   ggsave("ztest/iteration67.pfd", width = 15, height = 10)


test <- i8[i8$perimeter_m <= 12230909,]

x <- test$perimeter_m
y <- test$new_cost
plot(x,y,cex=0.3,pch=19)
grid()
bb=ese(x,y,0);bb

pese=bb[,3]

abline(v=pese)
cc=bese(x,y,0)
cc$iplast

