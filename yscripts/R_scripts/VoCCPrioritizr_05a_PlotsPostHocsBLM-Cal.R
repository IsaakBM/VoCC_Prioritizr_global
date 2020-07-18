
library(ggplot2)
library(dplyr)
library(patchwork)
library(inflection)


rs_final <- read.csv("prioritization_zblm-cal/PostHoc_Calibration_01.csv", stringsAsFactors = FALSE) %>% 
  # dplyr::mutate(scenario = as.factor(scenario)) %>% 
  dplyr::mutate(solution = as.factor(solution)) %>% 
  dplyr::mutate(BLM = as.factor(BLM))
head(rs_final)

scenarios <- unique(rs_final$scenario)
blm_list <- vector("list", length = length(scenarios))
for(i in 1:length(scenarios)) {
  # Reading the file
    single <- rs_final %>% 
      dplyr::filter(scenario == scenarios[i])
  # Getting the numetaros index for the Sweet spot
    cost_x <- mean(single$total_cost[single$BLM == 0], na.rm = TRUE)
    cost_y <- mean(single$total_cost[single$BLM == 1], na.rm = TRUE)
  # Getting the denominator index for the Sweet spot
    bound_x <- mean(single$perimeter_m[single$BLM == 0], na.rm = TRUE)
    bound_y <- mean(single$perimeter_m[single$BLM == 1], na.rm = TRUE)
  # 
    blm_list[[i]] <- abs((cost_x - cost_y)/(bound_x - bound_y))
}
names(blm_list) <- scenarios


