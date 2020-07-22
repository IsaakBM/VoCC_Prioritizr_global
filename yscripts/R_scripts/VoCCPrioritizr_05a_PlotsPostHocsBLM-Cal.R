
library(ggplot2)
library(dplyr)
library(patchwork)
library(inflection)


blm_sweet <- function(posthoc_csv, outdir) { 
  
  library(dplyr)
  # Reading
    rs_final <- read.csv(posthoc_csv, stringsAsFactors = FALSE) %>% 
      # dplyr::mutate(scenario = as.factor(scenario)) %>% 
      dplyr::mutate(solution = as.factor(solution)) %>% 
      dplyr::mutate(BLM = as.factor(BLM))
    
  # Plotting
    ggplot() +
      geom_point(data = rs_final, aes(x = perimeter_m, y = total_cost, colour = BLM), size = 3) +
      ggtitle("Perimeter vc Total Cost") +
      facet_wrap(~ scenario) +
      ggsave(paste(outdir, "BLM_sweet-spot", ".pdf", sep = ""), width = 15, height = 15)
      
  # Looping
    scenarios <- unique(rs_final$scenario)
    blm_list <- vector("list", length = length(scenarios))
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
      # Calculating the sweet spot
        blm_list[[i]] <- round(abs((cost_x - cost_y)/(bound_x - bound_y)), digits = 7)
    }
    names(blm_list) <- scenarios
    blm_df <- do.call(rbind, blm_list) %>% 
      as.data.frame()
  # Write data frame
    write.csv(blm_df, paste(outdir, "BLM_sweet-spot", ".csv", sep = ""))
  
}

blm_sweet(posthoc_csv = "prioritization_zblm-cal/PostHoc_Calibration_00.csv", 
          outdir = "prioritization_zblm-cal/")
