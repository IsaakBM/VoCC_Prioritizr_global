# This code was written by Isaac Brito-Morales (i.britomorales@uq.edu.au)
# Please do not distribute this code without permission.
# NO GUARANTEES THAT CODE IS CORRECT
# Caveat Emptor!

blm_sweet <- function(posthoc_csv, outdir, spot) {
  
  library(ggplot2)
  library(dplyr)
  library(patchwork)
  library(inflection)
  
  # Reading
    rs_final <- read.csv(posthoc_csv, stringsAsFactors = FALSE) %>% 
      # dplyr::mutate(scenario = as.factor(scenario)) %>% 
      dplyr::mutate(solution = as.factor(solution)) %>% 
      dplyr::mutate(BLM = as.factor(BLM)) %>% 
      dplyr::group_by(scenario, trade_off) %>% 
      dplyr::summarise(mean_cost = mean(total_cost, na.rm = TRUE), mean_perimeter = mean(perimeter_m , na.rm = TRUE)) %>% 
      data.frame()
    
  # Plotting
    ggplot() +
      geom_point(data = rs_final, aes(x = mean_perimeter, y = mean_cost, colour = trade_off), size = 3) +
      ggtitle("Perimeter vc Total Cost") +
      facet_wrap(~ scenario) +
      ggsave(paste(outdir, "BLM_sweet-spot", ".pdf", sep = ""), width = 15, height = 15)
      
  # Looping
    scenarios <- unique(rs_final$scenario)
    blm_list <- vector("list", length = length(scenarios))
    for(i in 1:length(scenarios)) {
      # Reading the file
        single <- rs_final %>% 
          dplyr::filter(scenario == scenarios[i])
        
        if(spot == "Z") {
          # Getting the numetaros index for the Sweet spot
            cost_X <- single$mean_cost[single$trade_off == "X"]
            cost_Y <- single$mean_cost[single$trade_off == "Y"]
          # Getting the denominator index for the Sweet spot
            bound_X <- single$mean_perimeter[single$trade_off == "X"]
            bound_Y <- single$mean_perimeter[single$trade_off == "Y"]
          # Calculating the sweet spot
            blm_list[[i]] <- round(abs((cost_X - cost_Y)/(bound_X - bound_Y)), digits = 7)
          
        } else if (spot == "b-c") {
          # Getting the numetaros index for the Sweet spot
            cost_X <- single$mean_cost[single$trade_off == "X"]
            cost_Y <- single$mean_cost[single$trade_off == "Y"]
            cost_Z <- single$mean_cost[single$trade_off == "Z"]
          # Getting the denominator index for the Sweet spot 
            bound_X <- single$mean_perimeter[single$trade_off == "X"]
            bound_Y <- single$mean_perimeter[single$trade_off == "Y"]
            bound_Z <- single$mean_perimeter[single$trade_off == "Z"]
          # Calculating the sweet spot
            blm_b <- round(abs((cost_Z - cost_Y)/(bound_Z - bound_Y)), digits = 7) 
            blm_c <- round(abs((cost_X - cost_Z)/(bound_X - bound_Z)), digits = 7)
              blm_list[[i]] <- cbind(blm_b, blm_c)
        }
    }
    names(blm_list) <- scenarios
    blm_df <- do.call(rbind, blm_list) %>% 
      as.data.frame()
  # Write data frame
    write.csv(blm_df, paste(outdir, "BLM_sweet-spot", ".csv", sep = ""))
  
}

blm_sweet(posthoc_csv = "Project05b_Rosa/prioritization_zblm-cal_rce-vocc_feature/PostHoc_Calibration.csv", 
          outdir = "Project05b_Rosa/prioritization_zblm-cal_rce-vocc_feature/", 
          spot = "b-c")



prio_scenarios <- unique(rs_final$scenario)
for(i in 1:length(prio_scenarios)) {
  
  single <- rs_final %>% 
    dplyr::filter(scenario == prio_scenarios[i])
  ggplot() +
    geom_point(data = single, aes(x = perimeter_m, y = total_cost, colour = BLM), size = 3) +
    ggtitle("Perimeter vc Total Cost") +
    ggsave(paste(outdir, prio_scenarios[i], ".pdf", sep = ""), width = 15, height = 10)
  
}

if(trade_off == "Z") {
  
  
}

