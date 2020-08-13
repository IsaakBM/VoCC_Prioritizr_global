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
        } else if (spot == "d-e") {
          # Getting the numetaros index for the Sweet spot
            cost_X <- single$mean_cost[single$trade_off == "X"]
            cost_Y <- single$mean_cost[single$trade_off == "Y"]
            cost_Z <- single$mean_cost[single$trade_off == "Z"]
            cost_b <- single$mean_cost[single$trade_off == "b"]
            cost_c <- single$mean_cost[single$trade_off == "c"]
          # Getting the denominator index for the Sweet spot 
            bound_X <- single$mean_perimeter[single$trade_off == "X"]
            bound_Y <- single$mean_perimeter[single$trade_off == "Y"]
            bound_Z <- single$mean_perimeter[single$trade_off == "Z"]
            bound_b <- single$mean_perimeter[single$trade_off == "b"]
            bound_c <- single$mean_perimeter[single$trade_off == "c"]
          # Calculating the sweet spot
            blm_d <- round(abs((cost_b - cost_Y)/(bound_b - bound_Y)), digits = 7) 
            blm_e <- round(abs((cost_X - cost_c)/(bound_X - bound_c)), digits = 7)
              blm_list[[i]] <- cbind(blm_d, blm_e)
        } else if (spot == "f-g") {
          # Getting the numetaros index for the Sweet spot
            cost_b <- single$mean_cost[single$trade_off == "b"]
            cost_c <- single$mean_cost[single$trade_off == "c"]
            cost_d <- single$mean_cost[single$trade_off == "d"]
            cost_e <- single$mean_cost[single$trade_off == "e"]
          # Getting the denominator index for the Sweet spot 
            bound_b <- single$mean_perimeter[single$trade_off == "b"]
            bound_c <- single$mean_perimeter[single$trade_off == "c"]
            bound_d <- single$mean_perimeter[single$trade_off == "d"]
            bound_e <- single$mean_perimeter[single$trade_off == "e"]
          # Calculating the sweet spot
            blm_f <- round(abs((cost_b - cost_d)/(bound_b - bound_d)), digits = 7) 
            blm_g <- round(abs((cost_e - cost_c)/(bound_e - bound_c)), digits = 7)
              blm_list[[i]] <- cbind(blm_f, blm_g)
          
        }
    }
    names(blm_list) <- scenarios
    blm_df <- do.call(rbind, blm_list) %>% 
      as.data.frame()
  # Write data frame
    write.csv(blm_df, paste(outdir, "BLM_sweet-spot", ".csv", sep = ""))
  
}

blm_sweet(posthoc_csv = "Project05b_Rosa/PostHoc_Calibration_0-1.csv", 
          outdir = "Project05b_Rosa/", 
          spot = "Z")

