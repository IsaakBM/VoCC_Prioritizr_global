# This code was written by Isaac Brito-Morales (i.britomorales@uq.edu.au)
# Please do not distribute this code without permission.
# NO GUARANTEES THAT CODE IS CORRECT
# Caveat Emptor!


kappa_correlation <- function(path, shp, outdir) {
  
  library(sf)
  library(dplyr)
  library(stringr)
  library(irr)
  library(ggplot2)
  library(RColorBrewer)
  library(reshape)
  
  # Kappa function to avoid double for loop
    kappa_function <- function(data, col_n) {
      f1 <- list()
      for(j in 1:ncol(data)) {
        df1 <- cbind(data[, col_n], data[, j])
        kappa_value <- irr::kappa2(df1)
        f1[[j]] <- kappa_value$value
      }
      f1_final <- do.call(rbind, f1)
      return(f1_final)
    }
  
  # List of directories with appropiate file
    dir.scenarios <- paste(list.dirs(path = path, full.names = TRUE, recursive = FALSE), sep = "/")
    dir.shp <- paste(list.dirs(path = shp, full.names = TRUE, recursive = FALSE), sep = "/")
    rds <- list.files(path = dir.scenarios, pattern = ".rds", full.names = TRUE)
    shps <- list.files(path = dir.shp, pattern = "*.shp$", full.names = TRUE)
    single_csv <- unlist(lapply(rds, function(x) {ff <- readRDS(x)[2]}), recursive = FALSE)
    single_shp <- lapply(shps, function(x) {ff <- st_read(x)})
    # Better names for files
      ns <- lapply(rds, function(x) {
        olayer <- str_extract(string = basename(x), pattern = paste0(c("Epipelagic", "Mesopelagic", "BathyAbyssopelagic", "Seafloor"), collapse = "|"))
        scenario <- str_extract(string = basename(x), pattern = paste0(c("ssp126", "ssp245", "ssp585"), collapse = "|"))
        ff <- paste(olayer, scenario, sep = "_")})
      names(single_csv) <- ns
      names(single_shp) <- basename(shps)
      
  
  if(ncol(single_csv[[1]]) <= 6) { # just for 1 solution now but later add "else"
    # Extract the solution for every Layer/SSP scenario
      df_list <- vector("list", length = length(single_csv))
      for(i in 1:length(single_csv)) {
        if(str_detect(string = names(single_csv[i]), pattern = "Epi") == TRUE) {
          df_list[[i]] <- single_csv[[i]]$solution_1[match(single_shp[[1]]$layer, single_csv[[i]]$id)]
        } else if(str_detect(string = names(single_csv[i]), pattern = "Meso") == TRUE) {
          df_list[[i]] <- single_csv[[i]]$solution_1[match(single_shp[[2]]$layer, single_csv[[i]]$id)]
        } else if(str_detect(string = names(single_csv[i]), pattern = "Bathy") == TRUE) {
          df_list[[i]] <- single_csv[[i]]$solution_1[match(single_shp[[3]]$layer, single_csv[[i]]$id)]
        } else if(str_detect(string = names(single_csv[i]), pattern = "Sea") == TRUE) {
          df_list[[i]] <- single_csv[[i]]$solution_1[match(single_shp[[4]]$layer, single_csv[[i]]$id)]
        }
      }
    # Creating a matrix for every climatic scenario
      df_final <- do.call(cbind, df_list)
      df_final[is.na(df_final)] <- 0 # just in case :-)
    # Creating a Kappa correlation matrix using the function above
      f_list <- list()
      for(j in 1:ncol(df_final)) {
        f_list[[j]]<- kappa_function(df_final, col_n = j)}
    # Final matrix + names
      final <- do.call(cbind, f_list)
      colnames(final) <- names(single_csv)
      rownames(final) <- names(single_csv)
      # Writing the object
        write.csv(final, paste(outdir, paste("Kappa_", basename(path), ".csv", sep = ""), sep = ""))
    
    # Reshaping the matrix for plotting
      final_reshape <- reshape::melt(final)
    # Some data manipulation
      final_reshape$X1 <- as.character(final_reshape$X1)
      final_reshape$X2 <- as.character(final_reshape$X2)
      x_scenarios <- as.character(unique(final_reshape$X1))
      x_scenarios <- c(x_scenarios[1], x_scenarios[2], x_scenarios[3],
                       x_scenarios[4], x_scenarios[5], x_scenarios[6],
                       x_scenarios[7], x_scenarios[8], x_scenarios[9], 
                       x_scenarios[10], x_scenarios[11], x_scenarios[12])
      x_labs <- c("Epipelagic SSP1-2.6", "Epipelagic SSP2-4.5", "Epipelagic SSP5-8.5", 
                  "Mesopelagic SSP1-2.6", "Mesopelagic SSP2-4.5", "Mesopelagic SSP5-8.5", 
                  "BathyAbyssopelagic SSP1-2.6", "BathyAbyssopelagic SSP2-4.5", "BathyAbyssopelagic SSP5-8.5", 
                  "Seafloor SSP1-2.6", "Seafloor SSP2-4.5", "Seafloor SSP5-8.5")
      y_scenarios <- c(x_scenarios[12],x_scenarios[11], x_scenarios[10],
                       x_scenarios[9],x_scenarios[8], x_scenarios[7], 
                       x_scenarios[6], x_scenarios[5], x_scenarios[4], 
                       x_scenarios[3], x_scenarios[2], x_scenarios[1])
      y_labs <- c("Seafloor SSP5-8.5", "Seafloor SSP2-4.5", "Seafloor SSP1-2.6",
                  "BathyAbyssopelagic SSP5-8.5", "BathyAbyssopelagic SSP2-4.5", "BathyAbyssopelagic SSP1-2.6",
                  "Mesopelagic SSP5-8.5", "Mesopelagic SSP2-4.5", "Mesopelagic SSP1-2.6",
                  "Epipelagic SSP5-8.5", "Epipelagic SSP2-4.5", "Epipelagic SSP1-2.6")
    # Defining themes
      theme_opts1 <- list(theme(plot.margin = margin(0, 0, 0, 0, "cm"),
                                axis.title.x = element_blank(),
                                axis.title.y = element_blank(),
                                panel.grid.major = element_blank(),
                                panel.border = element_blank(),
                                panel.background = element_blank(),
                                axis.ticks = element_blank()))
    # Plot
      ggheatmap <- ggplot(data = final_reshape, aes(x = X1, y = X2, fill = value)) + 
        geom_tile(color = "white") + 
        geom_text(aes(X2, X1, label = round(value, digits = 2)), color = "black", size = 4) +
        scale_fill_gradient2(low = "#deebf7", high = "#08519c", mid = "#4292c6",
                             midpoint = 0.5, limit = c(0,1), space = "Lab", 
                             name = "Kappa\nCorrelation") +
        scale_x_discrete(limits = x_scenarios, labels = x_labs) +
        scale_y_discrete(limits = y_scenarios, labels = y_labs) +
        theme_minimal() + 
        theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                         size = 10, hjust = 1)) +
        coord_fixed()
      ggheatmap +
        theme_opts1 +
        ggsave(paste(outdir, paste("Kappa_", basename(path), ".pdf", sep = ""), sep = ""), width = 10, height = 10, dpi = 300)
  }
}

system.time(kappa_correlation(path = "Prioritisation/PrioritizrSolutionsCost", 
                              shp = "Output/01_abnjs_nofilterdepth", 
                              outdir = "Figures/MS_v1/"))