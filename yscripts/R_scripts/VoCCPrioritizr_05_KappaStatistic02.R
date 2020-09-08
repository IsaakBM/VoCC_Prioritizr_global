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
    csvs <- list.files(path = dir.scenarios, pattern = "*_cost.*.csv$", full.names = TRUE)
    shps <- list.files(path = dir.shp, pattern = "*.shp$", full.names = TRUE)
    single_csv <- lapply(csvs, function(x) {final <- read.csv(x)})
    single_shp <- lapply(shps, function(x) {final <- st_read(x)})
    # Better names for files
      ns <- lapply(csvs, function(x) {
        olayer <- unlist(strsplit(basename(x), split = "_"))[2]
        scenario <- ifelse(str_detect(string = basename(x), pattern = "ssp"), 
                           unlist(strsplit(basename(x), split = "_"))[5], "Base")
        final <- paste(olayer, scenario, sep = "_")})
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
      x_scenarios <- c(x_scenarios[4], x_scenarios[1], x_scenarios[2], x_scenarios[3],
                       x_scenarios[8], x_scenarios[5], x_scenarios[6], x_scenarios[7],
                       x_scenarios[12], x_scenarios[9], x_scenarios[10], x_scenarios[11])
      x_labs <- c("Epipelagic Base", "Epipelagic SSP126", "Epipelagic SSP245", "Epipelagic SSP585", 
                  "Mesopelagic Base", "Mesopelagic SSP126", "Mesopelagic SSP245", "Mesopelagic SSP585", 
                  "BathyAbyssopelagic Base", "BathyAbyssopelagic SSP126", "BathyAbyssopelagic SSP245", "BathyAbyssopelagic SSP585")
      y_scenarios <- c(x_scenarios[12], x_scenarios[11], x_scenarios[10], x_scenarios[9],
                       x_scenarios[8], x_scenarios[7], x_scenarios[6], x_scenarios[5],
                       x_scenarios[4], x_scenarios[3], x_scenarios[2], x_scenarios[1])
      y_labs <- c("BathyAbyssopelagic SSP585", "BathyAbyssopelagic SSP245", "BathyAbyssopelagic SSP126", "BathyAbyssopelagic Base",
                  "Mesopelagic SSP585", "Mesopelagic SSP245", "Mesopelagic SSP126", "Mesopelagic Base",
                  "Epipelagic SSP585", "Epipelagic SSP245", "Epipelagic SSP126", "Epipelagic Base")
    # Defining generalities
      # pal1 <- brewer.pal(6, "RdPu")
      # cv <- c("0", "", "0.2", "", "", "0.5", "", "", "0.8", "", "1")
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
        # scale_fill_gradientn(name = "Kappa\nCorrelation",
        #                      colours = pal1,
        #                      limits = c(0, 1),
        #                      breaks = seq(0, 1, 0.1),
        #                      labels = cv, 
        #                      space = "Lab") +
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

system.time(kappa_correlation(path = "ublm-cal_0520rce-vocc040_targets-mix", 
                              shp = "shapefiles_rasters/01_abnjs_nofilterdepth", 
                              outdir = "ublm-cal_0520rce-vocc040_targets-mix/"))

# system.time(kappa_correlation(path = "/QRISdata/Q1216/BritoMorales/Project04b/vfinal-sol_figs/ublm-cal_0520rce-vocc020_targets-mix", 
#                               shp = "/QRISdata/Q1216/BritoMorales/Project04b/shapefiles_rasters/01_abnjs_nofilterdepth", 
#                               outdir = "/QRISdata/Q1216/BritoMorales/Project04b/vfinal-sol_figs/ublm-cal_0520rce-vocc020_targets-mix/"))



