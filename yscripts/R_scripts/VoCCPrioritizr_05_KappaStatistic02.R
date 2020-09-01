library(sf)
library(dplyr)
library(stringr)
library(irr)
library(psych)
library(ggplot2)
library(RColorBrewer)


path = "ublm-cal_0520rce-vocc040_targets-mix"
shp = "shapefiles_rasters/01_abnjs_nofilterdepth"
# Kappa function to avoid double for loop

  kappa_function <- function(data, col_n) {
    f1 <- list()
    for(j in 1:ncol(data)) {
      df1 <- cbind(data[, col_n], data[, j])
      f1[[j]]<- as.vector(unlist(unlist(cohen.kappa(x = df1))[1]))
    }
    f1_final <- do.call(rbind, f1)
    return(f1_final)
  }

# List of directories
  dir.scenarios <- paste(list.dirs(path = path, full.names = TRUE, recursive = FALSE), sep = "/")
  dir.shp <- paste(list.dirs(path = shp, full.names = TRUE, recursive = FALSE), sep = "/")
  csvs <- list.files(path = dir.scenarios, pattern = "*_cost.*.csv$", full.names = TRUE)
  shps <- list.files(path = dir.shp, pattern = "*.shp$", full.names = TRUE)
  single_csv <- lapply(csvs, function(x) {final <- read.csv(x)})
  single_shp <- lapply(shps, function(x) {final <- st_read(x)})
  ns <- lapply(csvs, function(x) {
    olayer <- unlist(strsplit(basename(x), split = "_"))[2]
    scenario <- ifelse(str_detect(string = basename(x), pattern = "ssp"), 
                       unlist(strsplit(basename(x), split = "_"))[5], "Base")
    final <- paste(olayer, scenario, sep = "_")})
  names(single_csv) <- ns
  names(single_shp) <- basename(shps)

if(ncol(single[[1]]) <= 6) { 
  
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
  
  df_final <- do.call(cbind, df_list)
  df_final[is.na(df_final)] <- 0
  
  f_list <- list()
  for(j in 1:ncol(df_final)) {
    f_list[[j]]<- kappa_function(df_final, col_n = j)
  }
  
  final <- do.call(cbind, f_list)
  colnames(final) <- names(single_csv)
  rownames(final) <- names(single_csv)
  test2 <- reshape::melt(final)
  
  write.csv()
  test2$X1 <- as.character(test2$X1)
  test2$X2 <- as.character(test2$X2)
  x_scenarios <- as.character(unique(test2$X1))
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
    
  
  ggheatmap <- ggplot(data = test2, aes(x = X1, y = X2, fill = value)) + 
    geom_tile(color = "white") + 
    geom_text(aes(X2, X1, label = round(value, digits = 2)), color = "black", size = 4) +
    scale_fill_gradient2(low = "blue", high = "red", mid = "yellow", 
                         midpoint = 0.5, limit = c(0,1), space = "Lab", 
                         name="Kappa\nCorrelation") +
    scale_x_discrete(limits = x_scenarios, labels = x_labs) +
    scale_y_discrete(limits = y_scenarios, labels = y_labs) +
    theme_minimal() + 
    theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                     size = 10, hjust = 1))+
    coord_fixed() +
    ggsave("wgeneral_figs/KAPPA_0520rce-vocc040_targets-mix.pdf", width = 10, height = 10, dpi = 300)
  
  }













