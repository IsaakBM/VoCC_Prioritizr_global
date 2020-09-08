library(data.table)
library(dplyr)
library(vegan)

dt0 <- read.csv("vfinal-sol_figs_test/ublm-cal_0520rce-vocc020_targets-mix/02_EpipelagicLayer_cost-fish_feat-sps-rce-vocc_base126/02_EpipelagicLayer_cost-fish_feat-sps-rce-vocc_base126_0_1.csv") %>% 
  dplyr::select(solution_1) %>% 
  dplyr::rename(base = solution_1)

dt01 <- transpose(dt0)
rownames(dt01) <- colnames(dt0)
colnames(dt01) <- rownames(dt0)

dt1 <- read.csv("vfinal-sol_figs_test/ublm-cal_0520rce-vocc020_targets-mix/02_EpipelagicLayer_cost-fish_feat-sps-rce-vocc_ssp585/02_EpipelagicLayer_cost-fish_feat-sps-rce-vocc_ssp585_0_1.csv") %>% 
  dplyr::select(solution_1) %>% 
  dplyr::rename(ssp585 = solution_1)

dt11 <- transpose(dt1)
rownames(dt11) <- colnames(dt1)
colnames(dt11) <- rownames(dt1)

dt_final <- rbind(dt01, dt11)
rownames(dt_final)



test <- vegdist(dt_final, method = "jaccard")
testClust <- hclust(test, method = "average")
plot(testClust, cex = 0.5)

path = "ublm-cal_0520rce-vocc040_targets-mix"
shp = "shapefiles_rasters/01_abnjs_nofilterdepth"
# List of directories with appropiate file
dir.scenarios <- paste(list.dirs(path = path, full.names = TRUE, recursive = FALSE), sep = "/")
dir.shp <- paste(list.dirs(path = shp, full.names = TRUE, recursive = FALSE), sep = "/")
csvs <- list.files(path = dir.scenarios, pattern = "*_cost.*.csv$", full.names = TRUE)
shps <- list.files(path = dir.shp, pattern = "*.shp$", full.names = TRUE)
single_csv <- lapply(csvs, function(x) {final <- read.csv(x)})
single_shp <- lapply(shps, function(x) {final <- st_read(x)})
# Bettwr names for files
ns <- lapply(csvs, function(x) {
  olayer <- unlist(strsplit(basename(x), split = "_"))[2]
  scenario <- ifelse(str_detect(string = basename(x), pattern = "ssp"), 
                     unlist(strsplit(basename(x), split = "_"))[5], "Base")
  final <- paste(olayer, scenario, sep = "_")})
names(single_csv) <- ns
names(single_shp) <- basename(shps)

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

# Creating the resemblance matrix 
  df_final <- do.call(rbind, df_list)
  df_final[is.na(df_final)] <- 0 # just in case :-)
  rownames(df_final) <- names(single_csv)
  colnames(df_final) <- single_csv[[1]]$id
# 
  test <- vegdist(df_final, method = "jaccard")
  testClust <- hclust(test, method = "average")
  dendo.test <- as.dendrogram(testClust)
  ddata <- dendro_data(dendo.test, type = "rectangle")
  
  p <- ggplot(segment(ddata)) + 
    geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) + 
    geom_text(data = ddata$labels, aes(x, y, label = label),
              hjust = 0, angle = 0, size = 3)+
    coord_flip() + 
    scale_y_reverse(expand = c(0.2, 0))

pdf("ublm-cal_0520rce-vocc040_targets-mix/ublm-cal_0520rce-vocc040_targets-mix.pdf", width = 20, height = 10)
plot(testClust, cex = 1)
dev.off()


test_mds <- metaMDS(test, distance = "jaccard", trymax = 100)
str(test_mds)
plot(test_mds)

row.names(test_mds$points)
text(test_mds, pos = 3, offset = 1)


