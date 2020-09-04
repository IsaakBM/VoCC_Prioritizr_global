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


# List of directories with appropiate file
dir.scenarios <- paste(list.dirs(path = path, full.names = TRUE, recursive = FALSE), sep = "/")
dir.shp <- paste(list.dirs(path = shp, full.names = TRUE, recursive = FALSE), sep = "/")
csvs <- list.files(path = dir.scenarios, pattern = "*_cost.*.csv$", full.names = TRUE)
shps <- list.files(path = dir.shp, pattern = "*.shp$", full.names = TRUE)
single_csv <- lapply(csvs, function(x) {final <- read.csv(x)})
single_shp <- lapply(shps, function(x) {final <- st_read(x)})