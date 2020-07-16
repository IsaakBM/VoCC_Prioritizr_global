
library(data.table)
library(dplyr)
library(sf)
library(exactextractr)

pu_vocc <- "features_CSVs/04_BathyAbyssopelagicLayer_cost-fish_feat-sps-rce_blm-vocc_ssp245/voccMag_04-bap_AEMean_ssp245_2020-2100.csv"
bound <- "prioritization_ydatfiles/04_BathyAbyssopelagicLayer_cost-fish_feat-sps-rce_blm-vocc_ssp245/bound_04_BathyAbyssopelagicLayer_cost-fish_feat-sps-rce_blm-vocc_ssp245.dat"

pus_vocc <- read.csv(pu_vocc)
pus_vocc <- pus_vocc %>% 
  dplyr::mutate(climate_feature = ifelse(is.na(climate_feature), 
                                         median(dplyr::filter(pus_vocc, pus_vocc$climate_feature != 0)$climate_feature), 
                                         climate_feature)) %>% 
  dplyr::mutate(climate_feature = abs(climate_feature))
  
head(pus_vocc)
bound <- read.table(bound, sep = ",", header = TRUE)
head(bound)

test <- list()
for(i in 1:nrow(bound)) {
  index1 <- unlist(bound[i,][1])
  index2 <- unlist(bound[i,][2])
  single1 <- pus_vocc[pus_vocc$pu == index1,][[5]] # number of colum is FIVE now
  single2 <- pus_vocc[pus_vocc$pu == index2,][[5]] # number of colum is FIVE now
  test[[i]] <- cbind(single1, single2)
}

a <- do.call(rbind, test)
head(a)

bound2 <- cbind(bound, a)
head(bound2)
bound2$boundary_vocc <- (bound2$single1 + bound2$single2)/2
  head(bound2, n = 20)
  names(bound2)
final <- bound2[,c(1,2,6)]
names(final) <- c("id1", "id2", "boundary")
  head(final, n = 20)
final <- final %>% na.omit()
head(final, n = 20)
write.table(final, "prioritization_ydatfiles/bound-vocc_04_BathyAbyssopelagicLayer_cost-fish_feat-sps-rce_blm-vocc_ssp245.dat", row.names = FALSE, sep = ",", quote = FALSE)

rm(list = ls())
