# This code was written by Isaac Brito-Morales (i.britomorales@uq.edu.au)
# Please do not distribute this code without permission.
# NO GUARANTEES THAT CODE IS CORRECT
# Caveat Emptor!

# MaxTarget% - PUs(i)/MaxPUs(all spp) * (MaxTarget%-MinTarget%)
# 0.50 - (5/100) * (0.50 - 0.10)

csvs_pus_provinces <- function(csv_olayer_prov, csv_olayer_species, olayer, min_target, max_target, outdir) {
  library(data.table)
  library(dplyr)
  
  # Species by planning units
  # Reading files
    file_olayer_pus <- fread(csv_olayer_prov) %>% 
      arrange(layer)
    file_olayer_species <- fread(csv_olayer_species) %>% 
      arrange(pu)
  # Match province name with species name
    file_olayer_species$province <- file_olayer_pus$province[match(file_olayer_species$pu, file_olayer_pus$layer)] # maybe with dplyr?
    file_olayer_species <- file_olayer_species %>% 
      mutate(feature_names_prov = ifelse(is.na(province), paste("non-categ", prov_name, sep = "_"),
                                         paste(feature_names, province, sep = "_")))
    # Writing the object [which is going to be used for 1b .dat function]
      ns1 <- paste("sps", olayer, "provinces", sep = "_")
      fwrite(file_olayer_species, paste(outdir, ns1, ".csv", sep = ""))
  
  # Defining targets ans creating .csv species targets file
    provinces_bypu <- unique(file_olayer_pus$province) # provinces for loop
    
    dt_final <- list()
    for(i in 1:length(provinces_bypu)) {
      
      dt1 <- file_olayer_pus %>% 
        filter(province == provinces_bypu[i])
      max_pus <- length(unique(dt1$layer))
      
      dt2 <- file_olayer_species %>% 
        filter(province == provinces_bypu[i]) %>%  
        group_by(feature_names_prov) %>% 
        summarise(cells = n()) %>% 
        mutate(targets = (max_target - ((cells/max_pus) * (max_target - min_target))))
      
      dt_final[[i]] <- dt2
      
    }
    dt_testing <- do.call(rbind, dt_final)
  
  # Writing the object [which is going to be used for 1b .dat function]
    ns2 <- paste("sps", olayer, "targets", sep = "_")
    fwrite(dt_testing, paste(outdir, ns2, ".csv", sep = ""))
}

csvs_pus_provinces(csv_olayer_prov = "CSVs/02_EpipelagicLayer/pus-epipelagic_Longhurst_.csv", 
                   csv_olayer_species = "CSVs/02_EpipelagicLayer/epipelagic.csv", 
                   olayer = "epipelagic", 
                   min_target = 0.1, 
                   max_target = 0.3, 
                   outdir = "CSVs/02_EpipelagicLayer/")


# ep <- fread("CSVs/02_EpipelagicLayer/sps_epipelagic_provinces.csv")
# ep_targets <- fread("CSVs/02_EpipelagicLayer/sps_epipelagic_targets.csv")
# length(unique(ep$feature_names_prov))
# length(unique(ep_targets$feature_names_prov))
# 
# mp <- fread("CSVs/03_MesopelagicLayer/sps_mesopelagic_provinces.csv")
# mp_targets <- fread("CSVs/03_MesopelagicLayer/sps_mesopelagic_targets.csv")
# length(unique(mp$feature_names_prov))
# length(unique(mp_targets$feature_names_prov))
# 
# bap <- fread("CSVs/04_BathyAbyssopelagicLayer/sps_bathyabyssopelagic_provinces.csv")
# bap_targets <- fread("CSVs/04_BathyAbyssopelagicLayer/sps_bathyabyssopelagic_targets.csv")
# length(unique(bap$feature_names_prov))
# length(unique(bap_targets$feature_names_prov))


