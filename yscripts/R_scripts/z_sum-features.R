

ep <- fread("features_0520CSV050_targets-mix/02_EpipelagicLayer_cost-fish_feat-sps-rce_blm-vocc/sps_EpipelagicLayer_targets.csv")
length(unique(ep$feature_names_prov))


ep_126 <- fread("features_0520CSV050_targets-mix/02_EpipelagicLayer_cost-fish_feat-sps-rce_blm-vocc_ssp126/sps-rce-vocc-mixall_EpipelagicLayer_provinces.csv")
ep_126_clim <- ep_126 %>% 
  dplyr::filter(str_detect(string = feature_names_prov, pattern = "_RCE") == TRUE) # just species
length(unique(ep_126_clim$feature_names_prov))

ep_245 <- fread("features_0520CSV050_targets-mix/02_EpipelagicLayer_cost-fish_feat-sps-rce_blm-vocc_ssp245/sps-rce-vocc-mixall_EpipelagicLayer_provinces.csv")
ep_245_clim <- ep_245 %>% 
  dplyr::filter(str_detect(string = feature_names_prov, pattern = "_RCE") == TRUE) # just species
length(unique(ep_245_clim$feature_names_prov))

ep_585 <- fread("features_0520CSV050_targets-mix/02_EpipelagicLayer_cost-fish_feat-sps-rce_blm-vocc_ssp585/sps-rce-vocc-mixall_EpipelagicLayer_provinces.csv")
ep_585_clim <- ep_585 %>% 
  dplyr::filter(str_detect(string = feature_names_prov, pattern = "_RCE") == TRUE) # just species
length(unique(ep_585_clim$feature_names_prov))


mp_126 <- fread("features_0520CSV050_targets-mix/03_MesopelagicLayer_cost-fish_feat-sps-rce_blm-vocc_ssp126/sps-rce-vocc-mixall_MesopelagicLayer_provinces.csv")
mp_126_clim <- mp_126 %>% 
  dplyr::filter(str_detect(string = feature_names_prov, pattern = "_RCE") == TRUE) # just species
length(unique(mp_126_clim$feature_names_prov))

mp_245 <- fread("features_0520CSV050_targets-mix/03_MesopelagicLayer_cost-fish_feat-sps-rce_blm-vocc_ssp245/sps-rce-vocc-mixall_MesopelagicLayer_provinces.csv")
mp_245_clim <- mp_245 %>% 
  dplyr::filter(str_detect(string = feature_names_prov, pattern = "_RCE") == TRUE) # just species
length(unique(mp_245_clim$feature_names_prov))

mp_585 <- fread("features_0520CSV050_targets-mix/03_MesopelagicLayer_cost-fish_feat-sps-rce_blm-vocc_ssp585/sps-rce-vocc-mixall_MesopelagicLayer_provinces.csv")
mp_585_clim <- mp_585 %>% 
  dplyr::filter(str_detect(string = feature_names_prov, pattern = "_VoCC") == TRUE) # just species
length(unique(mp_585_clim$feature_names_prov))


bap_126 <- fread("features_0520CSV050_targets-mix/04_BathyAbyssopelagicLayer_cost-fish_feat-sps-rce_blm-vocc_ssp126/sps-rce-vocc-mixall_BathyAbyssopelagicLayer_provinces.csv")
bap_126_clim <- bap_126 %>% 
  dplyr::filter(str_detect(string = feature_names_prov, pattern = "_RCE") == TRUE) # just species
length(unique(bap_126_clim$feature_names_prov))

bap_245 <- fread("features_0520CSV050_targets-mix/04_BathyAbyssopelagicLayer_cost-fish_feat-sps-rce_blm-vocc_ssp245/sps-rce-vocc-mixall_BathyAbyssopelagicLayer_provinces.csv")
bap_245_clim <- bap_245 %>% 
  dplyr::filter(str_detect(string = feature_names_prov, pattern = "_RCE") == TRUE) # just species
length(unique(bap_245_clim$feature_names_prov))

bap_585 <- fread("features_0520CSV050_targets-mix/04_BathyAbyssopelagicLayer_cost-fish_feat-sps-rce_blm-vocc_ssp585/sps-rce-vocc-mixall_BathyAbyssopelagicLayer_provinces.csv")
bap_585_clim <- bap_585 %>% 
  dplyr::filter(str_detect(string = feature_names_prov, pattern = "_RCE") == TRUE) # just species
length(unique(bap_585_clim$feature_names_prov))


# epipelagic: VoCC_126: 43 slow climate features; 154 biodiversity in slow climate features
# epipelagic: RCE_126: 43 low climate features; 164 biodiversity in slow climate features
# mesopelagic: VoCC_126: 29 slow climate features; 211 biodiversity in slow climate features 
# mesopelagic: RCE_126: 29 low climate features; 200 biodiversity in slow climate features
# bathyabyssopelagic: VoCC_126: 29 slow climate features; 211 biodiversity in slow climate features
# bathyabyssopelagic: RCE_126: 29 low climate features; 168 biodiversity in slow climate features
# 
# epipelagic: VoCC_245: 43 slow climate features; 157 biodiversity in slow climate features
# epipelagic: RCE_245: 43 low climate features; 142 biodiversity in slow climate features
# mesopelagic: VoCC_245: 29 slow climate features; 221 biodiversity in slow climate features
# mesopelagic: RCE_245: 29 low climate features; 207 biodiversity in slow climate features
# bathyabyssopelagic: VoCC_245: 29 slow climate features; 216 biodiversity in slow climate features
# bathyabyssopelagic: RCE_245: 29 low climate features; 169 biodiversity in slow climate features
# 
# epipelagic: VoCC_585: 43 slow climate features; 148 biodiversity in slow climate features
# epipelagic: RCE_585: 43 low climate features; 145 biodiversity in slow climate features
# mesopelagic: VoCC_585: 29 slow climate features; 224 biodiversity in slow climate features
# mesopelagic: RCE_585: 29 low climate features; 201 biodiversity in slow climate features
# bathyabyssopelagic: VoCC_245: 29 slow climate features; 220 biodiversity in slow climate features
# bathyabyssopelagic: RCE_245: 29 low climate features; 176 biodiversity in slow climate features




