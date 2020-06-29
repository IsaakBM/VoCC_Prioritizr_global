# This code was written by Isaac Brito-Morales (i.britomorales@uq.edu.au)
# Please do not distribute this code without permission.
# NO GUARANTEES THAT CODE IS CORRECT
# Caveat Emptor!

# AIM: 
# marxan_input: directory of the outcome from marxan_inputs.R script
# pu_shpfile:
# outdir: 
# cost_file: climate velocity raster (or any raster?)
# cost_type

# pu: information planning units id, cost and status (available locked in and locked out). status can be associated with a MPAs or a status??? put that in the argument function
# spec: id (species ID); prop (how proportion for this species); spf (species penalty factor. missing target..).[this file is not associated with planning units]
# puvsp.dat: species information by planning units
# puvsp_sporder.dat: same as puvsp.dat by ordered by species
# bound.dat: boundary shared by planning units

# ~28 minutes global analysis at 0.5° resolution no parallel

marxan_dat_files <- function(marxan_input, marxan_input_csv, pu_shpfile, outdir, cost_file, cost_type, proj.geo) { # in this case cost_file is velocity but could be other

### List of pacakges that we will use
    list.of.packages <- c("raster", "sf", "dplyr", "future.apply", "cleangeo", "prioritizr", "lwgeom", "stringr")
    # If is not installed, install the pacakge
      new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])] # is the package in MY list of packages
      if(length(new.packages)) install.packages(new.packages) # if not, installed
      # Load packages
        lapply(list.of.packages, require, character.only = TRUE) # or require
  
### Reading marxan_input file
    shp_file <- st_read(marxan_input_shp)
    shp_csv <- fread(marxan_input_csv)

### bound.dat FILE
    shp_PU_sp <- st_read(pu_shpfile) %>% 
      st_transform(crs = proj.geo)
    
    length_mtx <- prioritizr::boundary_matrix(shp_PU_sp) # a spare matrix ::: deleting the TRUE argument due crashed 
    length_data <- as(length_mtx, "dgTMatrix")
    length_data <- data.frame(id1 = length_data@i + 1, id2 = length_data@j + 1, boundary = length_data@x)
      # keep same name of original pus by converting into factor
        length_data$id2 <- as.factor(length_data$id2)
        length_data$id1 <- as.factor(length_data$id1)
        # replace those name with original shapefile pu layer
          levels(length_data$id2) <- shp_PU_sp$layer
          levels(length_data$id1) <- shp_PU_sp$layer
    
            bound_name <- paste("bound", sep = "_")
            write.table(length_data, file = paste(outdir, bound_name, ".dat", sep = ""), row.names = FALSE, sep = ",", quote = FALSE)
          
### puvsp FILE (species[a different number that the species' code], pu[planning unit], amount[area])
    shp_df <- shp_csv %>% 
      select(pu, area_km2, feature_names_prov) %>% 
      transform(id = as.numeric(factor(feature_names_prov)))
    
    puvsp <- shp_df %>% 
      select(id, pu, area_km2) %>% 
      rename(species = id, amount = area_km2) %>% 
      arrange(pu)
    # Write the file puvsp
      puvsp_name <- paste("puvsp", sep = "_")
      write.table(puvsp, file = paste(outdir, puvsp_name, ".dat", sep = ""), row.names = FALSE, sep = ",", quote = FALSE)
      ### Write the file puvsp_sporder (orderer by species column)
          puvsp_order <- puvsp %>% arrange(species)
            puvsp_name_order <- paste("puvsp_sporder", sep = "_")
            write.table(puvsp_order, file = paste(outdir, puvsp_name_order, ".dat", sep = ""), row.names = FALSE, sep = ",", quote = FALSE)
    
### spec FILE (id[species ID], prop[proportion for protection of these species... 0.3 in general], spf[species penalty factor], name[species' name/code])  
    spec <- shp_df %>% 
      group_by(id, feature_names_prov) %>% 
      summarize(total_area = sum(area_km2)) %>% 
      select(id, feature_names_prov) %>% 
      rename(id = id, name = feature_names_prov) %>% 
      mutate(prop = 0.2, spf = 1.1) %>% 
      select(id, prop, spf, name) %>% 
      data.frame()
    # Write the file
      spec_name <- paste("spec", sep = "_")
      write.table(spec, file = paste(outdir, spec_name, ".dat", sep = ""), row.names = FALSE, sep = ",", quote = FALSE)
      
### pu FILE (id[planning units id], cost[velocity], status[available locked in and locked out])
      pu_shpfile <- st_read(pu_shpfile)
        var.names <- colnames(pu_shpfile)
        pu_shpfile <- pu_shpfile %>% 
          magrittr::set_colnames(ifelse(str_detect(var.names, "(?i).*id*"), "id", # or (?i).*id*|(?i).*pu*?
                                        ifelse(str_detect(var.names, "(?i)cost"), "cost", var.names))) %>% 
          dplyr::select(id, geometry) %>% 
          arrange(id)

      if(cost_type == "Raster") {
        # Read raster object
          cost_file <- readAll(raster(cost_file)) %>% disaggregate(2)
            names(cost_file) <- "layer"
            cost_file <- crop(cost_file, pu_shpfile) %>% as("SpatialPolygonsDataFrame")
          # Transform Cost layer into a SF object
            geo.prj <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" 
            sd_rs1 <- cost_file %>% spTransform(CRS(geo.prj)) %>% st_as_sf()
          # Getting cost value by planning unit
            pu_file <- st_intersection(pu_shpfile, sd_rs1) %>% 
              filter(st_geometry_type(.) %in% c("POLYGON", "MULTIPOLYGON")) %>% 
              rename(id = layer, cost = layer) %>% 
              mutate(cost = abs(cost)) %>% 
              arrange(id)
            pu_file <- pu_file[!duplicated(pu_file$id),] # just in case
              pu_file <- pu_shpfile %>%
                mutate(cost = pu_file$cost) %>% 
                select(id, cost, geometry)
            # Write cost shapefile
              st_write(pu_file, dsn = paste(outdir, sep = ""), driver = "ESRI Shapefile")
            # Write .dat file
              pu_file_df <- pu_file %>%
                mutate(cost = abs(cost), status = 0) %>% # status may change in the future (e.g., locked in and locked out? in this case locked out)
                data.frame() %>% 
                select(id, cost, status)
              pu_name <- paste("pu", sep = "_")
              write.table(pu_file_df, file = paste(outdir, pu_name, ".dat", sep = ""), row.names = FALSE, sep = ",", quote = FALSE)
          
      } else {
        # Read shapefile object (in this case is the same as the pu_shpfile)
          cost_shpfile <- st_read(cost_file)
          var.cost <- colnames(cost_shpfile)
          cost_shpfile <- cost_shpfile %>% 
            magrittr::set_colnames(ifelse(str_detect(var.cost, "(?i).*id*"), "id", # or (?i).*id*|(?i).*pu*?
                                          ifelse(str_detect(var.cost, "(?i)cost"), "cost", var.cost))) %>% 
            dplyr::select(id, cost, geometry) %>% 
            arrange(id)
          # Getting cost value by planning unit
            pu_file <- cost_shpfile[cost_shpfile$id %in% pu_shpfile$id, ]
              pu_file <- pu_file[!duplicated(pu_file$id),]
            # Write cost shapefile
              st_write(pu_file, dsn = paste(outdir, sep = ""), driver = "ESRI Shapefile")
            # Write .dat file
              pu_file_df <- pu_file %>%
                mutate(cost = abs(cost), status = 0) %>% # status may change in the future (e.g., locked in and locked out? in this case locked out)
                data.frame() %>% 
                select(id, cost, status)
              pu_name <- paste("pu", sep = "_")
              write.table(pu_file_df, file = paste(outdir, pu_name, ".dat", sep = ""), row.names = FALSE, sep = ",", quote = FALSE)
      }
        
  }

  # # for vocc magnitude when cost is the raster file
  #   system.time(marxan_dat_files(marxan_input = "features_shapefiles/01-surface_med_sps/sps_by_pu/sps_by_pu.shp",
  #                                pu_shpfile = "features_shapefiles/costlayer2/costlayer2.shp",
  #                                outdir = "output_datfiles/02-cost-vocc_feat-sps_ssp245/",
  #                                cost_file = "annualvocc_b_inter/mag/ssp245/01_SurfaceLayer_ssp245/voccMag-thetao_01-Surface_MPI-ESM1-2-HR_ssp245-05deg_2020-2100.tif",
  #                                cost_type = "Raster",
  #                                geo.proj = "+proj=aea +lat_1=60 +lat_2=60 +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"))
  
  # for trajectories shapefile when cost is the shapefile fishing effort
    system.time(marxan_dat_files(marxan_input = "features_shapefiles/02-surface_med_slowspeed_ssp245/sps_by_pu/sps_by_pu.shp",
                                 pu_shpfile = "features_shapefiles/costlayer2/costlayer2.shp",
                                 outdir = "output_datfiles/03-cost-fish_feat-sps-slowspeed_ssp245/",
                                 cost_file = "features_shapefiles/costlayer2/costlayer2.shp",
                                 cost_type = "Shapefile",
                                 geo.proj = "+proj=aea +lat_1=60 +lat_2=60 +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"))

  
  # velocity cost
  # "/Users/bri273/Desktop/VoCC_Marxan/rasters/vocc/vocc_2_GFDL-CM4_ssp585_r1i1p1f1_2015-2100_interpolation_02.grd" 

