

rce <- function(omon, slp, from, to, outdir, ...) {
  
  library(raster)
  library(dplyr)
  library(sf)
  library(stringr)
  
  # A function to flip the data
    flip_rs <- function(data) {
      for (i in 1:nlayers(data)) {
        if (i == 1) {
          single <- subset(data, i)
          rs <- as.data.frame(rasterToPoints(single))
          rs[,1] <- ifelse(rs[,1] < 0, rs[,1] + 180, rs[,1] - 180)
          rs2 <- rasterFromXYZ(rs)
          st <- rs2
        } else {
          single <- subset(data, i)
          rs <- as.data.frame(rasterToPoints(single))
          rs[,1] <- ifelse(rs[,1] < 0, rs[,1] + 180, rs[,1] - 180)
          rs2 <- rasterFromXYZ(rs)
          st <- stack(st, rs2)
        }
      }
      return(st)
    }
  
  # Getting the years/month to calculate de RCE index
    names.yrs2 <- paste("X", seq(as.Date(paste(from, "1", "1", sep = "/")), as.Date(paste(to, "12", "1", sep = "/")), by = "month"), sep = "") %>%
      str_replace_all(pattern = "-", replacement = ".")
  # Read, subset and flip the data
    rs1 <- readAll(stack(omon)) %>% 
      subset(names.yrs2) %>% 
      flip_rs()
  # Get ANNUAL min and max to estimate the rage to get the RCE
    index1 <- rep(1:nlayers(rs1), each = 12, length.out = nlayers(rs1))
    rs1_min <- stackApply(x = rs1, indices = index1, fun = min)
    rs1_max <- stackApply(x = rs1, indices = index1, fun = max)
    # Range among the period selected
      rs1_range <- rs1_max - rs1_min
      rs1_range_mean <- stackApply(x = rs1_range, indices = nlayers(rs1_range), fun = mean) # calculate the annual mean range for the period selected
  # Get the slope
    slp  <- (readAll(raster(slp))*10)*8
  
  # Calculate the RCE
    RCE <- abs(slp/rs1_range_mean) # absolute value since it has no unit
    # Write the object
      ns <- basename(omon)
        olayer <- unlist(strsplit(x = ns, split = "_"))[1]
        model <- unlist(strsplit(x = ns, split = "_"))[3]
        ssp <- unlist(strsplit(x = ns, split = "_"))[4]
        name.rs <- paste(olayer, "RCE", model, ssp, paste(from, to, sep = "-"), sep = "_")
      # Write the raster 
        writeRaster(RCE, paste(outdir, name.rs, ".tif", sep = ""), overwrite = TRUE)
}

# system.time(rce(omon = "/QRISdata/Q1216/BritoMorales/Project04b/CMIP6_zrasters_zensemble/thetao_05deg/ssp126/01_SurfaceLayer/01-sf_thetao_AEMean_ssp126_r1i1p1f1_2015-2100.grd", 
#                 slp = "/QRISdata/Q1216/BritoMorales/Project04b/vocc_a_nointer/slpTrends/ssp126/01_SurfaceLayer/slpTrends_01-sf_AEMean_ssp126_r1i1p1f1_2020-2100_.tif", 
#                 from = 2015, 
#                 to = 2020, 
#                 outdir = "/QRISdata/Q1216/BritoMorales/Project04b/vocc_a_nointer/RCE/"))

system.time(rce(omon = "Inputs/CMIP6_zrasters_zensemble/ssp585/05_Seafloor/tob_05-bot_MEMean_ssp585_r1i1p1f1_2015-2100.grd", 
                slp = "Inputs/ClimateChange/SlpTrend/slpTrends_tob_MEMean_ssp585_r1i1p1f1_2050-2100_.tif", 
                from = 2015, 
                to = 2020, 
                outdir = "Inputs/ClimateChange/RCE/ssp585/05_Seafloor/"))

