



slp <- raster("CMIP6_zrasters_r1i1p1f1/ssp126/slpTrends_02-ep_AEMean_ssp126_r1i1p1f1_2020-2100_.tif")
slp2 <- ((slp*10)*8)
plot(slp2)

names.yrs2 <- paste("X", seq(as.Date(paste(2015, "1", "1", sep = "/")), as.Date(paste(2020, "12", "1", sep = "/")), by = "month"), sep = "") %>% 
  str_replace_all(pattern = "-", replacement = ".")

rs3 <- raster::subset(rs, names.yrs2)
rs3.b <- flip_rs(rs3)
index2 <- rep(1:nlayers(rs3.b), each = 12, length.out = nlayers(rs3.b))

rs3_min <- stackApply(x = rs3.b, indices = index2, fun = min)
plot(rs3_min$index_1)
rs3_max <- stackApply(x = rs3.b, indices = index2, fun = max)
plot(rs3_max$index_1)

rs3_range <- rs3_max - rs3_min
plot(rs3_range$index_1)

rs3_range_range_mean <- stackApply(x = rs3_range, indices = nlayers(rs3_range), fun = mean)
plot(rs3_range_range_mean)

test <- slp2/rs3_range_range_mean
plot(kader:::cuberoot(test$layer))

writeRaster(test, "CMIP6_zrasters_r1i1p1f1/ssp126/RCE_02-ep_AEMean_ssp126_r1i1p1f1_2015-2020_.tif")
