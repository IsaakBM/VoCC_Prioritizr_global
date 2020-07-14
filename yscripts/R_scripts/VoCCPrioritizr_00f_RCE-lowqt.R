path = "climate-change_inputs/RCE"
outdir = "climate-change_inputs/RCE_25qt/"

library(raster)
library(dplyr)
library(foreach)
library(doParallel)

# Folder's structure
dir.scenarios <- paste(list.dirs(path = path, full.names = TRUE, recursive = FALSE), sep = "/") # Climate Models Directory
dir.olayers <- paste(list.dirs(path = dir.scenarios, full.names = TRUE, recursive = FALSE), sep = "/") # Climate Models Directory per ocean layer

single.dir  <- paste(dir.olayers[1], list.files(path = paste(dir.olayers[1], sep = "/"), pattern = ".tif"), sep = "/") # what about more models?
rs <- readAll(stack(single.dir)) %>% subset(1)
qts <- quantile(rs)
rs[] <- ifelse(rs[] > as.vector(qts[2]), NA, rs[])
plot(kader:::cuberoot(rs))

ns <- basename(single.dir) # get the names to write the rasters
var <- unlist(strsplit(x = ns, split = "_"))[2]
olayer <- unlist(strsplit(x = ns, split = "_"))[1]
model <- unlist(strsplit(x = ns, split = "_"))[3]
ssp <- unlist(strsplit(x = ns, split = "_"))[4]
period <- unlist(strsplit(unlist(strsplit(x = ns, split = "[.]"))[1], split = "_"))[5]
name.rs <- paste(var, olayer, model, ssp, period, sep = "_")
# Write the rasters
writeRaster(rs_inter, paste(outdir, name.rs, ".tif", sep = ""), overwrite = TRUE)