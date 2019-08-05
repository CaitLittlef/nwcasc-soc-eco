## Bring in Terra Climate z-scores for eval'ing EVI change with departure in CMD.
# 4km is coarse enough th
 
## set directory for terraclimate
# tc.dir <- "//goshawk.sefs.uw.edu/Space_Lawler/Shared/BackedUp/Caitlin/NW CASC/Dir/terraclimate/"
tc.dir <- "D:/Shared/BackedUp/Caitlin/NW CASC/Dir/terraclimate/"
## Set directory for climatic deficit z-scores
# def.dir <- "//goshawk.sefs.uw.edu/Space_Lawler/Shared/BackedUp/Caitlin/NW CASC/Dir/terraclimate/def_z/"
def.dir <- "D:/Shared/BackedUp/Caitlin/NW CASC/Dir/terraclimate/def_z/"

# Compile all def z-scores as raster into a list -- keep only May - Sept
def.list <- lapply(list.files(def.dir, pattern = ".5.9.tif$", full.names = TRUE),
                   raster) # applies FUN (here, raster) to all files; dumps into list; $=end
def.list[[19]] # this is defz 1999
def.list[[37]] # this is defz 2017
def.list <- def.list[19:37] # Don't know why, but don't use double brackets to drop

defz <- stack(def.list)

# Process with EVI (may already be loaded)
EVI <- stack("D:/Shared/BackedUp/Caitlin/NW CASC/Dir/EVI.for.xfire.tif")
# crs already matches!
identical(crs(DEFZ), crs(EVI))

# Crop to study area, trim any rows/cols with all zeros.
# Mask will require same extents/res.
defz.crop <- defz %>% crop(EVI[[1]]) %>% trim()
extent(defz.crop) ; extent(EVI[[1]]) # Slight diff.

# Resample to EVI resolution.
defz.crop.rsmp <- resample(defz.crop, EVI[[1]])
plot(defz.crop.rsmp[[19]])

# Now mask out only those pixels that I'm keeping for EVI
defz.fin <- mask(defz.crop.rsmp, EVI[[1]])
plot(EVI[[1]])
plot(defz.fin[[1]])

# Save jic
writeRaster(defz.fin, "D:/Shared/BackedUp/Caitlin/NW CASC/Dir/defz.fin.tif")
defz.fin <- raster("D:/Shared/BackedUp/Caitlin/NW CASC/Dir/defz.fin.tif")

# Reclassify. Use 0.5 as drought bounds
hist(defz.fin[[1]])
# v wet: -Inf to -1 --> -2
# wet: -1 to -0.5 --> -1 
# baseline: -0.5 to 0.5 --> 0
# dry: 0.5 - 1 --> 1
# v. dry: 1 to Inf --> 2
drought <- reclassify(defz.fin, c(-Inf, -1, -2,
                                  -1, -0.5, -1,
                                  -0.5, 0.5, 0,
                                  0.5, 1, 1,
                                  1, Inf, 2))
plot(drought[[13]])

writeRaster(drought, "D:/Shared/BackedUp/Caitlin/NW CASC/Dir/drought.tif")

