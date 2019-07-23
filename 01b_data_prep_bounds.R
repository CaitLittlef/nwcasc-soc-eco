### MASK NON-FORESTS, NON-WA, MAYBE NON-PUBLIC ###
# nb Spatial objects and simple features (sf) get different treatment.
# packages sp, sf, raster yield different objects for simlar functions.
# e.g., st_crs() and crs() give different things.
# identical(st_crs()...) maybe true while identical(crs()...) may not be.

##############################
## LOAD & ADJUST DATA
##############################

## Load EVI data in tif (19 bands - 1 per yr) as raster stack
(EVI <- stack("WA_EVI_JJAS_mean_2000-2018.tif"))
# class       : RasterStack 
# dimensions  : 1541, 3533, 5444353, 19  (nrow, ncol, ncell, nlayers)
# resolution  : 0.002245788, 0.002245788  (x, y)
# extent      : -124.8501, -116.9157, 45.54234, 49.0031  (xmin, xmax, ymin, ymax)
# coord. ref. : +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 
# names       : WA_EVI_JJ//000.2018.1, WA_EVI_JJ//000.2018.2, WA_EVI_JJ//000.2018.3, WA_EVI_JJ//000.2018.4, WA_EVI_JJ//000.2018.5, WA_EVI_JJ//000.2018.6, WA_EVI_JJ//000.2018.7, WA_EVI_JJ//000.2018.8, WA_EVI_JJ//000.2018.9, WA_EVI_JJ//00.2018.10, WA_EVI_JJ//00.2018.11, WA_EVI_JJ//00.2018.12, WA_EVI_JJ//00.2018.13, WA_EVI_JJ//00.2018.14, WA_EVI_JJ//00.2018.15, ...

# Set names, which got truncated
names <- paste0("EVI_",(2000:2018))
names(EVI) <- names
st_crs(EVI) ; crs(EVI)
evi.crs <- paste0(st_crs(EVI)[2]) # [2] to drop EPSG code
res(EVI[[2]])


## Load NLCD dataset (using classes, not canopy cover dataset b/c burned = no/low canopy)
# nlcd <- raster("D:/Shared/Archived/NLCD_2011/nlcd_2011_landcover_2011_edition_2014_10_10/nlcd_2011_landcover_2011_edition_2014_10_10.img")
nlcd <- raster("D:/Shared/Scratch/Data/NLCD_2016/NLCD_2016_Land_Cover_L48_20190424.img")
st_is_longlat(nlcd) # FALSE
st_crs(nlcd) # No EPSG code but diff than EVI or states. 
nlcd.crs <- paste0(st_crs(nlcd))[2]

# Before re-classing NLCD, crop to WA buff.
## Create WA buffer
WA <- st_read("D:/Shared/BackedUp/Caitlin/boundaries/WA.shp")
WA <- WA[,-(2:49)] # Drops exdtraneous columns.
class(WA) # sfdf
# Alt: could use bounding box so buffer isn't squiggly.
# WA.box <- st_make_grid(WA, n = (1)) # Creates new spatial object (sf); n = 1 so it's not gridded.
# WA <- as(WA, 'Spatial') # Alt: as(st_geometry()) or as_Spatial()
WA.buff <- st_buffer(WA, dist = 10000) # WA crs is in m (same as nlcd)
WA.buff <- as(WA.buff, 'Spatial') # ended up as sf again & crop can't get extent of sf object.
# Alt: could define an extent on my own with, from st_bbox():
# ext <- as(extent(-2289642, -1480131, 6442251, 7131832), 'SpatialPolygons')
extent(nlcd) ; extent(WA.buff) # Doesn't match but there IS overlap therefore crop() ok.
# Set CRS to match nlcd. N.b., not st_transform (for sf) but spTransform() (for Spatial)
WA.buff.p <- spTransform(WA.buff, crs(nlcd))

# Crop & mask
nlcd.WA <- crop(nlcd, WA.buff.p) # geographic subset
nlcd.WA <- mask(nlcd.WA, WA.buff.p) # converts NA in y to NA in x.

# Everything outside of USA (WA) is zero. Set that to NA
nlcd.WA[nlcd.WA == 0] <- NA
nlcd.WA <- trim(nlcd.WA) # removes extraneous NAs

# plot(nlcd.WA) # takes long time

# Write raster
writeRaster(nlcd.WA, "D:/Shared/BackedUp/Caitlin/NW CASC/Dir/nlcd.WA.tif")

##########################################
## CLIP & RECLASSIFY
##########################################

## Clip out Columbia Plateau
ecoreg <- st_read("D:/Shared/BackedUp/Caitlin/EcoReg/North_American_Ecoregions__Level_III.shp")
cp <- ecoreg[ecoreg$NA_L3NAME == "Columbia Plateau",]
plot(st_geometry(cp))
cp <- as(cp, 'Spatial')
cp <- spTransform(cp, crs(nlcd.WA))

# Set areas in cp to NA. # Inverse does so instead of keeping those areas.
nlcd.msk <- mask(nlcd.WA, cp, inverse = TRUE) 


## Reclass non-forest as zero; keep barren land, shrub/scrub else lose lotsa harvest, areas in city parks.
# https://www.mrlc.gov/data/legends/national-land-cover-database-2016-nlcd2016-legend
nlcd.msk[nlcd.msk < 31 | nlcd.msk > 52] <- 0
# 31 barren land
# 41 decid fores
# 42 evergreen
# 43 mixed
# 52 shrub/scrub
nlcd.msk[nlcd.msk > 0] <- 1
nlcd.for.01 <- nlcd.msk

# Write raster of no forest (0) and forest (1)
writeRaster(nlcd.msk, "D:/Shared/BackedUp/Caitlin/NW CASC/Dir/nlcd.for.01.tif")

plot(nlcd.for.01)
e <- drawExtent() ## interactive! gives extent object
plot(nlcd.for.01, ext = e)
table(getValues(nlcd.for.01))


##############################
## PREP FOR USE WITH EVI 
##############################

## Even though EVI is smalle (even with multiple layers)...
# ncell(nlcd.WA) # 305070656
# ncell(EVI[[1]]) # 5444353
# ...projecting all EVI layers to match nlcd was taking looooong time.
# So, aggregate nlcd values and then resample to EVI projection
# N.b., resample not appropriate when making much coarser. Use agg first.

res(EVI[[1]])
# 1 dd E/W at 45 N/S is 78.71 km = 78710 m
#0.002245788 * 78710 # 176, which is abt what size (in m) seems in arc
res(nlcd.for.01) # 30. So, aggregate by factor of 5 (176/30)
# Try mode and max, which is most conservative re: retaining forest. no mean b/c categorical.
# nlcd.for.agg.md <- aggregate(nlcd.for.01, fact = 5, fun=modal)
nlcd.for.agg.mx <- aggregate(nlcd.for.01, fact = 5, fun=max)
plot(nlcd.for.agg.md)
plot(nlcd.for.agg.mx)
e <- drawExtent()
plot(nlcd.for.agg.md, ext = e)
plot(nlcd.for.agg.mx, ext = e)

# Stick with max - this is just serving as filter anyhow.
# "to" raster has parameters I want to project to (incl. res, crs); nbg = nearest neighbor
nlcd.p <- projectRaster(from = nlcd.for.agg.mx, to = EVI[[1]], method = "ngb")
identical(st_crs(EVI), st_crs(nlcd.p))
identical(res(EVI), res(nlcd.p))
identical(extent(EVI), extent(nlcd.p))

############################## 
##  OVERLAY FOREST & EVI 
##############################
EVI.for <- overlay(EVI, nlcd.p,
                   fun=function(r1, r2){return(r1*r2)}) # any for=0 gets EVI 0
# Returns a brick. May be faster, but less fleixble than stack which...
# ... allows pixel-based calcs on multiple layers. Reset to stack.
# Set all EVI 0 values to NA & trim rasters down
EVI.for <- stack(EVI.for)
EVI.for[EVI.for == 0] <- NA
EVI.for <- trim(EVI.for)

names <- paste0("EVI_",(2000:2018))
names(EVI.for) <- names

plot(EVI.for[["EVI_2006"]])
methow <- extent(-120.7312, -119.5421, 48.17172, 48.91705)
# class       : Extent 
# xmin        : -120.7312 
# xmax        : -119.5421 
# ymin        : 48.17172 
# ymax        : 48.91705 
plot(EVI.for[["EVI_2005"]], ext = methow)
plot(EVI.for[["EVI_2006"]], ext = methow)
plot(EVI.for[["EVI_2007"]], ext = e)
names(EVI.for)
plot(EVI.for[[1]])

# Write raster of EVI forested areas
# writeRaster(EVI.for, "D:/Shared/BackedUp/Caitlin/NW CASC/Dir/EVI.for.tif")
