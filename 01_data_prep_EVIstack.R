### CREATE CSV OF ALL EVI DATA ###


##############################
## LOAD & ADJUST DATA
##############################

# Load EVI data in tif (19 bands - 1 per yr) as raster stack
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

# Stick with projection (used by GEE) -- WA doesn't look distorted
# Peek
plot(EVI$EVI_2018)
EVI[[2]]

####################################
## Compile per pixel EVI into CSV ##
####################################
# Potential alternatives: extract() with crop and/or getValues() for whole raster

EVI.df.all <- data.frame()  

loop.ready <- 1:19 # Number of years of data (2000-2018)
for (i in loop.ready){
  
  # Status
  print(paste0("Extracting values from ", names(EVI[[i]]),"!"))
  
  # Extract EVI for given year
  EVI.df <- as.data.frame(rasterToPoints(EVI[[i]]))
  
  # First two cols: x&y; drop yr from EVI so rbind works
  colnames(EVI.df)[3] <- "EVI_val"
  
  # Unwieldy size therefore round EVI vals
  EVI.df[3] <- round(EVI.df[3], 0)
  
  # Add year to table
  EVI.df$year <- paste0(right(names(EVI[[i]]),4))
  
  # Compile across years
  EVI.df.all <- rbind(EVI.df.all, EVI.df)
  
  # Clean up
  rm(EVI.df)
  
  }

# Confirm all years are present
head(EVI.df.all)
tail(EVI.df.all)

# Shrink a bit with reclasifying variables
EVI.df.all$EVI_val <- as.integer(EVI.df.all$EVI_val)
EVI.df.all$year <- factor(EVI.df.all$year, ordered = TRUE)
object.size(EVI.df.all)

# Write csv
# write.csv(EVI.df.all, "EVI_JJAS_mean_2000-2018.csv")

