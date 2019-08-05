## Shove EVI and drougth values together then extrace baseline and responses to mod/sev drought.

# Load in EVI & drought
EVI <- stack("D:/Shared/BackedUp/Caitlin/NW CASC/Dir/EVI.for.xfire.tif")
drought <- stack("D:/Shared/BackedUp/Caitlin/NW CASC/Dir/drought.tif")
crs(drought)

# Create pixel ID grid
pixel.id.grid <- raster(ext = extent(drought[[1]]), res = res(drought[[1]]), crs = crs(drought[[1]]))
ncell(pixel.id.grid) # 5364221
# ^ assign each of those cells a value
vals <- 1:ncell(pixel.id.grid)
pixel.id.grid <- setValues(pixel.id.grid, vals)
plot(pixel.id.grid)
# mask to actual pixels used
pixel.id.grid <- pixel.id.grid %>% mask(drought[[1]])
plot(pixel.id.grid)
# writeRaster(pixel.id.grid, "D:/Shared/BackedUp/Caitlin/NW CASC/Dir/pixel.id.grid.tif", overwrite=TRUE)

# Turn into table
pixel.id.df <- as.data.frame(rasterToPoints(pixel.id.grid))
pixel.id.df <- pixel.id.df %>% dplyr::rename(ID = layer)
max(pixel.id.df$ID) # 5362312 fewer than before b/c some masked out


# Get years associated with each raster stack. THIS IS DANGEROUS!!!!!
names(drought)
names(drought) <- paste0("drought_",(1999:2017))
names(EVI)
names(EVI) <- paste0("EVI_",(2000:2018))

# COULD do antecedent year influence on subsequent year productivity, but later...
# So, drop 1999 from drought and drop 2018 from EVI
EVI <- EVI[[1:18]]
names(EVI)
drought <- drought[[2:19]]
names(drought)

## Stack all layers
# First crop pixel.id.grid down
s <- stack(pixel.id.grid, EVI, drought)
plot(s[[32]])
names(s) <- c("ID", names(EVI), names(drought))
EVI.drought <- as.data.frame(rasterToPoints(s))
# Remove NAs
EVI.drought <- EVI.drought[complete.cases(EVI.drought),]
# Save JIC
# write.csv(EVI.drought, "D:/Shared/BackedUp/Caitlin/NW CASC/Dir/EVI_drought_190801.csv")
# ^ includes incomplete cases (i.e., NAs)
# write.csv(EVI.drought, "D:/Shared/BackedUp/Caitlin/NW CASC/Dir/EVI_drought_190805.csv")
# ^ does NOT include incomplete cases (i.e., NAs are excluded)

## Clean-up
rm(pixel.id.grid, s)
gc()

## Gather into cols with ID, yr, EVI, drought.
# Make smaller by rounding EVI
EVI.vals <- EVI.drought %>%
  dplyr::select(-starts_with("drought")) %>% 
  gather(key = yr, value = EVI, starts_with("EVI"))
EVI.vals$yr <- EVI.vals$yr %>% right(4) %>% as.numeric()
EVI.vals$EVI <- round(EVI.vals$EVI, 0)
drought.vals <- EVI.drought %>%
  dplyr::select(-starts_with("EVI")) %>%
  gather(key = yr, value = drought, starts_with("drought_"), -starts_with("EVI"))
drought.vals$yr <- drought.vals$yr %>% right(4) %>% as.numeric()

EVI.by.drought <- left_join(EVI.vals, drought.vals, by = c("x" = "x",
                                                           "y" = "y",
                                                           "ID" = "ID",
                                                           "yr" = "yr"))
rownames(EVI.by.drought) <- NULL
# write.csv(EVI.by.drought, "D:/Shared/BackedUp/Caitlin/NW CASC/Dir/EVI_drought_TIDY_190805.csv")


## Loop to generate drought sensitivity

sens.df <- data.frame()

for (i in 1:length(EVI.by.drought$ID)){ # Loop across pixels
# for (i in c(1:20)){ # Loop across pixels
  
  ID <- EVI.by.drought$ID[i]
  
  # P is a table of all EVI and drought obs for this pixel
  p <- EVI.by.drought[which(EVI.by.drought$ID == ID) , ]

  
  ##########################
  ##     BASELINE EVI     ##
  ##########################
  
  # Create subset of obs for this pixel under baseline conditions (drought = 0).
  # Do not maintain if there are fewer than three obs.
  
  p.base <- p %>%
    filter(drought == 0)
  if(nrow(p.base) <3) {
    base.EVI < base.EVI.trend.p <- base.EVI.trend.cor <- NA
  } else {
    base.EVI <- round(mean(p.base$EVI, na.rm = TRUE), 0)
    base.EVI.trend.p <- cor.test(p.base$yr, p.base$EVI)$p.value
    base.EVI.trend.cor <- cor.test(p.base$yr, p.base$EVI)$estimate
  }
  
  ################################
  ##     DROUGHT SENSITIVITY    ##
  ################################
  
  ### drought category 1: moderate ###
  
  # select pixels with drought category 1
  p.d1 <- p %>%
    filter(drought == 1)
  # set blank for pixels w/o d1 and/or no baseline
  if (nrow(p.d1) == 0 | is.na(base.EVI)){
    s.d1 <- NA
  } else {
    # calculate sensitivity
    EVI.d1 <- round(mean(p.d1$EVI, na.rm = TRUE), 0)
    s.d1 <- ((base.EVI - EVI.d1) / base.EVI) * 100
  }
  
    ### drought category 2: severe ###
  
  # select pixels with drought category 2
  p.d2 <- p %>%
    filter(drought == 2)
  # set blank for pixels w/o d1 and/or no baseline
  if (nrow(p.d2) == 0 | is.na(base.EVI)){
    s.d2 <- NA
  } else {
    # calculate sensitivity
    EVI.d2 <- mean(p.d2$EVI, na.rm = TRUE)
    s.d2 <- ((base.EVI - EVI.d2) / base.EVI) * 100
  }
  
  
  ### drought category 12: either ###
  
  # select pixels with drought at least 1
  p.d12 <- p %>%
    filter(drought >= 1)
  # set blank for pixels w/o d1 and/or no baseline
  if (nrow(p.d12) == 0 | is.na(base.EVI)){
    s.d12 <- NA
  } else {
    # calculate sensitivity
    EVI.d12 <- mean(p.d12$EVI, na.rm = TRUE)
    s.d12 <- ((base.EVI - EVI.d12) / base.EVI) * 100
  }
  
  
  #############################################    
  ### COMPILE ALL STATISTICS FOR THIS PIXEL ###
  #############################################  
  
  p.stats <- data.frame(ID, base.EVI, base.EVI.trend.cor, base.EVI.trend.p, s.d1, s.d2, s.d12)
  
  sens.df <-rbind(sens.df, p.stats)
  
} # end loop across pixels

hist(sens.df$s.d1)
hist(sens.df$s.d2)
hist(sens.df$s.d12)

write.csv(sens.df, "D:/Shared/BackedUp/Caitlin/NW CASC/Dir/drought_sensitivity_190805.csv")


# Clean up
# rm(p, base.EVI, p.base, p.d1, s.d1, p.d2, s.d2, p.d12, s.d12, base.EVI.trend.cor, base.EVI.trend.p)
# gc()


#################################   
## BASELINE SENSITIVITY TRENDS ##
#################################  

# Convert EVI trend into categorical var: stable (0), signif down (-1), or signif up (+1) with time.
# Based on correlation btwn yr and EVI observations under baseline conditions.

sens.df$trend[sens.df$base.EVI.trend.cor<0]<-(-1)
sens.df$trend[sens.df$base.EVI.trend.cor>0]<- 1
sens.df$sig[sens.df$base.EVI.trend.p<0.05]<- 1
sens.df$sig[sens.df$base.EVI.trend.p>=0.05]<- 0
sens.df$base.EVI.trend <- sens.df$trend * sens.df$sig

sens.df <- sens.df[ , !(names(sens.df) %in% c("trend","sig"))]

# Display top few rows of sensitivity table now that it is complete:
head(sens.df)


####################################   
## SAVE RESULTING RASTER DATASETS ##
####################################


# These are the calculated variables to map:
vars.to.map <- c("base.EVI", "base.EVI.trend", "s.d1", "s.d2", "s.d12")
currentDate <- 

for (v in vars.to.map){
  for.raster <- sens.df[ , which(names(sens.df) %in% c("x", "y", v))]
  stat.raster <- rasterFromXYZ(for.raster, res = res(drought[[1]]), crs = crs(drought[[1]]))
  plot(stat.raster, col=rainbow(50), main=v)
  outname <- paste0(gsub("\\.", "_", v), "_", currentDate, ".tif")
  #writeRaster(stat.raster, outname, overwrite=TRUE)
}

# Clean up
rm(vars.to.map, v, stat.raster, outname, for.raster)









ecoregion level III
PPT (normals)
TMAX (normals)
CMD (normals)
ELEV use upslope.area in dynatopmodel
TWI
SLOPE
TRASP/HLI use spatioeco package
BULK DENSITY
