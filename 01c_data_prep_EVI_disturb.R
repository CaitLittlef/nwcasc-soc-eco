## Evaluate what percentage decrese in EVI happens with fire -- or min post-fire.
# Then, ID trends in all forested pixels starting at that minimum value?
# ...having categorized any precipitous drop as disturbed?

EVI.for <- stack("D:/Shared/BackedUp/Caitlin/NW CASC/Dir/EVI.for.tif")

## Load in large WA fires.
# Great post re: invalid geometries: https://www.r-spatial.org/r/2017/03/19/invalid.html
fire <- st_read("D:/Shared/BackedUp/Caitlin/NW CASC/Dir/WA_Large_Fires/wa_lrg_fires.shp")
fire <- fire[,c("FIRENAME", "YEAR")]
class(fire) # sfdf
plot(st_geometry(fire))
plot(st_geometry(fire[fire$FIRENAME == "Tripod (Tripod Complex)" ,]))

# Get tripod for extracting values.
tripod <- fire[fire$FIRENAME == "Tripod (Tripod Complex)" ,]
class(tripod) # sfdf
test <- as(tripod, 'Spatial') # fail
test <- sf::as_Spatial(tripod$geom) # fail
tripod[1,] # empty geometry. Is this why fail?
tripod[2,] # not empty.
plot(tripod[1,]) # fails
plot(tripod[2,]) # plots
st_dimension(tripod) # see NA (empty) & 2 (not empty)
# Apparently every geometry type must have a null instance??

any(is.na(st_dimension(tripod))) # TRUE (but not true for FIRE)
tripod <- (tripod[2,]) 
any(is.na(st_dimension(tripod))) # FALSE. Now it should work

EVI.tri <- raster::extract(EVI.for, tripod)
# Creates dataframe of all cells in tripod.
# do.call takes function (first) then LIST of args (second).
# Here, list is output of lapply that converged...
# eleents (numeric) in EVI.tri into dfs and returned a list of those dfs
EVI.tri.df <- do.call(cbind, lapply(EVI.tri, data.frame))

#####################
## TRIPOD TRENDLINE
#####################

## Rename columns as year
colnames(EVI.tri.df) <- right(colnames(EVI.tri.df), 4)
# Need unique ID
EVI.tri.df$ID <- paste0("ID",(rownames(EVI.tri.df)))
# Gather
data <- gather(data = EVI.tri.df, key = "year", value = "evi", -ID)
# Simplify by rounding
data$evi <- round(data$evi,0)
data$year <- as.numeric(data$year)
# Remove any NAs
data <- data[! is.na(data$evi),]

## Create trendlines for each pt in tripod; put into df (do() extracts tidied model outputs)
# Group by ID so each point gets its own linear model
evi.lm <- data %>%
  group_by(ID) %>%
  do(tidy(lm(evi ~ year, data =.)))
evi.lm <- as.data.frame(evi.lm)
evi.lm[1:10,]

moo <- data[data$ID == "ID500",]
plot(moo$year, moo$evi)
evi.lm[evi.lm$ID == "ID500",] # of course not signif: see drop then rebound.

## WHAT SHOULD I DO ABOUT DEFINING BASELINE.



## Keep only slopes
doo <- def.z.lm %>%
  filter(term == "def.year") %>%
  dplyr::select(PLOTID, estimate) %>%
  rename(def59.z.slope = estimate)



# May be useful for getting all trends post-fire. Maybe if no fire, start at 2000. Else start at fire yr.
https://stackoverflow.com/questions/53241918/extract-raster-by-a-list-of-spatialpolygonsdataframe-objects-in-r

