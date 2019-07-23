###########
## SETUP ##
###########

#####################################
## Set working directory to input data
# wd <- setwd("C:/Users/clittlef/Google Drive/2NWCASC/soc_eco_clim_adapt/data") 
# out.dir <- "C:/Users/clittlef/Google Drive/2NWCASC/soc_eco_clim_adapt/output/"

wd <- setwd("D:/Shared/BackedUp/Caitlin/NW CASC/Dir") # If on goshawk
# wd <- setwd("//goshawk.sefs.uw.edu/Space_Lawler/Shared/BackedUp/Caitlin/NW CASC/Dir")

#####################################
# Install packages if not already installed
required.packages <- c("ggplot2", "raster", "sp", "sf", "rgdal", "plyr", "maptools", "rgeos", "lubridate", 
                       "corrplot", "PerformanceAnalytics", "htmlTable", "readr", "modelr", "tidyverse",
                       "dplyr", "dismo", "gbm", "usdm", "pdp", "robustbase", "stringr", "grid", "gridExtra",
                       "lattice", "ggplot2", "broom")
new.packages <- required.packages[!(required.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
rm(required.packages, new.packages)

# Libraries
library(sf)
library(rgdal)
library(gdalUtils)
library(maptools)
library(raster)
library(rgeos)
library(lubridate)
library(corrplot)
library(plyr)
library(ncdf4)
library(readr)
library(tidyverse)
library(dplyr)
library(dismo)
library(gbm)
library(usdm)
library(pdp)
library(robustbase)
library(stringr)
library(grid)
library(gridExtra)
library(lattice)
library(ggplot2)
library(broom)

# Turn off scientific notation
options(scipen=999) 

# Text extraction functions
left = function(text, num_char) {
  substr(text, 1, num_char)
}

mid = function(text, start_num, num_char) {
  substr(text, start_num, start_num + num_char - 1)
}

right = function(text, num_char) {
  substr(text, nchar(text) - (num_char-1), nchar(text))
}
