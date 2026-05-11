#///////////////////
# Fitting models to vegetation composition data
# Alice Stears
# 6/26/24
#///////////////////


# load packages -----------------------------------------------------------

library(tidyverse)
library(terra)
#library(betareg)
library(corrplot)
library(sf)
#library(StepBeta)
#library(lmtest)

# load data ---------------------------------------------------------------

# load vegetation data
vegDat <- readRDS("./Data_processed/CoverData/dataForAnalysis_fireRemoved.rds") 


# remove locations that are ag. or developed using data from LCMAP --------
# retrieve the layer w/ LCMAP data that have been reclassified into "use/1" (not ag, water, or city) or "don't use/0" "ag, water, or city) 
LCMAP_temp <- terra::rast("./Data_raw/LCMAP/LCMAP_reclassifiedToUse.tif")
# reproject to dayMet crs 
# LCMAP_dayMet <- LCMAP_temp %>% 
#   terra::project(y = rast("/Users/astears/Documents/Dropbox_static/Work/NAU_USGS_postdoc/cleanPED/PED_vegClimModels/Data_raw/dayMet/rawMonthlyData/orders/70e0da02b9d2d6e8faa8c97d211f3546/Daymet_Monthly_V4R1/data/daymet_v4_prcp_monttl_na_1980.tif"))
# # write to file to save for later use 
# terra::writeRaster(LCMAP_dayMet, filename = "./Data_processed/LCMAP/LCMAP_reclassifiedToUseBiomass_dayMetScale.tif")
# reproject to the crs of vegDat data
LCMAP_use <-  LCMAP_temp %>% 
terra::project(y = vegDat) 
LCMAP_use2 <- LCMAP_use %>% 
  round()
crs(LCMAP_use2) == crs(vegDat)
#plot(LCMAP_use2) 
## there are some RAP points that are in areas that we excluded according to LCMAP class (random points w/in dayMet cells originally excluded these areas, but because of the upscaling appraoch, we ended up w/ some values from areas that we wanted to exclude according to LCMAP use... so just to be safe, remove those values now)
vegDat$UniquID <- 1:nrow(vegDat)
vegDat_removeLCMAP <- terra::extract(x = LCMAP_use2$LCMAP_CU_2021_V13_LCPRI, y = vegDat %>% select(UniquID, geometry), #%>% slice(1:5000) , 
                                     ID = TRUE, bind = TRUE)

vegDat_newTest_withOGdata <- vegDat %>% 
  left_join(vegDat_removeLCMAP %>% as.data.frame(), by = "UniquID") %>% 
  filter(!is.na(LCMAP_CU_2021_V13_LCPRI))

# ## save data for further analysis 
saveRDS(vegDat_newTest_withOGdata, file = "./Data_processed/CoverData/DataForModels.RDS")
