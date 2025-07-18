#///////////////////////////
# Get biomass data from GEDI
# Alice Stears
# 14 July 2025
#///////////////////////////


# load packages -----------------------------------------------------------

library(tidyverse)
library(sf)
library(terra)

# Get total biomass data from GEDI ---------------------------------
# https://daac.ornl.gov/cgi-bin/dsviewer.pl?ds_id=2299
# load raw data 
# biomass in Mg/ha
# based on remotely-sensed data from 2019-2023
GEDIBiomass <- terra::rast("./Data_raw/BiomassDataSources/GEDI04_B_MW019MW223_02_002_02_R01000M_MU.tif") 

# get the dayMet grid 
test_rast <-  rast("./Data_raw/dayMet/rawMonthlyData/orders/70e0da02b9d2d6e8faa8c97d211f3546/Daymet_Monthly_V4R1/data/daymet_v4_prcp_monttl_na_1980.tif") 

# resample the spawn biomass data to the dayMet grid 
GEDIBiomass_2 <- GEDIBiomass %>% 
  terra::project(test_rast, method = "bilinear") %>% 
  terra::resample(test_rast, method = "bilinear") %>% 
terra::crop(ext(-2560750, 3053250, -2091500, 1584500))

## save
terra::writeRaster(GEDIBiomass_2, filename = "./Data_processed/BiomassQuantityData/GEDI_biomassRaster.tif", overwrite = TRUE)

