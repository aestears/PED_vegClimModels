#///////////////////////////
# Get biomass data from Spawn, 2020 
# Alice Stears
# 14 July 2025
#///////////////////////////


# load packages -----------------------------------------------------------

library(tidyverse)
library(sf)
library(terra)

# Get total biomass data from Spawn paper ---------------------------------
# based on total aboveground biomass from Spawn et al., 2020
# https://daac.ornl.gov/cgi-bin/dsviewer.pl?ds_id=1763 Aboveground living
# biomass carbon stock density of combined woody and herbaceous cover in 2010.
# This includes carbon stored in living plant tissues that are located above the
# earth’s surface (stems, bark, branches, twigs). This does not include leaf
# litter or coarse woody debris that were once attached to living plants but
# have since been deposited and are no longer living. load raw data

spawnBiomass_temp <- terra::rast("./Data_raw/BiomassDataSources/aboveground_biomass_carbon_2010.tif") 
## unscale the biomass (data has a scaling factor of .1)
spawnBiomass <- spawnBiomass_temp*.1

# get the dayMet grid 
test_rast <-  rast("./Data_raw/dayMet/rawMonthlyData/orders/70e0da02b9d2d6e8faa8c97d211f3546/Daymet_Monthly_V4R1/data/daymet_v4_prcp_monttl_na_1980.tif") 

# resample the spawn biomass data to the dayMet grid 
spawnBiomass_2 <- spawnBiomass %>% 
  terra::project(test_rast) %>% 
  terra::resample(test_rast) 

# convert to biomass from Carbon
aboveground_biomass_hacked <- spawnBiomass_2 * 2
names(aboveground_biomass_hacked) <- "aboveground_biomass_hacked"

spawnBiomass_2 <- c(spawnBiomass_2, aboveground_biomass_hacked)

## save
terra::writeRaster(spawnBiomass_2, filename = "./Data_processed/BiomassQuantityData/Spawn2020_biomassRaster.tif", overwrite = TRUE)

