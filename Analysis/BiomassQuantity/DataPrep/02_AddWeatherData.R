#///////////////////
# Adding weather and climate data from dayMet to the 
# Alice Stears
# 07/14/2025
#///////////////////

# load packages -----------------------------------------------------------

library(tidyverse)

# load biomass data ------------------------------------------------------------ 

# Get total biomass data from Spawn paper ---------------------------------
spawnBiomass <- terra::rast("./Data_processed/BiomassQuantityData/Spawn2020_biomassRaster.tif") 
# reproject the points to match the crs of the biomass data 

# Get total biomass data from GEDI ----------------------------------------
# https://daac.ornl.gov/cgi-bin/dsviewer.pl?ds_id=2299
# load raw data
GEDIBiomass <- terra::rast("./Data_processed/BiomassQuantityData/GEDI_biomassRaster.tif") 
names(GEDIBiomass) <- "totalBiomass_MgHect"

# Read in RAP biomass data ------------------------------------------------
# units: Mg/hectare
RAPbiomass <- terra::rast("./Data_processed/BiomassQuantityData/RAP_biomassRaster_totalHerb_Mg_Hect_averagedfrom2019to2021.tif") 
# average from 2010 to 2020
#RAPbiomass_avg <- mean(RAPbiomass$year_2010, RAPbiomass$year_2011, RAPbiomass$year_2012, RAPbiomass$year_2013, RAPbiomass$year_2014, RAPbiomass$year_2015, RAPbiomass$year_2016, RAPbiomass$year_2017, RAPbiomass$year_2018, RAPbiomass$year_2019, RAPbiomass$year_2020)
RAPbiomass_avg <- RAPbiomass

# Read in FIA biomass data ------------------------------------------------
FIAbiomass <- terra::rast("./Data_processed/BiomassQuantityData/FIA_biomassRaster.tif")
# tree live biomass in Mg/hectare

# read in predicted cover data  -------------------------------------------
# scaled and relativized predictions using contemporary dayMet data (climate data ending in 2011, )
coverRast <- terra::rast("./Data_processed/CoverData/ModelPredictionData/ModelPreds_ContempClimateData_dayMetScale.tif")

# combine cover and biomass raster ----------------------------------------
biomassCoverRast <- c(GEDIBiomass %>% terra::crop(ext(coverRast)), coverRast)

# load climate data -------------------------------------------------------
climDat <- readRDS("./Data_processed/Biomass_climSoilData.rds")
# rename
climDat <- climDat %>% 
  dplyr::select(tmin_meanAnnAvg_CLIM:durationFrostFreeDays_meanAnnAvg_3yrAnom, NA_L1CODE, 
                NA_L1NAME, NA_L1KEY, newRegion, x, y, soilDepth:totalAvailableWaterHoldingCapacity) %>% 
  rename("tmin" = tmin_meanAnnAvg_CLIM, 
         "tmax" = tmax_meanAnnAvg_CLIM, #1 
         "tmean" = tmean_meanAnnAvg_CLIM, 
         "prcp" = prcp_meanAnnTotal_CLIM, 
         "t_warm" = T_warmestMonth_meanAnnAvg_CLIM,
         "t_cold" = T_coldestMonth_meanAnnAvg_CLIM, 
         "prcp_wet" = precip_wettestMonth_meanAnnAvg_CLIM,
         "prcp_dry" = precip_driestMonth_meanAnnAvg_CLIM, 
         "prcp_seasonality" = precip_Seasonality_meanAnnAvg_CLIM, #2
         "prcpTempCorr" = PrecipTempCorr_meanAnnAvg_CLIM,  #3
         "abvFreezingMonth" = aboveFreezing_month_meanAnnAvg_CLIM, 
         "isothermality" = isothermality_meanAnnAvg_CLIM, #4
         "annWatDef" = annWaterDeficit_meanAnnAvg_CLIM, 
         "annWetDegDays" = annWetDegDays_meanAnnAvg_CLIM,
         "VPD_mean" = annVPD_mean_meanAnnAvg_CLIM, 
         "VPD_max" = annVPD_max_meanAnnAvg_CLIM, #5
         "VPD_min" = annVPD_min_meanAnnAvg_CLIM, #6
         "VPD_max_95" = annVPD_max_95percentile_CLIM, 
         "annWatDef_95" = annWaterDeficit_95percentile_CLIM, 
         "annWetDegDays_5" = annWetDegDays_5percentile_CLIM, 
         "frostFreeDays_5" = durationFrostFreeDays_5percentile_CLIM, 
         "frostFreeDays" = durationFrostFreeDays_meanAnnAvg_CLIM, 
         "soilDepth" = soilDepth, #7
         "clay" = surfaceClay_perc, 
         "sand" = avgSandPerc_acrossDepth, #8
         "coarse" = avgCoarsePerc_acrossDepth, #9
         "carbon" = avgOrganicCarbonPerc_0_3cm, #10
         "AWHC" = totalAvailableWaterHoldingCapacity,
         ## anomaly variables
         # tmean_anom = tmean_meanAnnAvg_3yrAnom, #15
         # tmin_anom = tmin_meanAnnAvg_3yrAnom, #16
         # tmax_anom = tmax_meanAnnAvg_3yrAnom, #17
         # prcp_anom = prcp_meanAnnTotal_3yrAnom, #18
         # t_warm_anom = T_warmestMonth_meanAnnAvg_3yrAnom,  #19
         # t_cold_anom = T_coldestMonth_meanAnnAvg_3yrAnom, #20
         # prcp_wet_anom = precip_wettestMonth_meanAnnAvg_3yrAnom, #21
         # precp_dry_anom = precip_driestMonth_meanAnnAvg_3yrAnom,  #22
         # prcp_seasonality_anom = precip_Seasonality_meanAnnAvg_3yrAnom, #23 
         # prcpTempCorr_anom = PrecipTempCorr_meanAnnAvg_3yrAnom, #24
         # aboveFreezingMonth_anom = aboveFreezing_month_meanAnnAvg_3yrAnom, #25  
         # isothermality_anom = isothermality_meanAnnAvg_3yrAnom, #26
         # annWatDef_anom = annWaterDeficit_meanAnnAvg_3yrAnom, #27
         # annWetDegDays_anom = annWetDegDays_meanAnnAvg_3yrAnom,  #28
         # VPD_mean_anom = annVPD_mean_meanAnnAvg_3yrAnom, #29
         # VPD_min_anom = annVPD_min_meanAnnAvg_3yrAnom,  #30
         # VPD_max_anom = annVPD_max_meanAnnAvg_3yrAnom,  #31
         # VPD_max_95_anom = annVPD_max_95percentile_3yrAnom, #32
         # annWatDef_95_anom = annWaterDeficit_95percentile_3yrAnom, #33 
         # annWetDegDays_5_anom = annWetDegDays_5percentile_3yrAnom ,  #34
         # frostFreeDays_5_anom = durationFrostFreeDays_5percentile_3yrAnom, #35 
         # frostFreeDays_anom = durationFrostFreeDays_meanAnnAvg_3yrAnom #36
  ) %>% 
  dplyr::select(-c(tmin_meanAnnAvg_3yr:NA_L1CODE)) 

climDat_sf <- climDat %>% 
  sf::st_as_sf(coords = c("x", "y"), crs = st_crs(biomassCoverRast))
# combine cover and biomass with climate and soils data -------------------

biomassCoverClimSoils <- terra::extract(x = biomassCoverRast, y = climDat_sf, xy = TRUE) %>% 
  cbind(climDat)


# save data ---------------------------------------------------------------
saveRDS(biomassCoverClimSoils, "./Data_processed/BiomassQuantityData/GEDIbiomass_predictedCover_climateAndSoils.rds")
