#///////////////////
# Adding weather and climate data from dayMet to the biomass data from GEDI, RAP, and FIA
# Alice Stears
# 07/14/2025
#///////////////////

# load packages -----------------------------------------------------------

library(tidyverse)
library(sf)
library(terra)

# load biomass data ------------------------------------------------------------ 

# Get total biomass data from Spawn paper ---------------------------------
spawnBiomass <- terra::rast("./Data_processed/BiomassQuantityData/Spawn2020_biomassRaster.tif") 
# reproject the points to match the crs of the biomass data 


# Read in FIA biomass data ------------------------------------------------
FIAbiomass <- terra::rast("./Data_processed/BiomassQuantityData/FIA_biomassRaster.tif")
# tree live biomass in Mg/hectare
names(FIAbiomass) <- c("TotalTreeBiomass_Mg_Hect_FIA", "FIA_avgInvYear")

# Read in RAP biomass data ------------------------------------------------
# units: Mg/hectare
RAPbiomass <- terra::rast("./Data_processed/BiomassQuantityData/RAP_biomassRaster_totalHerb_Mg_Hect_averagedfrom2019to2021.tif") 
# average from 2010 to 2020
#RAPbiomass_avg <- mean(RAPbiomass$year_2010, RAPbiomass$year_2011, RAPbiomass$year_2012, RAPbiomass$year_2013, RAPbiomass$year_2014, RAPbiomass$year_2015, RAPbiomass$year_2016, RAPbiomass$year_2017, RAPbiomass$year_2018, RAPbiomass$year_2019, RAPbiomass$year_2020)
RAPbiomass<- RAPbiomass %>%
  terra::resample(FIAbiomass)

# Get total biomass data from GEDI ----------------------------------------
# https://daac.ornl.gov/cgi-bin/dsviewer.pl?ds_id=2299
# load raw data
GEDIBiomass <- terra::rast("./Data_processed/BiomassQuantityData/GEDI_biomassRaster.tif") 
names(GEDIBiomass) <- "totalBiomass_MgHect"
# aggregate the biomass raster so that we have slightly coarser information that matches better with the cover values 
GEDIBiomass_ag <- GEDIBiomass %>% 
  terra::aggregate(factor = 10, fun = "mean", na.rm = TRUE) %>% 
  terra::resample(FIAbiomass)


# read in predicted cover data  -------------------------------------------
# scaled and relativized predictions using contemporary dayMet data (climate data ending in 2011, )
coverRast <- terra::rast("./Data_processed/CoverData/ModelPredictionData/ModelPreds_ContempClimateData_dayMetScale.tif") %>% 
  terra::resample(FIAbiomass)
  
# read in raw cover data --------------------------------------------------
rawCoverData <- readRDS("./Data_processed/CoverData/DataForModels_spatiallyAveraged_withSoils_noSf.rds")
# crs is: "PROJCRS[\"unnamed\",\n BASEGEOGCRS[\"unknown\",\n DATUM[\"unknown\",\n ELLIPSOID[\"Spheroid\",6378137,298.257223563,\n LENGTHUNIT[\"metre\",1,\nID[\"EPSG\",9001]]]],\n PRIMEM[\"Greenwich\",0,\n ANGLEUNIT[\"degree\",0.0174532925199433,\n ID[\"EPSG\",9122]]]],\n CONVERSION[\"Lambert Conic Conformal (2SP)\",\n METHOD[\"Lambert Conic Conformal (2SP)\",\n ID[\"EPSG\",9802]],\n PARAMETER[\"Latitude of false origin\",42.5,\n ANGLEUNIT[\"degree\",0.0174532925199433],\n ID[\"EPSG\",8821]],\n PARAMETER[\"Longitude of false origin\",-100,\n ANGLEUNIT[\"degree\",0.0174532925199433],\n ID[\"EPSG\",8822]],\n PARAMETER[\"Latitude of 1st standard parallel\",25,\n ANGLEUNIT[\"degree\",0.0174532925199433],\n ID[\"EPSG\",8823]],\n PARAMETER[\"Latitude of 2nd standard parallel\",60,\n ANGLEUNIT[\"degree\",0.0174532925199433],\n ID[\"EPSG\",8824]],\n PARAMETER[\"Easting at false origin\",0,\n LENGTHUNIT[\"metre\",1],\n ID[\"EPSG\",8826]],\n PARAMETER[\"Northing at false origin\",0,\n LENGTHUNIT[\"metre\",1],\n ID[\"EPSG\",8827]]],\n CS[Cartesian,2],\n AXIS[\"easting\",east,\n ORDER[1],\n LENGTHUNIT[\"metre\",1,\n ID[\"EPSG\",9001]]],\n AXIS[\"northing\",north,\n ORDER[2],\n LENGTHUNIT[\"metre\",1,\n ID[\"EPSG\",9001]]]]"
# make into a spatial object
rawCoverData <- rawCoverData %>% 
  st_as_sf(coords = c("Long", "Lat"), crs = "PROJCRS[\"unnamed\",\n BASEGEOGCRS[\"unknown\",\n DATUM[\"unknown\",\n ELLIPSOID[\"Spheroid\",6378137,298.257223563,\n LENGTHUNIT[\"metre\",1,\nID[\"EPSG\",9001]]]],\n PRIMEM[\"Greenwich\",0,\n ANGLEUNIT[\"degree\",0.0174532925199433,\n ID[\"EPSG\",9122]]]],\n CONVERSION[\"Lambert Conic Conformal (2SP)\",\n METHOD[\"Lambert Conic Conformal (2SP)\",\n ID[\"EPSG\",9802]],\n PARAMETER[\"Latitude of false origin\",42.5,\n ANGLEUNIT[\"degree\",0.0174532925199433],\n ID[\"EPSG\",8821]],\n PARAMETER[\"Longitude of false origin\",-100,\n ANGLEUNIT[\"degree\",0.0174532925199433],\n ID[\"EPSG\",8822]],\n PARAMETER[\"Latitude of 1st standard parallel\",25,\n ANGLEUNIT[\"degree\",0.0174532925199433],\n ID[\"EPSG\",8823]],\n PARAMETER[\"Latitude of 2nd standard parallel\",60,\n ANGLEUNIT[\"degree\",0.0174532925199433],\n ID[\"EPSG\",8824]],\n PARAMETER[\"Easting at false origin\",0,\n LENGTHUNIT[\"metre\",1],\n ID[\"EPSG\",8826]],\n PARAMETER[\"Northing at false origin\",0,\n LENGTHUNIT[\"metre\",1],\n ID[\"EPSG\",8827]]],\n CS[Cartesian,2],\n AXIS[\"easting\",east,\n ORDER[1],\n LENGTHUNIT[\"metre\",1,\n ID[\"EPSG\",9001]]],\n AXIS[\"northing\",north,\n ORDER[2],\n LENGTHUNIT[\"metre\",1,\n ID[\"EPSG\",9001]]]]"
)
# make into a raster (GEDI data is from 2019-2023, so I've averaged the values here from 2010-2023 which is obviously not entirely accurate... but I think is good enough)
rawCoverRast_list <- lapply(c("ShrubCover", "TotalHerbaceousCover", "TotalTreeCover", "C3GramCover_prop", "C4GramCover_prop", "ForbCover_prop", "AngioTreeCover_prop", "ConifTreeCover_prop"), 
        FUN = function(x) {
          rawCoverData_temp <- rawCoverData[rawCoverData$Year %in% c(2010:2023),]
          cov_temp <- terra::rasterize(x = vect(rawCoverData_temp[,c(x)]), y = FIAbiomass, fun = "mean", field = x)
          names(cov_temp) <- paste0(x, "_raw")
          return(cov_temp)
        })
# ###
# ggplot(rawCoverData_temp[!is.na(rawCoverData_temp$TotalTreeCover),]) + 
#   geom_spatraster(data = GEDIBiomass)+ 
#   geom_sf(aes(col = TotalTreeCover)) 
# ###

names(rawCoverRast_list) <- c("ShrubCover", "TotalHerbaceousCover", "TotalTreeCover", "C3GramCover_prop", "C4GramCover_prop", "ForbCover_prop", "AngioTreeCover_prop", "ConifTreeCover_prop")
rawCoverRast <- c(rawCoverRast_list[[1]], rawCoverRast_list[[2]], rawCoverRast_list[[3]], rawCoverRast_list[[4]], rawCoverRast_list[[5]], rawCoverRast_list[[6]], rawCoverRast_list[[7]], rawCoverRast_list[[8]])

# combine cover and biomass raster ----------------------------------------
biomassCoverRast <- c(GEDIBiomass_ag %>% terra::crop(ext(coverRast)), 
                      FIAbiomass %>% terra::crop(ext(coverRast)),
                      RAPbiomass %>% terra::crop(ext(coverRast)),
                      #coverRast, 
                      rawCoverRast %>% terra::crop(ext(coverRast)))

# load climate data -------------------------------------------------------
climDat <- readRDS("./Data_processed/BiomassQuantity_climSoilData.rds")
# rename
climDat <- climDat %>% 
  dplyr::select(year, tmin_meanAnnAvg_CLIM:Start_CLIM, NA_L1CODE, 
                NA_L1NAME, NA_L1KEY, newRegion, Long, Lat, 
                tmean_meanAnnAvg_3yrAnom:durationFrostFreeDays_meanAnnAvg_3yrAnom, soilDepth:totalAvailableWaterHoldingCapacity) %>% 
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
         "AWHC" = totalAvailableWaterHoldingCapacity
  ) 

climDat_sf <- climDat %>% 
  select(Long, Lat) %>% 
  unique() %>% 
  sf::st_as_sf(coords = c("Long", "Lat"), crs = st_crs(biomassCoverRast), remove = FALSE)


# combine cover and biomass with climate and soils data -------------------
# get the biomass and cover data points for the locations where we have climate data
biomassCoverClimSoils <- terra::extract(x = biomassCoverRast, y = climDat_sf, #xy = TRUE, 
                                        ID = TRUE#, bind = TRUE
                                        ) 
# add in climate ID data 

biomassCoverClimSoils <- biomassCoverClimSoils %>% 
  rename(climDatSpatial_ID = ID)

# add spatial ID back to the climDat data
climDat_sf$climDatSpatial_ID <- 1:nrow(climDat_sf)
climDat_sfID <- climDat_sf %>% 
  # mutate(x = st_coordinates(.)[,1], 
  #        y = st_coordinates(.)[,2]) %>% 
  st_drop_geometry()

climDat <- climDat %>% 
  left_join(climDat_sfID, by = c("Long", "Lat"))

# Make biomass data into a long dataset
biomassCoverClimSoils_long <- biomassCoverClimSoils %>% 
  pivot_longer(cols = c(totalBiomass_MgHect, TotalTreeBiomass_Mg_Hect_FIA, TotalHerbaceousBiomass_Mg_Hect_AvgFrom2019to2021), 
               names_to = "biomassSource", values_to = "biomass_MgPerHect")
biomassCoverClimSoils_long$biomassType <- NA
biomassCoverClimSoils_long[biomassCoverClimSoils_long$biomassSource == "totalBiomass_MgHect", c("biomassSource")] <- c("GEDI")
biomassCoverClimSoils_long[biomassCoverClimSoils_long$biomassSource == "GEDI", c("biomassType")] <- c("total")

biomassCoverClimSoils_long[biomassCoverClimSoils_long$biomassSource == "TotalTreeBiomass_Mg_Hect_FIA", c("biomassSource")] <- c("FIA")
biomassCoverClimSoils_long[biomassCoverClimSoils_long$biomassSource == "FIA", c("biomassType")] <- c("tree")

biomassCoverClimSoils_long[biomassCoverClimSoils_long$biomassSource == "TotalHerbaceousBiomass_Mg_Hect_AvgFrom2019to2021", c("biomassSource")] <- c("RAP")
biomassCoverClimSoils_long[biomassCoverClimSoils_long$biomassSource == "RAP", c("biomassType")] <- c("herbaceous")


# add information about the year when biomass was collected (or at least when we want the corresponding climate data to be added)
biomassCoverClimSoils_long[biomassCoverClimSoils_long$biomassSource != "FIA", "FIA_avgInvYear"] <- NA
# remove weird artifacts of NA values for years with no FIA year and no biomass data
biomassCoverClimSoils_long <- biomassCoverClimSoils_long %>% 
  drop_na(biomass_MgPerHect)

biomassCoverClimSoils_long$year <- round(biomassCoverClimSoils_long$FIA_avgInvYear)
biomassCoverClimSoils_long[biomassCoverClimSoils_long$biomassSource == "RAP", "year"] <- 2020
biomassCoverClimSoils_long[biomassCoverClimSoils_long$biomassSource == "GEDI", "year"] <- 2011

## add the climate data 
biomassCoverClimSoils_long <- biomassCoverClimSoils_long %>% 
  left_join(climDat, by = c("climDatSpatial_ID", "year")) %>% 
  drop_na(Long, Lat)

# # add level 2 ecoregion data  -----------------------------------------------------
regions <- sf::st_read(dsn = "./Data_raw/Level2Ecoregions/", layer = "NA_CEC_Eco_Level2")

# ensure projections are the same
test_rast <-  rast("./Data_raw/dayMet/rawMonthlyData/orders/70e0da02b9d2d6e8faa8c97d211f3546/Daymet_Monthly_V4R1/data/daymet_v4_prcp_monttl_na_1980.tif")

regions_2 <- st_transform(regions, crs = st_crs(test_rast)) %>%
  st_make_valid()
crs(test_rast) == crs(regions_2)

# transform modDat to sf to later use st_join
biomassCoverClimSoils_long_sf <- st_as_sf(biomassCoverClimSoils_long, coords = c("Long", "Lat"), crs = st_crs(test_rast))

crs(regions_2) == crs(biomassCoverClimSoils_long_sf)

#mapview(regions_2 %>% slice_sample(n = 1000)) + mapview(biomassCoverClimSoils_long_sf %>% slice_sample(n = 1000))

# match the ecoregion to point data ---------------------------------------
biomassCoverClimSoils_long_ecoregion <- sf::st_join(biomassCoverClimSoils_long_sf, regions_2 %>% select(-NA_L1CODE, -NA_L1NAME, -NA_L1KEY))
#badRows <- (duplicated(biomassCoverClimSoils_long_ecoregion))
#biomassCoverClimSoils_long_ecoregion <- biomassCoverClimSoils_long_ecoregion[!badRows,]

# missing for places right on the coast... but not a big deal to not have those
# ggplot(biomassCoverClimSoils_long_ecoregion) +
#   geom_sf(aes(col = NA_L2NAME))

# group into coarser ecoregions -------------------------------------------
# make a lookup table
# (eastern forest) put eastern temperate forests, tropical wet forests and northern forests together
# (dry shrub and grass) put great plains, southern semiarid highlands, Mediterranean California and north american deserts together
# (western forest) put northwestern forested mountains, marine west coast forests together, and temperate sierras together

ecoReg_lu <- data.frame("NA_L1NAME" = sort(unique(biomassCoverClimSoils_long_ecoregion$NA_L1NAME)),
                        "newRegion" = c("eastForest", "dryShrubGrass", "westForest",
                                        "dryShrubGrass", "dryShrubGrass", "eastForest",
                                        "westForest", "dryShrubGrass", "westForest",
                                        "eastForest", NA
                        ))

# add to main data.frame
newDat <- biomassCoverClimSoils_long_ecoregion %>%
  left_join(ecoReg_lu) %>%
  mutate(newRegion = as.factor(newRegion)) %>%
 st_drop_geometry() %>%
 left_join(biomassCoverClimSoils_long)

# save data ---------------------------------------------------------------
saveRDS(newDat, "./Data_processed/BiomassQuantityData/GEDIbiomass_predictedCover_climateAndSoils.rds")
