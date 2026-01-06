#///////////////////
# Comparing variability in input data sources
# Alice Stears
# 11/25/25
#///////////////////

# install packages --------------------------------------------------------
library(tidyverse)
library(rSOILWAT2)
library(terra)
library(sf)
library(ggpubr)
theme_set(theme_classic())

# import functions --------------------------------------------------------
source("././Functions/glmTransformsIterates.R")
source("././Functions/transformPreds.R")


# read in data ------------------------------------------------------------

dat <- #readRDS("./Data_processed/CoverData/DataForModels.RDS")  (## veg data before spatial averaging) 

  readRDS("./Data_processed/CoverData/data_beforeSpatialAveraging_sampledLANDFIRE.rds") ## veg data before spatial averaging w/ LANDFIRE filtered
#readRDS("./Data_processed/CoverData/data_beforeSpatialAveraging_sampledRAP.rds") ## veg data before spatial averaging w/ RAP filtered
#readRDS("./Data_processed/CoverData/data_beforeSpatialAveraging_sampledRAP.rds") ## veg data before spatial averaging w/ RAP and LANDFIRE filtered

# data used for model fitting  (after spatial averaging) #with model predictions included 
preds_quantile_raw <- #readRDS("./Data_processed/CoverData/IntermediateAnalysisFiles/ModelPredictionDataWithObservations.rds")
  readRDS("./Data_processed/CoverData/DataForModels_spatiallyAveraged_withSoils_noSf_sampledLANDFIRE.rds")

# climate data
climDatNew <- readRDS("./Data_processed/CoverData/dayMetClimateValuesForAnalysis_final.rds")

# climDatNew %>% 
#   select(year, tmin_annAvg, tmax_annAvg, T_warmestMonth, T_coldestMonth, tmean) %>%  
#   ggplot() +
#   geom_point(aes(x = year, y = tmean)) + 
#   geom_smooth(aes(x = year, y = tmean), method = "lm")
# Add climate data to the raw cover data ----------------------------------

# Now, add the climate data to the pre-spatially averaged cover observations -----------------------------------------------
## assign a 'unique ID' to each location (So the same location has the same ID across years)
climDatNew$locID <- paste0(climDatNew$Lat, "_", climDatNew$Long)
climDatNew$uniqueID <- c(1:nrow(climDatNew))
# get sf point data


# make veg data spatial (and transform the CRS to match the climate data)
# dayMet extent 
test <-  rast("./Data_raw/dayMet/rawMonthlyData/orders/70e0da02b9d2d6e8faa8c97d211f3546/Daymet_Monthly_V4R1/data/daymet_v4_prcp_monttl_na_1980.tif") #%>% 
  #terra::project(crs(dat))
# make points for "locID" in a single year into a raster
climSF <- climDatNew %>% 
  dplyr::filter(year == as.integer(2011)) %>% 
  sf::st_as_sf(coords = c("Long", "Lat"), crs =  crs(test)) %>%
  select(locID)

# make veg data projection the same as raster data
v <- vect(st_drop_geometry(dat)[,c("Lon", "Lat")], geom = c("Lon", "Lat"), crs=crs(dat))
y <- project(v, crs(test))
# make sure the veg data is in the appropriate projection
dat2 <- dat %>%
  #st_buffer(.01) %>%
  #terra::vect() #%>%
  sf::st_transform(crs(y)) %>% 
  # st_buffer(.1)
  st_centroid() %>% 
  mutate("Year_char" = as.character(Year)) %>% 
  select(Year, Year_char, UniqueID, ShrubCover:geometry)

st_crs(dat2) == st_crs(climSF)
# 
# plot(climSF$geometry)
# points(dat2$geometry, col = "red")

#Add in the 'locID' column from the climSF data.frame
test7 <- dat2 %>%
  #   #slice_sample(n = 1000) %>% 
  #   rename(Lon = x, Lat = y) %>% 
  #   sf::st_as_sf(coords = c("Lon", "Lat"), crs = st_crs(dat2)) %>%
  #   st_transform(crs(test)) %>% 
  mutate(x = st_coordinates(.)[,1],
         y = st_coordinates(.)[,2]) %>% 
  #st_buffer(400) %>% 
  sf::st_join(climSF, join = st_nearest_feature)

# add back in all climate data based on the "locID"
allDat_avg <- test7 %>% 
  left_join(climDatNew, by = c("locID", "Year" = "year"))

# add soils data to the unaveraged veg. data
soilRast <- readRDS("./Data_processed/SoilsRaster.rds")

# sample soils data for veg. points
# sample raster to get values for the points in the cover dataset
vegSoils_df <- soilRast %>% 
  terra::extract(y = allDat_avg #%>% dplyr::select(-x,-y)
                 , xy = TRUE, bind = TRUE) %>% 
  as.data.frame()

# calculate soils variables w/ cover data 
names(vegSoils_df)[c(length(names(vegSoils_df))-1, length(names(vegSoils_df)))] <- c("x_UTM", "y_UTM")
vegSoils_new <- 
  vegSoils_df %>% 
  dplyr::mutate(
    # Soil depth 
    soilDepth = pmap_dbl(.[c("horizonThickness_cm_2cm" , "horizonThickness_cm_7cm" , "horizonThickness_cm_15cm" , 
                             "horizonThickness_cm_25cm" , "horizonThickness_cm_35cm" , "horizonThickness_cm_50cm" , 
                             "horizonThickness_cm_70cm" , "horizonThickness_cm_90cm" , "horizonThickness_cm_125cm" , 
                             "horizonThickness_cm_176cm")], sum, na.rm = TRUE),
    #Surface clay (influences how much moisture can get into the profile)
    surfaceClay_perc = clayPerc_2cm) %>% 
  mutate(soilDepth = replace(soilDepth, is.na(horizonThickness_cm_2cm), values = NA)) %>% 
  mutate( 
    # Sand average across depths (avg. weighted by width of layer)
    avgSandPerc_acrossDepth = pmap_dbl(.[c("horizonThickness_cm_2cm" , "horizonThickness_cm_7cm" , "horizonThickness_cm_15cm" , 
                                           "horizonThickness_cm_25cm" , "horizonThickness_cm_35cm" , "horizonThickness_cm_50cm" , 
                                           "horizonThickness_cm_70cm" , "horizonThickness_cm_90cm" , "horizonThickness_cm_125cm" , 
                                           "horizonThickness_cm_176cm", "sandPerc_2cm", "sandPerc_7cm" , "sandPerc_15cm",
                                           "sandPerc_25cm" , "sandPerc_35cm", "sandPerc_50cm" , "sandPerc_70cm", "sandPerc_90cm" ,
                                           "sandPerc_125cm", "sandPerc_176cm", "soilDepth")], 
                                       function(horizonThickness_cm_2cm , horizonThickness_cm_7cm , horizonThickness_cm_15cm , 
                                                horizonThickness_cm_25cm , horizonThickness_cm_35cm , horizonThickness_cm_50cm , 
                                                horizonThickness_cm_70cm , horizonThickness_cm_90cm , horizonThickness_cm_125cm , 
                                                horizonThickness_cm_176cm, sandPerc_2cm, sandPerc_7cm , sandPerc_15cm,
                                                sandPerc_25cm , sandPerc_35cm, sandPerc_50cm , sandPerc_70cm, sandPerc_90cm ,
                                                sandPerc_125cm,sandPerc_176cm, soilDepth) {
                                         y <- sum(c(sandPerc_2cm *  horizonThickness_cm_2cm/soilDepth, 
                                                    sandPerc_7cm *    horizonThickness_cm_7cm/soilDepth, 
                                                    sandPerc_15cm *   horizonThickness_cm_15cm/soilDepth, 
                                                    sandPerc_25cm *   horizonThickness_cm_25cm/soilDepth, 
                                                    sandPerc_35cm *   horizonThickness_cm_35cm/soilDepth, 
                                                    sandPerc_50cm *   horizonThickness_cm_50cm/soilDepth, 
                                                    sandPerc_70cm *   horizonThickness_cm_70cm/soilDepth, 
                                                    sandPerc_90cm *   horizonThickness_cm_90cm/soilDepth, 
                                                    sandPerc_125cm *  horizonThickness_cm_125cm/soilDepth, 
                                                    sandPerc_176cm *  horizonThickness_cm_176cm/soilDepth), 
                                                  na.rm = TRUE)/1 
                                         # following weighted average formula here: weighted average = sum(x * weight)/sum(weights)
                                         return(y)
                                       }
    ),
    # Coarse fragments average across depths (avg. weighted by width of layer)
    avgCoarsePerc_acrossDepth = pmap_dbl(.[c("horizonThickness_cm_2cm" , "horizonThickness_cm_7cm" , "horizonThickness_cm_15cm" , 
                                             "horizonThickness_cm_25cm" , "horizonThickness_cm_35cm" , "horizonThickness_cm_50cm" , 
                                             "horizonThickness_cm_70cm" , "horizonThickness_cm_90cm" , "horizonThickness_cm_125cm" , 
                                             "horizonThickness_cm_176cm", "coarsePerc_2cm", "coarsePerc_7cm" , "coarsePerc_15cm",
                                             "coarsePerc_25cm" , "coarsePerc_35cm", "coarsePerc_50cm" , "coarsePerc_70cm", "coarsePerc_90cm" ,
                                             "coarsePerc_125cm","coarsePerc_176cm", "soilDepth")], 
                                         function(horizonThickness_cm_2cm , horizonThickness_cm_7cm , horizonThickness_cm_15cm , 
                                                  horizonThickness_cm_25cm , horizonThickness_cm_35cm , horizonThickness_cm_50cm , 
                                                  horizonThickness_cm_70cm , horizonThickness_cm_90cm , horizonThickness_cm_125cm , 
                                                  horizonThickness_cm_176cm, coarsePerc_2cm, coarsePerc_7cm , coarsePerc_15cm,
                                                  coarsePerc_25cm , coarsePerc_35cm, coarsePerc_50cm , coarsePerc_70cm, coarsePerc_90cm ,
                                                  coarsePerc_125cm,coarsePerc_176cm, soilDepth) {
                                           y <- sum(c(coarsePerc_2cm *  horizonThickness_cm_2cm/soilDepth, 
                                                      coarsePerc_7cm *    horizonThickness_cm_7cm/soilDepth, 
                                                      coarsePerc_15cm *   horizonThickness_cm_15cm/soilDepth, 
                                                      coarsePerc_25cm *   horizonThickness_cm_25cm/soilDepth, 
                                                      coarsePerc_35cm *   horizonThickness_cm_35cm/soilDepth, 
                                                      coarsePerc_50cm *   horizonThickness_cm_50cm/soilDepth, 
                                                      coarsePerc_70cm *   horizonThickness_cm_70cm/soilDepth, 
                                                      coarsePerc_90cm *   horizonThickness_cm_90cm/soilDepth, 
                                                      coarsePerc_125cm *  horizonThickness_cm_125cm/soilDepth, 
                                                      coarsePerc_176cm *  horizonThickness_cm_176cm/soilDepth), 
                                                    na.rm = TRUE)/1 
                                           # following weighted average formula here: weighted average = sum(x * weight)/sum(weights)
                                           return(y)
                                         }
    ), 
    # soil organic carbon average across depths (avg. weighted by width of layer)
    avgOrganicCarbonPerc_0_3cm = organicCarbonPerc_2cm
  )

# # total profile available water-holding capacity
temp <- vegSoils_new %>% 
  mutate(clayPerc_2cm = clayPerc_2cm/100,
         clayPerc_7cm = clayPerc_7cm/100,
         clayPerc_15cm = clayPerc_15cm/100,
         clayPerc_25cm = clayPerc_25cm/100,
         clayPerc_35cm = clayPerc_35cm/100,
         clayPerc_50cm = clayPerc_50cm/100,
         clayPerc_70cm = clayPerc_70cm/100,
         clayPerc_90cm = clayPerc_90cm/100,
         clayPerc_125cm = clayPerc_125cm/100,
         clayPerc_176cm = clayPerc_176cm/100,
         sandPerc_2cm = sandPerc_2cm/100,
         sandPerc_7cm = sandPerc_7cm/100,
         sandPerc_15cm = sandPerc_15cm/100,
         sandPerc_25cm = sandPerc_25cm/100,
         sandPerc_35cm = sandPerc_35cm/100,
         sandPerc_50cm = sandPerc_50cm/100,
         sandPerc_70cm = sandPerc_70cm/100,
         sandPerc_90cm = sandPerc_90cm/100,
         sandPerc_125cm = sandPerc_125cm/100,
         sandPerc_176cm = sandPerc_176cm/100,
         coarsePerc_2cm = coarsePerc_2cm/100,
         coarsePerc_7cm = coarsePerc_7cm/100,
         coarsePerc_15cm = coarsePerc_15cm/100,
         coarsePerc_25cm = coarsePerc_25cm/100,
         coarsePerc_35cm = coarsePerc_35cm/100,
         coarsePerc_50cm = coarsePerc_50cm/100,
         coarsePerc_70cm = coarsePerc_70cm/100,
         coarsePerc_90cm = coarsePerc_90cm/100,
         coarsePerc_125cm = coarsePerc_125cm/100,
         coarsePerc_176cm = coarsePerc_176cm/100) #%>% 
#slice(1:3) 
# calculate # # intermediate value 'p' 
vegSoil_p <- pmap(.l = temp[,c("sandPerc_2cm", "sandPerc_7cm", "sandPerc_15cm", 
                               "sandPerc_25cm", "sandPerc_35cm", "sandPerc_50cm", 
                               "sandPerc_70cm", "sandPerc_90cm" ,"sandPerc_125cm", 
                               "sandPerc_176cm",
                               "clayPerc_2cm", "clayPerc_7cm" , "clayPerc_15cm", 
                               "clayPerc_25cm", "clayPerc_35cm", "clayPerc_50cm", 
                               "clayPerc_70cm", "clayPerc_90cm" ,"clayPerc_125cm", 
                               "clayPerc_176cm",
                               "coarsePerc_2cm", "coarsePerc_7cm" , "coarsePerc_15cm", 
                               "coarsePerc_25cm", "coarsePerc_35cm", "coarsePerc_50cm", 
                               "coarsePerc_70cm", "coarsePerc_90cm" ,"coarsePerc_125cm", 
                               "coarsePerc_176cm")], 
                  function (sandPerc_2cm, sandPerc_7cm, sandPerc_15cm, 
                            sandPerc_25cm, sandPerc_35cm, sandPerc_50cm, 
                            sandPerc_70cm, sandPerc_90cm ,sandPerc_125cm, 
                            sandPerc_176cm,
                            clayPerc_2cm, clayPerc_7cm , clayPerc_15cm, 
                            clayPerc_25cm, clayPerc_35cm, clayPerc_50cm, 
                            clayPerc_70cm, clayPerc_90cm ,clayPerc_125cm, 
                            clayPerc_176cm,
                            coarsePerc_2cm, coarsePerc_7cm , coarsePerc_15cm, 
                            coarsePerc_25cm, coarsePerc_35cm, coarsePerc_50cm, 
                            coarsePerc_70cm, coarsePerc_90cm ,coarsePerc_125cm, 
                            coarsePerc_176cm) {
                    p <- rSOILWAT2::ptf_estimate(
                      sand = c(sandPerc_2cm,sandPerc_7cm , sandPerc_15cm,
                               sandPerc_25cm , sandPerc_35cm, sandPerc_50cm , sandPerc_70cm, sandPerc_90cm ,
                               sandPerc_125cm,sandPerc_176cm),
                      clay = c(clayPerc_2cm,clayPerc_7cm , clayPerc_15cm,
                               clayPerc_25cm , clayPerc_35cm, clayPerc_50cm , clayPerc_70cm, clayPerc_90cm ,
                               clayPerc_125cm,clayPerc_176cm),
                      fcoarse = c(coarsePerc_2cm, coarsePerc_7cm , coarsePerc_15cm,
                                  coarsePerc_25cm , coarsePerc_35cm, coarsePerc_50cm , coarsePerc_70cm, coarsePerc_90cm ,
                                  coarsePerc_125cm,coarsePerc_176cm),
                      swrc_name = "Campbell1974",
                      ptf_name = "Cosby1984"
                    )
                  }
)

# calculate intermediate value 'tmp'
# reference "temp" data frame (which has the raw soil variables), as well as vegSoil_p, a list which has matrices for p calculated above
vegSoil_tmp <- purrr::map(.x = c(1:nrow(temp)), 
                   function (n) {
                     tmp <- rSOILWAT2::swrc_swp_to_vwc(
                       c(-1.5, -0.033), ##AES should I change this? not totally clear what these values indicate 
                       fcoarse = unlist(as.vector(temp[n,c("coarsePerc_2cm" ,                           
                                                           "coarsePerc_7cm" ,  "coarsePerc_15cm",                        
                                                           "coarsePerc_25cm",  "coarsePerc_35cm",                        
                                                           "coarsePerc_50cm",  "coarsePerc_70cm",                        
                                                           "coarsePerc_90cm",  "coarsePerc_125cm",                        
                                                           "coarsePerc_176cm")])),
                       swrc = list(name = "Campbell1974", swrcp = vegSoil_p[[n]])
                     )
                   }
)


#   # calculate final value 'awc' 
vegSoil_awc <- purrr::map(.x = c(1:nrow(temp)), 
                   function (n) {
                     awc <- temp[n,c("horizonThickness_cm_2cm"  ,                 
                                     "horizonThickness_cm_7cm"  ,                  "horizonThickness_cm_15cm"    ,              
                                     "horizonThickness_cm_25cm" ,                  "horizonThickness_cm_35cm"    ,              
                                     "horizonThickness_cm_50cm" ,                  "horizonThickness_cm_70cm"    ,              
                                     "horizonThickness_cm_90cm" ,                  "horizonThickness_cm_125cm"   ,              
                                     "horizonThickness_cm_176cm")] * as.vector(diff(vegSoil_tmp[[n]])
                                     )
                     #AES I assume that I sum these values across the entire profile to get "total profile awc"??
                     totAWC <- sum(awc, na.rm = TRUE)
                   }
)

vegSoils_new$totalAvailableWaterHoldingCapacity <- unlist(vegSoil_awc)

# remove unnecessary soils variables 
vegSoils_final <- vegSoils_new %>% 
  select(-c(clayPerc_2cm:organicCarbonPerc_176cm))

## drop data points for which there are no climate variables (prior to 2000)
vegSoils_final <- vegSoils_final %>% 
  filter(!is.na(durationFrostFreeDays_meanAnnAvg_3yrAnom))

# prepare final dataset
# give each row a unique ID
vegSoils_finalTemp <- 
  vegSoils_final %>% 
  mutate(uniqueID = seq(1:nrow(.)))

## scale the climate/weather/soils data for use in models 
# rename the climate variables
vegSoils_final <- vegSoils_finalTemp %>% 
  dplyr::select(Year, Lat.y, Long, ShrubCover,#ShrbCvr, 
                ForbCover_prop,#ForbCvr, 
                C3GramCover_prop, #C3GrmCv,
                C4GramCover_prop,#C4GrmCv, 
                AngioTreeCover_prop,#AngTrCv, 
                ConifTreeCover_prop, #CnfTrCv, 
                TotalTreeCover, #TtlTrCv, 
                TotalHerbaceousCover, #TtlHrbC, 
                BareGroundCover, #BrGrndC, 
                Source, 
                #C3GrmC_, C4GrmC_, FrbCvr_, AngTrC_, CnfTrC_,
    tmin_meanAnnAvg_CLIM:durationFrostFreeDays_meanAnnAvg_3yrAnom, x, y, soilDepth:totalAvailableWaterHoldingCapacity) %>% 
  mutate(         "ForbCover_abs_obs" = ForbCover_prop * TotalHerbaceousCover, 
                  "C3GrassCover_abs_obs" = C3GramCover_prop * TotalHerbaceousCover, 
                  "C4GrassCover_abs_obs" = C4GramCover_prop * TotalHerbaceousCover, 
                  "BLTree_abs_obs" = AngioTreeCover_prop * TotalTreeCover, 
                  "NLTree_abs_obs" = ConifTreeCover_prop * TotalTreeCover) %>% 
  rename("ShrubCover_obs" = ShrubCover,
         "TotalTreeCover_obs" = TotalTreeCover, 
         "TotalHerbaceousCover_obs" = TotalHerbaceousCover,
         "BareGroundCover_obs" = BareGroundCover,
         "C3GrassCover_prop_obs" =  C3GramCover_prop, 
         "C4GrassCover_prop_obs" =  C4GramCover_prop, 
         "ForbCover_prop_obs" = ForbCover_prop, 
         "BLTree_prop_obs" = AngioTreeCover_prop, 
         "NLTree_prop_obs" = ConifTreeCover_prop,
        "tmin" = tmin_meanAnnAvg_CLIM, 
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
         tmean_anom = tmean_meanAnnAvg_3yrAnom, #15
         tmin_anom = tmin_meanAnnAvg_3yrAnom, #16
         tmax_anom = tmax_meanAnnAvg_3yrAnom, #17
         prcp_anom = prcp_meanAnnTotal_3yrAnom, #18
         t_warm_anom = T_warmestMonth_meanAnnAvg_3yrAnom,  #19
         t_cold_anom = T_coldestMonth_meanAnnAvg_3yrAnom, #20
         prcp_wet_anom = precip_wettestMonth_meanAnnAvg_3yrAnom, #21
         precp_dry_anom = precip_driestMonth_meanAnnAvg_3yrAnom,  #22
         prcp_seasonality_anom = precip_Seasonality_meanAnnAvg_3yrAnom, #23 
         prcpTempCorr_anom = PrecipTempCorr_meanAnnAvg_3yrAnom, #24
         aboveFreezingMonth_anom = aboveFreezing_month_meanAnnAvg_3yrAnom, #25  
         isothermality_anom = isothermality_meanAnnAvg_3yrAnom, #26
         annWatDef_anom = annWaterDeficit_meanAnnAvg_3yrAnom, #27
         annWetDegDays_anom = annWetDegDays_meanAnnAvg_3yrAnom,  #28
         VPD_mean_anom = annVPD_mean_meanAnnAvg_3yrAnom, #29
         VPD_min_anom = annVPD_min_meanAnnAvg_3yrAnom,  #30
         VPD_max_anom = annVPD_max_meanAnnAvg_3yrAnom,  #31
         VPD_max_95_anom = annVPD_max_95percentile_3yrAnom, #32
         annWatDef_95_anom = annWaterDeficit_95percentile_3yrAnom, #33 
         annWetDegDays_5_anom = annWetDegDays_5percentile_3yrAnom ,  #34
         frostFreeDays_5_anom = durationFrostFreeDays_5percentile_3yrAnom, #35 
         frostFreeDays_anom = durationFrostFreeDays_meanAnnAvg_3yrAnom #36
  )

rm(vegSoils_finalTemp) 
gc()


## add in ecoregion information
# read in ecoregion data
regions <- sf::st_read(dsn = "./Data_raw/Level2Ecoregions/", layer = "NA_CEC_Eco_Level2") 
# reproject
regions_2 <- st_transform(regions, crs = st_crs(test7)) %>% 
  st_make_valid()
crs(test7) == crs(regions_2)

# transform modDat to sf to later use st_join
#modDat_2 <- st_as_sf(modDat, coords = c("Lon", "Lat"))
vegSoils_final_2 <- st_as_sf(vegSoils_final, coords = c("x","y"), crs = st_crs(test7))

# check that crs matches
crs(regions_2) == crs(vegSoils_final_2)

# match the ecoregion to point data 
modDat_3 <- sf::st_join(vegSoils_final_2, regions_2)
badRows <- (duplicated(modDat_3[,1:101]))
vegSoils_final <- modDat_3[!badRows,] %>% 
  st_drop_geometry()

# missing for places right on the coast... but not a big deal to not have those
# ggplot(modDat_3) + 
#   geom_point(aes(x, y, col = NA_L2NAME))

# group into coarser ecoregions -------------------------------------------
# make a lookup table
# (eastern forest) put eastern temperate forests, tropical wet forests and northern forests together
# (dry shrub and grass) put great plains, southern semiarid highlands, Mediterranean California and north american deserts together 
# (western forest) put northwestern forested mountains, marine west coast forests together, and temperate sierras together

ecoReg_lu <- data.frame("NA_L1NAME" = sort(unique(vegSoils_final$NA_L1NAME)), 
                        "newRegion" = c("eastForest", "dryShrubGrass", "westForest",
                                        "dryShrubGrass", "dryShrubGrass", "eastForest",
                                        "westForest", "dryShrubGrass", "westForest", 
                                        "eastForest", NA
                        ))

# add to main data.frame 
vegSoils_final <- vegSoils_final %>% 
  left_join(ecoReg_lu) %>% 
  mutate(newRegion = as.factor(newRegion)) 

## now, scale the climate and weather and soils data for use in models 
# get the scaling factors 
modDat_1_s <- readRDS("./Analysis/VegComposition/ModelFitting/models/scaledModelInputData.rds")
scaleParams <- modDat_1_s %>% 
  #filter(Year == 2016) %>% 
  dplyr::select(tmin_s:AWHC_s) %>% 
  reframe(across(all_of(names(.)), attributes)) 

# apply the scaling factors to the contemporary climate data 
namesToScale <- vegSoils_final %>% 
  dplyr::select(tmin:frostFreeDays, tmean_anom:frostFreeDays_anom, soilDepth:AWHC) %>% 
  names()

climDat_scaled <- purrr::map(namesToScale, .f = function(x) {
  x_new <- (vegSoils_final[,x] - scaleParams[,paste0(x, "_s")]$`scaled:center`)/scaleParams[,paste0(x, "_s")]$`scaled:scale`
  return(data.frame(x_new))
}) %>% 
  purrr::list_cbind()
names(climDat_scaled) <- paste0(namesToScale, "_s")

vegSoils_scaled <- vegSoils_final %>% 
  dplyr::select(Year:Source, ForbCover_abs_obs:NLTree_abs_obs) %>% 
  cbind(climDat_scaled)
names(vegSoils_scaled)[19:68] <- str_remove(names(vegSoils_scaled)[19:68], pattern = "_s$")

# Now, use un-averaged data to predict using models ------------------------
# predict ecoregion classification (using the unscaled climate and soils data from 'vegSoils_final')
modDat_ecoregionFitQuantile <- vegSoils_final %>% 
  #rename("Long" = x, "Lat" = y) %>% 
  dplyr::select(c(newRegion, t_warm, t_cold, prcp_wet, annWatDef, prcpTempCorr, isothermality, soilDepth, sand, coarse, carbon,
                  Long, Lat.y)) %>% 
  mutate(newRegion = as.factor(newRegion)) 

# get climate data from dayMet (d.f. is "climDatPred_unscaled")
modDat_ecoregionFitQuantile_unscaled <- modDat_ecoregionFitQuantile
names(modDat_ecoregionFitQuantile_unscaled)[c(2:11)] <- c("T_warmestMonth_meanAnnAvg_CLIM", "T_coldestMonth_meanAnnAvg_CLIM", 
                                                          "precip_wettestMonth_meanAnnAvg_CLIM", "annWaterDeficit_meanAnnAvg_CLIM", 
                                                          "PrecipTempCorr_meanAnnAvg_CLIM", "isothermality_meanAnnAvg_CLIM", 
                                                          "soilDepth", "avgSandPerc_acrossDepth", "avgCoarsePerc_acrossDepth", "avgOrganicCarbonPerc_0_3cm")

# predict with contemporary climate data 
preds_byHand_quantile <- modDat_ecoregionFitQuantile_unscaled %>% 
  mutate(pred = 1/(1 + exp(-( 9.8726 + -0.2999*T_warmestMonth_meanAnnAvg_CLIM +  0.2456*T_coldestMonth_meanAnnAvg_CLIM +  0.0106*precip_wettestMonth_meanAnnAvg_CLIM + -0.0621*annWaterDeficit_meanAnnAvg_CLIM + -2.7863*PrecipTempCorr_meanAnnAvg_CLIM +  0.0540*isothermality_meanAnnAvg_CLIM + -0.0076*soilDepth +  0.0335*avgSandPerc_acrossDepth +  0.0310*avgCoarsePerc_acrossDepth +  0.2726*avgOrganicCarbonPerc_0_3cm))))

# ggplot(preds_byHand_quantile) + 
#   geom_point(aes(x = Lon, y = Lat.x, col = pred))

## now, predict for each functional type 
# get model objects
totalHerb_GS <-  readRDS("./Analysis/VegComposition/ModelFitting/models/betaLASSO/TotalHerbaceousCover_shrubGrass_noTLP_FALSE_removeAnomaliesFALSE_trimAnom_oneSELambdaGLM.rds")
totalHerb_F <-  readRDS("./Analysis/VegComposition/ModelFitting/models/betaLASSO/TotalHerbaceousCover_forest_noTLP_FALSE_removeAnomaliesFALSE_trimAnom_bestLambdaGLM.rds")
totalTree_GS <- # include anomalies option
  #readRDS("./Analysis/VegComposition/ModelFitting/models/betaLASSO/TotalTreeCover_shrubGrass_noTLP_FALSE_removeAnomaliesFALSE_trimAnom_bestLambdaGLM.rds")
  #remove anomalies option #
  readRDS("./Analysis/VegComposition/ModelFitting/models/betaLASSO/TotalTreeCover_shrubGrass_noTLP_FALSE_removeAnomaliesTRUE_bestLambdaGLM.rds")
# Forest: best lambda model 
totalTree_F <- # include anomalies option
  #readRDS("./Analysis/VegComposition/ModelFitting/models/betaLASSO/TotalTreeCover_forest_noTLP_FALSE_removeAnomaliesFALSE_trimAnom_bestLambdaGLM.rds")
  #remove anomalies option #
  readRDS("./Analysis/VegComposition/ModelFitting/models/betaLASSO/TotalTreeCover_forest_noTLP_FALSE_removeAnomaliesTRUE_halfSELambdaGLM.rds")
bareGround_CONUS <-  readRDS("./Analysis/VegComposition/ModelFitting/models/betaLASSO/BareGroundCover_CONUS_noTLP_FALSE_removeAnomaliesFALSE_trimAnom_oneSELambdaGLM.rds")
shrub_CONUS <- readRDS("./Analysis/VegComposition/ModelFitting/models/betaLASSO/ShrubCover_CONUS_noTLP_FALSE_removeAnomaliesFALSE_trimAnom_bestLambdaGLM.rds")
BLtree_GS <- readRDS("./Analysis/VegComposition/ModelFitting/models/betaLASSO/AngioTreeCover_prop_shrubGrass_noTLP_FALSE_removeAnomaliesFALSE_trimAnom_bestLambdaGLM.rds")
BLtree_F <- readRDS("./Analysis/VegComposition/ModelFitting/models/betaLASSO/AngioTreeCover_prop_forest_noTLP_FALSE_removeAnomaliesFALSE_trimAnom_bestLambdaGLM.rds")
NLtree_GS <-  readRDS("./Analysis/VegComposition/ModelFitting/models/betaLASSO/ConifTreeCover_prop_shrubGrass_noTLP_FALSE_removeAnomaliesFALSE_trimAnom_bestLambdaGLM.rds")
NLtree_F <- readRDS("./Analysis/VegComposition/ModelFitting/models/betaLASSO/ConifTreeCover_prop_forest_noTLP_FALSE_removeAnomaliesFALSE_trimAnom_bestLambdaGLM.rds")
forb_CONUS <-  readRDS("./Analysis/VegComposition/ModelFitting/models/betaLASSO/ForbCover_prop_CONUS_noTLP_FALSE_removeAnomaliesFALSE_trimAnom_bestLambdaGLM.rds")
C3grass_CONUS <- readRDS("./Analysis/VegComposition/ModelFitting/models/betaLASSO/C3GramCover_prop_CONUS_noTLP_FALSE_removeAnomaliesFALSE_trimAnom_oneSELambdaGLM.rds")
C4grass_CONUS <- readRDS("./Analysis/VegComposition/ModelFitting/models/betaLASSO/C4GramCover_prop_CONUS_noTLP_FALSE_removeAnomaliesFALSE_trimAnom_bestLambdaGLM.rds")

# the data frame with scaled predictors is 'vegSoils_scaled' 
prednames_s <-  modDat_1_s %>%
  dplyr::select(tmin_s:AWHC_s) %>%
  names()
prednames <- str_replace(prednames_s, pattern = "_s$", replacement = "")

makePredictions <- function(predictionDF, modelObject, scale  = TRUE) {
  # pretend to scale the input variables so the model object can predict accurately
  if(scale == TRUE) {
    predictionDF <- predictionDF %>% 
      mutate(across(all_of(prednames), .fns = function(x) {base::scale(x, scale = FALSE, center = FALSE)})) 
  }
  # modelPredictions
  modelPreds <- predict(object = modelObject, newdata = predictionDF, type = "response")
  # add predictions back into the input data.frame
  predictionDF <- predictionDF %>% 
    cbind(modelPreds)
  
  # truncate all predictions to max out at 100 
  #predictionDF[predictionDF$modelPreds>100 & !is.na(predictionDF$modelPreds),"modelPreds"] <- 100
  predictionDF[predictionDF$modelPreds < 0 & !is.na(predictionDF$modelPreds),"modelPreds"] <- 0
  
  # print predicted data
  return(predictionDF)
}
totalHerb_F_predsQuantile <- makePredictions(predictionDF = vegSoils_scaled,
                                             modelObject = totalHerb_F, scale = TRUE) %>% 
  rename(absTotalHerb_F = "modelPreds")
totalHerb_GS_predsQuantile <- makePredictions(predictionDF = vegSoils_scaled,
                                              modelObject = totalHerb_GS, scale = TRUE) %>% 
  rename(absTotalHerb_GS = "modelPreds")

totalTree_F_predsQuantile<- makePredictions(predictionDF = vegSoils_scaled,
                                            modelObject = totalTree_F, scale = TRUE) %>% 
  rename(absTotalTree_F = "modelPreds")
totalTree_GS_predsQuantile<- makePredictions(predictionDF = vegSoils_scaled,
                                             modelObject = totalTree_GS, scale = TRUE) %>% 
  rename(absTotalTree_GS = "modelPreds")

bareGround_CONUS_predsQuantile<- makePredictions(predictionDF = vegSoils_scaled,
                                                 modelObject = bareGround_CONUS, scale = TRUE) %>% 
  rename(absBareGround_CONUS = "modelPreds")

shrub_CONUS_predsQuantile<- makePredictions(predictionDF = vegSoils_scaled,
                                            modelObject = shrub_CONUS, scale = TRUE) %>% 
  rename(absShrub_CONUS = "modelPreds")

BLtree_F_predsQuantile<- makePredictions(predictionDF = vegSoils_scaled,
                                         modelObject = BLtree_F, scale = TRUE) %>% 
  rename(percBLTree_F = "modelPreds")
BLtree_GS_predsQuantile<- makePredictions(predictionDF = vegSoils_scaled,
                                          modelObject = BLtree_GS, scale = TRUE) %>% 
  rename(percBLTree_GS = "modelPreds")

NLtree_F_predsQuantile<- makePredictions(predictionDF = vegSoils_scaled,
                                         modelObject = NLtree_F, scale = TRUE) %>% 
  rename(percNLTree_F = "modelPreds")
NLtree_GS_predsQuantile<- makePredictions(predictionDF = vegSoils_scaled,
                                          modelObject = NLtree_GS, scale = TRUE) %>% 
  rename(percNLTree_GS = "modelPreds")

C3grass_CONUS_predsQuantile<- makePredictions(predictionDF = vegSoils_scaled,
                                              modelObject = C3grass_CONUS, scale = TRUE) %>% 
  rename(percC3grass_CONUS = "modelPreds")

C4grass_CONUS_predsQuantile<- makePredictions(predictionDF = vegSoils_scaled,
                                              modelObject = C4grass_CONUS, scale = TRUE) %>% 
  rename(percC4grass_CONUS = "modelPreds")

forb_CONUS_predsQuantile<- makePredictions(predictionDF = vegSoils_scaled,
                                           modelObject = forb_CONUS, scale = TRUE) %>% 
  rename(percForb_CONUS = "modelPreds")

preds_quantile <- totalHerb_F_predsQuantile  %>% 
  cbind(totalHerb_GS_predsQuantile %>% select(absTotalHerb_GS)) %>% 
  cbind(totalTree_F_predsQuantile %>% select(absTotalTree_F)) %>% 
  cbind(totalTree_GS_predsQuantile %>% select(absTotalTree_GS)) %>% 
  cbind(bareGround_CONUS_predsQuantile %>% select(absBareGround_CONUS)) %>% 
  cbind(shrub_CONUS_predsQuantile %>% select(absShrub_CONUS)) %>% 
  cbind(BLtree_F_predsQuantile %>% select(percBLTree_F)) %>% 
  cbind(BLtree_GS_predsQuantile %>% select(percBLTree_GS)) %>% 
  cbind(NLtree_F_predsQuantile %>% select(percNLTree_F)) %>% 
  cbind(NLtree_GS_predsQuantile %>% select(percNLTree_GS)) %>% 
  cbind(C3grass_CONUS_predsQuantile %>% select(percC3grass_CONUS)) %>% 
  cbind(C4grass_CONUS_predsQuantile %>% select(percC4grass_CONUS)) %>% 
  cbind(forb_CONUS_predsQuantile %>% select(percForb_CONUS)) %>% 
  cbind(preds_byHand_quantile %>% select(pred)) %>% 
  rename(prob_Forest = "pred") %>% 
  mutate(prob_grassShrub = 1 - prob_Forest) %>% 
  cbind(preds_byHand_quantile %>% select(newRegion))

# for total herbaceous and total tree, scale according to ecoregion
preds_quantile <- preds_quantile %>% 
  mutate(absTotalHerb_CONUS = (prob_grassShrub * absTotalHerb_GS) + (prob_Forest * absTotalHerb_F),
         absTotalTree_CONUS = (prob_grassShrub * absTotalTree_GS) + (prob_Forest * absTotalTree_F),
  )

## For percentages (NL/BL tree and C4/C3/Forb), scale so that they sum to one (for C4/C3/Forb, already have CONUS-wide models) (for NL/BL tree, have versions for both grass/shrub and forest)
preds_quantile <- preds_quantile %>% 
  mutate(percBLTree_scaled_GS = percBLTree_GS/(percBLTree_GS + percNLTree_GS),
         percNLTree_scaled_GS = percNLTree_GS/(percBLTree_GS + percNLTree_GS),
         percBLTree_scaled_F = percBLTree_F/(percBLTree_F + percNLTree_F),
         percNLTree_scaled_F = percNLTree_F/(percBLTree_F + percNLTree_F),
         percC3grass_scaled_CONUS = percC3grass_CONUS/(percC3grass_CONUS + percC4grass_CONUS + percForb_CONUS),
         percC4grass_scaled_CONUS = percC4grass_CONUS/(percC3grass_CONUS + percC4grass_CONUS + percForb_CONUS),
         percForb_scaled_CONUS = percForb_CONUS/(percC3grass_CONUS + percC4grass_CONUS + percForb_CONUS))

## Convert percentages for level 2 cover into absolute cover values
preds_quantile <- preds_quantile %>% 
  mutate(absNLTree_GS = (percNLTree_scaled_GS * absTotalTree_GS), 
         absBLTree_GS = (percBLTree_scaled_GS * absTotalTree_GS), 
         absNLTree_F = (percNLTree_scaled_F * absTotalTree_F), 
         absBLTree_F = (percBLTree_scaled_F * absTotalTree_F), 
         absC3grass_CONUS = (percC3grass_scaled_CONUS * absTotalHerb_CONUS), # can use CONUS-wide total herbaceous since C3 grass isn't ecoregion-level
         absC4grass_CONUS = (percC4grass_scaled_CONUS * absTotalHerb_CONUS), # can use CONUS-wide total herbaceous since C4 grass isn't ecoregion-level
         absForb_CONUS = (percForb_scaled_CONUS * absTotalHerb_CONUS) # can use CONUS-wide total herbaceous since forb grass isn't ecoregion-level
  )

## For tree level 2 cover, scale by ecoregion probability prediction to be CONUS_wide
preds_quantile <- preds_quantile %>%
  mutate(absBLTree_CONUS = (prob_grassShrub * absBLTree_GS) + (prob_Forest * absBLTree_F),
         absNLTree_CONUS = (prob_grassShrub * absNLTree_GS) + (prob_Forest * absNLTree_F)
  )


# scale climate data thats in the spatially averaged dataset and predict responses using models --------
#preds_quantile_raw #(name of dataset that has spatially averaged data)
# fix names 
preds_quantile_raw <- preds_quantile_raw %>% 
  dplyr::select(Year, x, y, ShrubCover,#ShrbCvr, 
                ForbCover_prop,#ForbCvr, 
                C3GramCover_prop, #C3GrmCv,
                C4GramCover_prop,#C4GrmCv, 
                AngioTreeCover_prop,#AngTrCv, 
                ConifTreeCover_prop, #CnfTrCv, 
                TotalTreeCover, #TtlTrCv, 
                TotalHerbaceousCover, #TtlHrbC, 
                BareGroundCover, #BrGrndC, 
                #C3GrmC_, C4GrmC_, FrbCvr_, AngTrC_, CnfTrC_,
                tmin_meanAnnAvg_CLIM:durationFrostFreeDays_meanAnnAvg_3yrAnom, NA_L1CODE, 
                NA_L1NAME, NA_L1KEY, newRegion, x, y, soilDepth:totalAvailableWaterHoldingCapacity) %>% 
  mutate(         "ForbCover_abs_obs" = ForbCover_prop * TotalHerbaceousCover, 
                  "C3GrassCover_abs_obs" = C3GramCover_prop * TotalHerbaceousCover, 
                  "C4GrassCover_abs_obs" = C4GramCover_prop * TotalHerbaceousCover, 
                  "BLTree_abs_obs" = AngioTreeCover_prop * TotalTreeCover, 
                  "NLTree_abs_obs" = ConifTreeCover_prop * TotalTreeCover) %>% 
  rename("ShrubCover_obs" = ShrubCover,
         "TotalTreeCover_obs" = TotalTreeCover, 
         "TotalHerbaceousCover_obs" = TotalHerbaceousCover,
         "BareGroundCover_obs" = BareGroundCover,
         "C3GrassCover_prop_obs" =  C3GramCover_prop, 
         "C4GrassCover_prop_obs" =  C4GramCover_prop, 
         "ForbCover_prop_obs" = ForbCover_prop, 
         "BLTree_prop_obs" = AngioTreeCover_prop, 
         "NLTree_prop_obs" = ConifTreeCover_prop,
         "tmin" = tmin_meanAnnAvg_CLIM, 
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
         tmean_anom = tmean_meanAnnAvg_3yrAnom, #15
         tmin_anom = tmin_meanAnnAvg_3yrAnom, #16
         tmax_anom = tmax_meanAnnAvg_3yrAnom, #17
         prcp_anom = prcp_meanAnnTotal_3yrAnom, #18
         t_warm_anom = T_warmestMonth_meanAnnAvg_3yrAnom,  #19
         t_cold_anom = T_coldestMonth_meanAnnAvg_3yrAnom, #20
         prcp_wet_anom = precip_wettestMonth_meanAnnAvg_3yrAnom, #21
         precp_dry_anom = precip_driestMonth_meanAnnAvg_3yrAnom,  #22
         prcp_seasonality_anom = precip_Seasonality_meanAnnAvg_3yrAnom, #23 
         prcpTempCorr_anom = PrecipTempCorr_meanAnnAvg_3yrAnom, #24
         aboveFreezingMonth_anom = aboveFreezing_month_meanAnnAvg_3yrAnom, #25  
         isothermality_anom = isothermality_meanAnnAvg_3yrAnom, #26
         annWatDef_anom = annWaterDeficit_meanAnnAvg_3yrAnom, #27
         annWetDegDays_anom = annWetDegDays_meanAnnAvg_3yrAnom,  #28
         VPD_mean_anom = annVPD_mean_meanAnnAvg_3yrAnom, #29
         VPD_min_anom = annVPD_min_meanAnnAvg_3yrAnom,  #30
         VPD_max_anom = annVPD_max_meanAnnAvg_3yrAnom,  #31
         VPD_max_95_anom = annVPD_max_95percentile_3yrAnom, #32
         annWatDef_95_anom = annWaterDeficit_95percentile_3yrAnom, #33 
         annWetDegDays_5_anom = annWetDegDays_5percentile_3yrAnom ,  #34
         frostFreeDays_5_anom = durationFrostFreeDays_5percentile_3yrAnom, #35 
         frostFreeDays_anom = durationFrostFreeDays_meanAnnAvg_3yrAnom, #36
         Lon = x, 
         Lat = y
  ) 
# scale climate/weather/soils data
# get the scaling factors 

# apply the scaling factors to the contemporary climate data 
namesToScale <- preds_quantile_raw %>% 
  dplyr::select(tmin:frostFreeDays, tmean_anom:frostFreeDays_anom, soilDepth:AWHC) %>% 
  names()

climDat_scaled_spatAveraged <- purrr::map(namesToScale, .f = function(x) {
  x_new <- (preds_quantile_raw[,x] - scaleParams[,paste0(x, "_s")]$`scaled:center`)/scaleParams[,paste0(x, "_s")]$`scaled:scale`
  return(data.frame(x_new))
}) %>% 
  purrr::list_cbind()
names(climDat_scaled_spatAveraged) <- paste0(namesToScale, "_s")

## translate predictions from 0-100 scale to 0-1
preds_quantile_raw <- preds_quantile_raw %>% 
  mutate(ShrubCover_obs = ShrubCover_obs/100,
         TotalTreeCover_obs = TotalTreeCover_obs/100,
         TotalHerbaceousCover_obs = TotalHerbaceousCover_obs/100,
         BareGroundCover_obs = BareGroundCover_obs/100,
         ForbCover_abs_obs = ForbCover_abs_obs/100,
         C3GrassCover_abs_obs = C3GrassCover_abs_obs/100,
         C4GrassCover_abs_obs = C4GrassCover_abs_obs/100,
         BLTree_abs_obs = BLTree_abs_obs/100, 
         NLTree_abs_obs = NLTree_abs_obs/100)

# add scaled climate data to spatially averaged observed data 
preds_quantile_scaled <- preds_quantile_raw %>% 
  dplyr::select(Year:BareGroundCover_obs, ForbCover_abs_obs:NLTree_abs_obs, newRegion) %>% 
  cbind(climDat_scaled_spatAveraged)
names(preds_quantile_scaled)[19:68] <- str_remove(names(vegSoils_scaled)[19:68], pattern = "_s$")

## generate model predictions for ecoregion
modDat_ecoregionFit_spatAvg <- preds_quantile_raw %>% 
  #rename("Long" = x, "Lat" = y) %>% 
  dplyr::select(c(newRegion, t_warm, t_cold, prcp_wet, annWatDef, prcpTempCorr, isothermality, soilDepth, sand, coarse, carbon,
                  Lon, Lat)) %>% 
  mutate(newRegion = as.factor(newRegion)) 

# get climate data from dayMet (d.f. is "climDatPred_unscaled")
modDat_ecoregionFit_spatAvg_unscaled <- modDat_ecoregionFit_spatAvg
names(modDat_ecoregionFit_spatAvg_unscaled)[c(2:11)] <- c("T_warmestMonth_meanAnnAvg_CLIM", "T_coldestMonth_meanAnnAvg_CLIM", 
                                                          "precip_wettestMonth_meanAnnAvg_CLIM", "annWaterDeficit_meanAnnAvg_CLIM", 
                                                          "PrecipTempCorr_meanAnnAvg_CLIM", "isothermality_meanAnnAvg_CLIM", 
                                                          "soilDepth", "avgSandPerc_acrossDepth", "avgCoarsePerc_acrossDepth", "avgOrganicCarbonPerc_0_3cm")

# predict with contemporary climate data 
preds_byHand_stapAvg <- modDat_ecoregionFit_spatAvg_unscaled %>% 
  mutate(pred = 1/(1 + exp(-( 9.8726 + -0.2999*T_warmestMonth_meanAnnAvg_CLIM +  0.2456*T_coldestMonth_meanAnnAvg_CLIM +  0.0106*precip_wettestMonth_meanAnnAvg_CLIM + -0.0621*annWaterDeficit_meanAnnAvg_CLIM + -2.7863*PrecipTempCorr_meanAnnAvg_CLIM +  0.0540*isothermality_meanAnnAvg_CLIM + -0.0076*soilDepth +  0.0335*avgSandPerc_acrossDepth +  0.0310*avgCoarsePerc_acrossDepth +  0.2726*avgOrganicCarbonPerc_0_3cm))))

## now predict 
totalHerb_F_preds_spatAvg <- makePredictions(predictionDF = preds_quantile_scaled,
                                             modelObject = totalHerb_F, scale = TRUE) %>% 
  rename(absTotalHerb_F = "modelPreds")
totalHerb_GS_preds_spatAvg <- makePredictions(predictionDF = preds_quantile_scaled,
                                              modelObject = totalHerb_GS, scale = TRUE) %>% 
  rename(absTotalHerb_GS = "modelPreds")

totalTree_F_preds_spatAvg<- makePredictions(predictionDF = preds_quantile_scaled,
                                            modelObject = totalTree_F, scale = TRUE) %>% 
  rename(absTotalTree_F = "modelPreds")
totalTree_GS_preds_spatAvg<- makePredictions(predictionDF = preds_quantile_scaled,
                                             modelObject = totalTree_GS, scale = TRUE) %>% 
  rename(absTotalTree_GS = "modelPreds")

bareGround_CONUS_preds_spatAvg<- makePredictions(predictionDF = preds_quantile_scaled,
                                                 modelObject = bareGround_CONUS, scale = TRUE) %>% 
  rename(absBareGround_CONUS = "modelPreds")

shrub_CONUS_preds_spatAvg<- makePredictions(predictionDF = preds_quantile_scaled,
                                            modelObject = shrub_CONUS, scale = TRUE) %>% 
  rename(absShrub_CONUS = "modelPreds")

BLtree_F_preds_spatAvg<- makePredictions(predictionDF = preds_quantile_scaled,
                                         modelObject = BLtree_F, scale = TRUE) %>% 
  rename(percBLTree_F = "modelPreds")
BLtree_GS_preds_spatAvg<- makePredictions(predictionDF = preds_quantile_scaled,
                                          modelObject = BLtree_GS, scale = TRUE) %>% 
  rename(percBLTree_GS = "modelPreds")

NLtree_F_preds_spatAvg<- makePredictions(predictionDF = preds_quantile_scaled,
                                         modelObject = NLtree_F, scale = TRUE) %>% 
  rename(percNLTree_F = "modelPreds")
NLtree_GS_preds_spatAvg<- makePredictions(predictionDF = preds_quantile_scaled,
                                          modelObject = NLtree_GS, scale = TRUE) %>% 
  rename(percNLTree_GS = "modelPreds")

C3grass_CONUS_preds_spatAvg<- makePredictions(predictionDF = preds_quantile_scaled,
                                              modelObject = C3grass_CONUS, scale = TRUE) %>% 
  rename(percC3grass_CONUS = "modelPreds")

C4grass_CONUS_preds_spatAvg<- makePredictions(predictionDF = preds_quantile_scaled,
                                              modelObject = C4grass_CONUS, scale = TRUE) %>% 
  rename(percC4grass_CONUS = "modelPreds")

forb_CONUS_preds_spatAvg<- makePredictions(predictionDF = preds_quantile_scaled,
                                           modelObject = forb_CONUS, scale = TRUE) %>% 
  rename(percForb_CONUS = "modelPreds")

preds_spatAvg <- totalHerb_F_preds_spatAvg  %>% 
  cbind(totalHerb_GS_preds_spatAvg %>% select(absTotalHerb_GS)) %>% 
  cbind(totalTree_F_preds_spatAvg %>% select(absTotalTree_F)) %>% 
  cbind(totalTree_GS_preds_spatAvg %>% select(absTotalTree_GS)) %>% 
  cbind(bareGround_CONUS_preds_spatAvg %>% select(absBareGround_CONUS)) %>% 
  cbind(shrub_CONUS_preds_spatAvg %>% select(absShrub_CONUS)) %>% 
  cbind(BLtree_F_preds_spatAvg %>% select(percBLTree_F)) %>% 
  cbind(BLtree_GS_preds_spatAvg %>% select(percBLTree_GS)) %>% 
  cbind(NLtree_F_preds_spatAvg %>% select(percNLTree_F)) %>% 
  cbind(NLtree_GS_preds_spatAvg %>% select(percNLTree_GS)) %>% 
  cbind(C3grass_CONUS_preds_spatAvg %>% select(percC3grass_CONUS)) %>% 
  cbind(C4grass_CONUS_preds_spatAvg %>% select(percC4grass_CONUS)) %>% 
  cbind(forb_CONUS_preds_spatAvg %>% select(percForb_CONUS)) %>% 
  cbind(preds_byHand_stapAvg %>% select(pred)) %>% 
  rename(prob_Forest = "pred") %>% 
  mutate(prob_grassShrub = 1 - prob_Forest) #%>% 
  #cbind(preds_byHand_stapAvg %>% select(newRegion))

# for total herbaceous and total tree, scale according to ecoregion
preds_spatAvg <- preds_spatAvg %>% 
  mutate(absTotalHerb_CONUS = (prob_grassShrub * absTotalHerb_GS) + (prob_Forest * absTotalHerb_F),
         absTotalTree_CONUS = (prob_grassShrub * absTotalTree_GS) + (prob_Forest * absTotalTree_F),
  )

## For percentages (NL/BL tree and C4/C3/Forb), scale so that they sum to one (for C4/C3/Forb, already have CONUS-wide models) (for NL/BL tree, have versions for both grass/shrub and forest)
preds_spatAvg <- preds_spatAvg %>% 
  mutate(percBLTree_scaled_GS = percBLTree_GS/(percBLTree_GS + percNLTree_GS),
         percNLTree_scaled_GS = percNLTree_GS/(percBLTree_GS + percNLTree_GS),
         percBLTree_scaled_F = percBLTree_F/(percBLTree_F + percNLTree_F),
         percNLTree_scaled_F = percNLTree_F/(percBLTree_F + percNLTree_F),
         percC3grass_scaled_CONUS = percC3grass_CONUS/(percC3grass_CONUS + percC4grass_CONUS + percForb_CONUS),
         percC4grass_scaled_CONUS = percC4grass_CONUS/(percC3grass_CONUS + percC4grass_CONUS + percForb_CONUS),
         percForb_scaled_CONUS = percForb_CONUS/(percC3grass_CONUS + percC4grass_CONUS + percForb_CONUS))

## Convert percentages for level 2 cover into absolute cover values
preds_spatAvg <- preds_spatAvg %>% 
  mutate(absNLTree_GS = (percNLTree_scaled_GS * absTotalTree_GS), 
         absBLTree_GS = (percBLTree_scaled_GS * absTotalTree_GS), 
         absNLTree_F = (percNLTree_scaled_F * absTotalTree_F), 
         absBLTree_F = (percBLTree_scaled_F * absTotalTree_F), 
         absC3grass_CONUS = (percC3grass_scaled_CONUS * absTotalHerb_CONUS), # can use CONUS-wide total herbaceous since C3 grass isn't ecoregion-level
         absC4grass_CONUS = (percC4grass_scaled_CONUS * absTotalHerb_CONUS), # can use CONUS-wide total herbaceous since C4 grass isn't ecoregion-level
         absForb_CONUS = (percForb_scaled_CONUS * absTotalHerb_CONUS) # can use CONUS-wide total herbaceous since forb grass isn't ecoregion-level
  )
# Compare model predictions of total tree cover to observations -----------
preds_totTrees <- preds_quantile %>% 
  select(Year, Lat.y, Long, Source, TotalTreeCover_obs, 
         BLTree_abs_obs, BLTree_prop_obs, NLTree_abs_obs, NLTree_prop_obs, 
         tmean ,prcp ,   prcp_dry,  isothermality, AWHC ,                 
        prcpTempCorr ,  clay, carbon, coarse, sand, prcp_seasonality, 
        prob_Forest:newRegion, 
        absTotalTree_F, absTotalTree_GS, absTotalTree_CONUS,
        percBLTree_F, percBLTree_GS,  percNLTree_F, percNLTree_GS, percBLTree_scaled_GS:percNLTree_scaled_F, 
        absNLTree_GS:absBLTree_F, 
        absBLTree_CONUS, absNLTree_CONUS
        ) %>% 
  mutate(TotalTreeCover_obs = TotalTreeCover_obs/100) # convert total tree observed into proportion (from percentages)

### GS model of total tree in GS ecoregion -- with complete set of possible predictor data 
# make dataset wide w/ a column for total tree observations for each data source
preds_totTrees_GSall <- preds_totTrees %>%
  filter(newRegion == "dryShrubGrass") %>%
  select(Year, Lat.y, Long, TotalTreeCover_obs,  absTotalTree_GS, Source,
         absTotalTree_F,
         prcp , prcp_seasonality, sand, AWHC) %>%
  mutate(uniqueID = 1:nrow(.)) %>% 
  pivot_wider(names_from = Source, values_from = TotalTreeCover_obs, 
              names_prefix = "TotalTreeCover_obs_")


preds_averaged_GSall <- preds_spatAvg %>% 
  filter(newRegion == "dryShrubGrass") %>% 
  select(TotalTreeCover_obs, absTotalTree_GS,  prcp, prcp_seasonality, sand, AWHC) %>% 
  drop_na()
  
deciles_averagedPreds_totTree_GSall <- predvars2deciles_multObs(df = preds_averaged_GSall, 
                           response_vars = c("TotalTreeCover_obs", "absTotalTree_GS"), 
                           pred_vars = c( "prcp",  "prcp_seasonality", "sand", "AWHC"), # for predictors, combine list of predictors for all models (total tree for GS and F, and perc NL tree for GS and F)
                           cut_points = seq(0, 1, 0.03)
                           )

deciles_totTree_GSall <- predvars2deciles_multObs(df = preds_totTrees_GSall, 
                                          response_vars = c("TotalTreeCover_obs_FIA","TotalTreeCover_obs_LANDFIRE", "TotalTreeCover_obs_LDC", "TotalTreeCover_obs_RAP",
                                                            "absTotalTree_GS"), # name of observations, followed by name of predictions
                                    pred_vars = c( "prcp",  "prcp_seasonality", "sand", "AWHC"), # for predictors, combine list of predictors for all models (total tree for GS and F, and perc NL tree for GS and F)
                                    cut_points = seq(0, 1, 0.03)
)
deciles_test <- deciles_totTree_GSall %>% 
 full_join(deciles_averagedPreds_totTree_GSall %>% 
             select(name, decile, TotalTreeCover_obs, TotalTreeCover_obs_IQR_high, TotalTreeCover_obs_IQR_low))


quantPlot_totTree_GSall <- decile_dotplot_MultObs(df = deciles_test, response= c("TotalTreeCover_obs_FIA","TotalTreeCover_obs_LANDFIRE", "TotalTreeCover_obs_LDC", "TotalTreeCover_obs_RAP",
                                                                                 "TotalTreeCover_obs", "absTotalTree_GS"), IQR = TRUE,
                                                   CI = FALSE
) + ggtitle("Total tree cover - Grass/shrub model in Grass/shrub") + ylab("TotalTreeCover_obs")

## how many observations are included in this dataset from each data source? 
pieChart_totTree_GSall <- preds_totTrees_GSall %>% 
  summarize(n_FIA = sum(!is.na(TotalTreeCover_obs_FIA)),
            n_AIM = sum(!is.na(TotalTreeCover_obs_LDC)),
            n_LANDFIRE = sum(!is.na(TotalTreeCover_obs_LANDFIRE)),
            n_RAP = sum(!is.na(TotalTreeCover_obs_RAP))) %>% 
  pivot_longer(cols = n_FIA:n_RAP, names_to = "source", values_to = "n_observations") %>% 
ggplot(aes( y = n_observations, fill = source)) + 
  geom_bar(aes(x = ""), width = 1, stat = "identity", color = "white") +
  theme(axis.line = element_blank(), 
        plot.title = element_text(hjust=0.5)) +
  scale_fill_manual(values = c("darkred", "darkgreen", "darkblue", "darkorange")) +
  coord_polar("y", start=0) +
  theme_void() + 
  ggtitle("No. of observations \n per dataset")

## add to quantile plot
quantPlot_totTree_GSall <- ggarrange(quantPlot_totTree_GSall, ggarrange(#inset,
  ggplot(), pieChart_totTree_GSall
  , ggplot(), widths = c(1.5,1,1.5), nrow = 1), heights = c(3, 1), nrow = 2)

### GS model of total tree in GS ecoregion -- with only predictors that have observations for NL/BL trees
# make dataset wide w/ a column for total tree observations for each data source
preds_totTrees_GSsmall <- preds_totTrees %>%
  filter(!is.na(BLTree_prop_obs)) %>% 
  filter(newRegion == "dryShrubGrass") %>%
  select(Year, Lat.y, Long, TotalTreeCover_obs,  absTotalTree_GS, Source,
         absTotalTree_F,
         prcp , prcp_seasonality, sand, AWHC) %>%
  mutate(uniqueID = 1:nrow(.)) %>% 
  pivot_wider(names_from = Source, values_from = TotalTreeCover_obs, 
              names_prefix = "TotalTreeCover_obs_")

preds_averaged_GSsmall <- preds_spatAvg %>% 
  filter(newRegion == "dryShrubGrass") %>% 
  filter(!is.na(NLTree_prop_obs)) %>% 
  select(TotalTreeCover_obs, absTotalTree_GS,  prcp, prcp_seasonality, sand, AWHC) %>% 
  drop_na()

deciles_averagedPreds_totTree_GSsmall <- predvars2deciles_multObs(df = preds_averaged_GSsmall, 
                                                                response_vars = c("TotalTreeCover_obs", "absTotalTree_GS"), 
                                                                pred_vars = c( "prcp",  "prcp_seasonality", "sand", "AWHC"), # for predictors, combine list of predictors for all models (total tree for GS and F, and perc NL tree for GS and F)
                                                                cut_points = seq(0, 1, 0.03)
)

deciles_totTree_GSsmall <- predvars2deciles_multObs(df = preds_totTrees_GSsmall, 
                                                  response_vars = c("TotalTreeCover_obs_FIA","TotalTreeCover_obs_LANDFIRE", "TotalTreeCover_obs_LDC", 
                                                                    "absTotalTree_GS"), # name of observations, followed by name of predictions
                                                  pred_vars = c( "prcp",  "prcp_seasonality", "sand", "AWHC"), # for predictors, combine list of predictors for all models (total tree for GS and F, and perc NL tree for GS and F)
                                                  cut_points = seq(0, 1, 0.03)
)
deciles_test <- deciles_totTree_GSsmall %>% 
  full_join(deciles_averagedPreds_totTree_GSsmall %>% 
                select(name, decile, TotalTreeCover_obs, TotalTreeCover_obs_IQR_high, TotalTreeCover_obs_IQR_low))


quantPlot_totTree_GSsmall <- decile_dotplot_MultObs(df = deciles_test, response= c("TotalTreeCover_obs_FIA","TotalTreeCover_obs_LANDFIRE", "TotalTreeCover_obs_LDC", 
                                                                                 "TotalTreeCover_obs", "absTotalTree_GS"), IQR = TRUE,
                                                  CI = FALSE
) + ggtitle("Total tree cover - Grass/shrub model in Grass/shrub - \nonly observations that have NL/BL tree info") + ylab("TotalTreeCover_obs")

## how many observations are included in this dataset from each data source? 
pieChart_totTree_GSsmall <- preds_totTrees_GSsmall %>% 
  summarize(n_FIA = sum(!is.na(TotalTreeCover_obs_FIA)),
            n_AIM = sum(!is.na(TotalTreeCover_obs_LDC)),
            n_LANDFIRE = sum(!is.na(TotalTreeCover_obs_LANDFIRE)),
            #n_RAP = sum(!is.na(TotalTreeCover_obs_RAP))
            ) %>% 
  pivot_longer(cols = n_FIA:n_LANDFIRE#n_RAP
               , names_to = "source", values_to = "n_observations") %>% 
  ggplot(aes( y = n_observations, fill = source)) + 
  geom_bar(aes(x = ""), width = 1, stat = "identity", color = "white") +
  theme(axis.line = element_blank(), 
        plot.title = element_text(hjust=0.5)) +
  scale_fill_manual(values = c("darkred", "darkgreen", "darkblue"#, "darkorange"
                               )) +
  coord_polar("y", start=0) +
  theme_void() + 
  ggtitle("No. of observations \n per dataset")

## add to quantile plot
quantPlot_totTree_GSsmall <- ggarrange(quantPlot_totTree_GSsmall, ggarrange(#inset,
  ggplot(), pieChart_totTree_GSsmall
  , ggplot(), widths = c(1.5,1,1.5), nrow = 1), heights = c(3, 1), nrow = 2)

### Forest model of total tree in Forest ecoregion -- with complete set of possible predictor data 
# make dataset wide w/ a column for total tree observations for each data source
preds_totTrees_Fall <- preds_totTrees %>%
  filter(newRegion != "dryShrubGrass") %>%
  select(Year, Lat.y, Long, TotalTreeCover_obs,  absTotalTree_F, Source,
         absTotalTree_F,
         tmean, prcp, prcp_dry, isothermality, AWHC, prcpTempCorr, clay, carbon, coarse, sand
         ) %>%
  mutate(uniqueID = 1:nrow(.)) %>% 
  pivot_wider(names_from = Source, values_from = TotalTreeCover_obs, 
              names_prefix = "TotalTreeCover_obs_")

preds_averaged_Fall <- preds_spatAvg %>% 
  filter(newRegion != "dryShrubGrass") %>% 
  select(TotalTreeCover_obs,absTotalTree_F,  "tmean", "prcp", "prcp_dry", "isothermality", "AWHC", 
                                                                "prcpTempCorr", "clay", "carbon", "coarse", "sand") %>% 
  drop_na()


deciles_averagedPreds_totTree_Fall <- predvars2deciles_multObs(df = preds_averaged_Fall, 
                                                                  response_vars = c("TotalTreeCover_obs", "absTotalTree_F"), 
                                                                  pred_vars = c("tmean", "prcp", "prcp_dry", "isothermality", "AWHC", 
                                                                                "prcpTempCorr", "clay", "carbon", "coarse", "sand"), # for predictors, combine list of predictors for all models (total tree for GS and F, and perc NL tree for GS and F)
                                                                  cut_points = seq(0, 1, 0.03)
)
deciles_totTree_Fall <- predvars2deciles_multObs(df = preds_totTrees_Fall, 
                                                  response_vars = c("TotalTreeCover_obs_FIA","TotalTreeCover_obs_LANDFIRE", "TotalTreeCover_obs_LDC",
                                                                    "absTotalTree_F"), # name of observations, followed by name of predictions
                                                  pred_vars = c("tmean", "prcp", "prcp_dry", "isothermality", "AWHC", 
                                                                "prcpTempCorr", "clay", "carbon", "coarse", "sand"), # for predictors, combine list of predictors for all models (total tree for GS and F, and perc NL tree for GS and F)
                                                  cut_points = seq(0, 1, 0.03)
)
deciles_test <- deciles_totTree_Fall %>% 
  full_join(deciles_averagedPreds_totTree_Fall %>% 
              select(name, decile, TotalTreeCover_obs, TotalTreeCover_obs_IQR_high, TotalTreeCover_obs_IQR_low))

quantPlot_totTree_Fall <- decile_dotplot_MultObs(df = deciles_test, response= c("TotalTreeCover_obs_FIA","TotalTreeCover_obs_LANDFIRE", "TotalTreeCover_obs_LDC", 
                                                                                          "TotalTreeCover_obs", "absTotalTree_F"), IQR = TRUE,
                                                  CI = FALSE
) + ggtitle("Total tree cover - Forest model in Forest") + ylab("TotalTreeCover_obs")

## how many observations are included in this dataset from each data source? 
pieChart_totTree_Fall <- preds_totTrees_Fall %>% 
  summarize(n_FIA = sum(!is.na(TotalTreeCover_obs_FIA)),
            n_AIM = sum(!is.na(TotalTreeCover_obs_LDC)),
            n_LANDFIRE = sum(!is.na(TotalTreeCover_obs_LANDFIRE)),
            #n_RAP = sum(!is.na(TotalTreeCover_obs_RAP))
  ) %>% 
  pivot_longer(cols = n_FIA:n_LANDFIRE#n_RAP
               , names_to = "source", values_to = "n_observations") %>% 
  ggplot(aes( y = n_observations, fill = source)) + 
  geom_bar(aes(x = ""), width = 1, stat = "identity", color = "white") +
  theme(axis.line = element_blank(), 
        plot.title = element_text(hjust=0.5)) +
  scale_fill_manual(values = c("darkred", "darkgreen", "darkblue"#, "darkorange"
  )) +
  coord_polar("y", start=0) +
  theme_void() + 
  ggtitle("No. of observations \n per dataset")

## add to quantile plot
quantPlot_totTree_Fall <- ggarrange(quantPlot_totTree_Fall, ggarrange(#inset,
  ggplot(), pieChart_totTree_Fall
  , ggplot(), widths = c(1.5,1,1.5), nrow = 1), heights = c(3, 1), nrow = 2)

### Forest model of total tree in Forest ecoregion -- with only predictors that have observations for NL/BL trees
# make dataset wide w/ a column for total tree observations for each data source
preds_totTrees_Fsmall <- preds_totTrees %>%
  filter(!is.na(BLTree_prop_obs)) %>% 
  filter(newRegion != "dryShrubGrass") %>%
  select(Year, Lat.y, Long, TotalTreeCover_obs,  absTotalTree_F, Source,
         absTotalTree_F,
         tmean, prcp, prcp_dry, isothermality, AWHC, prcpTempCorr, clay, carbon, coarse, sand) %>%
  mutate(uniqueID = 1:nrow(.)) %>% 
  pivot_wider(names_from = Source, values_from = TotalTreeCover_obs, 
              names_prefix = "TotalTreeCover_obs_")

preds_averaged_Fsmall <- preds_spatAvg %>% 
  filter(newRegion != "dryShrubGrass") %>% 
  filter(!is.na(NLTree_prop_obs)) %>% 
  select(TotalTreeCover_obs,absTotalTree_F,  "tmean", "prcp", "prcp_dry", "isothermality", "AWHC", 
         "prcpTempCorr", "clay", "carbon", "coarse", "sand") %>% 
  drop_na()


deciles_averagedPreds_totTree_Fsmall <- predvars2deciles_multObs(df = preds_averaged_Fsmall, 
                                                               response_vars = c("TotalTreeCover_obs", "absTotalTree_F"), 
                                                               pred_vars = c("tmean", "prcp", "prcp_dry", "isothermality", "AWHC", 
                                                                             "prcpTempCorr", "clay", "carbon", "coarse", "sand"), # for predictors, combine list of predictors for all models (total tree for GS and F, and perc NL tree for GS and F)
                                                               cut_points = seq(0, 1, 0.03)
)
deciles_totTree_Fsmall <- predvars2deciles_multObs(df = preds_totTrees_Fsmall, 
                                                 response_vars = c("TotalTreeCover_obs_FIA","TotalTreeCover_obs_LANDFIRE", "TotalTreeCover_obs_LDC",
                                                                   "absTotalTree_F"), # name of observations, followed by name of predictions
                                                 pred_vars = c("tmean", "prcp", "prcp_dry", "isothermality", "AWHC", 
                                                               "prcpTempCorr", "clay", "carbon", "coarse", "sand"), # for predictors, combine list of predictors for all models (total tree for GS and F, and perc NL tree for GS and F)
                                                 cut_points = seq(0, 1, 0.03)
)
deciles_test <- deciles_totTree_Fsmall %>% 
  full_join(deciles_averagedPreds_totTree_Fsmall %>% 
              select(name, decile, TotalTreeCover_obs, TotalTreeCover_obs_IQR_high, TotalTreeCover_obs_IQR_low))

quantPlot_totTree_Fsmall <- decile_dotplot_MultObs(df = deciles_test, response= c("TotalTreeCover_obs_FIA","TotalTreeCover_obs_LANDFIRE", "TotalTreeCover_obs_LDC", 
                                                                                "TotalTreeCover_obs", "absTotalTree_F"), IQR = TRUE,
                                                 CI = FALSE
) + ggtitle("Total tree cover - Forest model in Forest - \nonly observations that have NL/BL tree info") + ylab("TotalTreeCover_obs")

## how many observations are included in this dataset from each data source? 
pieChart_totTree_Fsmall <- preds_totTrees_Fsmall %>% 
  summarize(n_FIA = sum(!is.na(TotalTreeCover_obs_FIA)),
            n_AIM = sum(!is.na(TotalTreeCover_obs_LDC)),
            n_LANDFIRE = sum(!is.na(TotalTreeCover_obs_LANDFIRE)),
            #n_RAP = sum(!is.na(TotalTreeCover_obs_RAP))
  ) %>% 
  pivot_longer(cols = n_FIA:n_LANDFIRE#n_RAP
               , names_to = "source", values_to = "n_observations") %>% 
  ggplot(aes( y = n_observations, fill = source)) + 
  geom_bar(aes(x = ""), width = 1, stat = "identity", color = "white") +
  theme(axis.line = element_blank(), 
        plot.title = element_text(hjust=0.5)) +
  scale_fill_manual(values = c("darkred", "darkgreen", "darkblue"#, "darkorange"
  )) +
  coord_polar("y", start=0) +
  theme_void() + 
  ggtitle("No. of observations \n per dataset")

## add to quantile plot
quantPlot_totTree_Fsmall <- ggarrange(quantPlot_totTree_Fsmall, ggarrange(#inset,
  ggplot(), pieChart_totTree_Fsmall
  , ggplot(), widths = c(1.5,1,1.5), nrow = 1), heights = c(3, 1), nrow = 2)

# # by hand just to test - tree model in forests, all available data
# (decilesPlot_byHand_NLTree <- preds_totTrees_Fall %>%
#   pivot_longer(cols = tmean:sand, names_to = "predName", values_to = "predValue")  %>% 
# ggplot() +
#   facet_wrap(~predName, scales = "free") +
#   geom_point(aes(x = predValue, y = TotalTreeCover_obs_FIA), alpha = .1, col = "darkgreen") + # observed - FIA
#     geom_point(aes(x = predValue, y = TotalTreeCover_obs_LANDFIRE), alpha = .1, col = "darkblue") + # observed - LANDFIRE
#     geom_point(aes(x = predValue, y = TotalTreeCover_obs_LDC), alpha = .1, col = "darkred") + # observed - AIM
#   geom_point(aes(x = predValue, y = absTotalTree_F), alpha = .1, col = "darkgrey") + # predicted
#   geom_smooth(aes(x = predValue, y = TotalTreeCover_obs_FIA), col = "darkgreen") +
#     geom_smooth(aes(x = predValue, y = TotalTreeCover_obs_LANDFIRE), col = "darkblue") +
#     geom_smooth(aes(x = predValue, y = TotalTreeCover_obs_LDC), col = "darkred") +
#   geom_smooth(aes(x = predValue, y = absTotalTree_F), col = "black")
# )
# 
# quantPlot_totTree_Fall

# Visualize position of total tree cover datasets in climate space -------------------------
## grass shrub model of total trees in grass/shrub, all available data (PCA only includes climate/weather/soils variables that are included in the final model)
pcaDat_GSall <- preds_totTrees %>%
  filter(newRegion == "dryShrubGrass") %>% 
  filter(!is.na(TotalTreeCover_obs))
  
pca_GSall <- prcomp(pcaDat_GSall[,c( "prcp",  "prcp_seasonality", "sand", "AWHC")])
library(factoextra)
pcaFig_totTree_GSall <- (fviz_pca_biplot(pca_GSall,  
              geom.var = c("arrow", "text"),
              col.var = "black", 
              habillage = pcaDat_GSall$Source, label = "var", 
              addEllipses = TRUE, ellipse.level = .95, ggtheme = theme_minimal(), alpha.ind = .05, 
              palette = c("darkgreen", "darkblue", "darkred", "darkorange"), 
              title = "PCA showing observations of total trees within the predictor space \n (predictors are those in the final model of total trees in Grass/Shrub) \n (using complete available dataset) "))


## grass shrub model of total trees in grass/shrub, only locations w/ NL/BL tree info (PCA only includes climate/weather/soils variables that are included in the final model)
pcaDat_GSsmall <- preds_totTrees %>%
  filter(newRegion == "dryShrubGrass") %>% 
  filter(!is.na(NLTree_prop_obs)) %>% 
  filter(!is.na(TotalTreeCover_obs))

pca_GSsmall <- prcomp(pcaDat_GSsmall[,c( "prcp",  "prcp_seasonality", "sand", "AWHC")])

pcaFig_totTree_GSsmall <- (fviz_pca_biplot(pca_GSsmall,  
                 geom.var = c("arrow", "text"),
                 col.var = "black", 
                 habillage = pcaDat_GSsmall$Source, label = "var", 
                 addEllipses = TRUE, ellipse.level = .95, ggtheme = theme_minimal(), alpha.ind = .05, 
                 palette = c("darkgreen", "darkblue", "darkred", "darkorange"), 
                 title = "PCA showing observations of total trees within the predictor space \n (predictors are those in the final model of total trees in Grass/Shrub) \n (using only data from locations with NL/BL data) "))

## grass shrub model of total trees in grass/shrub, compare all available data to data w/ observations for NL/Bl trees
pcaDat_GSsmall$dataset <- "curtailed"
pcaDat_GSall$dataset <- "complete"

pcaDat_GScompare <- pcaDat_GSsmall %>% 
  rbind(pcaDat_GSall)
  
pca_GScompare <- prcomp(pcaDat_GScompare[,c( "prcp",  "prcp_seasonality", "sand", "AWHC")])
pcaFig_totTree_GSCompare <- (fviz_pca_biplot(pca_GScompare,  
                 geom.var = c("arrow", "text"),
                 col.var = "black", 
                 habillage = pcaDat_GScompare$dataset, label = "var", 
                 addEllipses = TRUE, ellipse.level = .95, ggtheme = theme_minimal(), alpha.ind = .05, 
                 #palette = c("darkgreen", "darkblue", "darkred", "darkorange"), 
                 title = "PCA showing observations of total trees within the predictor space \n (predictors are those in the final model of total trees in Grass/Shrub) \n (compare the 'complete' dataset to the 'curtailed' dataset) "))


## forest model of total trees in Forest, all available data (PCA only includes climate/weather/soils variables that are included in the final model)
pcaDat_Fall <- preds_totTrees %>%
  filter(newRegion != "dryShrubGrass") %>% 
  filter(!is.na(TotalTreeCover_obs)) %>% 
  select(TotalTreeCover_obs, "tmean", "prcp", "prcp_dry", "isothermality", "AWHC", 
         "prcpTempCorr", "clay", "carbon", "coarse", "sand", Source) %>% 
  drop_na()

pca_Fall <- prcomp(pcaDat_Fall[,c("tmean", "prcp", "prcp_dry", "isothermality", "AWHC", 
                                  "prcpTempCorr", "clay", "carbon", "coarse", "sand")])

pcaFig_totTree_Fall <- (fviz_pca_biplot(pca_Fall,  
                 geom.var = c("arrow", "text"),
                 col.var = "black", 
                 habillage = pcaDat_Fall$Source, label = "var", 
                 addEllipses = TRUE, ellipse.level = .95, ggtheme = theme_minimal(), alpha.ind = .05, 
                 palette = c("darkgreen", "darkblue", "darkred", "darkorange"), 
                 title = "PCA showing observations of total trees within the predictor space \n (predictors are those in the final model of total trees in Forest) \n (using complete available dataset) "))


## forest model of total trees in Forest, only locations w/ NL/BL tree info (PCA only includes climate/weather/soils variables that are included in the final model)
pcaDat_Fsmall <- preds_totTrees %>%
  filter(newRegion != "dryShrubGrass") %>% 
  filter(!is.na(NLTree_prop_obs)) %>% 
  filter(!is.na(TotalTreeCover_obs)) %>% 
  select(TotalTreeCover_obs, "tmean", "prcp", "prcp_dry", "isothermality", "AWHC", 
         "prcpTempCorr", "clay", "carbon", "coarse", "sand", Source) %>% 
  drop_na()

pca_Fsmall <- prcomp(pcaDat_Fsmall[,c( "tmean", "prcp", "prcp_dry", "isothermality", "AWHC", 
                                       "prcpTempCorr", "clay", "carbon", "coarse", "sand")])

pcaFig_totTree_Fsmall <- (fviz_pca_biplot(pca_Fsmall,  
                 geom.var = c("arrow", "text"),
                 col.var = "black", 
                 habillage = pcaDat_Fsmall$Source, label = "var", 
                 addEllipses = TRUE, ellipse.level = .95, ggtheme = theme_minimal(), alpha.ind = .05, 
                 palette = c("darkgreen", "darkblue", "darkred", "darkorange"), 
                 title = "PCA showing observations of total trees within the predictor space \n (predictors are those in the final model of total trees in Forest) \n (using only data from locations with NL/BL data) "))

## forest model of total trees in forest, compare all available data to data w/ observations for NL/Bl trees
pcaDat_Fsmall$dataset <- "curtailed"
pcaDat_Fall$dataset <- "complete"

pcaDat_Fcompare <- pcaDat_Fsmall %>% 
  rbind(pcaDat_Fall)

pca_Fcompare <- prcomp(pcaDat_Fcompare[,c("tmean", "prcp", "prcp_dry", "isothermality", "AWHC", 
                                              "prcpTempCorr", "clay", "carbon", "coarse", "sand")])
pcaFig_totTree_FCompare <-(fviz_pca_biplot(pca_Fcompare,  
                 geom.var = c("arrow", "text"),
                 col.var = "black", 
                 habillage = pcaDat_Fcompare$dataset, label = "var", 
                 addEllipses = TRUE, ellipse.level = .95, ggtheme = theme_minimal(), alpha.ind = .05, 
                 #palette = c("darkgreen", "darkblue", "darkred", "darkorange"), 
                 title = "PCA showing observations of total trees within the predictor space \n (predictors are those in the final model of total trees in Forest) \n (compare the 'complete' dataset to the 'curtailed' dataset) "))

# Compare model predictions of total herbaceous cover to observations -----------
preds_totHerb <- preds_quantile %>% 
  select(Year,  Lat.y, Long, Source, TotalHerbaceousCover_obs, 
        ForbCover_abs_obs, C3GrassCover_abs_obs, C4GrassCover_abs_obs, ForbCover_prop_obs, C3GrassCover_prop_obs, C4GrassCover_prop_obs,
         prcp, prcp_dry, prcpTempCorr, isothermality, sand, coarse, AWHC, isothermality_anom,
         tmean, prcp_anom, clay, carbon,
         prob_Forest:newRegion, 
         absTotalHerb_GS, absTotalHerb_F, absTotalHerb_CONUS,
         percForb_CONUS, percC3grass_CONUS, percC4grass_CONUS,
        percC3grass_scaled_CONUS:percForb_scaled_CONUS, 
        absC3grass_CONUS:absForb_CONUS
  ) %>% 
  mutate(TotalHerbaceousCover_obs = TotalHerbaceousCover_obs/100) # convert total tree observed into proportion (from percentages)

### GS model of total tree in GS ecoregion -- with complete set of possible predictor data 
# make dataset wide w/ a column for total tree observations for each data source
preds_totHerb_GSall <- preds_totHerb %>%
  filter(newRegion == "dryShrubGrass") %>%
  select(Year, Lat.y, Long, TotalHerbaceousCover_obs,  absTotalHerb_GS, Source,
         absTotalHerb_F, prcpTempCorr, isothermality, sand, coarse, AWHC) %>%
  mutate(uniqueID = 1:nrow(.)) %>% 
  pivot_wider(names_from = Source, values_from = TotalHerbaceousCover_obs, 
              names_prefix = "TotalHerbaceousCover_obs_")


preds_averaged_GSall <- preds_spatAvg %>% 
  filter(newRegion == "dryShrubGrass") %>% 
  select(TotalHerbaceousCover_obs,absTotalHerb_GS, prcpTempCorr, isothermality, sand, coarse, AWHC) %>% 
  drop_na()

deciles_averagedPreds_totHerb_GSall <- predvars2deciles_multObs(df = preds_averaged_GSall, 
                                                                response_vars = c("TotalHerbaceousCover_obs", "absTotalHerb_GS"), 
                                                                pred_vars = c("prcpTempCorr", "isothermality", "sand", "coarse", "AWHC"), # for predictors, combine list of predictors for all models (total tree for GS and F, and perc NL tree for GS and F)
                                                                cut_points = seq(0, 1, 0.03)
)

deciles_totHerb_GSall <- predvars2deciles_multObs(df = preds_totHerb_GSall, 
                                                  response_vars = c("TotalHerbaceousCover_obs_FIA","TotalHerbaceousCover_obs_LANDFIRE", "TotalHerbaceousCover_obs_LDC", "TotalHerbaceousCover_obs_RAP",
                                                                    "absTotalHerb_GS"), # name of observations, followed by name of predictions
                                                  pred_vars = c("prcpTempCorr", "isothermality", "sand", "coarse", "AWHC"), # for predictors, combine list of predictors for all models (total tree for GS and F, and perc NL tree for GS and F)
                                                  cut_points = seq(0, 1, 0.03)
)
deciles_test <- deciles_totHerb_GSall %>% 
  full_join(deciles_averagedPreds_totHerb_GSall %>% 
              select(name, decile, TotalHerbaceousCover_obs, TotalHerbaceousCover_obs_IQR_high, TotalHerbaceousCover_obs_IQR_low))


quantPlot_totHerb_GSall <- decile_dotplot_MultObs(df = deciles_test, response= c("TotalHerbaceousCover_obs_FIA","TotalHerbaceousCover_obs_LANDFIRE", "TotalHerbaceousCover_obs_LDC", "TotalHerbaceousCover_obs_RAP",
                                                                                 "TotalHerbaceousCover_obs", "absTotalHerb_GS"), IQR = TRUE,
                                                  CI = FALSE
) + ggtitle("Total herbaceous cover - Grass/shrub model in Grass/shrub") + ylab("TotalHerbaceousCover_obs")

## how many observations are included in this dataset from each data source? 
pieChart_totHerb_GSall <- preds_totHerb_GSall %>% 
  summarize(n_FIA = sum(!is.na(TotalHerbaceousCover_obs_FIA)),
            n_AIM = sum(!is.na(TotalHerbaceousCover_obs_LDC)),
            n_LANDFIRE = sum(!is.na(TotalHerbaceousCover_obs_LANDFIRE)),
            n_RAP = sum(!is.na(TotalHerbaceousCover_obs_RAP))) %>% 
  pivot_longer(cols = n_FIA:n_RAP, names_to = "source", values_to = "n_observations") %>% 
  ggplot(aes( y = n_observations, fill = source)) + 
  geom_bar(aes(x = ""), width = 1, stat = "identity", color = "white") +
  theme(axis.line = element_blank(), 
        plot.title = element_text(hjust=0.5)) +
  scale_fill_manual(values = c("darkred", "darkgreen", "darkblue", "darkorange")) +
  coord_polar("y", start=0) +
  theme_void() + 
  ggtitle("No. of observations \n per dataset")

## add to quantile plot
quantPlot_totHerb_GSall <- ggarrange(quantPlot_totHerb_GSall, ggarrange(#inset,
  ggplot(), pieChart_totHerb_GSall
  , ggplot(), widths = c(1.5,1,1.5), nrow = 1), heights = c(3, 1), nrow = 2)

### GS model of total tree in GS ecoregion -- with only predictors that have observations for C3/C4/Forbs
# make dataset wide w/ a column for total tree observations for each data source
preds_totHerb_GSsmall <- preds_totHerb %>%
  filter(!is.na(C3GrassCover_prop_obs)) %>% 
  filter(newRegion == "dryShrubGrass") %>%
  select(Year, Lat.y, Long, Source, TotalHerbaceousCover_obs, 
         ForbCover_abs_obs, C3GrassCover_abs_obs, C4GrassCover_abs_obs, ForbCover_prop_obs, C3GrassCover_prop_obs, C4GrassCover_prop_obs,
         prcp, prcp_dry, prcpTempCorr, isothermality, sand, coarse, AWHC, isothermality_anom,
         tmean, prcp_anom, clay, carbon,
         prob_Forest:newRegion, 
         absTotalHerb_GS, absTotalHerb_F, absTotalHerb_CONUS,
         percForb_CONUS, percC3grass_CONUS, percC4grass_CONUS,
         percC3grass_scaled_CONUS:percForb_scaled_CONUS, 
         absC3grass_CONUS:absForb_CONUS) %>%
  mutate(uniqueID = 1:nrow(.)) %>% 
  pivot_wider(names_from = Source, values_from = TotalHerbaceousCover_obs, 
              names_prefix = "TotalHerbaceousCover_obs_")

preds_averaged_GSsmall <- preds_spatAvg %>% 
  filter(newRegion == "dryShrubGrass") %>% 
    filter(!is.na(C3GrassCover_prop_obs  )) %>% 
  select(TotalHerbaceousCover_obs, absTotalHerb_GS,  "prcpTempCorr", "isothermality", "sand", "coarse", "AWHC") %>% 
  drop_na()

deciles_averagedPreds_totHerb_GSsmall <- predvars2deciles_multObs(df = preds_averaged_GSsmall, 
                                                                  response_vars = c("TotalHerbaceousCover_obs", "absTotalHerb_GS"), 
                                                                  pred_vars = c("prcpTempCorr", "isothermality", "sand", "coarse", "AWHC"), # for predictors, combine list of predictors for all models (Total herbaceousfor GS and F, and perc NL tree for GS and F)
                                                                  cut_points = seq(0, 1, 0.03)
)

deciles_totHerb_GSsmall <- predvars2deciles_multObs(df = preds_totHerb_GSsmall, 
                                                    response_vars = c("TotalHerbaceousCover_obs_LANDFIRE", "TotalHerbaceousCover_obs_LDC", 
                                                                      "absTotalHerb_GS"), # name of observations, followed by name of predictions
                                                    pred_vars = c("prcpTempCorr", "isothermality", "sand", "coarse", "AWHC"), # for predictors, combine list of predictors for all models (Total herbaceousfor GS and F, and perc NL tree for GS and F)
                                                    cut_points = seq(0, 1, 0.03)
)
deciles_test <- deciles_totHerb_GSsmall %>% 
  full_join(deciles_averagedPreds_totHerb_GSsmall %>% 
              select(name, decile, TotalHerbaceousCover_obs, TotalHerbaceousCover_obs_IQR_high, TotalHerbaceousCover_obs_IQR_low))


quantPlot_totHerb_GSsmall <- decile_dotplot_MultObs(df = deciles_test, response= c("TotalHerbaceousCover_obs_LANDFIRE", "TotalHerbaceousCover_obs_LDC", 
                                                                                   "TotalHerbaceousCover_obs", "absTotalHerb_GS"), IQR = TRUE,
                                                    CI = FALSE
) + ggtitle("Total herbaceous cover - Grass/shrub model in Grass/shrub - \nonly observations that have C3/C4/Forb info") + ylab("TotalHerbaceousCover_obs")

## how many observations are included in this dataset from each data source? 
pieChart_totHerb_GSsmall <- preds_totHerb_GSsmall %>% 
  summarize(#n_FIA = sum(!is.na(TotalHerbaceousCover_obs_FIA)),
            n_AIM = sum(!is.na(TotalHerbaceousCover_obs_LDC)),
            n_LANDFIRE = sum(!is.na(TotalHerbaceousCover_obs_LANDFIRE)),
            #n_RAP = sum(!is.na(TotalHerbaceousCover_obs_RAP))
  ) %>% 
  pivot_longer(cols = n_AIM:n_LANDFIRE#n_RAP
               , names_to = "source", values_to = "n_observations") %>% 
  ggplot(aes( y = n_observations, fill = source)) + 
  geom_bar(aes(x = ""), width = 1, stat = "identity", color = "white") +
  theme(axis.line = element_blank(), 
        plot.title = element_text(hjust=0.5)) +
  scale_fill_manual(values = c("darkred",# "darkgreen", 
                               "darkblue"#, "darkorange"
  )) +
  coord_polar("y", start=0) +
  theme_void() + 
  ggtitle("No. of observations \n per dataset")

## add to quantile plot
quantPlot_totHerb_GSsmall <- ggarrange(quantPlot_totHerb_GSsmall, ggarrange(#inset,
  ggplot(), pieChart_totHerb_GSsmall
  , ggplot(), widths = c(1.5,1,1.5), nrow = 1), heights = c(3, 1), nrow = 2)

### Forest model of Total herbaceous in Forest ecoregion -- with complete set of possible predictor data 
# make dataset wide w/ a column for Total herbaceous observations for each data source
preds_totHerb_Fall <- preds_totHerb %>%
  filter(newRegion != "dryShrubGrass") %>%
  select(Year, Lat.y, Long, TotalHerbaceousCover_obs,  absTotalHerb_F, Source,
         absTotalHerb_F,
         prcp, prcp_dry, prcpTempCorr, isothermality, sand, coarse, AWHC, isothermality_anom, 
         tmean, prcp_anom, clay, carbon
  ) %>%
  mutate(uniqueID = 1:nrow(.)) %>% 
  pivot_wider(names_from = Source, values_from = TotalHerbaceousCover_obs, 
              names_prefix = "TotalHerbaceousCover_obs_")

preds_averaged_Fall <- preds_spatAvg %>% 
  filter(newRegion != "dryShrubGrass") %>% 
  select(TotalHerbaceousCover_obs,absTotalHerb_F,  prcp, prcp_dry, prcpTempCorr, isothermality, sand, coarse, AWHC, isothermality_anom, 
         tmean, prcp_anom, clay, carbon) %>% 
  drop_na()


deciles_averagedPreds_totHerb_Fall <- predvars2deciles_multObs(df = preds_averaged_Fall, 
                                                               response_vars = c("TotalHerbaceousCover_obs", "absTotalHerb_F"), 
                                                               pred_vars = c( "prcp", "prcp_dry", "prcpTempCorr", "isothermality", "sand", "coarse", "AWHC", "isothermality_anom", 
                                                                              "tmean", "prcp_anom", "clay", "carbon"), # for predictors, combine list of predictors for all models (Total herbaceous for GS and F, and perc NL tree for GS and F)
                                                               cut_points = seq(0, 1, 0.03)
)
deciles_totHerb_Fall <- predvars2deciles_multObs(df = preds_totHerb_Fall, 
                                                 response_vars = c("TotalHerbaceousCover_obs_FIA","TotalHerbaceousCover_obs_LANDFIRE", "TotalHerbaceousCover_obs_LDC",
                                                                   "absTotalHerb_F"), # name of observations, followed by name of predictions
                                                 pred_vars = c("prcp", "prcp_dry", "prcpTempCorr", "isothermality", "sand", "coarse", "AWHC", "isothermality_anom", 
                                                               "tmean", "prcp_anom", "clay", "carbon"), # for predictors, combine list of predictors for all models (Total herbaceous for GS and F, and perc NL tree for GS and F)
                                                 cut_points = seq(0, 1, 0.03)
)
deciles_test <- deciles_totHerb_Fall %>% 
  full_join(deciles_averagedPreds_totHerb_Fall %>% 
              select(name, decile, TotalHerbaceousCover_obs, TotalHerbaceousCover_obs_IQR_high, TotalHerbaceousCover_obs_IQR_low))

quantPlot_totHerb_Fall <- decile_dotplot_MultObs(df = deciles_test, response= c("TotalHerbaceousCover_obs_FIA","TotalHerbaceousCover_obs_LANDFIRE", "TotalHerbaceousCover_obs_LDC", 
                                                                                "TotalHerbaceousCover_obs", "absTotalHerb_F"), IQR = TRUE,
                                                 CI = FALSE
) + ggtitle("Total herbaceous cover - Forest model in Forest") + ylab("TotalHerbaceousCover_obs")

## how many observations are included in this dataset from each data source? 
pieChart_totHerb_Fall <- preds_totHerb_Fall %>% 
  summarize(n_FIA = sum(!is.na(TotalHerbaceousCover_obs_FIA)),
            n_AIM = sum(!is.na(TotalHerbaceousCover_obs_LDC)),
            n_LANDFIRE = sum(!is.na(TotalHerbaceousCover_obs_LANDFIRE)),
            #n_RAP = sum(!is.na(TotalHerbaceousCover_obs_RAP))
  ) %>% 
  pivot_longer(cols = n_FIA:n_LANDFIRE#n_RAP
               , names_to = "source", values_to = "n_observations") %>% 
  ggplot(aes( y = n_observations, fill = source)) + 
  geom_bar(aes(x = ""), width = 1, stat = "identity", color = "white") +
  theme(axis.line = element_blank(), 
        plot.title = element_text(hjust=0.5)) +
  scale_fill_manual(values = c("darkred", "darkgreen", "darkblue"#, "darkorange"
  )) +
  coord_polar("y", start=0) +
  theme_void() + 
  ggtitle("No. of observations \n per dataset")

## add to quantile plot
quantPlot_totHerb_Fall <- ggarrange(quantPlot_totHerb_Fall, ggarrange(#inset,
  ggplot(), pieChart_totHerb_Fall
  , ggplot(), widths = c(1.5,1,1.5), nrow = 1), heights = c(3, 1), nrow = 2)

### Forest model of Total herbaceous in Forest ecoregion -- with only predictors that have observations for C3/C4/Forbs
# make dataset wide w/ a column for Total herbaceous observations for each data source
preds_totHerb_Fsmall <- preds_totHerb %>%
  filter(!is.na(C3GrassCover_prop_obs)) %>% 
  filter(newRegion != "dryShrubGrass") %>%
  select(Year, Lat.y, Long, TotalHerbaceousCover_obs,  absTotalHerb_F, Source,
         absTotalHerb_F,
         prcp, prcp_dry, prcpTempCorr, isothermality, sand, coarse, AWHC, isothermality_anom, 
         tmean, prcp_anom, clay, carbon) %>%
  mutate(uniqueID = 1:nrow(.)) %>% 
  pivot_wider(names_from = Source, values_from = TotalHerbaceousCover_obs, 
              names_prefix = "TotalHerbaceousCover_obs_")

preds_averaged_Fsmall <- preds_spatAvg %>% 
  filter(newRegion != "dryShrubGrass") %>% 
  filter(!is.na(C3GrassCover_prop_obs)) %>% 
  select(TotalHerbaceousCover_obs,absTotalHerb_F,"prcp", "prcp_dry", "prcpTempCorr", "isothermality", "sand", "coarse", "AWHC", "isothermality_anom", 
         "tmean", "prcp_anom", "clay", "carbon") %>% 
  drop_na()


deciles_averagedPreds_totHerb_Fsmall <- predvars2deciles_multObs(df = preds_averaged_Fsmall, 
                                                                 response_vars = c("TotalHerbaceousCover_obs", "absTotalHerb_F"), 
                                                                 pred_vars = c("prcp", "prcp_dry", "prcpTempCorr", "isothermality", "sand", "coarse", "AWHC", "isothermality_anom", 
                                                                               "tmean", "prcp_anom", "clay", "carbon"), # for predictors, combine list of predictors for all models (Total herbaceous for GS and F, and perc NL tree for GS and F)
                                                                 cut_points = seq(0, 1, 0.03)
)
deciles_totHerb_Fsmall <- predvars2deciles_multObs(df = preds_totHerb_Fsmall, 
                                                   response_vars = c("TotalHerbaceousCover_obs_LANDFIRE", "TotalHerbaceousCover_obs_LDC",
                                                                     "absTotalHerb_F"), # name of observations, followed by name of predictions
                                                   pred_vars = c("prcp", "prcp_dry", "prcpTempCorr", "isothermality", "sand", "coarse", "AWHC", "isothermality_anom", 
                                                                 "tmean", "prcp_anom", "clay", "carbon"), # for predictors, combine list of predictors for all models (Total herbaceous for GS and F, and perc NL tree for GS and F)
                                                   cut_points = seq(0, 1, 0.03)
)
deciles_test <- deciles_totHerb_Fsmall %>% 
  full_join(deciles_averagedPreds_totHerb_Fsmall %>% 
              select(name, decile, TotalHerbaceousCover_obs, TotalHerbaceousCover_obs_IQR_high, TotalHerbaceousCover_obs_IQR_low))

quantPlot_totHerb_Fsmall <- decile_dotplot_MultObs(df = deciles_test, 
                                                   response= c("TotalHerbaceousCover_obs_LANDFIRE", "TotalHerbaceousCover_obs_LDC", 
                                                                                  "TotalHerbaceousCover_obs", "absTotalHerb_F"), 
                                                   IQR = TRUE,
                                                   CI = FALSE
) + ggtitle("Total herbaceous cover - Forest model in Forest - \nonly observations that have C3/C4/Forb info") + ylab("TotalHerbaceousCover_obs")

## how many observations are included in this dataset from each data source? 
pieChart_totHerb_Fsmall <- preds_totHerb_Fsmall %>% 
  summarize(#n_FIA = sum(!is.na(TotalHerbaceousCover_obs_FIA)),
            n_AIM = sum(!is.na(TotalHerbaceousCover_obs_LDC)),
            n_LANDFIRE = sum(!is.na(TotalHerbaceousCover_obs_LANDFIRE)),
            #n_RAP = sum(!is.na(TotalHerbaceousCover_obs_RAP))
  ) %>% 
  pivot_longer(cols = n_AIM:n_LANDFIRE#n_RAP
               , names_to = "source", values_to = "n_observations") %>% 
  ggplot(aes( y = n_observations, fill = source)) + 
  geom_bar(aes(x = ""), width = 1, stat = "identity", color = "white") +
  theme(axis.line = element_blank(), 
        plot.title = element_text(hjust=0.5)) +
  scale_fill_manual(values = c("darkred", "darkblue"#, "darkorange"
  )) +
  coord_polar("y", start=0) +
  theme_void() + 
  ggtitle("No. of observations \n per dataset")

## add to quantile plot
quantPlot_totHerb_Fsmall <- ggarrange(quantPlot_totHerb_Fsmall, ggarrange(#inset,
  ggplot(), pieChart_totHerb_Fsmall
  , ggplot(), widths = c(1.5,1,1.5), nrow = 1), heights = c(3, 1), nrow = 2)

# # by hand just to test - tree model in forests, all available data
# (decilesPlot_byHand_NLTree <- preds_totHerb_Fall %>%
#   pivot_longer(cols = tmean:sand, names_to = "predName", values_to = "predValue")  %>% 
# ggplot() +
#   facet_wrap(~predName, scales = "free") +
#   geom_point(aes(x = predValue, y = TotalHerbaceousCover_obs_FIA), alpha = .1, col = "darkgreen") + # observed - FIA
#     geom_point(aes(x = predValue, y = TotalHerbaceousCover_obs_LANDFIRE), alpha = .1, col = "darkblue") + # observed - LANDFIRE
#     geom_point(aes(x = predValue, y = TotalHerbaceousCover_obs_LDC), alpha = .1, col = "darkred") + # observed - AIM
#   geom_point(aes(x = predValue, y = absTotalHerb_F), alpha = .1, col = "darkgrey") + # predicted
#   geom_smooth(aes(x = predValue, y = TotalHerbaceousCover_obs_FIA), col = "darkgreen") +
#     geom_smooth(aes(x = predValue, y = TotalHerbaceousCover_obs_LANDFIRE), col = "darkblue") +
#     geom_smooth(aes(x = predValue, y = TotalHerbaceousCover_obs_LDC), col = "darkred") +
#   geom_smooth(aes(x = predValue, y = absTotalHerb_F), col = "black")
# )
# 
# quantPlot_totHerb_Fall

# Visualize position of total herbaceous cover datasets in climate space -------------------------
## grass shrub model of total trees in grass/shrub, all available data (PCA only includes climate/weather/soils variables that are included in the final model)
pcaDat_GSall <- preds_totHerb %>%
  filter(newRegion == "dryShrubGrass") %>% 
  filter(!is.na(TotalHerbaceousCover_obs))

pca_GSall <- prcomp(pcaDat_GSall[,c("prcpTempCorr", "isothermality", "sand", "coarse", "AWHC")])
library(factoextra)
pcaFig_totHerb_GSall <- (fviz_pca_biplot(pca_GSall,  
                 geom.var = c("arrow", "text"),
                 col.var = "black", 
                 habillage = pcaDat_GSall$Source, label = "var", 
                 addEllipses = TRUE, ellipse.level = .95, ggtheme = theme_minimal(), alpha.ind = .05, 
                 palette = c("darkgreen", "darkblue", "darkred", "darkorange"), 
                 title = "PCA showing observations of total herbaceous within the predictor space \n (predictors are those in the final model of total herbaceous in Grass/Shrub) \n (using complete available dataset) "))


## grass shrub model of total herbaceous in grass/shrub, only locations w/ C3/C4/Forb info (PCA only includes climate/weather/soils variables that are included in the final model)
pcaDat_GSsmall <- preds_totHerb %>%
  filter(newRegion == "dryShrubGrass") %>% 
  filter(!is.na(C3GrassCover_prop_obs)) %>% 
  filter(!is.na(TotalHerbaceousCover_obs))

pca_GSsmall <- prcomp(pcaDat_GSsmall[,c( "prcpTempCorr", "isothermality", "sand", "coarse", "AWHC")])

pcaFig_totHerb_GSsmall <- (fviz_pca_biplot(pca_GSsmall,  
                 geom.var = c("arrow", "text"),
                 col.var = "black", 
                 habillage = pcaDat_GSsmall$Source, label = "var", 
                 addEllipses = TRUE, ellipse.level = .95, ggtheme = theme_minimal(), alpha.ind = .05, 
                 palette = c(#"darkgreen", 
                   "darkblue", "darkred"#, "darkorange"
                   ), 
                 title = "PCA showing observations of total herbaceous within the predictor space \n (predictors are those in the final model of total herbaceous in Grass/Shrub) \n (using only data from locations with C3/C4/Forb data) "))

## grass shrub model of total herbaceous in grass/shrub, compare all available data to data w/ observations for C3/C4/Forbs
pcaDat_GSsmall$dataset <- "curtailed"
pcaDat_GSall$dataset <- "complete"

pcaDat_GScompare <- pcaDat_GSsmall %>% 
  rbind(pcaDat_GSall)

pca_GScompare <- prcomp(pcaDat_GScompare[,c("prcpTempCorr", "isothermality", "sand", "coarse", "AWHC")])
pcaFig_totHerb_GScompare <- (fviz_pca_biplot(pca_GScompare,  
                 geom.var = c("arrow", "text"),
                 col.var = "black", 
                 habillage = pcaDat_GScompare$dataset, label = "var", 
                 addEllipses = TRUE, ellipse.level = .95, ggtheme = theme_minimal(), alpha.ind = .05, 
                 #palette = c("darkgreen", "darkblue", "darkred", "darkorange"), 
                 title = "PCA showing observations of total herbaceous within the predictor space \n (predictors are those in the final model of total herbaceous in Grass/Shrub) \n (compare the 'complete' dataset to the 'curtailed' dataset) "))


## forest model of total herbaceous in Forest, all available data (PCA only includes climate/weather/soils variables that are included in the final model)
pcaDat_Fall <- preds_totHerb %>%
  filter(newRegion != "dryShrubGrass") %>% 
  filter(!is.na(TotalHerbaceousCover_obs)) %>% 
  select(TotalHerbaceousCover_obs,"prcp", "prcp_dry", "prcpTempCorr", "isothermality", "sand", "coarse", "AWHC", "isothermality_anom", 
         "tmean", "prcp_anom", "clay", "carbon", Source) %>% 
  drop_na()

pca_Fall <- prcomp(pcaDat_Fall[,c("prcp", "prcp_dry", "prcpTempCorr", "isothermality", "sand", "coarse", "AWHC", "isothermality_anom", 
                                  "tmean", "prcp_anom", "clay", "carbon")])


pcaFig_totHerb_Fall <- (fviz_pca_biplot(pca_Fall,  
                 geom.var = c("arrow", "text"),
                 col.var = "black", 
                 habillage = pcaDat_Fall$Source, label = "var", 
                 addEllipses = TRUE, ellipse.level = .95, ggtheme = theme_minimal(), alpha.ind = .05, 
                 palette = c("darkgreen", "darkblue", "darkred", "darkorange"), 
                 title = "PCA showing observations of total herbaceous within the predictor space \n (predictors are those in the final model of total herbaceous in Forest) \n (using complete available dataset) "))


## forest model of total herbaceous in Forest, only locations w/ C3/C4/Forb info (PCA only includes climate/weather/soils variables that are included in the final model)
pcaDat_Fsmall <- preds_totHerb %>%
  filter(newRegion != "dryShrubGrass") %>% 
  filter(!is.na(C3GrassCover_prop_obs)) %>% 
  filter(!is.na(TotalHerbaceousCover_obs)) %>% 
  select(TotalHerbaceousCover_obs, "prcp", "prcp_dry", "prcpTempCorr", "isothermality", "sand", "coarse", "AWHC", "isothermality_anom", 
         "tmean", "prcp_anom", "clay", "carbon", Source) %>% 
  drop_na()

pca_Fsmall <- prcomp(pcaDat_Fsmall[,c("prcp", "prcp_dry", "prcpTempCorr", "isothermality", "sand", "coarse", "AWHC", "isothermality_anom", 
                                      "tmean", "prcp_anom", "clay", "carbon")])

pcaFig_totHerb_Fsmall <- (fviz_pca_biplot(pca_Fsmall,  
                 geom.var = c("arrow", "text"),
                 col.var = "black", 
                 habillage = pcaDat_Fsmall$Source, label = "var", 
                 addEllipses = TRUE, ellipse.level = .95, ggtheme = theme_minimal(), alpha.ind = .05, 
                 palette = c( "darkblue", "darkred"), 
                 title = "PCA showing observations of total herbaceous within the predictor space \n (predictors are those in the final model of total herbaceous in Forest) \n (using only data from locations with C3/C4/Forb data) "))

## forest model of total herbaceous in forest, compare all available data to data w/ observations for C3/C4/Forbs
pcaDat_Fsmall$dataset <- "curtailed"
pcaDat_Fall$dataset <- "complete"

pcaDat_Fcompare <- pcaDat_Fsmall %>% 
  rbind(pcaDat_Fall)

pca_Fcompare <- prcomp(pcaDat_Fcompare[,c("prcp", "prcp_dry", "prcpTempCorr", "isothermality", "sand", "coarse", "AWHC", "isothermality_anom", 
                                          "tmean", "prcp_anom", "clay", "carbon")])

pcaFig_totHerb_Fcompare <- (fviz_pca_biplot(pca_Fcompare,  
                 geom.var = c("arrow", "text"),
                 col.var = "black", 
                 habillage = pcaDat_Fcompare$dataset, label = "var", 
                 addEllipses = TRUE, ellipse.level = .95, ggtheme = theme_minimal(), alpha.ind = .05, 
                 #palette = c("darkgreen", "darkblue", "darkred", "darkorange"), 
                 title = "PCA showing observations of total herbaceous within the predictor space \n (predictors are those in the final model of total herbaceous in Forest) \n (compare the 'complete' dataset to the 'curtailed' dataset) "))


# print all figures and save ----------------------------------------------
png(file = "./Figures/CoverDatFigures/ComparingFilteredDataPredictions_LANDFIREsampled.png", 
    width = 2200, height = 22000#, res = 200
      )
ggarrange(
  annotate_figure(ggarrange(
  ## tree model quantile figures and pca figures in order shown in googleDrive
  quantPlot_totTree_GSall,
  pcaFig_totTree_GSall,
  quantPlot_totTree_GSsmall,
  pcaFig_totTree_GSsmall,
  pcaFig_totTree_GSCompare,
  ncol = 1,
  heights = c(.9, .5, .9, .5, .5)
),
  top = text_grob("Observations and Model Predictions of Total Tree Cover - \nGrass/Shrub model", color = "darkblue", face = "bold.italic", size = 18),
fig.lab.face = "bold.italic",
fig.lab.size = 18
),
annotate_figure(ggarrange(
  ## tree model quantile figures and pca figures in order shown in googleDrive
  quantPlot_totTree_Fall,
  pcaFig_totTree_Fall,
  quantPlot_totTree_Fsmall,
  pcaFig_totTree_Fsmall,
  pcaFig_totTree_FCompare,
  ncol = 1,
  heights = c(.9, .5, .9, .5, .5)
),
top = text_grob("Observations and Model Predictions of Total Tree Cover - \nForest model", color = "darkblue", face = "bold.italic", size = 18),
fig.lab.face = "bold.italic",
fig.lab.size = 18
),
annotate_figure(ggarrange(
  ## tree model quantile figures and pca figures in order shown in googleDrive
  quantPlot_totHerb_GSall,
  pcaFig_totHerb_GSall,
  quantPlot_totHerb_GSsmall,
  pcaFig_totHerb_GSsmall,
  pcaFig_totHerb_GScompare,
  ncol = 1,
  heights = c(.9, .5, .9, .5, .5)
),
top = text_grob("Observations and Model Predictions of Total Herbaceous Cover - \nGrass/Shrub model", color = "darkblue", face = "bold.italic", size = 18),
fig.lab.face = "bold.italic",
fig.lab.size = 18
),
annotate_figure(ggarrange(
  ## tree model quantile figures and pca figures in order shown in googleDrive
  quantPlot_totHerb_Fall,
  pcaFig_totHerb_Fall,
  quantPlot_totHerb_Fsmall,
  pcaFig_totHerb_Fsmall,
  pcaFig_totHerb_Fcompare,
  ncol = 1,
  heights = c(.9, .5, .9, .5, .5)
),
top = text_grob("Observations and Model Predictions of Total Herbaceous Cover - \nForest model", color = "darkblue", face = "bold.italic", size = 18),
fig.lab.face = "bold.italic",
fig.lab.size = 18
),
ncol = 1
)

dev.off()

