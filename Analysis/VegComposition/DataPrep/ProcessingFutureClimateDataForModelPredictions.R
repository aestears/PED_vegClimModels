#///////////////
# Processing wall-to-wall predictions of climate information from forecasted climate data (from MACA)
# Alice Stears
# 26 June 2026
#///////////////

# load packages
library(tidyverse)
library(terra) 

# load data ---------------------------------------------------------------

# both datasets are predictions for end century (2086-2099) and rcp 8.5
# data from BNU-ESM model 
future1_rast <- rast("./Data_processed/WallToWallClimateData/BNU_ESM_1i1p1_rcp85_2086_2099/ForecastedClimateData_BNU-ESM_rcp85_CLIM.tif")

# data from IPSL-CM5A-MR model 
future2_rast <- rast("./Data_processed/WallToWallClimateData/IPSL-CM5A-MR_r1i1p1_rcp85_2086_2099/ForecastedClimateData_IPSL-CM5A-MR_rcp85_CLIM.tif")

# get data for contemporary climate, which we will use to get a data.frame version of this data
climDat_Temp <- readRDS("./Data_processed/WallToWallClimateData/DayMetData_allCONUS_2023ClimateValues_withSoils.rds")

# Extract points ----------------------------------------------------------
future1_points <- future1_rast %>% 
  #terra::crop(ext(-510000, -500000, -510000, -500000)) %>% 
  terra::as.points() %>% 
  st_as_sf() %>% 
  sf::st_join(climDat_Temp[,c("soilDepth", "surfaceClay_perc", "avgSandPerc_acrossDepth", 
                             "avgCoarsePerc_acrossDepth",  "avgOrganicCarbonPerc_0_3cm", "totalAvailableWaterHoldingCapacity")], 
              join = st_nearest_feature)

future2_points <- future2_rast %>% 
  #terra::crop(ext(-510000, -500000, -510000, -500000)) %>% 
  terra::as.points() %>% 
  st_as_sf() %>% 
  sf::st_join(climDat_Temp[,c("soilDepth", "surfaceClay_perc", "avgSandPerc_acrossDepth", 
                              "avgCoarsePerc_acrossDepth",  "avgOrganicCarbonPerc_0_3cm", "totalAvailableWaterHoldingCapacity")], 
              join = st_nearest_feature)



# remove weird bit on the top of the PNW that extends to Canada -----------
library(USAboundaries)
cropped_states <- us_states(resolution = "low")
cropped_states <- suppressMessages(cropped_states %>%
                                     dplyr::filter(name!="Hawaii") %>%
                                     dplyr::filter(name!="Alaska") %>%
                                     dplyr::filter(name!="Puerto Rico") %>%
                                     dplyr::filter(name!="American Samoa") %>%
                                     dplyr::filter(name!="Guam") %>%
                                     dplyr::filter(name!="Commonwealth of the Northern Mariana Islands") %>%
                                     dplyr::filter(name!="United States Virgin Islands")  %>%
                                     sf::st_transform(sf::st_crs(test_rast)))
# lazy hack to get the entire US boundary
cropped_states_all <- st_union(cropped_states)
# model 1
future1_points_keep <- future1_points %>% 
  #slice_sample(n = 100000) %>% 
  sf::st_covered_by(cropped_states_all, sparse = FALSE)
future1_points <- future1_points[future1_points_keep,]

# model 2
future2_points_keep <- future2_points %>% 
  #slice_sample(n = 100000) %>% 
  sf::st_covered_by(cropped_states_all, sparse = FALSE)
future2_points <- future2_points[future2_points_keep,]

# save for later use ------------------------------------------------------
saveRDS(future1_points, "./Data_processed/WallToWallClimateData/BNU_ESM_1i1p1_rcp85_2086_2099/ForecastedClimateData_BNU-ESM_PointsWithSoils.rds")

saveRDS(future2_points, "./Data_processed/WallToWallClimateData/IPSL-CM5A-MR_r1i1p1_rcp85_2086_2099/ForecastedClimateData_IPSL-CM5A-MR_PointsWithSoils.rds")

