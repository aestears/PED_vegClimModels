#///////////////////
# Acquiring and adding soils data to cover data
# Alice Stears
# 11/5/24
#///////////////////


# load packages -----------------------------------------------------------

library(tidyverse)
library(RNetCDF)
library(terra)
library(sf)

# read in veg data --------------------------------------------------------
vegDat <- readRDS("./Data_processed/CoverData/DataForModels_withEcoregion.rds")


# read in soils data ------------------------------------------------------
# this dataset is from Daniel Schlaepfer, who provided the following information:
# They are based on SOLUS100 data by Travis Nauman et al.
# Data: https://agdatacommons.nal.usda.gov/articles/dataset/Data_from_Soil_Landscapes_of_the_United_States_100-meter_SOLUS100_soil_property_maps_project_repository/25033856
# Article: in revision
# 
# Attached files were calculated from the original 100-m at specific point depths with the following parameters:
#   * aggregation 100-m to 4-km grid: average
# * soil layers: 0-3, 10, 20, 30, 40, 60, 80, 100, 150, 201 cm
# * soil depth: resdept (i.e., bedrock)
# * deepest layer:
#   if (resdept >= 5 cm next shallow standard layer depth) then new layer
# * conversion point depths to soil layers: trapezoidal rule
# * extrapolation from 150-cm point depth to deeper layers: 150-cm values


# load gridded soils data from Daniel (currently an old version, will be updated w/ SOLUS100 data)
gridClay <- terra::rast(x = "./Data_raw/soilsDB_new/claytotal_PED-CONUS4km_SOLUS100.nc") %>% 
  terra::project(y = crs(vegDat)) %>% 
  rename("horizonThickness_cm_2cm" ='claytotal_vertical=2',
         "horizonThickness_cm_7cm" = 'claytotal_vertical=7',
         "horizonThickness_cm_15cm" = "claytotal_vertical=15",
         "horizonThickness_cm_25cm" = "claytotal_vertical=25",
         "horizonThickness_cm_35cm" =  "claytotal_vertical=35",
         "horizonThickness_cm_50cm" = "claytotal_vertical=50",
         "horizonThickness_cm_70cm" = "claytotal_vertical=70",
         "horizonThickness_cm_90cm" = "claytotal_vertical=90",
         "horizonThickness_cm_125cm" ="claytotal_vertical=125",
         "horizonThickness_cm_176cm" ="claytotal_vertical=176")
gridSand <- terra::rast(x = "./Data_raw/soilsDB_new/sandtotal_PED-CONUS4km_SOLUS100.nc") %>% 
  terra::project(y = crs(vegDat) )%>% 
  rename("sandPerc_2cm" ='sandtotal_vertical=2',
         "sandPerc_7cm" = 'sandtotal_vertical=7',
         "sandPerc_15cm" = "sandtotal_vertical=15",
         "sandPerc_25cm" = "sandtotal_vertical=25",
         "sandPerc_35cm" =  "sandtotal_vertical=35",
         "sandPerc_50cm" = "sandtotal_vertical=50",
         "sandPerc_70cm" = "sandtotal_vertical=70",
         "sandPerc_90cm" = "sandtotal_vertical=90",
         "sandPerc_125cm" ="sandtotal_vertical=125",
         "sandPerc_176cm" ="sandtotal_vertical=176")
gridSilt <- terra::rast(x = "./Data_raw/soilsDB_new/silttotal_PED-CONUS4km_SOLUS100.nc") %>% 
  terra::project(y = crs(vegDat)) %>% 
  rename("siltPerc_2cm" ='silttotal_vertical=2',
         "siltPerc_7cm" = 'silttotal_vertical=7',
         "siltPerc_15cm" = "silttotal_vertical=15",
         "siltPerc_25cm" = "silttotal_vertical=25",
         "siltPerc_35cm" =  "silttotal_vertical=35",
         "siltPerc_50cm" = "silttotal_vertical=50",
         "siltPerc_70cm" = "silttotal_vertical=70",
         "siltPerc_90cm" = "silttotal_vertical=90",
         "siltPerc_125cm" ="silttotal_vertical=125",
         "siltPerc_176cm" ="silttotal_vertical=176")
gridDensity <- terra::rast(x = "./Data_raw/soilsDB_new/dbovendry_PED-CONUS4km_SOLUS100.nc") %>% 
  terra::project(y = crs(vegDat)) %>% 
  rename("density_gcm3_2cm" ='dbovendry_vertical=2',
         "density_gcm3_7cm" = 'dbovendry_vertical=7',
         "density_gcm3_15cm" = "dbovendry_vertical=15",
         "density_gcm3_25cm" = "dbovendry_vertical=25",
         "density_gcm3_35cm" =  "dbovendry_vertical=35",
         "density_gcm3_50cm" = "dbovendry_vertical=50",
         "density_gcm3_70cm" = "dbovendry_vertical=70",
         "density_gcm3_90cm" = "dbovendry_vertical=90",
         "density_gcm3_125cm" ="dbovendry_vertical=125",
         "density_gcm3_176cm" ="dbovendry_vertical=176")
gridThickness <- terra::rast(x = "./Data_raw/soilsDB_new/hzthk_PED-CONUS4km_SOLUS100.nc") %>% 
  terra::project(y = crs(vegDat)) %>% 
  rename("horizonThickness_cm_2cm" ='hzthk_vertical=2',
         "horizonThickness_cm_7cm" = 'hzthk_vertical=7',
         "horizonThickness_cm_15cm" = "hzthk_vertical=15",
         "horizonThickness_cm_25cm" = "hzthk_vertical=25",
         "horizonThickness_cm_35cm" =  "hzthk_vertical=35",
         "horizonThickness_cm_50cm" = "hzthk_vertical=50",
         "horizonThickness_cm_70cm" = "hzthk_vertical=70",
         "horizonThickness_cm_90cm" = "hzthk_vertical=90",
         "horizonThickness_cm_125cm" ="hzthk_vertical=125",
         "horizonThickness_cm_176cm" ="hzthk_vertical=176")
gridCoarse <- terra::rast(x = "./Data_raw/soilsDB_new/fragvol_PED-CONUS4km_SOLUS100.nc") %>% 
  terra::project(y = crs(vegDat)) %>% 
  rename("coarsePerc_2cm" ='fragvol_vertical=2',
         "coarsePerc_7cm" = 'fragvol_vertical=7',
         "coarsePerc_15cm" = "fragvol_vertical=15",
         "coarsePerc_25cm" = "fragvol_vertical=25",
         "coarsePerc_35cm" =  "fragvol_vertical=35",
         "coarsePerc_50cm" = "fragvol_vertical=50",
         "coarsePerc_70cm" = "fragvol_vertical=70",
         "coarsePerc_90cm" = "fragvol_vertical=90",
         "coarsePerc_125cm" ="fragvol_vertical=125",
         "coarsePerc_176cm" ="fragvol_vertical=176")

# stack into a single raster
soilRast <- c(gridClay, gridSand, gridSilt, gridCoarse, gridDensity, gridThickness)
saveRDS(soilRast, file = "./Data_processed/SoilsRaster.rds")

# sample raster to get values for the points in the cover dataset
soilRast_sampled <- soilRast %>% 
  terra::extract(y = vegDat)




### get soils data for this gridcell
# get indices for soil grid Lat and Lon
# the closest latitude
soilLat_i <- which((soilGridLats-Lat) == min(abs(soilGridLats - Lat)))

# the closest longitude 
soilLon_i <- which((soilGridLons-Long) == min(abs(soilGridLons-Long)))
#round(soilGridLons,2)==round(Long,2))
#clay (as a percentage...convert to a fraction by dividing by 100)
clay_i <- var.get.nc(soils_gridClay, "claytotal", start = c(soilLon_i, soilLat_i,1), 
                     count = c(1,1,dim.inq.nc(soils_gridClay, "vertical")$length))/100
#sand (as a percentage...convert to a fraction by dividing by 100)
sand_i <- var.get.nc(soils_gridSand, "sandtotal", start = c(soilLon_i, soilLat_i,1), 
                     count = c(1,1,dim.inq.nc(soils_gridSand, "vertical")$length))/100
#silt (as a percentage...convert to a fraction by dividing by 100)
silt_i <- var.get.nc(soils_gridSilt, "silttotal", start = c(soilLon_i, soilLat_i,1), 
                     count = c(1,1,dim.inq.nc(soils_gridSilt, "vertical")$length))/100
#coarse material (as a percentage...convert to a fraction by dividing by 100)
coarse_i <- var.get.nc(soils_gridCoarse, "fragvol", start = c(soilLon_i, soilLat_i,1), 
                       count = c(1,1,dim.inq.nc(soils_gridCoarse, "vertical")$length))/100
#thickness
thickness_i <- var.get.nc(soils_gridThickness, "hzthk", start = c(soilLon_i, soilLat_i,1), 
                          count = c(1,1,dim.inq.nc(soils_gridThickness, "vertical")$length))   
# units are in cm

bulkdensity_i <- var.get.nc(soils_gridDensity, "dbovendry", start = c(soilLon_i, soilLat_i,1), 
                            count = c(1,1,dim.inq.nc(soils_gridDensity, "vertical")$length)) # units = g/cm3

## get the depths also (the "vertical_bnds" dimension contains a matrix with 
# the upper and lower bounds of each depth band--we want the lower bounds)

depths_i <- var.get.nc(soils_gridThickness, "vertical_bnds")[2,]
##AES this part below is a test... see what other folks think about this...
# trim soils data so that there are not NAs (the data stops at the depth for which we have data)
# also get the depths for the layers included
depths_i <- depths_i[!is.na(clay_i)]
clay_i <- clay_i[!is.na(clay_i)] 
sand_i <- sand_i[!is.na(sand_i)]
silt_i <- silt_i[!is.na(silt_i)]
coarse_i <- coarse_i[!is.na(coarse_i)]
thickness_i <- thickness_i[!is.na(thickness_i)]
bulkdensity_i <- bulkdensity_i[!is.na(bulkdensity_i)]


