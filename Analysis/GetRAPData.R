#///////////////////
# Rangeland Analysis Platform (RAP) data aquisition and cleaning
# Alice Stears
# 6/5/24
#///////////////////


# load packages -----------------------------------------------------------

library(tidyverse)
library(sf)
library(terra)

# generate random points to sample RAP data with --------------------------
## first, get a shapefile of CONUS
CONUS <- st_read(dsn = "./data/CONUS_extent/", layer = "CONUS_boundary")
## use a mask to remove agricultural or developed area
# get the masks from land use data from LCMAP (used data from 2021, since it's likely the most exclusive...?)
# from: https://eros.usgs.gov/lcmap/apps/data-downloads
LCMAP <- terra::rast("./data/LCMAP/LCMAP_CU_2021_V13_LCPRI.tif")
# make the crs EPSG:4326
#crs(LCMAP) <- "EPSG:4326"
# reclassify so that 1 = raster cells that are developed (1) or cropland (2)  or water (5) and 0 = any other land use
LCMAP_use <- classify(LCMAP, rcl = matrix(c(1,2,3,4,5,6,7,8,1,1,0,0,1,0,0,0), nrow = 8))
terra::plot(LCMAP_use)
# use the raster to mask the shapefile ?

## randomly sample points (start with the same # of data points we have from field-sampled plots?)
