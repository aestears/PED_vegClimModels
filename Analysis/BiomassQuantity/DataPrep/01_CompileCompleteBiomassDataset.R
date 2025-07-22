#///////////////////
# Compiling the complete biomass dataset for use in subsequent modeling
# Alice Stears
# 07/14/2025
#///////////////////

# load packages -----------------------------------------------------------

library(tidyverse)

# load data ---------------------------------------------------------------

# # Get total biomass data from Spawn paper ---------------------------------
# spawnBiomass <- terra::rast("./Data_processed/BiomassQuantityData/Spawn2020_biomassRaster.tif") 
# # reproject the points to match the crs of the biomass data 
# 
# # Get total biomass data from GEDI ----------------------------------------
# # https://daac.ornl.gov/cgi-bin/dsviewer.pl?ds_id=2299
# # load raw data
# GEDIBiomass <- terra::rast("./Data_processed/BiomassQuantityData/GEDI_biomassRaster.tif") 
# 
# # Read in RAP biomass data ------------------------------------------------
# # units: Mg/hectare
# RAPbiomass <- terra::rast("./Data_processed/BiomassQuantityData/RAP_biomassRaster_totalHerb_Mg_Hect_averagedfrom2019to2021.tif") 
# # average from 2010 to 2020
# #RAPbiomass_avg <- mean(RAPbiomass$year_2010, RAPbiomass$year_2011, RAPbiomass$year_2012, RAPbiomass$year_2013, RAPbiomass$year_2014, RAPbiomass$year_2015, RAPbiomass$year_2016, RAPbiomass$year_2017, RAPbiomass$year_2018, RAPbiomass$year_2019, RAPbiomass$year_2020)
# RAPbiomass_avg <- RAPbiomass
# 
# # Read in FIA biomass data ------------------------------------------------
# FIAbiomass <- terra::rast("./Data_processed/BiomassQuantityData/FIA_biomassRaster.tif")
# # tree live biomass in Mg/hectare
# 

