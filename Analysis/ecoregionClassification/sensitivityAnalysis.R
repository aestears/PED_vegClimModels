## modules to load on Denali: 
## module load cray-R/4.1.2.0  gcc/11.2.0 gdal/3.0.4 proj/6.2.1 geos/3.8.1 cray-netcdf/4.8.1.1                    


# load packages -----------------------------------------------------------
# quick code chunk to calculate VPD
require(terra)
library(sf)
library(tidyverse)
library(tidyterra)
library(pdp)

# load data from bioclim --------------------------------------------------
# load model data 
source("./Analysis/VegComposition/ModelFitting/01_EcoregionClassification.R")
#list all files in folder # arbitrarily chose end-century, highest ssp
#(ssp_585)
# from CMCC-ESM2 model (could do another, this was just first on the list)
# these are monthly values averaged over a 20 year period from 2081 to 2100
# these data are at a 10 minute spatial resolution; each layer is a monthly value 
# https://www.worldclim.org/data/cmip6/cmip6_clim10m.html
## read in bioClim variables 
bioClim <- rast("./Data_raw/worldClim_climateVars/futureClimate_midCentury_ssp585/CMCC-ESM2/wc2.1_10m_bioc_CMCC-ESM2_ssp585_2041-2060.tif")

tmin <- rast("./Data_raw/worldClim_climateVars/futureClimate_midCentury_ssp585/CMCC-ESM2/wc2.1_10m_tmin_CMCC-ESM2_ssp585_2041-2060.tif")

tmax <- rast("./Data_raw/worldClim_climateVars/futureClimate_midCentury_ssp585/CMCC-ESM2/wc2.1_10m_tmax_CMCC-ESM2_ssp585_2041-2060.tif")

precip <-rast("./Data_raw/worldClim_climateVars/futureClimate_midCentury_ssp585/CMCC-ESM2/wc2.1_10m_prec_CMCC-ESM2_ssp585_2041-2060.tif")

# crop to CONUS extent
# get CONUS data
CONUS <- st_read("./Data_raw/CONUS_extent/", layer = "CONUS_boundary")
# change the crs of conus boundary shapefile
CONUS <- CONUS %>% 
  st_transform(crs(precip)) %>% 
  vect()

bioClim <- bioClim %>% 
  terra::crop(CONUS) %>% 
  terra::mask(CONUS) 

tmin <- tmin %>% 
  terra::crop(CONUS) %>% 
  terra::mask(CONUS) 

tmax <- tmax %>% 
  terra::crop(CONUS) %>% 
  terra::mask(CONUS) 

precip <- precip %>% 
  terra::crop(CONUS) %>% 
  terra::mask(CONUS) 

# calcultmaxPoints# calculate tmean 
tmean <- (tmin + tmax)/2

# make set of points to subsample rasters
sampPoints <- precip %>% 
  terra::crds() %>% 
  as.data.frame() 

ggplot(sampPoints) + 
  geom_point(aes(x,y))


# get ecoregion extents ---------------------------------------------------
# get level 1 ecoregions shapefiles
regions <- sf::st_read(dsn = "./Data_raw/Level1Ecoregions/", layer = "NA_CEC_Eco_Level1") %>% 
  sf::st_transform(crs(bioClim))
# trim to CONUS extent 
sf_use_s2(FALSE)

regionsTemp <- regions %>% 
  filter(NA_L1NAME != "WATER" &
           NA_L1NAME != "TAIGA" &
           NA_L1NAME != "TUNDRA" &
           #NA_L1NAME != "ARCTIC CORDILLERA" &
           NA_L1NAME != "HUDSON PLAIN" &
           #NA_L1NAME != "MARINE WEST COAST FOREST"  &
           NA_L1NAME != "TROPICAL DRY FORESTS")
  
regionsCONUS <- st_as_sf(CONUS)%>% st_make_valid() %>% 
  st_intersection(regionsTemp %>% st_make_valid(), sparse = TRUE)

# make lookup table
ecoregLU <- data.frame("NA_L1NAME" = c( "NORTH AMERICAN DESERTS"  ,"MEDITERRANEAN CALIFORNIA" ,"SOUTHERN SEMIARID HIGHLANDS",    
                                        "TEMPERATE SIERRAS"  ,"TROPICAL WET FORESTS","NORTHERN FORESTS"  ,    
                                        "NORTHWESTERN FORESTED MOUNTAINS" ,"MARINE WEST COAST FOREST" ,"EASTERN TEMPERATE FORESTS"  ,    
                                         "GREAT PLAINS"), 
                       "ecoregion" = c("grass/shrub", "grass/shrub", "grass/shrub", 
                                       "forest", "forest", "forest", 
                                       "forest", "forest", "forest", 
                                       "grass/shrub"))
regionsCONUS <- regionsCONUS %>% 
  left_join(ecoregLU)

grassShrub <- regionsCONUS %>% 
  filter(ecoregion == "grass/shrub") %>% 
  st_union()

forest <- regionsCONUS %>% 
  filter(ecoregion == "forest") %>% 
  st_union()

ecoregionsSF_temp <- rbind(grassShrub, forest)
ecoregionsSF <- data.frame("ecoregion" = c("grassShrub", "forest"), 
                           "geometry" = c(grassShrub, forest)) %>% 
  st_as_sf()

ecoregionsSF2 <- sf::st_simplify(ecoregionsSF, preserveTopology = TRUE, dTolerance = .1)
#mapview(ecoregionsSF2)

# calculate VPD 95th percentile (from contemporary data) -------------------------------------------------------
soilRastTemp <- readRDS("./Data_processed/SoilsRaster.rds") 
soilRast <- soilRastTemp %>% 
  terra::project(crs(bioClim))

# get data averaged over contemporary 30 year period
maxVPD_95_Rast <- modDat_use %>% 
  dplyr::select(annVPD_max_95percentile_30yr, Long, Lat) %>% 
  terra::vect(geom = c("Long", "Lat"), crs =     crs("PROJCRS[\"unnamed\",\n BASEGEOGCRS[\"unknown\",\n DATUM[\"unknown\",\nELLIPSOID[\"Spheroid\",6378137,298.257223563,\n LENGTHUNIT[\"metre\",1,\n ID[\"EPSG\",9001]]]],\n PRIMEM[\"Greenwich\",0,\n ANGLEUNIT[\"degree\",0.0174532925199433,\n ID[\"EPSG\",9122]]]],\n CONVERSION[\"Lambert Conic Conformal (2SP)\",\n METHOD[\"Lambert Conic Conformal (2SP)\",\n ID[\"EPSG\",9802]],\nPARAMETER[\"Latitude of false origin\",42.5,\nANGLEUNIT[\"degree\",0.0174532925199433],\n ID[\"EPSG\",8821]],\n PARAMETER[\"Longitude of false origin\",-100,\n ANGLEUNIT[\"degree\",0.0174532925199433],\n ID[\"EPSG\",8822]],\n PARAMETER[\"Latitude of 1st standard parallel\",25,\n ANGLEUNIT[\"degree\",0.0174532925199433],\n ID[\"EPSG\",8823]],\n PARAMETER[\"Latitude of 2nd standard parallel\",60,\n ANGLEUNIT[\"degree\",0.0174532925199433],\n ID[\"EPSG\",8824]],\n PARAMETER[\"Easting at false origin\",0,\n LENGTHUNIT[\"metre\",1],\n ID[\"EPSG\",8826]],\n PARAMETER[\"Northing at false origin\",0,\n LENGTHUNIT[\"metre\",1],\n ID[\"EPSG\",8827]]],\n CS[Cartesian,2],\nAXIS[\"easting\",east,\n ORDER[1],\nLENGTHUNIT[\"metre\",1,\n ID[\"EPSG\",9001]]],\n AXIS[\"northing\",north,\nORDER[2],\nLENGTHUNIT[\"metre\",1,\n ID[\"EPSG\",9001]]]]")) %>% 
  terra::rasterize(y = ( soilRast %>% terra::project("PROJCRS[\"unnamed\",\n BASEGEOGCRS[\"unknown\",\n DATUM[\"unknown\",\nELLIPSOID[\"Spheroid\",6378137,298.257223563,\n LENGTHUNIT[\"metre\",1,\n ID[\"EPSG\",9001]]]],\n PRIMEM[\"Greenwich\",0,\n ANGLEUNIT[\"degree\",0.0174532925199433,\n ID[\"EPSG\",9122]]]],\n CONVERSION[\"Lambert Conic Conformal (2SP)\",\n METHOD[\"Lambert Conic Conformal (2SP)\",\n ID[\"EPSG\",9802]],\nPARAMETER[\"Latitude of false origin\",42.5,\nANGLEUNIT[\"degree\",0.0174532925199433],\n ID[\"EPSG\",8821]],\n PARAMETER[\"Longitude of false origin\",-100,\n ANGLEUNIT[\"degree\",0.0174532925199433],\n ID[\"EPSG\",8822]],\n PARAMETER[\"Latitude of 1st standard parallel\",25,\n ANGLEUNIT[\"degree\",0.0174532925199433],\n ID[\"EPSG\",8823]],\n PARAMETER[\"Latitude of 2nd standard parallel\",60,\n ANGLEUNIT[\"degree\",0.0174532925199433],\n ID[\"EPSG\",8824]],\n PARAMETER[\"Easting at false origin\",0,\n LENGTHUNIT[\"metre\",1],\n ID[\"EPSG\",8826]],\n PARAMETER[\"Northing at false origin\",0,\n LENGTHUNIT[\"metre\",1],\n ID[\"EPSG\",8827]]],\n CS[Cartesian,2],\nAXIS[\"easting\",east,\n ORDER[1],\nLENGTHUNIT[\"metre\",1,\n ID[\"EPSG\",9001]]],\n AXIS[\"northing\",north,\nORDER[2],\nLENGTHUNIT[\"metre\",1,\n ID[\"EPSG\",9001]]]]")), 
                   field =  "annVPD_max_95percentile_30yr", fun = mean, na.rm = TRUE) %>% 
  terra::project(crs(bioClim))

maxVPD_95_points <- maxVPD_95_Rast %>% 
  terra::extract(sampPoints, xy = TRUE)

ggplot(maxVPD_95_points) + 
  geom_point(aes(x, y, color = mean))


# precip of driest month --------------------------------------------------
# this is bioclim variable 14
precip_driestMonth <- bioClim$bio14
plot(precip_driestMonth)


## get precip driest month as points
precip_driestMonth_points <- precip_driestMonth %>% 
  terra::extract(sampPoints, xy = TRUE)

# precip of wettest month --------------------------------------------------
# this is bioclim variable 13
precip_wettestMonth <- bioClim$bio13
plot(precip_wettestMonth)


## get precip driest month as points
precip_wettestMonth_points <- precip_wettestMonth %>% 
  terra::extract(sampPoints, xy = TRUE)

# temp of warmest month --------------------------------------------------
# this is bioclim variable 5
temp_warmestMonth <- bioClim$bio05
plot(temp_warmestMonth)


## get precip driest month as points
temp_warmestMonth_points <- temp_warmestMonth %>% 
  terra::extract(sampPoints, xy = TRUE)

# temp of coldest month --------------------------------------------------
# this is bioclim variable 6
temp_coldestMonth <- bioClim$bio06
plot(temp_coldestMonth)


## get precip driest month as points
temp_coldestMonth_points <- temp_coldestMonth %>% 
  terra::extract(sampPoints, xy = TRUE)

# isothermality --------------------------------------------------
# this is bioclim variable 3
isotherm <- bioClim$bio03
plot(isotherm)


## get isothermality as points
isotherm_points <- isotherm %>% 
  terra::extract(sampPoints, xy = TRUE)

# correlation of monthly temp and precip ----------------------------------

# get data averaged over contemporary 30 year period
ptCorrRast <- modDat_use %>% 
  dplyr::select(PrecipTempCorr_meanAnnAvg_30yr, Long, Lat) %>% 
  terra::vect(geom = c("Long", "Lat"), crs =     crs("PROJCRS[\"unnamed\",\n BASEGEOGCRS[\"unknown\",\n DATUM[\"unknown\",\nELLIPSOID[\"Spheroid\",6378137,298.257223563,\n LENGTHUNIT[\"metre\",1,\n ID[\"EPSG\",9001]]]],\n PRIMEM[\"Greenwich\",0,\n ANGLEUNIT[\"degree\",0.0174532925199433,\n ID[\"EPSG\",9122]]]],\n CONVERSION[\"Lambert Conic Conformal (2SP)\",\n METHOD[\"Lambert Conic Conformal (2SP)\",\n ID[\"EPSG\",9802]],\nPARAMETER[\"Latitude of false origin\",42.5,\nANGLEUNIT[\"degree\",0.0174532925199433],\n ID[\"EPSG\",8821]],\n PARAMETER[\"Longitude of false origin\",-100,\n ANGLEUNIT[\"degree\",0.0174532925199433],\n ID[\"EPSG\",8822]],\n PARAMETER[\"Latitude of 1st standard parallel\",25,\n ANGLEUNIT[\"degree\",0.0174532925199433],\n ID[\"EPSG\",8823]],\n PARAMETER[\"Latitude of 2nd standard parallel\",60,\n ANGLEUNIT[\"degree\",0.0174532925199433],\n ID[\"EPSG\",8824]],\n PARAMETER[\"Easting at false origin\",0,\n LENGTHUNIT[\"metre\",1],\n ID[\"EPSG\",8826]],\n PARAMETER[\"Northing at false origin\",0,\n LENGTHUNIT[\"metre\",1],\n ID[\"EPSG\",8827]]],\n CS[Cartesian,2],\nAXIS[\"easting\",east,\n ORDER[1],\nLENGTHUNIT[\"metre\",1,\n ID[\"EPSG\",9001]]],\n AXIS[\"northing\",north,\nORDER[2],\nLENGTHUNIT[\"metre\",1,\n ID[\"EPSG\",9001]]]]")) %>% 
  terra::rasterize(y = ( soilRast %>% terra::project("PROJCRS[\"unnamed\",\n BASEGEOGCRS[\"unknown\",\n DATUM[\"unknown\",\nELLIPSOID[\"Spheroid\",6378137,298.257223563,\n LENGTHUNIT[\"metre\",1,\n ID[\"EPSG\",9001]]]],\n PRIMEM[\"Greenwich\",0,\n ANGLEUNIT[\"degree\",0.0174532925199433,\n ID[\"EPSG\",9122]]]],\n CONVERSION[\"Lambert Conic Conformal (2SP)\",\n METHOD[\"Lambert Conic Conformal (2SP)\",\n ID[\"EPSG\",9802]],\nPARAMETER[\"Latitude of false origin\",42.5,\nANGLEUNIT[\"degree\",0.0174532925199433],\n ID[\"EPSG\",8821]],\n PARAMETER[\"Longitude of false origin\",-100,\n ANGLEUNIT[\"degree\",0.0174532925199433],\n ID[\"EPSG\",8822]],\n PARAMETER[\"Latitude of 1st standard parallel\",25,\n ANGLEUNIT[\"degree\",0.0174532925199433],\n ID[\"EPSG\",8823]],\n PARAMETER[\"Latitude of 2nd standard parallel\",60,\n ANGLEUNIT[\"degree\",0.0174532925199433],\n ID[\"EPSG\",8824]],\n PARAMETER[\"Easting at false origin\",0,\n LENGTHUNIT[\"metre\",1],\n ID[\"EPSG\",8826]],\n PARAMETER[\"Northing at false origin\",0,\n LENGTHUNIT[\"metre\",1],\n ID[\"EPSG\",8827]]],\n CS[Cartesian,2],\nAXIS[\"easting\",east,\n ORDER[1],\nLENGTHUNIT[\"metre\",1,\n ID[\"EPSG\",9001]]],\n AXIS[\"northing\",north,\nORDER[2],\nLENGTHUNIT[\"metre\",1,\n ID[\"EPSG\",9001]]]]")), 
                   field =  "PrecipTempCorr_meanAnnAvg_30yr", fun = mean, na.rm = TRUE) %>% 
  terra::project(crs(bioClim))

ptCorr_points <- ptCorrRast %>% 
  terra::extract(sampPoints, xy = TRUE)

ggplot(ptCorr_points) + 
  geom_point(aes(x, y, color = mean))


# get mean annual precip --------------------------------------------------
precip_points <- precip %>% 
  terra::extract(sampPoints, xy = TRUE) %>% 
  rowwise() %>% 
  mutate(MAP = mean(wc2.1_2.5m_prec_01:wc2.1_2.5m_prec_12))

ggplot(precip_points) + 
  geom_point(aes(x, y, col = MAP))

#  get Organic carbon for the appropriate points from soils data ------------
# otherwise, read in the file
soilRast <- readRDS("./Data_processed/SoilsRaster.rds") %>% 
  terra::project(crs(bioClim))

# sample soils data for  points consistent w/ the other dataset

soilPoints <- soilRast %>% 
  terra::extract(sampPoints, xy = TRUE)

# get organic carbon in the first 3 cm 
soilCarbon <- soilPoints %>% 
  dplyr::select(organicCarbonPerc_2cm, x, y)

ggplot(soilCarbon) + 
  geom_point(aes(x, y, col = organicCarbonPerc_2cm))

#  get surface clay for the appropriate points from soils data ------------
# sample soils data for  points consistent w/ the other dataset

# use "soilPoints" from above
# get clay in the first 3 cm 
soilClay <- soilPoints %>% 
  dplyr::select(clayPerc_2cm, x, y) %>% 
  rename(surfaceClay_perc = clayPerc_2cm)

ggplot(soilClay) + 
  geom_point(aes(x, y, col = surfaceClay_perc))

#  get soil depth for the appropriate points from soils data ------------
# otherwise, read in the file

# sample soils data for  points consistent w/ the other dataset


# get organic carbon in the first 3 cm 
soilDepth <- soilPoints %>% 
  # calculate soil depth 
  mutate( soilDepth = pmap_dbl(.[c("horizonThickness_cm_2cm" , "horizonThickness_cm_7cm" , "horizonThickness_cm_15cm" , 
                                   "horizonThickness_cm_25cm" , "horizonThickness_cm_35cm" , "horizonThickness_cm_50cm" , 
                                   "horizonThickness_cm_70cm" , "horizonThickness_cm_90cm" , "horizonThickness_cm_125cm" , 
                                   "horizonThickness_cm_176cm")], sum, na.rm = TRUE)) %>% 
  # b/c I used na.rm = TRUE (some locations don't have a value all the way
  # down), locations without any data that should actually be NA have 0
  # values... fix this!
  rowwise() %>% 
  mutate(soilDepth = replace(soilDepth, is.na(horizonThickness_cm_2cm), values = NA)) 

ggplot(soilDepth) + 
  geom_point(aes(x, y, col = soilDepth))


#  get sand averaged across soil profile depth for the appropriate points from soils data ------------
# otherwise, read in the file

# sample soils data for  points consistent w/ the other dataset
# define function for coarse fraction averaged across depth 
sand_Fun <- function(horizonThickness_cm_2cm , horizonThickness_cm_7cm , horizonThickness_cm_15cm , 
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

# get sand
sand <- soilDepth %>% 
  # calculate avg. sand fraction across the depth of the soil profile
  rowwise() %>% 
  mutate(avgSandPerc_acrossDepth = sand_Fun(horizonThickness_cm_2cm , horizonThickness_cm_7cm , horizonThickness_cm_15cm , 
                                                horizonThickness_cm_25cm , horizonThickness_cm_35cm , horizonThickness_cm_50cm , 
                                                horizonThickness_cm_70cm , horizonThickness_cm_90cm , horizonThickness_cm_125cm , 
                                                horizonThickness_cm_176cm, sandPerc_2cm, sandPerc_7cm , sandPerc_15cm,
                                                sandPerc_25cm , sandPerc_35cm, sandPerc_50cm , sandPerc_70cm, sandPerc_90cm ,
                                                sandPerc_125cm,sandPerc_176cm, soilDepth))
# # b/c I used na.rm = TRUE (some locations don't have a value all the way
# # down), locations without any data that should actually be NA have 0
# # values... fix this!
# select(x, y, avgCoarsePerc_acrossDepth)

ggplot(sand) + 
  geom_point(aes(x, y, col = avgSandPerc_acrossDepth))


#  get coarse fraction for the appropriate points from soils data ------------
# otherwise, read in the file

# sample soils data for  points consistent w/ the other dataset
# define function for coarse fraction averaged across depth 
coarse_Fun <- function(horizonThickness_cm_2cm , horizonThickness_cm_7cm , horizonThickness_cm_15cm , 
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

# get organic carbon in the first 3 cm 
coarse <- soilDepth %>% 
  # calculate avg. coarse fraction across the depth of the soil profile
  rowwise() %>% 
  mutate(avgCoarsePerc_acrossDepth = coarse_Fun(horizonThickness_cm_2cm , horizonThickness_cm_7cm , horizonThickness_cm_15cm , 
                                                horizonThickness_cm_25cm , horizonThickness_cm_35cm , horizonThickness_cm_50cm , 
                                                horizonThickness_cm_70cm , horizonThickness_cm_90cm , horizonThickness_cm_125cm , 
                                                horizonThickness_cm_176cm, coarsePerc_2cm, coarsePerc_7cm , coarsePerc_15cm,
                                                coarsePerc_25cm , coarsePerc_35cm, coarsePerc_50cm , coarsePerc_70cm, coarsePerc_90cm ,
                                                coarsePerc_125cm,coarsePerc_176cm, soilDepth))
    # # b/c I used na.rm = TRUE (some locations don't have a value all the way
  # # down), locations without any data that should actually be NA have 0
  # # values... fix this!
  # select(x, y, avgCoarsePerc_acrossDepth)

ggplot(coarse) + 
  geom_point(aes(x, y, col = avgCoarsePerc_acrossDepth))

# Put all input variables together ----------------------------------------
sensDat <- (precip_driestMonth_points %>% dplyr::select(bio14))  %>% 
  cbind(ptCorr_points %>% dplyr::select(mean) %>% rename(PrecipTempCorr_meanAnnAvg_30yr = mean)) %>% #contemporary data 
  cbind(soilCarbon %>% dplyr::select(organicCarbonPerc_2cm)) %>% #contemporary data 
  cbind(maxVPD_95_points %>% dplyr::select(mean) %>% rename(annVPD_max_95percentile_30yr = mean)) %>% #contemporary data 
  cbind(coarse %>% dplyr::select(avgCoarsePerc_acrossDepth)) %>% #contemporary data 
  cbind(precip_points %>% dplyr::select(MAP)) %>% 
  cbind(soilClay %>% dplyr::select(surfaceClay_perc)) %>% #contemporary data
  cbind(soilDepth %>% dplyr::select(soilDepth)) %>% #contemporary data 
  cbind(sand %>% dplyr::select(avgSandPerc_acrossDepth)) %>%   # contemporary data
  cbind(isotherm_points %>% dplyr::select(bio03) %>% rename(isothermality_meanAnnAvg_30yr = bio03)) %>% 
  cbind(precip_wettestMonth_points %>% dplyr::select(bio13) %>% rename(precip_wettestMonth_meanAnnAvg_30yr = bio13)) %>% 
  cbind(temp_warmestMonth_points %>% dplyr::select(bio05) %>% rename(T_warmestMonth_meanAnnAvg_30yr = bio05)) %>% 
  cbind(temp_coldestMonth_points %>% dplyr::select(bio06) %>% rename(T_coldestMonth_meanAnnAvg_30yr = bio06)) %>% 
  cbind(sampPoints) %>% 
  dplyr::rename(avgOrganicCarbonPerc_0_3cm = organicCarbonPerc_2cm,
         precip_driestMonth_meanAnnAvg_30yr = bio14,
         prcp_meanAnnTotal_30yr = MAP,
         Long = x, 
         Lat = y
         ) %>% 
  drop_na()

## precip of driest month 
(comparePrecipDriest <- ggplot() + 
  geom_density(data = sensDat, aes(x = precip_driestMonth_meanAnnAvg_30yr), col = "red") + 
  geom_density(data = modDat_use, aes(precip_driestMonth_meanAnnAvg_30yr), col = "blue") + 
  ggtitle("Precip in driest month", subtitle = "red = end of century, blue = current"))
# predicted values 
predsTemp <- (sensDat %>% 
            terra::vect(geom = c("Long", "Lat"), crs = crs(ecoregionsSF2)) %>% 
            terra::rasterize(y = bioClim
                             , field = "precip_driestMonth_meanAnnAvg_30yr"))
# contemporary values
contempTemp <- (modDat_use %>% 
              terra::vect(geom = c("Long", "Lat"), crs = crs(soilRastTemp)) %>% 
              terra::project(crs(ptCorrRast)) %>% 
              terra::rasterize(y = bioClim, field = "precip_driestMonth_meanAnnAvg_30yr"))
plotDatTemp <- predsTemp - contempTemp
 
(comparePrecipDriest_2 <- ggplot() +
  geom_spatraster(data = plotDatTemp) + 
  guides(fill = guide_legend("Forecast - \n Contemporary")) + 
  ggtitle("Precip of the Driest Month"))

## precip of wettest month 
(comparePrecipWettest <- ggplot() + 
    geom_density(data = sensDat, aes(x = precip_wettestMonth_meanAnnAvg_30yr), col = "red") + 
    geom_density(data = modDat_use, aes(precip_wettestMonth_meanAnnAvg_30yr), col = "blue") + 
    ggtitle("Precip in wettest month", subtitle = "red = end of century, blue = current"))
# predicted values 
predsTemp <- (sensDat %>% 
                terra::vect(geom = c("Long", "Lat"), crs = crs(ecoregionsSF2)) %>% 
                terra::rasterize(y = bioClim
                                 , field = "precip_wettestMonth_meanAnnAvg_30yr"))
# contemporary values
contempTemp <- (modDat_use %>% 
                  terra::vect(geom = c("Long", "Lat"), crs = crs(soilRastTemp)) %>% 
                  terra::project(crs(ptCorrRast)) %>% 
                  terra::rasterize(y = bioClim, field = "precip_wettestMonth_meanAnnAvg_30yr"))
plotDatTemp <- predsTemp - contempTemp
(comparePrecipWettest_2 <- ggplot() +
    geom_spatraster(data = plotDatTemp) + 
    guides(fill = guide_legend("Forecast - \n Contemporary")) + 
    ggtitle("Precip of the Wettest Month"))

## temp of warmest month 
(compareTempWarmest <- ggplot() +
  geom_density(data = sensDat, aes(x = T_warmestMonth_meanAnnAvg_30yr), col = "red") +
  geom_density(data = modDat_use, aes(T_warmestMonth_meanAnnAvg_30yr), col = "blue") +
  ggtitle("Temp of Warmest Month", subtitle = "red = end of century, blue = current"))
# predicted values 
predsTemp <- (sensDat %>% 
                terra::vect(geom = c("Long", "Lat"), crs = crs(ecoregionsSF2)) %>% 
                terra::rasterize(y = bioClim
                                 , field = "T_warmestMonth_meanAnnAvg_30yr"))
# contemporary values
contempTemp <- (modDat_use %>% 
                  terra::vect(geom = c("Long", "Lat"), crs = crs(soilRastTemp)) %>% 
                  terra::project(crs(ptCorrRast)) %>% 
                  terra::rasterize(y = bioClim, field = "T_warmestMonth_meanAnnAvg_30yr"))
plotDatTemp <- predsTemp - contempTemp
(compareTempWarmest_2 <- ggplot() +
    geom_spatraster(data = plotDatTemp) + 
    guides(fill = guide_legend("Forecast - \n Contemporary")) + 
    ggtitle("Temp of the Warmest Month"))

## temp of coldest month 
(compareTempColdest <- ggplot() +
    geom_density(data = sensDat, aes(x = T_coldestMonth_meanAnnAvg_30yr), col = "red") +
    geom_density(data = modDat_use, aes(T_coldestMonth_meanAnnAvg_30yr), col = "blue") +
    ggtitle("Temp of Coldest Month", subtitle = "red = end of century, blue = current"))
# predicted values 
predsTemp <- (sensDat %>% 
                terra::vect(geom = c("Long", "Lat"), crs = crs(ecoregionsSF2)) %>% 
                terra::rasterize(y = bioClim
                                 , field = "T_coldestMonth_meanAnnAvg_30yr"))
# contemporary values
contempTemp <- (modDat_use %>% 
                  terra::vect(geom = c("Long", "Lat"), crs = crs(soilRastTemp)) %>% 
                  terra::project(crs(ptCorrRast)) %>% 
                  terra::rasterize(y = bioClim, field = "T_coldestMonth_meanAnnAvg_30yr"))
plotDatTemp <- predsTemp - contempTemp
(compareTempColdest_2 <- ggplot() +
    geom_spatraster(data = plotDatTemp) + 
    guides(fill = guide_legend("Forecast - \n Contemporary")) + 
    ggtitle("Temp of the Coldest Month"))

## isothermality
(compareIsothermality <- ggplot() + 
    geom_density(data = sensDat, aes(x = isothermality_meanAnnAvg_30yr), col = "red") + 
    geom_density(data = modDat_use, aes(isothermality_meanAnnAvg_30yr), col = "blue") + 
    ggtitle("isothermality", subtitle = "red = end of century, blue = current"))
# predicted values 
predsTemp <- (sensDat %>% 
                terra::vect(geom = c("Long", "Lat"), crs = crs(ecoregionsSF2)) %>% 
                terra::rasterize(y = bioClim
                                 , field = "isothermality_meanAnnAvg_30yr"))
# contemporary values
contempTemp <- (modDat_use %>% 
                  terra::vect(geom = c("Long", "Lat"), crs = crs(soilRastTemp)) %>% 
                  terra::project(crs(ptCorrRast)) %>% 
                  terra::rasterize(y = bioClim, field = "isothermality_meanAnnAvg_30yr"))
plotDatTemp <- predsTemp - contempTemp
(compareIsothermality_2 <- ggplot() +
    geom_spatraster(data = plotDatTemp) + 
    guides(fill = guide_legend("Forecast - \n Contemporary")) + 
    ggtitle("Isothermality"))

bitmap(file = "./Figures/EcoRegionModelFigures/RegressionModel_2EcoregionsNoVPD_PredContempComparison.bmp", 
       width = 16, height = 20, res = 200)

  patchwork::plot_layout((comparePrecipDriest+
                       comparePrecipDriest_2)/
                       (comparePrecipWettest+ 
                       comparePrecipWettest_2)/
                       (compareIsothermality+
                       compareIsothermality_2)/ 
                       (compareTempWarmest+ 
                       compareTempWarmest_2)/
                       (compareTempColdest+
                       compareTempColdest_2)) %>% 
  patchwork::plot_annotation(title = "Mid-century climate values (2041-2060) from CMCC-ESM2 model, ssp 585")

dev.off()

# read in No VPD model w/ 2 ecoregions ------------------------------------------------
# the model is called "testMod_2" 
# newRegionFact ~ precip_wettestMonth_meanAnnAvg_30yr  + 
# precip_driestMonth_meanAnnAvg_30yr    +
#   PrecipTempCorr_meanAnnAvg_30yr       +
#   isothermality_meanAnnAvg_30yr        +
#   annVPD_max_95percentile_30yr         +  
#   soilDepth                             +
#   surfaceClay_perc                     + 
#   avgCoarsePerc_acrossDepth            +  avgOrganicCarbonPerc_0_3cm

# predict categories based on this simple model

regModPreds_2_df <- sensDat %>%
  cbind("newRegion_pred" = predict(testMod_2, newdata = sensDat, "response")) %>%  rowwise() %>% 
  mutate(newRegion_group = newRegion_pred) %>% 
  mutate(newRegion_group = replace(newRegion_group, newRegion_pred<.5, "dryShrubGrass"), 
         newRegion_group = replace(newRegion_group, newRegion_pred>.5, "forest")) 

regModPreds_2 <- regModPreds_2_df %>% 
  terra::vect(geom = c("Long", "Lat"), crs = crs(ecoregionsSF2)) %>% 
  terra::rasterize(y = 
                     terra::aggregate(soilRastTemp %>% terra::project(bioClim) , fact = 1, fun = mean)
                   , field = "newRegion_pred")
  

#//
(regMod2_PredMAP <- ggplot() +
    geom_spatraster(data = regModPreds_2) +
    # geom_point(aes(#Long, Lat, 
    #   col = newRegion_pred)) +
    ggtitle("model-predicted ecoregion classification -- fit to a combination of contemporary data and data from 2041-2060 \nfrom CMCC-ESM2 model, ssp 585 ",
            subtitle = paste0("model: ecoregion ~ temp_warmestMonth + temp_coldestMonth + precip_wettestMonth +
            precip_driestMonth + precipTempCorr + isothermality + soilDepth + % sand + % coarse + soil carbon
            *Black lines indicate contemporary ecoregion extent")) +
    #scale_fill_discrete(type = c("#bf812d","#35978f")) +
    scale_fill_distiller(type = "div", palette = 1, direction = 1) +
    geom_sf(data = ecoregionsSF2, color = "black", fill = NA, lwd = 1.25) + 
    lims(x = c(-130, -65), y = c(24, 50))
   # geom_sf(data = ecoregionsSF2, aes(fill = NULL))
)

# ggplot(data = ecoregionsSF2) + 
#   geom_sf()

# predictions w/ mix of true forecasted values and 'faked' forecasts --------

#"faked" meaning that for the variables not available from worldClim, I increased or decreased them uniformly
sensDat_2b <- sensDat %>% 
  mutate(PrecipTempCorr_meanAnnAvg_30yr = PrecipTempCorr_meanAnnAvg_30yr*1.1,
         avgOrganicCarbonPerc_0_3cm = avgOrganicCarbonPerc_0_3cm*1.1, 
         annVPD_max_95percentile_30yr = annVPD_max_95percentile_30yr*1.1, 
         avgCoarsePerc_acrossDepth = avgCoarsePerc_acrossDepth*1.1, 
         surfaceClay_perc = surfaceClay_perc*1.1,
         soilDepth = soilDepth*1.1,
         precip_driestMonth_meanAnnAvg_30yr = precip_driestMonth_meanAnnAvg_30yr*1.1,
         prcp_meanAnnTotal_30yr = prcp_meanAnnTotal_30yr*1.1,
         avgSandPerc_acrossDepth = avgSandPerc_acrossDepth*1.1,
         isothermality_meanAnnAvg_30yr = isothermality_meanAnnAvg_30yr*1.1,
         precip_wettestMonth_meanAnnAvg_30yr = precip_wettestMonth_meanAnnAvg_30yr*1.1,
         T_warmestMonth_meanAnnAvg_30yr = T_warmestMonth_meanAnnAvg_30yr*1.1,
         T_coldestMonth_meanAnnAvg_30yr = T_coldestMonth_meanAnnAvg_30yr*1.1
         ) 


regModPreds_2b <- sensDat_2b %>%
  cbind("newRegion_pred" = predict(testMod_2, newdata = sensDat_2b, "response")) %>%  rowwise() %>% 
  mutate(newRegion_group = newRegion_pred) %>% 
  mutate(newRegion_group = replace(newRegion_group, newRegion_pred<.5, "dryShrubGrass"), 
         newRegion_group = replace(newRegion_group, newRegion_pred>.5, "forest")) %>% 
  terra::vect(geom = c("Long", "Lat"), crs = crs(ecoregionsSF2)) %>% 
  terra::rasterize(y = 
                     terra::aggregate(soilRastTemp %>% terra::project(bioClim) , fact = 1, fun = mean)
                   , field = "newRegion_pred")

#//
(regMod2b_PredMAP <- ggplot() +
    geom_spatraster(data = regModPreds_2b) +
    # geom_point(aes(#Long, Lat, 
    #   col = newRegion_pred)) +
    ggtitle("model-predicted ecoregion classification -- fit to a combination of contemporary data *(increased by 10%)* and data from 2041-2060 \nfrom CMCC-ESM2 model, ssp 585 ",
            subtitle = paste0("model: ecoregion ~ temp_warmestMonth + temp_coldestMonth + precip_wettestMonth +
            precip_driestMonth + precipTempCorr + isothermality + soilDepth + % sand + % coarse + soil carbon
            *Black lines indicate contemporary ecoregion extent")) +
    #scale_fill_discrete(type = c("#bf812d","#35978f")) +
    scale_fill_distiller(type = "div", palette = 1, direction = 1) +
    geom_sf(data = ecoregionsSF2, color = "black", fill = NA, lwd = 1.25) + 
    lims(x = c(-130, -65), y = c(24, 50))
)

# another try 

#"faked" meaning that for the variables not available from worldClim, I increased or decreased them uniformly
sensDat_2c <- sensDat %>% 
  mutate(PrecipTempCorr_meanAnnAvg_30yr = PrecipTempCorr_meanAnnAvg_30yr*.9,
         avgOrganicCarbonPerc_0_3cm = avgOrganicCarbonPerc_0_3cm*.9, 
         annVPD_max_95percentile_30yr = annVPD_max_95percentile_30yr*.9, 
         avgCoarsePerc_acrossDepth = avgCoarsePerc_acrossDepth*.9, 
         surfaceClay_perc = surfaceClay_perc*.9,
         soilDepth = soilDepth*.9,
         precip_driestMonth_meanAnnAvg_30yr = precip_driestMonth_meanAnnAvg_30yr*.9,
         prcp_meanAnnTotal_30yr = prcp_meanAnnTotal_30yr*.9,
         avgSandPerc_acrossDepth = avgSandPerc_acrossDepth*.9,
         isothermality_meanAnnAvg_30yr = isothermality_meanAnnAvg_30yr*.9,
         precip_wettestMonth_meanAnnAvg_30yr = precip_wettestMonth_meanAnnAvg_30yr*.9,
         T_warmestMonth_meanAnnAvg_30yr = T_warmestMonth_meanAnnAvg_30yr*.9,
         T_coldestMonth_meanAnnAvg_30yr = T_coldestMonth_meanAnnAvg_30yr*.9
  ) 


regModPreds_2c <- sensDat_2c %>%
  cbind("newRegion_pred" = predict(testMod_2, newdata = sensDat_2c, "response")) %>%  rowwise() %>% 
  mutate(newRegion_group = newRegion_pred) %>% 
  mutate(newRegion_group = replace(newRegion_group, newRegion_pred<.5, "dryShrubGrass"), 
         newRegion_group = replace(newRegion_group, newRegion_pred>.5, "forest")) %>% 
  terra::vect(geom = c("Long", "Lat"), crs = crs(ecoregionsSF2)) %>% 
  terra::rasterize(y = 
                     terra::aggregate(soilRastTemp %>% terra::project(bioClim) , fact = 1, fun = mean)
                   , field = "newRegion_pred")


#//
(regMod2c_PredMAP <- ggplot() +
    geom_spatraster(data = regModPreds_2c) +
    # geom_point(aes(#Long, Lat, 
    #   col = newRegion_pred)) +
    ggtitle("model-predicted ecoregion classification -- fit to a combination of contemporary data *(decreased by 10%)* and data from 2041-2060 \nfrom CMCC-ESM2 model, ssp 585 ",
            subtitle = paste0("model: ecoregion ~ temp_warmestMonth + temp_coldestMonth + precip_wettestMonth +
            precip_driestMonth + precipTempCorr + isothermality + soilDepth + % sand + % coarse + soil carbon
            *Black lines indicate contemporary ecoregion extent")) +
    #scale_fill_discrete(type = c("#bf812d","#35978f")) +
    scale_fill_distiller(type = "div", palette = 1, direction = 1) +
    geom_sf(data = ecoregionsSF2, color = "black", fill = NA, lwd = 1.25) + 
    lims(x = c(-130, -65), y = c(24, 50))
  )

(maps <- ggarrange(regMod2_PredMAP, regMod2b_PredMAP, regMod2c_PredMAP, ncol = 1)
)
# sensitivity analysis  ---------------------------------------------------

# use contemporary dataset (modDat_testNew)
# names of variables to perturb: 

# T_warmestMonth_meanAnnAvg_30yr       T_coldestMonth_meanAnnAvg_30yr  
# precip_wettestMonth_meanAnnAvg_30yr   precip_driestMonth_meanAnnAvg_30yr       PrecipTempCorr_meanAnnAvg_30yr  
# isothermality_meanAnnAvg_30yr                            soilDepth              avgSandPerc_acrossDepth  
# avgCoarsePerc_acrossDepth           avgOrganicCarbonPerc_0_3cm 

predictors <- c( "T_warmestMonth_meanAnnAvg_30yr"  , 
                 "T_coldestMonth_meanAnnAvg_30yr"    ,
                 "precip_wettestMonth_meanAnnAvg_30yr"       ,
                 "precip_driestMonth_meanAnnAvg_30yr"        ,
                 "PrecipTempCorr_meanAnnAvg_30yr"         ,  
                 "isothermality_meanAnnAvg_30yr" ,
                 "soilDepth"  , 
                 "avgSandPerc_acrossDepth" ,  
                 "avgCoarsePerc_acrossDepth",
                 "avgOrganicCarbonPerc_0_3cm") 
# perturbation function 
perturbFun_2 <- function(predictor, 
                       predictData, 
                       perturbVal, 
                       model) {
  #predictor <- predictors[1]
  #predictData <- modDat_testNew
  #perturbVal <- c(-.2, -.1, .1, .2)
  #model <- testMod_2
  predsOut <- vector(mode = "list", length(perturbVal))
  # for each value of perturbVal 
  if (predictor == "PrecipTempCorr_meanAnnAvg_30yr") {
    for (i in 1:length(perturbVal)) {
      predictDat_i <- predictData
      # transform predictor value of choice
      predictDat_i[,predictor] <-  predictDat_i[,predictor] + predictDat_i[,predictor]*perturbVal[i] 
      # if the predicted values are less than -1 , change to -1
      predictDat_i[ predictDat_i[,predictor] < -1 ,predictor] <- -1
      # if the predicted values are greater than 1 , change to 1
      predictDat_i[ predictDat_i[,predictor] > 1 ,predictor] <- 1
      # predict 
      regModPreds_i <- predictDat_i %>%
        cbind("newRegion_pred" = predict(testMod_2, newdata = predictDat_i, "response")) %>%  
        rowwise() %>% 
        mutate(newRegion_group = newRegion_pred) %>% 
        mutate(newRegion_group = replace(newRegion_group, newRegion_pred<.5, "dryShrubGrass"), 
               newRegion_group = replace(newRegion_group, newRegion_pred>.5, "forest")) 
      
      # save predictions
      predsOut[[i]] <- list("preds" = regModPreds_i,
                            "perturbVal" = perturbVal[i],
                            "perturbPredictor" = predictor)
    }
  } else if (predictor %in% c("surfaceClay_perc", "avgCoarsePerc_acrossDepth", "avgOrganicCarbonPerc_0_3cm") ) {
    for (i in 1:length(perturbVal)) {
      predictDat_i <- predictData
      # transform predictor value of choice
      predictDat_i[,predictor] <-  predictDat_i[,predictor] + predictDat_i[,predictor]*perturbVal[i] 
      # if the predicted values are less than 0 , change to 0
      predictDat_i[ predictDat_i[,predictor] < 0 ,predictor] <- 0
      # if the predicted values are greater than 100 , change to 100
      predictDat_i[ predictDat_i[,predictor] > 100 ,predictor] <- 100
      # predict 
      regModPreds_i <- predictDat_i %>%
        cbind("newRegion_pred" = predict(testMod_2, newdata = predictDat_i, "response")) %>%  
        rowwise() %>% 
        mutate(newRegion_group = newRegion_pred) %>% 
        mutate(newRegion_group = replace(newRegion_group, newRegion_pred<.5, "dryShrubGrass"), 
               newRegion_group = replace(newRegion_group, newRegion_pred>.5, "forest")) 
      
      # save predictions
      predsOut[[i]] <- list("preds" = regModPreds_i,
                            "perturbVal" = perturbVal[i],
                            "perturbPredictor" = predictor)
    }
  } else  {
    for (i in 1:length(perturbVal)) {
      predictDat_i <- predictData
      # transform predictor value of choice
      predictDat_i[,predictor] <-  predictDat_i[,predictor] + predictDat_i[,predictor]*perturbVal[i] 
      # predict 
      regModPreds_i <- predictDat_i %>%
        cbind("newRegion_pred" = predict(testMod_2, newdata = predictDat_i, "response")) %>%  
        rowwise() %>% 
        mutate(newRegion_group = newRegion_pred) %>% 
        mutate(newRegion_group = replace(newRegion_group, newRegion_pred<.5, "dryShrubGrass"), 
               newRegion_group = replace(newRegion_group, newRegion_pred>.5, "forest")) 
      
      # save predictions
      predsOut[[i]] <- list("preds" = regModPreds_i,
                            "perturbVal" = perturbVal[i],
                            "perturbPredictor" = predictor)
    }
  }
  
  return(predsOut)
}

# predictions for temp of warmest month
preds_tempWarmestMonth_2 <- 
  perturbFun_2(predictor = predictors[1],
               predictData = modDat_testNew, 
               perturbVal = c(-.4, -.25, -.1, .1, .25, .4),
               model = testMod)

# predictions for temp of coldest month 
preds_tempColdestMonth_2 <- 
  perturbFun_2(predictor = predictors[2],
             predictData = modDat_testNew, 
             perturbVal = c(-.4, -.25, -.1, .1, .25, .4),
             model = testMod)

# predictions for precip of wettest month
preds_precipWettestMonth_2 <- 
  perturbFun_2(predictor = predictors[3],
               predictData = modDat_testNew, 
               perturbVal =  c(-.4, -.25, -.1, .1, .25, .4),
               model = testMod) 

# predictions for precip of driest month
preds_precipDriestMonth_2 <- 
  perturbFun_2(predictor = predictors[4],
             predictData = modDat_testNew, 
             perturbVal =  c(-.4, -.25, -.1, .1, .25, .4),
             model = testMod)

# predictions for precipTemp correlation
preds_precipTempCorr_2 <- 
  perturbFun_2(predictor = predictors[5],
             predictData = modDat_testNew, 
             perturbVal =  c(-.4, -.25, -.1, .1, .25, .4),
             model = testMod)

# predictions for isothermality
preds_isotherm_2 <- 
  perturbFun_2(predictor = predictors[6],
             predictData = modDat_testNew, 
             perturbVal =  c(-.4, -.25, -.1, .1, .25, .4),
             model = testMod)

# # predictions for VPD max 95th percentile
# preds_VPDmax_95_2 <- 
#   perturbFun_2(predictor = predictors[5],
#              predictData = modDat_testNew, 
#              perturbVal =  c(-.4, -.25, -.1, .1, .25, .4),
#              model = testMod)

# predictions for soil depth
preds_soilDepth_2 <- 
  perturbFun_2(predictor = predictors[7],
             predictData = modDat_testNew, 
             perturbVal =  c(-.4, -.25, -.1, .1, .25, .4),
             model = testMod)

# predictions for % clay in soil surface
preds_sand_2 <- 
  perturbFun_2(predictor = predictors[8],
             predictData = modDat_testNew, 
             perturbVal =  c(-.4, -.25, -.1, .1, .25, .4),
             model = testMod)

# predictions for % coarse soil parts across depth
preds_coarse_2 <- 
  perturbFun_2(predictor = predictors[9],
             predictData = modDat_testNew, 
             perturbVal =  c(-.4, -.25, -.1, .1, .25, .4),
             model = testMod)

# predictions for % surface organic matter in the soil 
preds_carbon_2 <- 
  perturbFun_2(predictor = predictors[10],
             predictData = modDat_testNew, 
             perturbVal =  c(-.4, -.25, -.1, .1, .25, .4),
             model = testMod)


# make figures showing predictions ----------------------------------------
makeFigures_sens_2 <- function(modPreds, 
                             modPreds_noPerturb) {
  ## add predictions together while keeping track of the perturbation 
  for (i in 1:length(modPreds)){  
    predictor_i <- modPreds[[i]]$perturbPredictor
    perturb_i <- modPreds[[i]]$perturbVal
    preds_i <- modPreds[[i]]$preds[,c(predictor_i, "newRegion_pred","newRegion_group")]
    preds_i$perturbVal <- perturb_i
    if (i==1) {
      predsOut <- preds_i
    } else {
      predsOut <- rbind(predsOut, preds_i)
    }
  }
  # add in values for predictions with no perturbation
  predsOut_final <- rbind(predsOut,
                          cbind(modPreds_noPerturb[,c(predictor_i, "newRegion_pred","newRegion_group")],
                                data.frame("perturbVal" = rep_len(0, length.out = nrow(modPreds_noPerturb))))
  )
  
  # transform probabilities for shrubGrass to be from .5 to .1
  predsOut_final[predsOut_final$newRegion_group == "dryShrubGrass","newRegion_pred"] <- 1-
    predsOut_final[predsOut_final$newRegion_group == "dryShrubGrass","newRegion_pred"] 
  
  names(predsOut_final)[1] <- "predictor"
  # make a figure showing change in response predictions across levels of perturbation
  shrubGrassPred_line<- ggplot(data = predsOut_final) + 
    # geom_point(aes(x =  predictor, y = dryShrubGrass_prob, col = as.factor(perturbVal))) +
    geom_smooth(aes(x = predictor, y = newRegion_pred, col = as.factor(perturbVal)), se = FALSE) +
    facet_wrap(~newRegion_group) +
    theme_minimal() +
    ggtitle("Sensitivity of Ecoregion predicted probability") +
    ylab("P(Ecoregion Assignment)")+
    xlab(predictor_i) +
    labs(color = "perturb. value")

  shrubGrassPred_point <- ggplot(data = predsOut_final) + 
    geom_point(aes(x =  predictor, y = newRegion_pred, col = as.factor(perturbVal))) +
    facet_wrap(~newRegion_group) +
    theme_minimal() +
    ggtitle("Sensitivity of Ecoregion predicted probability") +
    ylab("P(Ecoregion Assignment)")+
    xlab(predictor_i) +
    labs(color = "perturb. value")
  
  ## combine into one figure
  predMod <- ggarrange(shrubGrassPred_point,
                       shrubGrassPred_line,
                       common.legend = TRUE, nrow = 2, ncol = 1) %>% 
    annotate_figure(paste0("Impact of purturbations in ", predictor_i," on model predictions"))
  
  return(predMod)
}

#  avgSandPerc_acrossDepth  
# 

## make figures
# predictions for temp of warmest month 
(predsFits_2_tempWarmestMonth <- makeFigures_sens_2(modPreds = preds_tempWarmestMonth_2, modPreds_noPerturb = regModPreds_2_df)
)

# predictions for temp of coldest month
(predsFits_2_tempColdestMonth <- makeFigures_sens_2(modPreds = preds_tempColdestMonth_2, modPreds_noPerturb = regModPreds_2_df)
)


# predictions for precip of wettest month
(predsFits_2_precipWettestMonth <- makeFigures_sens_2(modPreds = preds_precipWettestMonth_2, modPreds_noPerturb = regModPreds_2_df)
)

# predictions for precip of driest month
(predsFits_2_precipDriestMonth <- makeFigures_sens_2(modPreds = preds_precipDriestMonth_2, modPreds_noPerturb = regModPreds_2_df)
)

# predictions for precip of wettest month 
(predsFits_2_precipTempCorr <- makeFigures_sens_2(modPreds = preds_precipTempCorr_2, modPreds_noPerturb = regModPreds_2_df)
)
# predictions for isothermality
(predsFits_2_isotherm <- makeFigures_sens_2(modPreds = preds_isotherm_2, modPreds_noPerturb = regModPreds_2_df)
)
# # predictions for VPD max 95th percentile
# predsFits_2_VPDmax_95 <- makeFigures_sens_2(modPreds = preds_VPDmax_95_2, modPreds_noPerturb = regModPreds_2_df)

# predictions for soil depth
(predsFits_2_soilDepth <- makeFigures_sens_2(modPreds = preds_soilDepth_2, modPreds_noPerturb = regModPreds_2_df)
)
# # predictions for % clay in soil surface
# predsFits_2_clay <- makeFigures_sens_2(modPreds = preds_clay_2, modPreds_noPerturb = regModPreds_2_df)

# predictions for % sand across depth
(predsFits_2_sand <- makeFigures_sens_2(modPreds = preds_sand_2, modPreds_noPerturb = regModPreds_2_df)
)
# predictions for % coarse soil parts across depth
(predsFits_2_coarse <- makeFigures_sens_2(modPreds = preds_coarse_2, modPreds_noPerturb = regModPreds_2_df)
)
# predictions for % surface organic matter in the soil 
(predsFits_2_carbon <- makeFigures_sens_2(modPreds = preds_carbon_2, modPreds_noPerturb = regModPreds_2_df)
)


# make partial dependence plots -------------------------------------------
# % carbon
pdp_carbon <- pdp::partial(testMod_2, pred.var = c("avgOrganicCarbonPerc_0_3cm"), 
                           prob = TRUE, rug = TRUE, plot = TRUE)

(pdp_carbon <- update(pdp_carbon, main = "Soil Carbon",ylim = c(0,1)) )
# % coarse
pdp_coarse <- pdp::partial(testMod_2, pred.var = c("avgCoarsePerc_acrossDepth"), 
                           prob = TRUE, rug = TRUE, plot = TRUE)

(pdp_coarse <- update(pdp_coarse, main = "% coarse fragments", ylim = c(0,1)))

# %sand
pdp_sand <- pdp::partial(testMod_2, pred.var = c("avgSandPerc_acrossDepth"), 
                           prob = TRUE, rug = TRUE, plot = TRUE)

(pdp_sand <- update(pdp_sand, main = "% sand", ylim = c(0,1)))

# soil depth
pdp_soilDepth<- pdp::partial(testMod_2, pred.var = c("soilDepth"), 
                         prob = TRUE, rug = TRUE, plot = TRUE)

(pdp_soilDepth <- update(pdp_soilDepth, main = "soil depth", ylim = c(0,1)))

# isothermality
pdp_isothermality <- pdp::partial(testMod_2, pred.var = c("isothermality_meanAnnAvg_30yr"), 
                         prob = TRUE, rug = TRUE, plot = TRUE)

(pdp_isothermality <- update(pdp_isothermality, main = "isothermality", ylim = c(0,1)))
  
# isothermality
pdp_precipTempCorr <- pdp::partial(testMod_2, pred.var = c("PrecipTempCorr_meanAnnAvg_30yr"), 
                                  prob = TRUE, rug = TRUE, plot = TRUE)

(pdp_precipTempCorr <- update(pdp_precipTempCorr, main = "Precip-Temp Corr.", ylim = c(0,1)))

# precipDriestMonth
pdp_precipDriestMonth <- pdp::partial(testMod_2, pred.var = c("precip_driestMonth_meanAnnAvg_30yr"), 
                                   prob = TRUE, rug = TRUE, plot = TRUE)

(pdp_precipDriestMonth <- update(pdp_precipDriestMonth, main = "Precip of Driest Month", ylim = c(0,1)))

# precipWettestMonth
pdp_precipWettestMonth <- pdp::partial(testMod_2, pred.var = c("precip_wettestMonth_meanAnnAvg_30yr"), 
                                      prob = TRUE, rug = TRUE, plot = TRUE)

(pdp_precipWettestMonth <- update(pdp_precipWettestMonth, main = "Precip of Wettest Month", ylim = c(0,1)))

# temp coldest month
pdp_TColdestMonth <- pdp::partial(testMod_2, pred.var = c("T_coldestMonth_meanAnnAvg_30yr"), 
                                       prob = TRUE, rug = TRUE, plot = TRUE)

(pdp_TColdestMonth <- update(pdp_TColdestMonth, main = "Temp of Coldest Month", ylim = c(0,1)))

# temp warmest month
pdp_TWarmestMonth <- pdp::partial(testMod_2, pred.var = c("T_warmestMonth_meanAnnAvg_30yr"), 
                                  prob = TRUE, rug = TRUE, plot = TRUE)

(pdp_TWarmestMonth <- update(pdp_TWarmestMonth, main = "Temp of Warmest Month", ylim = c(0,1)))

bitmap(file = "./Figures/EcoRegionModelFigures/RegressionModel_2Ecoregions_PartialDependencyPlots.bmp", 
       width = 16, height =11, res = 300)
print(pdp_carbon, split = c(1,1,4,3), more = TRUE)
print(pdp_coarse, split = c(2,1,4,3), more = TRUE)
print(pdp_sand, split = c(3,1,4,3), more = TRUE)
print(pdp_soilDepth, split = c(4,1,4,3), more = TRUE)
print(pdp_isothermality, split = c(1,2,4,3), more = TRUE)
print(pdp_precipTempCorr, split = c(2,2,4,3), more = TRUE)
print(pdp_precipDriestMonth, split = c(3,2,4,3), more = TRUE)
print(pdp_precipWettestMonth, split = c(4,2,4,3), more = TRUE)
print(pdp_TWarmestMonth, split = c(1,3,4,3), more = TRUE)
print(pdp_TColdestMonth, split = c(2,3,4,3), more = FALSE)
dev.off()


# make maps of perturbations for a few important variables ----------------
# precip of driest month
#preds_precipDriestMonth_2 is the data frame to use 
precipDriestMonthPerturbRast <- preds_precipDriestMonth_2[[6]]$preds %>%
  #cbind("newRegion_pred" = predict(testMod_2, newdata = sensDat_2b, "response")) %>%  rowwise() %>% 
  mutate(newRegion_group = newRegion_pred) %>% 
  mutate(newRegion_group = replace(newRegion_group, newRegion_pred<.5, "dryShrubGrass"), 
         newRegion_group = replace(newRegion_group, newRegion_pred>.5, "forest")) %>% 
  terra::vect(geom = c("Long", "Lat"), crs = crs(soilRastTemp)) %>% 
  terra::rasterize(y = 
                     terra::aggregate(soilRastTemp, fact = 2, fun = mean)
                   , field = "newRegion_pred")

#//
(precipDriestMonthPerturb_MAP_high <- ggplot() +
    geom_spatraster(data = precipDriestMonthPerturbRast) +
    # geom_point(aes(#Long, Lat, 
    #   col = newRegion_pred)) +
    ggtitle("model-predicted ecoregion classification -- fit to contemporary data w/ 
            40% *increase* in precip of driest month") +
    #scale_fill_discrete(type = c("#bf812d","#35978f")) +
    scale_fill_distiller(type = "div", palette = 1, direction = 1) +
    geom_sf(data = ecoregionsSF2, color = "black", fill = NA, lwd = 1.25) )

#preds_precipDriestMonth_2 is the data frame to use 
precipDriestMonthPerturbRast <- preds_precipDriestMonth_2[[1]]$preds %>%
  #cbind("newRegion_pred" = predict(testMod_2, newdata = sensDat_2b, "response")) %>%  rowwise() %>% 
  mutate(newRegion_group = newRegion_pred) %>% 
  mutate(newRegion_group = replace(newRegion_group, newRegion_pred<.5, "dryShrubGrass"), 
         newRegion_group = replace(newRegion_group, newRegion_pred>.5, "forest")) %>% 
  terra::vect(geom = c("Long", "Lat"), crs = crs(soilRastTemp)) %>% 
  terra::rasterize(y = 
                     terra::aggregate(soilRastTemp, fact = 2, fun = mean)
                   , field = "newRegion_pred")

#//
(precipDriestMonthPerturb_MAP_low <- ggplot() +
    geom_spatraster(data = precipDriestMonthPerturbRast) +
    # geom_point(aes(#Long, Lat, 
    #   col = newRegion_pred)) +
    ggtitle("model-predicted ecoregion classification -- fit to contemporary data 
            40% *decrease* in precip of driest month") +
  #scale_fill_discrete(type = c("#bf812d","#35978f")) +
  scale_fill_distiller(type = "div", palette = 1, direction = 1) +
  geom_sf(data = ecoregionsSF2, color = "black", fill = NA, lwd = 1.25) )

### precip of wettest month
#preds_precipWettestMonth_2 is the data frame to use 
precipWettestMonthPerturbRast <- preds_precipWettestMonth_2[[6]]$preds %>%
  #cbind("newRegion_pred" = predict(testMod_2, newdata = sensDat_2b, "response")) %>%  rowwise() %>% 
  mutate(newRegion_group = newRegion_pred) %>% 
  mutate(newRegion_group = replace(newRegion_group, newRegion_pred<.5, "dryShrubGrass"), 
         newRegion_group = replace(newRegion_group, newRegion_pred>.5, "forest")) %>% 
  terra::vect(geom = c("Long", "Lat"), crs = crs(soilRastTemp)) %>% 
  terra::rasterize(y = 
                     terra::aggregate(soilRastTemp, fact = 2, fun = mean)
                   , field = "newRegion_pred")


(precipWettestMonthPertur_MAP_high <- ggplot() +
    geom_spatraster(data = precipWettestMonthPerturbRast) +
    # geom_point(aes(#Long, Lat, 
    #   col = newRegion_pred)) +
    ggtitle("model-predicted ecoregion classification -- fit to contemporary data w/ 
            40% *increase* in precip of wettest month") +
    #scale_fill_discrete(type = c("#bf812d","#35978f")) +
    scale_fill_distiller(type = "div", palette = 1, direction = 1) +
    geom_sf(data = ecoregionsSF2, color = "black", fill = NA, lwd = 1.25) )

#preds_precipDriestMonth_2 is the data frame to use 
precipWettestMonthPerturbRast <- preds_precipWettestMonth_2[[1]]$preds %>%
  #cbind("newRegion_pred" = predict(testMod_2, newdata = sensDat_2b, "response")) %>%  rowwise() %>% 
  mutate(newRegion_group = newRegion_pred) %>% 
  mutate(newRegion_group = replace(newRegion_group, newRegion_pred<.5, "dryShrubGrass"), 
         newRegion_group = replace(newRegion_group, newRegion_pred>.5, "forest")) %>% 
  terra::vect(geom = c("Long", "Lat"), crs = crs(soilRastTemp)) %>% 
  terra::rasterize(y = 
                     terra::aggregate(soilRastTemp, fact = 2, fun = mean)
                   , field = "newRegion_pred")


(precipWettestMonthPertur_MAP_low <- ggplot() +
    geom_spatraster(data = precipWettestMonthPerturbRast) +
    # geom_point(aes(#Long, Lat, 
    #   col = newRegion_pred)) +
    ggtitle("model-predicted ecoregion classification -- fit to contemporary data w/ 
            40% *decrease* in precip of wettest month") +
    #scale_fill_discrete(type = c("#bf812d","#35978f")) +
    scale_fill_distiller(type = "div", palette = 1, direction = 1) +
    geom_sf(data = ecoregionsSF2, color = "black", fill = NA, lwd = 1.25) )

### temp of warmest month
tempWarmestMonthPerturbRast <- preds_tempWarmestMonth_2[[6]]$preds %>%
  #cbind("newRegion_pred" = predict(testMod_2, newdata = sensDat_2b, "response")) %>%  rowwise() %>% 
  mutate(newRegion_group = newRegion_pred) %>% 
  mutate(newRegion_group = replace(newRegion_group, newRegion_pred<.5, "dryShrubGrass"), 
         newRegion_group = replace(newRegion_group, newRegion_pred>.5, "forest")) %>% 
  terra::vect(geom = c("Long", "Lat"), crs = crs(soilRastTemp)) %>% 
  terra::rasterize(y = 
                     terra::aggregate(soilRastTemp, fact = 2, fun = mean)
                   , field = "newRegion_pred")


(tempWarmestMonthPerturb_MAP_high <- ggplot() +
    geom_spatraster(data = tempWarmestMonthPerturbRast) +
    # geom_point(aes(#Long, Lat, 
    #   col = newRegion_pred)) +
    ggtitle("model-predicted ecoregion classification -- fit to contemporary data w/ 
            40% *increase* in temp of warmest month") +
    #scale_fill_discrete(type = c("#bf812d","#35978f")) +
    scale_fill_distiller(type = "div", palette = 1, direction = 1) +
    geom_sf(data = ecoregionsSF2, color = "black", fill = NA, lwd = 1.25) )

#preds_precipDriestMonth_2 is the data frame to use 
tempWarmestMonthPerturbRast <- preds_tempWarmestMonth_2[[1]]$preds %>%
  #cbind("newRegion_pred" = predict(testMod_2, newdata = sensDat_2b, "response")) %>%  rowwise() %>% 
  mutate(newRegion_group = newRegion_pred) %>% 
  mutate(newRegion_group = replace(newRegion_group, newRegion_pred<.5, "dryShrubGrass"), 
         newRegion_group = replace(newRegion_group, newRegion_pred>.5, "forest")) %>% 
  terra::vect(geom = c("Long", "Lat"), crs = crs(soilRastTemp)) %>% 
  terra::rasterize(y = 
                     terra::aggregate(soilRastTemp, fact = 2, fun = mean)
                   , field = "newRegion_pred")


(tempWarmestMonthPerturb_MAP_low <- ggplot() +
    geom_spatraster(data = tempWarmestMonthPerturbRast) +
    # geom_point(aes(#Long, Lat, 
    #   col = newRegion_pred)) +
    ggtitle("model-predicted ecoregion classification -- fit to contemporary data w/ 
            40% *decrease* in temp of warmest month") +
    #scale_fill_discrete(type = c("#bf812d","#35978f")) +
    scale_fill_distiller(type = "div", palette = 1, direction = 1) +
    geom_sf(data = ecoregionsSF2, color = "black", fill = NA, lwd = 1.25) )

### temp of coldest month
tempColdestMonthPerturbRast <- preds_tempColdestMonth_2[[6]]$preds %>%
  #cbind("newRegion_pred" = predict(testMod_2, newdata = sensDat_2b, "response")) %>%  rowwise() %>% 
  mutate(newRegion_group = newRegion_pred) %>% 
  mutate(newRegion_group = replace(newRegion_group, newRegion_pred<.5, "dryShrubGrass"), 
         newRegion_group = replace(newRegion_group, newRegion_pred>.5, "forest")) %>% 
  terra::vect(geom = c("Long", "Lat"), crs = crs(soilRastTemp)) %>% 
  terra::rasterize(y = 
                     terra::aggregate(soilRastTemp, fact = 2, fun = mean)
                   , field = "newRegion_pred")


(tempColdestMonthPerturb_MAP_high <- ggplot() +
    geom_spatraster(data = tempColdestMonthPerturbRast) +
    # geom_point(aes(#Long, Lat, 
    #   col = newRegion_pred)) +
    ggtitle("model-predicted ecoregion classification -- fit to contemporary data w/ 
            40% *increase* in temp of coldest month") +
    #scale_fill_discrete(type = c("#bf812d","#35978f")) +
    scale_fill_distiller(type = "div", palette = 1, direction = 1) +
    geom_sf(data = ecoregionsSF2, color = "black", fill = NA, lwd = 1.25) )

#preds_precipDriestMonth_2 is the data frame to use 
tempColdestMonthPerturbRast <- preds_tempColdestMonth_2[[1]]$preds %>%
  #cbind("newRegion_pred" = predict(testMod_2, newdata = sensDat_2b, "response")) %>%  rowwise() %>% 
  mutate(newRegion_group = newRegion_pred) %>% 
  mutate(newRegion_group = replace(newRegion_group, newRegion_pred<.5, "dryShrubGrass"), 
         newRegion_group = replace(newRegion_group, newRegion_pred>.5, "forest")) %>% 
  terra::vect(geom = c("Long", "Lat"), crs = crs(soilRastTemp)) %>% 
  terra::rasterize(y = 
                     terra::aggregate(soilRastTemp, fact = 2, fun = mean)
                   , field = "newRegion_pred")


(tempColdestMonthPerturb_MAP_low <- ggplot() +
    geom_spatraster(data = tempColdestMonthPerturbRast) +
    # geom_point(aes(#Long, Lat, 
    #   col = newRegion_pred)) +
    ggtitle("model-predicted ecoregion classification -- fit to contemporary data w/ 
            40% *decrease* in temp of coldest month") +
    #scale_fill_discrete(type = c("#bf812d","#35978f")) +
    scale_fill_distiller(type = "div", palette = 1, direction = 1) +
    geom_sf(data = ecoregionsSF2, color = "black", fill = NA, lwd = 1.25) )

### precipTempCorr
precipTempCorrPerturbRast <- preds_precipTempCorr_2[[6]]$preds %>%
  #cbind("newRegion_pred" = predict(testMod_2, newdata = sensDat_2b, "response")) %>%  rowwise() %>% 
  mutate(newRegion_group = newRegion_pred) %>% 
  mutate(newRegion_group = replace(newRegion_group, newRegion_pred<.5, "dryShrubGrass"), 
         newRegion_group = replace(newRegion_group, newRegion_pred>.5, "forest")) %>% 
  terra::vect(geom = c("Long", "Lat"), crs = crs(soilRastTemp)) %>% 
  terra::rasterize(y = 
                     terra::aggregate(soilRastTemp, fact = 2, fun = mean)
                   , field = "newRegion_pred")


(precipTempCorrPerturb_MAP_high <- ggplot() +
    geom_spatraster(data = precipTempCorrPerturbRast) +
    # geom_point(aes(#Long, Lat, 
    #   col = newRegion_pred)) +
    ggtitle("model-predicted ecoregion classification -- fit to contemporary data w/ 
            40% *increase* in correlation of precip. and temp.") +
    #scale_fill_discrete(type = c("#bf812d","#35978f")) +
    scale_fill_distiller(type = "div", palette = 1, direction = 1) +
    geom_sf(data = ecoregionsSF2, color = "black", fill = NA, lwd = 1.25) )

precipTempCorrPerturbRast <- preds_precipTempCorr_2[[1]]$preds %>%
  #cbind("newRegion_pred" = predict(testMod_2, newdata = sensDat_2b, "response")) %>%  rowwise() %>% 
  mutate(newRegion_group = newRegion_pred) %>% 
  mutate(newRegion_group = replace(newRegion_group, newRegion_pred<.5, "dryShrubGrass"), 
         newRegion_group = replace(newRegion_group, newRegion_pred>.5, "forest")) %>% 
  terra::vect(geom = c("Long", "Lat"), crs = crs(soilRastTemp)) %>% 
  terra::rasterize(y = 
                     terra::aggregate(soilRastTemp, fact = 2, fun = mean)
                   , field = "newRegion_pred")


(precipTempCorrPerturb_MAP_low <- ggplot() +
    geom_spatraster(data = precipTempCorrPerturbRast) +
    # geom_point(aes(#Long, Lat, 
    #   col = newRegion_pred)) +
    ggtitle("model-predicted ecoregion classification -- fit to contemporary data w/ 
            40% *decrease* in correlation of precip. and temp.") +
    #scale_fill_discrete(type = c("#bf812d","#35978f")) +
    scale_fill_distiller(type = "div", palette = 1, direction = 1) +
    geom_sf(data = ecoregionsSF2, color = "black", fill = NA, lwd = 1.25) )

### isothermality
isothermPerturbRast <- preds_isotherm_2[[6]]$preds %>%
  #cbind("newRegion_pred" = predict(testMod_2, newdata = sensDat_2b, "response")) %>%  rowwise() %>% 
  mutate(newRegion_group = newRegion_pred) %>% 
  mutate(newRegion_group = replace(newRegion_group, newRegion_pred<.5, "dryShrubGrass"), 
         newRegion_group = replace(newRegion_group, newRegion_pred>.5, "forest")) %>% 
  terra::vect(geom = c("Long", "Lat"), crs = crs(soilRastTemp)) %>% 
  terra::rasterize(y = 
                     terra::aggregate(soilRastTemp, fact = 2, fun = mean)
                   , field = "newRegion_pred")


(isothermPerturb_MAP_high <- ggplot() +
    geom_spatraster(data = isothermPerturbRast) +
    # geom_point(aes(#Long, Lat, 
    #   col = newRegion_pred)) +
    ggtitle("model-predicted ecoregion classification -- fit to contemporary data w/ 
            40% *increase* in isothermality") +
    #scale_fill_discrete(type = c("#bf812d","#35978f")) +
    scale_fill_distiller(type = "div", palette = 1, direction = 1) +
    geom_sf(data = ecoregionsSF2, color = "black", fill = NA, lwd = 1.25) )

isothermPerturbRast <- preds_isotherm_2[[1]]$preds %>%
  #cbind("newRegion_pred" = predict(testMod_2, newdata = sensDat_2b, "response")) %>%  rowwise() %>% 
  mutate(newRegion_group = newRegion_pred) %>% 
  mutate(newRegion_group = replace(newRegion_group, newRegion_pred<.5, "dryShrubGrass"), 
         newRegion_group = replace(newRegion_group, newRegion_pred>.5, "forest")) %>% 
  terra::vect(geom = c("Long", "Lat"), crs = crs(soilRastTemp)) %>% 
  terra::rasterize(y = 
                     terra::aggregate(soilRastTemp, fact = 2, fun = mean)
                   , field = "newRegion_pred")


(isothermPerturb_MAP_low <- ggplot() +
    geom_spatraster(data = isothermPerturbRast) +
    # geom_point(aes(#Long, Lat, 
    #   col = newRegion_pred)) +
    ggtitle("model-predicted ecoregion classification -- fit to contemporary data w/ 
            40% *decrease* in isothermality") +
    #scale_fill_discrete(type = c("#bf812d","#35978f")) +
    scale_fill_distiller(type = "div", palette = 1, direction = 1) +
    geom_sf(data = ecoregionsSF2, color = "black", fill = NA, lwd = 1.25) )

### organic carbon
#preds_precipDriestMonth_2 is the data frame to use 
carbonPerturbRast <- preds_carbon_2[[6]]$preds %>%
  #cbind("newRegion_pred" = predict(testMod_2, newdata = sensDat_2b, "response")) %>%  rowwise() %>% 
  mutate(newRegion_group = newRegion_pred) %>% 
  mutate(newRegion_group = replace(newRegion_group, newRegion_pred<.5, "dryShrubGrass"), 
         newRegion_group = replace(newRegion_group, newRegion_pred>.5, "forest")) %>% 
  terra::vect(geom = c("Long", "Lat"), crs = crs(soilRastTemp)) %>% 
  terra::rasterize(y = 
                     terra::aggregate(soilRastTemp, fact = 2, fun = mean)
                   , field = "newRegion_pred")

#//
(carbonPerturb_MAP_high <- ggplot() +
    geom_spatraster(data = carbonPerturbRast) +
    # geom_point(aes(#Long, Lat, 
    #   col = newRegion_pred)) +
    ggtitle("model-predicted ecoregion classification -- fit to contemporary data w/ 
            40% *increase* in soil carbon") +
    #scale_fill_discrete(type = c("#bf812d","#35978f")) +
    scale_fill_distiller(type = "div", palette = 1, direction = 1) +
    geom_sf(data = ecoregionsSF2, color = "black", fill = NA, lwd = 1.25) )

#preds_carbon_2 is the data frame to use 
carbonPerturbRast <- preds_carbon_2[[1]]$preds %>%
  #cbind("newRegion_pred" = predict(testMod_2, newdata = sensDat_2b, "response")) %>%  rowwise() %>% 
  mutate(newRegion_group = newRegion_pred) %>% 
  mutate(newRegion_group = replace(newRegion_group, newRegion_pred<.5, "dryShrubGrass"), 
         newRegion_group = replace(newRegion_group, newRegion_pred>.5, "forest")) %>% 
  terra::vect(geom = c("Long", "Lat"), crs = crs(soilRastTemp)) %>% 
  terra::rasterize(y = 
                     terra::aggregate(soilRastTemp, fact = 2, fun = mean)
                   , field = "newRegion_pred")

#//
(carbonPerturb_MAP_low <- ggplot() +
    geom_spatraster(data = carbonPerturbRast) +
    # geom_point(aes(#Long, Lat, 
    #   col = newRegion_pred)) +
    ggtitle("model-predicted ecoregion classification -- fit to contemporary data 
            40% *decrease* in soil carbon") +
    #scale_fill_discrete(type = c("#bf812d","#35978f")) +
    scale_fill_distiller(type = "div", palette = 1, direction = 1) +
    geom_sf(data = ecoregionsSF2, color = "black", fill = NA, lwd = 1.25) )

# make figures to save ----------------------------------------------------

plots <- ggarrange(regMod2_PredMAP, regMod2b_PredMAP, regMod2c_PredMAP, 
                  # partialDepPlots_2,
                   predsFits_2_tempWarmestMonth, predsFits_2_tempColdestMonth, 
                   predsFits_2_precipWettestMonth,predsFits_2_precipDriestMonth, predsFits_2_precipTempCorr, 
                   predsFits_2_isotherm,predsFits_2_soilDepth, 
                   predsFits_2_sand, predsFits_2_coarse, predsFits_2_carbon, ncol = 1) 



bitmap(file = "./Figures/EcoRegionModelFigures/RegressionModel_2Ecoregions_Sensitivity.bmp", 
       width = 16, height = 140, res = 120)
plots
dev.off()
 
(predictionPlots <- ggarrange(precipDriestMonthPerturb_MAP_high, precipDriestMonthPerturb_MAP_low, 
                              precipWettestMonthPertur_MAP_high, precipWettestMonthPertur_MAP_low,
                              tempWarmestMonthPerturb_MAP_high, tempWarmestMonthPerturb_MAP_low,
                              tempColdestMonthPerturb_MAP_high, tempColdestMonthPerturb_MAP_low,
                              precipTempCorrPerturb_MAP_high, precipTempCorrPerturb_MAP_low,
                              isothermPerturb_MAP_high, isothermPerturb_MAP_low,
                             carbonPerturb_MAP_low, carbonPerturb_MAP_high, ncol = 2, nrow = 7)
)

bitmap(file = "./Figures/EcoRegionModelFigures/RegressionModel_2EcoregionsNoVPD_PerturbationMaps.bmp", 
       width = 15, height = 30, res = 250)
predictionPlots
dev.off()

# # read in model  
# # the model is calleds "testMod" 
# # newRegionFact ~ precip_wettestMonth_meanAnnAvg_30yr  + 
# # precip_driestMonth_meanAnnAvg_30yr    +
# #   PrecipTempCorr_meanAnnAvg_30yr       +
# #   isothermality_meanAnnAvg_30yr        +
# #   annVPD_max_95percentile_30yr         +  
# #   soilDepth                             +
# #   surfaceClay_perc                     + 
# #   avgCoarsePerc_acrossDepth            +  avgOrganicCarbonPerc_0_3cm
# 
# # predict categories based on this simple model
# 
# regModPreds <- sensDat %>%
#   cbind(predict(testMod, newdata = sensDat, "probs")) %>% 
#   mutate(
#     newRegion_predicted = pmap_dbl(.[c("dryShrubGrass", "eastForest", "westForest")], max),
#     across(dryShrubGrass:westForest, ~.x == newRegion_predicted)) %>%
#   mutate(dryShrubGrass = str_replace(dryShrubGrass, 'TRUE', 'dryShrubGrass'),
#          eastForest = str_replace(eastForest, 'TRUE', "eastForest"),
#          westForest = str_replace(westForest, 'TRUE', "westForest"),
#          dryShrubGrass = str_replace(dryShrubGrass, 'FALSE', ""),
#          eastForest = str_replace(eastForest, 'FALSE', ""),
#          westForest = str_replace(westForest, 'FALSE', "")) %>%
#   mutate(
#     newRegion_group = as.factor(pmap_chr(.[c("dryShrubGrass", "eastForest", "westForest")], paste0))) %>% 
#   select(-dryShrubGrass, -eastForest, -westForest) %>% 
#   cbind(predict(testMod, newdata = sensDat, "probs")) %>% 
#   rename("dryShrubGrass_prob" = dryShrubGrass, 
#          "eastForest_prob" = eastForest, 
#          "westForest_prob" = westForest)
# 
# #//
# (regMod_PredMAP <- ggplot(regModPreds) +
#     geom_point(aes(Long, Lat, col = newRegion_group)) +
#     ggtitle("model-predicted ecoregion classification -- fit to a combination of contemporary data and data from 2041-2060 \nfrom CMCC-ESM2 model, ssp 585 ",
#             subtitle = paste0("model: ecoregion ~ precip of wettest Month  + precip of driest month + precip/temp corr. + isothermality + 
#             95th percentile of VPD max + soil depth + surface clay + coarse fraction + surface organic matter")) 
# )
# 
# 
# # predictions w/ mix of true forecasted values and faked forecasts 
# 
# #"faked" meaning that for the variables not available from worldClim, I increased or decreased them uniformly
# sensDat_2 <- sensDat %>% 
#   mutate(PrecipTempCorr_meanAnnAvg_30yr = PrecipTempCorr_meanAnnAvg_30yr*1.1,
#          avgOrganicCarbonPerc_0_3cm = avgOrganicCarbonPerc_0_3cm*1.1, 
#          annVPD_max_95percentile_30yr = annVPD_max_95percentile_30yr*1.1, 
#          avgCoarsePerc_acrossDepth = avgCoarsePerc_acrossDepth*1.1, 
#          surfaceClay_perc = surfaceClay_perc*1.1,
#          soilDepth = soilDepth*1.1)
# 
# regModPreds_2 <- sensDat_2 %>%
#   cbind(predict(testMod, newdata = sensDat_2, "probs")) %>% 
#   mutate(
#     newRegion_predicted = pmap_dbl(.[c("dryShrubGrass", "eastForest", "westForest")], max),
#     across(dryShrubGrass:westForest, ~.x == newRegion_predicted)) %>%
#   mutate(dryShrubGrass = str_replace(dryShrubGrass, 'TRUE', 'dryShrubGrass'),
#          eastForest = str_replace(eastForest, 'TRUE', "eastForest"),
#          westForest = str_replace(westForest, 'TRUE', "westForest"),
#          dryShrubGrass = str_replace(dryShrubGrass, 'FALSE', ""),
#          eastForest = str_replace(eastForest, 'FALSE', ""),
#          westForest = str_replace(westForest, 'FALSE', "")) %>%
#   mutate(
#     newRegion_group = as.factor(pmap_chr(.[c("dryShrubGrass", "eastForest", "westForest")], paste0))) %>% 
#   select(-dryShrubGrass, -eastForest, -westForest) %>% 
#   cbind(predict(testMod, newdata = sensDat_2, "probs")) %>% 
#   rename("dryShrubGrass_prob" = dryShrubGrass, 
#          "eastForest_prob" = eastForest, 
#          "westForest_prob" = westForest)
# 
# #//
# (regMod_2_PredMAP <- ggplot(regModPreds_2) +
#     geom_point(aes(Long, Lat, col = newRegion_group)) +
#     ggtitle("model-predicted ecoregion classification -- fit to a combination of contemporary data  +10% and data from 2041-2060 \nfrom CMCC-ESM2 model, ssp 585 ",
#             subtitle = paste0("model: ecoregion ~ precip of wettest Month  + precip of driest month + precip/temp corr. + isothermality + 
#             95th percentile of VPD max + soil depth + surface clay + coarse fraction + surface organic matter")) 
# )
# 
# # another try 
# sensDat_3 <- sensDat %>% 
#   mutate(PrecipTempCorr_meanAnnAvg_30yr = PrecipTempCorr_meanAnnAvg_30yr*.9,
#          avgOrganicCarbonPerc_0_3cm = avgOrganicCarbonPerc_0_3cm*.9, 
#          annVPD_max_95percentile_30yr = annVPD_max_95percentile_30yr*.9, 
#          avgCoarsePerc_acrossDepth = avgCoarsePerc_acrossDepth*.9, 
#          surfaceClay_perc = surfaceClay_perc*.9,
#          soilDepth = soilDepth*.9)
# 
# regModPreds_3 <- sensDat_3 %>%
#   cbind(predict(testMod, newdata = sensDat_3, "probs")) %>% 
#   mutate(
#     newRegion_predicted = pmap_dbl(.[c("dryShrubGrass", "eastForest", "westForest")], max),
#     across(dryShrubGrass:westForest, ~.x == newRegion_predicted)) %>%
#   mutate(dryShrubGrass = str_replace(dryShrubGrass, 'TRUE', 'dryShrubGrass'),
#          eastForest = str_replace(eastForest, 'TRUE', "eastForest"),
#          westForest = str_replace(westForest, 'TRUE', "westForest"),
#          dryShrubGrass = str_replace(dryShrubGrass, 'FALSE', ""),
#          eastForest = str_replace(eastForest, 'FALSE', ""),
#          westForest = str_replace(westForest, 'FALSE', "")) %>%
#   mutate(
#     newRegion_group = as.factor(pmap_chr(.[c("dryShrubGrass", "eastForest", "westForest")], paste0))) %>% 
#   select(-dryShrubGrass, -eastForest, -westForest) %>% 
#   cbind(predict(testMod, newdata = sensDat_3, "probs")) %>% 
#   rename("dryShrubGrass_prob" = dryShrubGrass, 
#          "eastForest_prob" = eastForest, 
#          "westForest_prob" = westForest)
# 
# #//
# (regMod_3_PredMAP <- ggplot(regModPreds_3) +
#     geom_point(aes(Long, Lat, col = newRegion_group)) +
#     ggtitle("model-predicted ecoregion classification -- fit to a combination of contemporary data -10% and data from 2041-2060 \nfrom CMCC-ESM2 model, ssp 585 ",
#             subtitle = paste0("model: ecoregion ~ precip of wettest Month  + precip of driest month + precip/temp corr. + isothermality + 
#             95th percentile of VPD max + soil depth + surface clay + coarse fraction + surface organic matter")) 
# )
# 
# 
# (maps <- ggarrange(regMod_PredMAP, regMod_2_PredMAP, regMod_3_PredMAP, ncol = 1)
# )
# # sensitivity analysis  
# 
# # use contemporary dataset (modDat_testNew)
# # names of variables to perturb 
# predictors <- c( "precip_wettestMonth_meanAnnAvg_30yr"  , 
#                  "precip_driestMonth_meanAnnAvg_30yr"    ,
#                  "PrecipTempCorr_meanAnnAvg_30yr"       ,
#                  "isothermality_meanAnnAvg_30yr"        ,
#                  "annVPD_max_95percentile_30yr"         ,  
#                  "soilDepth"                             ,
#                  "surfaceClay_perc"                     , 
#                  "avgCoarsePerc_acrossDepth"            ,  "avgOrganicCarbonPerc_0_3cm") 
# # perturbation function 
# perturbFun <- function(predictor, 
#                        predictData, 
#                        perturbVal, 
#                        model) {
#   #predictor <- predictors[1]
#   #predictData <- modDat_testNew
#   #perturbVal <- c(-.2, -.1, .1, .2)
#   #model <- testMod
#   predsOut <- vector(mode = "list", length(perturbVal))
#   # for each value of perturbVal 
#   if (predictor == "PrecipTempCorr_meanAnnAvg_30yr") {
#     for (i in 1:length(perturbVal)) {
#       predictDat_i <- predictData
#       # transform predictor value of choice
#       predictDat_i[,predictor] <-  predictDat_i[,predictor] + predictDat_i[,predictor]*perturbVal[i] 
#       # if the predicted values are less than -1 , change to -1
#       predictDat_i[ predictDat_i[,predictor] < -1 ,predictor] <- -1
#       # if the predicted values are greater than 1 , change to 1
#       predictDat_i[ predictDat_i[,predictor] > 1 ,predictor] <- 1
#       # predict 
#       regModPreds_i <- predictDat_i %>%
#         cbind(predict(model, newdata = predictDat_i, "probs")) %>% 
#         mutate(
#           newRegion_predicted = pmap_dbl(.[c("dryShrubGrass", "eastForest", "westForest")], max),
#           across(dryShrubGrass:westForest, ~.x == newRegion_predicted)) %>%
#         mutate(dryShrubGrass = str_replace(dryShrubGrass, 'TRUE', 'dryShrubGrass'),
#                eastForest = str_replace(eastForest, 'TRUE', "eastForest"),
#                westForest = str_replace(westForest, 'TRUE', "westForest"),
#                dryShrubGrass = str_replace(dryShrubGrass, 'FALSE', ""),
#                eastForest = str_replace(eastForest, 'FALSE', ""),
#                westForest = str_replace(westForest, 'FALSE', "")) %>%
#         mutate(
#           newRegion_group = as.factor(pmap_chr(.[c("dryShrubGrass", "eastForest", "westForest")], paste0))) %>% 
#         select(-dryShrubGrass, -eastForest, -westForest) %>% 
#         cbind(predict(testMod, newdata = predictDat_i, "probs")) %>% 
#         rename("dryShrubGrass_prob" = dryShrubGrass, 
#                "eastForest_prob" = eastForest, 
#                "westForest_prob" = westForest)
#       
#       # save predictions
#       predsOut[[i]] <- list("preds" = regModPreds_i,
#                             "perturbVal" = perturbVal[i],
#                             "perturbPredictor" = predictor)
#     }
#   } else if (predictor %in% c("surfaceClay_perc", "avgCoarsePerc_acrossDepth", "avgOrganicCarbonPerc_0_3cm") ) {
#     for (i in 1:length(perturbVal)) {
#       predictDat_i <- predictData
#       # transform predictor value of choice
#       predictDat_i[,predictor] <-  predictDat_i[,predictor] + predictDat_i[,predictor]*perturbVal[i] 
#       # if the predicted values are less than 0 , change to 0
#       predictDat_i[ predictDat_i[,predictor] < 0 ,predictor] <- 0
#       # if the predicted values are greater than 100 , change to 100
#       predictDat_i[ predictDat_i[,predictor] > 100 ,predictor] <- 100
#       # predict 
#       regModPreds_i <- predictDat_i %>%
#         cbind(predict(model, newdata = predictDat_i, "probs")) %>% 
#         mutate(
#           newRegion_predicted = pmap_dbl(.[c("dryShrubGrass", "eastForest", "westForest")], max),
#           across(dryShrubGrass:westForest, ~.x == newRegion_predicted)) %>%
#         mutate(dryShrubGrass = str_replace(dryShrubGrass, 'TRUE', 'dryShrubGrass'),
#                eastForest = str_replace(eastForest, 'TRUE', "eastForest"),
#                westForest = str_replace(westForest, 'TRUE', "westForest"),
#                dryShrubGrass = str_replace(dryShrubGrass, 'FALSE', ""),
#                eastForest = str_replace(eastForest, 'FALSE', ""),
#                westForest = str_replace(westForest, 'FALSE', "")) %>%
#         mutate(
#           newRegion_group = as.factor(pmap_chr(.[c("dryShrubGrass", "eastForest", "westForest")], paste0))) %>% 
#         select(-dryShrubGrass, -eastForest, -westForest) %>% 
#         cbind(predict(testMod, newdata = predictDat_i, "probs")) %>% 
#         rename("dryShrubGrass_prob" = dryShrubGrass, 
#                "eastForest_prob" = eastForest, 
#                "westForest_prob" = westForest)
#       
#       # save predictions
#       predsOut[[i]] <- list("preds" = regModPreds_i,
#                             "perturbVal" = perturbVal[i],
#                             "perturbPredictor" = predictor)
#     }
#   } else  {
#     for (i in 1:length(perturbVal)) {
#       predictDat_i <- predictData
#       # transform predictor value of choice
#       predictDat_i[,predictor] <-  predictDat_i[,predictor] + predictDat_i[,predictor]*perturbVal[i] 
#       # predict 
#       regModPreds_i <- predictDat_i %>%
#         cbind(predict(model, newdata = predictDat_i, "probs")) %>% 
#         mutate(
#           newRegion_predicted = pmap_dbl(.[c("dryShrubGrass", "eastForest", "westForest")], max),
#           across(dryShrubGrass:westForest, ~.x == newRegion_predicted)) %>%
#         mutate(dryShrubGrass = str_replace(dryShrubGrass, 'TRUE', 'dryShrubGrass'),
#                eastForest = str_replace(eastForest, 'TRUE', "eastForest"),
#                westForest = str_replace(westForest, 'TRUE', "westForest"),
#                dryShrubGrass = str_replace(dryShrubGrass, 'FALSE', ""),
#                eastForest = str_replace(eastForest, 'FALSE', ""),
#                westForest = str_replace(westForest, 'FALSE', "")) %>%
#         mutate(
#           newRegion_group = as.factor(pmap_chr(.[c("dryShrubGrass", "eastForest", "westForest")], paste0))) %>% 
#         select(-dryShrubGrass, -eastForest, -westForest) %>% 
#         cbind(predict(testMod, newdata = predictDat_i, "probs")) %>% 
#         rename("dryShrubGrass_prob" = dryShrubGrass, 
#                "eastForest_prob" = eastForest, 
#                "westForest_prob" = westForest)
#       
#       # save predictions
#       predsOut[[i]] <- list("preds" = regModPreds_i,
#                             "perturbVal" = perturbVal[i],
#                             "perturbPredictor" = predictor)
#     }
#   }
#   
#   return(predsOut)
# }
# 
# # predictions for precip of wettest year 
# preds_precipWettestMonth <- 
#   perturbFun(predictor = predictors[1],
#              predictData = modDat_testNew, 
#              perturbVal = c(-.3, -.2, -.1, .1, .2, .3),
#              model = testMod)
# 
# # predictions for precip of driest month
# preds_precipDriestMonth <- 
#   perturbFun(predictor = predictors[2],
#              predictData = modDat_testNew, 
#              perturbVal =  c(-.3, -.2, -.1, .1, .2, .3),
#              model = testMod)
# 
# # predictions for precip of wettest month 
# preds_precipTempCorr <- 
#   perturbFun(predictor = predictors[3],
#              predictData = modDat_testNew, 
#              perturbVal =  c(-.3, -.2, -.1, .1, .2, .3),
#              model = testMod)
# 
# # predictions for isothermality
# preds_isotherm <- 
#   perturbFun(predictor = predictors[4],
#              predictData = modDat_testNew, 
#              perturbVal =  c(-.3, -.2, -.1, .1, .2, .3),
#              model = testMod)
# 
# # predictions for VPD max 95th percentile
# preds_VPDmax_95 <- 
#   perturbFun(predictor = predictors[5],
#              predictData = modDat_testNew, 
#              perturbVal =  c(-.3, -.2, -.1, .1, .2, .3),
#              model = testMod)
# 
# # predictions for soil depth
# preds_soilDepth <- 
#   perturbFun(predictor = predictors[6],
#              predictData = modDat_testNew, 
#              perturbVal =  c(-.3, -.2, -.1, .1, .2, .3),
#              model = testMod)
# 
# # predictions for % clay in soil surface
# preds_clay <- 
#   perturbFun(predictor = predictors[7],
#              predictData = modDat_testNew, 
#              perturbVal =  c(-.3, -.2, -.1, .1, .2, .3),
#              model = testMod)
# 
# # predictions for % coarse soil parts across depth
# preds_coarse <- 
#   perturbFun(predictor = predictors[8],
#              predictData = modDat_testNew, 
#              perturbVal =  c(-.3, -.2, -.1, .1, .2, .3),
#              model = testMod)
# 
# # predictions for % surface organic matter in the soil 
# preds_carbon <- 
#   perturbFun(predictor = predictors[9],
#              predictData = modDat_testNew, 
#              perturbVal =  c(-.3, -.2, -.1, .1, .2, .3),
#              model = testMod)
# 
# 
# # make figures showing predictions 
# makeFigures_sens <- function(modPreds, 
#                              modPreds_noPerturb) {
#   ## add predictions together while keeping track of the perturbation 
#   for (i in 1:length(modPreds)){  
#     predictor_i <- modPreds[[i]]$perturbPredictor
#     perturb_i <- modPreds[[i]]$perturbVal
#     preds_i <- modPreds[[i]]$preds[,c(predictor_i, "dryShrubGrass_prob", "eastForest_prob", "westForest_prob")]
#     preds_i$perturbVal <- perturb_i
#     if (i==1) {
#       predsOut <- preds_i
#     } else {
#       predsOut <- rbind(predsOut, preds_i)
#     }
#   }
#   # add in values for predicions with no perturbation
#   predsOut_final <- rbind(predsOut,
#                           cbind(modPreds_noPerturb[,c(predictor_i, "dryShrubGrass_prob", "eastForest_prob", "westForest_prob")],
#                                 data.frame("perturbVal" = rep_len(0, length.out = nrow(modPreds_noPerturb))))
#   )
#   
#   names(predsOut_final)[1] <- "predictor"
#   # make a figure showing change in response predictions across levels of perturbation
#   shrubGrassPred_line<- ggplot(data = predsOut_final[predsOut_final$dryShrubGrass_prob >.5,]) + 
#     # geom_point(aes(x =  predictor, y = dryShrubGrass_prob, col = as.factor(perturbVal))) +
#     geom_smooth(aes(x = predictor, y = dryShrubGrass_prob, col = as.factor(perturbVal)), se = FALSE) +
#     theme_minimal() +
#     ggtitle("Sensitivity of shrub/grassland predicted probability") +
#     ylab("P(dry shrub/grassland)")+
#     xlab(predictor_i) +
#     labs(color = "perturb. value")
#   
#   westForestPred_line <- ggplot(data = predsOut_final[predsOut_final$westForest_prob >.5,]) + 
#     #geom_point(aes(x = predictor, y = westForest_prob, col = as.factor(perturbVal))) +
#     geom_smooth(aes(x = predictor, y = dryShrubGrass_prob, col = as.factor(perturbVal)), se = FALSE) +
#     theme_minimal() +
#     ggtitle("Sensitivity of western forest predicted probability")+
#     ylab("P(western forest)") +
#     xlab(predictor_i) +
#     labs(color = "perturb. value")
#   
#   eastForestPred_line <- ggplot(data = predsOut_final[predsOut_final$eastForest_prob >.5,]) + 
#     #geom_point(aes(x = predictor, y = eastForest_prob, col = as.factor(perturbVal))) +
#     geom_smooth(aes(x = predictor, y = dryShrubGrass_prob, col = as.factor(perturbVal)), se = FALSE) +
#     theme_minimal() +
#     ggtitle("Sensitivity of eastern forest predicted probability") +
#     ylab("P(eastern forest)") +
#     xlab(predictor_i) +
#     labs(color = "perturb. value")
#   
#   shrubGrassPred_point<- ggplot(data = predsOut_final[predsOut_final$dryShrubGrass_prob >.5,]) + 
#     geom_point(aes(x =  predictor, y = dryShrubGrass_prob, col = as.factor(perturbVal))) +
#     theme_minimal() +
#     ggtitle("Sensitivity of shrub/grassland predicted probability") +
#     ylab("P(dry shrub/grassland)")+
#     xlab(predictor_i) +
#     labs(color = "perturb. value")
#   
#   westForestPred_point <- ggplot(data = predsOut_final[predsOut_final$westForest_prob >.5,]) + 
#     geom_point(aes(x = predictor, y = westForest_prob, col = as.factor(perturbVal))) +
#     theme_minimal() +
#     ggtitle("Sensitivity of western forest predicted probability")+
#     ylab("P(western forest)") +
#     xlab(predictor_i) +
#     labs(color = "perturb. value")
#   
#   eastForestPred_point <- ggplot(data = predsOut_final[predsOut_final$eastForest_prob >.5,]) + 
#     geom_point(aes(x = predictor, y = eastForest_prob, col = as.factor(perturbVal))) +
#     theme_minimal() +
#     ggtitle("Sensitivity of eastern forest predicted probability") +
#     ylab("P(eastern forest)") +
#     xlab(predictor_i) +
#     labs(color = "perturb. value")
#   
#   ## combine into one figure
#   predMod <- ggarrange(shrubGrassPred_point, westForestPred_point, eastForestPred_point, 
#                        shrubGrassPred_line, westForestPred_line, eastForestPred_line,
#                        common.legend = TRUE, nrow = 2, ncol = 3) %>% 
#     annotate_figure(paste0("Impact of purturbations in ", predictor_i," on model predictions"))
#   
#   return(predMod)
# }
# 
# 
# ## make figures
# # predictions for precip of wettest year 
# (predsFits_precipWettestMonth <- makeFigures_sens(modPreds = preds_precipWettestMonth, modPreds_noPerturb = regModPreds)
# )
# 
# # predictions for precip of driest month
# predsFits_precipDriestMonth <- makeFigures_sens(modPreds = preds_precipDriestMonth, modPreds_noPerturb = regModPreds)
# 
# # predictions for precip of wettest month 
# predsFits_precipTempCorr <- makeFigures_sens(modPreds = preds_precipTempCorr, modPreds_noPerturb = regModPreds)
# 
# # predictions for isothermality
# predsFits_isotherm <- makeFigures_sens(modPreds = preds_isotherm, modPreds_noPerturb = regModPreds)
# 
# # predictions for VPD max 95th percentile
# predsFits_VPDmax_95 <- makeFigures_sens(modPreds = preds_VPDmax_95, modPreds_noPerturb = regModPreds)
# 
# # predictions for soil depth
# predsFits_soilDepth <- makeFigures_sens(modPreds = preds_soilDepth, modPreds_noPerturb = regModPreds)
# 
# # predictions for % clay in soil surface
# predsFits_clay <- makeFigures_sens(modPreds = preds_clay, modPreds_noPerturb = regModPreds)
# 
# # predictions for % coarse soil parts across depth
# predsFits_coarse <- makeFigures_sens(modPreds = preds_coarse, modPreds_noPerturb = regModPreds)
# 
# # predictions for % surface organic matter in the soil 
# predsFits_carbon <- makeFigures_sens(modPreds = preds_carbon, modPreds_noPerturb = regModPreds)
# 
# plots <- ggarrange(regMod_PredMAP, predsFits_precipWettestMonth,predsFits_precipDriestMonth, predsFits_precipTempCorr, 
#                    predsFits_isotherm, predsFits_VPDmax_95,predsFits_soilDepth, 
#                    predsFits_clay, predsFits_coarse, predsFits_carbon, ncol = 1) 
# 
# bitmap(file = "./Figures/EcoRegionModelFigures/RegressionModelSensitivity.bmp", 
#        width = 16, height = 40)
# plots
# dev.off()
# 
