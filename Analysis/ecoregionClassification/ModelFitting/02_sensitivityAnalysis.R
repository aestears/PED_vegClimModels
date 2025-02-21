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
#source("./Analysis/VegComposition/ModelFitting/01_EcoregionClassification.R")
#list all files in folder # arbitrarily chose end-century, highest ssp
#(ssp_585)

# # from CMCC-ESM2 model (could do another, this was just first on the list)
# # these are monthly values averaged over a 20 year period from 2081 to 2100
# # these data are at a 10 minute spatial resolution; each layer is a monthly value 
# # https://www.worldclim.org/data/cmip6/cmip6_clim10m.html
# ## read in bioClim variables
# bioClim <- rast("./Data_raw/worldClim_climateVars/futureClimate_midCentury_ssp585/CMCC-ESM2/wc2.1_10m_bioc_CMCC-ESM2_ssp585_2041-2060.tif")
# 
# # degrees C
# tmin <- rast("./Data_raw/worldClim_climateVars/futureClimate_midCentury_ssp585/CMCC-ESM2/wc2.1_10m_tmin_CMCC-ESM2_ssp585_2041-2060.tif")
# 
# # degrees C
# tmax <- rast("./Data_raw/worldClim_climateVars/futureClimate_midCentury_ssp585/CMCC-ESM2/wc2.1_10m_tmax_CMCC-ESM2_ssp585_2041-2060.tif")
# 
# # in mm
# precip <-rast("./Data_raw/worldClim_climateVars/futureClimate_midCentury_ssp585/CMCC-ESM2/wc2.1_10m_prec_CMCC-ESM2_ssp585_2041-2060.tif")

# ## try a different climate model 
# # from EC-Earth3-Veg model
# # these are monthly values averaged over a 20 year period from 2041 to 2060
# # these data are at a 10 minute spatial resolution; each layer is a monthly value
# # https://www.worldclim.org/data/cmip6/cmip6_clim10m.html
# ## read in bioClim variables
# bioClim <- rast("./Data_raw/worldClim_climateVars/futureClimate_midCentury_ssp585/EC-Earth3-Veg/wc2.1_10m_bioc_EC-Earth3-Veg_ssp585_2041-2060.tif")
# 
# # degrees C
# tmin <- rast("./Data_raw/worldClim_climateVars/futureClimate_midCentury_ssp585/EC-Earth3-Veg/wc2.1_10m_tmin_EC-Earth3-Veg_ssp585_2041-2060.tif")
# 
# # degrees C
# tmax <- rast("./Data_raw/worldClim_climateVars/futureClimate_midCentury_ssp585/EC-Earth3-Veg/wc2.1_10m_tmax_EC-Earth3-Veg_ssp585_2041-2060.tif")
# 
# # in mm
# precip <-rast("./Data_raw/worldClim_climateVars/futureClimate_midCentury_ssp585/EC-Earth3-Veg/wc2.1_10m_prec_EC-Earth3-Veg_ssp585_2041-2060.tif")
# 

## try a different climate model
# from GISS-E2-1-G model
# these are monthly values averaged over a 20 year period from 2041 to 2060
# these data are at a 10 minute spatial resolution; each layer is a monthly value
# https://www.worldclim.org/data/cmip6/cmip6_clim10m.html
## read in bioClim variables
bioClim <- rast("./Data_raw/worldClim_climateVars/futureClimate_midCentury_ssp585/GISS-E2-1-G/wc2.1_10m_bioc_GISS-E2-1-G_ssp585_2021-2040.tif")

# degrees C
tmin <- rast("./Data_raw/worldClim_climateVars/futureClimate_midCentury_ssp585/GISS-E2-1-G/wc2.1_10m_tmin_GISS-E2-1-G_ssp585_2021-2040.tif")

# degrees C
tmax <- rast("./Data_raw/worldClim_climateVars/futureClimate_midCentury_ssp585/GISS-E2-1-G/wc2.1_10m_tmax_GISS-E2-1-G_ssp585_2021-2040.tif")

# in mm
precip <-rast("./Data_raw/worldClim_climateVars/futureClimate_midCentury_ssp585/GISS-E2-1-G/wc2.1_10m_prec_GISS-E2-1-G_ssp585_2021-2040.tif")


# # crop to CONUS extent
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

# precip seasonality --------------------------------------------------
# this is bioclim variable 15
precipSeasonality <- bioClim$bio15
plot(precipSeasonality)

# ## why are the units so different from ours? -- forecast is between 6.6 and 111.0. while ours is between 0 and just over 2...
# testSeasonality <- terra::rast("~/Downloads/wc2.1_10m_bio/wc2.1_10m_bio_15.tif") %>% 
#   terra::project(y = crs(CONUS)) %>% 
#   terra::crop(CONUS) %>% 
#   terra::mask(CONUS) 
# plot(testSeasonality)

## get isothermality as points
precipSeasonality_points <- precipSeasonality %>% 
  terra::extract(sampPoints, xy = TRUE)


# get mean annual precip --------------------------------------------------
precip_points <- precip %>% 
  terra::extract(sampPoints, xy = TRUE) %>% 
  rowwise() %>% 
  mutate(MAP = mean(wc2.1_2.5m_prec_01:wc2.1_2.5m_prec_12))

ggplot(precip_points) + 
  geom_point(aes(x, y, col = MAP))

# annual water deficit ----------------------------------
# calculate tmean points
tmean_rast <- (tmax + tmin)/2
plot(tmean_rast)
names(tmean_rast) <- paste0("tmean_",1:12)
# tmean_points <- tmean_rast %>% 
#   terra::extract(sampPoints, xy = TRUE)

# calculate first step of water deficit
monthlyWatDeficit_TEMP <- tmean_rast*2 - precip
# when values are less than 0, replace with an NA
monthlyWatDeficit_rast <- classify(x = monthlyWatDeficit_TEMP, rcl = matrix(c(-100000,0,0), ncol = 3))
# extract to points 
annWatDeficit_rast <- sum(monthlyWatDeficit_rast)
plot(annWatDeficit_rast)

annWatDeficit_points <- annWatDeficit_rast %>% 
  terra::extract(sampPoints, xy = TRUE)
  
# mutate(
#   # monthly water deficit 
#   awd_Jan = tmean_Jan*2 - prcp_Jan,
#   awd_Feb = tmean_Feb*2 - prcp_Feb,
#   awd_March = tmean_March*2 - prcp_March,
#   awd_April = tmean_April*2 - prcp_April,
#   awd_May = tmean_May*2 - prcp_May,
#   awd_June = tmean_June*2 - prcp_June, 
#   awd_July = tmean_July*2 - prcp_July,
#   awd_Aug = tmean_Aug*2 - prcp_Aug, 
#   awd_Sept = tmean_Sept*2 - prcp_Sept, 
#   awd_Oct = tmean_Oct*2 - prcp_Oct,   
#   awd_Nov = tmean_Nov*2 - prcp_Nov,   
#   awd_Dec = tmean_Dec*2 - prcp_Dec



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
  cbind(precipSeasonality_points %>% dplyr::select(bio15) %>% rename(precip_Seasonality_meanAnnAvg_30yr = bio15)) %>% 
  cbind(annWatDeficit_points %>% dplyr::select(sum) %>% rename(annWaterDeficit_meanAnnAvg_30yr = sum)) %>% 
  cbind(sampPoints) %>% 
  dplyr::rename(avgOrganicCarbonPerc_0_3cm = organicCarbonPerc_2cm,
         precip_driestMonth_meanAnnAvg_30yr = bio14,
         prcp_meanAnnTotal_30yr = MAP,
         Long = x, 
         Lat = y
         ) %>% 
  drop_na()

## precip water deficit
(compareAnnWatDef <- ggplot() +
    geom_density(data = sensDat, aes(x = annWaterDeficit_meanAnnAvg_30yr), col = "red") +
    geom_density(data = modDat_use, aes(annWaterDeficit_meanAnnAvg_30yr), col = "blue") +
    ggtitle("Annual Water Deficit", subtitle = "red = end of century, blue = current"))

# predicted values
predsTemp <- (sensDat %>%
                terra::vect(geom = c("Long", "Lat"), crs = crs(ecoregionsSF2)) %>%
                terra::rasterize(y = bioClim
                                 , field = "annWaterDeficit_meanAnnAvg_30yr"))
# contemporary values
contempTemp <- (modDat_use %>%
                  terra::vect(geom = c("Long", "Lat"), crs = crs(soilRastTemp)) %>%
                  terra::project(crs(ptCorrRast)) %>%
                  terra::rasterize(y = bioClim, field = "annWaterDeficit_meanAnnAvg_30yr"))
plotDatTemp <- predsTemp - contempTemp

(compareAnnWaterDef_2 <- ggplot() +
    geom_spatraster(data = plotDatTemp) +
    guides(fill = guide_legend("Forecast - \n Contemporary")) +
    ggtitle("Annual Water Deficit")+
    scale_fill_gradient2(low = 'darkgreen', mid = 'white', high = 'orchid', midpoint =0))


## precip seasonality 
(comparePrecipSeasonality <- ggplot() +
    geom_density(data = sensDat, aes(x = precip_Seasonality_meanAnnAvg_30yr), col = "red") +
    geom_density(data = modDat_use, aes(precip_Seasonality_meanAnnAvg_30yr), col = "blue") +
    ggtitle("Precip seasonality", subtitle = "red = end of century, blue = current"))

# predicted values
predsTemp <- (sensDat %>%
            terra::vect(geom = c("Long", "Lat"), crs = crs(ecoregionsSF2)) %>%
            terra::rasterize(y = bioClim
                             , field = "precip_Seasonality_meanAnnAvg_30yr"))
# contemporary values
contempTemp <- (modDat_use %>%
              terra::vect(geom = c("Long", "Lat"), crs = crs(soilRastTemp)) %>%
              terra::project(crs(ptCorrRast)) %>%
              terra::rasterize(y = bioClim, field = "precip_Seasonality_meanAnnAvg_30yr"))
plotDatTemp <- predsTemp - contempTemp

(comparePrecipSeasonality_2 <- ggplot() +
  geom_spatraster(data = plotDatTemp) +
  guides(fill = guide_legend("Forecast - \n Contemporary")) +
  ggtitle("Precip Seasonality")+
    scale_fill_gradient2(low = 'darkgreen', mid = 'white', high = 'orchid', midpoint =0))

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
    ggtitle("Precip of the Wettest Month") + 
    scale_fill_gradient2(low = 'darkgreen', mid = 'white', high = 'orchid', midpoint =0))

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
    ggtitle("Temp of the Warmest Month")+ 
    scale_fill_gradient2(low = 'darkgreen', mid = 'white', high = 'orchid', midpoint =0))

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
    ggtitle("Temp of the Coldest Month")+ 
    scale_fill_gradient2(low = 'darkgreen', mid = 'white', high = 'orchid', midpoint =0))

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
    ggtitle("Isothermality")+ 
    scale_fill_gradient2(low = 'darkgreen', mid = 'white', high = 'orchid', midpoint =0))


# read in No VPD model w/ 2 ecoregions ------------------------------------------------
modUse <- testMod_5 
outputFile <- "CompareModelForecastsWithDifferentClimMods"#"Model_REPLACESPrecipDriestMonth_withAnnWatDef"


# compare predictions to contemporary data --------------------------------

bitmap(file = paste0("./Figures/EcoRegionModelFigures/",outputFile, "/ComparingContemporaryAndForecastData.bmp"), 
       width = 16, height = 20, res = 200)

patchwork::plot_layout(#(comparePrecipDriest+
  #comparePrecipDriest_2)/
  #(comparePrecipSeasonality + comparePrecipSeasonality_2)/
  (compareAnnWatDef + compareAnnWaterDef_2)/
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

# the model is called "modUse" 
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
  cbind("newRegion_pred" = predict(modUse, newdata = sensDat, "response")) %>%  rowwise() %>% 
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
            subtitle = paste0("model: ecoregion ~", str_flatten(names(coefficients(modUse))[2:4], collapse = " + ") , " + \n",
            str_flatten(names(coefficients(modUse))[5:length(coefficients(modUse))], collapse = " + "))) +
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
  cbind("newRegion_pred" = predict(modUse, newdata = sensDat_2b, "response")) %>%  rowwise() %>% 
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
            subtitle = paste0("model: ecoregion ~", str_flatten(names(coefficients(modUse))[2:4], collapse = " + ") , " + \n",
                              str_flatten(names(coefficients(modUse))[5:length(coefficients(modUse))]), collapse = " + ")) +
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
  cbind("newRegion_pred" = predict(modUse, newdata = sensDat_2c, "response")) %>%  rowwise() %>% 
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
            subtitle = paste0("model: ecoregion ~", str_flatten(names(coefficients(modUse))[2:4], collapse = " + ") , " + \n",
                              str_flatten(names(coefficients(modUse))[5:length(coefficients(modUse))]), collapse = " + ")) +
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

predictors <- names(coefficients(modUse))[-1][1:10] 
# perturbation function 
perturbFun_2 <- function(predictor, 
                       predictData, 
                       perturbVal, 
                       model) {
  #predictor <- predictors[1]
  #predictData <- modDat_testNew
  #perturbVal <- c(-.2, -.1, .1, .2)
  #model <- model
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
        cbind("newRegion_pred" = predict(model, newdata = predictDat_i, "response")) %>%  
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
        cbind("newRegion_pred" = predict(model, newdata = predictDat_i, "response")) %>%  
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
        cbind("newRegion_pred" = predict(model, newdata = predictDat_i, "response")) %>%  
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

# generate predictions for each predictor variable
lapply(predictors, FUN = function(x) {
  temp <-  perturbFun_2(predictor = x,
                        predictData = modDat_testNew, 
                        perturbVal = c(-.4, -.25, -.1, .1, .25, .4),
                        model = modUse)
  assign(x = paste0("preds_",x), value = temp, pos = ".GlobalEnv")
})


# make figures showing predictions ----------------------------------------
# get model predictions w/ no perturbations 
predictionDat <- modDat_testNew
predictionDatTemp <- cbind(predictionDat,"newRegion_pred" = predict(modUse, newdata = predictionDat, "response"))
# for each observation, determine which category has the highest predicted probability
(predData_noPerturb <- predictionDatTemp %>%
    rowwise() %>% 
    mutate(newRegion_group = newRegion_pred) %>% 
    mutate(newRegion_group = replace(newRegion_group, newRegion_pred<.5, "dryShrubGrass"), 
           newRegion_group = replace(newRegion_group, newRegion_pred>.5, "forest")) 
)

# make figures
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
  # predsOut_final[predsOut_final$newRegion_group == "dryShrubGrass","newRegion_pred"] <- 1-
  #   predsOut_final[predsOut_final$newRegion_group == "dryShrubGrass","newRegion_pred"] 
  
  names(predsOut_final)[1] <- "predictor"
  
  # calculate the mean response prediction for each 'bin' of the predictor value
  quantiles <- stats::quantile(predsOut_final[[1]], c(0, .09,.1,.19, .2, .29, .3, .39, .4, .49, 
                                         .5, .59, .6, .69, .7, .79, .8, .89, .9, 1))
  predsOut_final$quantile <- NA
  predsOut_final[predsOut_final$predictor > quantiles[1] & predsOut_final$predictor <= quantiles[2],"quantile"] <- quantiles[2]  
  predsOut_final[predsOut_final$predictor > quantiles[3] & predsOut_final$predictor <= quantiles[4],"quantile"] <- quantiles[4]  
  predsOut_final[predsOut_final$predictor > quantiles[5] & predsOut_final$predictor <= quantiles[6],"quantile"] <- quantiles[6]  
  predsOut_final[predsOut_final$predictor > quantiles[7] & predsOut_final$predictor <= quantiles[8],"quantile"] <- quantiles[8]  
  predsOut_final[predsOut_final$predictor > quantiles[9] & predsOut_final$predictor <= quantiles[10],"quantile"] <- quantiles[10]  
  predsOut_final[predsOut_final$predictor > quantiles[11] & predsOut_final$predictor <= quantiles[12],"quantile"] <- quantiles[12] 
  predsOut_final[predsOut_final$predictor > quantiles[13] & predsOut_final$predictor <= quantiles[14],"quantile"] <- quantiles[14]   
  predsOut_final[predsOut_final$predictor > quantiles[15] & predsOut_final$predictor <= quantiles[16],"quantile"] <- quantiles[16]
  predsOut_final[predsOut_final$predictor > quantiles[17] & predsOut_final$predictor <= quantiles[18],"quantile"] <- quantiles[18] 
  predsOut_final[predsOut_final$predictor > quantiles[19] & predsOut_final$predictor <= quantiles[20],"quantile"] <- quantiles[20] 
  
  figDat <- predsOut_final %>% 
    group_by(quantile, perturbVal) %>% 
    summarize(meanPredictedNewRegionProb = mean(newRegion_pred))
  
  # make a figure showing change in response predictions across levels of perturbation
  shrubGrassPred_line <- ggplot(data = figDat) + 
    # geom_point(aes(x =  predictor, y = dryShrubGrass_prob, col = as.factor(perturbVal))) +
    # geom_smooth(aes(x = predictor, y = newRegion_pred, col = as.factor(perturbVal)), #method = "glm", 
    #             se = FALSE) +
    geom_hline(aes(yintercept = .5), col = "grey50", lty = 2) +
      geom_line(aes(x = quantile, y = meanPredictedNewRegionProb, col = as.factor(perturbVal))) +
    #facet_wrap(~newRegion_group) +
    theme_minimal() +
    ggtitle(paste0("Impact of Perturbations in ",predictor_i," on Ecoregion predicted probability")) +
    ylab("Mean prediction P(Forest)")+
    xlab(predictor_i) +
    labs(color = "perturb. value")

  # shrubGrassPred_point <- ggplot(data = predsOut_final) + 
  #   geom_point(aes(x =  predictor, y = newRegion_pred, col = as.factor(perturbVal))) +
  #   facet_wrap(~newRegion_group) +
  #   theme_minimal() +
  #   ggtitle("Sensitivity of Ecoregion predicted probability") +
  #   ylab("P(Ecoregion Assignment)")+
  #   xlab(predictor_i) +
  #   labs(color = "perturb. value")
  
  ## combine into one figure
  predMod <- #ggarrange(shrubGrassPred_point,
                       shrubGrassPred_line
  #,common.legend = TRUE, nrow = 2, ncol = 1) %>% 
    #annotate_figure(paste0("Impact of purturbations in ", predictor_i," on model predictions"))
  
  return(predMod)
}

## make figures

lapply(predictors, FUN = function(x) {
  temp <-  makeFigures_sens_2(modPreds = get(paste0(x = paste0("preds_",x))), 
                              modPreds_noPerturb = predData_noPerturb)

  assign(x = paste0("predsFits_",x), value = temp, pos = ".GlobalEnv")
})


# make partial dependence plots -------------------------------------------

lapply(predictors, FUN = function(x) {
  temp <- pdp::partial(modUse, pred.var = c(x), 
               prob = TRUE, rug = TRUE, plot = TRUE)
  temp <- update(temp, main = paste(x), ylim = c(0,1))
  assign(x = paste0("pdp_",x), value = temp, pos = ".GlobalEnv")
})

# save figures
bitmap(file = paste0("./Figures/EcoRegionModelFigures/",outputFile, "/PartialDependencePlots.bmp"), 
       width = 16, height =11, res = 300)
print(get(paste0("pdp_",predictors[1])), split = c(1,1,4,3), more = TRUE)
print(get(paste0("pdp_",predictors[2])), split = c(2,1,4,3), more = TRUE)
print(get(paste0("pdp_",predictors[3])), split = c(3,1,4,3), more = TRUE)
print(get(paste0("pdp_",predictors[4])), split = c(4,1,4,3), more = TRUE)
print(get(paste0("pdp_",predictors[5])), split = c(1,2,4,3), more = TRUE)
print(get(paste0("pdp_",predictors[6])), split = c(2,2,4,3), more = TRUE)
print(get(paste0("pdp_",predictors[7])), split = c(3,2,4,3), more = TRUE)
print(get(paste0("pdp_",predictors[8])), split = c(4,2,4,3), more = TRUE)
print(get(paste0("pdp_",predictors[9])), split = c(1,3,4,3), more = TRUE)
print(get(paste0("pdp_",predictors[10])), split = c(2,3,4,3), more = FALSE)
dev.off()


# make maps of perturbations for a few important variables ----------------
lapply(predictors, FUN = function(x) {
  ## 'high' perturbations
  # rasterize data
  tempDat <-  get(paste0("preds_",x))[[6]]$preds %>%
    mutate(newRegion_group = newRegion_pred) %>% 
    mutate(newRegion_group = replace(newRegion_group, newRegion_pred<.5, "dryShrubGrass"), 
           newRegion_group = replace(newRegion_group, newRegion_pred>.5, "forest")) %>% 
    terra::vect(geom = c("Long", "Lat"), crs = crs(soilRastTemp)) %>% 
    terra::rasterize(y = 
                       terra::aggregate(soilRastTemp, fact = 2, fun = mean)
                     , field = "newRegion_pred")
  # make plot
  (tempPlot <- ggplot() +
      geom_spatraster(data = tempDat) +
      # geom_point(aes(#Long, Lat, 
      #   col = newRegion_pred)) +
      ggtitle(paste0("model-predicted ecoregion classification -- fit to contemporary data w/ 
            40% *increase* in ",x)) +
      #scale_fill_discrete(type = c("#bf812d","#35978f")) +
      scale_fill_distiller(type = "div", palette = 1, direction = 1) +
      geom_sf(data = ecoregionsSF2, color = "black", fill = NA, lwd = 1.25) )
  # save plot
  assign(x = paste0(x,"_PerturbMAP_high"), tempPlot, pos = ".GlobalEnv")
  
  ## 'low' perturbations
  # rasterize data
  tempDat <-  get(paste0("preds_",x))[[1]]$preds %>%
    mutate(newRegion_group = newRegion_pred) %>% 
    mutate(newRegion_group = replace(newRegion_group, newRegion_pred<.5, "dryShrubGrass"), 
           newRegion_group = replace(newRegion_group, newRegion_pred>.5, "forest")) %>% 
    terra::vect(geom = c("Long", "Lat"), crs = crs(soilRastTemp)) %>% 
    terra::rasterize(y = 
                       terra::aggregate(soilRastTemp, fact = 2, fun = mean)
                     , field = "newRegion_pred")
  # make plot
  (tempPlot <- ggplot() +
      geom_spatraster(data = tempDat) +
      # geom_point(aes(#Long, Lat, 
      #   col = newRegion_pred)) +
      ggtitle(paste0("model-predicted ecoregion classification -- fit to contemporary data w/ 
            40% *decrease* in ",x)) +
      #scale_fill_discrete(type = c("#bf812d","#35978f")) +
      scale_fill_distiller(type = "div", palette = 1, direction = 1) +
      geom_sf(data = ecoregionsSF2, color = "black", fill = NA, lwd = 1.25) )
  # save plot
  assign(x = paste0(x,"_PerturbMAP_low"), tempPlot, pos = ".GlobalEnv")
  
})


# make maps of contemporary data + one forecasted value ------------------------

lapply(predictors, FUN = function(x) {
  # predicted values 
  predsTemp <- (sensDat %>% 
                  terra::vect(geom = c("Long", "Lat"), crs = crs(ecoregionsSF2)) %>% 
                  terra::rasterize(y = bioClim
                                   , field = x))
  names(predsTemp) <- paste(x)
  # contemporary values
  contempTemp <- (modDat_testNew %>% 
                    terra::vect(geom = c("Long", "Lat"), crs = crs(soilRastTemp)) %>% 
                    terra::project(crs(ptCorrRast)) %>% 
                    terra::rasterize(y = bioClim, field = predictors[!str_detect(string = predictors, pattern = x)]
                                       ))
  
  plotDatTemp <- c(predsTemp, contempTemp)
  centroids <- crds(plotDatTemp) %>%  data.frame()# %>% terra::vect(crs = crs(soilRastTemp))
  plotDatPoints <- terra::extract(x = plotDatTemp, 
                                  y = centroids, method = "bilinear", 
                                  xy = TRUE)
  # predict the model output
  predictionDat <- plotDatPoints
  predictionDatTemp <- cbind(predictionDat,"newRegion_pred" = predict(modUse, newdata = predictionDat, "response"))
  # rasterize the predictions
  
  ForecastRast <- predictionDatTemp %>%
    mutate(newRegion_group = newRegion_pred) %>% 
    mutate(newRegion_group = replace(newRegion_group, newRegion_pred<.5, "dryShrubGrass"), 
           newRegion_group = replace(newRegion_group, newRegion_pred>.5, "forest")) %>% 
    terra::vect(geom = c("x", "y"), crs = crs(plotDatTemp)) %>% 
    terra::rasterize(y = 
                       terra::aggregate(plotDatTemp, fact = 2, fun = mean)
                     , field = "newRegion_pred")
# make map
  temp <- ggplot() +
      geom_spatraster(data = ForecastRast) +
      # geom_point(aes(#Long, Lat, 
      #   col = newRegion_pred)) +
      ggtitle(paste0("model-predicted ecoregion classification -- fit to contemporary data + forecasted data for ", x)) +
      #scale_fill_discrete(type = c("#bf812d","#35978f")) +
      scale_fill_distiller(type = "div", palette = 1, direction = 1) +
      geom_sf(data = ecoregionsSF2, color = "black", fill = NA, lwd = 1.25) 
  
  
  assign(paste0(x, "_Forecast_MAP"), temp, pos = ".GlobalEnv")
})


# make figures to save ----------------------------------------------------
# predictions + sensitivity figures
plots <- ggarrange(regMod2_PredMAP, regMod2b_PredMAP, regMod2c_PredMAP, 
                   ggarrange
                   (get(paste0("predsFits_",predictors[1])),
                     get(paste0("predsFits_",predictors[2])),
                     get(paste0("predsFits_",predictors[3])),
                     get(paste0("predsFits_",predictors[4])),
                     get(paste0("predsFits_",predictors[5])),
                     get(paste0("predsFits_",predictors[6])),
                     get(paste0("predsFits_",predictors[7])),
                     get(paste0("predsFits_",predictors[8])),
                     get(paste0("predsFits_",predictors[9])),
                     get(paste0("predsFits_",predictors[10])),
                     ncol = 2, nrow = 5)
                  , ncol = 1) %>% 
  ggpubr::annotate_figure(top = str_wrap(
                    paste0("Model: ecoregion ~ ", modUse$formula[3]), 
                    width = 80
                  )
                  )



bitmap(file = paste0("./Figures/EcoRegionModelFigures/", outputFile,"/SensitivityFigure_ForecastMaps.bmp"), 
       width = 20, height = 55, res = 250)
plots
dev.off()

# plots of perturbations w/ contemporary data + perturbation of one variable 
(predictionPlots <- ggarrange(#precipDriestMonthPerturb_MAP_high, precipDriestMonthPerturb_MAP_low, 
  get(paste0(predictors[1],"_PerturbMAP_low")), get(paste0(predictors[1],"_PerturbMAP_high")),
  get(paste0(predictors[2],"_PerturbMAP_low")), get(paste0(predictors[2],"_PerturbMAP_high")),
  get(paste0(predictors[3],"_PerturbMAP_low")), get(paste0(predictors[3],"_PerturbMAP_high")),
  get(paste0(predictors[4],"_PerturbMAP_low")), get(paste0(predictors[4],"_PerturbMAP_high")),
  get(paste0(predictors[5],"_PerturbMAP_low")), get(paste0(predictors[5],"_PerturbMAP_high")),
  get(paste0(predictors[6],"_PerturbMAP_low")), get(paste0(predictors[6],"_PerturbMAP_high")),
  get(paste0(predictors[7],"_PerturbMAP_low")), get(paste0(predictors[7],"_PerturbMAP_high")),
  get(paste0(predictors[8],"_PerturbMAP_low")), get(paste0(predictors[8],"_PerturbMAP_high")),
  get(paste0(predictors[9],"_PerturbMAP_low")), get(paste0(predictors[9],"_PerturbMAP_high")),
  get(paste0(predictors[10],"_PerturbMAP_low")), get(paste0(predictors[10],"_PerturbMAP_high")),
  ncol = 2, nrow = length(predictors)) %>% 
    ggpubr::annotate_figure(top = str_wrap(
    paste0("Model: ecoregion ~ ", modUse$formula[3]), 
    width = 80
  )
  )
)

bitmap(file = paste0("./Figures/EcoRegionModelFigures/", outputFile,"/SingleVarPerturbationMaps.bmp"), 
       width = 15, height = 55, res = 250)
predictionPlots
dev.off()


# forecasted predictors (excluding precipTempCorr and soil variables)
forecastPreds <- predictors[!(str_detect(predictors, "PrecipTempCorr") + 
                              str_detect(predictors, "soilDepth") + 
                               str_detect(predictors, "Sand") + 
                               str_detect(predictors, "Coarse"))]
## plots of contemporary + forecast for one variable
(predictionPlots_Mixed <- ggarrange(
  get(paste0(forecastPreds[1],"_Forecast_MAP")),
  get(paste0(forecastPreds[2],"_Forecast_MAP")),
  get(paste0(forecastPreds[3],"_Forecast_MAP")),
  get(paste0(forecastPreds[4],"_Forecast_MAP")),
  get(paste0(forecastPreds[5],"_Forecast_MAP")),
  get(paste0(forecastPreds[6],"_Forecast_MAP")), ncol = 1, nrow = length(forecastPreds)) %>% 
    ggpubr::annotate_figure(top = str_trunc(str_wrap(
      paste0("Model: ecoregion ~ ", modUse$formula[3]), 
      width = 80
    ), width = 320)
    )
)

bitmap(file = paste0("./Figures/EcoRegionModelFigures/", outputFile, "/ModelPreds_contempPlusOneForecastVar.bmp"), 
       width = 15, height = 30, res = 250)
predictionPlots_Mixed
dev.off()

