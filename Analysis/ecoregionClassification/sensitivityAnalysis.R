## modules to load on Denali: 
## module load cray-R/4.1.2.0  gcc/11.2.0 gdal/3.0.4 proj/6.2.1 geos/3.8.1 cray-netcdf/4.8.1.1                    


# load packages -----------------------------------------------------------
# quick code chunk to calculate VPD
require(terra)
library(sf)
library(tidyverse)

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

# calculate VPD 95th percentile (from contemporary data) -------------------------------------------------------
# 
# # Calculate VPD from TMEAN & TMIN
# #calculate monthly SVP according to Williams et al NatCC 2012 supplementary material -  units haPa
# a0<-6.107799961
# a1<-0.4436518521
# a2<-0.01428945805
# a3<-0.0002650648471
# a4<-0.000003031240396
# a5<-0.00000002034080948
# a6<-0.00000000006136820929
# SVP2 <- 0.1*(a0+ tmean*(a1+ tmean *(a2+ tmean *(a3+ tmean *(a4+tmean *(a5 + tmean *a6))))))
# # # Running et al. assume RH drops to 100% overnight
# # AVP2 <- (0.6108 * exp(17.27 * tmin / (tmin + 237.3)))
# # # AVP2 <-stack(AVP2)
# # VPD <- (SVP2 - AVP2)
# 
# # calculate it in the same way we do previously
# VPD <- ((( a0+ tmean*(a1+ tmean *(a2+ tmean *(a3+ tmean *(a4	+ tmean *(a5	+ tmean *a6)))))))*100 -  (tmean))/1000
# 
# names(VPD) <- c("vpd_month01" ,"vpd_month02","vpd_month03", "vpd_month04", "vpd_month05", "vpd_month06",
#                 "vpd_month07", "vpd_month08", "vpd_month09", "vpd_month10", "vpd_month11", "vpd_month12")
# 
# 
# VPD_max <- terra::app(VPD,
#                        fun = max)
# plot(VPD_max)
# 
# ## get VPD as points
# VPD_max_points <- VPD_max %>% 
#   terra::extract(sampPoints, xy = TRUE)
# # modDat_fit[,c("annVPD_max_meanAnnAvg_30yr", "Long", "Lat")] %>% 
# #   ggplot() + 
# #   geom_point(aes(Long, Lat, col = annVPD_max_meanAnnAvg_30yr))
# # use writeRaster to export to a desired filepath

soilRast <- readRDS("./Data_processed/SoilsRaster.rds") %>% 
  terra::project(crs(bioClim))

# get data averaged over contemporary 30 year period
maxVPD_95_Rast <- modDat_use %>% 
  select(annVPD_max_95percentile_30yr, Long, Lat) %>% 
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
  select(PrecipTempCorr_meanAnnAvg_30yr, Long, Lat) %>% 
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
  select(organicCarbonPerc_2cm, x, y)

ggplot(soilCarbon) + 
  geom_point(aes(x, y, col = organicCarbonPerc_2cm))

#  get surface clay for the appropriate points from soils data ------------
# sample soils data for  points consistent w/ the other dataset

# use "soilPoints" from above
# get clay in the first 3 cm 
soilClay <- soilPoints %>% 
  select(clayPerc_2cm, x, y) %>% 
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
sensDat <- (precip_driestMonth_points %>% select(bio14))  %>% 
  cbind(ptCorr_points %>% select(mean) %>% rename(PrecipTempCorr_meanAnnAvg_30yr = mean)) %>% #contemporary data 
  cbind(soilCarbon %>% select(organicCarbonPerc_2cm)) %>% #contemporary data 
  cbind(maxVPD_95_points %>% select(mean) %>% rename(annVPD_max_95percentile_30yr = mean)) %>% #contemporary data 
  cbind(coarse %>% select(avgCoarsePerc_acrossDepth)) %>% #contemporary data 
  cbind(precip_points %>% select(MAP)) %>% 
  cbind(soilClay %>% select(surfaceClay_perc)) %>% #contemporary data
  cbind(soilDepth %>% select(soilDepth)) %>% #contemporary data 
  cbind(isotherm_points %>% select(bio03) %>% rename(isothermality_meanAnnAvg_30yr = bio03)) %>% 
  cbind(precip_wettestMonth_points %>% select(bio13) %>% rename(precip_wettestMonth_meanAnnAvg_30yr = bio13)) %>% 
  cbind(sampPoints) %>% 
  dplyr::rename(avgOrganicCarbonPerc_0_3cm = organicCarbonPerc_2cm,
         precip_driestMonth_meanAnnAvg_30yr = bio14,
         prcp_meanAnnTotal_30yr = MAP,
         Long = x, 
         Lat = y
         ) %>% 
  drop_na()


(comparePrecipDriest <- ggplot() + 
  geom_density(data = sensDat, aes(x = precip_driestMonth_meanAnnAvg_30yr), col = "red") + 
  geom_density(data = modDat_use, aes(precip_driestMonth_meanAnnAvg_30yr), col = "blue") + 
  ggtitle("Precip in driest month", subtitle = "red = end of century, blue = current"))

(comparePrecipWettest <- ggplot() + 
    geom_density(data = sensDat, aes(x = precip_wettestMonth_meanAnnAvg_30yr), col = "red") + 
    geom_density(data = modDat_use, aes(precip_wettestMonth_meanAnnAvg_30yr), col = "blue") + 
    ggtitle("Precip in wettest month", subtitle = "red = end of century, blue = current"))

# (comparePrecipTempCorr <- ggplot() + 
#   geom_density(data = sensDat, aes(x = PrecipTempCorr_meanAnnAvg_30yr), col = "red") + 
#   geom_density(data = modDat_use, aes(PrecipTempCorr_meanAnnAvg_30yr), col = "blue") + 
#   ggtitle("Precip/Temp correlation", subtitle = "red = end of century, blue = current"))

# (compareVPD_95 <- ggplot() + 
#     geom_density(data = sensDat, aes(x = annVPD_max_95percentile_30yr), col = "red") + 
#     geom_density(data = modDat_use, aes(annVPD_max_95percentile_30yr), col = "blue") + 
#     ggtitle("VPD max", subtitle = "red = end of century, blue = current"))

(compareIsothermality <- ggplot() + 
    geom_density(data = sensDat, aes(x = isothermality_meanAnnAvg_30yr), col = "red") + 
    geom_density(data = modDat_use, aes(isothermality_meanAnnAvg_30yr), col = "blue") + 
    ggtitle("isothermality", subtitle = "red = end of century, blue = current"))


(predFigures <- annotate_figure(ggarrange(comparePrecipDriest,
                                          comparePrecipWettest, 
                                          compareIsothermality), 
                top = "Mid-century climate values (2041-2060) from CMCC-ESM2 model, ssp 585"))
   

# read in model  ----------------------------------------------------------
# the model is calleds "testMod" 
# newRegionFact ~ precip_wettestMonth_meanAnnAvg_30yr  + 
# precip_driestMonth_meanAnnAvg_30yr    +
#   PrecipTempCorr_meanAnnAvg_30yr       +
#   isothermality_meanAnnAvg_30yr        +
#   annVPD_max_95percentile_30yr         +  
#   soilDepth                             +
#   surfaceClay_perc                     + 
#   avgCoarsePerc_acrossDepth            +  avgOrganicCarbonPerc_0_3cm

# predict categories based on this simple model

regModPreds <- sensDat %>%
  cbind(predict(testMod, newdata = sensDat, "probs")) %>% 
  mutate(
         newRegion_predicted = pmap_dbl(.[c("dryShrubGrass", "eastForest", "westForest")], max),
         across(dryShrubGrass:westForest, ~.x == newRegion_predicted)) %>%
  mutate(dryShrubGrass = str_replace(dryShrubGrass, 'TRUE', 'dryShrubGrass'),
         eastForest = str_replace(eastForest, 'TRUE', "eastForest"),
         westForest = str_replace(westForest, 'TRUE', "westForest"),
         dryShrubGrass = str_replace(dryShrubGrass, 'FALSE', ""),
         eastForest = str_replace(eastForest, 'FALSE', ""),
         westForest = str_replace(westForest, 'FALSE', "")) %>%
  mutate(
    newRegion_group = as.factor(pmap_chr(.[c("dryShrubGrass", "eastForest", "westForest")], paste0))) %>% 
  select(-dryShrubGrass, -eastForest, -westForest) %>% 
  cbind(predict(testMod, newdata = sensDat, "probs")) %>% 
  rename("dryShrubGrass_prob" = dryShrubGrass, 
         "eastForest_prob" = eastForest, 
         "westForest_prob" = westForest)

#//
(regMod_PredMAP <- ggplot(regModPreds) +
    geom_point(aes(Long, Lat, col = newRegion_group)) +
    ggtitle("model-predicted ecoregion classification -- fit to a combination of contemporary data and data from 2041-2060 \nfrom CMCC-ESM2 model, ssp 585 ",
            subtitle = paste0("model: ecoregion ~ precip of wettest Month  + precip of driest month + precip/temp corr. + isothermality + 
            95th percentile of VPD max + soil depth + surface clay + coarse fraction + surface organic matter")) 
  )


# predictions w/ mix of true forecasted values and faked forecasts --------

#"faked" meaning that for the variables not available from worldClim, I increased or decreased them uniformly
sensDat_2 <- sensDat %>% 
  mutate(PrecipTempCorr_meanAnnAvg_30yr = PrecipTempCorr_meanAnnAvg_30yr*1.1,
         avgOrganicCarbonPerc_0_3cm = avgOrganicCarbonPerc_0_3cm*1.1, 
         annVPD_max_95percentile_30yr = annVPD_max_95percentile_30yr*1.1, 
         avgCoarsePerc_acrossDepth = avgCoarsePerc_acrossDepth*1.1, 
         surfaceClay_perc = surfaceClay_perc*1.1,
         soilDepth = soilDepth*1.1)

regModPreds_2 <- sensDat_2 %>%
  cbind(predict(testMod, newdata = sensDat_2, "probs")) %>% 
  mutate(
    newRegion_predicted = pmap_dbl(.[c("dryShrubGrass", "eastForest", "westForest")], max),
    across(dryShrubGrass:westForest, ~.x == newRegion_predicted)) %>%
  mutate(dryShrubGrass = str_replace(dryShrubGrass, 'TRUE', 'dryShrubGrass'),
         eastForest = str_replace(eastForest, 'TRUE', "eastForest"),
         westForest = str_replace(westForest, 'TRUE', "westForest"),
         dryShrubGrass = str_replace(dryShrubGrass, 'FALSE', ""),
         eastForest = str_replace(eastForest, 'FALSE', ""),
         westForest = str_replace(westForest, 'FALSE', "")) %>%
  mutate(
    newRegion_group = as.factor(pmap_chr(.[c("dryShrubGrass", "eastForest", "westForest")], paste0))) %>% 
  select(-dryShrubGrass, -eastForest, -westForest) %>% 
  cbind(predict(testMod, newdata = sensDat_2, "probs")) %>% 
  rename("dryShrubGrass_prob" = dryShrubGrass, 
         "eastForest_prob" = eastForest, 
         "westForest_prob" = westForest)

#//
(regMod_2_PredMAP <- ggplot(regModPreds_2) +
    geom_point(aes(Long, Lat, col = newRegion_group)) +
    ggtitle("model-predicted ecoregion classification -- fit to a combination of contemporary data  +10% and data from 2041-2060 \nfrom CMCC-ESM2 model, ssp 585 ",
            subtitle = paste0("model: ecoregion ~ precip of wettest Month  + precip of driest month + precip/temp corr. + isothermality + 
            95th percentile of VPD max + soil depth + surface clay + coarse fraction + surface organic matter")) 
)

# another try 
sensDat_3 <- sensDat %>% 
  mutate(PrecipTempCorr_meanAnnAvg_30yr = PrecipTempCorr_meanAnnAvg_30yr*.9,
         avgOrganicCarbonPerc_0_3cm = avgOrganicCarbonPerc_0_3cm*.9, 
         annVPD_max_95percentile_30yr = annVPD_max_95percentile_30yr*.9, 
         avgCoarsePerc_acrossDepth = avgCoarsePerc_acrossDepth*.9, 
         surfaceClay_perc = surfaceClay_perc*.9,
         soilDepth = soilDepth*.9)

regModPreds_3 <- sensDat_3 %>%
  cbind(predict(testMod, newdata = sensDat_3, "probs")) %>% 
  mutate(
    newRegion_predicted = pmap_dbl(.[c("dryShrubGrass", "eastForest", "westForest")], max),
    across(dryShrubGrass:westForest, ~.x == newRegion_predicted)) %>%
  mutate(dryShrubGrass = str_replace(dryShrubGrass, 'TRUE', 'dryShrubGrass'),
         eastForest = str_replace(eastForest, 'TRUE', "eastForest"),
         westForest = str_replace(westForest, 'TRUE', "westForest"),
         dryShrubGrass = str_replace(dryShrubGrass, 'FALSE', ""),
         eastForest = str_replace(eastForest, 'FALSE', ""),
         westForest = str_replace(westForest, 'FALSE', "")) %>%
  mutate(
    newRegion_group = as.factor(pmap_chr(.[c("dryShrubGrass", "eastForest", "westForest")], paste0))) %>% 
  select(-dryShrubGrass, -eastForest, -westForest) %>% 
  cbind(predict(testMod, newdata = sensDat_3, "probs")) %>% 
  rename("dryShrubGrass_prob" = dryShrubGrass, 
         "eastForest_prob" = eastForest, 
         "westForest_prob" = westForest)

#//
(regMod_3_PredMAP <- ggplot(regModPreds_3) +
    geom_point(aes(Long, Lat, col = newRegion_group)) +
    ggtitle("model-predicted ecoregion classification -- fit to a combination of contemporary data -10% and data from 2041-2060 \nfrom CMCC-ESM2 model, ssp 585 ",
            subtitle = paste0("model: ecoregion ~ precip of wettest Month  + precip of driest month + precip/temp corr. + isothermality + 
            95th percentile of VPD max + soil depth + surface clay + coarse fraction + surface organic matter")) 
)


(maps <- ggarrange(regMod_PredMAP, regMod_2_PredMAP, regMod_3_PredMAP, ncol = 1)
)
# sensitivity analysis  ---------------------------------------------------

# use contemporary dataset (modDat_test)
# names of variables to perturb 
predictors <- c( "precip_wettestMonth_meanAnnAvg_30yr"  , 
                 "precip_driestMonth_meanAnnAvg_30yr"    ,
                 "PrecipTempCorr_meanAnnAvg_30yr"       ,
                 "isothermality_meanAnnAvg_30yr"        ,
                 "annVPD_max_95percentile_30yr"         ,  
                 "soilDepth"                             ,
                 "surfaceClay_perc"                     , 
                 "avgCoarsePerc_acrossDepth"            ,  "avgOrganicCarbonPerc_0_3cm") 
# perturbation function 
perturbFun <- function(predictor, 
                       predictData, 
                       perturbVal, 
                       model) {
  #predictor <- predictors[1]
  #predictData <- modDat_test
  #perturbVal <- c(-.2, -.1, .1, .2)
  #model <- testMod
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
        cbind(predict(model, newdata = predictDat_i, "probs")) %>% 
        mutate(
          newRegion_predicted = pmap_dbl(.[c("dryShrubGrass", "eastForest", "westForest")], max),
          across(dryShrubGrass:westForest, ~.x == newRegion_predicted)) %>%
        mutate(dryShrubGrass = str_replace(dryShrubGrass, 'TRUE', 'dryShrubGrass'),
               eastForest = str_replace(eastForest, 'TRUE', "eastForest"),
               westForest = str_replace(westForest, 'TRUE', "westForest"),
               dryShrubGrass = str_replace(dryShrubGrass, 'FALSE', ""),
               eastForest = str_replace(eastForest, 'FALSE', ""),
               westForest = str_replace(westForest, 'FALSE', "")) %>%
        mutate(
          newRegion_group = as.factor(pmap_chr(.[c("dryShrubGrass", "eastForest", "westForest")], paste0))) %>% 
        select(-dryShrubGrass, -eastForest, -westForest) %>% 
        cbind(predict(testMod, newdata = predictDat_i, "probs")) %>% 
        rename("dryShrubGrass_prob" = dryShrubGrass, 
               "eastForest_prob" = eastForest, 
               "westForest_prob" = westForest)
      
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
        cbind(predict(model, newdata = predictDat_i, "probs")) %>% 
        mutate(
          newRegion_predicted = pmap_dbl(.[c("dryShrubGrass", "eastForest", "westForest")], max),
          across(dryShrubGrass:westForest, ~.x == newRegion_predicted)) %>%
        mutate(dryShrubGrass = str_replace(dryShrubGrass, 'TRUE', 'dryShrubGrass'),
               eastForest = str_replace(eastForest, 'TRUE', "eastForest"),
               westForest = str_replace(westForest, 'TRUE', "westForest"),
               dryShrubGrass = str_replace(dryShrubGrass, 'FALSE', ""),
               eastForest = str_replace(eastForest, 'FALSE', ""),
               westForest = str_replace(westForest, 'FALSE', "")) %>%
        mutate(
          newRegion_group = as.factor(pmap_chr(.[c("dryShrubGrass", "eastForest", "westForest")], paste0))) %>% 
        select(-dryShrubGrass, -eastForest, -westForest) %>% 
        cbind(predict(testMod, newdata = predictDat_i, "probs")) %>% 
        rename("dryShrubGrass_prob" = dryShrubGrass, 
               "eastForest_prob" = eastForest, 
               "westForest_prob" = westForest)
      
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
        cbind(predict(model, newdata = predictDat_i, "probs")) %>% 
        mutate(
          newRegion_predicted = pmap_dbl(.[c("dryShrubGrass", "eastForest", "westForest")], max),
          across(dryShrubGrass:westForest, ~.x == newRegion_predicted)) %>%
        mutate(dryShrubGrass = str_replace(dryShrubGrass, 'TRUE', 'dryShrubGrass'),
               eastForest = str_replace(eastForest, 'TRUE', "eastForest"),
               westForest = str_replace(westForest, 'TRUE', "westForest"),
               dryShrubGrass = str_replace(dryShrubGrass, 'FALSE', ""),
               eastForest = str_replace(eastForest, 'FALSE', ""),
               westForest = str_replace(westForest, 'FALSE', "")) %>%
        mutate(
          newRegion_group = as.factor(pmap_chr(.[c("dryShrubGrass", "eastForest", "westForest")], paste0))) %>% 
        select(-dryShrubGrass, -eastForest, -westForest) %>% 
        cbind(predict(testMod, newdata = predictDat_i, "probs")) %>% 
        rename("dryShrubGrass_prob" = dryShrubGrass, 
               "eastForest_prob" = eastForest, 
               "westForest_prob" = westForest)
      
      # save predictions
      predsOut[[i]] <- list("preds" = regModPreds_i,
                            "perturbVal" = perturbVal[i],
                            "perturbPredictor" = predictor)
    }
  }
  
  return(predsOut)
}

# predictions for precip of wettest year 
preds_precipWettestMonth <- 
  perturbFun(predictor = predictors[1],
              predictData = modDat_test, 
             perturbVal = c(-.3, -.2, -.1, .1, .2, .3),
             model = testMod)

# predictions for precip of driest month
preds_precipDriestMonth <- 
  perturbFun(predictor = predictors[2],
             predictData = modDat_test, 
             perturbVal =  c(-.3, -.2, -.1, .1, .2, .3),
             model = testMod)

# predictions for precip of wettest month 
preds_precipTempCorr <- 
  perturbFun(predictor = predictors[3],
             predictData = modDat_test, 
             perturbVal =  c(-.3, -.2, -.1, .1, .2, .3),
             model = testMod)

# predictions for isothermality
preds_isotherm <- 
  perturbFun(predictor = predictors[4],
             predictData = modDat_test, 
             perturbVal =  c(-.3, -.2, -.1, .1, .2, .3),
             model = testMod)

# predictions for VPD max 95th percentile
preds_VPDmax_95 <- 
  perturbFun(predictor = predictors[5],
             predictData = modDat_test, 
             perturbVal =  c(-.3, -.2, -.1, .1, .2, .3),
             model = testMod)

# predictions for soil depth
preds_soilDepth <- 
  perturbFun(predictor = predictors[6],
             predictData = modDat_test, 
             perturbVal =  c(-.3, -.2, -.1, .1, .2, .3),
             model = testMod)

# predictions for % clay in soil surface
preds_clay <- 
  perturbFun(predictor = predictors[7],
             predictData = modDat_test, 
             perturbVal =  c(-.3, -.2, -.1, .1, .2, .3),
             model = testMod)

# predictions for % coarse soil parts across depth
preds_coarse <- 
  perturbFun(predictor = predictors[8],
             predictData = modDat_test, 
             perturbVal =  c(-.3, -.2, -.1, .1, .2, .3),
             model = testMod)

# predictions for % surface organic matter in the soil 
preds_carbon <- 
  perturbFun(predictor = predictors[9],
             predictData = modDat_test, 
             perturbVal =  c(-.3, -.2, -.1, .1, .2, .3),
             model = testMod)


# make figures showing predictions ----------------------------------------
makeFigures_sens <- function(modPreds, 
                             modPreds_noPerturb) {
  ## add predictions together while keeping track of the perturbation 
  for (i in 1:length(modPreds)){  
    predictor_i <- modPreds[[i]]$perturbPredictor
  perturb_i <- modPreds[[i]]$perturbVal
  preds_i <- modPreds[[i]]$preds[,c(predictor_i, "dryShrubGrass_prob", "eastForest_prob", "westForest_prob")]
  preds_i$perturbVal <- perturb_i
  if (i==1) {
    predsOut <- preds_i
  } else {
    predsOut <- rbind(predsOut, preds_i)
  }
  }
  # add in values for predicions with no perturbation
  predsOut_final <- rbind(predsOut,
  cbind(modPreds_noPerturb[,c(predictor_i, "dryShrubGrass_prob", "eastForest_prob", "westForest_prob")],
  data.frame("perturbVal" = rep_len(0, length.out = nrow(modPreds_noPerturb))))
  )

names(predsOut_final)[1] <- "predictor"
  # make a figure showing change in response predictions across levels of perturbation
  shrubGrassPred_line<- ggplot(data = predsOut_final[predsOut_final$dryShrubGrass_prob >.5,]) + 
   # geom_point(aes(x =  predictor, y = dryShrubGrass_prob, col = as.factor(perturbVal))) +
   geom_smooth(aes(x = predictor, y = dryShrubGrass_prob, col = as.factor(perturbVal)), se = FALSE) +
    theme_minimal() +
    ggtitle("Sensitivity of shrub/grassland predicted probability") +
    ylab("P(dry shrub/grassland)")+
    xlab(predictor_i) +
    labs(color = "perturb. value")
  
  westForestPred_line <- ggplot(data = predsOut_final[predsOut_final$westForest_prob >.5,]) + 
      #geom_point(aes(x = predictor, y = westForest_prob, col = as.factor(perturbVal))) +
      geom_smooth(aes(x = predictor, y = dryShrubGrass_prob, col = as.factor(perturbVal)), se = FALSE) +
      theme_minimal() +
      ggtitle("Sensitivity of western forest predicted probability")+
      ylab("P(western forest)") +
    xlab(predictor_i) +
    labs(color = "perturb. value")
  
  eastForestPred_line <- ggplot(data = predsOut_final[predsOut_final$eastForest_prob >.5,]) + 
      #geom_point(aes(x = predictor, y = eastForest_prob, col = as.factor(perturbVal))) +
      geom_smooth(aes(x = predictor, y = dryShrubGrass_prob, col = as.factor(perturbVal)), se = FALSE) +
      theme_minimal() +
      ggtitle("Sensitivity of eastern forest predicted probability") +
      ylab("P(eastern forest)") +
    xlab(predictor_i) +
    labs(color = "perturb. value")
  
  shrubGrassPred_point<- ggplot(data = predsOut_final[predsOut_final$dryShrubGrass_prob >.5,]) + 
    geom_point(aes(x =  predictor, y = dryShrubGrass_prob, col = as.factor(perturbVal))) +
    theme_minimal() +
    ggtitle("Sensitivity of shrub/grassland predicted probability") +
    ylab("P(dry shrub/grassland)")+
    xlab(predictor_i) +
    labs(color = "perturb. value")
  
  westForestPred_point <- ggplot(data = predsOut_final[predsOut_final$westForest_prob >.5,]) + 
    geom_point(aes(x = predictor, y = westForest_prob, col = as.factor(perturbVal))) +
    theme_minimal() +
    ggtitle("Sensitivity of western forest predicted probability")+
    ylab("P(western forest)") +
    xlab(predictor_i) +
    labs(color = "perturb. value")
  
  eastForestPred_point <- ggplot(data = predsOut_final[predsOut_final$eastForest_prob >.5,]) + 
    geom_point(aes(x = predictor, y = eastForest_prob, col = as.factor(perturbVal))) +
    theme_minimal() +
    ggtitle("Sensitivity of eastern forest predicted probability") +
    ylab("P(eastern forest)") +
    xlab(predictor_i) +
    labs(color = "perturb. value")
  
  ## combine into one figure
  predMod <- ggarrange(shrubGrassPred_point, westForestPred_point, eastForestPred_point, 
                       shrubGrassPred_line, westForestPred_line, eastForestPred_line,
                       common.legend = TRUE, nrow = 2, ncol = 3) %>% 
      annotate_figure(paste0("Impact of purturbations in ", predictor_i," on model predictions"))
  
  return(predMod)
}


## make figures
# predictions for precip of wettest year 
(predsFits_precipWettestMonth <- makeFigures_sens(modPreds = preds_precipWettestMonth, modPreds_noPerturb = regModPreds)
)

# predictions for precip of driest month
predsFits_precipDriestMonth <- makeFigures_sens(modPreds = preds_precipDriestMonth, modPreds_noPerturb = regModPreds)

# predictions for precip of wettest month 
predsFits_precipTempCorr <- makeFigures_sens(modPreds = preds_precipTempCorr, modPreds_noPerturb = regModPreds)

# predictions for isothermality
predsFits_isotherm <- makeFigures_sens(modPreds = preds_isotherm, modPreds_noPerturb = regModPreds)

# predictions for VPD max 95th percentile
predsFits_VPDmax_95 <- makeFigures_sens(modPreds = preds_VPDmax_95, modPreds_noPerturb = regModPreds)

# predictions for soil depth
predsFits_soilDepth <- makeFigures_sens(modPreds = preds_soilDepth, modPreds_noPerturb = regModPreds)

# predictions for % clay in soil surface
predsFits_clay <- makeFigures_sens(modPreds = preds_clay, modPreds_noPerturb = regModPreds)

# predictions for % coarse soil parts across depth
predsFits_coarse <- makeFigures_sens(modPreds = preds_coarse, modPreds_noPerturb = regModPreds)

# predictions for % surface organic matter in the soil 
predsFits_carbon <- makeFigures_sens(modPreds = preds_carbon, modPreds_noPerturb = regModPreds)

plots <- ggarrange(regMod_PredMAP, predsFits_precipWettestMonth,predsFits_precipDriestMonth, predsFits_precipTempCorr, 
          predsFits_isotherm, predsFits_VPDmax_95,predsFits_soilDepth, 
          predsFits_clay, predsFits_coarse, predsFits_carbon, ncol = 1) 

bitmap(file = "./Figures/EcoRegionModelFigures/RegressionModelSensitivity.bmp", 
       width = 16, height = 40)
plots
dev.off()

