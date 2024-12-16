## modules to load on Denali: 
## module load cray-R/4.1.2.0  gcc/11.2.0 gdal/3.0.4 proj/6.2.1 geos/3.8.1 cray-netcdf/4.8.1.1                    


# load packages -----------------------------------------------------------
# quick code chunk to calculate VPD
require(terra)
library(sf)

# tmin_path <- "/caldera/projects/usgs/ecosystems/swbsc/DrylandEcohydrologyLab/ENM_Projects/input_data/raw/raw_historic_tmin"
# tmean_path <- "/caldera/projects/usgs/ecosystems/swbsc/DrylandEcohydrologyLab/ENM_Projects/input_data/raw/raw_historic_tavg"
#climVar_path <- "/caldera/projects/usgs/ecosystems/swbsc/DrylandEcohydrologyLab/ENM_Projects/input_data/raw/raw_future_climate"


# load data from bioclim --------------------------------------------------
# load model data 
source("./Analysis/VegComposition/ModelFitting/01_EcoregionClassification.R")
#list all files in folder # arbitrarily chose end-century, highest ssp
#(ssp_585)
# from ACCESS-CM2 model (could do another, this was just first on the list)
# these are monthly values averaged over a 20 year period from 2081 to 2100
# these data are at a 10 minute spatial resolution; each layer is a monthly value 
# https://www.worldclim.org/data/cmip6/cmip6_clim10m.html
## read in bioClim variables 
bioClim <- rast("./Data_raw/worldClim_climateVars/futureClimate_endCentury_ssp585/ACCESS-CM2/wc2.1_10m_bioc_ACCESS-CM2_ssp585_2081-2100.tif")

tmin <- rast("./Data_raw/worldClim_climateVars/futureClimate_endCentury_ssp585/ACCESS-CM2/wc2.1_10m_tmin_ACCESS-CM2_ssp585_2081-2100.tif")

tmax <- rast("./Data_raw/worldClim_climateVars/futureClimate_endCentury_ssp585/ACCESS-CM2/wc2.1_10m_tmax_ACCESS-CM2_ssp585_2081-2100.tif")

precip <-rast("./Data_raw/worldClim_climateVars/futureClimate_endCentury_ssp585/ACCESS-CM2/wc2.1_10m_prec_ACCESS-CM2_ssp585_2081-2100.tif")

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

# calculate VPD max -------------------------------------------------------

# Calculate VPD from TMEAN & TMIN
#calculate monthly SVP according to Williams et al NatCC 2012 supplementary material -  units haPa
a0<-6.107799961
a1<-0.4436518521
a2<-0.01428945805
a3<-0.0002650648471
a4<-0.000003031240396
a5<-0.00000002034080948
a6<-0.00000000006136820929
SVP2 <- 0.1*(a0+ tmean*(a1+ tmean *(a2+ tmean *(a3+ tmean *(a4+tmean *(a5 + tmean *a6))))))
# # Running et al. assume RH drops to 100% overnight
# AVP2 <- (0.6108 * exp(17.27 * tmin / (tmin + 237.3)))
# # AVP2 <-stack(AVP2)
# VPD <- (SVP2 - AVP2)

# calculate it in the same way we do previously
VPD <- ((( a0+ tmean*(a1+ tmean *(a2+ tmean *(a3+ tmean *(a4	+ tmean *(a5	+ tmean *a6)))))))*100 -  (tmean))/1000

names(VPD) <- c("vpd_month01" ,"vpd_month02","vpd_month03", "vpd_month04", "vpd_month05", "vpd_month06",
                "vpd_month07", "vpd_month08", "vpd_month09", "vpd_month10", "vpd_month11", "vpd_month12")


VPD_max <- terra::app(VPD,
                       fun = max)
plot(VPD_max)

## get VPD as points
VPD_max_points <- VPD_max %>% 
  terra::extract(sampPoints, xy = TRUE)
# modDat_fit[,c("annVPD_max_meanAnnAvg_30yr", "Long", "Lat")] %>% 
#   ggplot() + 
#   geom_point(aes(Long, Lat, col = annVPD_max_meanAnnAvg_30yr))
# use writeRaster to export to a desired filepath

# precip of driest month --------------------------------------------------
# this is bioclim variable 14
precip_driestMonth <- bioClim$bio14
plot(precip_driestMonth)


## get precip driest month as points
precip_driestMonth_points <- precip_driestMonth %>% 
  terra::extract(sampPoints, xy = TRUE)

# correlation of monthly temp and precip ----------------------------------
tmean_points <- tmean %>% 
  terra::extract(sampPoints, xy = TRUE)

precip_points <- precip %>% 
  terra::extract(sampPoints, xy = TRUE)

tempPrecip <- tmean_points %>% 
  left_join(precip_points)

test <- apply(tempPrecip, MARGIN = 1, FUN = function(x) {
  corr <- cor(x = as.vector(x[c("tmin01" , "tmin02"  ,"tmin03" , "tmin04" ,           
               "tmin05" , "tmin06"  ,"tmin07" , "tmin08" , "tmin09",           
                "tmin10",  "tmin11",  "tmin12")], mode = "numeric"),
              y = as.vector(x[c("wc2.1_2.5m_prec_01", "wc2.1_2.5m_prec_02", "wc2.1_2.5m_prec_03", "wc2.1_2.5m_prec_04", "wc2.1_2.5m_prec_05",
                 "wc2.1_2.5m_prec_06" ,"wc2.1_2.5m_prec_07" ,"wc2.1_2.5m_prec_08" ,"wc2.1_2.5m_prec_09" ,"wc2.1_2.5m_prec_10",
                 "wc2.1_2.5m_prec_11" ,"wc2.1_2.5m_prec_12")], mode = "numeric")
              )
  }
)

tempPrecip$correlation <- test

tempPrecipCorr_points <- tempPrecip %>% 
  select(x, y, correlation)



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



# Put all input variables together ----------------------------------------
sensDat <- (precip_driestMonth_points %>% select(bio14))  %>% 
  cbind(tempPrecipCorr_points %>% select(correlation)) %>% 
  cbind(soilCarbon %>% select(organicCarbonPerc_2cm)) %>% 
  cbind(VPD_max_points %>% select(max)) %>%
  cbind(precip_points %>% select(MAP)) %>% 
  cbind(sampPoints) %>% 
  dplyr::rename(avgOrganicCarbonPerc_0_3cm = organicCarbonPerc_2cm,
         precip_driestMonth_meanAnnAvg_30yr = bio14,
         PrecipTempCorr_meanAnnAvg_30yr = correlation, 
         annVPD_max_meanAnnAvg_30yr = max,
         prcp_meanAnnTotal_30yr = MAP,
         Long = x, 
         Lat = y
         )

sensDat %>% 
  mutate(PrecipTempCorr_meanAnnAvg_30yr = 10*PrecipTempCorr_meanAnnAvg_30yr) %>% 
  pivot_longer(cols = precip_driestMonth_meanAnnAvg_30yr:annVPD_max_meanAnnAvg_30yr, names_to = "predictor") %>% 
  ggplot() +
  geom_point(aes(x = Long, y = Lat, col = value)) + 
  facet_wrap(~predictor)


(comparePrecipDriest <- ggplot() + 
  geom_density(data = sensDat, aes(x = precip_driestMonth_meanAnnAvg_30yr), col = "red") + 
  geom_density(data = modDat_use, aes(precip_driestMonth_meanAnnAvg_30yr), col = "blue") + 
  ggtitle("Precip in driest month", subtitle = "red = end of century, blue = current"))

(comparePrecipTempCorr <- ggplot() + 
  geom_density(data = sensDat, aes(x = PrecipTempCorr_meanAnnAvg_30yr), col = "red") + 
  geom_density(data = modDat_use, aes(PrecipTempCorr_meanAnnAvg_30yr), col = "blue") + 
  ggtitle("Precip/Temp correlation", subtitle = "red = end of century, blue = current"))

(compareVPD <- ggplot() + 
    geom_density(data = sensDat, aes(x = annVPD_max_meanAnnAvg_30yr), col = "red") + 
    geom_density(data = modDat_use, aes(annVPD_max_meanAnnAvg_30yr), col = "blue") + 
    ggtitle("VPD max", subtitle = "red = end of century, blue = current"))

predFigures <- annotate_figure(ggarrange(comparePrecipDriest, comparePrecipTempCorr, compareVPD), 
                top = "End-century climate values (2081-2100) from ACCESS-CM2 model, ssp 585")
   

# read in model  ----------------------------------------------------------
# newRegion ~ precip_driestMonth_meanAnnAvg_30yr + PrecipTempCorr_meanAnnAvg_30yr + 
# avgOrganicCarbonPerc_0_3cm + annVPD_max_meanAnnAvg_30yr
# partyTree_5 <- readRDS("./Data_processed/EcoregionModelForTesting.rds")

# predict categories based on this simple model
partyTree_5Preds <- cbind(sensDat, "partyPrediction" = partykit::predict.party(partyTree_5, newdata = sensDat, type = "response")) %>%
  cbind("partyPrediction" = partykit::predict.party(partyTree_5, newdata = sensDat, type = "prob")) 


(partyTree_5PredMAP <- ggplot(partyTree_5Preds) +
    geom_point(aes(Long, Lat, col = partyPrediction), alpha = .5) +
    ggtitle("model-predicted ecoregion classification -- fit to data from 2081-2100 \nfrom ACCESS-CM2 model, ssp 585 ",
            subtitle = paste0("model: ecoregion ~ precip of driest month + precip/temp correlation + \norg. carbon + VPD max; depth = 6" )) #+
    #geom_point(data = partyTree_5Preds[partyTree_5Preds$goodPred == FALSE,], aes(Long, Lat, fill = partyPrediction),
               # col = "black"
               # , shape = 21)
  )


# try model w/ deeper tree but same predictors ---------------------------------
# this model is called "partyTree_4"
# newRegion ~ precip_driestMonth_meanAnnAvg_30yr + PrecipTempCorr_meanAnnAvg_30yr +
# avgOrganicCarbonPerc_0_3cm + annVPD_max_meanAnnAvg_30yr
# partyTree_4 <- readRDS("./Data_processed/EcoregionModelForTesting.rds")

# predict categories based on this simple model
partyTree_4Preds <- cbind(sensDat, "partyPrediction" = partykit::predict.party(partyTree_4, newdata = sensDat, type = "response")) %>%
  cbind("partyPrediction" = partykit::predict.party(partyTree_4, newdata = sensDat, type = "prob"))


(partyTree_4PredMAP <- ggplot(partyTree_4Preds) +
    geom_point(aes(Long, Lat, col = partyPrediction), alpha = .5) +
    ggtitle("model-predicted ecoregion classification -- fit to data from 2081-2100 \nfrom ACCESS-CM2 model, ssp 585 ",
            subtitle = paste0("model: ecoregion ~ precip of driest month + precip/temp correlation + \norg. carbon + VPD max; depth = 7" )) #+
  #geom_point(data = partyTree_4Preds[partyTree_4Preds$goodPred == FALSE,], aes(Long, Lat, fill = partyPrediction),
  # col = "black"
  # , shape = 21)
)
 ## maybe even worse :-(

# try model w/ really deep tree but same predictors ---------------------------------
# this model is called "partyTree_4"
# newRegion ~ precip_driestMonth_meanAnnAvg_30yr + PrecipTempCorr_meanAnnAvg_30yr +
# avgOrganicCarbonPerc_0_3cm + annVPD_max_meanAnnAvg_30yr
# partyTree_4 <- readRDS("./Data_processed/EcoregionModelForTesting.rds")

partyTree_deep <- partykit::ctree(newRegion ~   precip_driestMonth_meanAnnAvg_30yr   + PrecipTempCorr_meanAnnAvg_30yr        +
                                 avgOrganicCarbonPerc_0_3cm + 
                                 annVPD_max_meanAnnAvg_30yr   ,
                               
                               data = modDat_fit)

partyTree_deep

# predict categories based on this simple model
partyTree_deepPreds <- cbind(sensDat, "partyPrediction" = partykit::predict.party(partyTree_deep, newdata = sensDat, type = "response")) %>%
  cbind("partyPrediction" = partykit::predict.party(partyTree_deep, newdata = sensDat, type = "prob"))


(partyTree_deepPredMAP <- ggplot(partyTree_deepPreds) +
    geom_point(aes(Long, Lat, col = partyPrediction), alpha = .5) +
    ggtitle("model-predicted ecoregion classification -- fit to data from 2081-2100 \nfrom ACCESS-CM2 model, ssp 585 ",
            subtitle = paste0("model: ecoregion ~ precip of driest month + precip/temp correlation + \norg. carbon + VPD max; unrestricted tree depth" )) #+
  #geom_point(data = partyTree_deepPreds[partyTree_deepPreds$goodPred == FALSE,], aes(Long, Lat, fill = partyPrediction),
  # col = "black"
  # , shape = 21)
)

# try model w/ top 5 predictors and depth of 7 ---------------------------------
# this model is called "partyTree_3"
# newRegion ~ precip_driestMonth_meanAnnAvg_30yr + PrecipTempCorr_meanAnnAvg_30yr +
# avgOrganicCarbonPerc_0_3cm + annVPD_max_meanAnnAvg_30yr
# partyTree_4 <- readRDS("./Data_processed/EcoregionModelForTesting.rds")

# predict categories based on this simple model
partyTree_3 <- cbind(sensDat, "partyPrediction" = partykit::predict.party(partyTree_3, newdata = sensDat, type = "response")) %>%
  cbind("partyPrediction" = partykit::predict.party(partyTree_3, newdata = sensDat, type = "prob"))


(partyTree_3PredMAP <- ggplot(partyTree_3) +
    geom_point(aes(Long, Lat, col = partyPrediction), alpha = .5) +
    ggtitle("model-predicted ecoregion classification -- fit to data from 2081-2100 \nfrom ACCESS-CM2 model, ssp 585 ",
            subtitle = paste0("model: ecoregion ~ precip of driest month + precip/temp correlation + \norg. carbon + VPD max + MAP ; depth = 7" )) #+
  #geom_point(data = partyTree_3[partyTree_3$goodPred == FALSE,], aes(Long, Lat, fill = partyPrediction),
  # col = "black"
  # , shape = 21)
)



ggarrange(predFigures, partyTree_5PredMAP, 
          partyTree_4PredMAP,
          partyTree_deepPredMAP,
          partyTree_3PredMAP,
          ncol = 1)  %>% 
  ggexport(
    filename = "./Figures/EcoRegionModelFigures/Sensitivity_endCenturyACESS_CM2data.pdf",
    width = 12, height = 23)
