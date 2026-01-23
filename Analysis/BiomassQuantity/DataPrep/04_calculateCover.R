#///////////////////
# Calculating cover w/ models from cover analysis for locations of biomass data
# Alice Stears
# 10/13/2025
#///////////////////


# load packages -----------------------------------------------------------
library(tidyverse)
library(sf) 
library(terra)

# load data ---------------------------------------------------------------
dat <- readRDS("./Data_processed/BiomassQuantityData/GEDIbiomass_predictedCover_climateAndSoils.rds")

# rename 
dat2 <- dat %>% 
  rename(twarm_CLIM = t_warm,
         tcold_CLIM = t_cold, 
         precipWet_CLIM = prcp_wet, 
         annWatDef_CLIM = annWatDef, 
         PrecipTempCorr_CLIM = prcpTempCorr, 
         isothermality_CLIM = isothermality,
         tmean_CLIM = tmean,
         precipSeasonality_CLIM = prcp_seasonality,
         precipTempCorr_WEATH = PrecipTempCorr_meanAnnAvg_3yrAnom,
         isothermality_WEATH = isothermality_meanAnnAvg_3yrAnom,
         annWatDef_WEATH = annWaterDeficit_meanAnnAvg_3yrAnom,
         precip_CLIM = prcp,
         precipSeasonality_WEATH = precip_Seasonality_meanAnnAvg_3yrAnom,
         precipDry_CLIM = prcp_dry,
         precip_WEATH = prcp_meanAnnTotal_3yrAnom,
         annWetDegDays_CLIM = annWetDegDays,
         annWetDegDays_WEATH = annWetDegDays_meanAnnAvg_3yrAnom
         )
# Get Equation for ecoregion classification -------------------------------
#P(forest) ~ 1/(1 + exp(-( 9.872597456 + -0.299906791*twarm_CLIM + 0.245551132*tcold_CLIM +
                            # 0.010607279*precipWet_CLIM + -0.062058523*annWatDef_CLIM +
                            # -2.786336969*PrecipTempCorr_CLIM + 0.054028905*isothermality_CLIM +
                            # -0.007599899*soilDepth + 0.033478424*sand + 0.031037682*coarse + 0.272601351*carbon)))
## predict for ecoregion classification
dat3 <- dat2 %>% 
  mutate(P_forest = 1/(1 + exp(-( 9.872597456 + -0.299906791*twarm_CLIM + 0.245551132*tcold_CLIM +
                                     0.010607279*precipWet_CLIM + -0.062058523*annWatDef_CLIM +
                                     -2.786336969*PrecipTempCorr_CLIM + 0.054028905*isothermality_CLIM +
                                     -0.007599899*soilDepth + 0.033478424*sand + 0.031037682*coarse + 0.272601351*carbon))))



## scale climate data 
# get data that has the scaling factors
modDat_1_s <- readRDS("./Analysis/VegComposition/ModelFitting/models/scaledModelInputData.rds")

# rename
climDat <- dat3 %>% 
  rename("tmin" = tmin, 
           "tmax" = tmax, #1 
         "tmean" = tmean_CLIM, 
         "prcp" = precip_CLIM, 
         "t_warm" = twarm_CLIM,
         "t_cold" = tcold_CLIM, 
         "prcp_wet" = precipWet_CLIM,
         "prcp_dry" = precipDry_CLIM, 
         "prcp_seasonality" = precipSeasonality_CLIM, #2
         "prcpTempCorr" = PrecipTempCorr_CLIM,  #3
         "abvFreezingMonth" = abvFreezingMonth, 
         "isothermality" = isothermality_CLIM, #4
         "annWatDef" = annWatDef_CLIM, 
         "annWetDegDays" = annWetDegDays_CLIM,
         "VPD_mean" = VPD_mean, 
         "VPD_max" = VPD_max, #5
         "VPD_min" = VPD_min, #6
         "VPD_max_95" = VPD_max_95, 
         "annWatDef_95" = annWatDef_95, 
         "annWetDegDays_5" = annWetDegDays_5, 
         "frostFreeDays_5" = frostFreeDays_5, 
         "frostFreeDays" = frostFreeDays, 
         "soilDepth" = soilDepth, #7
         "clay" = clay, 
         "sand" = sand, #8
         "coarse" = coarse, #9
         "carbon" = carbon, #10
           "AWHC" = AWHC,
         ## anomaly variables
         tmean_anom = tmean_meanAnnAvg_3yrAnom, #15
         tmin_anom = tmin_meanAnnAvg_3yrAnom, #16
         tmax_anom = tmax_meanAnnAvg_3yrAnom, #17
         prcp_anom = precip_WEATH, #18
         t_warm_anom = T_warmestMonth_meanAnnAvg_3yrAnom,  #19
         t_cold_anom = T_coldestMonth_meanAnnAvg_3yrAnom, #20
         prcp_wet_anom = precip_wettestMonth_meanAnnAvg_3yrAnom, #21
         precp_dry_anom = precip_driestMonth_meanAnnAvg_3yrAnom,  #22
         prcp_seasonality_anom = precipSeasonality_WEATH, #23 
         prcpTempCorr_anom = precipTempCorr_WEATH, #24
         aboveFreezingMonth_anom = aboveFreezing_month_meanAnnAvg_3yrAnom, #25  
         isothermality_anom = isothermality_WEATH, #26
         annWatDef_anom = annWatDef_WEATH, #27
         annWetDegDays_anom = annWetDegDays_WEATH,  #28
         VPD_mean_anom = annVPD_mean_meanAnnAvg_3yrAnom, #29
         VPD_min_anom = annVPD_min_meanAnnAvg_3yrAnom,  #30
         VPD_max_anom = annVPD_max_meanAnnAvg_3yrAnom,  #31
         VPD_max_95_anom = annVPD_max_95percentile_3yrAnom, #32
         annWatDef_95_anom = annWaterDeficit_95percentile_3yrAnom, #33 
         annWetDegDays_5_anom = annWetDegDays_5percentile_3yrAnom ,  #34
         frostFreeDays_5_anom = durationFrostFreeDays_5percentile_3yrAnom, #35 
         frostFreeDays_anom = durationFrostFreeDays_meanAnnAvg_3yrAnom #36
  ) 

## now, scale the contemporary climate data for use in models 
# get the scaling factors 
scaleParams <- modDat_1_s %>% 
  #filter(Year == 2016) %>% 
  dplyr::select(tmin_s:AWHC_s) %>% 
  reframe(across(all_of(names(.)), attributes)) 

# apply the scaling factors to the contemporary climate data 
namesToScale <- climDat %>% 
  dplyr::select(tmin:frostFreeDays, tmean_anom:frostFreeDays_anom, soilDepth:AWHC) %>% 
  names()

climDat_scaled <- map(namesToScale, .f = function(x) {
  x_new <- (climDat[,x] - scaleParams[,paste0(x, "_s")]$`scaled:center`)/scaleParams[,paste0(x, "_s")]$`scaled:scale`
  return(data.frame(x_new))
}) %>% 
  purrr::list_cbind()
names(climDat_scaled) <- paste0(namesToScale, "_s")

climDatPred <- climDat %>% 
  dplyr::select(NA_L1CODE:newRegion, x:y) %>% 
  cbind(climDat_scaled)
names(climDatPred)[7:56] <- str_remove(names(climDatPred)[7:56], pattern = "_s$")

# get model objects  ------------------------------------------------------
# Total herbaceous cover: 
# Grass/shrub: 1SE lambda model
totalHerb_GS <-  readRDS("./Analysis/VegComposition/ModelFitting/models/betaLASSO/TotalHerbaceousCover_shrubGrass_noTLP_FALSE_removeAnomaliesFALSE_trimAnom_oneSELambdaGLM.rds")
# Forest: best lambda model
totalHerb_F <-  readRDS("./Analysis/VegComposition/ModelFitting/models/betaLASSO/TotalHerbaceousCover_forest_noTLP_FALSE_removeAnomaliesFALSE_trimAnom_bestLambdaGLM.rds")

# Total tree cover: 
# Grass/shrub: best lambda model 
totalTree_GS <-
  readRDS("./Analysis/VegComposition/ModelFitting/models/betaLASSO/TotalTreeCover_shrubGrass_noTLP_FALSE_removeAnomaliesTRUE_bestLambdaGLM.rds")
# Forest: best lambda model 
totalTree_F <-  #readRDS("./models/betaLASSO/TotalTreeCover_forest_noTLP_FALSE_removeAnomaliesFALSE_trimAnom_bestLambdaGLM.rds")
  #remove anomalies option #
  readRDS("./Analysis/VegComposition/ModelFitting/models/betaLASSO/TotalTreeCover_forest_noTLP_FALSE_removeAnomaliesTRUE_halfSELambdaGLM.rds")

# Bare ground: # CONUS - 1SE lambda model
bareGround_CONUS <-  readRDS("./Analysis/VegComposition/ModelFitting/models/betaLASSO/BareGroundCover_CONUS_noTLP_FALSE_removeAnomaliesFALSE_trimAnom_oneSELambdaGLM.rds")

# Shrub: # CONUS - best lambda model
shrub_CONUS <- readRDS("./Analysis/VegComposition/ModelFitting/models/betaLASSO/ShrubCover_CONUS_noTLP_FALSE_removeAnomaliesFALSE_trimAnom_bestLambdaGLM.rds")

# Broad-leaved tree: 
# Grass/shrub: best lambda model
BLtree_GS <- readRDS("./Analysis/VegComposition/ModelFitting/models/betaLASSO/AngioTreeCover_prop_shrubGrass_noTLP_FALSE_removeAnomaliesFALSE_trimAnom_bestLambdaGLM.rds")
# Forest:  best lambda model
BLtree_F <- readRDS("./Analysis/VegComposition/ModelFitting/models/betaLASSO/AngioTreeCover_prop_forest_noTLP_FALSE_removeAnomaliesFALSE_trimAnom_bestLambdaGLM.rds")

# Needle-leaved tree: 
# Grass/shrub: best lambda model
NLtree_GS <-  readRDS("./Analysis/VegComposition/ModelFitting/models/betaLASSO/ConifTreeCover_prop_shrubGrass_noTLP_FALSE_removeAnomaliesFALSE_trimAnom_bestLambdaGLM.rds")
# Forest:  best lambda model
NLtree_F <- readRDS("./Analysis/VegComposition/ModelFitting/models/betaLASSO/ConifTreeCover_prop_forest_noTLP_FALSE_removeAnomaliesFALSE_trimAnom_bestLambdaGLM.rds")

# Forb: # CONUS: best lambda model
forb_CONUS <-  readRDS("./Analysis/VegComposition/ModelFitting/models/betaLASSO/ForbCover_prop_CONUS_noTLP_FALSE_removeAnomaliesFALSE_trimAnom_bestLambdaGLM.rds")

# C3 grass:# CONUS: 1SE lambda model
C3grass_CONUS <- readRDS("./Analysis/VegComposition/ModelFitting/models/betaLASSO/C3GramCover_prop_CONUS_noTLP_FALSE_removeAnomaliesFALSE_trimAnom_oneSELambdaGLM.rds")

# C4 grass: # CONUS: best lambda model
C4grass_CONUS <- readRDS("./Analysis/VegComposition/ModelFitting/models/betaLASSO/C4GramCover_prop_CONUS_noTLP_FALSE_removeAnomaliesFALSE_trimAnom_bestLambdaGLM.rds")


# Now, predict absolute cover  --------------------------------------------
# function to make predictions
prednames_s <-  modDat_1_s %>%
  dplyr::select(tmin_s:AWHC_s) %>%
  names()
prednames <- str_replace(prednames_s, pattern = "_s$", replacement = "")

makePredictions <- function(predictionDF, modelObject, scale  = TRUE) {
  ##
  # predictionDF <- climDatPred
  # modelObject <- bestLambdaMod_grassShrub_totalHerb
  # # ##
  # predictionDF = modDat_quantile,
  # modelObject = forb_CONUS, scale = FALSE
  # 
  # pretend to scale the input variables so the model object can predict accurately
  if(scale == TRUE) {
    predictionDF <- predictionDF %>% 
      mutate(across(all_of(prednames), base::scale,scale = FALSE, center = FALSE)) 
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

## Now, predict absolute cover with scaled climate/weather/soils data 
## with contemporary climate data
totalHerb_F_predsContemp <- makePredictions(predictionDF = climDatPred,
                                            modelObject = totalHerb_F) %>% 
  rename(absTotalHerb_F = "modelPreds")
totalHerb_GS_predsContemp <- makePredictions(predictionDF = climDatPred,
                                             modelObject = totalHerb_GS) %>% 
  rename(absTotalHerb_GS = "modelPreds")

totalTree_F_predsContemp<- makePredictions(predictionDF = climDatPred,
                                           modelObject = totalTree_F) %>% 
  rename(absTotalTree_F = "modelPreds")
totalTree_GS_predsContemp<- makePredictions(predictionDF = climDatPred,
                                            modelObject = totalTree_GS) %>% 
  rename(absTotalTree_GS = "modelPreds")

bareGround_CONUS_predsContemp<- makePredictions(predictionDF = climDatPred,
                                                modelObject = bareGround_CONUS) %>% 
  rename(absBareGround_CONUS = "modelPreds")

shrub_CONUS_predsContemp<- makePredictions(predictionDF = climDatPred,
                                           modelObject = shrub_CONUS) %>% 
  rename(absShrub_CONUS = "modelPreds")

BLtree_F_predsContemp<- makePredictions(predictionDF = climDatPred,
                                        modelObject = BLtree_F) %>% 
  rename(percBLTree_F = "modelPreds")
BLtree_GS_predsContemp<- makePredictions(predictionDF = climDatPred,
                                         modelObject = BLtree_GS) %>% 
  rename(percBLTree_GS = "modelPreds")

NLtree_F_predsContemp<- makePredictions(predictionDF = climDatPred,
                                        modelObject = NLtree_F) %>% 
  rename(percNLTree_F = "modelPreds")
NLtree_GS_predsContemp<- makePredictions(predictionDF = climDatPred,
                                         modelObject = NLtree_GS) %>% 
  rename(percNLTree_GS = "modelPreds")

C3grass_CONUS_predsContemp<- makePredictions(predictionDF = climDatPred,
                                             modelObject = C3grass_CONUS) %>% 
  rename(percC3grass_CONUS = "modelPreds")

C4grass_CONUS_predsContemp<- makePredictions(predictionDF = climDatPred,
                                             modelObject = C4grass_CONUS) %>% 
  rename(percC4grass_CONUS = "modelPreds")

forb_CONUS_predsContemp<- makePredictions(predictionDF = climDatPred,
                                          modelObject = forb_CONUS) %>% 
  rename(percForb_CONUS = "modelPreds")

contempPreds <- totalHerb_F_predsContemp  %>% 
  cbind(totalHerb_GS_predsContemp %>% select(absTotalHerb_GS)) %>% 
  cbind(totalTree_F_predsContemp %>% select(absTotalTree_F)) %>% 
  cbind(totalTree_GS_predsContemp %>% select(absTotalTree_GS)) %>% 
  cbind(bareGround_CONUS_predsContemp %>% select(absBareGround_CONUS)) %>% 
  cbind(shrub_CONUS_predsContemp %>% select(absShrub_CONUS)) %>% 
  cbind(BLtree_F_predsContemp %>% select(percBLTree_F)) %>% 
  cbind(BLtree_GS_predsContemp %>% select(percBLTree_GS)) %>% 
  cbind(NLtree_F_predsContemp %>% select(percNLTree_F)) %>% 
  cbind(NLtree_GS_predsContemp %>% select(percNLTree_GS)) %>% 
  cbind(C3grass_CONUS_predsContemp %>% select(percC3grass_CONUS)) %>% 
  cbind(C4grass_CONUS_predsContemp %>% select(percC4grass_CONUS)) %>% 
  cbind(forb_CONUS_predsContemp %>% select(percForb_CONUS)) %>% 
  cbind(dat3 %>% select(P_forest)) %>% 
  rename(prob_Forest = "P_forest") %>% 
  mutate(prob_grassShrub = 1 - prob_Forest)



# scale predictions --------------------
## scale according to ecoregion
contempPreds <- contempPreds %>% 
  mutate(absTotalHerb_CONUS = (prob_grassShrub * absTotalHerb_GS) + (prob_Forest * absTotalHerb_F),
         absTotalTree_CONUS = (prob_grassShrub * absTotalTree_GS) + (prob_Forest * absTotalTree_F)
  )

        
## Then, for those models that are predicting proportions, we scale all of the relevant predictions so they sum to one. If models are ecoregion-level, we do this process at the ecoregion scale. 
# (e.g. Model predictions of the proportion of total herbaceous cover that is forbs, the proportion of total herbaceous cover that is C3 grass, and the proportion of total herbaceous cover that is C4 grass should sum to 1, so: scaled proportion of total herbaceous cover that is forbs = proportion of total herbaceous cover that is forbs/(proportion of total herbaceous cover that is forbs + proportion of total herbaceous cover that is C3 grass + proportion of total herbaceous cover that is C4 grass))

contempPreds <- contempPreds %>% 
  mutate(percBLTree_scaled_GS = percBLTree_GS/(percBLTree_GS + percNLTree_GS),
         percNLTree_scaled_GS = percNLTree_GS/(percBLTree_GS + percNLTree_GS),
         percBLTree_scaled_F = percBLTree_F/(percBLTree_F + percNLTree_F),
         percNLTree_scaled_F = percNLTree_F/(percBLTree_F + percNLTree_F),
         percC3grass_scaled_CONUS = percC3grass_CONUS/(percC3grass_CONUS + percC4grass_CONUS + percForb_CONUS),
         percC4grass_scaled_CONUS = percC4grass_CONUS/(percC3grass_CONUS + percC4grass_CONUS + percForb_CONUS),
         percForb_scaled_CONUS = percForb_CONUS/(percC3grass_CONUS + percC4grass_CONUS + percForb_CONUS))

## Then, we convert these predicted proportions into predictions of absolute cover. For the functional types that are predicted at the ecoregion level, we do this process for each ecoregion. 
contempPreds <- contempPreds %>% 
  mutate(absNLTree_GS = (percNLTree_scaled_GS * absTotalTree_GS), 
         absBLTree_GS = (percBLTree_scaled_GS * absTotalTree_GS), 
         absNLTree_F = (percNLTree_scaled_F * absTotalTree_F), 
         absBLTree_F = (percBLTree_scaled_F * absTotalTree_F), 
         absC3grass_CONUS = (percC3grass_scaled_CONUS * absTotalHerb_CONUS), # can use CONUS-wide total herbaceous since C3 grass isn't ecoregion-level
         absC4grass_CONUS = (percC4grass_scaled_CONUS * absTotalHerb_CONUS), # can use CONUS-wide total herbaceous since C4 grass isn't ecoregion-level
         absForb_CONUS = (percForb_scaled_CONUS * absTotalHerb_CONUS) # can use CONUS-wide total herbaceous since forb grass isn't ecoregion-level
  )

## Finally, for those functional types that are predicted at the ecoregion level, we scale them to be CONUS-wide.
contempPreds <- contempPreds %>% 
  mutate(absBLTree_CONUS = (prob_grassShrub * absBLTree_GS) + (prob_Forest * absBLTree_F),
         absNLTree_CONUS = (prob_grassShrub * absNLTree_GS) + (prob_Forest * absNLTree_F)
  )

## now, make cover relative for all types except for trees 
contempPreds <- contempPreds %>% 
  mutate(total_NonTree_AbsCover_CONUS = (absTotalHerb_CONUS + #absTotalTree_CONUS + 
                                           absBareGround_CONUS + absShrub_CONUS)) %>% 
  mutate(relCoverB_totalTree = absTotalTree_CONUS,#/total_NonTree_AbsCover_CONUS,
         relCoverB_totalHerb = absTotalHerb_CONUS/total_NonTree_AbsCover_CONUS,
         relCoverB_shrub = absShrub_CONUS/total_NonTree_AbsCover_CONUS,
         relCoverB_bareGround = absBareGround_CONUS/total_NonTree_AbsCover_CONUS,
         relCoverB_NLTree = absNLTree_CONUS,
         relCoverB_BLTree = absBLTree_CONUS,
         relCoverB_C3Grass = absC3grass_CONUS/total_NonTree_AbsCover_CONUS,
         relCoverB_C4Grass = absC4grass_CONUS/total_NonTree_AbsCover_CONUS,
         relCoverB_Forb = absForb_CONUS/total_NonTree_AbsCover_CONUS
  )

## add back into biomass dataset
dat4 <- dat3 %>% 
  cbind(contempPreds %>% 
          select(relCoverB_totalTree:relCoverB_Forb))
saveRDS(dat4, "./Data_processed/BiomassQuantityData/GEDIbiomass_climateAndSoils_modeledCover.rds")
# 
# ggplot(dat4 %>% slice_sample(n = 10000)) + 
#   geom_point(aes(x, y, col = relCoverB_BLTree + relCoverB_NLTree))
