#///////////////////
# Classifying locations across the US into ecoregions using Random Forest 
# Alice Stears
# 7/30/24
#///////////////////

# Load Packages -----------------------------------------------------------
library(tidyverse)
library(sf)
#library(nnet)
library(caret)
library(GGally) # for ggpairs()
#library(partykit)
#library(ggtern)
library(patchwork)
library(ggpubr)
library(gt)
library(glm.predict)
library(tidyterra)
library(MASS)
library(terra)

# Load Data ---------------------------------------------------------------
# data ready for modeling 
modDat <- readRDS("./Data_processed/EcoRegion_climSoilData.rds")

# get the soil raster, which we'll use for rasterizing the imagery
soilRastTemp <- readRDS("./Data_processed/SoilsRaster.rds") %>% 
terra::unwrap()
#   terra::project("GEOGCRS[\"WGS 84\",\n    ENSEMBLE[\"World Geodetic System 1984 ensemble\",\n        MEMBER[\"World Geodetic System 1984 (Transit)\"],\n        MEMBER[\"World Geodetic System 1984 (G730)\"],\n        MEMBER[\"World Geodetic System 1984 (G873)\"],\n        MEMBER[\"World Geodetic System 1984 (G1150)\"],\n        MEMBER[\"World Geodetic System 1984 (G1674)\"],\n        MEMBER[\"World Geodetic System 1984 (G1762)\"],\n        MEMBER[\"World Geodetic System 1984 (G2139)\"],\n        ELLIPSOID[\"WGS 84\",6378137,298.257223563,\n            LENGTHUNIT[\"metre\",1]],\n        ENSEMBLEACCURACY[2.0]],\n    PRIMEM[\"Greenwich\",0,\n        ANGLEUNIT[\"degree\",0.0174532925199433]],\n    CS[ellipsoidal,2],\n        AXIS[\"geodetic latitude (Lat)\",north,\n            ORDER[1],\n            ANGLEUNIT[\"degree\",0.0174532925199433]],\n        AXIS[\"geodetic longitude (Lon)\",east,\n            ORDER[2],\n            ANGLEUNIT[\"degree\",0.0174532925199433]],\n    USAGE[\n        SCOPE[\"Horizontal component of 3D system.\"],\n        AREA[\"World.\"],\n        BBOX[-90,-180,90,180]],\n    ID[\"EPSG\",4326]]"
#                  )
# Prepare data for model-fitting  ----------------------------------------------
# (eastern forest) put eastern temperate forests, tropical wet forests and northern forests together
# (dry shrub and grass) put great plains, southern semiarid highlands, Mediterranean California and north american deserts together 
# (western forest) put northwestern forested mountains, marine west coast forests together, and temperate sierras together
set.seed(12011993)

# ggplot(modDat) + 
#   geom_point(aes(x = Long, y = Lat))

modDat_use <- modDat %>% 
  ## arbitrarily chose three years of climate data to use 
  filter(year %in% c(2010, 2015, 2020)) %>% 
  rename("Long" = x, "Lat" = y) %>% 
  dplyr::select(c(newRegion, #swe_meanAnnAvg_CLIM:
                  tmin_meanAnnAvg_CLIM:durationFrostFreeDays_meanAnnAvg_CLIM,
                                         soilDepth                  , surfaceClay_perc          ,                
                                         avgSandPerc_acrossDepth    ,  avgCoarsePerc_acrossDepth,                 
                                         avgOrganicCarbonPerc_0_3cm ,  totalAvailableWaterHoldingCapacity,
                                         Long, Lat)) %>% 
  mutate(newRegion = as.factor(newRegion)) %>% 
  drop_na() #%>% 
  #mutate(isothermality_meanAnnAvg_CLIM = -1 * isothermality_meanAnnAvg_CLIM)
  
default_idx <-  caret::createDataPartition(modDat$newRegion, p = 0.8, list = FALSE)
modDat_fit <- modDat_use[default_idx, ] %>% 
    # dplyr::select(newRegion, swe_meanAnnAvg_CLIM , annWetDegDays_meanAnnAvg_CLIM , 
  #           tmean_meanAnnAvg_CLIM , isothermality_meanAnnAvg_CLIM , 
  #         prcp_meanAnnTotal_CLIM , PrecipTempCorr_meanAnnAvg_CLIM, 
  #        soilDepth                  , surfaceClay_perc          ,                
  #        avgSandPerc_acrossDepth    ,  avgCoarsePerc_acrossDepth,                 
  #        avgOrganicCarbonPerc_0_3cm ,  totalAvailableWaterHoldingCapacity, Long, Lat) %>% 
  drop_na() %>% 
  st_drop_geometry() 

modDat_test <-  modDat_use[-default_idx, ] %>% 
  mutate(newRegion = as.factor(newRegion)) %>% 
  # dplyr::select(c(newRegion, swe_meanAnnAvg_CLIM:
  #                   # tmean_meanAnnAvg_CLIM:prcp_meanAnnTotal_CLIM,
  #                   #                        precip_wettestMonth_meanAnnAvg_CLIM:PrecipTempCorr_meanAnnAvg_CLIM, 
  #                   #                        isothermality_meanAnnAvg_CLIM:annVPD_min_meanAnnAvg_CLIM, 
  #                   durationFrostFreeDays_meanAnnAvg_CLIM,
  #                 soilDepth                  , surfaceClay_perc          ,                
  #                 avgSandPerc_acrossDepth    ,  avgCoarsePerc_acrossDepth,                 
  #                 avgOrganicCarbonPerc_0_3cm ,  totalAvailableWaterHoldingCapacity,
  #                 Long, Lat)) %>% 
  drop_na() #%>%
 # anti_join(modDat_fit)

# look at correlation between variables
# function to add colors to correlation plot
my_fn <- function(data, mapping, method="p", use="pairwise", ...){
  
  # grab data
  x <- eval_data_col(data, mapping$x)
  y <- eval_data_col(data, mapping$y)
  
  # calculate correlation
  corr <- cor(x, y, method=method, use=use)
  
  # calculate colour based on correlation value
  # Here I have set a correlation of minus one to blue, 
  # zero to white, and one to red 
  # Change this to suit: possibly extend to add as an argument of `my_fn`
  colFn <- colorRampPalette(c("red", "white", "blue"), interpolate ='spline')
  fill <- colFn(100)[findInterval(corr, seq(-1, 1, length=100))]
  
  ggally_cor(data = data, mapping = mapping, stars = FALSE, 
             digits = 2, colour = I("black"),...) + 
    theme_void() +
    theme(panel.background = element_rect(fill=fill))
  
}

# # WITH VPD: drop correlated variables ------------------------------------
# 
# (corrPlot2 <- 
#    modDat_fit %>% 
#    dplyr::select(
#      "tmin_meanAnnAvg_CLIM"                  ,
#      "tmax_meanAnnAvg_CLIM"                  , "tmean_meanAnnAvg_CLIM"                 ,
#      "prcp_meanAnnTotal_CLIM"                , "T_warmestMonth_meanAnnAvg_CLIM"       , 
#      "T_coldestMonth_meanAnnAvg_CLIM"        , "precip_wettestMonth_meanAnnAvg_CLIM"  , 
#      "precip_driestMonth_meanAnnAvg_CLIM"    , "precip_Seasonality_meanAnnAvg_CLIM"   , 
#      "PrecipTempCorr_meanAnnAvg_CLIM"       ,  "aboveFreezing_month_meanAnnAvg_CLIM"   ,
#      "isothermality_meanAnnAvg_CLIM"        ,  "annWaterDeficit_meanAnnAvg_CLIM"       ,
#      "annWetDegDays_meanAnnAvg_CLIM"        ,  "annVPD_mean_meanAnnAvg_CLIM"           ,
#      "annVPD_max_meanAnnAvg_CLIM"           ,  "annVPD_min_meanAnnAvg_CLIM"            ,
#      "annVPD_max_95percentile_CLIM"         ,  
#      "annWaterDeficit_95percentile_CLIM"     ,
#      "annWetDegDays_5percentile_CLIM"       ,  "durationFrostFreeDays_5percentile_CLIM",
#      "durationFrostFreeDays_meanAnnAvg_CLIM",  "soilDepth"                             ,
#      "surfaceClay_perc"                     ,  "avgSandPerc_acrossDepth"               ,
#      "avgCoarsePerc_acrossDepth"            ,  "avgOrganicCarbonPerc_0_3cm"            ,
#      "totalAvailableWaterHoldingCapacity" 
#    ) %>% 
#    rename(
#      "prcp" = prcp_meanAnnTotal_CLIM,
#      "prcp \n TempCorr" = PrecipTempCorr_meanAnnAvg_CLIM,  "isothermality" = isothermality_meanAnnAvg_CLIM,
#      "Wet \n DegDays" = annWetDegDays_meanAnnAvg_CLIM) %>%
#    cor()  %>% 
#    caret::findCorrelation(cutoff = .7, verbose = TRUE, names = TRUE, exact = TRUE))
# # the findCorrelation() function says we should remove these variables: 
# # "tmean_meanAnnAvg_CLIM"                  "tmin_meanAnnAvg_CLIM"                   "annVPD_mean_meanAnnAvg_CLIM"           
# # "aboveFreezing_month_meanAnnAvg_CLIM"    "durationFrostFreeDays_meanAnnAvg_CLIM"  "durationFrostFreeDays_5percentile_CLIM"
# # "tmax_meanAnnAvg_CLIM"                   "annVPD_min_meanAnnAvg_CLIM"             "T_coldestMonth_meanAnnAvg_CLIM"        
# #  "annVPD_max_meanAnnAvg_CLIM"             "T_warmestMonth_meanAnnAvg_CLIM"         "Wet \n DegDays"                        
# #  "annWaterDeficit_95percentile_CLIM"      "annWaterDeficit_meanAnnAvg_CLIM"        "annWetDegDays_5percentile_CLIM"        
# #  "prcp"                                   "precip_Seasonality_meanAnnAvg_CLIM"     "totalAvailableWaterHoldingCapacity"    
# #  "avgSandPerc_acrossDepth"      
# 
# ## also remove wet degree days, since it's correlated w/ 
# (corrPlot2 <- 
#     modDat_fit %>% 
#     dplyr::select(#"tmin_meanAnnAvg_CLIM"              ,
#       #"tmax_meanAnnAvg_CLIM" ,                 
#       #"tmean_meanAnnAvg_CLIM"                  ,
#      #"prcp_meanAnnTotal_CLIM"                 ,
#       #"T_warmestMonth_meanAnnAvg_CLIM"       , 
#       #"T_coldestMonth_meanAnnAvg_CLIM"         ,
#       "precip_wettestMonth_meanAnnAvg_CLIM"    ,
#       "precip_driestMonth_meanAnnAvg_CLIM"   , 
#       #"precip_Seasonality_meanAnnAvg_CLIM"     ,
#       "PrecipTempCorr_meanAnnAvg_CLIM"        , #"aboveFreezing_month_meanAnnAvg_CLIM" ,  
#       "isothermality_meanAnnAvg_CLIM"          ,#"annWaterDeficit_meanAnnAvg_CLIM"       , #"annWetDegDays_meanAnnAvg_CLIM"       ,  
#       #"annVPD_mean_meanAnnAvg_CLIM"            ,
#       #"annVPD_max_meanAnnAvg_CLIM"            , #"annVPD_min_meanAnnAvg_CLIM"          ,  
#       "annVPD_max_95percentile_CLIM"           ,#"annWaterDeficit_95percentile_CLIM"     , 
#       #"annWetDegDays_5percentile_CLIM"      ,  
#       #"durationFrostFreeDays_5percentile_CLIM" ,#"durationFrostFreeDays_meanAnnAvg_CLIM" , 
#       "soilDepth"                           ,  
#        "surfaceClay_perc"                       ,
#       #"avgSandPerc_acrossDepth"               , 
#       "avgCoarsePerc_acrossDepth"           ,  
#       "avgOrganicCarbonPerc_0_3cm"            #, #"totalAvailableWaterHoldingCapacity"
#     ) %>% 
#     rename(#"MAP" =  prcp_meanAnnTotal_CLIM,
#       #"T_warmest \nmonth" = T_warmestMonth_meanAnnAvg_CLIM, 
#       #"T_coldest \nmonth" = T_coldestMonth_meanAnnAvg_CLIM,
#       "precip_wettest" = precip_wettestMonth_meanAnnAvg_CLIM, 
#       "precip_driest" = precip_driestMonth_meanAnnAvg_CLIM,
#       "P/T corr" = PrecipTempCorr_meanAnnAvg_CLIM,
#       "isothermality" = isothermality_meanAnnAvg_CLIM,
#       #"watDef" = annWaterDeficit_meanAnnAvg_CLIM,
#       #"Wet \n DegDays" = annWetDegDays_meanAnnAvg_CLIM,
#       #"VPD_max" = annVPD_max_meanAnnAvg_CLIM,
#       #"VPD_min" = annVPD_min_meanAnnAvg_CLIM,
#       "VPD_max_95" = annVPD_max_95percentile_CLIM, 
#       #"watDef_95" = annWaterDeficit_95percentile_CLIM,
#       #"wetDegDays_5" = annWetDegDays_5percentile_CLIM,
#       "surfClay" = surfaceClay_perc,
#       #"sand" = avgSandPerc_acrossDepth,
#       "coarse" = avgCoarsePerc_acrossDepth,
#       "carbon" = avgOrganicCarbonPerc_0_3cm #, "waterCap." = totalAvailableWaterHoldingCapacity
#     ) %>%
#     slice_sample(n = 5e4) %>% 
#     #cor() %>% 
#      #findCorrelation(cutoff = .7, verbose = TRUE, names = TRUE, exact = TRUE) 
#     ggpairs( upper = list(continuous = my_fn), lower = list(continuous = GGally::wrap("points", alpha = 0.1, size=0.2)), progress = FALSE))
# 
# # WITH VPD: Fit a simple regression w/ two ecoregions ----------------
# 
# modDat_fitNew <- modDat_fit %>% 
#   mutate(newRegion = str_replace(newRegion, "eastForest", "forest"),
#          newRegion = str_replace(newRegion, "westForest", "forest")) %>% 
#   mutate(newRegionFact = as.factor(newRegion))
# 
# modDat_testNew <- modDat_test %>% 
#   mutate(newRegion = str_replace(newRegion, "eastForest", "forest"),
#          newRegion = str_replace(newRegion, "westForest", "forest")) %>% 
#   mutate(newRegionFact = as.factor(newRegion))
# 
# testMod_3 <- glm(newRegionFact ~ precip_wettestMonth_meanAnnAvg_CLIM    +
#                  precip_driestMonth_meanAnnAvg_CLIM   + 
#                  PrecipTempCorr_meanAnnAvg_CLIM        +
#                  isothermality_meanAnnAvg_CLIM          +
#                  annVPD_max_95percentile_CLIM           +
#                  soilDepth                           +  
#                  surfaceClay_perc                       +
#                  avgCoarsePerc_acrossDepth           +  
#                  avgOrganicCarbonPerc_0_3cm,
#                  data = modDat_fitNew, 
#                  family = "binomial"
# )
# 
# summary(testMod_3)
# 
# head(pp <- fitted(testMod_3))
# 
# AIC(testMod_3) #AIC: 232623.3
# # tetestMod_2# test the predictions of the model
# predictionDat <- modDat_testNew
# predictionDatTemp <- cbind(predictionDat,"newRegion_pred" = predict(testMod_3, newdata = predictionDat, "response"))
# 
# # for each observation, determine which category has the highest predicted probability
# (temp_b <- predictionDatTemp %>%
#     rowwise() %>% 
#     mutate(newRegion_predClass = newRegion_pred) %>% 
#     mutate(newRegion_predClass = replace(newRegion_predClass, newRegion_pred<.5, "dryShrubGrass"), 
#            newRegion_predClass = replace(newRegion_predClass, newRegion_pred>.5, "forest")) 
# )
# 
# temp_b$newRegion_predClass <- relevel(as.factor(temp_b$newRegion_predClass), ref = "dryShrubGrass")
# 
# # are these predictions the same as the actual classifications
# temp_b$goodPred <- temp_b$newRegionFact == temp_b$newRegion_predClass
# sum(!temp_b$goodPred)/nrow(temp_b) * 100
# 
# ## make predictions rasterized 
# temp_b2 <- temp_b %>% 
#   terra::vect(geom = c("Long", "Lat"), crs = crs(soilRastTemp)
#   ) %>% 
#   terra::rasterize(y = 
#                      terra::aggregate(soilRastTemp, fact = 2, fun = mean, na.rm= TRUE)
#                    , field = "newRegion_pred")
# (regMod_3_PredMAP <- ggplot(temp_b2) +
#     geom_spatraster(data = temp_b2) +
#     # geom_point(aes(#Long, Lat, 
#     #   col = newRegion_pred)) + 
#     scale_fill_distiller(type = "div", palette = 1, direction = 1) +
#     #geom_sf(data = ecoregionsSF2, color = "black", fill = NA) + 
#     #lims(x = c(-130, -65), y = c(24, 50))
#     
#     
#     #geom_point(aes(Long, Lat, col = newRegion_pred), alpha = .1) +
#     ggtitle("model-predicted ecoregion classification -- 2 ecoregions; with VPD",
#             subtitle = paste0("ecoregion ~ precipWettestMonth + precipDriestMonth + precipTempCorr + 
#             isothermality + VPDmax + soilDepth + % clay + % coarse + carbon
#            \n black dots indicate misclassification; \n  ", 
#                               round((100-sum(!temp_b$goodPred)/nrow(temp_b) * 100),1), 
#                               "% classification accuracy" )) +
#     geom_point(data = temp_b[temp_b$goodPred == FALSE,], aes(Long, Lat#, fill = as.factor(newRegion_predClass)
#     ),
#     col = "black"
#     , shape = 21) #+ 
#   #scale_fill_discrete(type = c("#bf812d","#35978f"))
# )
# 
# 
# ## zoom in on the great basin to check out mis-classification
# (regMod_3_PredMAP_GB <- ggplot(temp_b) +
#     geom_spatraster(data = temp_b2) +
#     # geom_point(aes(#Long, Lat, 
#     #   col = newRegion_pred)) + 
#     scale_fill_distiller(type = "div", palette = 1, direction = 1) +
#     #geom_sf(data = ecoregionsSF2, color = "black", fill = NA) + 
#     #lims(x = c(-130, -65), y = c(24, 50))
#     
#     
#     #geom_point(aes(Long, Lat, col = newRegion_pred), alpha = .1) +
#     ggtitle("model-predicted ecoregion classification -- 2 ecoregions; with VPD",
#             subtitle = paste0("Zoom-In on Great Basin Area
#            \n dots indicate misclassification; color indicates true classification; \n  ", 
#                               round((100-sum(!temp_b$goodPred)/nrow(temp_b) * 100),1), 
#                               "% classification accuracy" )) +
#     geom_point(data = temp_b[temp_b$goodPred == FALSE,], aes(Long, Lat#, fill = as.factor(newRegion_predClass)
#     ),
#     col = "black"
#     , shape = 21) +
#     xlim(c(-1911149,-999000)) + 
#     ylim(c(-600105, 0))
#   #scale_fill_discrete(type = c("#bf812d","#35978f"))
# )
# 
# ## zoom in on the AZ high elevation area to check out mis-classification
# (regMod_3_PredMAP_PIPO <- ggplot(temp_b2) +
#     geom_spatraster(data = temp_b2) +
#     # geom_point(aes(#Long, Lat, 
#     #   col = newRegion_pred)) + 
#     scale_fill_distiller(type = "div", palette = 1, direction = 1) +
#     #geom_sf(data = ecoregionsSF2, color = "black", fill = NA) + 
#     #lims(x = c(-130, -65), y = c(24, 50))
#     
#     
#     #geom_point(aes(Long, Lat, col = newRegion_pred), alpha = .1) +
#     ggtitle("model-predicted ecoregion classification -- 2 ecoregions; with VPD",
#             subtitle = paste0("Zoom-In on high elevation area in the SW;
#            \n dots indicate misclassification; color indicates true classification \n  ", 
#                               round((100-sum(!temp_b$goodPred)/nrow(temp_b) * 100),1), 
#                               "% classification accuracy" )) +
#     geom_point(data = temp_b[temp_b$goodPred == FALSE,], aes(Long, Lat#, fill = as.factor(newRegion_predClass)
#     ),
#     col = "black"
#     , shape = 21) +
#     xlim(c(-1211149,-559000)) + 
#     ylim(c(-1050105, -550000))
#   #scale_fill_discrete(type = c("#bf812d","#35978f"))
# )
# 
# ## Look at the relationship between misclassification and elevation
# # get elevation information for each raster cell 
# temp_TEMP <- temp_b %>% 
#   st_as_sf(coords = c("Long", "Lat"), crs = crs(soilRastTemp))
# 
# temp_elev <- elevatr::get_elev_point(temp_TEMP, src = "aws")
# 
# temp_TEMP <- temp_TEMP %>% st_join(temp_elev, left = TRUE, join = st_nearest_feature)
# 
# # # within each band of elevation (100 m), what is the frequency of bad predictions
# # (badClass_elev <- temp_TEMP %>% 
# #     mutate(elevRound = plyr::round_any(elevation, 100)) %>% 
# #     st_drop_geometry() %>% 
# #     group_by(elevRound, newRegion) %>% 
# #     dplyr::summarize(goodPredNumber = sum(goodPred),
# #                      badPredNumber = sum(!goodPred),
# #                      totalPredNumber = n()) %>% 
# #     mutate(badPredRate = badPredNumber/totalPredNumber)
# # )
# # 
# # (badPredElevation_mapA <- ggplot(badClass_elev,aes(x = elevRound, y = badPredRate, col = newRegion)) + 
# #     geom_point(data = badClass_elev[badClass_elev$totalPredNumber<100,],
# #                aes(x = elevRound, y = badPredRate), col = "black", pch = 8, size = 4) +
# #     geom_point() +
# #     geom_line() +
# #     labs(title = "Misclassification",
# #          x = "elevation - m", 
# #          y = "Average rate of misclassification", 
# #          subtitle = "asterisks indicates elevation bands where there are <100 observations") +
# #     theme_minimal() +
# #     scale_color_discrete(type = c("#bf812d","#35978f")))
# # 
# 
# ## confusion matrix
# regMod_3_confMat <- caret::confusionMatrix(data = temp_b$newRegion_predClass,
#                                            reference = temp_b$newRegionFact)$table
# 
# ## plot effect sizes
# beta.table_3 <- data.frame(summary(testMod_3)$coef)
# beta.table_3$variable <- row.names(beta.table_3)
# beta.table_3 <- mutate(beta.table_3, estimate = Estimate, 
#                        se = Std..Error, 
#                        low = Estimate - 2*se, 
#                        high = Estimate + 2*se,
#                        p = Pr...z..)  
# effSizeFig_3 <- ggplot(beta.table_3, aes(y=estimate, x=variable, ymin=low, ymax=high, color = (p < 0.05))) + 
#   #guides(color = guide_legend(NULL))+
#   geom_pointrange() +  
#   geom_hline(yintercept = 0, col="darkgrey", lty = 3, lwd=2) +
#   coord_flip() + theme_minimal() + ggtitle("Effect Sizes")
# 
# 
# ## make plots of good and bad probability
# # just good predictions
# (regMod_3_ternGoodPreds <- temp_b %>% 
#     filter(goodPred == TRUE) %>% 
#     ggplot() +
#     geom_density(aes(x = newRegion_pred, col = newRegionFact), trim = "FALSE") +
#     theme_minimal() + 
#     labs(color = "true ecoregion") +
#     ggtitle("Accurate Predictions") +
#     xlab("Model-Predicted Ecoregion")+
#     scale_x_continuous(breaks = c(0, .25, .5, .75, 1), 
#                        labels = c("shrub/grass", 0.25, 0.50, 0.75, "forest")) + 
#     scale_color_discrete(type = c("#bf812d","#35978f"))) 
# # just bad predictions
# badDat <- temp_b %>% 
#   filter(goodPred == FALSE)
# (regMod_3_ternBadPreds <- 
#     ggplot(badDat) +
#     geom_density(aes(x = newRegion_pred, col = newRegionFact
#     ), trim = "FALSE") +
#     theme_minimal() + 
#     labs(color = "true ecoregion") +
#     ggtitle("Inaccurate Predictions")+
#     xlab("Model-Predicted Ecoregion") +
#     scale_x_continuous(breaks = c(0, .25, .5, .75, 1), 
#                        labels = c("shrub/grass", 0.25, 0.50, 0.75, "forest")) + 
#     scale_color_discrete(type = c("#bf812d","#35978f")))
# 
# (regMod_3Pred_mapBadGrass <- ggplot() +
#     geom_point(data = temp_b, aes(Long, Lat, col = newRegion), alpha = .3) +
#     ggtitle("Severity of Grass/Shrub misclassification") +
#     geom_point(data = temp_b[temp_b$goodPred == FALSE & temp_b$newRegion_predClass == "dryShrubGrass",], 
#                aes(Long, Lat, fill = newRegion_pred),
#                col = "black" ,
#                shape = 21) +
#     scale_fill_gradient(high = "#f5f5f5", low = "#543005") +
#     scale_color_discrete(type = c("#bf812d","#35978f")) +
#     theme_minimal()
# )
# 
# (regMod_3Pred_mapForest <- ggplot() +
#     geom_point(data = temp_b, aes(Long, Lat, col = newRegion), alpha = .1) +
#     ggtitle("Severity of Forest misclassification") +
#     geom_point(data = temp_b[temp_b$goodPred == FALSE & temp_b$newRegion_predClass == "forest",], 
#                aes(Long, Lat, fill = newRegion_pred),
#                col = "black" ,
#                shape = 21) +
#     scale_fill_gradient(high = "#003c30", low = "#f5f5f5") +
#     scale_color_discrete(type = c("#bf812d","#35978f")) +
#     theme_minimal()
# ) 
# 
# ## arrange all of the figures for this model
# ggpubr::annotate_figure(ggarrange(
#   # regMod_3_varImpPlot,
#   ggplotify::as.grob(gt::as_gtable(gt::gt(data.frame("dryShrubGrass" =  data.frame(regMod_3_confMat)$Freq[1:2], 
#                                                      "Forest" = data.frame(regMod_3_confMat)$Freq[3:4],
#                                                      row.names = c("dryShrubGrass", "Forest")), 
#                                           rownames_to_stub = TRUE))), 
#   effSizeFig_3,
#   regMod_3_PredMAP, 
#   regMod_3_PredMAP_GB, 
#   regMod_3_PredMAP_PIPO,
#   #badPredElevation_mapA,
#   regMod_3_ternGoodPreds,
#   regMod_3_ternBadPreds,
#   regMod_3Pred_mapBadGrass, 
#   regMod_3Pred_mapForest, 
#   nrow = 6
# )#, 
# #top = c("Model with depth = 7 and all climate and soil predictors")
# ) %>% 
#   ggexport(
#     filename = "./Figures/EcoRegionModelFigures/ModelFigures_WithVPD_TwoEcoregionsRegressionModelResults.pdf",
#     width = 15, height = 45)
# 
# 
# # w/out VPD: drop correlated variables ------------------------------------
# 
# (corrPlot <- 
#    modDat_fit %>% 
#    dplyr::select(
#      "tmin_meanAnnAvg_CLIM"                  ,
#      "tmax_meanAnnAvg_CLIM"                  , "tmean_meanAnnAvg_CLIM"                 ,
#      "prcp_meanAnnTotal_CLIM"                , "T_warmestMonth_meanAnnAvg_CLIM"       , 
#      "T_coldestMonth_meanAnnAvg_CLIM"        , "precip_wettestMonth_meanAnnAvg_CLIM"  , 
#      "precip_driestMonth_meanAnnAvg_CLIM"    , "precip_Seasonality_meanAnnAvg_CLIM"   , 
#      "PrecipTempCorr_meanAnnAvg_CLIM"       ,  "aboveFreezing_month_meanAnnAvg_CLIM"   ,
#      "isothermality_meanAnnAvg_CLIM"        ,  "annWaterDeficit_meanAnnAvg_CLIM"       ,
#      "annWetDegDays_meanAnnAvg_CLIM"        ,  #"annVPD_mean_meanAnnAvg_CLIM"           ,
#      #"annVPD_max_meanAnnAvg_CLIM"           ,  "annVPD_min_meanAnnAvg_CLIM"            ,
#      #"annVPD_max_95percentile_CLIM"         ,  
#      "annWaterDeficit_95percentile_CLIM"     ,
#      "annWetDegDays_5percentile_CLIM"       ,  "durationFrostFreeDays_5percentile_CLIM",
#      "durationFrostFreeDays_meanAnnAvg_CLIM",  "soilDepth"                             ,
#      "surfaceClay_perc"                     ,  "avgSandPerc_acrossDepth"               ,
#      "avgCoarsePerc_acrossDepth"            ,  "avgOrganicCarbonPerc_0_3cm"            ,
#      "totalAvailableWaterHoldingCapacity" 
#    ) %>% 
#    rename(
#      "prcp" = prcp_meanAnnTotal_CLIM,
#      "prcp \n TempCorr" = PrecipTempCorr_meanAnnAvg_CLIM,  "isothermality" = isothermality_meanAnnAvg_CLIM,
#      "Wet \n DegDays" = annWetDegDays_meanAnnAvg_CLIM) %>%
#    cor()  %>% 
#    caret::findCorrelation(cutoff = .7, verbose = TRUE, names = TRUE, exact = TRUE))
# # the findCorrelation() function says we should remove these variables: 
# # "tmin_meanAnnAvg_CLIM"  
# #  "durationFrostFreeDays_meanAnnAvg_CLIM" 
# # "tmean_meanAnnAvg_CLIM"   
# # "aboveFreezing_month_meanAnnAvg_CLIM"  
# #, "durationFrostFreeDays_5percentile_CLIM",
# # "tmax_meanAnnAvg_CLIM"
# # "Wet \n DegDays" 
# # "annWaterDeficit_95percentile_CLIM"
# # "annWetDegDays_5percentile_CLIM
# # "annWaterDeficit_meanAnnAvg_CLIM"
# # "prcp" 
# # "precip_Seasonality_meanAnnAvg_CLIM" 
# # "totalAvailableWaterHoldingCapacity"
# # "surfaceClay_perc
# 
# ## also remove wet degree days, since it's correlated w/ 
# (corrPlot2 <- 
#     modDat_fit %>% 
#     dplyr::select(#"tmin_meanAnnAvg_CLIM"              ,
#       #"tmax_meanAnnAvg_CLIM" ,                 
#       #"tmean_meanAnnAvg_CLIM"                  ,
#       #"prcp_meanAnnTotal_CLIM"                 ,
#       "T_warmestMonth_meanAnnAvg_CLIM"       , 
#       "T_coldestMonth_meanAnnAvg_CLIM"         ,"precip_wettestMonth_meanAnnAvg_CLIM"    ,
#       "precip_driestMonth_meanAnnAvg_CLIM"   , 
#       #"precip_Seasonality_meanAnnAvg_CLIM"     ,
#       "PrecipTempCorr_meanAnnAvg_CLIM"        , #"aboveFreezing_month_meanAnnAvg_CLIM" ,  
#       "isothermality_meanAnnAvg_CLIM"          ,#"annWaterDeficit_meanAnnAvg_CLIM"       , #"annWetDegDays_meanAnnAvg_CLIM"       ,  
#       #"annVPD_mean_meanAnnAvg_CLIM"            ,"annVPD_max_meanAnnAvg_CLIM"            , "annVPD_min_meanAnnAvg_CLIM"          ,  
#       #"annVPD_max_95percentile_CLIM"           ,#"annWaterDeficit_95percentile_CLIM"     , 
#       #"annWetDegDays_5percentile_CLIM"      ,  
#       #"durationFrostFreeDays_5percentile_CLIM" ,#"durationFrostFreeDays_meanAnnAvg_CLIM" , 
#       "soilDepth"                           ,  
#       # "surfaceClay_perc"                       ,
#       "avgSandPerc_acrossDepth"               , "avgCoarsePerc_acrossDepth"           ,  
#       "avgOrganicCarbonPerc_0_3cm"            #, #"totalAvailableWaterHoldingCapacity"
#     ) %>% 
#     rename(#"MAP" =  prcp_meanAnnTotal_CLIM,
#       "T_warmest \nmonth" = T_warmestMonth_meanAnnAvg_CLIM, "T_coldest \nmonth" = T_coldestMonth_meanAnnAvg_CLIM,
#       "precip_wettest" = precip_wettestMonth_meanAnnAvg_CLIM, "precip_driest" = precip_driestMonth_meanAnnAvg_CLIM,
#       "P/T corr" = PrecipTempCorr_meanAnnAvg_CLIM, "isothermality" = isothermality_meanAnnAvg_CLIM,
#       #"watDef" = annWaterDeficit_meanAnnAvg_CLIM,
#       #"Wet \n DegDays" = annWetDegDays_meanAnnAvg_CLIM,
#       #"VPD_max" = annVPD_max_meanAnnAvg_CLIM,
#       #"VPD_min" = annVPD_min_meanAnnAvg_CLIM,
#       #"VPD_max_95" = annVPD_max_95percentile_CLIM, 
#       #"watDef_95" = annWaterDeficit_95percentile_CLIM,
#       #"wetDegDays_5" = annWetDegDays_5percentile_CLIM,
#       #"surfClay" = surfaceClay_perc,
#       "sand" = avgSandPerc_acrossDepth,
#       "coarse" = avgCoarsePerc_acrossDepth,
#       "carbon" = avgOrganicCarbonPerc_0_3cm #, "waterCap." = totalAvailableWaterHoldingCapacity
#     ) %>%
#     #cor() %>% 
#     # findCorrelation(cutoff = .7, verbose = TRUE, names = TRUE, exact = TRUE) 
#     slice_sample(n = 5e4) %>% 
#     ggpairs( upper = list(continuous = my_fn), lower = list(continuous = GGally::wrap("points", alpha = 0.1, size=0.2)), progress = FALSE))
# 
# # w/out VPD: Fit a simple regression w/ two ecoregions ----------------
# 
# modDat_fitNew <- modDat_fit %>% 
#   mutate(newRegion = str_replace(newRegion, "eastForest", "forest"),
#          newRegion = str_replace(newRegion, "westForest", "forest")) %>% 
#   mutate(newRegionFact = as.factor(newRegion)) %>% 
#   mutate(T_warmestMonth_S = scale(T_warmestMonth_meanAnnAvg_CLIM),
#          T_coldestMonth_S = scale(T_coldestMonth_meanAnnAvg_CLIM),
#          precip_wettestMonth_S = scale(precip_wettestMonth_meanAnnAvg_CLIM),
#          precip_driestMonth_S = scale(precip_driestMonth_meanAnnAvg_CLIM),
#          PrecipTempCorr_S = scale(PrecipTempCorr_meanAnnAvg_CLIM),
#          isothermality_S = scale(isothermality_meanAnnAvg_CLIM),
#          soilDepth_S = scale(soilDepth),
#          avgSandPerc_S = scale(avgSandPerc_acrossDepth),
#          avgCoarsePerc_S = scale(avgCoarsePerc_acrossDepth),
#          avgOrganicCarbon_S = scale(avgOrganicCarbonPerc_0_3cm))
# 
# modDat_testNew <- modDat_test %>% 
#   mutate(newRegion = str_replace(newRegion, "eastForest", "forest"),
#          newRegion = str_replace(newRegion, "westForest", "forest")) %>% 
#   mutate(newRegionFact = as.factor(newRegion))
# 
# testMod_2 <- glm(newRegionFact ~ T_warmestMonth_meanAnnAvg_CLIM       + 
#                    T_coldestMonth_meanAnnAvg_CLIM         +
#                    precip_wettestMonth_meanAnnAvg_CLIM    +
#                    precip_driestMonth_meanAnnAvg_CLIM   + 
#                    PrecipTempCorr_meanAnnAvg_CLIM        +   
#                    isothermality_meanAnnAvg_CLIM          +
#                    soilDepth                           +  
#                    avgSandPerc_acrossDepth               + avgCoarsePerc_acrossDepth           +  
#                    avgOrganicCarbonPerc_0_3cm,
#                  data = modDat_fitNew, 
#                  family = "binomial", 
#                  na.action = na.fail
# )
# 
# summary(testMod_2)
# 
# head(pp <- fitted(testMod_2))
# 
# AIC(testMod_2) #AIC: 241225.9
# # tetestMod_2# test the predictions of the model
# predictionDat <- modDat_testNew
# predictionDatTemp <- cbind(predictionDat,"newRegion_pred" = predict(testMod_2, newdata = predictionDat, "response"))
# 
# # for each observation, determine which category has the highest predicted probability
# (temp <- predictionDatTemp %>%
#     rowwise() %>% 
#     mutate(newRegion_predClass = newRegion_pred) %>% 
#     mutate(newRegion_predClass = replace(newRegion_predClass, newRegion_pred<.5, "dryShrubGrass"), 
#            newRegion_predClass = replace(newRegion_predClass, newRegion_pred>.5, "forest")) 
# )
# 
# temp$newRegion_predClass <- relevel(as.factor(temp$newRegion_predClass), ref = "dryShrubGrass")
# 
# # are these predictions the same as the actual classifications
# temp$goodPred <- temp$newRegionFact == temp$newRegion_predClass
# sum(!temp$goodPred)/nrow(temp) * 100
# 
# 
# ## make predictions rasterized 
# 
# temp2 <- temp %>% 
#   terra::vect(geom = c("Long", "Lat"), crs = crs(soilRastTemp)
#   ) %>% 
#   terra::rasterize(y = 
#                      terra::aggregate(soilRastTemp, fact = 2, fun = mean, na.rm= TRUE)
#                    , field = "newRegion_pred")
# 
# (regMod_2_PredMAP <- ggplot(temp) +
#     geom_spatraster(data = temp2) +
#     # geom_point(aes(#Long, Lat, 
#     #   col = newRegion_pred)) + 
#     scale_fill_distiller(type = "div", palette = 1, direction = 1) +
#     #geom_sf(data = ecoregionsSF2, color = "black", fill = NA) + 
#     #lims(x = c(-130, -65), y = c(24, 50))
#     
#     
#     #geom_point(aes(Long, Lat, col = newRegion_pred), alpha = .1) +
#     ggtitle("model-predicted ecoregion classification -- 2 ecoregions; no VPD",
#             subtitle = paste0("ecoregion ~ temp_warmestMonth + temp_coldestMonth + precip_wettestMonth +
#             precip_driestMonth + precipTempCorr + isothermality + soilDepth + % sand + % coarse + soil carbon
#            \n black dots indicate misclassification; \n  ", 
#                               round((100-sum(!temp$goodPred)/nrow(temp) * 100),1), 
#                               "% classification accuracy" )) +
#     geom_point(data = temp[temp$goodPred == FALSE,], aes(Long, Lat#, fill = as.factor(newRegion_predClass)
#     ),
#     col = "black"
#     , shape = 21) #+ 
#   #scale_fill_discrete(type = c("#bf812d","#35978f"))
# )
# 
# ## zoom in on the great basin to check out mis-classification
# (regMod_2_PredMAP_GB <- ggplot(temp) +
#     geom_spatraster(data = temp2) +
#     # geom_point(aes(#Long, Lat, 
#     #   col = newRegion_pred)) + 
#     scale_fill_distiller(type = "div", palette = 1, direction = 1) +
#     #geom_sf(data = ecoregionsSF2, color = "black", fill = NA) + 
#     #lims(x = c(-130, -65), y = c(24, 50))
#     
#     
#     #geom_point(aes(Long, Lat, col = newRegion_pred), alpha = .1) +
#     ggtitle("model-predicted ecoregion classification -- 2 ecoregions; no VPD",
#             subtitle = paste0("Zoom-In on Great Basin Area
#            \n dots indicate misclassification; color indicates true classification; \n  ", 
#                               round((100-sum(!temp$goodPred)/nrow(temp) * 100),1), 
#                               "% classification accuracy" )) +
#     geom_point(data = temp[temp$goodPred == FALSE,], aes(Long, Lat#, fill = as.factor(newRegion_predClass)
#     ),
#     col = "black"
#     , shape = 21) +
#     xlim(c(-1911149,-999000)) + 
#     ylim(c(-600105, 0))
#   #scale_fill_discrete(type = c("#bf812d","#35978f"))
# )
# 
# ## zoom in on the AZ high elevation area to check out mis-classification
# (regMod_2_PredMAP_PIPO <- ggplot(temp) +
#     geom_spatraster(data = temp2) +
#     # geom_point(aes(#Long, Lat, 
#     #   col = newRegion_pred)) + 
#     scale_fill_distiller(type = "div", palette = 1, direction = 1) +
#     #geom_sf(data = ecoregionsSF2, color = "black", fill = NA) + 
#     #lims(x = c(-130, -65), y = c(24, 50))
#     
#     
#     #geom_point(aes(Long, Lat, col = newRegion_pred), alpha = .1) +
#     ggtitle("model-predicted ecoregion classification -- 2 ecoregions; no VPD",
#             subtitle = paste0("Zoom-In on high elevation area in the SW;
#            \n dots indicate misclassification; color indicates true classification \n  ", 
#                               round((100-sum(!temp$goodPred)/nrow(temp) * 100),1), 
#                               "% classification accuracy" )) +
#     geom_point(data = temp[temp$goodPred == FALSE,], aes(Long, Lat#, fill = as.factor(newRegion_predClass)
#     ),
#     col = "black"
#     , shape = 21) +
#     xlim(c(-1211149,-559000)) + 
#     ylim(c(-1050105, -550000))
#   #scale_fill_discrete(type = c("#bf812d","#35978f"))
# )
# 
# ## Look at the relationship between misclassification and elevation
# # get elevation information for each raster cell 
# temp_TEMP <- temp %>% 
#   st_as_sf(coords = c("Long", "Lat"), crs = crs(soilRastTemp))
# 
# temp_elev <- elevatr::get_elev_point(temp2, src = "aws")
# 
# temp_TEMP <- temp_TEMP %>% st_join(temp_elev, left = TRUE, join = st_nearest_feature)
# 
# # within each band of elevation (100 m), what is the frequency of bad predictions
# (badClass_elev <- temp_TEMP %>% 
#   mutate(elevRound = plyr::round_any(elevation, 100)) %>% 
#   st_drop_geometry() %>% 
#   group_by(elevRound, newRegion) %>% 
#   dplyr::summarize(goodPredNumber = sum(goodPred),
#                    badPredNumber = sum(!goodPred),
#             totalPredNumber = n()) %>% 
#   mutate(badPredRate = badPredNumber/totalPredNumber)
# )
# 
# (badPredElevation_mapA <- ggplot(badClass_elev,aes(x = elevRound, y = badPredRate, col = newRegion)) + 
#     geom_point(data = badClass_elev[badClass_elev$totalPredNumber<100,],
#                aes(x = elevRound, y = badPredRate), col = "black", pch = 8, size = 4) +
#   geom_point() +
#   geom_line() +
#     labs(title = "Misclassification",
#          x = "elevation - m", 
#          y = "Average rate of misclassification", 
#          subtitle = "asterisks indicates elevation bands where there are <100 observations") +
#   theme_minimal() +
#   scale_color_discrete(type = c("#bf812d","#35978f")))
# 
#   # badPredElevation_mapB <- ggplot(badClass_elev,aes(x = elevRound, y = totalPredNumber, col = newRegion)) + 
#   # geom_point() +
#   # geom_line() +
#   #   theme_minimal() +
#   # labs(title = "Misclassification",
#   #      x = "elevation - m", 
#   #      y = "Average rate of misclassification", 
#   #      subtitle = "asterisk indicates elevation bands where there are <100 observations")+
#   # 
#   # scale_color_discrete(type = c("#bf812d","#35978f")) + 
#   # ylim(c(0,100)) 
# 
# ## confusion matrix
# regMod_2_confMat <- caret::confusionMatrix(data = temp$newRegion_predClass,
#                                            reference = temp$newRegionFact)$table
# 
# ## make plots of good and bad probability
# # just good predictions
# (regMod_2_ternGoodPreds <- temp %>% 
#     filter(goodPred == TRUE) %>% 
#     ggplot() +
#     geom_density(aes(x = newRegion_pred, col = newRegionFact), trim = "FALSE") +
#     theme_minimal() + 
#     labs(color = "true ecoregion") +
#     ggtitle("Accurate Predictions") +
#     xlab("Model-Predicted Ecoregion")+
#     scale_x_continuous(breaks = c(0, .25, .5, .75, 1), 
#                        labels = c("shrub/grass", 0.25, 0.50, 0.75, "forest")) + 
#     scale_color_discrete(type = c("#bf812d","#35978f"))
# )
# # just bad predictions
# badDat <- temp %>% 
#   filter(goodPred == FALSE)
# (regMod_2_ternBadPreds <- 
#     ggplot(badDat) +
#     geom_density(aes(x = newRegion_pred, col = newRegionFact
#     ), trim = "FALSE") +
#     theme_minimal() + 
#     labs(color = "true ecoregion") +
#     ggtitle("Inaccurate Predictions")+
#     xlab("Model-Predicted Ecoregion") +
#     scale_x_continuous(breaks = c(0, .25, .5, .75, 1), 
#                        labels = c("shrub/grass", 0.25, 0.50, 0.75, "forest")) + 
#     scale_color_discrete(type = c("#bf812d","#35978f")))
# 
# (regMod_2Pred_mapBadGrass <- ggplot() +
#     geom_point(data = temp, aes(Long, Lat, col = newRegion), alpha = .3) +
#     ggtitle("Severity of Grass/Shrub misclassification") +
#     geom_point(data = temp[temp$goodPred == FALSE & temp$newRegion_predClass == "dryShrubGrass",], 
#                aes(Long, Lat, fill = newRegion_pred),
#                col = "black" ,
#                shape = 21) +
#     scale_fill_gradient(high = "#f5f5f5", low = "#543005") +
#     scale_color_discrete(type = c("#bf812d","#35978f")) +
#     theme_minimal()
# )
# 
# (regMod_2Pred_mapForest <- ggplot() +
#     geom_point(data = temp, aes(Long, Lat, col = newRegion), alpha = .1) +
#     ggtitle("Severity of Forest misclassification") +
#     geom_point(data = temp[temp$goodPred == FALSE & temp$newRegion_predClass == "forest",], 
#                aes(Long, Lat, fill = newRegion_pred),
#                col = "black" ,
#                shape = 21) +
#     scale_fill_gradient(high = "#003c30", low = "#f5f5f5") +
#     scale_color_discrete(type = c("#bf812d","#35978f")) +
#     theme_minimal()
# ) 
# 
# 
# ## make variable importance figures
# caret::getModelInfo()
# varImp(testMod_2)
# beta.table_2 <- data.frame(summary(testMod_2)$coef)
# beta.table_2$variable <- row.names(beta.table_2)
# beta.table_2 <- mutate(beta.table_2, estimate = Estimate, 
#                        se = Std..Error, 
#                        low = Estimate - 2*se, 
#                        high = Estimate + 2*se,
#                        p = Pr...z..)  
# 
# effSizeFig_2 <- ggplot(beta.table_2, aes(y=estimate, x=variable, ymin=low, ymax=high, color = (p < 0.05))) + 
#   #guides(color = guide_legend(NULL))+
#   geom_pointrange() +  
#   geom_hline(yintercept = 0, col="darkgrey", lty = 3, lwd=2) +
#   coord_flip() + theme_minimal() + ggtitle("Effect Sizes")
# 
# ## make decile plots 
# # reorganize data frame
# # get decile values
# tempDec <- temp %>% 
#   pivot_longer(cols = c(T_warmestMonth_meanAnnAvg_CLIM, T_coldestMonth_meanAnnAvg_CLIM , precip_wettestMonth_meanAnnAvg_CLIM , 
#                         precip_driestMonth_meanAnnAvg_CLIM , PrecipTempCorr_meanAnnAvg_CLIM , 
#                         isothermality_meanAnnAvg_CLIM , soilDepth , avgSandPerc_acrossDepth , 
#                         avgCoarsePerc_acrossDepth , avgOrganicCarbonPerc_0_3cm ), 
#                names_to = "predictor_name",
#                values_to = "predictor_value"
#   ) %>% 
#   dplyr::select(newRegion_pred, predictor_name, predictor_value)
# tempQuantiles <- tempDec %>% 
#   group_by(predictor_name) %>% 
#   reframe(quantiles = stats::quantile(predictor_value, c(0, .09,.1,.19, .2, .29, .3, .39, .4, .49, 
#                                                          .5, .59, .6, .69, .7, .79, .8, .89, .9, 1)), 
#           quantilesNames = paste0("quant_",c(0, .09,.1,.19, .2, .29, .3, .39, .4, .49, 
#                                              .5, .59, .6, .69, .7, .79, .8, .89, .9, 1))) %>% 
#   pivot_wider( names_from = quantilesNames, 
#                values_from = quantiles)
# 
# test <- tempDec %>% 
#   left_join(tempQuantiles) %>% 
#   mutate(#YesQuant0 = dplyr::between(predictor_value, left = quant_0, right = quant_0.09),
#     YesQuant1 = dplyr::between(predictor_value, left = quant_0.1 , right = quant_0.19),
#     YesQuant2 = dplyr::between(predictor_value, left = quant_0.2 , right = quant_0.29),
#     YesQuant3 = dplyr::between(predictor_value, left = quant_0.3 , right = quant_0.39),
#     YesQuant4 = dplyr::between(predictor_value, left = quant_0.4 , right = quant_0.49),
#     YesQuant5 = dplyr::between(predictor_value, left = quant_0.5 , right = quant_0.59),
#     YesQuant6 = dplyr::between(predictor_value, left = quant_0.6 , right = quant_0.69),
#     YesQuant7 = dplyr::between(predictor_value, left = quant_0.7 , right = quant_0.79),
#     YesQuant8 = dplyr::between(predictor_value, left = quant_0.8 , right = quant_0.89),
#     #YesQuant9 = dplyr::between(predictor_value, left = quant_0.9 , right = quant_1)
#     ) %>% 
#   mutate(#YesQuant0 = case_match(YesQuant0, TRUE ~ quant_0.09),
#     YesQuant1 = case_match(YesQuant1, TRUE ~ quant_0.19),
#     YesQuant2 = case_match(YesQuant2, TRUE ~ quant_0.29),
#     YesQuant3 = case_match(YesQuant3, TRUE ~ quant_0.39),
#     YesQuant4 = case_match(YesQuant4, TRUE ~ quant_0.49),
#     YesQuant5 = case_match(YesQuant5, TRUE ~ quant_0.59),
#     YesQuant6 = case_match(YesQuant6, TRUE ~ quant_0.69),
#     YesQuant7 = case_match(YesQuant7, TRUE ~ quant_0.79),
#     YesQuant8 = case_match(YesQuant8, TRUE ~ quant_0.89)
#     #,YesQuant9 = case_match(YesQuant9, TRUE ~ quant_1)
#     ) %>% 
#   #slice(1:1000) %>% 
#   rowwise() %>% 
#   mutate(Quantile_value = round(sum(na.omit(YesQuant1), na.omit(YesQuant2), na.omit(YesQuant3),
#                                     na.omit(YesQuant4), na.omit(YesQuant5), na.omit(YesQuant6), na.omit(YesQuant7),
#                                     na.omit(YesQuant8)#, na.omit(YesQuant0)
#                                     ), 
#                                     5)) %>% 
#   dplyr::select(-(quant_1:YesQuant8)) %>% 
#   group_by(predictor_name, Quantile_value) %>% 
#   summarize(newRegion_pred_mean = mean(newRegion_pred)) 
# 
# # make a plot 
# (quantPlot <- ggplot(data = test) + 
#     facet_wrap(~predictor_name, scales = "free_x") + 
#     #geom_density(aes(Quantile_value, col = predictor_name)) + 
#     geom_line(aes(x = Quantile_value, y = newRegion_pred_mean, col = predictor_name)) +
#     geom_point(aes(x = Quantile_value, y = newRegion_pred_mean, col = predictor_name)) + 
#     theme_minimal() + 
#     labs(y = "mean predicted probability w/in a decile", x = "predictor value") + 
#     scale_color_discrete(guide = "none"))
# 
# ## arrange all of the figures for this model
# ggpubr::annotate_figure(ggarrange(
#   # regMod_2_varImpPlot,
#   ggplotify::as.grob(gt::as_gtable(gt::gt(data.frame("dryShrubGrass" =  data.frame(regMod_2_confMat)$Freq[1:2], 
#                                                      "Forest" = data.frame(regMod_2_confMat)$Freq[3:4],
#                                                      row.names = c("dryShrubGrass", "Forest")), 
#                                           rownames_to_stub = TRUE))), 
#   effSizeFig_2,
#   regMod_2_PredMAP, 
#   regMod_2_PredMAP_GB, 
#   regMod_2_PredMAP_PIPO,
#   #badPredElevation_mapA,
#   regMod_2_ternGoodPreds,
#   regMod_2_ternBadPreds,
#   regMod_2Pred_mapBadGrass, 
#   regMod_2Pred_mapForest, 
#   nrow = 6
# )#, 
# #top = c("Model with depth = 7 and all climate and soil predictors")
# ) %>% 
#   ggexport(
#     filename = "./Figures/EcoRegionModelFigures/ModelFigures_WithOUTVPD_TwoEcoregionsRegressionModelResults.pdf",
#     width = 15, height = 45)
# 
# 
# 
# 
# 
# # w/out VPD -- precip driest month REMOVED  : Fit a simple regression w/ two ecoregions ----------------
# 
# modDat_fitNew <- modDat_fit %>% 
#   mutate(newRegion = str_replace(newRegion, "eastForest", "forest"),
#          newRegion = str_replace(newRegion, "westForest", "forest")) %>% 
#   mutate(newRegionFact = as.factor(newRegion)) 
# 
# modDat_testNew <- modDat_test %>% 
#   mutate(newRegion = str_replace(newRegion, "eastForest", "forest"),
#          newRegion = str_replace(newRegion, "westForest", "forest")) %>% 
#   mutate(newRegionFact = as.factor(newRegion))
# 
# testMod_2b <- glm(newRegionFact ~ T_warmestMonth_meanAnnAvg_CLIM       + 
#       T_coldestMonth_meanAnnAvg_CLIM         +
#       precip_wettestMonth_meanAnnAvg_CLIM    +
#       #precip_driestMonth_meanAnnAvg_CLIM   + 
#       PrecipTempCorr_meanAnnAvg_CLIM        +   
#       isothermality_meanAnnAvg_CLIM          +
#       soilDepth                           +  
#       avgSandPerc_acrossDepth               + avgCoarsePerc_acrossDepth           +  
#       avgOrganicCarbonPerc_0_3cm,
#     data = modDat_fitNew, 
#     family = "binomial", 
#     na.action = na.fail
# )
# 
# summary(testMod_2b)
# 
# head(pp <- fitted(testMod_2b))
# 
# AIC(testMod_2b) #AIC: 241225.9
# # tetestMod_2b# test the predictions of the model
# predictionDat <- modDat_testNew
# predictionDatTemp <- cbind(predictionDat,"newRegion_pred" = predict(testMod_2b, newdata = predictionDat, "response"))
# 
# # for each observation, determine which category has the highest predicted probability
# (temp <- predictionDatTemp %>%
#     rowwise() %>% 
#     mutate(newRegion_predClass = newRegion_pred) %>% 
#     mutate(newRegion_predClass = replace(newRegion_predClass, newRegion_pred<.5, "dryShrubGrass"), 
#            newRegion_predClass = replace(newRegion_predClass, newRegion_pred>.5, "forest")) 
# )
# 
# temp$newRegion_predClass <- relevel(as.factor(temp$newRegion_predClass), ref = "dryShrubGrass")
# 
# # are these predictions the same as the actual classifications
# temp$goodPred <- temp$newRegionFact == temp$newRegion_predClass
# sum(!temp$goodPred)/nrow(temp) * 100
# 
# 
# ## make predictions rasterized 
# 
# temp2 <- temp %>% 
#   terra::vect(geom = c("Long", "Lat"), crs = crs(soilRastTemp)
#   ) %>% 
#   terra::rasterize(y = 
#                      terra::aggregate(soilRastTemp, fact = 2, fun = mean, na.rm= TRUE)
#                    , field = "newRegion_pred")
# 
# (regMod_2_PredMAP <- ggplot(temp) +
#     geom_spatraster(data = temp2) +
#     # geom_point(aes(#Long, Lat, 
#     #   col = newRegion_pred)) + 
#     scale_fill_distiller(type = "div", palette = 1, direction = 1) +
#     #geom_sf(data = ecoregionsSF2, color = "black", fill = NA) + 
#     #lims(x = c(-130, -65), y = c(24, 50))
#     
#     
#     #geom_point(aes(Long, Lat, col = newRegion_pred), alpha = .1) +
#     ggtitle("model-predicted ecoregion classification -- 2 ecoregions; no VPD",
#             subtitle = paste0("ecoregion ~ temp_warmestMonth + temp_coldestMonth + precip_wettestMonth +
#             precipTempCorr + isothermality + soilDepth + % sand + % coarse + soil carbon
#            \n black dots indicate misclassification; \n  ", 
#                               round((100-sum(!temp$goodPred)/nrow(temp) * 100),1), 
#                               "% classification accuracy" )) +
#     geom_point(data = temp[temp$goodPred == FALSE,], aes(Long, Lat#, fill = as.factor(newRegion_predClass)
#     ),
#     col = "black"
#     , shape = 21) #+ 
#   #scale_fill_discrete(type = c("#bf812d","#35978f"))
# )
# 
# ## zoom in on the great basin to check out mis-classification
# (regMod_2_PredMAP_GB <- ggplot(temp) +
#     geom_spatraster(data = temp2) +
#     # geom_point(aes(#Long, Lat, 
#     #   col = newRegion_pred)) + 
#     scale_fill_distiller(type = "div", palette = 1, direction = 1) +
#     #geom_sf(data = ecoregionsSF2, color = "black", fill = NA) + 
#     #lims(x = c(-130, -65), y = c(24, 50))
#     
#     
#     #geom_point(aes(Long, Lat, col = newRegion_pred), alpha = .1) +
#     ggtitle("model-predicted ecoregion classification -- 2 ecoregions; no VPD",
#             subtitle = paste0("Zoom-In on Great Basin Area
#            \n dots indicate misclassification; color indicates true classification; \n  ", 
#                               round((100-sum(!temp$goodPred)/nrow(temp) * 100),1), 
#                               "% classification accuracy" )) +
#     geom_point(data = temp[temp$goodPred == FALSE,], aes(Long, Lat#, fill = as.factor(newRegion_predClass)
#     ),
#     col = "black"
#     , shape = 21) +
#     xlim(c(-1911149,-999000)) + 
#     ylim(c(-600105, 0))
#   #scale_fill_discrete(type = c("#bf812d","#35978f"))
# )
# 
# ## zoom in on the AZ high elevation area to check out mis-classification
# (regMod_2_PredMAP_PIPO <- ggplot(temp) +
#     geom_spatraster(data = temp2) +
#     # geom_point(aes(#Long, Lat, 
#     #   col = newRegion_pred)) + 
#     scale_fill_distiller(type = "div", palette = 1, direction = 1) +
#     #geom_sf(data = ecoregionsSF2, color = "black", fill = NA) + 
#     #lims(x = c(-130, -65), y = c(24, 50))
#     
#     
#     #geom_point(aes(Long, Lat, col = newRegion_pred), alpha = .1) +
#     ggtitle("model-predicted ecoregion classification -- 2 ecoregions; no VPD",
#             subtitle = paste0("Zoom-In on high elevation area in the SW;
#            \n dots indicate misclassification; color indicates true classification \n  ", 
#                               round((100-sum(!temp$goodPred)/nrow(temp) * 100),1), 
#                               "% classification accuracy" )) +
#     geom_point(data = temp[temp$goodPred == FALSE,], aes(Long, Lat#, fill = as.factor(newRegion_predClass)
#     ),
#     col = "black"
#     , shape = 21) +
#     xlim(c(-1211149,-559000)) + 
#     ylim(c(-1050105, -550000))
#   #scale_fill_discrete(type = c("#bf812d","#35978f"))
# )
# 
# ## Look at the relationship between misclassification and elevation
# # # get elevation information for each raster cell 
# # temp_TEMP <- temp %>% 
# #   st_as_sf(coords = c("Long", "Lat"), crs = crs(soilRastTemp))
# # 
# # temp_elev <- elevatr::get_elev_point(temp2, src = "aws")
# # 
# # temp_TEMP <- temp_TEMP %>% st_join(temp_elev, left = TRUE, join = st_nearest_feature)
# 
# ## confusion matrix
# regMod_2_confMat <- caret::confusionMatrix(data = temp$newRegion_predClass,
#                                            reference = temp$newRegionFact)$table
# 
# ## make plots of good and bad probability
# # just good predictions
# (regMod_2_ternGoodPreds <- temp %>% 
#     filter(goodPred == TRUE) %>% 
#     ggplot() +
#     geom_density(aes(x = newRegion_pred, col = newRegionFact), trim = "FALSE") +
#     theme_minimal() + 
#     labs(color = "true ecoregion") +
#     ggtitle("Accurate Predictions") +
#     xlab("Model-Predicted Ecoregion")+
#     scale_x_continuous(breaks = c(0, .25, .5, .75, 1), 
#                        labels = c("shrub/grass", 0.25, 0.50, 0.75, "forest")) + 
#     scale_color_discrete(type = c("#bf812d","#35978f"))
# )
# # just bad predictions
# badDat <- temp %>% 
#   filter(goodPred == FALSE)
# (regMod_2_ternBadPreds <- 
#     ggplot(badDat) +
#     geom_density(aes(x = newRegion_pred, col = newRegionFact
#     ), trim = "FALSE") +
#     theme_minimal() + 
#     labs(color = "true ecoregion") +
#     ggtitle("Inaccurate Predictions")+
#     xlab("Model-Predicted Ecoregion") +
#     scale_x_continuous(breaks = c(0, .25, .5, .75, 1), 
#                        labels = c("shrub/grass", 0.25, 0.50, 0.75, "forest")) + 
#     scale_color_discrete(type = c("#bf812d","#35978f")))
# 
# (regMod_2Pred_mapBadGrass <- ggplot() +
#     geom_point(data = temp, aes(Long, Lat, col = newRegion), alpha = .3) +
#     ggtitle("Severity of Grass/Shrub misclassification") +
#     geom_point(data = temp[temp$goodPred == FALSE & temp$newRegion_predClass == "dryShrubGrass",], 
#                aes(Long, Lat, fill = newRegion_pred),
#                col = "black" ,
#                shape = 21) +
#     scale_fill_gradient(high = "#f5f5f5", low = "#543005") +
#     scale_color_discrete(type = c("#bf812d","#35978f")) +
#     theme_minimal()
# )
# 
# (regMod_2Pred_mapForest <- ggplot() +
#     geom_point(data = temp, aes(Long, Lat, col = newRegion), alpha = .1) +
#     ggtitle("Severity of Forest misclassification") +
#     geom_point(data = temp[temp$goodPred == FALSE & temp$newRegion_predClass == "forest",], 
#                aes(Long, Lat, fill = newRegion_pred),
#                col = "black" ,
#                shape = 21) +
#     scale_fill_gradient(high = "#003c30", low = "#f5f5f5") +
#     scale_color_discrete(type = c("#bf812d","#35978f")) +
#     theme_minimal()
# ) 
# 
# 
# ## make variable importance figures
# #caret::getModelInfo()
# varImp(testMod_2b)
# beta.table_2 <- data.frame(summary(testMod_2b)$coef)
# beta.table_2$variable <- row.names(beta.table_2)
# beta.table_2 <- mutate(beta.table_2, estimate = Estimate, 
#                        se = Std..Error, 
#                        low = Estimate - 2*se, 
#                        high = Estimate + 2*se,
#                        p = Pr...z..)  
# 
# effSizeFig_2 <- ggplot(beta.table_2, aes(y=estimate, x=variable, ymin=low, ymax=high, color = (p < 0.05))) + 
#   #guides(color = guide_legend(NULL))+
#   geom_pointrange() +  
#   geom_hline(yintercept = 0, col="darkgrey", lty = 3, lwd=2) +
#   coord_flip() + theme_minimal() + ggtitle("Effect Sizes")
# 
# ## make decile plots 
# # reorganize data frame
# # get decile values
# tempDec <- temp %>% 
#   pivot_longer(cols = c(T_warmestMonth_meanAnnAvg_CLIM, T_coldestMonth_meanAnnAvg_CLIM , precip_wettestMonth_meanAnnAvg_CLIM , 
#                         precip_driestMonth_meanAnnAvg_CLIM , PrecipTempCorr_meanAnnAvg_CLIM , 
#                         isothermality_meanAnnAvg_CLIM , soilDepth , avgSandPerc_acrossDepth , 
#                         avgCoarsePerc_acrossDepth , avgOrganicCarbonPerc_0_3cm ), 
#                names_to = "predictor_name",
#                values_to = "predictor_value"
#   ) %>% 
#   dplyr::select(newRegion_pred, predictor_name, predictor_value)
# tempQuantiles <- tempDec %>% 
#   group_by(predictor_name) %>% 
#   reframe(quantiles = stats::quantile(predictor_value, c(0, .09,.1,.19, .2, .29, .3, .39, .4, .49, 
#                                                          .5, .59, .6, .69, .7, .79, .8, .89, .9, 1)), 
#           quantilesNames = paste0("quant_",c(0, .09,.1,.19, .2, .29, .3, .39, .4, .49, 
#                                              .5, .59, .6, .69, .7, .79, .8, .89, .9, 1))) %>% 
#   pivot_wider( names_from = quantilesNames, 
#                values_from = quantiles)
# 
# test <- tempDec %>% 
#   left_join(tempQuantiles) %>% 
#   mutate(#YesQuant0 = dplyr::between(predictor_value, left = quant_0, right = quant_0.09),
#     YesQuant1 = dplyr::between(predictor_value, left = quant_0.1 , right = quant_0.19),
#     YesQuant2 = dplyr::between(predictor_value, left = quant_0.2 , right = quant_0.29),
#     YesQuant3 = dplyr::between(predictor_value, left = quant_0.3 , right = quant_0.39),
#     YesQuant4 = dplyr::between(predictor_value, left = quant_0.4 , right = quant_0.49),
#     YesQuant5 = dplyr::between(predictor_value, left = quant_0.5 , right = quant_0.59),
#     YesQuant6 = dplyr::between(predictor_value, left = quant_0.6 , right = quant_0.69),
#     YesQuant7 = dplyr::between(predictor_value, left = quant_0.7 , right = quant_0.79),
#     YesQuant8 = dplyr::between(predictor_value, left = quant_0.8 , right = quant_0.89),
#     #YesQuant9 = dplyr::between(predictor_value, left = quant_0.9 , right = quant_1)
#   ) %>% 
#   mutate(#YesQuant0 = case_match(YesQuant0, TRUE ~ quant_0.09),
#     YesQuant1 = case_match(YesQuant1, TRUE ~ quant_0.19),
#     YesQuant2 = case_match(YesQuant2, TRUE ~ quant_0.29),
#     YesQuant3 = case_match(YesQuant3, TRUE ~ quant_0.39),
#     YesQuant4 = case_match(YesQuant4, TRUE ~ quant_0.49),
#     YesQuant5 = case_match(YesQuant5, TRUE ~ quant_0.59),
#     YesQuant6 = case_match(YesQuant6, TRUE ~ quant_0.69),
#     YesQuant7 = case_match(YesQuant7, TRUE ~ quant_0.79),
#     YesQuant8 = case_match(YesQuant8, TRUE ~ quant_0.89)
#     #,YesQuant9 = case_match(YesQuant9, TRUE ~ quant_1)
#   ) %>% 
#   #slice(1:1000) %>% 
#   rowwise() %>% 
#   mutate(Quantile_value = round(sum(na.omit(YesQuant1), na.omit(YesQuant2), na.omit(YesQuant3),
#                                     na.omit(YesQuant4), na.omit(YesQuant5), na.omit(YesQuant6), na.omit(YesQuant7),
#                                     na.omit(YesQuant8)#, na.omit(YesQuant0)
#   ), 
#   5)) %>% 
#   dplyr::select(-(quant_1:YesQuant8)) %>% 
#   group_by(predictor_name, Quantile_value) %>% 
#   summarize(newRegion_pred_mean = mean(newRegion_pred)) 
# 
# # make a plot 
# (quantPlot <- ggplot(data = test) + 
#     facet_wrap(~predictor_name, scales = "free_x") + 
#     #geom_density(aes(Quantile_value, col = predictor_name)) + 
#     geom_line(aes(x = Quantile_value, y = newRegion_pred_mean, col = predictor_name)) +
#     geom_point(aes(x = Quantile_value, y = newRegion_pred_mean, col = predictor_name)) + 
#     theme_minimal() + 
#     labs(y = "mean predicted probability w/in a decile", x = "predictor value") + 
#     scale_color_discrete(guide = "none"))
# 
# ## arrange all of the figures for this model
# ggpubr::annotate_figure(ggarrange(
#   # regMod_2_varImpPlot,
#   ggplotify::as.grob(gt::as_gtable(gt::gt(data.frame("dryShrubGrass" =  data.frame(regMod_2_confMat)$Freq[1:2], 
#                                                      "Forest" = data.frame(regMod_2_confMat)$Freq[3:4],
#                                                      row.names = c("dryShrubGrass", "Forest")), 
#                                           rownames_to_stub = TRUE))), 
#   effSizeFig_2,
#   quantPlot,
#   regMod_2_PredMAP, 
#   regMod_2_PredMAP_GB, 
#   regMod_2_PredMAP_PIPO,
#   #badPredElevation_mapA,
#   regMod_2_ternGoodPreds,
#   regMod_2_ternBadPreds,
#   regMod_2Pred_mapBadGrass, 
#   regMod_2Pred_mapForest, 
#   nrow = 6
# )#, 
# #top = c("Model with depth = 7 and all climate and soil predictors")
# ) %>% 
#   ggexport(
#     filename = "./Figures/EcoRegionModelFigures/Model_EXCLUDESPrecipDriestMonth/ContemporaryModelResults.pdf",
#     width = 15, height = 45)
# 
# 
# # w/out VPD and precip driest month replaced w/ Precip Seasonality: drop correlated variables ------------------------------------
# 
# (corrPlot <- 
#    modDat_fit %>% 
#    dplyr::select(
#      "tmin_meanAnnAvg_CLIM"                  ,
#      "tmax_meanAnnAvg_CLIM"                  , 
#      "tmean_meanAnnAvg_CLIM"                 ,
#      "prcp_meanAnnTotal_CLIM"                , 
#      "T_warmestMonth_meanAnnAvg_CLIM"       , 
#      "T_coldestMonth_meanAnnAvg_CLIM"        , 
#      "precip_wettestMonth_meanAnnAvg_CLIM"  ,  #"precip_driestMonth_meanAnnAvg_CLIM"    , 
#      "precip_Seasonality_meanAnnAvg_CLIM"   , 
#      "PrecipTempCorr_meanAnnAvg_CLIM"       ,  
#      "aboveFreezing_month_meanAnnAvg_CLIM"   ,
#      "isothermality_meanAnnAvg_CLIM"        , 
#      "annWaterDeficit_meanAnnAvg_CLIM"       ,
#      "annWetDegDays_meanAnnAvg_CLIM"        ,  #"annVPD_mean_meanAnnAvg_CLIM"           ,#"annVPD_max_meanAnnAvg_CLIM"           ,  "annVPD_min_meanAnnAvg_CLIM"            ,#"annVPD_max_95percentile_CLIM"         ,  
#      "annWaterDeficit_95percentile_CLIM"     ,
#      "annWetDegDays_5percentile_CLIM"       ,  
#      "durationFrostFreeDays_5percentile_CLIM",
#      "durationFrostFreeDays_meanAnnAvg_CLIM",  
#      "soilDepth"                             ,
#      "surfaceClay_perc"                     ,  
#      "avgSandPerc_acrossDepth"               ,
#      "avgCoarsePerc_acrossDepth"            ,  
#      "avgOrganicCarbonPerc_0_3cm"            ,
#      "totalAvailableWaterHoldingCapacity" 
#    ) %>% 
#    rename(
#      "prcp" = prcp_meanAnnTotal_CLIM,
#      "prcp \n TempCorr" = PrecipTempCorr_meanAnnAvg_CLIM,  "isothermality" = isothermality_meanAnnAvg_CLIM,
#      "Wet \n DegDays" = annWetDegDays_meanAnnAvg_CLIM) %>%
#    cor()  %>% 
#    caret::findCorrelation(cutoff = .7, verbose = TRUE, names = TRUE, exact = TRUE))
# # the findCorrelation() function says we should remove these variables: 
# #1. chose between tmean and tmin: chose tmin
# #2. chose between tmin and aboveFreezing_month_meanAnnAvg_CLIM: chose aboveFreezing_month_meanAnnAvg_CLIM
# #3. chose between aboveFreezing_month_meanAnnAvg_CLIM and durationFrostFreeDays_meanAnnAvg_CLIM: chose durationFrostFreeDays_meanAnnAvg_CLIM
# #4. chose between durationFrostFreeDays_meanAnnAvg_CLIM and durationFrostFreeDays_5percentile_CLIM: chose durationFrostFreeDays_5percentile_CLIM
# #5. chose between durationFrostFreeDays_5percentile_CLIM and tmax_meanAnnAvg_CLIM: chose tmax_meanAnnAvg_CLIM
# #6. chose between tmax_meanAnnAvg_CLIM and T_coldestMonth_meanAnnAvg_CLIM: chose T_coldestMonth_meanAnnAvg_CLIM
# #7. chose between annWetDegDays_meanAnnAvg_CLIM and annWetDegDays_5percentile_CLIM: chose annWetDegDays_5percentile_CLIM
# #8. chose between annWaterDeficit_95percentile_CLIM and annWaterDeficit_meanAnnAvg_CLIM: chose annWaterDeficit_meanAnnAvg_CLIM
# #9. chose between annWaterDeficit_meanAnnAvg_CLIM and precip_Seasonality_meanAnnAvg_CLIM: chose precip_Seasonality_meanAnnAvg_CLIM
# #10. chose between annWetDegDays_5percentile_CLIM and prcp_meanAnnTotal_CLIM: chose prcp_meanAnnTotal_CLIM
# #11. chose between prcp_meanAnnTotal_CLIM and precip_wettestMonth_meanAnnAvg_CLIM: chose precip_wettestMonth_meanAnnAvg_CLIM
# #12. chose between totalAvailableWaterHoldingCapacity and soilDepth: chose soilDepth
# #13. chose between "avgSandPerc_acrossDepth" and "surfaceClay_perc": chose avgSandPerc_acrossDepth       
# 
# #"tmean_meanAnnAvg_CLIM"                  "tmin_meanAnnAvg_CLIM"                  
# #"aboveFreezing_month_meanAnnAvg_CLIM"    "durationFrostFreeDays_meanAnnAvg_CLIM" 
# #"durationFrostFreeDays_5percentile_CLIM" "tmax_meanAnnAvg_CLIM"                  
# #"Wet \n DegDays"                         "annWaterDeficit_95percentile_CLIM"     
# #"annWaterDeficit_meanAnnAvg_CLIM"        "annWetDegDays_5percentile_CLIM"        
# # "prcp"                                   "totalAvailableWaterHoldingCapacity"    
# # "surfaceClay_perc"       
# 
# ## also remove wet degree days, since it's correlated w/ 
# (corrPlot2 <- 
#     modDat_fit %>% 
#     dplyr::select(#"tmin_meanAnnAvg_CLIM"              ,
#       #"tmax_meanAnnAvg_CLIM" ,                 
#       #"tmean_meanAnnAvg_CLIM"                  ,
#       #"prcp_meanAnnTotal_CLIM"                 ,
#       "T_warmestMonth_meanAnnAvg_CLIM"       , 
#       "T_coldestMonth_meanAnnAvg_CLIM"         ,"precip_wettestMonth_meanAnnAvg_CLIM"    ,
#       #"precip_driestMonth_meanAnnAvg_CLIM"   , 
#       "precip_Seasonality_meanAnnAvg_CLIM"     ,
#       "PrecipTempCorr_meanAnnAvg_CLIM"        , #"aboveFreezing_month_meanAnnAvg_CLIM" ,  
#       "isothermality_meanAnnAvg_CLIM"          ,#"annWaterDeficit_meanAnnAvg_CLIM"       , #"annWetDegDays_meanAnnAvg_CLIM"       ,  
#       # "annVPD_mean_meanAnnAvg_CLIM"            ,"annVPD_max_meanAnnAvg_CLIM"            , "annVPD_min_meanAnnAvg_CLIM"          ,  
#       # "annVPD_max_95percentile_CLIM"           ,
#       # "annWaterDeficit_95percentile_CLIM"     , 
#       # "annWetDegDays_5percentile_CLIM"      ,  
#       # "durationFrostFreeDays_5percentile_CLIM" ,#"durationFrostFreeDays_meanAnnAvg_CLIM" , 
#       "soilDepth"                           ,  
#       # "surfaceClay_perc"                       ,
#       "avgSandPerc_acrossDepth"               , "avgCoarsePerc_acrossDepth"           ,  
#       "avgOrganicCarbonPerc_0_3cm"            #, "totalAvailableWaterHoldingCapacity"
#     ) %>% 
#     rename(#"MAP" =  prcp_meanAnnTotal_CLIM,
#       "T_warmest \nmonth" = T_warmestMonth_meanAnnAvg_CLIM, "T_coldest \nmonth" = T_coldestMonth_meanAnnAvg_CLIM,
#       "precip_wettest" = precip_wettestMonth_meanAnnAvg_CLIM, #"precip_driest" = precip_driestMonth_meanAnnAvg_CLIM,
#       "P/T corr" = PrecipTempCorr_meanAnnAvg_CLIM, "isothermality" = isothermality_meanAnnAvg_CLIM,
#       #"watDef" = annWaterDeficit_meanAnnAvg_CLIM,
#       #"Wet \n DegDays" = annWetDegDays_meanAnnAvg_CLIM,
#       #"VPD_max" = annVPD_max_meanAnnAvg_CLIM,
#       #"VPD_min" = annVPD_min_meanAnnAvg_CLIM,
#       #"VPD_max_95" = annVPD_max_95percentile_CLIM, 
#       #"watDef_95" = annWaterDeficit_95percentile_CLIM,
#       #"wetDegDays_5" = annWetDegDays_5percentile_CLIM,
#       #"surfClay" = surfaceClay_perc,
#       "sand" = avgSandPerc_acrossDepth,
#       "coarse" = avgCoarsePerc_acrossDepth,
#       "carbon" = avgOrganicCarbonPerc_0_3cm #, "waterCap." = totalAvailableWaterHoldingCapacity
#     ) %>%
#     #cor() %>% 
#      #findCorrelation(cutoff = .7, verbose = TRUE, names = TRUE, exact = TRUE) 
#     slice_sample(n = 5e4) %>% 
#     ggpairs(upper = list(continuous = my_fn), lower = list(continuous = GGally::wrap("points", alpha = 0.1, size=0.2)), progress = FALSE))
# 
# # w/out VPD and precip driest month replaced w/ Precip Seasonality: Fit a simple regression w/ two ecoregions ----------------
# 
# modDat_fitNew <- modDat_fit %>% 
#   mutate(newRegion = str_replace(newRegion, "eastForest", "forest"),
#          newRegion = str_replace(newRegion, "westForest", "forest")) %>% 
#   mutate(newRegionFact = as.factor(newRegion)) 
# 
# modDat_testNew <- modDat_test %>% 
#   mutate(newRegion = str_replace(newRegion, "eastForest", "forest"),
#          newRegion = str_replace(newRegion, "westForest", "forest")) %>% 
#   mutate(newRegionFact = as.factor(newRegion))
# 
# testMod_4 <- glm(newRegionFact ~ #
#                    T_warmestMonth_meanAnnAvg_CLIM    + 
#                  T_coldestMonth_meanAnnAvg_CLIM      + precip_wettestMonth_meanAnnAvg_CLIM    +
#                  precip_Seasonality_meanAnnAvg_CLIM  +
#                  PrecipTempCorr_meanAnnAvg_CLIM   +  
#                  isothermality_meanAnnAvg_CLIM   +
#                  soilDepth                  +  
#                  avgSandPerc_acrossDepth      + avgCoarsePerc_acrossDepth    +  
#                  avgOrganicCarbonPerc_0_3cm   
#                    #
#                    ,
#                  data = modDat_fitNew, 
#                  family = "binomial", 
#                  na.action = na.fail
# )
# 
# summary(testMod_4)
# 
# head(pp <- fitted(testMod_4))
# 
# AIC(testMod_4) #AIC: 241225.9
# # tetestMod_4# test the predictions of the model
# predictionDat <- modDat_testNew
# predictionDatTemp <- cbind(predictionDat,"newRegion_pred" = predict(testMod_4, newdata = predictionDat, "response"))
# 
# # for each observation, determine which category has the highest predicted probability
# (temp <- predictionDatTemp %>%
#     rowwise() %>% 
#     mutate(newRegion_predClass = newRegion_pred) %>% 
#     mutate(newRegion_predClass = replace(newRegion_predClass, newRegion_pred<.5, "dryShrubGrass"), 
#            newRegion_predClass = replace(newRegion_predClass, newRegion_pred>.5, "forest")) 
# )
# 
# temp$newRegion_predClass <- relevel(as.factor(temp$newRegion_predClass), ref = "dryShrubGrass")
# 
# # are these predictions the same as the actual classifications
# temp$goodPred <- temp$newRegionFact == temp$newRegion_predClass
# sum(!temp$goodPred)/nrow(temp) * 100
# 
# 
# ## make predictions rasterized 
# 
# temp2 <- temp %>% 
#   terra::vect(geom = c("Long", "Lat"), crs = crs(soilRastTemp)
#   ) %>% 
#   terra::rasterize(y = 
#                      terra::aggregate(soilRastTemp, fact = 2, fun = mean, na.rm= TRUE)
#                    , field = "newRegion_pred")
# 
# (regMod_2_PredMAP <- ggplot(temp) +
#     geom_spatraster(data = temp2) +
#     # geom_point(aes(#Long, Lat, 
#     #   col = newRegion_pred)) + 
#     scale_fill_distiller(type = "div", palette = 1, direction = 1) +
#     #geom_sf(data = ecoregionsSF2, color = "black", fill = NA) + 
#     #lims(x = c(-130, -65), y = c(24, 50))
#     
#     
#     #geom_point(aes(Long, Lat, col = newRegion_pred), alpha = .1) +
#     ggtitle("model-predicted ecoregion classification -- 2 ecoregions; no VPD",
#             subtitle = paste0("ecoregion ~ temp_warmestMonth + temp_coldestMonth + precip_wettestMonth +
#             precip_driestMonth + precipTempCorr + isothermality + soilDepth + % sand + % coarse + soil carbon
#            \n black dots indicate misclassification; \n  ", 
#                               round((100-sum(!temp$goodPred)/nrow(temp) * 100),1), 
#                               "% classification accuracy" )) +
#     geom_point(data = temp[temp$goodPred == FALSE,], aes(Long, Lat#, fill = as.factor(newRegion_predClass)
#     ),
#     col = "black"
#     , shape = 21) #+ 
#   #scale_fill_discrete(type = c("#bf812d","#35978f"))
# )
# 
# ## zoom in on the great basin to check out mis-classification
# (regMod_2_PredMAP_GB <- ggplot(temp) +
#     geom_spatraster(data = temp2) +
#     # geom_point(aes(#Long, Lat, 
#     #   col = newRegion_pred)) + 
#     scale_fill_distiller(type = "div", palette = 1, direction = 1) +
#     #geom_sf(data = ecoregionsSF2, color = "black", fill = NA) + 
#     #lims(x = c(-130, -65), y = c(24, 50))
#     
#     
#     #geom_point(aes(Long, Lat, col = newRegion_pred), alpha = .1) +
#     ggtitle("model-predicted ecoregion classification -- 2 ecoregions; no VPD",
#             subtitle = paste0("Zoom-In on Great Basin Area
#            \n dots indicate misclassification; color indicates true classification; \n  ", 
#                               round((100-sum(!temp$goodPred)/nrow(temp) * 100),1), 
#                               "% classification accuracy" )) +
#     geom_point(data = temp[temp$goodPred == FALSE,], aes(Long, Lat#, fill = as.factor(newRegion_predClass)
#     ),
#     col = "black"
#     , shape = 21) +
#     xlim(c(-1911149,-999000)) + 
#     ylim(c(-600105, 0))
#   #scale_fill_discrete(type = c("#bf812d","#35978f"))
# )
# 
# ## zoom in on the AZ high elevation area to check out mis-classification
# (regMod_2_PredMAP_PIPO <- ggplot(temp) +
#     geom_spatraster(data = temp2) +
#     # geom_point(aes(#Long, Lat, 
#     #   col = newRegion_pred)) + 
#     scale_fill_distiller(type = "div", palette = 1, direction = 1) +
#     #geom_sf(data = ecoregionsSF2, color = "black", fill = NA) + 
#     #lims(x = c(-130, -65), y = c(24, 50))
#     
#     
#     #geom_point(aes(Long, Lat, col = newRegion_pred), alpha = .1) +
#     ggtitle("model-predicted ecoregion classification -- 2 ecoregions; no VPD",
#             subtitle = paste0("Zoom-In on high elevation area in the SW;
#            \n dots indicate misclassification; color indicates true classification \n  ", 
#                               round((100-sum(!temp$goodPred)/nrow(temp) * 100),1), 
#                               "% classification accuracy" )) +
#     geom_point(data = temp[temp$goodPred == FALSE,], aes(Long, Lat#, fill = as.factor(newRegion_predClass)
#     ),
#     col = "black"
#     , shape = 21) +
#     xlim(c(-1211149,-559000)) + 
#     ylim(c(-1050105, -550000))
#   #scale_fill_discrete(type = c("#bf812d","#35978f"))
# )
# 
# # ## Look at the relationship between misclassification and elevation
# # # get elevation information for each raster cell 
# # temp_TEMP <- temp %>% 
# #   st_as_sf(coords = c("Long", "Lat"), crs = crs(soilRastTemp))
# # 
# # temp_elev <- elevatr::get_elev_point(temp2, src = "aws")
# # 
# # temp_TEMP <- temp_TEMP %>% st_join(temp_elev, left = TRUE, join = st_nearest_feature)
# 
# ## confusion matrix
# regMod_2_confMat <- caret::confusionMatrix(data = temp$newRegion_predClass,
#                                            reference = temp$newRegionFact)$table
# 
# ## make plots of good and bad probability
# # just good predictions
# (regMod_2_ternGoodPreds <- temp %>% 
#     filter(goodPred == TRUE) %>% 
#     ggplot() +
#     geom_density(aes(x = newRegion_pred, col = newRegionFact), trim = "FALSE") +
#     theme_minimal() + 
#     labs(color = "true ecoregion") +
#     ggtitle("Accurate Predictions") +
#     xlab("Model-Predicted Ecoregion")+
#     scale_x_continuous(breaks = c(0, .25, .5, .75, 1), 
#                        labels = c("shrub/grass", 0.25, 0.50, 0.75, "forest")) + 
#     scale_color_discrete(type = c("#bf812d","#35978f"))
# )
# # just bad predictions
# badDat <- temp %>% 
#   filter(goodPred == FALSE)
# (regMod_2_ternBadPreds <- 
#     ggplot(badDat) +
#     geom_density(aes(x = newRegion_pred, col = newRegionFact
#     ), trim = "FALSE") +
#     theme_minimal() + 
#     labs(color = "true ecoregion") +
#     ggtitle("Inaccurate Predictions")+
#     xlab("Model-Predicted Ecoregion") +
#     scale_x_continuous(breaks = c(0, .25, .5, .75, 1), 
#                        labels = c("shrub/grass", 0.25, 0.50, 0.75, "forest")) + 
#     scale_color_discrete(type = c("#bf812d","#35978f")))
# 
# (regMod_2Pred_mapBadGrass <- ggplot() +
#     geom_point(data = temp, aes(Long, Lat, col = newRegion), alpha = .3) +
#     ggtitle("Severity of Grass/Shrub misclassification") +
#     geom_point(data = temp[temp$goodPred == FALSE & temp$newRegion_predClass == "dryShrubGrass",], 
#                aes(Long, Lat, fill = newRegion_pred),
#                col = "black" ,
#                shape = 21) +
#     scale_fill_gradient(high = "#f5f5f5", low = "#543005") +
#     scale_color_discrete(type = c("#bf812d","#35978f")) +
#     theme_minimal()
# )
# 
# (regMod_2Pred_mapForest <- ggplot() +
#     geom_point(data = temp, aes(Long, Lat, col = newRegion), alpha = .1) +
#     ggtitle("Severity of Forest misclassification") +
#     geom_point(data = temp[temp$goodPred == FALSE & temp$newRegion_predClass == "forest",], 
#                aes(Long, Lat, fill = newRegion_pred),
#                col = "black" ,
#                shape = 21) +
#     scale_fill_gradient(high = "#003c30", low = "#f5f5f5") +
#     scale_color_discrete(type = c("#bf812d","#35978f")) +
#     theme_minimal()
# ) 
# 
# 
# ## make variable importance figures
# #caret::getModelInfo()
# varImp(testMod_4)
# beta.table_2 <- data.frame(summary(testMod_4)$coef)
# beta.table_2$variable <- row.names(beta.table_2)
# beta.table_2 <- mutate(beta.table_2, estimate = Estimate, 
#                        se = Std..Error, 
#                        low = Estimate - 2*se, 
#                        high = Estimate + 2*se,
#                        p = Pr...z..)  
# 
# effSizeFig_2 <- ggplot(beta.table_2, aes(y=estimate, x=variable, ymin=low, ymax=high, color = (p < 0.05))) + 
#   #guides(color = guide_legend(NULL))+
#   geom_pointrange() +  
#   geom_hline(yintercept = 0, col="darkgrey", lty = 3, lwd=2) +
#   coord_flip() + theme_minimal() + ggtitle("Effect Sizes")
# 
# ## make decile plots 
# # reorganize data frame
# # get decile values
# tempDec <- temp %>% 
#   pivot_longer(cols = c(T_warmestMonth_meanAnnAvg_CLIM, T_coldestMonth_meanAnnAvg_CLIM , precip_wettestMonth_meanAnnAvg_CLIM , 
#                         precip_Seasonality_meanAnnAvg_CLIM , PrecipTempCorr_meanAnnAvg_CLIM , 
#                         isothermality_meanAnnAvg_CLIM , soilDepth , avgSandPerc_acrossDepth , 
#                         avgCoarsePerc_acrossDepth , avgOrganicCarbonPerc_0_3cm ), 
#                names_to = "predictor_name",
#                values_to = "predictor_value"
#   ) %>% 
#   dplyr::select(newRegion_pred, predictor_name, predictor_value)
# tempQuantiles <- tempDec %>% 
#   group_by(predictor_name) %>% 
#   reframe(quantiles = stats::quantile(predictor_value, c(0, .09,.1,.19, .2, .29, .3, .39, .4, .49, 
#                                                          .5, .59, .6, .69, .7, .79, .8, .89, .9, 1)), 
#           quantilesNames = paste0("quant_",c(0, .09,.1,.19, .2, .29, .3, .39, .4, .49, 
#                                              .5, .59, .6, .69, .7, .79, .8, .89, .9, 1))) %>% 
#   pivot_wider( names_from = quantilesNames, 
#                values_from = quantiles)
# 
# test <- tempDec %>% 
#   left_join(tempQuantiles) %>% 
#   mutate(#YesQuant0 = dplyr::between(predictor_value, left = quant_0, right = quant_0.09),
#     YesQuant1 = dplyr::between(predictor_value, left = quant_0.1 , right = quant_0.19),
#     YesQuant2 = dplyr::between(predictor_value, left = quant_0.2 , right = quant_0.29),
#     YesQuant3 = dplyr::between(predictor_value, left = quant_0.3 , right = quant_0.39),
#     YesQuant4 = dplyr::between(predictor_value, left = quant_0.4 , right = quant_0.49),
#     YesQuant5 = dplyr::between(predictor_value, left = quant_0.5 , right = quant_0.59),
#     YesQuant6 = dplyr::between(predictor_value, left = quant_0.6 , right = quant_0.69),
#     YesQuant7 = dplyr::between(predictor_value, left = quant_0.7 , right = quant_0.79),
#     YesQuant8 = dplyr::between(predictor_value, left = quant_0.8 , right = quant_0.89),
#     #YesQuant9 = dplyr::between(predictor_value, left = quant_0.9 , right = quant_1)
#   ) %>% 
#   mutate(#YesQuant0 = case_match(YesQuant0, TRUE ~ quant_0.09),
#     YesQuant1 = case_match(YesQuant1, TRUE ~ quant_0.19),
#     YesQuant2 = case_match(YesQuant2, TRUE ~ quant_0.29),
#     YesQuant3 = case_match(YesQuant3, TRUE ~ quant_0.39),
#     YesQuant4 = case_match(YesQuant4, TRUE ~ quant_0.49),
#     YesQuant5 = case_match(YesQuant5, TRUE ~ quant_0.59),
#     YesQuant6 = case_match(YesQuant6, TRUE ~ quant_0.69),
#     YesQuant7 = case_match(YesQuant7, TRUE ~ quant_0.79),
#     YesQuant8 = case_match(YesQuant8, TRUE ~ quant_0.89)
#     #,YesQuant9 = case_match(YesQuant9, TRUE ~ quant_1)
#   ) %>% 
#   #slice(1:1000) %>% 
#   rowwise() %>% 
#   mutate(Quantile_value = round(sum(na.omit(YesQuant1), na.omit(YesQuant2), na.omit(YesQuant3),
#                                     na.omit(YesQuant4), na.omit(YesQuant5), na.omit(YesQuant6), na.omit(YesQuant7),
#                                     na.omit(YesQuant8)#, na.omit(YesQuant0)
#   ), 
#   5)) %>% 
#   dplyr::select(-(quant_1:YesQuant8)) %>% 
#   group_by(predictor_name, Quantile_value) %>% 
#   summarize(newRegion_pred_mean = mean(newRegion_pred)) 
# 
# # make a plot 
# (quantPlot <- ggplot(data = test) + 
#     facet_wrap(~predictor_name, scales = "free_x") + 
#     #geom_density(aes(Quantile_value, col = predictor_name)) + 
#     geom_line(aes(x = Quantile_value, y = newRegion_pred_mean, col = predictor_name)) +
#     geom_point(aes(x = Quantile_value, y = newRegion_pred_mean, col = predictor_name)) + 
#     theme_minimal() + 
#     labs(y = "mean predicted probability w/in a decile", x = "predictor value") + 
#     scale_color_discrete(guide = "none"))
# 
# ## arrange all of the figures for this model
# ggpubr::annotate_figure(ggarrange(
#   # regMod_2_varImpPlot,
#   ggplotify::as.grob(gt::as_gtable(gt::gt(data.frame("dryShrubGrass" =  data.frame(regMod_2_confMat)$Freq[1:2], 
#                                                      "Forest" = data.frame(regMod_2_confMat)$Freq[3:4],
#                                                      row.names = c("dryShrubGrass", "Forest")), 
#                                           rownames_to_stub = TRUE))), 
#   effSizeFig_2,
#   quantPlot,
#   regMod_2_PredMAP, 
#   regMod_2_PredMAP_GB, 
#   regMod_2_PredMAP_PIPO,
#   #badPredElevation_mapA,
#   regMod_2_ternGoodPreds,
#   regMod_2_ternBadPreds,
#   regMod_2Pred_mapBadGrass, 
#   regMod_2Pred_mapForest, 
#   nrow = 6
# )#, 
# #top = c("Model with depth = 7 and all climate and soil predictors")
# ) %>% 
#   ggexport(
#     filename = "./Figures/EcoRegionModelFigures/Model_REPLACESPrecipDriestMonth_withPrecipSeasonality/ContemporaryModelResults.pdf",
#     width = 15, height = 45)
# 
# 
# 
# # w/out VPD and precip driest month replaced with ann water deficit: drop correlated variables ------------------------------------
# 
# (corrPlot <- 
#    modDat_fit %>% 
#    dplyr::select(
#      "tmin_meanAnnAvg_CLIM"                  ,
#      "tmax_meanAnnAvg_CLIM"                  , 
#      "tmean_meanAnnAvg_CLIM"                 ,
#      "prcp_meanAnnTotal_CLIM"                , 
#      "T_warmestMonth_meanAnnAvg_CLIM"       , 
#      "T_coldestMonth_meanAnnAvg_CLIM"        , 
#      "precip_wettestMonth_meanAnnAvg_CLIM"  ,  #"precip_driestMonth_meanAnnAvg_CLIM"    , 
#      #"precip_Seasonality_meanAnnAvg_CLIM"   , 
#      "PrecipTempCorr_meanAnnAvg_CLIM"       ,  
#      "aboveFreezing_month_meanAnnAvg_CLIM"   ,
#      "isothermality_meanAnnAvg_CLIM"        , 
#      "annWaterDeficit_meanAnnAvg_CLIM"       ,
#      "annWetDegDays_meanAnnAvg_CLIM"        ,  #"annVPD_mean_meanAnnAvg_CLIM"           ,#"annVPD_max_meanAnnAvg_CLIM"           ,  "annVPD_min_meanAnnAvg_CLIM"            ,#"annVPD_max_95percentile_CLIM"         ,  
#      "annWaterDeficit_95percentile_CLIM"     ,
#      "annWetDegDays_5percentile_CLIM"       ,  
#      "durationFrostFreeDays_5percentile_CLIM",
#      "durationFrostFreeDays_meanAnnAvg_CLIM",  
#      "soilDepth"                             ,
#      "surfaceClay_perc"                     ,  
#      "avgSandPerc_acrossDepth"               ,
#      "avgCoarsePerc_acrossDepth"            ,  
#      "avgOrganicCarbonPerc_0_3cm"            ,
#      "totalAvailableWaterHoldingCapacity" 
#    ) %>% 
#    rename(
#      "prcp" = prcp_meanAnnTotal_CLIM,
#      "prcp \n TempCorr" = PrecipTempCorr_meanAnnAvg_CLIM,  "isothermality" = isothermality_meanAnnAvg_CLIM,
#      "Wet \n DegDays" = annWetDegDays_meanAnnAvg_CLIM) %>%
#    cor()  %>% 
#    caret::findCorrelation(cutoff = .7, verbose = TRUE, names = TRUE, exact = TRUE))
# # the findCorrelation() function says we should remove these variables: 
# #1. chose between tmin_meanAnnAvg_CLIM and tmean_meanAnnAvg_CLIM: chose tmean_meanAnnAvg_CLIM
# #2. chose between tmean_meanAnnAvg_CLIM and durationFrostFreeDays_meanAnnAvg_CLIM: chose durationFrostFreeDays_meanAnnAvg_CLIM
# #3. chose between durationFrostFreeDays_meanAnnAvg_CLIM and aboveFreezing_month_meanAnnAvg_CLIM: chose aboveFreezing_month_meanAnnAvg_CLIM
# #4. chose between aboveFreezing_month_meanAnnAvg_CLIM and durationFrostFreeDays_5percentile_CLIM: chose durationFrostFreeDays_5percentile_CLIM
# #5. chose between durationFrostFreeDays_5percentile_CLIM and tmax_meanAnnAvg_CLIM: chose tmax_meanAnnAvg_CLIM
# #6. chose between tmax_meanAnnAvg_CLIM and T_coldestMonth_meanAnnAvg_CLIM: chose T_coldestMonth_meanAnnAvg_CLIM
# #7. chose between isothermality and annWetDegDays_5percentile_CLIM: chose annWetDegDays_5percentile_CLIM
# #8. chose between annWaterDeficit_95percentile_CLIM and annWaterDeficit_meanAnnAvg_CLIM: chose annWaterDeficit_meanAnnAvg_CLIM
# #9. chose between annWetDegDays_5percentile_CLIM and prcp: chose prcp
# #10. chose between prcp and precip_wettestMonth_meanAnnAvg_CLIM: chose precip_wettestMonth_meanAnnAvg_CLIM
# #11. chose between totalAvailableWaterHoldingCapacity and soil Depth: chose soil Depth
# #12. chose between avgSandPerc_acrossDepth and surfaceClay_perc: chose avgSandPerc_acrossDepth
#  ## drop  [1] "tmin_meanAnnAvg_CLIM"                   "tmean_meanAnnAvg_CLIM"                 
# # [3] "durationFrostFreeDays_meanAnnAvg_CLIM"  "aboveFreezing_month_meanAnnAvg_CLIM"   
# # [5] "durationFrostFreeDays_5percentile_CLIM" "tmax_meanAnnAvg_CLIM"                  
# # [7] "Wet \n DegDays"                         "annWaterDeficit_95percentile_CLIM"     
# # [9] "annWetDegDays_5percentile_CLIM"         "prcp"                                  
# # [11] "totalAvailableWaterHoldingCapacity"     "surfaceClay_perc"      
# 
# ## also remove wet degree days, since it's correlated w/ 
# (corrPlot2 <- 
#     modDat_fit %>% 
#     dplyr::select(#"tmin_meanAnnAvg_CLIM"              ,
#       #"tmax_meanAnnAvg_CLIM" ,                 
#       #"tmean_meanAnnAvg_CLIM"                  ,
#       #"prcp_meanAnnTotal_CLIM"                 ,
#       "T_warmestMonth_meanAnnAvg_CLIM"       , 
#       "T_coldestMonth_meanAnnAvg_CLIM"         ,"precip_wettestMonth_meanAnnAvg_CLIM"    ,
#       #"precip_driestMonth_meanAnnAvg_CLIM"   , 
#       #"precip_Seasonality_meanAnnAvg_CLIM"     ,
#       "PrecipTempCorr_meanAnnAvg_CLIM"        , #"aboveFreezing_month_meanAnnAvg_CLIM" ,  
#       "isothermality_meanAnnAvg_CLIM"          ,"annWaterDeficit_meanAnnAvg_CLIM"       , 
#       #"annWetDegDays_meanAnnAvg_CLIM"       ,  
#        #"annVPD_mean_meanAnnAvg_CLIM"            ,"annVPD_max_meanAnnAvg_CLIM"            , "annVPD_min_meanAnnAvg_CLIM"          ,  
#        #"annVPD_max_95percentile_CLIM"           ,
#        #"annWaterDeficit_95percentile_CLIM"     , 
#        #"annWetDegDays_5percentile_CLIM"      ,  
#        #"durationFrostFreeDays_5percentile_CLIM" ,#"durationFrostFreeDays_meanAnnAvg_CLIM" , 
#       "soilDepth"                           ,  
#        #"surfaceClay_perc"                       ,
#       "avgSandPerc_acrossDepth"               , "avgCoarsePerc_acrossDepth"           ,  
#       "avgOrganicCarbonPerc_0_3cm"            #, "totalAvailableWaterHoldingCapacity"
#     ) %>% 
#     rename(#"MAP" =  prcp_meanAnnTotal_CLIM,
#       "T_warmest \nmonth" = T_warmestMonth_meanAnnAvg_CLIM, "T_coldest \nmonth" = T_coldestMonth_meanAnnAvg_CLIM,
#       "precip_wettest" = precip_wettestMonth_meanAnnAvg_CLIM, #"precip_driest" = precip_driestMonth_meanAnnAvg_CLIM,
#       "P/T corr" = PrecipTempCorr_meanAnnAvg_CLIM, "isothermality" = isothermality_meanAnnAvg_CLIM,
#       #"watDef" = annWaterDeficit_meanAnnAvg_CLIM,
#       #"Wet \n DegDays" = annWetDegDays_meanAnnAvg_CLIM,
#       #"VPD_max" = annVPD_max_meanAnnAvg_CLIM,
#       #"VPD_min" = annVPD_min_meanAnnAvg_CLIM,
#       #"VPD_max_95" = annVPD_max_95percentile_CLIM, 
#       #"watDef_95" = annWaterDeficit_95percentile_CLIM,
#       #"wetDegDays_5" = annWetDegDays_5percentile_CLIM,
#       #"surfClay" = surfaceClay_perc,
#       "sand" = avgSandPerc_acrossDepth,
#       "coarse" = avgCoarsePerc_acrossDepth,
#       "carbon" = avgOrganicCarbonPerc_0_3cm #, "waterCap." = totalAvailableWaterHoldingCapacity
#     ) %>%
#     #cor() %>% 
#     #findCorrelation(cutoff = .7, verbose = TRUE, names = TRUE, exact = TRUE) 
#     slice_sample(n = 5e4) %>% 
#     ggpairs(upper = list(continuous = my_fn), lower = list(continuous = GGally::wrap("points", alpha = 0.1, size=0.2)), progress = FALSE))

#THIS ONE w/out VPD and precip driest month replaced with ann water deficit: Fit a simple regression w/ two ecoregions ----------------

modDat_fitNew <- modDat_fit %>% 
  mutate(newRegion = str_replace(newRegion, "eastForest", "forest"),
         newRegion = str_replace(newRegion, "westForest", "forest")) %>% 
  mutate(newRegionFact = as.factor(newRegion)) 

modDat_testNew <- modDat_test %>% 
  mutate(newRegion = str_replace(newRegion, "eastForest", "forest"),
         newRegion = str_replace(newRegion, "westForest", "forest")) %>% 
  mutate(newRegionFact = as.factor(newRegion))

testMod_5 <- glm(newRegionFact ~ #
                   T_warmestMonth_meanAnnAvg_CLIM    + 
                   T_coldestMonth_meanAnnAvg_CLIM      + precip_wettestMonth_meanAnnAvg_CLIM    +
                   annWaterDeficit_meanAnnAvg_CLIM  +
                   PrecipTempCorr_meanAnnAvg_CLIM   +  
                   isothermality_meanAnnAvg_CLIM   +
                   soilDepth                  +  
                   avgSandPerc_acrossDepth      + avgCoarsePerc_acrossDepth    +  
                   avgOrganicCarbonPerc_0_3cm   
                 #
                 ,
                 data = modDat_fitNew, 
                 family = "binomial", 
                 na.action = na.fail
)

summary(testMod_5)

head(pp <- fitted(testMod_5))

AIC(testMod_5) #AIC: 241225.9
# tetestMod_5# test the predictions of the model
predictionDat <- modDat_testNew
predictionDatTemp <- cbind(predictionDat,"newRegion_pred" = predict(testMod_5, newdata = predictionDat, "response"))

# for each observation, determine which category has the highest predicted probability
(temp <- predictionDatTemp %>%
    rowwise() %>% 
    mutate(newRegion_predClass = newRegion_pred) %>% 
    mutate(newRegion_predClass = replace(newRegion_predClass, newRegion_pred<.5, "dryShrubGrass"), 
           newRegion_predClass = replace(newRegion_predClass, newRegion_pred>.5, "forest")) 
)

temp$newRegion_predClass <- relevel(as.factor(temp$newRegion_predClass), ref = "dryShrubGrass")

# are these predictions the same as the actual classifications
temp$goodPred <- temp$newRegionFact == temp$newRegion_predClass
sum(!temp$goodPred)/nrow(temp) * 100


## make predictions rasterized 

temp2 <- temp %>% 
  terra::vect(geom = c("Long", "Lat"), crs = crs(soilRastTemp)
  ) %>% 
  terra::rasterize(y = 
                     terra::aggregate(soilRastTemp, fact = 2, fun = mean, na.rm= TRUE)
                   , field = "newRegion_pred")

(regMod_2_PredMAP <- ggplot(temp) +
    geom_spatraster(data = temp2) +
    # geom_point(aes(#Long, Lat, 
    #   col = newRegion_pred)) + 
    scale_fill_distiller(type = "div", palette = 1, direction = 1) +
    #geom_sf(data = ecoregionsSF2, color = "black", fill = NA) + 
    #lims(x = c(-130, -65), y = c(24, 50))
    
    
    #geom_point(aes(Long, Lat, col = newRegion_pred), alpha = .1) +
    ggtitle("model-predicted ecoregion classification -- 2 ecoregions; no VPD",
            subtitle = paste0("ecoregion ~ temp_warmestMonth + temp_coldestMonth + precip_wettestMonth +
            annual water deficit + precipTempCorr + isothermality + soilDepth + % sand + % coarse + soil carbon
           \n black dots indicate misclassification; \n  ", 
                              round((100-sum(!temp$goodPred)/nrow(temp) * 100),1), 
                              "% classification accuracy" )) +
    geom_point(data = temp[temp$goodPred == FALSE,], aes(Long, Lat#, fill = as.factor(newRegion_predClass)
    ),
    col = "black"
    , shape = 21) #+ 
  #scale_fill_discrete(type = c("#bf812d","#35978f"))
)

## zoom in on the great basin to check out mis-classification
(regMod_2_PredMAP_GB <- ggplot(temp) +
    geom_spatraster(data = temp2) +
    # geom_point(aes(#Long, Lat, 
    #   col = newRegion_pred)) + 
    scale_fill_distiller(type = "div", palette = 1, direction = 1) +
    #geom_sf(data = ecoregionsSF2, color = "black", fill = NA) + 
    #lims(x = c(-130, -65), y = c(24, 50))
    
    
    #geom_point(aes(Long, Lat, col = newRegion_pred), alpha = .1) +
    ggtitle("model-predicted ecoregion classification -- 2 ecoregions; no VPD",
            subtitle = paste0("Zoom-In on Great Basin Area
           \n dots indicate misclassification; color indicates true classification; \n  ", 
                              round((100-sum(!temp$goodPred)/nrow(temp) * 100),1), 
                              "% classification accuracy" )) +
    geom_point(data = temp[temp$goodPred == FALSE,], aes(Long, Lat#, fill = as.factor(newRegion_predClass)
    ),
    col = "black"
    , shape = 21) +
    xlim(c(-1911149,-999000)) + 
    ylim(c(-600105, 0))
  #scale_fill_discrete(type = c("#bf812d","#35978f"))
)

## zoom in on the AZ high elevation area to check out mis-classification
(regMod_2_PredMAP_PIPO <- ggplot(temp) +
    geom_spatraster(data = temp2) +
    # geom_point(aes(#Long, Lat, 
    #   col = newRegion_pred)) + 
    scale_fill_distiller(type = "div", palette = 1, direction = 1) +
    #geom_sf(data = ecoregionsSF2, color = "black", fill = NA) + 
    #lims(x = c(-130, -65), y = c(24, 50))
    
    
    #geom_point(aes(Long, Lat, col = newRegion_pred), alpha = .1) +
    ggtitle("model-predicted ecoregion classification -- 2 ecoregions; no VPD",
            subtitle = paste0("Zoom-In on high elevation area in the SW;
           \n dots indicate misclassification; color indicates true classification \n  ", 
                              round((100-sum(!temp$goodPred)/nrow(temp) * 100),1), 
                              "% classification accuracy" )) +
    geom_point(data = temp[temp$goodPred == FALSE,], aes(Long, Lat#, fill = as.factor(newRegion_predClass)
    ),
    col = "black"
    , shape = 21) +
    xlim(c(-1211149,-559000)) + 
    ylim(c(-1050105, -550000))
  #scale_fill_discrete(type = c("#bf812d","#35978f"))
)

## Look at the relationship between misclassification and elevation
# # get elevation information for each raster cell 
# temp_TEMP <- temp %>% 
#   st_as_sf(coords = c("Long", "Lat"), crs = crs(soilRastTemp))
# 
# temp_elev <- elevatr::get_elev_point(temp2, src = "aws")
# 
# temp_TEMP <- temp_TEMP %>% st_join(temp_elev, left = TRUE, join = st_nearest_feature)
# 
## confusion matrix
regMod_2_confMat <- caret::confusionMatrix(data = temp$newRegion_predClass,
                                           reference = temp$newRegionFact)$table

## make plots of good and bad probability
# just good predictions
(regMod_2_ternGoodPreds <- temp %>% 
    filter(goodPred == TRUE) %>% 
    ggplot() +
    geom_density(aes(x = newRegion_pred, col = newRegionFact), trim = "FALSE") +
    theme_minimal() + 
    labs(color = "true ecoregion") +
    ggtitle("Accurate Predictions") +
    xlab("Model-Predicted Ecoregion")+
    scale_x_continuous(breaks = c(0, .25, .5, .75, 1), 
                       labels = c("shrub/grass", 0.25, 0.50, 0.75, "forest")) + 
    scale_color_discrete(type = c("#bf812d","#35978f"))
)
# just bad predictions
badDat <- temp %>% 
  filter(goodPred == FALSE)
(regMod_2_ternBadPreds <- 
    ggplot(badDat) +
    geom_density(aes(x = newRegion_pred, col = newRegionFact
    ), trim = "FALSE") +
    theme_minimal() + 
    labs(color = "true ecoregion") +
    ggtitle("Inaccurate Predictions")+
    xlab("Model-Predicted Ecoregion") +
    scale_x_continuous(breaks = c(0, .25, .5, .75, 1), 
                       labels = c("shrub/grass", 0.25, 0.50, 0.75, "forest")) + 
    scale_color_discrete(type = c("#bf812d","#35978f")))

(regMod_2Pred_mapBadGrass <- ggplot() +
    geom_point(data = temp, aes(Long, Lat, col = newRegion), alpha = .3) +
    ggtitle("Severity of Grass/Shrub misclassification") +
    geom_point(data = temp[temp$goodPred == FALSE & temp$newRegion_predClass == "dryShrubGrass",], 
               aes(Long, Lat, fill = newRegion_pred),
               col = "black" ,
               shape = 21) +
    scale_fill_gradient(high = "#f5f5f5", low = "#543005") +
    scale_color_discrete(type = c("#bf812d","#35978f")) +
    theme_minimal()
)

(regMod_2Pred_mapForest <- ggplot() +
    geom_point(data = temp, aes(Long, Lat, col = newRegion), alpha = .1) +
    ggtitle("Severity of Forest misclassification") +
    geom_point(data = temp[temp$goodPred == FALSE & temp$newRegion_predClass == "forest",], 
               aes(Long, Lat, fill = newRegion_pred),
               col = "black" ,
               shape = 21) +
    scale_fill_gradient(high = "#003c30", low = "#f5f5f5") +
    scale_color_discrete(type = c("#bf812d","#35978f")) +
    theme_minimal()
) 


## make variable importance figures
#caret::getModelInfo()
varImp(testMod_5)
beta.table_2 <- data.frame(summary(testMod_5)$coef)
beta.table_2$variable <- row.names(beta.table_2)
beta.table_2 <- mutate(beta.table_2, estimate = Estimate, 
                       se = Std..Error, 
                       low = Estimate - 2*se, 
                       high = Estimate + 2*se,
                       p = Pr...z..)  

effSizeFig_2 <- ggplot(beta.table_2, aes(y=estimate, x=variable, ymin=low, ymax=high, color = (p < 0.05))) + 
  #guides(color = guide_legend(NULL))+
  geom_pointrange() +  
  geom_hline(yintercept = 0, col="darkgrey", lty = 3, lwd=2) +
  coord_flip() + theme_minimal() + ggtitle("Effect Sizes")

## make decile plots 
# reorganize data frame
# get decile values
tempDec <- temp %>% 
  pivot_longer(cols = c(T_warmestMonth_meanAnnAvg_CLIM, T_coldestMonth_meanAnnAvg_CLIM , precip_wettestMonth_meanAnnAvg_CLIM , 
                        annWaterDeficit_meanAnnAvg_CLIM , PrecipTempCorr_meanAnnAvg_CLIM , 
                        isothermality_meanAnnAvg_CLIM , soilDepth , avgSandPerc_acrossDepth , 
                        avgCoarsePerc_acrossDepth , avgOrganicCarbonPerc_0_3cm ), 
               names_to = "predictor_name",
               values_to = "predictor_value"
  ) %>% 
  dplyr::select(newRegion_pred, predictor_name, predictor_value)
tempQuantiles <- tempDec %>% 
  group_by(predictor_name) %>% 
  reframe(quantiles = stats::quantile(predictor_value, c(0, .09,.1,.19, .2, .29, .3, .39, .4, .49, 
                                                         .5, .59, .6, .69, .7, .79, .8, .89, .9, 1)), 
          quantilesNames = paste0("quant_",c(0, .09,.1,.19, .2, .29, .3, .39, .4, .49, 
                                             .5, .59, .6, .69, .7, .79, .8, .89, .9, 1))) %>% 
  pivot_wider( names_from = quantilesNames, 
               values_from = quantiles)

test <- tempDec %>% 
  left_join(tempQuantiles) %>% 
  mutate(#YesQuant0 = dplyr::between(predictor_value, left = quant_0, right = quant_0.09),
    YesQuant1 = dplyr::between(predictor_value, left = quant_0.1 , right = quant_0.19),
    YesQuant2 = dplyr::between(predictor_value, left = quant_0.2 , right = quant_0.29),
    YesQuant3 = dplyr::between(predictor_value, left = quant_0.3 , right = quant_0.39),
    YesQuant4 = dplyr::between(predictor_value, left = quant_0.4 , right = quant_0.49),
    YesQuant5 = dplyr::between(predictor_value, left = quant_0.5 , right = quant_0.59),
    YesQuant6 = dplyr::between(predictor_value, left = quant_0.6 , right = quant_0.69),
    YesQuant7 = dplyr::between(predictor_value, left = quant_0.7 , right = quant_0.79),
    YesQuant8 = dplyr::between(predictor_value, left = quant_0.8 , right = quant_0.89),
    #YesQuant9 = dplyr::between(predictor_value, left = quant_0.9 , right = quant_1)
  ) %>% 
  mutate(#YesQuant0 = case_match(YesQuant0, TRUE ~ quant_0.09),
    YesQuant1 = case_match(YesQuant1, TRUE ~ quant_0.19),
    YesQuant2 = case_match(YesQuant2, TRUE ~ quant_0.29),
    YesQuant3 = case_match(YesQuant3, TRUE ~ quant_0.39),
    YesQuant4 = case_match(YesQuant4, TRUE ~ quant_0.49),
    YesQuant5 = case_match(YesQuant5, TRUE ~ quant_0.59),
    YesQuant6 = case_match(YesQuant6, TRUE ~ quant_0.69),
    YesQuant7 = case_match(YesQuant7, TRUE ~ quant_0.79),
    YesQuant8 = case_match(YesQuant8, TRUE ~ quant_0.89)
    #,YesQuant9 = case_match(YesQuant9, TRUE ~ quant_1)
  ) %>% 
  #slice(1:1000) %>% 
  rowwise() %>% 
  mutate(Quantile_value = round(sum(na.omit(YesQuant1), na.omit(YesQuant2), na.omit(YesQuant3),
                                    na.omit(YesQuant4), na.omit(YesQuant5), na.omit(YesQuant6), na.omit(YesQuant7),
                                    na.omit(YesQuant8)#, na.omit(YesQuant0)
  ), 
  5)) %>% 
  dplyr::select(-(quant_1:YesQuant8)) %>% 
  group_by(predictor_name, Quantile_value) %>% 
  summarize(newRegion_pred_mean = mean(newRegion_pred)) 

# make a plot 
(quantPlot <- ggplot(data = test) + 
    facet_wrap(~predictor_name, scales = "free_x") + 
    #geom_density(aes(Quantile_value, col = predictor_name)) + 
    geom_line(aes(x = Quantile_value, y = newRegion_pred_mean, col = predictor_name)) +
    geom_point(aes(x = Quantile_value, y = newRegion_pred_mean, col = predictor_name)) + 
    theme_minimal() + 
    labs(y = "mean predicted probability w/in a decile", x = "predictor value") + 
    scale_color_discrete(guide = "none"))

## arrange all of the figures for this model
ggpubr::annotate_figure(ggarrange(
  # regMod_2_varImpPlot,
  ggplotify::as.grob(gt::as_gtable(gt::gt(data.frame("dryShrubGrass" =  data.frame(regMod_2_confMat)$Freq[1:2], 
                                                     "Forest" = data.frame(regMod_2_confMat)$Freq[3:4],
                                                     row.names = c("dryShrubGrass", "Forest")), 
                                          rownames_to_stub = TRUE))), 
  effSizeFig_2,
  quantPlot,
  regMod_2_PredMAP, 
  regMod_2_PredMAP_GB, 
  regMod_2_PredMAP_PIPO,
  #badPredElevation_mapA,
  regMod_2_ternGoodPreds,
  regMod_2_ternBadPreds,
  regMod_2Pred_mapBadGrass, 
  regMod_2Pred_mapForest, 
  nrow = 6
)#, 
#top = c("Model with depth = 7 and all climate and soil predictors")
) %>% 
  ggexport(
    filename = "./Figures/EcoRegionModelFigures/Model_REPLACESPrecipDriestMonth_withAnnWatDef/ContemporaryModelResults.pdf",
    width = 15, height = 45)

## save model object 
saveRDS(testMod_5, file = "./Analysis/ecoregionClassification/ModelFitting/ModelObjects/EcoregionClassificationModel.rds")
# # w/out VPD and precip driest month replaced with ann water deficit: Investigate Interactions ----------------
# 
# # make null model  (use testMod_2 as the full model )
# testMod.null <- glm(formula = newRegionFact ~ 1, family = "binomial", 
#                     data = modDat_fitNew)
# testMod.full <- 
#   glm(data =  modDat_fitNew %>% 
#         dplyr::select(newRegionFact, 
#                         T_warmestMonth_meanAnnAvg_CLIM    , 
#                         T_coldestMonth_meanAnnAvg_CLIM      , precip_wettestMonth_meanAnnAvg_CLIM    ,
#                         annWaterDeficit_meanAnnAvg_CLIM  ,
#                         PrecipTempCorr_meanAnnAvg_CLIM   ,  
#                         isothermality_meanAnnAvg_CLIM   ,
#                         soilDepth                  ,  
#                         avgSandPerc_acrossDepth      , avgCoarsePerc_acrossDepth    ,  
#                         avgOrganicCarbonPerc_0_3cm )
#       , formula = newRegionFact ~ . + .^2 , family = "binomial", 
#       na.action = na.fail)
# 
# # stepwiseResults <- stepAIC(testMod.full, scope = list(lower = testMod.null, upper = ~ .^2)
# #                          , direction="both",test="Chisq", trace = T)
# 
# stepwiseResults_back <- stepAIC(testMod.full, scope = list(lower = testMod.null, upper =  testMod.full)
#                                 , direction="backward",test="Chisq", trace = T)
# 
# ## the brute-force method includes 44 interactions! which seems unreasonable... but will proceed to see what happens for now
# summary(stepwiseResults_back)
# 
# testMod_6 <- stepwiseResults_back
# 
# summary(testMod_6)
# 
# head(pp <- fitted(testMod_6))
# 
# AIC(testMod_6) #AIC: 241225.9
# # tetestMod_6# test the predictions of the model
# predictionDat <- modDat_testNew
# predictionDatTemp <- cbind(predictionDat,"newRegion_pred" = predict(testMod_6, newdata = predictionDat, "response"))
# 
# # for each observation, determine which category has the highest predicted probability
# (temp <- predictionDatTemp %>%
#     rowwise() %>% 
#     mutate(newRegion_predClass = newRegion_pred) %>% 
#     mutate(newRegion_predClass = replace(newRegion_predClass, newRegion_pred<.5, "dryShrubGrass"), 
#            newRegion_predClass = replace(newRegion_predClass, newRegion_pred>.5, "forest")) 
# )
# 
# temp$newRegion_predClass <- relevel(as.factor(temp$newRegion_predClass), ref = "dryShrubGrass")
# 
# # are these predictions the same as the actual classifications
# temp$goodPred <- temp$newRegionFact == temp$newRegion_predClass
# sum(!temp$goodPred)/nrow(temp) * 100
# 
# 
# ## make predictions rasterized 
# 
# temp2 <- temp %>% 
#   terra::vect(geom = c("Long", "Lat"), crs = crs(soilRastTemp)
#   ) %>% 
#   terra::rasterize(y = 
#                      terra::aggregate(soilRastTemp, fact = 2, fun = mean, na.rm= TRUE)
#                    , field = "newRegion_pred")
# 
# (regMod_2_PredMAP <- ggplot(temp) +
#     geom_spatraster(data = temp2) +
#     # geom_point(aes(#Long, Lat, 
#     #   col = newRegion_pred)) + 
#     scale_fill_distiller(type = "div", palette = 1, direction = 1) +
#     #geom_sf(data = ecoregionsSF2, color = "black", fill = NA) + 
#     #lims(x = c(-130, -65), y = c(24, 50))
#     
#     
#     #geom_point(aes(Long, Lat, col = newRegion_pred), alpha = .1) +
#     ggtitle("model-predicted ecoregion classification -- 2 ecoregions; no VPD",
#             subtitle = paste0("ecoregion ~ temp_warmestMonth + temp_coldestMonth + precip_wettestMonth +
#             annual water deficit + precipTempCorr + isothermality + soilDepth + % sand + % coarse + soil carbon + 
#             lots of interactions
#           black dots indicate misclassification; \n  ", 
#                               round((100-sum(!temp$goodPred)/nrow(temp) * 100),1), 
#                               "% classification accuracy" )) +
#     geom_point(data = temp[temp$goodPred == FALSE,], aes(Long, Lat#, fill = as.factor(newRegion_predClass)
#     ),
#     col = "black"
#     , shape = 21) #+ 
#   #scale_fill_discrete(type = c("#bf812d","#35978f"))
# )
# 
# ## zoom in on the great basin to check out mis-classification
# (regMod_2_PredMAP_GB <- ggplot(temp) +
#     geom_spatraster(data = temp2) +
#     # geom_point(aes(#Long, Lat, 
#     #   col = newRegion_pred)) + 
#     scale_fill_distiller(type = "div", palette = 1, direction = 1) +
#     #geom_sf(data = ecoregionsSF2, color = "black", fill = NA) + 
#     #lims(x = c(-130, -65), y = c(24, 50))
#     
#     
#     #geom_point(aes(Long, Lat, col = newRegion_pred), alpha = .1) +
#     ggtitle("model-predicted ecoregion classification -- 2 ecoregions; no VPD",
#             subtitle = paste0("Zoom-In on Great Basin Area
#            \n dots indicate misclassification; color indicates true classification; \n  ", 
#                               round((100-sum(!temp$goodPred)/nrow(temp) * 100),1), 
#                               "% classification accuracy" )) +
#     geom_point(data = temp[temp$goodPred == FALSE,], aes(Long, Lat#, fill = as.factor(newRegion_predClass)
#     ),
#     col = "black"
#     , shape = 21) +
#     xlim(c(-1911149,-999000)) + 
#     ylim(c(-600105, 0))
#   #scale_fill_discrete(type = c("#bf812d","#35978f"))
# )
# 
# ## zoom in on the AZ high elevation area to check out mis-classification
# (regMod_2_PredMAP_PIPO <- ggplot(temp) +
#     geom_spatraster(data = temp2) +
#     # geom_point(aes(#Long, Lat, 
#     #   col = newRegion_pred)) + 
#     scale_fill_distiller(type = "div", palette = 1, direction = 1) +
#     #geom_sf(data = ecoregionsSF2, color = "black", fill = NA) + 
#     #lims(x = c(-130, -65), y = c(24, 50))
#     
#     
#     #geom_point(aes(Long, Lat, col = newRegion_pred), alpha = .1) +
#     ggtitle("model-predicted ecoregion classification -- 2 ecoregions; no VPD",
#             subtitle = paste0("Zoom-In on high elevation area in the SW;
#            \n dots indicate misclassification; color indicates true classification \n  ", 
#                               round((100-sum(!temp$goodPred)/nrow(temp) * 100),1), 
#                               "% classification accuracy" )) +
#     geom_point(data = temp[temp$goodPred == FALSE,], aes(Long, Lat#, fill = as.factor(newRegion_predClass)
#     ),
#     col = "black"
#     , shape = 21) +
#     xlim(c(-1211149,-559000)) + 
#     ylim(c(-1050105, -550000))
#   #scale_fill_discrete(type = c("#bf812d","#35978f"))
# )
# 
# ## Look at the relationship between misclassification and elevation
# # # get elevation information for each raster cell 
# # temp_TEMP <- temp %>% 
# #   st_as_sf(coords = c("Long", "Lat"), crs = crs(soilRastTemp))
# # 
# # temp_elev <- elevatr::get_elev_point(temp2, src = "aws")
# # 
# # temp_TEMP <- temp_TEMP %>% st_join(temp_elev, left = TRUE, join = st_nearest_feature)
# # 
# ## confusion matrix
# regMod_2_confMat <- caret::confusionMatrix(data = temp$newRegion_predClass,
#                                            reference = temp$newRegionFact)$table
# 
# ## make plots of good and bad probability
# # just good predictions
# (regMod_2_ternGoodPreds <- temp %>% 
#     filter(goodPred == TRUE) %>% 
#     ggplot() +
#     geom_density(aes(x = newRegion_pred, col = newRegionFact), trim = "FALSE") +
#     theme_minimal() + 
#     labs(color = "true ecoregion") +
#     ggtitle("Accurate Predictions") +
#     xlab("Model-Predicted Ecoregion")+
#     scale_x_continuous(breaks = c(0, .25, .5, .75, 1), 
#                        labels = c("shrub/grass", 0.25, 0.50, 0.75, "forest")) + 
#     scale_color_discrete(type = c("#bf812d","#35978f"))
# )
# # just bad predictions
# badDat <- temp %>% 
#   filter(goodPred == FALSE)
# (regMod_2_ternBadPreds <- 
#     ggplot(badDat) +
#     geom_density(aes(x = newRegion_pred, col = newRegionFact
#     ), trim = "FALSE") +
#     theme_minimal() + 
#     labs(color = "true ecoregion") +
#     ggtitle("Inaccurate Predictions")+
#     xlab("Model-Predicted Ecoregion") +
#     scale_x_continuous(breaks = c(0, .25, .5, .75, 1), 
#                        labels = c("shrub/grass", 0.25, 0.50, 0.75, "forest")) + 
#     scale_color_discrete(type = c("#bf812d","#35978f")))
# 
# (regMod_2Pred_mapBadGrass <- ggplot() +
#     geom_point(data = temp, aes(Long, Lat, col = newRegion), alpha = .3) +
#     ggtitle("Severity of Grass/Shrub misclassification") +
#     geom_point(data = temp[temp$goodPred == FALSE & temp$newRegion_predClass == "dryShrubGrass",], 
#                aes(Long, Lat, fill = newRegion_pred),
#                col = "black" ,
#                shape = 21) +
#     scale_fill_gradient(high = "#f5f5f5", low = "#543005") +
#     scale_color_discrete(type = c("#bf812d","#35978f")) +
#     theme_minimal()
# )
# 
# (regMod_2Pred_mapForest <- ggplot() +
#     geom_point(data = temp, aes(Long, Lat, col = newRegion), alpha = .1) +
#     ggtitle("Severity of Forest misclassification") +
#     geom_point(data = temp[temp$goodPred == FALSE & temp$newRegion_predClass == "forest",], 
#                aes(Long, Lat, fill = newRegion_pred),
#                col = "black" ,
#                shape = 21) +
#     scale_fill_gradient(high = "#003c30", low = "#f5f5f5") +
#     scale_color_discrete(type = c("#bf812d","#35978f")) +
#     theme_minimal()
# ) 
# 
# 
# ## make variable importance figures
# #caret::getModelInfo()
# varImp(testMod_6)
# beta.table_2 <- data.frame(summary(testMod_6)$coef)
# beta.table_2$variable <- row.names(beta.table_2)
# beta.table_2 <- mutate(beta.table_2, estimate = Estimate, 
#                        se = Std..Error, 
#                        low = Estimate - 2*se, 
#                        high = Estimate + 2*se,
#                        p = Pr...z..)  
# 
# effSizeFig_2 <- ggplot(beta.table_2, aes(y=estimate, x=variable, ymin=low, ymax=high, color = (p < 0.05))) + 
#   #guides(color = guide_legend(NULL))+
#   geom_pointrange() +  
#   geom_hline(yintercept = 0, col="darkgrey", lty = 3, lwd=2) +
#   coord_flip() + theme_minimal() + ggtitle("Effect Sizes")
# 
# ## make decile plots 
# # reorganize data frame
# # get decile values
# tempDec <- temp %>% 
#   pivot_longer(cols = c(T_warmestMonth_meanAnnAvg_CLIM, T_coldestMonth_meanAnnAvg_CLIM , precip_wettestMonth_meanAnnAvg_CLIM , 
#                         annWaterDeficit_meanAnnAvg_CLIM , PrecipTempCorr_meanAnnAvg_CLIM , 
#                         isothermality_meanAnnAvg_CLIM , soilDepth , avgSandPerc_acrossDepth , 
#                         avgCoarsePerc_acrossDepth , avgOrganicCarbonPerc_0_3cm ), 
#                names_to = "predictor_name",
#                values_to = "predictor_value"
#   ) %>% 
#   dplyr::select(newRegion_pred, predictor_name, predictor_value)
# tempQuantiles <- tempDec %>% 
#   group_by(predictor_name) %>% 
#   reframe(quantiles = stats::quantile(predictor_value, c(0, .09,.1,.19, .2, .29, .3, .39, .4, .49, 
#                                                          .5, .59, .6, .69, .7, .79, .8, .89, .9, 1)), 
#           quantilesNames = paste0("quant_",c(0, .09,.1,.19, .2, .29, .3, .39, .4, .49, 
#                                              .5, .59, .6, .69, .7, .79, .8, .89, .9, 1))) %>% 
#   pivot_wider( names_from = quantilesNames, 
#                values_from = quantiles)
# 
# test <- tempDec %>% 
#   left_join(tempQuantiles) %>% 
#   mutate(#YesQuant0 = dplyr::between(predictor_value, left = quant_0, right = quant_0.09),
#     YesQuant1 = dplyr::between(predictor_value, left = quant_0.1 , right = quant_0.19),
#     YesQuant2 = dplyr::between(predictor_value, left = quant_0.2 , right = quant_0.29),
#     YesQuant3 = dplyr::between(predictor_value, left = quant_0.3 , right = quant_0.39),
#     YesQuant4 = dplyr::between(predictor_value, left = quant_0.4 , right = quant_0.49),
#     YesQuant5 = dplyr::between(predictor_value, left = quant_0.5 , right = quant_0.59),
#     YesQuant6 = dplyr::between(predictor_value, left = quant_0.6 , right = quant_0.69),
#     YesQuant7 = dplyr::between(predictor_value, left = quant_0.7 , right = quant_0.79),
#     YesQuant8 = dplyr::between(predictor_value, left = quant_0.8 , right = quant_0.89),
#     #YesQuant9 = dplyr::between(predictor_value, left = quant_0.9 , right = quant_1)
#   ) %>% 
#   mutate(#YesQuant0 = case_match(YesQuant0, TRUE ~ quant_0.09),
#     YesQuant1 = case_match(YesQuant1, TRUE ~ quant_0.19),
#     YesQuant2 = case_match(YesQuant2, TRUE ~ quant_0.29),
#     YesQuant3 = case_match(YesQuant3, TRUE ~ quant_0.39),
#     YesQuant4 = case_match(YesQuant4, TRUE ~ quant_0.49),
#     YesQuant5 = case_match(YesQuant5, TRUE ~ quant_0.59),
#     YesQuant6 = case_match(YesQuant6, TRUE ~ quant_0.69),
#     YesQuant7 = case_match(YesQuant7, TRUE ~ quant_0.79),
#     YesQuant8 = case_match(YesQuant8, TRUE ~ quant_0.89)
#     #,YesQuant9 = case_match(YesQuant9, TRUE ~ quant_1)
#   ) %>% 
#   #slice(1:1000) %>% 
#   rowwise() %>% 
#   mutate(Quantile_value = round(sum(na.omit(YesQuant1), na.omit(YesQuant2), na.omit(YesQuant3),
#                                     na.omit(YesQuant4), na.omit(YesQuant5), na.omit(YesQuant6), na.omit(YesQuant7),
#                                     na.omit(YesQuant8)#, na.omit(YesQuant0)
#   ), 
#   5)) %>% 
#   dplyr::select(-(quant_1:YesQuant8)) %>% 
#   group_by(predictor_name, Quantile_value) %>% 
#   summarize(newRegion_pred_mean = mean(newRegion_pred)) 
# 
# # make a plot 
# (quantPlot <- ggplot(data = test) + 
#     facet_wrap(~predictor_name, scales = "free_x") + 
#     #geom_density(aes(Quantile_value, col = predictor_name)) + 
#     geom_line(aes(x = Quantile_value, y = newRegion_pred_mean, col = predictor_name)) +
#     geom_point(aes(x = Quantile_value, y = newRegion_pred_mean, col = predictor_name)) + 
#     theme_minimal() + 
#     labs(y = "mean predicted probability w/in a decile", x = "predictor value") + 
#     scale_color_discrete(guide = "none"))
# 
# ## arrange all of the figures for this model
# ggpubr::annotate_figure(ggarrange(
#   # regMod_2_varImpPlot,
#   ggplotify::as.grob(gt::as_gtable(gt::gt(data.frame("dryShrubGrass" =  data.frame(regMod_2_confMat)$Freq[1:2], 
#                                                      "Forest" = data.frame(regMod_2_confMat)$Freq[3:4],
#                                                      row.names = c("dryShrubGrass", "Forest")), 
#                                           rownames_to_stub = TRUE))), 
#   effSizeFig_2,
#   quantPlot,
#   regMod_2_PredMAP, 
#   regMod_2_PredMAP_GB, 
#   regMod_2_PredMAP_PIPO,
#   #badPredElevation_mapA,
#   regMod_2_ternGoodPreds,
#   regMod_2_ternBadPreds,
#   regMod_2Pred_mapBadGrass, 
#   regMod_2Pred_mapForest, 
#   nrow = 6
# )#, 
# #top = c("Model with depth = 7 and all climate and soil predictors")
# ) %>% 
#   ggexport(
#     filename = "./Figures/EcoRegionModelFigures/Model_REPLACESPrecipDriestMonth_withAnnWatDef_withInteractions/ContemporaryModelResults.pdf",
#     width = 15, height = 45)
# 
# 
# # w/out VPD and precip driest month replaced with ann water deficit: Selected Interactions ----------------
# 
# # make null model  (use testMod_2 as the full model )
# testMod.null <- glm(formula = newRegionFact ~ 1, family = "binomial", 
#                     data = modDat_fitNew)
# testMod_7  <- 
#   glm(data =  modDat_fitNew %>% 
#         dplyr::select(newRegionFact, 
#                       T_warmestMonth_meanAnnAvg_CLIM    , 
#                       T_coldestMonth_meanAnnAvg_CLIM      , precip_wettestMonth_meanAnnAvg_CLIM    ,
#                       annWaterDeficit_meanAnnAvg_CLIM  ,
#                       PrecipTempCorr_meanAnnAvg_CLIM   ,  
#                       isothermality_meanAnnAvg_CLIM   ,
#                       soilDepth                  ,  
#                       avgSandPerc_acrossDepth      , avgCoarsePerc_acrossDepth    ,  
#                       avgOrganicCarbonPerc_0_3cm )
#       , formula = newRegionFact ~ . + T_warmestMonth_meanAnnAvg_CLIM:T_coldestMonth_meanAnnAvg_CLIM +
#         T_warmestMonth_meanAnnAvg_CLIM:precip_wettestMonth_meanAnnAvg_CLIM + 
#         T_warmestMonth_meanAnnAvg_CLIM:annWaterDeficit_meanAnnAvg_CLIM +
#         T_warmestMonth_meanAnnAvg_CLIM:PrecipTempCorr_meanAnnAvg_CLIM +
#         T_warmestMonth_meanAnnAvg_CLIM:isothermality_meanAnnAvg_CLIM + 
#         T_coldestMonth_meanAnnAvg_CLIM:precip_wettestMonth_meanAnnAvg_CLIM  +
#         T_coldestMonth_meanAnnAvg_CLIM:annWaterDeficit_meanAnnAvg_CLIM  +
#         T_coldestMonth_meanAnnAvg_CLIM:PrecipTempCorr_meanAnnAvg_CLIM +
#         T_coldestMonth_meanAnnAvg_CLIM:isothermality_meanAnnAvg_CLIM  +
#         precip_wettestMonth_meanAnnAvg_CLIM:annWaterDeficit_meanAnnAvg_CLIM +
#         precip_wettestMonth_meanAnnAvg_CLIM:PrecipTempCorr_meanAnnAvg_CLIM +
#         precip_wettestMonth_meanAnnAvg_CLIM:isothermality_meanAnnAvg_CLIM + 
#         annWaterDeficit_meanAnnAvg_CLIM:PrecipTempCorr_meanAnnAvg_CLIM  +
#         annWaterDeficit_meanAnnAvg_CLIM:isothermality_meanAnnAvg_CLIM  +
#         PrecipTempCorr_meanAnnAvg_CLIM:isothermality_meanAnnAvg_CLIM  ,
#         family = "binomial", 
#       na.action = na.fail)
# 
# summary(testMod_7)
# 
# head(pp <- fitted(testMod_7))
# 
# AIC(testMod_7) #AIC: 241225.9
# # tetestMod_7# test the predictions of the model
# predictionDat <- modDat_testNew
# predictionDatTemp <- cbind(predictionDat,"newRegion_pred" = predict(testMod_7, newdata = predictionDat, "response"))
# 
# # for each observation, determine which category has the highest predicted probability
# (temp <- predictionDatTemp %>%
#     rowwise() %>% 
#     mutate(newRegion_predClass = newRegion_pred) %>% 
#     mutate(newRegion_predClass = replace(newRegion_predClass, newRegion_pred<.5, "dryShrubGrass"), 
#            newRegion_predClass = replace(newRegion_predClass, newRegion_pred>.5, "forest")) 
# )
# 
# temp$newRegion_predClass <- relevel(as.factor(temp$newRegion_predClass), ref = "dryShrubGrass")
# 
# # are these predictions the same as the actual classifications
# temp$goodPred <- temp$newRegionFact == temp$newRegion_predClass
# sum(!temp$goodPred)/nrow(temp) * 100
# 
# 
# ## make predictions rasterized 
# 
# temp2 <- temp %>% 
#   terra::vect(geom = c("Long", "Lat"), crs = crs(soilRastTemp)
#   ) %>% 
#   terra::rasterize(y = 
#                      terra::aggregate(soilRastTemp, fact = 2, fun = mean, na.rm= TRUE)
#                    , field = "newRegion_pred")
# 
# (regMod_2_PredMAP <- ggplot(temp) +
#     geom_spatraster(data = temp2) +
#     # geom_point(aes(#Long, Lat, 
#     #   col = newRegion_pred)) + 
#     scale_fill_distiller(type = "div", palette = 1, direction = 1) +
#     #geom_sf(data = ecoregionsSF2, color = "black", fill = NA) + 
#     #lims(x = c(-130, -65), y = c(24, 50))
#     
#     
#     #geom_point(aes(Long, Lat, col = newRegion_pred), alpha = .1) +
#     ggtitle("model-predicted ecoregion classification -- 2 ecoregions; no VPD",
#             subtitle = paste0("ecoregion ~ temp_warmestMonth + temp_coldestMonth + precip_wettestMonth +
#             annual water deficit + precipTempCorr + isothermality + soilDepth + % sand + % coarse + soil carbon + 
#             lots of interactions
#           black dots indicate misclassification; \n  ", 
#                               round((100-sum(!temp$goodPred)/nrow(temp) * 100),1), 
#                               "% classification accuracy" )) +
#     geom_point(data = temp[temp$goodPred == FALSE,], aes(Long, Lat#, fill = as.factor(newRegion_predClass)
#     ),
#     col = "black"
#     , shape = 21) #+ 
#   #scale_fill_discrete(type = c("#bf812d","#35978f"))
# )
# 
# ## zoom in on the great basin to check out mis-classification
# (regMod_2_PredMAP_GB <- ggplot(temp) +
#     geom_spatraster(data = temp2) +
#     # geom_point(aes(#Long, Lat, 
#     #   col = newRegion_pred)) + 
#     scale_fill_distiller(type = "div", palette = 1, direction = 1) +
#     #geom_sf(data = ecoregionsSF2, color = "black", fill = NA) + 
#     #lims(x = c(-130, -65), y = c(24, 50))
#     
#     
#     #geom_point(aes(Long, Lat, col = newRegion_pred), alpha = .1) +
#     ggtitle("model-predicted ecoregion classification -- 2 ecoregions; no VPD",
#             subtitle = paste0("Zoom-In on Great Basin Area
#            \n dots indicate misclassification; color indicates true classification; \n  ", 
#                               round((100-sum(!temp$goodPred)/nrow(temp) * 100),1), 
#                               "% classification accuracy" )) +
#     geom_point(data = temp[temp$goodPred == FALSE,], aes(Long, Lat#, fill = as.factor(newRegion_predClass)
#     ),
#     col = "black"
#     , shape = 21) +
#     xlim(c(-1911149,-999000)) + 
#     ylim(c(-600105, 0))
#   #scale_fill_discrete(type = c("#bf812d","#35978f"))
# )
# 
# ## zoom in on the AZ high elevation area to check out mis-classification
# (regMod_2_PredMAP_PIPO <- ggplot(temp) +
#     geom_spatraster(data = temp2) +
#     # geom_point(aes(#Long, Lat, 
#     #   col = newRegion_pred)) + 
#     scale_fill_distiller(type = "div", palette = 1, direction = 1) +
#     #geom_sf(data = ecoregionsSF2, color = "black", fill = NA) + 
#     #lims(x = c(-130, -65), y = c(24, 50))
#     
#     
#     #geom_point(aes(Long, Lat, col = newRegion_pred), alpha = .1) +
#     ggtitle("model-predicted ecoregion classification -- 2 ecoregions; no VPD",
#             subtitle = paste0("Zoom-In on high elevation area in the SW;
#            \n dots indicate misclassification; color indicates true classification \n  ", 
#                               round((100-sum(!temp$goodPred)/nrow(temp) * 100),1), 
#                               "% classification accuracy" )) +
#     geom_point(data = temp[temp$goodPred == FALSE,], aes(Long, Lat#, fill = as.factor(newRegion_predClass)
#     ),
#     col = "black"
#     , shape = 21) +
#     xlim(c(-1211149,-559000)) + 
#     ylim(c(-1050105, -550000))
#   #scale_fill_discrete(type = c("#bf812d","#35978f"))
# )
# 
# ## Look at the relationship between misclassification and elevation
# # # get elevation information for each raster cell 
# # temp_TEMP <- temp %>% 
# #   st_as_sf(coords = c("Long", "Lat"), crs = crs(soilRastTemp))
# # 
# # temp_elev <- elevatr::get_elev_point(temp2, src = "aws")
# # 
# # temp_TEMP <- temp_TEMP %>% st_join(temp_elev, left = TRUE, join = st_nearest_feature)
# # 
# ## confusion matrix
# regMod_2_confMat <- caret::confusionMatrix(data = temp$newRegion_predClass,
#                                            reference = temp$newRegionFact)$table
# 
# ## make plots of good and bad probability
# # just good predictions
# (regMod_2_ternGoodPreds <- temp %>% 
#     filter(goodPred == TRUE) %>% 
#     ggplot() +
#     geom_density(aes(x = newRegion_pred, col = newRegionFact), trim = "FALSE") +
#     theme_minimal() + 
#     labs(color = "true ecoregion") +
#     ggtitle("Accurate Predictions") +
#     xlab("Model-Predicted Ecoregion")+
#     scale_x_continuous(breaks = c(0, .25, .5, .75, 1), 
#                        labels = c("shrub/grass", 0.25, 0.50, 0.75, "forest")) + 
#     scale_color_discrete(type = c("#bf812d","#35978f"))
# )
# # just bad predictions
# badDat <- temp %>% 
#   filter(goodPred == FALSE)
# (regMod_2_ternBadPreds <- 
#     ggplot(badDat) +
#     geom_density(aes(x = newRegion_pred, col = newRegionFact
#     ), trim = "FALSE") +
#     theme_minimal() + 
#     labs(color = "true ecoregion") +
#     ggtitle("Inaccurate Predictions")+
#     xlab("Model-Predicted Ecoregion") +
#     scale_x_continuous(breaks = c(0, .25, .5, .75, 1), 
#                        labels = c("shrub/grass", 0.25, 0.50, 0.75, "forest")) + 
#     scale_color_discrete(type = c("#bf812d","#35978f")))
# 
# (regMod_2Pred_mapBadGrass <- ggplot() +
#     geom_point(data = temp, aes(Long, Lat, col = newRegion), alpha = .3) +
#     ggtitle("Severity of Grass/Shrub misclassification") +
#     geom_point(data = temp[temp$goodPred == FALSE & temp$newRegion_predClass == "dryShrubGrass",], 
#                aes(Long, Lat, fill = newRegion_pred),
#                col = "black" ,
#                shape = 21) +
#     scale_fill_gradient(high = "#f5f5f5", low = "#543005") +
#     scale_color_discrete(type = c("#bf812d","#35978f")) +
#     theme_minimal()
# )
# 
# (regMod_2Pred_mapForest <- ggplot() +
#     geom_point(data = temp, aes(Long, Lat, col = newRegion), alpha = .1) +
#     ggtitle("Severity of Forest misclassification") +
#     geom_point(data = temp[temp$goodPred == FALSE & temp$newRegion_predClass == "forest",], 
#                aes(Long, Lat, fill = newRegion_pred),
#                col = "black" ,
#                shape = 21) +
#     scale_fill_gradient(high = "#003c30", low = "#f5f5f5") +
#     scale_color_discrete(type = c("#bf812d","#35978f")) +
#     theme_minimal()
# ) 
# 
# 
# ## make variable importance figures
# #caret::getModelInfo()
# varImp(testMod_7)
# beta.table_2 <- data.frame(summary(testMod_7)$coef)
# beta.table_2$variable <- row.names(beta.table_2)
# beta.table_2 <- mutate(beta.table_2, estimate = Estimate, 
#                        se = Std..Error, 
#                        low = Estimate - 2*se, 
#                        high = Estimate + 2*se,
#                        p = Pr...z..)  
# 
# effSizeFig_2 <- ggplot(beta.table_2, aes(y=estimate, x=variable, ymin=low, ymax=high, color = (p < 0.05))) + 
#   #guides(color = guide_legend(NULL))+
#   geom_pointrange() +  
#   geom_hline(yintercept = 0, col="darkgrey", lty = 3, lwd=2) +
#   coord_flip() + theme_minimal() + ggtitle("Effect Sizes")
# 
# ## make decile plots 
# # reorganize data frame
# # get decile values
# tempDec <- temp %>% 
#   pivot_longer(cols = c(T_warmestMonth_meanAnnAvg_CLIM, T_coldestMonth_meanAnnAvg_CLIM , precip_wettestMonth_meanAnnAvg_CLIM , 
#                         annWaterDeficit_meanAnnAvg_CLIM , PrecipTempCorr_meanAnnAvg_CLIM , 
#                         isothermality_meanAnnAvg_CLIM , soilDepth , avgSandPerc_acrossDepth , 
#                         avgCoarsePerc_acrossDepth , avgOrganicCarbonPerc_0_3cm ), 
#                names_to = "predictor_name",
#                values_to = "predictor_value"
#   ) %>% 
#   dplyr::select(newRegion_pred, predictor_name, predictor_value)
# tempQuantiles <- tempDec %>% 
#   group_by(predictor_name) %>% 
#   reframe(quantiles = stats::quantile(predictor_value, c(0, .09,.1,.19, .2, .29, .3, .39, .4, .49, 
#                                                          .5, .59, .6, .69, .7, .79, .8, .89, .9, 1)), 
#           quantilesNames = paste0("quant_",c(0, .09,.1,.19, .2, .29, .3, .39, .4, .49, 
#                                              .5, .59, .6, .69, .7, .79, .8, .89, .9, 1))) %>% 
#   pivot_wider( names_from = quantilesNames, 
#                values_from = quantiles)
# 
# test <- tempDec %>% 
#   left_join(tempQuantiles) %>% 
#   mutate(#YesQuant0 = dplyr::between(predictor_value, left = quant_0, right = quant_0.09),
#     YesQuant1 = dplyr::between(predictor_value, left = quant_0.1 , right = quant_0.19),
#     YesQuant2 = dplyr::between(predictor_value, left = quant_0.2 , right = quant_0.29),
#     YesQuant3 = dplyr::between(predictor_value, left = quant_0.3 , right = quant_0.39),
#     YesQuant4 = dplyr::between(predictor_value, left = quant_0.4 , right = quant_0.49),
#     YesQuant5 = dplyr::between(predictor_value, left = quant_0.5 , right = quant_0.59),
#     YesQuant6 = dplyr::between(predictor_value, left = quant_0.6 , right = quant_0.69),
#     YesQuant7 = dplyr::between(predictor_value, left = quant_0.7 , right = quant_0.79),
#     YesQuant8 = dplyr::between(predictor_value, left = quant_0.8 , right = quant_0.89),
#     #YesQuant9 = dplyr::between(predictor_value, left = quant_0.9 , right = quant_1)
#   ) %>% 
#   mutate(#YesQuant0 = case_match(YesQuant0, TRUE ~ quant_0.09),
#     YesQuant1 = case_match(YesQuant1, TRUE ~ quant_0.19),
#     YesQuant2 = case_match(YesQuant2, TRUE ~ quant_0.29),
#     YesQuant3 = case_match(YesQuant3, TRUE ~ quant_0.39),
#     YesQuant4 = case_match(YesQuant4, TRUE ~ quant_0.49),
#     YesQuant5 = case_match(YesQuant5, TRUE ~ quant_0.59),
#     YesQuant6 = case_match(YesQuant6, TRUE ~ quant_0.69),
#     YesQuant7 = case_match(YesQuant7, TRUE ~ quant_0.79),
#     YesQuant8 = case_match(YesQuant8, TRUE ~ quant_0.89)
#     #,YesQuant9 = case_match(YesQuant9, TRUE ~ quant_1)
#   ) %>% 
#   #slice(1:1000) %>% 
#   rowwise() %>% 
#   mutate(Quantile_value = round(sum(na.omit(YesQuant1), na.omit(YesQuant2), na.omit(YesQuant3),
#                                     na.omit(YesQuant4), na.omit(YesQuant5), na.omit(YesQuant6), na.omit(YesQuant7),
#                                     na.omit(YesQuant8)#, na.omit(YesQuant0)
#   ), 
#   5)) %>% 
#   dplyr::select(-(quant_1:YesQuant8)) %>% 
#   group_by(predictor_name, Quantile_value) %>% 
#   summarize(newRegion_pred_mean = mean(newRegion_pred)) 
# 
# 
# ## Make interaction plots 
# library(sjPlot)
# library(sjmisc)
# (int1_plot <- plot_model(testMod_7, type = "pred", terms = c("T_warmestMonth_meanAnnAvg_CLIM", 
#                                                "T_coldestMonth_meanAnnAvg_CLIM")))
# (int2_plot <- plot_model(testMod_7, type = "pred", terms = c("T_warmestMonth_meanAnnAvg_CLIM", 
#                                                              "precip_wettestMonth_meanAnnAvg_CLIM")))
# (int3_plot <- plot_model(testMod_7, type = "pred", terms = c("T_warmestMonth_meanAnnAvg_CLIM", 
#                                                              "annWaterDeficit_meanAnnAvg_CLIM")))
# (int4_plot <- plot_model(testMod_7, type = "pred", terms = c("T_warmestMonth_meanAnnAvg_CLIM", 
#                                                              "PrecipTempCorr_meanAnnAvg_CLIM")))
# (int5_plot <- plot_model(testMod_7, type = "pred", terms = c("T_warmestMonth_meanAnnAvg_CLIM", 
#                                                              "isothermality_meanAnnAvg_CLIM")))
# 
# (int6_plot <- plot_model(testMod_7, type = "pred", terms = c("T_coldestMonth_meanAnnAvg_CLIM", 
#                                                              "precip_wettestMonth_meanAnnAvg_CLIM")))
# (int7_plot <- plot_model(testMod_7, type = "pred", terms = c("T_coldestMonth_meanAnnAvg_CLIM", 
#                                                              "annWaterDeficit_meanAnnAvg_CLIM")))
# (int7_plot <- plot_model(testMod_7, type = "pred", terms = c("T_coldestMonth_meanAnnAvg_CLIM", 
#                                                              "PrecipTempCorr_meanAnnAvg_CLIM")))
# (int8_plot <- plot_model(testMod_7, type = "pred", terms = c("T_coldestMonth_meanAnnAvg_CLIM", 
#                                                              "isothermality_meanAnnAvg_CLIM")))
# 
# (int9_plot <- plot_model(testMod_7, type = "pred", terms = c("annWaterDeficit_meanAnnAvg_CLIM", 
#                                                              "PrecipTempCorr_meanAnnAvg_CLIM")))
# (int10_plot <- plot_model(testMod_7, type = "pred", terms = c("annWaterDeficit_meanAnnAvg_CLIM", 
#                                                              "isothermality_meanAnnAvg_CLIM")))
# 
# (int11_plot <- plot_model(testMod_7, type = "pred", terms = c("PrecipTempCorr_meanAnnAvg_CLIM",  
#                                                               "isothermality_meanAnnAvg_CLIM")))
#  
#                                                               
# (intPlots <-  ggarrange(int1_plot, int2_plot, int3_plot, int4_plot, int5_plot, 
#             int6_plot, int7_plot, int8_plot,
#             int9_plot, int10_plot,
#             int11_plot,
#                       ncol = 2, nrow = 6, align = "hv")
#   )
# 
# # make a plot 
# (quantPlot <- ggplot(data = test) + 
#     facet_wrap(~predictor_name, scales = "free_x") + 
#     #geom_density(aes(Quantile_value, col = predictor_name)) + 
#     geom_line(aes(x = Quantile_value, y = newRegion_pred_mean, col = predictor_name)) +
#     geom_point(aes(x = Quantile_value, y = newRegion_pred_mean, col = predictor_name)) + 
#     theme_minimal() + 
#     labs(y = "mean predicted probability w/in a decile", x = "predictor value") + 
#     scale_color_discrete(guide = "none"))
# 
# ## arrange all of the figures for this model
# ggpubr::annotate_figure(ggarrange(
#   # regMod_2_varImpPlot,
#   ggplotify::as.grob(gt::as_gtable(gt::gt(data.frame("dryShrubGrass" =  data.frame(regMod_2_confMat)$Freq[1:2], 
#                                                      "Forest" = data.frame(regMod_2_confMat)$Freq[3:4],
#                                                      row.names = c("dryShrubGrass", "Forest")), 
#                                           rownames_to_stub = TRUE))), 
#   effSizeFig_2,
#   quantPlot,
#   regMod_2_PredMAP, 
#   regMod_2_PredMAP_GB, 
#   regMod_2_PredMAP_PIPO,
#   #badPredElevation_mapA,
#   regMod_2_ternGoodPreds,
#   regMod_2_ternBadPreds,
#   regMod_2Pred_mapBadGrass, 
#   regMod_2Pred_mapForest, 
#   nrow = 6
# )#, 
# #top = c("Model with depth = 7 and all climate and soil predictors")
# ) %>% 
#   ggexport(
#     filename = "./Figures/EcoRegionModelFigures/Model_REPLACESPrecipDriestMonth_withAnnWatDef_withSOMEInteractions/ContemporaryModelResults.pdf",
#     width = 15, height = 45)
# 
# 
# # w/out VPD and precip driest month replaced with ann water deficit: Even FewerInteractions ----------------
# 
# # in previous model, many interactions didn't have that much of an impact (as seen in interaction figures... )
# # will retain these: 
# # annWaterDeficit_meanAnnAvg_CLIM:T_warmestMonth_meanAnnAvg_CLIM
# # T_coldestMonth_meanAnnAvg_CLIM:PrecipTempCorr_meanAnnAvg_CLIM
# # T_warmestMonth_meanAnnAvg_CLIM:PrecipTempCorr_meanAnnAvg_CLIM
# 
# testMod_8  <- 
#   glm(data =  modDat_fitNew %>% 
#         dplyr::select(newRegionFact, 
#                       T_warmestMonth_meanAnnAvg_CLIM    , 
#                       T_coldestMonth_meanAnnAvg_CLIM      , precip_wettestMonth_meanAnnAvg_CLIM    ,
#                       annWaterDeficit_meanAnnAvg_CLIM  ,
#                       PrecipTempCorr_meanAnnAvg_CLIM   ,  
#                       isothermality_meanAnnAvg_CLIM   ,
#                       soilDepth                  ,  
#                       avgSandPerc_acrossDepth      , avgCoarsePerc_acrossDepth    ,  
#                       avgOrganicCarbonPerc_0_3cm )
#       , formula = newRegionFact ~ . + annWaterDeficit_meanAnnAvg_CLIM:T_warmestMonth_meanAnnAvg_CLIM + 
#         T_coldestMonth_meanAnnAvg_CLIM:PrecipTempCorr_meanAnnAvg_CLIM +
#         T_warmestMonth_meanAnnAvg_CLIM:PrecipTempCorr_meanAnnAvg_CLIM,
#       family = "binomial", 
#       na.action = na.fail)
# 
# summary(testMod_8)
# 
# AIC(testMod_8) #AIC: 241225.9
# # tetestMod_8# test the predictions of the model
# predictionDat <- modDat_testNew
# predictionDatTemp <- cbind(predictionDat,"newRegion_pred" = predict(testMod_8, newdata = predictionDat, "response"))
# 
# # for each observation, determine which category has the highest predicted probability
# (temp <- predictionDatTemp %>%
#     rowwise() %>% 
#     mutate(newRegion_predClass = newRegion_pred) %>% 
#     mutate(newRegion_predClass = replace(newRegion_predClass, newRegion_pred<.5, "dryShrubGrass"), 
#            newRegion_predClass = replace(newRegion_predClass, newRegion_pred>.5, "forest")) 
# )
# 
# temp$newRegion_predClass <- relevel(as.factor(temp$newRegion_predClass), ref = "dryShrubGrass")
# 
# # are these predictions the same as the actual classifications
# temp$goodPred <- temp$newRegionFact == temp$newRegion_predClass
# sum(!temp$goodPred)/nrow(temp) * 100
# 
# ## make predictions rasterized 
# 
# temp2 <- temp %>% 
#   terra::vect(geom = c("Long", "Lat"), crs = crs(soilRastTemp)
#   ) %>% 
#   terra::rasterize(y = 
#                      terra::aggregate(soilRastTemp, fact = 2, fun = mean, na.rm= TRUE)
#                    , field = "newRegion_pred")
# 
# (regMod_2_PredMAP <- ggplot(temp) +
#     geom_spatraster(data = temp2) +
#     # geom_point(aes(#Long, Lat, 
#     #   col = newRegion_pred)) + 
#     scale_fill_distiller(type = "div", palette = 1, direction = 1) +
#     #geom_sf(data = ecoregionsSF2, color = "black", fill = NA) + 
#     #lims(x = c(-130, -65), y = c(24, 50))
#     
#     
#     #geom_point(aes(Long, Lat, col = newRegion_pred), alpha = .1) +
#     ggtitle("model-predicted ecoregion classification -- 2 ecoregions; no VPD",
#             subtitle = paste0("ecoregion ~ temp_warmestMonth + temp_coldestMonth + precip_wettestMonth +
#             annual water deficit + precipTempCorr + isothermality + soilDepth + % sand + % coarse + soil carbon + 
#             lots of interactions
#           black dots indicate misclassification; \n  ", 
#                               round((100-sum(!temp$goodPred)/nrow(temp) * 100),1), 
#                               "% classification accuracy" )) +
#     geom_point(data = temp[temp$goodPred == FALSE,], aes(Long, Lat#, fill = as.factor(newRegion_predClass)
#     ),
#     col = "black"
#     , shape = 21) #+ 
#   #scale_fill_discrete(type = c("#bf812d","#35978f"))
# )
# 
# ## zoom in on the great basin to check out mis-classification
# (regMod_2_PredMAP_GB <- ggplot(temp) +
#     geom_spatraster(data = temp2) +
#     # geom_point(aes(#Long, Lat, 
#     #   col = newRegion_pred)) + 
#     scale_fill_distiller(type = "div", palette = 1, direction = 1) +
#     #geom_sf(data = ecoregionsSF2, color = "black", fill = NA) + 
#     #lims(x = c(-130, -65), y = c(24, 50))
#     
#     
#     #geom_point(aes(Long, Lat, col = newRegion_pred), alpha = .1) +
#     ggtitle("model-predicted ecoregion classification -- 2 ecoregions; no VPD",
#             subtitle = paste0("Zoom-In on Great Basin Area
#            \n dots indicate misclassification; color indicates true classification; \n  ", 
#                               round((100-sum(!temp$goodPred)/nrow(temp) * 100),1), 
#                               "% classification accuracy" )) +
#     geom_point(data = temp[temp$goodPred == FALSE,], aes(Long, Lat#, fill = as.factor(newRegion_predClass)
#     ),
#     col = "black"
#     , shape = 21) +
#     xlim(c(-1911149,-999000)) + 
#     ylim(c(-600105, 0))
#   #scale_fill_discrete(type = c("#bf812d","#35978f"))
# )
# 
# ## zoom in on the AZ high elevation area to check out mis-classification
# (regMod_2_PredMAP_PIPO <- ggplot(temp) +
#     geom_spatraster(data = temp2) +
#     # geom_point(aes(#Long, Lat, 
#     #   col = newRegion_pred)) + 
#     scale_fill_distiller(type = "div", palette = 1, direction = 1) +
#     #geom_sf(data = ecoregionsSF2, color = "black", fill = NA) + 
#     #lims(x = c(-130, -65), y = c(24, 50))
#     
#     
#     #geom_point(aes(Long, Lat, col = newRegion_pred), alpha = .1) +
#     ggtitle("model-predicted ecoregion classification -- 2 ecoregions; no VPD",
#             subtitle = paste0("Zoom-In on high elevation area in the SW;
#            \n dots indicate misclassification; color indicates true classification \n  ", 
#                               round((100-sum(!temp$goodPred)/nrow(temp) * 100),1), 
#                               "% classification accuracy" )) +
#     geom_point(data = temp[temp$goodPred == FALSE,], aes(Long, Lat#, fill = as.factor(newRegion_predClass)
#     ),
#     col = "black"
#     , shape = 21) +
#     xlim(c(-1211149,-559000)) + 
#     ylim(c(-1050105, -550000))
#   #scale_fill_discrete(type = c("#bf812d","#35978f"))
# )
# 
# ## Look at the relationship between misclassification and elevation
# # # get elevation information for each raster cell 
# # temp_TEMP <- temp %>% 
# #   st_as_sf(coords = c("Long", "Lat"), crs = crs(soilRastTemp))
# # 
# # temp_elev <- elevatr::get_elev_point(temp2, src = "aws")
# # 
# # temp_TEMP <- temp_TEMP %>% st_join(temp_elev, left = TRUE, join = st_nearest_feature)
# # 
# ## confusion matrix
# regMod_2_confMat <- caret::confusionMatrix(data = temp$newRegion_predClass,
#                                            reference = temp$newRegionFact)$table
# 
# ## make plots of good and bad probability
# # just good predictions
# (regMod_2_ternGoodPreds <- temp %>% 
#     filter(goodPred == TRUE) %>% 
#     ggplot() +
#     geom_density(aes(x = newRegion_pred, col = newRegionFact), trim = "FALSE") +
#     theme_minimal() + 
#     labs(color = "true ecoregion") +
#     ggtitle("Accurate Predictions") +
#     xlab("Model-Predicted Ecoregion")+
#     scale_x_continuous(breaks = c(0, .25, .5, .75, 1), 
#                        labels = c("shrub/grass", 0.25, 0.50, 0.75, "forest")) + 
#     scale_color_discrete(type = c("#bf812d","#35978f"))
# )
# # just bad predictions
# badDat <- temp %>% 
#   filter(goodPred == FALSE)
# (regMod_2_ternBadPreds <- 
#     ggplot(badDat) +
#     geom_density(aes(x = newRegion_pred, col = newRegionFact
#     ), trim = "FALSE") +
#     theme_minimal() + 
#     labs(color = "true ecoregion") +
#     ggtitle("Inaccurate Predictions")+
#     xlab("Model-Predicted Ecoregion") +
#     scale_x_continuous(breaks = c(0, .25, .5, .75, 1), 
#                        labels = c("shrub/grass", 0.25, 0.50, 0.75, "forest")) + 
#     scale_color_discrete(type = c("#bf812d","#35978f")))
# 
# (regMod_2Pred_mapBadGrass <- ggplot() +
#     geom_point(data = temp, aes(Long, Lat, col = newRegion), alpha = .3) +
#     ggtitle("Severity of Grass/Shrub misclassification") +
#     geom_point(data = temp[temp$goodPred == FALSE & temp$newRegion_predClass == "dryShrubGrass",], 
#                aes(Long, Lat, fill = newRegion_pred),
#                col = "black" ,
#                shape = 21) +
#     scale_fill_gradient(high = "#f5f5f5", low = "#543005") +
#     scale_color_discrete(type = c("#bf812d","#35978f")) +
#     theme_minimal()
# )
# 
# (regMod_2Pred_mapForest <- ggplot() +
#     geom_point(data = temp, aes(Long, Lat, col = newRegion), alpha = .1) +
#     ggtitle("Severity of Forest misclassification") +
#     geom_point(data = temp[temp$goodPred == FALSE & temp$newRegion_predClass == "forest",], 
#                aes(Long, Lat, fill = newRegion_pred),
#                col = "black" ,
#                shape = 21) +
#     scale_fill_gradient(high = "#003c30", low = "#f5f5f5") +
#     scale_color_discrete(type = c("#bf812d","#35978f")) +
#     theme_minimal()
# ) 
# 
# ## make variable importance figures
# #caret::getModelInfo()
# varImp(testMod_8)
# beta.table_2 <- data.frame(summary(testMod_8)$coef)
# beta.table_2$variable <- row.names(beta.table_2)
# beta.table_2 <- mutate(beta.table_2, estimate = Estimate, 
#                        se = Std..Error, 
#                        low = Estimate - 2*se, 
#                        high = Estimate + 2*se,
#                        p = Pr...z..)  
# 
# effSizeFig_2 <- ggplot(beta.table_2, aes(y=estimate, x=variable, ymin=low, ymax=high, color = (p < 0.05))) + 
#   #guides(color = guide_legend(NULL))+
#   geom_pointrange() +  
#   geom_hline(yintercept = 0, col="darkgrey", lty = 3, lwd=2) +
#   coord_flip() + theme_minimal() + ggtitle("Effect Sizes")
# 
# ## make decile plots 
# # reorganize data frame
# # get decile values
# tempDec <- temp %>% 
#   pivot_longer(cols = c(T_warmestMonth_meanAnnAvg_CLIM, T_coldestMonth_meanAnnAvg_CLIM , precip_wettestMonth_meanAnnAvg_CLIM , 
#                         annWaterDeficit_meanAnnAvg_CLIM , PrecipTempCorr_meanAnnAvg_CLIM , 
#                         isothermality_meanAnnAvg_CLIM , soilDepth , avgSandPerc_acrossDepth , 
#                         avgCoarsePerc_acrossDepth , avgOrganicCarbonPerc_0_3cm ), 
#                names_to = "predictor_name",
#                values_to = "predictor_value"
#   ) %>% 
#   dplyr::select(newRegion_pred, predictor_name, predictor_value)
# tempQuantiles <- tempDec %>% 
#   group_by(predictor_name) %>% 
#   reframe(quantiles = stats::quantile(predictor_value, c(0, .09,.1,.19, .2, .29, .3, .39, .4, .49, 
#                                                          .5, .59, .6, .69, .7, .79, .8, .89, .9, 1)), 
#           quantilesNames = paste0("quant_",c(0, .09,.1,.19, .2, .29, .3, .39, .4, .49, 
#                                              .5, .59, .6, .69, .7, .79, .8, .89, .9, 1))) %>% 
#   pivot_wider( names_from = quantilesNames, 
#                values_from = quantiles)
# 
# test <- tempDec %>% 
#   left_join(tempQuantiles) %>% 
#   mutate(#YesQuant0 = dplyr::between(predictor_value, left = quant_0, right = quant_0.09),
#     YesQuant1 = dplyr::between(predictor_value, left = quant_0.1 , right = quant_0.19),
#     YesQuant2 = dplyr::between(predictor_value, left = quant_0.2 , right = quant_0.29),
#     YesQuant3 = dplyr::between(predictor_value, left = quant_0.3 , right = quant_0.39),
#     YesQuant4 = dplyr::between(predictor_value, left = quant_0.4 , right = quant_0.49),
#     YesQuant5 = dplyr::between(predictor_value, left = quant_0.5 , right = quant_0.59),
#     YesQuant6 = dplyr::between(predictor_value, left = quant_0.6 , right = quant_0.69),
#     YesQuant7 = dplyr::between(predictor_value, left = quant_0.7 , right = quant_0.79),
#     YesQuant8 = dplyr::between(predictor_value, left = quant_0.8 , right = quant_0.89),
#     #YesQuant9 = dplyr::between(predictor_value, left = quant_0.9 , right = quant_1)
#   ) %>% 
#   mutate(#YesQuant0 = case_match(YesQuant0, TRUE ~ quant_0.09),
#     YesQuant1 = case_match(YesQuant1, TRUE ~ quant_0.19),
#     YesQuant2 = case_match(YesQuant2, TRUE ~ quant_0.29),
#     YesQuant3 = case_match(YesQuant3, TRUE ~ quant_0.39),
#     YesQuant4 = case_match(YesQuant4, TRUE ~ quant_0.49),
#     YesQuant5 = case_match(YesQuant5, TRUE ~ quant_0.59),
#     YesQuant6 = case_match(YesQuant6, TRUE ~ quant_0.69),
#     YesQuant7 = case_match(YesQuant7, TRUE ~ quant_0.79),
#     YesQuant8 = case_match(YesQuant8, TRUE ~ quant_0.89)
#     #,YesQuant9 = case_match(YesQuant9, TRUE ~ quant_1)
#   ) %>% 
#   #slice(1:1000) %>% 
#   rowwise() %>% 
#   mutate(Quantile_value = round(sum(na.omit(YesQuant1), na.omit(YesQuant2), na.omit(YesQuant3),
#                                     na.omit(YesQuant4), na.omit(YesQuant5), na.omit(YesQuant6), na.omit(YesQuant7),
#                                     na.omit(YesQuant8)#, na.omit(YesQuant0)
#   ), 
#   5)) %>% 
#   dplyr::select(-(quant_1:YesQuant8)) %>% 
#   group_by(predictor_name, Quantile_value) %>% 
#   summarize(newRegion_pred_mean = mean(newRegion_pred)) 
# 
# # make a plot 
# (quantPlot <- ggplot(data = test) + 
#     facet_wrap(~predictor_name, scales = "free_x") + 
#     #geom_density(aes(Quantile_value, col = predictor_name)) + 
#     geom_line(aes(x = Quantile_value, y = newRegion_pred_mean, col = predictor_name)) +
#     geom_point(aes(x = Quantile_value, y = newRegion_pred_mean, col = predictor_name)) + 
#     theme_minimal() + 
#     labs(y = "mean predicted probability w/in a decile", x = "predictor value") + 
#     scale_color_discrete(guide = "none"))
# 
# ## Make interaction plots 
# library(sjPlot)
# library(sjmisc)
# (int1_plot <- plot_model(testMod_8, type = "pred", terms = c("T_warmestMonth_meanAnnAvg_CLIM", 
#                                                              "annWaterDeficit_meanAnnAvg_CLIM")))
# (int2_plot <- plot_model(testMod_8, type = "pred", terms = c("T_coldestMonth_meanAnnAvg_CLIM", 
#                                                              "PrecipTempCorr_meanAnnAvg_CLIM")))
# (int3_plot <- plot_model(testMod_8, type = "pred", terms = c("T_warmestMonth_meanAnnAvg_CLIM", 
#                                                              "PrecipTempCorr_meanAnnAvg_CLIM")))
# 
# 
# (intPlots <-  ggarrange(int1_plot, int2_plot, int3_plot,
#                         ncol = 2, nrow = 2, align = "hv")
# )
# 
# 
# 
# ## arrange all of the figures for this model
# ggpubr::annotate_figure(ggarrange(
#   # regMod_2_varImpPlot,
#   ggplotify::as.grob(gt::as_gtable(gt::gt(data.frame("dryShrubGrass" =  data.frame(regMod_2_confMat)$Freq[1:2], 
#                                                      "Forest" = data.frame(regMod_2_confMat)$Freq[3:4],
#                                                      row.names = c("dryShrubGrass", "Forest")), 
#                                           rownames_to_stub = TRUE))), 
#   effSizeFig_2,
#   quantPlot,
#   regMod_2_PredMAP, 
#   regMod_2_PredMAP_GB, 
#   regMod_2_PredMAP_PIPO,
#   #badPredElevation_mapA,
#   regMod_2_ternGoodPreds,
#   regMod_2_ternBadPreds,
#   regMod_2Pred_mapBadGrass, 
#   regMod_2Pred_mapForest, 
#   nrow = 6
# )#, 
# #top = c("Model with depth = 7 and all climate and soil predictors")
# ) %>% 
#   ggexport(
#     filename = "./Figures/EcoRegionModelFigures/Model_REPLACESPrecipDriestMonth_withAnnWatDef_withEvenFewerInteractions/ContemporaryModelResults.pdf",
#     width = 15, height = 45)
# 
# 
# 
# # w/out VPD: Fit a simple regression w/ two ecoregions SCALED input variables ----------------
# 
# modDat_fitNew <- modDat_fit %>% 
#   mutate(newRegion = str_replace(newRegion, "eastForest", "forest"),
#          newRegion = str_replace(newRegion, "westForest", "forest")) %>% 
#   mutate(newRegionFact = as.factor(newRegion)) %>% 
#   mutate(T_warmestMonth_S = scale(T_warmestMonth_meanAnnAvg_CLIM),
#          T_coldestMonth_S = scale(T_coldestMonth_meanAnnAvg_CLIM),
#          precip_wettestMonth_S = scale(precip_wettestMonth_meanAnnAvg_CLIM),
#          precip_driestMonth_S = scale(precip_driestMonth_meanAnnAvg_CLIM),
#          PrecipTempCorr_S = scale(PrecipTempCorr_meanAnnAvg_CLIM),
#          isothermality_S = scale(isothermality_meanAnnAvg_CLIM),
#          soilDepth_S = scale(soilDepth),
#          avgSandPerc_S = scale(avgSandPerc_acrossDepth),
#          avgCoarsePerc_S = scale(avgCoarsePerc_acrossDepth),
#          avgOrganicCarbon_S = scale(avgOrganicCarbonPerc_0_3cm))
# 
# modDat_testNew <- modDat_test %>% 
#   mutate(newRegion = str_replace(newRegion, "eastForest", "forest"),
#          newRegion = str_replace(newRegion, "westForest", "forest")) %>% 
#   mutate(newRegionFact = as.factor(newRegion)) %>% 
#   mutate(T_warmestMonth_S = scale(T_warmestMonth_meanAnnAvg_CLIM),
#          T_coldestMonth_S = scale(T_coldestMonth_meanAnnAvg_CLIM),
#          precip_wettestMonth_S = scale(precip_wettestMonth_meanAnnAvg_CLIM),
#          precip_driestMonth_S = scale(precip_driestMonth_meanAnnAvg_CLIM),
#          PrecipTempCorr_S = scale(PrecipTempCorr_meanAnnAvg_CLIM),
#          isothermality_S = scale(isothermality_meanAnnAvg_CLIM),
#          soilDepth_S = scale(soilDepth),
#          avgSandPerc_S = scale(avgSandPerc_acrossDepth),
#          avgCoarsePerc_S = scale(avgCoarsePerc_acrossDepth),
#          avgOrganicCarbon_S = scale(avgOrganicCarbonPerc_0_3cm))
# 
# testMod_2b <- glm(newRegionFact ~ T_warmestMonth_S       + 
#                     T_coldestMonth_S         +
#                     precip_wettestMonth_S    +
#                    precip_driestMonth_S   + 
#                    PrecipTempCorr_S        +   
#                    isothermality_S          +
#                    soilDepth_S                           +  
#                     avgSandPerc_S               + avgCoarsePerc_S           +  
#                     avgOrganicCarbon_S,
#                  data = modDat_fitNew, 
#                  family = "binomial", 
#                  na.action = na.fail
# )
# 
# summary(testMod_2b)
# 
# head(pp <- fitted(testMod_2b))
# 
# AIC(testMod_2b) #AIC: 241225.9
# # tetestMod_2b# test the predictions of the model
# predictionDat <- modDat_testNew
# predictionDatTemp <- cbind(predictionDat,"newRegion_pred" = predict(testMod_2b, newdata = predictionDat, "response"))
# 
# # for each observation, determine which category has the highest predicted probability
# (temp <- predictionDatTemp %>%
#     rowwise() %>% 
#     mutate(newRegion_predClass = newRegion_pred) %>% 
#     mutate(newRegion_predClass = replace(newRegion_predClass, newRegion_pred<.5, "dryShrubGrass"), 
#            newRegion_predClass = replace(newRegion_predClass, newRegion_pred>.5, "forest")) 
# )
# 
# temp$newRegion_predClass <- relevel(as.factor(temp$newRegion_predClass), ref = "dryShrubGrass")
# 
# # are these predictions the same as the actual classifications
# temp$goodPred <- temp$newRegionFact == temp$newRegion_predClass
# sum(!temp$goodPred)/nrow(temp) * 100
# 
# 
# ## make predictions rasterized 
# 
# temp2b <- temp %>% 
#   terra::vect(geom = c("Long", "Lat"), crs = crs(soilRastTemp)
#   ) %>% 
#   terra::rasterize(y = 
#                      terra::aggregate(soilRastTemp, fact = 2, fun = mean, na.rm= TRUE)
#                    , field = "newRegion_pred")
# 
# (regMod_2b_PredMAP <- ggplot(temp) +
#     geom_spatraster(data = temp2b) +
#     # geom_point(aes(#Long, Lat, 
#     #   col = newRegion_pred)) + 
#     scale_fill_distiller(type = "div", palette = 1, direction = 1) +
#     #geom_sf(data = ecoregionsSF2, color = "black", fill = NA) + 
#     #lims(x = c(-130, -65), y = c(24, 50))
#     
#     
#     #geom_point(aes(Long, Lat, col = newRegion_pred), alpha = .1) +
#     ggtitle("model-predicted ecoregion classification -- 2 ecoregions; no VPD",
#             subtitle = paste0("ecoregion ~ temp_warmestMonth + temp_coldestMonth + precip_wettestMonth +
#             precip_driestMonth + precipTempCorr + isothermality + soilDepth + % sand + % coarse + soil carbon
#            \n black dots indicate misclassification; \n  ", 
#                               round((100-sum(!temp$goodPred)/nrow(temp) * 100),1), 
#                               "% classification accuracy" )) +
#     geom_point(data = temp[temp$goodPred == FALSE,], aes(Long, Lat#, fill = as.factor(newRegion_predClass)
#     ),
#     col = "black"
#     , shape = 21) #+ 
#   #scale_fill_discrete(type = c("#bf812d","#35978f"))
# )
# 
# ## zoom in on the great basin to check out mis-classification
# (regMod_2b_PredMAP_GB <- ggplot(temp) +
#     geom_spatraster(data = temp2b) +
#     # geom_point(aes(#Long, Lat, 
#     #   col = newRegion_pred)) + 
#     scale_fill_distiller(type = "div", palette = 1, direction = 1) +
#     #geom_sf(data = ecoregionsSF2, color = "black", fill = NA) + 
#     #lims(x = c(-130, -65), y = c(24, 50))
#     
#     
#     #geom_point(aes(Long, Lat, col = newRegion_pred), alpha = .1) +
#     ggtitle("model-predicted ecoregion classification -- 2 ecoregions; no VPD",
#             subtitle = paste0("Zoom-In on Great Basin Area
#            \n dots indicate misclassification; color indicates true classification; \n  ", 
#                               round((100-sum(!temp$goodPred)/nrow(temp) * 100),1), 
#                               "% classification accuracy" )) +
#     geom_point(data = temp[temp$goodPred == FALSE,], aes(Long, Lat#, fill = as.factor(newRegion_predClass)
#     ),
#     col = "black"
#     , shape = 21) +
#     xlim(c(-1911149,-999000)) + 
#     ylim(c(-600105, 0))
#   #scale_fill_discrete(type = c("#bf812d","#35978f"))
# )
# 
# ## zoom in on the AZ high elevation area to check out mis-classification
# (regMod_2b_PredMAP_PIPO <- ggplot(temp) +
#     geom_spatraster(data = temp2b) +
#     # geom_point(aes(#Long, Lat, 
#     #   col = newRegion_pred)) + 
#     scale_fill_distiller(type = "div", palette = 1, direction = 1) +
#     #geom_sf(data = ecoregionsSF2, color = "black", fill = NA) + 
#     #lims(x = c(-130, -65), y = c(24, 50))
#     
#     
#     #geom_point(aes(Long, Lat, col = newRegion_pred), alpha = .1) +
#     ggtitle("model-predicted ecoregion classification -- 2 ecoregions; no VPD",
#             subtitle = paste0("Zoom-In on high elevation area in the SW;
#            \n dots indicate misclassification; color indicates true classification \n  ", 
#                               round((100-sum(!temp$goodPred)/nrow(temp) * 100),1), 
#                               "% classification accuracy" )) +
#     geom_point(data = temp[temp$goodPred == FALSE,], aes(Long, Lat#, fill = as.factor(newRegion_predClass)
#     ),
#     col = "black"
#     , shape = 21) +
#     xlim(c(-1211149,-559000)) + 
#     ylim(c(-1050105, -550000))
#   #scale_fill_discrete(type = c("#bf812d","#35978f"))
# )
# 
# ## Look at the relationship between misclassification and elevation
# # get elevation information for each raster cell 
# temp_TEMP <- temp %>% 
#   st_as_sf(coords = c("Long", "Lat"), crs = crs(soilRastTemp))
# 
# temp_elev <- elevatr::get_elev_point(temp2, src = "aws")
# 
# temp_TEMP <- temp_TEMP %>% st_join(temp_elev, left = TRUE, join = st_nearest_feature)
# 
# # within each band of elevation (100 m), what is the frequency of bad predictions
# (badClass_elev <- temp_TEMP %>% 
#     mutate(elevRound = plyr::round_any(elevation, 100)) %>% 
#     st_drop_geometry() %>% 
#     group_by(elevRound, newRegion) %>% 
#     dplyr::summarize(goodPredNumber = sum(goodPred),
#                      badPredNumber = sum(!goodPred),
#                      totalPredNumber = n()) %>% 
#     mutate(badPredRate = badPredNumber/totalPredNumber)
# )
# 
# (badPredElevation_mapA <- ggplot(badClass_elev,aes(x = elevRound, y = badPredRate, col = newRegion)) + 
#     geom_point(data = badClass_elev[badClass_elev$totalPredNumber<100,],
#                aes(x = elevRound, y = badPredRate), col = "black", pch = 8, size = 4) +
#     geom_point() +
#     geom_line() +
#     labs(title = "Misclassification",
#          x = "elevation - m", 
#          y = "Average rate of misclassification", 
#          subtitle = "asterisks indicates elevation bands where there are <100 observations") +
#     theme_minimal() +
#     scale_color_discrete(type = c("#bf812d","#35978f")))
# 
# 
# 
# ## confusion matrix
# regMod_2b_confMat <- caret::confusionMatrix(data = temp$newRegion_predClass,
#                                            reference = temp$newRegionFact)$table
# 
# ## make plots of good and bad probability
# # just good predictions
# (regMod_2b_ternGoodPreds <- temp %>% 
#     filter(goodPred == TRUE) %>% 
#     ggplot() +
#     geom_density(aes(x = newRegion_pred, col = newRegionFact), trim = "FALSE") +
#     theme_minimal() + 
#     labs(color = "true ecoregion") +
#     ggtitle("Accurate Predictions") +
#     xlab("Model-Predicted Ecoregion")+
#     scale_x_continuous(breaks = c(0, .25, .5, .75, 1), 
#                        labels = c("shrub/grass", 0.25, 0.50, 0.75, "forest")) + 
#     scale_color_discrete(type = c("#bf812d","#35978f"))
# )
# # just bad predictions
# badDat <- temp %>% 
#   filter(goodPred == FALSE)
# (regMod_2b_ternBadPreds <- 
#     ggplot(badDat) +
#     geom_density(aes(x = newRegion_pred, col = newRegionFact
#     ), trim = "FALSE") +
#     theme_minimal() + 
#     labs(color = "true ecoregion") +
#     ggtitle("Inaccurate Predictions")+
#     xlab("Model-Predicted Ecoregion") +
#     scale_x_continuous(breaks = c(0, .25, .5, .75, 1), 
#                        labels = c("shrub/grass", 0.25, 0.50, 0.75, "forest")) + 
#     scale_color_discrete(type = c("#bf812d","#35978f")))
# 
# (regMod_2bPred_mapBadGrass <- ggplot() +
#     geom_point(data = temp, aes(Long, Lat, col = newRegion), alpha = .3) +
#     ggtitle("Severity of Grass/Shrub misclassification") +
#     geom_point(data = temp[temp$goodPred == FALSE & temp$newRegion_predClass == "dryShrubGrass",], 
#                aes(Long, Lat, fill = newRegion_pred),
#                col = "black" ,
#                shape = 21) +
#     scale_fill_gradient(high = "#f5f5f5", low = "#543005") +
#     scale_color_discrete(type = c("#bf812d","#35978f")) +
#     theme_minimal()
# )
# 
# (regMod_2bPred_mapForest <- ggplot() +
#     geom_point(data = temp, aes(Long, Lat, col = newRegion), alpha = .1) +
#     ggtitle("Severity of Forest misclassification") +
#     geom_point(data = temp[temp$goodPred == FALSE & temp$newRegion_predClass == "forest",], 
#                aes(Long, Lat, fill = newRegion_pred),
#                col = "black" ,
#                shape = 21) +
#     scale_fill_gradient(high = "#003c30", low = "#f5f5f5") +
#     scale_color_discrete(type = c("#bf812d","#35978f")) +
#     theme_minimal()
# ) 
# 
# 
# ## make variable importance figures
# caret::getModelInfo()
# varImp(testMod_2b)
# beta.table_2b <- data.frame(summary(testMod_2b)$coef)
# beta.table_2b$variable <- row.names(beta.table_2b)
# beta.table_2b <- mutate(beta.table_2, estimate = Estimate, 
#                        se = Std..Error, 
#                        low = Estimate - 2*se, 
#                        high = Estimate + 2*se,
#                        p = Pr...z..)  
# 
# effSizeFig_2b <- ggplot(beta.table_2, aes(y=estimate, x=variable, ymin=low, ymax=high, color = (p < 0.05))) + 
#   #guides(color = guide_legend(NULL))+
#   geom_pointrange() +  
#   geom_hline(yintercept = 0, col="darkgrey", lty = 3, lwd=2) +
#   coord_flip() + theme_minimal() + ggtitle("Effect Sizes")
# 
# ## arrange all of the figures for this model
# ggpubr::annotate_figure(ggarrange(
#   # regMod_2_varImpPlot,
#   ggplotify::as.grob(gt::as_gtable(gt::gt(data.frame("dryShrubGrass" =  data.frame(regMod_2b_confMat)$Freq[1:2], 
#                                                      "Forest" = data.frame(regMod_2b_confMat)$Freq[3:4],
#                                                      row.names = c("dryShrubGrass", "Forest")), 
#                                           rownames_to_stub = TRUE))), 
#   effSizeFig_2b,
#   regMod_2b_PredMAP, 
#   regMod_2b_PredMAP_GB, 
#   regMod_2b_PredMAP_PIPO,
#   #badPredElevation_mapA,
#   regMod_2b_ternGoodPreds,
#   regMod_2b_ternBadPreds,
#   regMod_2bPred_mapBadGrass, 
#   regMod_2bPred_mapForest, 
#   nrow = 6
# )#, 
# #top = c("Model with depth = 7 and all climate and soil predictors")
# ) %>% 
#   ggexport(
#     filename = "./Figures/EcoRegionModelFigures/ModelFigures_WithOUTVPD_TwoEcoregionsSCALEDRegressionModelResults.pdf",
#     width = 15, height = 45)
# 
# 
# 
# # w/out VPD; w/ stepwise model selection: Fit a simple regression w/ two ecoregions w/ interactions ----------------
# 
# # make null model  (use testMod_2 as the full model )
# testMod.null <- glm(formula = newRegionFact ~ 1, family = "binomial", 
#                     data = modDat_fitNew)
# testMod.full <- 
#   glm(data =  modDat_fitNew %>% 
#         dplyr::select(newRegionFact, T_warmestMonth_meanAnnAvg_CLIM, T_coldestMonth_meanAnnAvg_CLIM , precip_wettestMonth_meanAnnAvg_CLIM , 
#                       precip_driestMonth_meanAnnAvg_CLIM , PrecipTempCorr_meanAnnAvg_CLIM , 
#                       isothermality_meanAnnAvg_CLIM , soilDepth , avgSandPerc_acrossDepth , 
#                       avgCoarsePerc_acrossDepth , avgOrganicCarbonPerc_0_3cm )
#       , formula = newRegionFact ~ . + .^2 , family = "binomial", 
#                      na.action = na.fail)
# 
# # stepwiseResults <- stepAIC(testMod.full, scope = list(lower = testMod.null, upper = ~ .^2)
#  #                          , direction="both",test="Chisq", trace = T)
# 
# stepwiseResults_back <- stepAIC(testMod.full, scope = list(lower = testMod.null, upper =  testMod.full)
#                            , direction="backward",test="Chisq", trace = T)
# 
# ## the brute-force method includes 44 interactions! which seems unreasonable... but will proceed to see what happens for now
# summary(stepwiseResults_back)
# 
# # testepwiseResults_back# test the predictions of the model
# predictionDat <- modDat_testNew
# predictionDatTemp <- cbind(predictionDat,"newRegion_pred" = predict(stepwiseResults_back, newdata = predictionDat, "response"))
# 
# # for each observation, determine which category has the highest predicted probability
# (temp <- predictionDatTemp %>%
#     rowwise() %>% 
#     mutate(newRegion_predClass = newRegion_pred) %>% 
#     mutate(newRegion_predClass = replace(newRegion_predClass, newRegion_pred<.5, "dryShrubGrass"), 
#            newRegion_predClass = replace(newRegion_predClass, newRegion_pred>.5, "forest")) 
# )
# 
# temp$newRegion_predClass <- relevel(as.factor(temp$newRegion_predClass), ref = "dryShrubGrass")
# 
# # are these predictions the same as the actual classifications
# temp$goodPred <- temp$newRegionFact == temp$newRegion_predClass
# sum(!temp$goodPred)/nrow(temp) * 100
# 
# ## make predictions rasterized 
# temp2 <- temp %>% 
#   terra::vect(geom = c("Long", "Lat"), crs = crs(soilRastTemp)
#   ) %>% 
#   terra::rasterize(y = 
#                      terra::aggregate(soilRastTemp, fact = 9, fun = mean, na.rm= TRUE)
#                    , field = "newRegion_pred")
# (regMod_2_PredMAP <- ggplot(temp) +
#     geom_spatraster(data = temp2) +
#     # geom_point(aes(#Long, Lat, 
#     #   col = newRegion_pred)) + 
#     scale_fill_distiller(type = "div", palette = 1, direction = 1) +
#     #geom_sf(data = ecoregionsSF2, color = "black", fill = NA) + 
#     #lims(x = c(-130, -65), y = c(24, 50))
#     
#     
#     #geom_point(aes(Long, Lat, col = newRegion_pred), alpha = .1) +
#     ggtitle("model-predicted ecoregion classification -- 2 ecoregions; no VPD",
#             subtitle = paste0("ecoregion ~ temp_warmestMonth + temp_coldestMonth + precip_wettestMonth +
#             precip_driestMonth + precipTempCorr + isothermality + soilDepth + % sand + % coarse + soil carbon 
#             + 44 interactions...
#            \n black dots indicate misclassification; \n  ", 
#                               round((100-sum(!temp$goodPred)/nrow(temp) * 100),1), 
#                               "% classification accuracy" )) +
#     geom_point(data = temp[temp$goodPred == FALSE,], aes(Long, Lat#, fill = as.factor(newRegion_predClass)
#     ),
#     col = "black"
#     , shape = 21) #+ 
#   #scale_fill_discrete(type = c("#bf812d","#35978f"))
# )
# 
# ## confusion matrix
# regMod_2_confMat <- caret::confusionMatrix(data = temp$newRegion_predClass,
#                                            reference = temp$newRegionFact)$table
# 
# ## make plots of good and bad probability
# # just good predictions
# (regMod_2_ternGoodPreds <- temp %>% 
#     filter(goodPred == TRUE) %>% 
#     ggplot() +
#     geom_density(aes(x = newRegion_pred, col = newRegionFact), trim = "FALSE") +
#     theme_minimal() + 
#     labs(color = "true ecoregion") +
#     ggtitle("Accurate Predictions") +
#     xlab("Model-Predicted Ecoregion")+
#     scale_x_continuous(breaks = c(0, .25, .5, .75, 1), 
#                        labels = c("shrub/grass", 0.25, 0.50, 0.75, "forest")) + 
#     scale_color_discrete(type = c("#bf812d","#35978f"))) 
# # just bad predictions
# badDat <- temp %>% 
#   filter(goodPred == FALSE)
# (regMod_2_ternBadPreds <- 
#     ggplot(badDat) +
#     geom_density(aes(x = newRegion_pred, col = newRegionFact
#     ), trim = "FALSE") +
#     theme_minimal() + 
#     labs(color = "true ecoregion") +
#     ggtitle("Inaccurate Predictions")+
#     xlab("Model-Predicted Ecoregion") +
#     scale_x_continuous(breaks = c(0, .25, .5, .75, 1), 
#                        labels = c("shrub/grass", 0.25, 0.50, 0.75, "forest")) + 
#     scale_color_discrete(type = c("#bf812d","#35978f")))
# 
# (regMod_2Pred_mapBadGrass <- ggplot() +
#     geom_point(data = temp, aes(Long, Lat, col = newRegion), alpha = .3) +
#     ggtitle("Severity of Grass/Shrub misclassification") +
#     geom_point(data = temp[temp$goodPred == FALSE & temp$newRegion_predClass == "dryShrubGrass",], 
#                aes(Long, Lat, fill = newRegion_pred),
#                col = "black" ,
#                shape = 21) +
#     scale_fill_gradient(high = "#f5f5f5", low = "#543005") +
#     scale_color_discrete(type = c("#bf812d","#35978f")) +
#     theme_minimal()
# )
# 
# (regMod_2Pred_mapForest <- ggplot() +
#     geom_point(data = temp, aes(Long, Lat, col = newRegion), alpha = .1) +
#     ggtitle("Severity of Forest misclassification") +
#     geom_point(data = temp[temp$goodPred == FALSE & temp$newRegion_predClass == "forest",], 
#                aes(Long, Lat, fill = newRegion_pred),
#                col = "black" ,
#                shape = 21) +
#     scale_fill_gradient(high = "#003c30", low = "#f5f5f5") +
#     scale_color_discrete(type = c("#bf812d","#35978f")) +
#     theme_minimal()
# ) 
# 
# 
# ## make variable importance figures
# beta.table_2 <- data.frame(summary(stepwiseResults_back)$coef)
# beta.table_2$variable <- row.names(beta.table_2)
# beta.table_2 <- mutate(beta.table_2, estimate = Estimate, 
#                        se = Std..Error, 
#                        low = Estimate - 2*se, 
#                        high = Estimate + 2*se,
#                        p = Pr...z..)  
# 
# effSizeFig_2 <- ggplot(beta.table_2, aes(y=estimate, x=variable, ymin=low, ymax=high, color = (p < 0.05))) + 
#   #guides(color = guide_legend(NULL))+
#   geom_pointrange() +  
#   geom_hline(yintercept = 0, col="darkgrey", lty = 3, lwd=2) +
#   coord_flip() + theme_minimal() + ggtitle("Effect Sizes")
# 
# ## make decile plots 
# # reorganize data frame
# # get decile values
# tempDec <- temp %>% 
#   pivot_longer(cols = c(T_warmestMonth_meanAnnAvg_CLIM, T_coldestMonth_meanAnnAvg_CLIM , precip_wettestMonth_meanAnnAvg_CLIM , 
#                                     precip_driestMonth_meanAnnAvg_CLIM , PrecipTempCorr_meanAnnAvg_CLIM , 
#                                     isothermality_meanAnnAvg_CLIM , soilDepth , avgSandPerc_acrossDepth , 
#                                     avgCoarsePerc_acrossDepth , avgOrganicCarbonPerc_0_3cm ), 
#                names_to = "predictor_name",
#                values_to = "predictor_value"
#                 ) %>% 
#   dplyr::select(newRegion_pred, predictor_name, predictor_value)
# tempQuantiles <- tempDec %>% 
#   group_by(predictor_name) %>% 
#   reframe(quantiles = stats::quantile(predictor_value, c(0, .09,.1,.19, .2, .29, .3, .39, .4, .49, 
#                                                          .5, .59, .6, .69, .7, .79, .8, .89, .9, 1)), 
#           quantilesNames = paste0("quant_",c(0, .09,.1,.19, .2, .29, .3, .39, .4, .49, 
#                                     .5, .59, .6, .69, .7, .79, .8, .89, .9, 1))) %>% 
#   pivot_wider( names_from = quantilesNames, 
#               values_from = quantiles)
# 
# test <- tempDec %>% 
#   left_join(tempQuantiles) %>% 
#   mutate(#YesQuant0 = dplyr::between(predictor_value, left = quant_0, right = quant_0.09),
#          YesQuant1 = dplyr::between(predictor_value, left = quant_0.1 , right = quant_0.19),
#          YesQuant2 = dplyr::between(predictor_value, left = quant_0.2 , right = quant_0.29),
#          YesQuant3 = dplyr::between(predictor_value, left = quant_0.3 , right = quant_0.39),
#          YesQuant4 = dplyr::between(predictor_value, left = quant_0.4 , right = quant_0.49),
#          YesQuant5 = dplyr::between(predictor_value, left = quant_0.5 , right = quant_0.59),
#          YesQuant6 = dplyr::between(predictor_value, left = quant_0.6 , right = quant_0.69),
#          YesQuant7 = dplyr::between(predictor_value, left = quant_0.7 , right = quant_0.79),
#          YesQuant8 = dplyr::between(predictor_value, left = quant_0.8 , right = quant_0.89),
#          YesQuant9 = dplyr::between(predictor_value, left = quant_0.9 , right = quant_1)) %>% 
#   mutate(#YesQuant0 = case_match(YesQuant0, TRUE ~ quant_0),
#          YesQuant1 = case_match(YesQuant1, TRUE ~ quant_0.1),
#          YesQuant2 = case_match(YesQuant2, TRUE ~ quant_0.2),
#          YesQuant3 = case_match(YesQuant3, TRUE ~ quant_0.3),
#          YesQuant4 = case_match(YesQuant4, TRUE ~ quant_0.4),
#          YesQuant5 = case_match(YesQuant5, TRUE ~ quant_0.5),
#          YesQuant6 = case_match(YesQuant6, TRUE ~ quant_0.6),
#          YesQuant7 = case_match(YesQuant7, TRUE ~ quant_0.7),
#          YesQuant8 = case_match(YesQuant8, TRUE ~ quant_0.8),
#          YesQuant9 = case_match(YesQuant9, TRUE ~ quant_0.9)) %>% 
#   #slice(1:1000) %>% 
#   rowwise() %>% 
#   mutate(Quantile_value = round(sum(na.omit(YesQuant1), na.omit(YesQuant2), na.omit(YesQuant3),
#                               na.omit(YesQuant4), na.omit(YesQuant5), na.omit(YesQuant6), na.omit(YesQuant7),
#                               na.omit(YesQuant8), na.omit(YesQuant9)), 5)) %>% 
#   dplyr::select(-(quant_1:YesQuant9)) %>% 
#   group_by(predictor_name, Quantile_value) %>% 
#   summarize(newRegion_pred_mean = mean(newRegion_pred)) 
# 
# # make a plot 
# (quantPlot <- ggplot(data = test) + 
#   facet_wrap(~predictor_name, scales = "free_x") +
#   geom_line(aes(x = Quantile_value, y = newRegion_pred_mean, col = predictor_name)) +
#   geom_point(aes(x = Quantile_value, y = newRegion_pred_mean, col = predictor_name)) + 
#   theme_minimal() + 
#   labs(y = "mean predicted probability w/in a decile", x = "predictor value") + 
#   scale_color_discrete(guide = "none"))
# 
# ## arrange all of the figures for this model
# ggpubr::annotate_figure(ggarrange(
#   # regMod_2_varImpPlot,
#   ggplotify::as.grob(gt::as_gtable(gt::gt(data.frame("dryShrubGrass" =  data.frame(regMod_2_confMat)$Freq[1:2], 
#                                                      "Forest" = data.frame(regMod_2_confMat)$Freq[3:4],
#                                                      row.names = c("dryShrubGrass", "Forest")), 
#                                           rownames_to_stub = TRUE))), 
#   effSizeFig_2,
#   quantPlot,
#   regMod_2_PredMAP, 
#   regMod_2_ternGoodPreds,
#   regMod_2_ternBadPreds,
#   regMod_2Pred_mapBadGrass, 
#   regMod_2Pred_mapForest, 
#   nrow = 6
# )#, 
# #top = c("Model with depth = 7 and all climate and soil predictors")
# ) %>% 
#   ggexport(
#     filename = "./Figures/EcoRegionModelFigures/ModelFigures_WithOUTVPD_WithALLInteractions_TwoEcoregionsRegressionModelResults.pdf",
#     width = 15, height = 40)
# 
# 
