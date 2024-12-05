#///////////////////
# Classifying locations across the US into ecoregions using Random Forest 
# Alice Stears
# 7/30/24
#///////////////////

# Load Packages -----------------------------------------------------------
library(tidyverse)
library(sf)
library(nnet)

# Load Data ---------------------------------------------------------------
# data ready for modeling 
modDat <- readRDS("./Data_processed/CoverData/DataForModels_spatiallyAveraged_withSoils_noSf.rds")

# Lump Ecoregions ---------------------------------------------------------
# (eastern forest) put eastern temperate forests, tropical wet forests and northern forests together
# (dry shrub and grass) put great plains, southern semiarid highlands, Mediterranean California and north american deserts together 
# (western forest) put northwestern forested mountains, marine west coast forests together, and temperate sierras together

modDat_fit <- modDat  %>% 
  mutate(newRegion = as.factor(newRegion)) %>% 
  dplyr::select(newRegion, swe_meanAnnAvg_30yr , annWetDegDays_meanAnnAvg_30yr , 
            tmean_meanAnnAvg_30yr , isothermality_meanAnnAvg_30yr , 
          prcp_meanAnnTotal_30yr , PrecipTempCorr_meanAnnAvg_30yr, 
         soilDepth                  , surfaceClay_perc          ,                
         avgSandPerc_acrossDepth    ,  avgCoarsePerc_acrossDepth,                 
         avgOrganicCarbonPerc_0_3cm ,  totalAvailableWaterHoldingCapacity, Long, Lat) %>% 
  drop_na() %>% 
  st_drop_geometry() %>% 
  slice_sample(n = 50000, replace = FALSE)

modDat_test <- modDat %>% 
  mutate(newRegion = as.factor(newRegion)) %>% 
  dplyr::select(newRegion, swe_meanAnnAvg_30yr , annWetDegDays_meanAnnAvg_30yr , 
         tmean_meanAnnAvg_30yr , isothermality_meanAnnAvg_30yr , 
         prcp_meanAnnTotal_30yr , PrecipTempCorr_meanAnnAvg_30yr, 
         
         soilDepth                  , surfaceClay_perc          ,                
         avgSandPerc_acrossDepth    ,  avgCoarsePerc_acrossDepth,                 
         avgOrganicCarbonPerc_0_3cm ,  totalAvailableWaterHoldingCapacity,
         Long, Lat) %>% 
  drop_na() %>%
  anti_join(modDat_fit)

# Fit a simple regression (multinomial log-normal regression) ------------------------
# following this example
modDat_fit$newRegionFact <- relevel(modDat_fit$newRegion, ref = "dryShrubGrass")
testMod <- nnet::multinom(newRegionFact ~  swe_meanAnnAvg_30yr + annWetDegDays_meanAnnAvg_30yr + 
                    tmean_meanAnnAvg_30yr + isothermality_meanAnnAvg_30yr + prcp_meanAnnTotal_30yr + 
                      PrecipTempCorr_meanAnnAvg_30yr +  soilDepth + surfaceClay_perc +             
                    avgSandPerc_acrossDepth + avgCoarsePerc_acrossDepth +              
                    avgOrganicCarbonPerc_0_3cm + totalAvailableWaterHoldingCapacity,
                    data = modDat_fit
                  )

summary(testMod)

head(pp <- fitted(testMod))

# test the predictions of the model
predictionDat <- modDat_test[,c("newRegion"                     , "swe_meanAnnAvg_30yr"           , "annWetDegDays_meanAnnAvg_30yr",     
                                "tmean_meanAnnAvg_30yr"         , "isothermality_meanAnnAvg_30yr" , "prcp_meanAnnTotal_30yr"       ,     
                                "PrecipTempCorr_meanAnnAvg_30yr", "soilDepth"                     , "surfaceClay_perc"             ,     
                                 "avgSandPerc_acrossDepth"      ,  "avgCoarsePerc_acrossDepth"    ,  "avgOrganicCarbonPerc_0_3cm"  ,      
                                 "totalAvailableWaterHoldingCapacity"
)]
predictionDatTemp <- cbind(predictionDat, predict(testMod, newdata = predictionDat, "probs"))

# for each observation, determine which category has the highest predicted probability
(temp <- predictionDatTemp %>% 
  mutate(newRegion_Fact = as.factor(newRegion),
    newRegion_predicted = pmap_dbl(.[c("dryShrubGrass", "eastForest", "westForest")], max), 
         across(dryShrubGrass:westForest, ~.x == newRegion_predicted)) %>% 
  mutate(dryShrubGrass = str_replace(dryShrubGrass, 'TRUE', 'dryShrubGrass'), 
         eastForest = str_replace(eastForest, 'TRUE', "eastForest"), 
         westForest = str_replace(westForest, 'TRUE', "westForest"),
         dryShrubGrass = str_replace(dryShrubGrass, 'FALSE', ""), 
         eastForest = str_replace(eastForest, 'FALSE', ""), 
         westForest = str_replace(westForest, 'FALSE', "")) %>% 
    mutate(, 
           newRegion_group = as.factor(pmap_chr(.[c("dryShrubGrass", "eastForest", "westForest")], paste0))) %>% 
    cbind(modDat_test %>% 
            dplyr::select(Long, Lat)))
# are these predictions the same as the actual classifications
temp$goodPred <- temp$newRegion_Fact == temp$newRegion_group
# ggplot(temp) + 
#   geom_point(aes(Long, Lat, col = goodPred), alpha = .5) + 
#   ggtitle("Locations of incorrect classifications")
# ggplot(temp) + 
#   geom_point(aes(Long, Lat, col = newRegionFact), alpha = .5) + 
#   ggtitle("true ecoregion classification")
ggplot(temp) + 
  geom_point(aes(Long, Lat, col = newRegion_group), alpha = .5) + 
  ggtitle("model-predicted ecoregion classification", 
          subtitle = paste0("black dots indicate misclassification; ", round((100-sum(!temp$goodPred)/nrow(temp) * 100),1), "% classification accuracy" )) + 
  geom_point(data = temp[temp$goodPred == FALSE,], aes(Long, Lat, col = goodPred), col = "black")

## the predictions are ok, but not great 



# Classification Tree -----------------------------------------------------
## first, try using rpart R package
#https://www.datacamp.com/doc/r/cart
#https://cran.r-project.org/web/packages/party/vignettes/party.pdf
library(rpart)
# fit a basic classification tree model
treeMod <- rpart(newRegionFact ~  swe_meanAnnAvg_30yr + annWetDegDays_meanAnnAvg_30yr + 
                   tmean_meanAnnAvg_30yr + isothermality_meanAnnAvg_30yr + prcp_meanAnnTotal_30yr + 
                   PrecipTempCorr_meanAnnAvg_30yr +  soilDepth + surfaceClay_perc +             
                   avgSandPerc_acrossDepth + avgCoarsePerc_acrossDepth +              
                   avgOrganicCarbonPerc_0_3cm + totalAvailableWaterHoldingCapacity,
                 data = modDat_fit, method = 'class')
print(treeMod)

# prune the tree
# first, find the cross-validation error
printcp(treeMod)
treeMod_p <- prune(treeMod, cp = .01)
print(treeMod_p)
summary(treeMod_p)
par(xpd = TRUE)
plot(treeMod_p, compress = TRUE)
text(treeMod_p, use.n = TRUE)

## now, try using party r package (conditional inference, nonparametric regression tree)
library(party)
partyTree <- ctree(newRegionFact ~  swe_meanAnnAvg_30yr + annWetDegDays_meanAnnAvg_30yr + 
                     tmean_meanAnnAvg_30yr + isothermality_meanAnnAvg_30yr + prcp_meanAnnTotal_30yr + 
                     PrecipTempCorr_meanAnnAvg_30yr +  soilDepth + surfaceClay_perc +             
                     avgSandPerc_acrossDepth + avgCoarsePerc_acrossDepth +              
                     avgOrganicCarbonPerc_0_3cm + totalAvailableWaterHoldingCapacity,
                   data = modDat_fit)
# try restricting the tree depth (too long right now)
partyTree <- ctree(newRegionFact ~  swe_meanAnnAvg_30yr + annWetDegDays_meanAnnAvg_30yr + 
                     tmean_meanAnnAvg_30yr + isothermality_meanAnnAvg_30yr + prcp_meanAnnTotal_30yr + 
                     PrecipTempCorr_meanAnnAvg_30yr +  soilDepth + surfaceClay_perc +             
                     avgSandPerc_acrossDepth + avgCoarsePerc_acrossDepth +              
                     avgOrganicCarbonPerc_0_3cm + totalAvailableWaterHoldingCapacity,
                   data = modDat_fit, controls = ctree_control(maxdepth = 7))
partyTree
plot(partyTree, main="Conditional Inference Tree for ecoregions")
# predict categories based on this simple model
partyTreePreds <- cbind(predictionDat, "partyPrediction" = Predict(partyTree, newdata = predictionDat)) %>% 
  cbind(modDat_test %>% 
          dplyr::select(Long, Lat))
partyTreePreds$goodPred <- partyTreePreds$newRegion == partyTreePreds$partyPrediction
# percentage of misclassifications
sum(!partyTreePreds$goodPred)/nrow(partyTreePreds) * 100

ggplot(partyTreePreds) + 
  geom_point(aes(Long, Lat, col = partyPrediction), alpha = .5) + 
  ggtitle("model-predicted ecoregion classification", 
          subtitle = paste0("black dots indicate misclassification; ", round((100-sum(!partyTreePreds$goodPred)/nrow(partyTreePreds) * 100),1), "% classification accuracy" )) + 
  geom_point(data = partyTreePreds[partyTreePreds$goodPred == FALSE,], aes(Long, Lat, col = goodPred), col = "black")
 

