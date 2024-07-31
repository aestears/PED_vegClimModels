#///////////////////
# Classifying locations across the US into ecoregions using Random Forest 
# Alice Stears
# 7/30/24
#///////////////////


# Load Packages -----------------------------------------------------------
library(tidyverse)
library(sf)
library(ranger)

# Load Data ---------------------------------------------------------------
# data ready for modeling 
modDat <- readRDS("./data/DataForModels_withEcoregion.rds")

# Lump Ecoregions ---------------------------------------------------------
# (eastern forest) put eastern temperate forests, tropical wet forests and northern forests together
# (dry shrub and grass) put great plains, southern semiarid highlands, Mediterranean California and north american deserts together 
# (western forest) put northwestern forested mountains, marine west coast forests together, and temperate sierras together

# make a lookup table
ecoReg_lu <- data.frame("NA_L1NAME" = unique(modDat$NA_L1NAME), "newRegion" = c("westForest", "dryShrubGrass", "westForest", "dryShrubGrass", "dryShrubGrass", "dryShrubGrass",
                                                                             "westForest", NA, "eastForest", "eastForest", "eastForest", NA))
# add to main data.frame 
modDat_fit <- modDat %>% left_join(ecoReg_lu) %>% 
  mutate(newRegion = as.factor(newRegion)) %>% 
  select(newRegion, swe_meanAnnAvg_30yr , annWetDegDays_meanAnnAvg_30yr , 
            tmean_meanAnnAvg_30yr , isothermality_meanAnnAvg_30yr , 
          prcp_meanAnnTotal_30yr , PrecipTempCorr_meanAnnAvg_30yr) %>% 
  drop_na() %>% 
  st_drop_geometry() %>% 
  slice_sample(n = 50000, replace = FALSE)

modDat_test <-modDat %>% left_join(ecoReg_lu) %>% 
  mutate(newRegion = as.factor(newRegion)) %>% 
  select(newRegion, swe_meanAnnAvg_30yr , annWetDegDays_meanAnnAvg_30yr , 
         tmean_meanAnnAvg_30yr , isothermality_meanAnnAvg_30yr , 
         prcp_meanAnnTotal_30yr , PrecipTempCorr_meanAnnAvg_30yr) %>% 
  drop_na() %>% 
  anti_join(modDat_fit)
# # get ecoregion shapefiles 
# ecoReg <- st_read("./data/Level1Ecoregions/", layer = "NA_CEC_Eco_Level1")
# ecoReg_1 <- ecoReg %>% left_join(ecoReg_lu)
# ggplot(ecoReg_1) + 
#   geom_sf(aes(col = newRegion))


# Fit Basic Random forest classification algorithm ------------------------
set.seed(12011993)
testMod <- ranger(newRegion ~  swe_meanAnnAvg_30yr + annWetDegDays_meanAnnAvg_30yr + 
                    tmean_meanAnnAvg_30yr + isothermality_meanAnnAvg_30yr + prcp_meanAnnTotal_30yr + PrecipTempCorr_meanAnnAvg_30yr
                    , data = modDat_fit, write.forest = TRUE, 
                  importance = "permutation",
                  #scale.permutation.importance = TRUE
                  )


# test goodness of fit ----------------------------------------------------
# get the model confusion matrix
print(testMod$confusion.matrix)
#                       predicted
# true            dryShrubGrass eastForest westForest
# dryShrubGrass         24912         84        672
# eastForest               35      10786          0
# westForest              941          1      12569

# look at variable importance
head(importance_pvalues(testMod, formula = newRegion ~  swe_meanAnnAvg_30yr + annWetDegDays_meanAnnAvg_30yr + 
                          tmean_meanAnnAvg_30yr + isothermality_meanAnnAvg_30yr + prcp_meanAnnTotal_30yr + 
                          PrecipTempCorr_meanAnnAvg_30yr, 
                        data = modDat_1, method = "altmann"))
# output: 
#                                importance     pvalue
# swe_meanAnnAvg_30yr             0.1651012 0.00990099
# annWetDegDays_meanAnnAvg_30yr   0.2189938 0.00990099
# tmean_meanAnnAvg_30yr           0.1736015 0.00990099
# isothermality_meanAnnAvg_30yr   0.1384066 0.00990099
# prcp_meanAnnTotal_30yr          0.3283871 0.00990099
# PrecipTempCorr_meanAnnAvg_30yr  0.2304628 0.00990099

# get predictions from the model

  preds <- predict(testMod, data=st_drop_geometry(modDat_test))

# look at the spatial distribution of the predictions 
modPreds <- modDat_test 
modPreds$newRegion_pred <- preds$predictions
modPreds$BadPred<- ifelse(modPreds$newRegion == modPreds$newRegion_pred, NA, TRUE)

# see where the forest is predicting incorrectly
ggplot() + 
  geom_sf(data = modPreds, aes(col = newRegion_pred), alpha = .3, pch = 20) + 
  geom_sf(data = modPreds[modPreds$BadPred == TRUE,], aes(#col = newRegion_pred
                                                          ), pch = 20, col = "black")


saveRDS(testMod, file = "./Analysis/VegComposition/ModelFitting/models/randForestClassification.rds")

# Get Model  --------------------------------------------------------------
treeMod <- treeInfo(testMod)

# ## can I reconstruct the decision tree? 
# for (i in 1:100000){#nrow(modDat_test)) {
#   dat <- modDat_test[i,]
#   # node 0
#   if (dat$swe_meanAnnAvg_30yr <= 1.924646e+01) {
#     # node 1
#     if (dat$tmean_meanAnnAvg_30yr <= 6.712225e+00) {
#       # node 3
#       if (dat$tmean_meanAnnAvg_30yr <= 5.686742e+00 ) {
#         # node 7
#         if (dat$swe_meanAnnAvg_30yr <= 1.288767e-02) {
#           # node 15
#           if (dat$PrecipTempCorr_meanAnnAvg_30yr <= 1.384537e-01) {
#             # node 31
#           } else {
#             # node 32
#           }
#         } else {
#           # node 16
#           if (dat$PrecipTempCorr_meanAnnAvg_30yr <=  -2.662799e-01) {
#             # node 33
#           } else {
#             # node 34
#           }
#         }
#       } else {
#         # node 8
#         if (dat$swe_meanAnnAvg_30yr <= 2.154517e+00) {
#           # node 17
#           if (dat$PrecipTempCorr_meanAnnAvg_30yr <= -3.362111e-01 ) {
#             # node 35
#           } else {
#             # node 36
#           }
#         } else {
#           # node 18
#           if (dat$prcp_meanAnnTotal_30yr <= 6.616515e+02) {
#             # node 37
#           } else {
#             # node 38
#           }
#         }
#       }
#     } else {
#       # node 4
#       if (dat$prcp_meanAnnTotal_30yr  <= 4.419938e+02 ) {
#         # node 9
#         if (dat$prcp_meanAnnTotal_30yr <= 3.616855e+02) {
#           # node 19
#           if (dat$PrecipTempCorr_meanAnnAvg_30yr <= 2.898600e-01) {
#             # node 39
#           } else {
#             # node 40
#           }
#         } else {
#           # node 20
#           if (dat$PrecipTempCorr_meanAnnAvg_30yr <= 3.719435e-01) {
#             # node 41
#           } else {
#             # node 42
#           }
#         }
#       } else {
#         # node 10
#         if (dat$prcp_meanAnnTotal_30yr <= 1.128762e+03 ) {
#           # node 21
#           if (dat$PrecipTempCorr_meanAnnAvg_30yr <= 7.278897e-02) {
#             # node 43
#           } else {
#             # node 44
#           }
#         } else {
#           # node 22
#           if (dat$PrecipTempCorr_meanAnnAvg_30yr <= -4.500292e-01){
#             # node 45
#           } else {
#             # node 46
#           }
#         }
#       }
#     }
#   } else {
#     # node 2
#     if (dat$isothermality_meanAnnAvg_30yr <= -2.949601e+01 ) {
#       # node 5
#       if (dat$swe_meanAnnAvg_30yr <=  4.628927e+01) {
#         # node 11
#         if (dat$isothermality_meanAnnAvg_30yr <=  -3.173486e+01) {
#           # node 23
#           if (dat$prcp_meanAnnTotal_30yr <= 5.322318e+02) {
#             # node 47
#           } else {
#             # node 48
#           }
#         } else {
#           # node 24
#           if (dat$PrecipTempCorr_meanAnnAvg_30yr <= -2.553431e-02){
#             # node 49
#           } else {
#             # node 50
#           }
#         }
#       } else {
#         # node 12
#         if (dat$PrecipTempCorr_meanAnnAvg_30yr <= 7.727684e-02 ) {
#           # node 25
#           if (dat$swe_meanAnnAvg_30yr <=  7.067461e+01) {
#             # node 51
#           } else {
#             # node 52
#           }
#         } else {
#           # node 26
#           if (dat$isothermality_meanAnnAvg_30yr <= -3.178565e+01) {
#             # node 53
#           } else {
#             # node 54
#           }
#         }
#       }
#     } else {
#       #  node 6
#       if (dat$PrecipTempCorr_meanAnnAvg_30yr <= -1.712363e-01) {
#         # node 13
#         if (dat$prcp_meanAnnTotal_30yr <= 7.168480e+02) {
#           # node 27
#           if (dat$PrecipTempCorr_meanAnnAvg_30yr <= -2.827414e-01) {
#             # node 55
#           } else {
#             # node 56
#           }
#         } else {
#           # node 28
#           if (dat$tmean_meanAnnAvg_30yr <= 5.162721e+00) {
#             # node 57
#           } else {
#             # node 58
#           }
#         }
#       } else {
#         # node 14
#         if (dat$tmean_meanAnnAvg_30yr <= 4.754435e+00) {
#           # node 29
#           print(c(as.character(dat$newRegion), "westForest"))
#         } 
#         else {
#           # node 30
#         }
#       }
#     }
#   }
# }

