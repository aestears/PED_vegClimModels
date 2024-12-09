#///////////////////
# Classifying locations across the US into ecoregions using Random Forest 
# Alice Stears
# 7/30/24
#///////////////////

# Load Packages -----------------------------------------------------------
library(tidyverse)
library(sf)
library(nnet)
library(caret)
library(GGally) # for ggpairs()
library(ipred)

# Load Data ---------------------------------------------------------------
# data ready for modeling 
modDat <- readRDS("./Data_processed/CoverData/DataForModels_spatiallyAveraged_withSoils_noSf.rds")

# Prepare data for model-fitting  ----------------------------------------------
# (eastern forest) put eastern temperate forests, tropical wet forests and northern forests together
# (dry shrub and grass) put great plains, southern semiarid highlands, Mediterranean California and north american deserts together 
# (western forest) put northwestern forested mountains, marine west coast forests together, and temperate sierras together
set.seed(12011993)
default_idx = createDataPartition(modDat$newRegion, p = 0.8, list = FALSE)
modDat_fit <- modDat[default_idx, ] %>% 
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

modDat_test <-  modDat[-default_idx, ] %>% 
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

(corrPlot <- 
    modDat_fit %>% 
    select(c("swe_meanAnnAvg_30yr","annWetDegDays_meanAnnAvg_30yr",     
             "tmean_meanAnnAvg_30yr" ,"isothermality_meanAnnAvg_30yr", "prcp_meanAnnTotal_30yr" ,      
             "PrecipTempCorr_meanAnnAvg_30yr" ,"soilDepth" ,      "surfaceClay_perc"            ,      
              "avgSandPerc_acrossDepth" , "avgCoarsePerc_acrossDepth"   ,       "avgOrganicCarbonPerc_0_3cm" ,       
              "totalAvailableWaterHoldingCapacity")) %>% 
    rename("swe" = swe_meanAnnAvg_30yr,  "tmean" = tmean_meanAnnAvg_30yr,
            "prcp" = prcp_meanAnnTotal_30yr, 
           "prcp \n TempCorr" = PrecipTempCorr_meanAnnAvg_30yr,  "isothermality" = isothermality_meanAnnAvg_30yr,
           "Wet \n DegDays" = annWetDegDays_meanAnnAvg_30yr) %>% 
    slice_sample(n = 5e4) %>% 
    #select(-matches("_")) %>% 
    ggpairs( upper = list(continuous = my_fn), lower = list(continuous = GGally::wrap("points", alpha = 0.1, size=0.2)), progress = FALSE))

## remove Soil Depth, since it's highly correlated w/ total available water holding capacity

# Fit a simple regression (multinomial log-normal regression) 
# # I think the classification algorithm would be more straightforward to implement?? 
# # following this example: 
# # https://stats.oarc.ucla.edu/r/dae/multinomial-logistic-regression/
# modDat_fit$newRegionFact <- relevel(modDat_fit$newRegion, ref = "dryShrubGrass")
# testMod <- nnet::multinom(newRegionFact ~  swe_meanAnnAvg_30yr + annWetDegDays_meanAnnAvg_30yr + 
#                     tmean_meanAnnAvg_30yr + isothermality_meanAnnAvg_30yr + prcp_meanAnnTotal_30yr + 
#                       PrecipTempCorr_meanAnnAvg_30yr +  soilDepth + surfaceClay_perc +             
#                     avgSandPerc_acrossDepth + avgCoarsePerc_acrossDepth +              
#                     avgOrganicCarbonPerc_0_3cm + totalAvailableWaterHoldingCapacity,
#                     data = modDat_fit
#                   )
# 
# summary(testMod)
# 
# head(pp <- fitted(testMod))
# 
# # get variable importance
# # first get z scores
# z <- summary(testMod)$coefficients/summary(testMod)$standard.errors
# z
# # then do a 2-tailed z test
# p <- (1 - pnorm(abs(z), 0, 1)) * 2
# p

# test the predictions of the model
predictionDat <- modDat_test[,c("newRegion"                     , "swe_meanAnnAvg_30yr"           , "annWetDegDays_meanAnnAvg_30yr",     
                                "tmean_meanAnnAvg_30yr"         , "isothermality_meanAnnAvg_30yr" , "prcp_meanAnnTotal_30yr"       ,     
                                "PrecipTempCorr_meanAnnAvg_30yr" , "surfaceClay_perc"             ,     
                                 "avgSandPerc_acrossDepth"      ,  "avgCoarsePerc_acrossDepth"    ,  "avgOrganicCarbonPerc_0_3cm"  ,      
                                 "totalAvailableWaterHoldingCapacity"
)]
# predictionDatTemp <- cbind(predictionDat, predict(testMod, newdata = predictionDat, "probs"))
# 
# # for each observation, determine which category has the highest predicted probability
# (temp <- predictionDatTemp %>% 
#   mutate(newRegion_Fact = as.factor(newRegion),
#     newRegion_predicted = pmap_dbl(.[c("dryShrubGrass", "eastForest", "westForest")], max), 
#          across(dryShrubGrass:westForest, ~.x == newRegion_predicted)) %>% 
#   mutate(dryShrubGrass = str_replace(dryShrubGrass, 'TRUE', 'dryShrubGrass'), 
#          eastForest = str_replace(eastForest, 'TRUE', "eastForest"), 
#          westForest = str_replace(westForest, 'TRUE', "westForest"),
#          dryShrubGrass = str_replace(dryShrubGrass, 'FALSE', ""), 
#          eastForest = str_replace(eastForest, 'FALSE', ""), 
#          westForest = str_replace(westForest, 'FALSE', "")) %>% 
#     mutate(
#            newRegion_group = as.factor(pmap_chr(.[c("dryShrubGrass", "eastForest", "westForest")], paste0))) %>% 
#     cbind(modDat_test %>% 
#             dplyr::select(Long, Lat)))
# # are these predictions the same as the actual classifications
# temp$goodPred <- temp$newRegion_Fact == temp$newRegion_group
# ggplot(temp) +
#   geom_point(aes(Long, Lat, col = goodPred), alpha = .5) +
#   ggtitle("Locations of incorrect classifications")
# ggplot(temp) +
#   geom_point(aes(Long, Lat, col = newRegion_Fact), alpha = .5) +
#   ggtitle("true ecoregion classification")
# ggplot(temp) + 
#   geom_point(aes(Long, Lat, col = newRegion_group), alpha = .5) + 
#   ggtitle("model-predicted ecoregion classification", 
#           subtitle = paste0("black dots indicate misclassification; ", round((100-sum(!temp$goodPred)/nrow(temp) * 100),1), "% classification accuracy" )) + 
# geom_point(data = temp[temp$goodPred == FALSE,], aes(Long, Lat, fill = newRegion_group), 
#            col = "black"
#            , shape = 21)
# ## the predictions are ok, but not great 




# Fit single classification tree  ----------------------------------------------
## first, try using rpart R package
#https://www.datacamp.com/doc/r/cart
#http://cran.nexr.com/web/packages/partykit/vignettes/partykit.pdf

library(rpart)
# fit a basic classification tree model
treeMod <- rpart(newRegionFact ~  swe_meanAnnAvg_30yr + annWetDegDays_meanAnnAvg_30yr + 
                   tmean_meanAnnAvg_30yr + isothermality_meanAnnAvg_30yr + prcp_meanAnnTotal_30yr + 
                   PrecipTempCorr_meanAnnAvg_30yr + surfaceClay_perc +             
                   avgSandPerc_acrossDepth + avgCoarsePerc_acrossDepth +              
                   avgOrganicCarbonPerc_0_3cm + totalAvailableWaterHoldingCapacity,
                 data = modDat_fit, method = 'class')
print(treeMod)
# look at variable importance
# variable importance
varImp_temp <- data.frame("variable" = names(sort((treeMod$variable.importance), decreasing = TRUE)), 
                          "importance" = sort((treeMod$variable.importance), decreasing = TRUE))
varImp <- varImp_temp  %>% 
  # tibble::rownames_to_column() %>% 
  # dplyr::rename("variable" = rowname) %>% 
  dplyr::arrange(importance) %>%
  dplyr::mutate(variable = forcats::fct_inorder(variable))
ggplot(varImp) + 
  geom_segment(aes(x = variable, y = 0, xend = variable, yend = importance), 
               size = 1.5, alpha = 0.7) +
  geom_point(aes(x = variable, y = importance, col = variable), 
             size = 4, show.legend = F) +
  coord_flip() +
  theme_bw()

# look at cross-validation error 
plotcp(treeMod)
treeMod$cptable

# prune the tree
# tune typer-parameters
hyper_grid <- expand.grid(
  minsplit = seq(5, 15, 1),
  maxdepth = seq(5, 15, 1)
)
head(hyper_grid)
# total number of combinations
nrow(hyper_grid)
## [1] 121

# fit different models to find the ideal set of hyperparameters
models <- list()

for (i in 1:nrow(hyper_grid)) {
  
  # get minsplit, maxdepth values at row i
  minsplit <- hyper_grid$minsplit[i]
  maxdepth <- hyper_grid$maxdepth[i]
  
  # train a model and store in the list
  models[[i]] <- rpart(
    newRegionFact ~  swe_meanAnnAvg_30yr + annWetDegDays_meanAnnAvg_30yr + 
      tmean_meanAnnAvg_30yr + isothermality_meanAnnAvg_30yr + prcp_meanAnnTotal_30yr + 
      PrecipTempCorr_meanAnnAvg_30yr + surfaceClay_perc +             
      avgSandPerc_acrossDepth + avgCoarsePerc_acrossDepth +              
      avgOrganicCarbonPerc_0_3cm + totalAvailableWaterHoldingCapacity,
    data = modDat_fit, method = 'class',
    control = list(minsplit = minsplit, maxdepth = maxdepth)
  )
}

# function to get optimal cp
get_cp <- function(x) {
  min    <- which.min(x$cptable[, "xerror"])
  cp <- x$cptable[min, "CP"] 
}

# function to get minimum error
get_min_error <- function(x) {
  min    <- which.min(x$cptable[, "xerror"])
  xerror <- x$cptable[min, "xerror"] 
}

bestParams <- hyper_grid %>%
  mutate(
    cp    = purrr::map_dbl(models, get_cp),
    error = purrr::map_dbl(models, get_min_error)
  ) %>%
  arrange(error) %>%
  top_n(-5, wt = error)
# minsplit maxdepth   cp     error
# 1       15        8 0.01 0.2303705

## fit the best tree
bestTree <- rpart(
  newRegionFact ~  swe_meanAnnAvg_30yr + annWetDegDays_meanAnnAvg_30yr + 
    tmean_meanAnnAvg_30yr + isothermality_meanAnnAvg_30yr + prcp_meanAnnTotal_30yr + 
    PrecipTempCorr_meanAnnAvg_30yr + surfaceClay_perc +             
    avgSandPerc_acrossDepth + avgCoarsePerc_acrossDepth +              
    avgOrganicCarbonPerc_0_3cm + totalAvailableWaterHoldingCapacity,
  data = modDat_fit, method = 'class',
  control = list(minsplit = 15, maxdepth = 8)
)


# predict categories based on this single 'best tree' 
(treePreds <- predictionDat %>% 
    cbind( "treePrediction" = predict(bestTree, newdata = predictionDat)) %>% 
    mutate(newRegion_Fact = as.factor(newRegion),
           newRegion_predicted = pmap_dbl(.[c("treePrediction.dryShrubGrass", 
                                              "treePrediction.eastForest", 
                                              "treePrediction.westForest")], max), 
           across(treePrediction.dryShrubGrass:treePrediction.westForest, ~.x == newRegion_predicted)) %>% 
    mutate(treePrediction.dryShrubGrass = str_replace(treePrediction.dryShrubGrass, 'TRUE', 'dryShrubGrass'), 
           treePrediction.eastForest = str_replace(treePrediction.eastForest, 'TRUE', "eastForest"), 
           treePrediction.westForest = str_replace(treePrediction.westForest, 'TRUE', "westForest"),
           treePrediction.dryShrubGrass = str_replace(treePrediction.dryShrubGrass, 'FALSE', ""), 
           treePrediction.eastForest = str_replace(treePrediction.eastForest, 'FALSE', ""), 
           treePrediction.westForest = str_replace(treePrediction.westForest, 'FALSE', "")) %>% 
    mutate(
      newRegion_group = as.factor(pmap_chr(.[c("treePrediction.dryShrubGrass", "treePrediction.eastForest", 
                                               "treePrediction.westForest")], paste0))) %>% 
    cbind(modDat_test %>% 
            dplyr::select(Long, Lat)))
# 
treePreds$goodPred <- treePreds$newRegion_Fact == treePreds$newRegion_group
# percentage of misclassifications
sum(!treePreds$goodPred)/nrow(treePreds) * 100
ggplot(treePreds) + 
  geom_point(aes(Long, Lat, col = newRegion_group), alpha = .1) + 
  ggtitle("model-predicted ecoregion classification", 
          subtitle = paste0("black dots indicate misclassification, \n fill color indicates which ecoregion a point was misclassified as;\n  ",
                            round((100-sum(!treePreds$goodPred)/nrow(treePreds) 
                                          * 100),1), "% classification accuracy" )) + 
  geom_point(data = treePreds[treePreds$goodPred == FALSE,], aes(Long, Lat, fill = newRegion_group), 
             col = "black"
             , shape = 21)

## confusion matrix 
treeMatrix <- treePreds[,c("newRegion_Fact", "newRegion_group")]
caret::confusionMatrix(data = treeMatrix$newRegion_Fact, 
                       reference = treeMatrix$newRegion_group)



# Bagged classification tree with caret package ------------------------------------
ctrl <- trainControl(method = "cv",  number = 10) 

# CV bagged model
bagged_tree <- caret::train(
    newRegion ~  swe_meanAnnAvg_30yr + annWetDegDays_meanAnnAvg_30yr + 
      tmean_meanAnnAvg_30yr + isothermality_meanAnnAvg_30yr + prcp_meanAnnTotal_30yr + 
      PrecipTempCorr_meanAnnAvg_30yr + surfaceClay_perc +             
      avgSandPerc_acrossDepth + avgCoarsePerc_acrossDepth +              
      avgOrganicCarbonPerc_0_3cm + totalAvailableWaterHoldingCapacity,
    data = modDat_fit, #method = 'class',
    # control = list(minsplit = 15, maxdepth = 8)
  method = "treebag",
  trControl = ctrl,
  importance = TRUE
)

# assess results
bagged_tree

# plot most important variables
plot(varImp(bagged_tree))  

# predict the classes
(treePreds <- predictionDat %>% 
    cbind( "treePrediction" = predict(bagged_tree, newdata = predictionDat, type = "prob"), 
           "treePrediction" = predict(bagged_tree, newdata = predictionDat)) %>% 
    cbind(modDat_test %>% 
            dplyr::select(Long, Lat)))

treePreds$goodPred <- as.factor(treePreds$newRegion) == treePreds$treePrediction
# percentage of misclassifications
sum(!treePreds$goodPred)/nrow(treePreds) * 100

ggplot(treePreds) + 
  geom_point(aes(Long, Lat, col = treePrediction), alpha = .1) + 
  ggtitle("model-predicted ecoregion classification", 
          subtitle = paste0("black dots indicate misclassification, \n fill color indicates which ecoregion a point was misclassified as;\n  ",
                            round((100-sum(!treePreds$goodPred)/nrow(treePreds) 
                                   * 100),1), "% classification accuracy" )) + 
  geom_point(data = treePreds[treePreds$goodPred == FALSE,], aes(Long, Lat, fill = treePrediction), 
             col = "black"
             , shape = 21)

## confusion matrix 
treeMatrix <- treePreds[,c("newRegion_Fact", "newRegion_group")]
caret::confusionMatrix(data = treeMatrix$newRegion_Fact, 
                       reference = treeMatrix$newRegion_group)


# bagged classification tree with ipred package ---------------------------

set.seed(12011993)

# train bagged model
tree_bag1 <- bagging(
  formula = newRegion ~  swe_meanAnnAvg_30yr + annWetDegDays_meanAnnAvg_30yr + 
    tmean_meanAnnAvg_30yr + isothermality_meanAnnAvg_30yr + prcp_meanAnnTotal_30yr + 
    PrecipTempCorr_meanAnnAvg_30yr + surfaceClay_perc +             
    avgSandPerc_acrossDepth + avgCoarsePerc_acrossDepth +              
    avgOrganicCarbonPerc_0_3cm + totalAvailableWaterHoldingCapacity,
  data = modDat_fit,
  nbagg = 10,  
  coob = TRUE,
  control = list(minsplit = 15, maxdepth = 8)
)

tree_bag1 

# 
# 
# ## now, try using partykit r package (conditional inference, nonparametric regression tree)
# library(partykit)
# # partyTree <- ctree(newRegionFact ~  swe_meanAnnAvg_30yr + annWetDegDays_meanAnnAvg_30yr + 
# #                      tmean_meanAnnAvg_30yr + isothermality_meanAnnAvg_30yr + prcp_meanAnnTotal_30yr + 
# #                      PrecipTempCorr_meanAnnAvg_30yr +  soilDepth + surfaceClay_perc +             
# #                      avgSandPerc_acrossDepth + avgCoarsePerc_acrossDepth +              
# #                      avgOrganicCarbonPerc_0_3cm + totalAvailableWaterHoldingCapacity,
# #                    data = modDat_fit)
# # try restricting the tree depth (too long right now)
# partyTree <- partykit::ctree(newRegionFact ~  swe_meanAnnAvg_30yr + annWetDegDays_meanAnnAvg_30yr + 
#                      tmean_meanAnnAvg_30yr + isothermality_meanAnnAvg_30yr + prcp_meanAnnTotal_30yr + 
#                      PrecipTempCorr_meanAnnAvg_30yr +  soilDepth + surfaceClay_perc +             
#                      avgSandPerc_acrossDepth + avgCoarsePerc_acrossDepth +              
#                      avgOrganicCarbonPerc_0_3cm + totalAvailableWaterHoldingCapacity,
#                    data = modDat_fit, control = partykit::ctree_control(maxdepth = 7))
# partyTree
# # variable importance
# varImp_temp <- data.frame("variable" = names(sort((varimp(partyTree)), decreasing = TRUE)), 
#                      "importance" = sort((varimp(partyTree)), decreasing = TRUE))
# varImp <- varImp_temp  %>% 
#   # tibble::rownames_to_column() %>% 
#   # dplyr::rename("variable" = rowname) %>% 
#   dplyr::arrange(importance) %>%
#   dplyr::mutate(variable = forcats::fct_inorder(variable))
# ggplot(varImp) + 
#   geom_segment(aes(x = variable, y = 0, xend = variable, yend = importance), 
#                size = 1.5, alpha = 0.7) +
#   geom_point(aes(x = variable, y = importance, col = variable), 
#              size = 4, show.legend = F) +
#   coord_flip() +
#   theme_bw()
# 
# plot(partyTree, main="Conditional Inference Tree for ecoregions")
# 
# # predict categories based on this simple model
# partyTreePreds <- cbind(predictionDat, "partyPrediction" = partykit::predict.party(partyTree, newdata = predictionDat)) %>% 
#   cbind(modDat_test %>% 
#           dplyr::select(Long, Lsat))
# partyTreePreds$goodPred <- partyTreePreds$newRegion == partyTreePreds$partyPrediction
# # percentage of misclassifications
# sum(!partyTreePreds$goodPred)/nrow(partyTreePreds) * 100
# 
# ggplot(partyTreePreds) + 
#   geom_point(aes(Long, Lat, col = partyPrediction), alpha = .1) + 
#   ggtitle("model-predicted ecoregion classification", 
#           subtitle = paste0("black dots indicate misclassification, \n fill color indicates which ecoregion a point was misclassified as; \n  ", round((100-sum(!partyTreePreds$goodPred)/nrow(partyTreePreds) * 100),1), "% classification accuracy" )) + 
#   geom_point(data = partyTreePreds[partyTreePreds$goodPred == FALSE,], aes(Long, Lat, fill = partyPrediction), 
#              col = "black"
#              , shape = 21)
#  
# ## confusion matrix 
# partyMatrix <- partyTreePreds[,c("newRegion", "partyPrediction")]
# caret::confusionMatrix(data = partyTreePreds$partyPrediction, 
#                        reference = partyTreePreds$newRegion)
# 
# ## remove swe and awdd
# # try restricting the tree depth (too long right now)
# partyTree_2 <- partykit::ctree(newRegionFact ~  
#                                tmean_meanAnnAvg_30yr + isothermality_meanAnnAvg_30yr + prcp_meanAnnTotal_30yr + 
#                                PrecipTempCorr_meanAnnAvg_30yr +  soilDepth + surfaceClay_perc +             
#                                avgSandPerc_acrossDepth + avgCoarsePerc_acrossDepth +              
#                                avgOrganicCarbonPerc_0_3cm + totalAvailableWaterHoldingCapacity,
#                              data = modDat_fit, control = partykit::ctree_control(maxdepth = 7))
# partyTree_2
# # variable importance
# varImp_temp <- data.frame("variable" = names(sort((varimp(partyTree_2)), decreasing = TRUE)), 
#                           "importance" = sort((varimp(partyTree_2)), decreasing = TRUE))
# varImp <- varImp_temp  %>% 
#   # tibble::rownames_to_column() %>% 
#   # dplyr::rename("variable" = rowname) %>% 
#   dplyr::arrange(importance) %>%
#   dplyr::mutate(variable = forcats::fct_inorder(variable))
# ggplot(varImp) + 
#   geom_segment(aes(x = variable, y = 0, xend = variable, yend = importance), 
#                size = 1.5, alpha = 0.7) +
#   geom_point(aes(x = variable, y = importance, col = variable), 
#              size = 4, show.legend = F) +
#   coord_flip() +
#   theme_bw()
# 
# plot(partyTree_2, main="Conditional Inference Tree for ecoregions")
# 
# # predict categories based on this simple model
# partyTree_2Preds <- cbind(predictionDat, "partyPrediction" = partykit::predict.party(partyTree_2, newdata = predictionDat)) %>% 
#   cbind(modDat_test %>% 
#           dplyr::select(Long, Lat))
# partyTree_2Preds$goodPred <- partyTree_2Preds$newRegion == partyTree_2Preds$partyPrediction
# # percentage of misclassifications
# sum(!partyTree_2Preds$goodPred)/nrow(partyTree_2Preds) * 100
# 
# ggplot(partyTree_2Preds) + 
#   geom_point(aes(Long, Lat, col = partyPrediction), alpha = .1) + 
#   ggtitle("model-predicted ecoregion classification", 
#           subtitle = paste0("black dots indicate misclassification, \n fill color indicates which ecoregion a point was misclassified as; \n  ", round((100-sum(!partyTree_2Preds$goodPred)/nrow(partyTree_2Preds) * 100),1), "% classification accuracy" )) + 
#   geom_point(data = partyTree_2Preds[partyTree_2Preds$goodPred == FALSE,], aes(Long, Lat, fill = partyPrediction), 
#              col = "black"
#              , shape = 21)
# 
# ## confusion matrix 
# partyMatrix <- partyTree_2Preds[,c("newRegion", "partyPrediction")]
# caret::confusionMatrix(data = partyTree_2Preds$partyPrediction, 
#                        reference = partyTree_2Preds$newRegion)
