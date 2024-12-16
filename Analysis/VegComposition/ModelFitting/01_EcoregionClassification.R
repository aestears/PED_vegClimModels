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
library(partykit)
library(ggtern)
library(patchwork)
library(ggpubr)
library(gt)

# Load Data ---------------------------------------------------------------
# data ready for modeling 
modDat <- readRDS("./Data_processed/EcoRegion_climSoilData.rds")

# Prepare data for model-fitting  ----------------------------------------------
# (eastern forest) put eastern temperate forests, tropical wet forests and northern forests together
# (dry shrub and grass) put great plains, southern semiarid highlands, Mediterranean California and north american deserts together 
# (western forest) put northwestern forested mountains, marine west coast forests together, and temperate sierras together
set.seed(12011993)

# ggplot(modDat) + 
#   geom_point(aes(x = Long, y = Lat))

modDat_use <- modDat %>% 
  dplyr::select(c(newRegion, #swe_meanAnnAvg_30yr:
                  tmin_meanAnnAvg_30yr:durationFrostFreeDays_meanAnnAvg_30yr,
                                         soilDepth                  , surfaceClay_perc          ,                
                                         avgSandPerc_acrossDepth    ,  avgCoarsePerc_acrossDepth,                 
                                         avgOrganicCarbonPerc_0_3cm ,  totalAvailableWaterHoldingCapacity,
                                         Long, Lat)) %>% 
  mutate(newRegion = as.factor(newRegion)) %>% 
  drop_na()
  
default_idx <-  createDataPartition(modDat$newRegion, p = 0.8, list = FALSE)
modDat_fit <- modDat_use[default_idx, ] %>% 
    # dplyr::select(newRegion, swe_meanAnnAvg_30yr , annWetDegDays_meanAnnAvg_30yr , 
  #           tmean_meanAnnAvg_30yr , isothermality_meanAnnAvg_30yr , 
  #         prcp_meanAnnTotal_30yr , PrecipTempCorr_meanAnnAvg_30yr, 
  #        soilDepth                  , surfaceClay_perc          ,                
  #        avgSandPerc_acrossDepth    ,  avgCoarsePerc_acrossDepth,                 
  #        avgOrganicCarbonPerc_0_3cm ,  totalAvailableWaterHoldingCapacity, Long, Lat) %>% 
  drop_na() %>% 
  st_drop_geometry() 

modDat_test <-  modDat_use[-default_idx, ] %>% 
  mutate(newRegion = as.factor(newRegion)) %>% 
  # dplyr::select(c(newRegion, swe_meanAnnAvg_30yr:
  #                   # tmean_meanAnnAvg_30yr:prcp_meanAnnTotal_30yr,
  #                   #                        precip_wettestMonth_meanAnnAvg_30yr:PrecipTempCorr_meanAnnAvg_30yr, 
  #                   #                        isothermality_meanAnnAvg_30yr:annVPD_min_meanAnnAvg_30yr, 
  #                   durationFrostFreeDays_meanAnnAvg_30yr,
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

(corrPlot <- 
    modDat_fit %>% 
    select(precip_driestMonth_meanAnnAvg_30yr, 
           PrecipTempCorr_meanAnnAvg_30yr, 
           avgOrganicCarbonPerc_0_3cm, 
           T_warmestMonth_meanAnnAvg_30yr, 
          # vp_meanAnnAvg_30yr, 
           prcp_meanAnnTotal_30yr) %>% 
    # select(c("swe_meanAnnAvg_30yr","annWetDegDays_meanAnnAvg_30yr",     
    #          "tmean_meanAnnAvg_30yr" ,"isothermality_meanAnnAvg_30yr", "prcp_meanAnnTotal_30yr" ,      
    #          "PrecipTempCorr_meanAnnAvg_30yr" ,"soilDepth" ,      "surfaceClay_perc"            ,      
    #           "avgSandPerc_acrossDepth" , "avgCoarsePerc_acrossDepth"   ,       "avgOrganicCarbonPerc_0_3cm" ,       
    #           "totalAvailableWaterHoldingCapacity")) %>% 
     # rename("swe" = swe_meanAnnAvg_30yr,  "tmean" = tmean_meanAnnAvg_30yr,
     #        "prcp" = prcp_meanAnnTotal_30yr, 
     #       "prcp \n TempCorr" = PrecipTempCorr_meanAnnAvg_30yr,  "isothermality" = isothermality_meanAnnAvg_30yr,
     #       "Wet \n DegDays" = annWetDegDays_meanAnnAvg_30yr) %>% 
    slice_sample(n = 5e4) %>% 
    #select(-matches("_")) %>% 
    ggpairs( upper = list(continuous = my_fn), lower = list(continuous = GGally::wrap("points", alpha = 0.1, size=0.2)), progress = FALSE))

## remove Soil Depth, since it's highly correlated w/ total available water holding capacity



# set controls ------------------------------------------------------------

# fit models --------------------------------------------------------------
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
predictionDat <- modDat_test
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

# library(rpart)
# # fit a basic classification tree model
# treeMod <- rpart(newRegion ~  swe_meanAnnAvg_30yr + annWetDegDays_meanAnnAvg_30yr + 
#                    tmean_meanAnnAvg_30yr + isothermality_meanAnnAvg_30yr + prcp_meanAnnTotal_30yr + 
#                    PrecipTempCorr_meanAnnAvg_30yr + surfaceClay_perc +             
#                    avgSandPerc_acrossDepth + avgCoarsePerc_acrossDepth +              
#                    avgOrganicCarbonPerc_0_3cm + totalAvailableWaterHoldingCapacity,
#                  data = modDat_fit, method = 'class')
# print(treeMod)
# # look at variable importance
# # variable importance
# varImp_temp <- data.frame("variable" = names(sort((treeMod$variable.importance), decreasing = TRUE)), 
#                           "importance" = sort((treeMod$variable.importance), decreasing = TRUE))
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
# # look at cross-validation error 
# plotcp(treeMod)
# treeMod$cptable
# 
# # prune the tree
# # tune typer-parameters
# hyper_grid <- expand.grid(
#   minsplit = seq(5, 15, 1),
#   maxdepth = seq(5, 15, 1)
# )
# head(hyper_grid)
# # total number of combinations
# nrow(hyper_grid)
# ## [1] 121
# 
# # fit different models to find the ideal set of hyperparameters
# models <- list()
# 
# for (i in 1:nrow(hyper_grid)) {
#   
#   # get minsplit, maxdepth values at row i
#   minsplit <- hyper_grid$minsplit[i]
#   maxdepth <- hyper_grid$maxdepth[i]
#   
#   # train a model and store in the list
#   models[[i]] <- rpart(
#     newRegion ~  swe_meanAnnAvg_30yr + annWetDegDays_meanAnnAvg_30yr + 
#       tmean_meanAnnAvg_30yr + isothermality_meanAnnAvg_30yr + prcp_meanAnnTotal_30yr + 
#       PrecipTempCorr_meanAnnAvg_30yr + surfaceClay_perc +             
#       avgSandPerc_acrossDepth + avgCoarsePerc_acrossDepth +              
#       avgOrganicCarbonPerc_0_3cm + totalAvailableWaterHoldingCapacity,
#     data = modDat_fit, method = 'class',
#     control = list(minsplit = minsplit, maxdepth = maxdepth)
#   )
# }
# 
# # function to get optimal cp
# get_cp <- function(x) {
#   min    <- which.min(x$cptable[, "xerror"])
#   cp <- x$cptable[min, "CP"] 
# }
# 
# # function to get minimum error
# get_min_error <- function(x) {
#   min    <- which.min(x$cptable[, "xerror"])
#   xerror <- x$cptable[min, "xerror"] 
# }
# 
# bestParams <- hyper_grid %>%
#   mutate(
#     cp    = purrr::map_dbl(models, get_cp),
#     error = purrr::map_dbl(models, get_min_error)
#   ) %>%
#   arrange(error) %>%
#   top_n(-5, wt = error)
# # minsplit maxdepth   cp     error
# # 1       15        8 0.01 0.2303705
# 
# ## fit the best tree
# bestTree <- rpart(
#   newRegion ~  swe_meanAnnAvg_30yr + annWetDegDays_meanAnnAvg_30yr + 
#     tmean_meanAnnAvg_30yr + isothermality_meanAnnAvg_30yr + prcp_meanAnnTotal_30yr + 
#     PrecipTempCorr_meanAnnAvg_30yr + surfaceClay_perc +             
#     avgSandPerc_acrossDepth + avgCoarsePerc_acrossDepth +              
#     avgOrganicCarbonPerc_0_3cm + totalAvailableWaterHoldingCapacity,
#   data = modDat_fit, method = 'class',
#   control = list(minsplit = 15, maxdepth = 8)
# )
# 
# 
# # predict categories based on this single 'best tree' 
# (treePreds <- predictionDat %>% 
#     cbind( "treePrediction" = predict(bestTree, newdata = predictionDat)) %>% 
#     mutate(newRegion_Fact = as.factor(newRegion),
#            newRegion_predicted = pmap_dbl(.[c("treePrediction.dryShrubGrass", 
#                                               "treePrediction.eastForest", 
#                                               "treePrediction.westForest")], max), 
#            across(treePrediction.dryShrubGrass:treePrediction.westForest, ~.x == newRegion_predicted)) %>% 
#     mutate(treePrediction.dryShrubGrass = str_replace(treePrediction.dryShrubGrass, 'TRUE', 'dryShrubGrass'), 
#            treePrediction.eastForest = str_replace(treePrediction.eastForest, 'TRUE', "eastForest"), 
#            treePrediction.westForest = str_replace(treePrediction.westForest, 'TRUE', "westForest"),
#            treePrediction.dryShrubGrass = str_replace(treePrediction.dryShrubGrass, 'FALSE', ""), 
#            treePrediction.eastForest = str_replace(treePrediction.eastForest, 'FALSE', ""), 
#            treePrediction.westForest = str_replace(treePrediction.westForest, 'FALSE', "")) %>% 
#     mutate(
#       newRegion_group = as.factor(pmap_chr(.[c("treePrediction.dryShrubGrass", "treePrediction.eastForest", 
#                                                "treePrediction.westForest")], paste0))) %>% 
#     cbind(modDat_test %>% 
#             dplyr::select(Long, Lat)))
# # 
# treePreds$goodPred <- treePreds$newRegion_Fact == treePreds$newRegion_group
# # percentage of misclassifications
# sum(!treePreds$goodPred)/nrow(treePreds) * 100
# ggplot(treePreds) + 
#   geom_point(aes(Long, Lat, col = newRegion_group), alpha = .1) + 
#   ggtitle("model-predicted ecoregion classification", 
#           subtitle = paste0("black dots indicate misclassification, \n fill color indicates which ecoregion a point was misclassified as;\n  ",
#                             round((100-sum(!treePreds$goodPred)/nrow(treePreds) 
#                                           * 100),1), "% classification accuracy" )) + 
#   geom_point(data = treePreds[treePreds$goodPred == FALSE,], aes(Long, Lat, fill = newRegion_group), 
#              col = "black"
#              , shape = 21)
# 
# ## confusion matrix 
# treeMatrix <- treePreds[,c("newRegion_Fact", "newRegion_group")]
# caret::confusionMatrix(data = treeMatrix$newRegion_Fact, 
#                        reference = treeMatrix$newRegion_group)
# 



# Conditional Inference Regression Tree with all possible variables ------------
## now, try using partykit r package (conditional inference, nonparametric regression tree)
partyTreeALL <- partykit::ctree(newRegion ~  tmin_meanAnnAvg_30yr                  +
                             tmax_meanAnnAvg_30yr                   + tmean_meanAnnAvg_30yr                            +
                             prcp_meanAnnTotal_30yr                 + T_warmestMonth_meanAnnAvg_30yr    +     T_coldestMonth_meanAnnAvg_30yr         +
                              precip_wettestMonth_meanAnnAvg_30yr   +  precip_driestMonth_meanAnnAvg_30yr   +  precip_Seasonality_meanAnnAvg_30yr     +
                              PrecipTempCorr_meanAnnAvg_30yr        +  aboveFreezing_month_meanAnnAvg_30yr  +  isothermality_meanAnnAvg_30yr          +
                              annWaterDeficit_meanAnnAvg_30yr       +  annWetDegDays_meanAnnAvg_30yr        +  annVPD_mean_meanAnnAvg_30yr            +
                              annVPD_max_meanAnnAvg_30yr            +  annVPD_min_meanAnnAvg_30yr           +  annVPD_max_95percentile_30yr           +
                              annWaterDeficit_95percentile_30yr     +  annWetDegDays_5percentile_30yr       +  durationFrostFreeDays_5percentile_30yr +
                              durationFrostFreeDays_meanAnnAvg_30yr +  soilDepth                            +  surfaceClay_perc                       +
                              avgSandPerc_acrossDepth               +  avgCoarsePerc_acrossDepth            + avgOrganicCarbonPerc_0_3cm +
                              totalAvailableWaterHoldingCapacity,
                   data = modDat_fit, control = partykit::ctree_control(maxdepth = 7))
 partyTreeALL

# variable importance
varImp_temp <- data.frame("variable" = names(sort((varimp(partyTreeALL)), decreasing = TRUE)),
                     "importance" = sort((varimp(partyTreeALL)), decreasing = TRUE))
varImp <- varImp_temp  %>%
  # tibble::rownames_to_column() %>%
  # dplyr::rename("variable" = rowname) %>%
  dplyr::arrange(importance) %>%
  dplyr::mutate(variable = forcats::fct_inorder(variable))
(partyTreeALL_varImpPlot <- ggplot(varImp) +
  geom_segment(aes(x = variable, y = 0, xend = variable, yend = importance),
               size = 1.5, alpha = 0.7) +
  geom_point(aes(x = variable, y = importance, col = variable),
             size = 4, show.legend = F) +
  coord_flip() +
  theme_bw()
)
# predict categories based on this simple model
partyTreeALLPreds <- cbind(predictionDat, "partyPrediction" = partykit::predict.party(partyTreeALL, newdata = predictionDat, type = "response")) %>%
  cbind("partyPrediction" = partykit::predict.party(partyTreeALL, newdata = predictionDat, type = "prob")) 
partyTreeALLPreds$goodPred <- partyTreeALLPreds$newRegion == partyTreeALLPreds$partyPrediction
# percentage of misclassifications
sum(!partyTreeALLPreds$goodPred)/nrow(partyTreeALLPreds) * 100

(partyTreeALLPredMAP <- ggplot(partyTreeALLPreds) +
  geom_point(aes(Long, Lat, col = partyPrediction), alpha = .1) +
  ggtitle("model-predicted ecoregion classification -- Model with all possible climate predictors",
          subtitle = paste0("black dots indicate misclassification, \n fill color indicates which ecoregion a point was misclassified as; \n  ", round((100-sum(!partyTreeALLPreds$goodPred)/nrow(partyTreeALLPreds) * 100),1), "% classification accuracy" )) +
  geom_point(data = partyTreeALLPreds[partyTreeALLPreds$goodPred == FALSE,], aes(Long, Lat, fill = partyPrediction),
             col = "black"
             , shape = 21))

## confusion matrix
partyMatrix <- partyTreeALLPreds[,c("newRegion", "partyPrediction")]
partyTreeALL_confMat <- caret::confusionMatrix(data = partyTreeALLPreds$partyPrediction,
                                              reference = partyTreeALLPreds$newRegion)$table

## make ternary plots
# just good predictions
(partyTreeALL_ternGoodPreds <- partyTreeALLPreds %>% 
    filter(goodPred == TRUE) %>% 
    ggtern(aes(x=jitter(partyPrediction.dryShrubGrass*100 + 1, 50),
               y=jitter(partyPrediction.eastForest*100 + 1, 50),
               z=jitter(partyPrediction.westForest*100 + 1, 50), col = newRegion)) +
    geom_point(alpha = .2) +
    Tlab("Eastern Forest") +
    Llab("Shrub/\nGrass") + 
    Rlab("Western \nForest") +
    theme_minimal() + 
    ggtitle("Accurate Predictions"))
# just bad predictions
badDat <- partyTreeALLPreds %>% 
  filter(goodPred == FALSE)
(partyTreeALL_ternBadPreds <- 
    ggtern( data = badDat, aes(x=jitter(partyPrediction.dryShrubGrass*100 + 1, 50),
                               y=jitter(partyPrediction.eastForest*100 + 1, 50),
                               z=jitter(partyPrediction.westForest*100 + 1, 50), col = newRegion)) +
    geom_point(alpha = .2) +
    Tlab("Eastern Forest") +
    Llab("Shrub/\nGrass") + 
    Rlab("Western \nForest") +
    theme_minimal() + 
    ggtitle("Bad Predictions"))

# map of misclassified points 
partyTreeALLPreds$color <- rgb(red = partyTreeALLPreds$partyPrediction.dryShrubGrass, 
                              green = partyTreeALLPreds$partyPrediction.eastForest, 
                              blue = partyTreeALLPreds$partyPrediction.westForest)

(partyTreeALLPred_mapBadGrass <- ggplot() +
    geom_point(data = partyTreeALLPreds, aes(Long, Lat, col = newRegion), alpha = .1) +
    ggtitle("Severity of Grass/Shrub misclassification") +
    geom_point(data = partyTreeALLPreds[partyTreeALLPreds$goodPred == FALSE & partyTreeALLPreds$partyPrediction == "dryShrubGrass",], 
               aes(Long, Lat, fill = partyPrediction.dryShrubGrass),
               col = "black" ,
               shape = 21) +
    scale_fill_distiller(type = "seq", palette = "Reds")
)
(partyTreeALLPred_mapBadEastForest <- ggplot() +
    geom_point(data = partyTreeALLPreds, aes(Long, Lat, col = newRegion), alpha = .1) +
    ggtitle("Severity of Eastern Forest misclassification") +
    geom_point(data = partyTreeALLPreds[partyTreeALLPreds$goodPred == FALSE & partyTreeALLPreds$partyPrediction == "eastForest",], 
               aes(Long, Lat, fill = partyPrediction.eastForest),
               col = "black" ,
               shape = 21) +
    scale_fill_distiller(type = "seq", palette = "Greens")
) 

(partyTreeALLPred_mapBadWestForest <- ggplot() +
    geom_point(data = partyTreeALLPreds, aes(Long, Lat, col = newRegion), alpha = .1) +
    ggtitle("Severity of Western Forest misclassification") +
    geom_point(data = partyTreeALLPreds[partyTreeALLPreds$goodPred == FALSE & partyTreeALLPreds$partyPrediction == "westForest",], 
               aes(Long, Lat, fill = partyPrediction.westForest),
               col = "black" ,
               shape = 21) +
    scale_fill_distiller(type = "seq", palette = "Blues")
) 



## arrange all of the figures for this model
ggpubr::annotate_figure(ggarrange(
  partyTreeALL_varImpPlot,
  ggplotify::as.grob(gt::as_gtable(gt::gt(data.frame("dryShrubGrass" =  data.frame(partyTreeALL_confMat)$Freq[1:3], 
                                                               "eastForest" = data.frame(partyTreeALL_confMat)$Freq[4:6],
                                                               "westForest" = data.frame(partyTreeALL_confMat)$Freq[7:9], 
                                                               row.names = c("dryShrubGrass", "eastForest", "westForest")), 
                                                    rownames_to_stub = TRUE))), 
            # ggplotify::as.grob(gt::as_gtable(gt::gt(data.frame("inputs" = c("tmin_meanAnnAvg"      ,
            #                                                                 "tmax_meanAnnAvg"    ,"tmean_meanAnnAvg" ,    "vp_meanAnnAvg", "prcp_meanAnnTotal", 
            #                                                                 "T_warmestMonth_meanAnnAvg"    ,    "T_coldestMonth_meanAnnAvg", NA),
            #                                                    "inputs...1" =  c( "precip_wettestMonth_meanAnnAvg"   , "precip_driestMonth_meanAnnAvg", "precip_Seasonality_meanAnnAvg"     ,
            #                                                                 "PrecipTempCorr_meanAnnAvg", "aboveFreezing_month_meanAnnAvg"  , "isothermality_meanAnnAvg"          ,
            #                                                                 "annWaterDeficit_meanAnnAvg", "annWetDegDays_meanAnnAvg" )   , 
            #                                                    "inputs...2" =c( "annVPD_mean_meanAnnAvg" ,  "annVPD_max_meanAnnAvg", "annVPD_min_meanAnnAvg",
            #                                                                     "annVPD_max_95percentile", "annWaterDeficit_95percentile"     , "annWetDegDays_5percentile",
            #                                                                     "durationFrostFreeDays_5percentile" , "durationFrostFreeDays_meanAnnAvg" )  , 
            #                                                    "inputs...3" =c("soilDepth", "surfaceClay_perc"                       ,
            #                                                                    "avgSandPerc_acrossDepth"               , "avgCoarsePerc_acrossDepth"             ,
            #                                                                    "totalAvailableWaterHoldingCapacity", NA, NA, NA)
            #                                                    )))),
  partyTreeALLPredMAP, 
                                  ggarrange((ggtern::arrangeGrob(partyTreeALL_ternGoodPreds)), 
                                            (ggtern::arrangeGrob(partyTreeALL_ternBadPreds))), 
                                  partyTreeALLPred_mapBadGrass, 
                                  partyTreeALLPred_mapBadEastForest, 
                                  partyTreeALLPred_mapBadWestForest, 
                                  nrow = 6
)#, 
#top = c("Model with depth = 7 and all climate and soil predictors")
) %>% 
  ggexport(
    filename = "./Figures/EcoRegionModelFigures/ModelFigures_depth7_AllPossiblePredictors.pdf",
    width = 12, height = 23)

# Conditional Inference Regression Tree with top 7 most important variables ------------
# try restricting the tree depth (too long right now)
partyTree_1 <- partykit::ctree(newRegion ~  precip_driestMonth_meanAnnAvg_30yr   + PrecipTempCorr_meanAnnAvg_30yr        +
                                avgOrganicCarbonPerc_0_3cm + 
                                 surfaceClay_perc + 
                                 annVPD_max_meanAnnAvg_30yr + 
                                 prcp_meanAnnTotal_30yr + 
                                 annWetDegDays_5percentile_30yr
                                  ,
                                data = modDat_fit, control = partykit::ctree_control(maxdepth = 7))
partyTree_1

# variable importance
varImp_temp <- data.frame("variable" = names(sort((varimp(partyTree_1)), decreasing = TRUE)),
                          "importance" = sort((varimp(partyTree_1)), decreasing = TRUE))
varImp <- varImp_temp  %>%
  # tibble::rownames_to_column() %>%
  # dplyr::rename("variable" = rowname) %>%
  dplyr::arrange(importance) %>%
  dplyr::mutate(variable = forcats::fct_inorder(variable))
(partyTree_1_varImpPlot <- ggplot(varImp) +
  geom_segment(aes(x = variable, y = 0, xend = variable, yend = importance),
               size = 1.5, alpha = 0.7) +
  geom_point(aes(x = variable, y = importance, col = variable),
             size = 4, show.legend = F) +
  coord_flip() +
  theme_bw())

# predict categories based on this simple model
partyTree_1Preds <- cbind(predictionDat, "partyPrediction" = partykit::predict.party(partyTree_1, newdata = predictionDat, type = "response")) %>%
  cbind("partyPrediction" = partykit::predict.party(partyTree_1, newdata = predictionDat, type = "prob")) 
partyTree_1Preds$goodPred <- partyTree_1Preds$newRegion == partyTree_1Preds$partyPrediction
# percentage of misclassifications
sum(!partyTree_1Preds$goodPred)/nrow(partyTree_1Preds) * 100

(partyTree_1PredMAP <- ggplot(partyTree_1Preds) +
    geom_point(aes(Long, Lat, col = partyPrediction), alpha = .1) +
    ggtitle("model-predicted ecoregion classification -- Model with all possible climate predictors",
            subtitle = paste0("black dots indicate misclassification, \n fill color indicates which ecoregion a point was misclassified as; \n  ", round((100-sum(!partyTree_1Preds$goodPred)/nrow(partyTree_1Preds) * 100),1), "% classification accuracy" )) +
    geom_point(data = partyTree_1Preds[partyTree_1Preds$goodPred == FALSE,], aes(Long, Lat, fill = partyPrediction),
               col = "black"
               , shape = 21))

## confusion matrix
partyMatrix <- partyTree_1Preds[,c("newRegion", "partyPrediction")]
partyTree_1_confMat <- caret::confusionMatrix(data = partyTree_1Preds$partyPrediction,
                                               reference = partyTree_1Preds$newRegion)$table

## make ternary plots
# just good predictions
(partyTree_1_ternGoodPreds <- partyTree_1Preds %>% 
    filter(goodPred == TRUE) %>% 
    ggtern(aes(x=jitter(partyPrediction.dryShrubGrass*100 + 1, 50),
               y=jitter(partyPrediction.eastForest*100 + 1, 50),
               z=jitter(partyPrediction.westForest*100 + 1, 50), col = newRegion)) +
    geom_point(alpha = .2) +
    Tlab("Eastern Forest") +
    Llab("Shrub/\nGrass") + 
    Rlab("Western \nForest") +
    theme_minimal() + 
    ggtitle("Accurate Predictions"))
# just bad predictions
badDat <- partyTree_1Preds %>% 
  filter(goodPred == FALSE)
(partyTree_1_ternBadPreds <- 
    ggtern( data = badDat, aes(x=jitter(partyPrediction.dryShrubGrass*100 + 1, 50),
                               y=jitter(partyPrediction.eastForest*100 + 1, 50),
                               z=jitter(partyPrediction.westForest*100 + 1, 50), col = newRegion)) +
    geom_point(alpha = .2) +
    Tlab("Eastern Forest") +
    Llab("Shrub/\nGrass") + 
    Rlab("Western \nForest") +
    theme_minimal() + 
    ggtitle("Bad Predictions"))

# map of misclassified points 
partyTree_1Preds$color <- rgb(red = partyTree_1Preds$partyPrediction.dryShrubGrass, 
                               green = partyTree_1Preds$partyPrediction.eastForest, 
                               blue = partyTree_1Preds$partyPrediction.westForest)

(partyTree_1Pred_mapBadGrass <- ggplot() +
    geom_point(data = partyTree_1Preds, aes(Long, Lat, col = newRegion), alpha = .1) +
    ggtitle("Severity of Grass/Shrub misclassification") +
    geom_point(data = partyTree_1Preds[partyTree_1Preds$goodPred == FALSE & partyTree_1Preds$partyPrediction == "dryShrubGrass",], 
               aes(Long, Lat, fill = partyPrediction.dryShrubGrass),
               col = "black" ,
               shape = 21) +
    scale_fill_distiller(type = "seq", palette = "Reds")
)
(partyTree_1Pred_mapBadEastForest <- ggplot() +
    geom_point(data = partyTree_1Preds, aes(Long, Lat, col = newRegion), alpha = .1) +
    ggtitle("Severity of Eastern Forest misclassification") +
    geom_point(data = partyTree_1Preds[partyTree_1Preds$goodPred == FALSE & partyTree_1Preds$partyPrediction == "eastForest",], 
               aes(Long, Lat, fill = partyPrediction.eastForest),
               col = "black" ,
               shape = 21) +
    scale_fill_distiller(type = "seq", palette = "Greens")
) 

(partyTree_1Pred_mapBadWestForest <- ggplot() +
    geom_point(data = partyTree_1Preds, aes(Long, Lat, col = newRegion), alpha = .1) +
    ggtitle("Severity of Western Forest misclassification") +
    geom_point(data = partyTree_1Preds[partyTree_1Preds$goodPred == FALSE & partyTree_1Preds$partyPrediction == "westForest",], 
               aes(Long, Lat, fill = partyPrediction.westForest),
               col = "black" ,
               shape = 21) +
    scale_fill_distiller(type = "seq", palette = "Blues")
) 


## make response curves 
# (partyTree_1_precipOfDriestMonth <-  partyTree_1Preds %>%
#     mutate(
#       partyPrediction_prob = pmap_dbl(.[c("partyPrediction.dryShrubGrass", "partyPrediction.eastForest", "partyPrediction.westForest")], max),
#            across(partyPrediction.dryShrubGrass:partyPrediction.westForest, ~.x == partyPrediction)) %>%
#   select(c("precip_driestMonth_meanAnnAvg_30yr"  ,   "PrecipTempCorr_meanAnnAvg_30yr"      ,
#            "T_warmestMonth_meanAnnAvg_30yr"    ,"avgOrganicCarbonPerc_0_3cm" ,    
#            "vp_meanAnnAvg_30yr", "prcp_meanAnnTotal_30yr", 
#            "swe_meanAnnAvg_30yr", "partyPrediction", "partyPrediction_prob")) %>% 
# ggplot() + 
#   geom_point(aes(x = precip_driestMonth_meanAnnAvg_30yr, y = jitter(partyPrediction_prob, 30), color = partyPrediction), alpha = .5) + 
#   theme_minimal() + 
#   theme() + 
#   xlab("Precip of the driest month") + 
#   labs(color = "Ecoregion") + 
#   ylab("Probability") +
#   facet_wrap(~partyPrediction) + 
#   geom_smooth(aes(x = precip_driestMonth_meanAnnAvg_30yr,  y = partyPrediction_prob),color = "black", se = FALSE) )
# 
# (partyTree_1_preciTempCorr <-  partyTree_1Preds %>%
#     mutate(
#       partyPrediction_prob = pmap_dbl(.[c("partyPrediction.dryShrubGrass", "partyPrediction.eastForest", "partyPrediction.westForest")], max),
#       across(partyPrediction.dryShrubGrass:partyPrediction.westForest, ~.x == partyPrediction)) %>%
#     select(c("precip_driestMonth_meanAnnAvg_30yr"  ,   "PrecipTempCorr_meanAnnAvg_30yr"      ,
#              "T_warmestMonth_meanAnnAvg_30yr"    ,"avgOrganicCarbonPerc_0_3cm" ,    
#              "vp_meanAnnAvg_30yr", "prcp_meanAnnTotal_30yr", 
#              "swe_meanAnnAvg_30yr", "partyPrediction", "partyPrediction_prob")) %>% 
#     ggplot() + 
#     geom_point(aes(x = PrecipTempCorr_meanAnnAvg_30yr, y = jitter(partyPrediction_prob, 30), color = partyPrediction), alpha = .5) + 
#     theme_minimal() + 
#     theme() + 
#     xlab("Precip / Temp. Correlation") + 
#     labs(color = "Ecoregion") + 
#     ylab("Probability") +
#     facet_wrap(~partyPrediction) + 
#     geom_smooth(aes(x = PrecipTempCorr_meanAnnAvg_30yr,  y = partyPrediction_prob),color = "black", se = FALSE) +
#     ylim(c(.5, 1)))   
#  
# #
# (partyTree_1_TempWarmestMonth <-  partyTree_1Preds %>%
#     mutate(
#       partyPrediction_prob = pmap_dbl(.[c("partyPrediction.dryShrubGrass", "partyPrediction.eastForest", "partyPrediction.westForest")], max),
#       across(partyPrediction.dryShrubGrass:partyPrediction.westForest, ~.x == partyPrediction)) %>%
#     select(c("precip_driestMonth_meanAnnAvg_30yr"  ,   "PrecipTempCorr_meanAnnAvg_30yr"      ,
#              "T_warmestMonth_meanAnnAvg_30yr"    ,"avgOrganicCarbonPerc_0_3cm" ,    
#              "vp_meanAnnAvg_30yr", "prcp_meanAnnTotal_30yr", 
#              "swe_meanAnnAvg_30yr", "partyPrediction", "partyPrediction_prob")) %>% 
#     ggplot() + 
#     geom_point(aes(x = T_warmestMonth_meanAnnAvg_30yr, y = jitter(partyPrediction_prob, 30), color = partyPrediction), alpha = .5) + 
#     theme_minimal() + 
#     theme() + 
#     xlab("Temp of warmest month") + 
#     labs(color = "Ecoregion") + 
#     ylab("Probability") +
#     facet_wrap(~partyPrediction) + 
#     geom_smooth(aes(x = T_warmestMonth_meanAnnAvg_30yr,  y = partyPrediction_prob),color = "black", se = FALSE) +
#     ylim(c(.5, 1)))   
# 
# (partyTree_1_Carbon <-  partyTree_1Preds %>%
#     mutate(
#       partyPrediction_prob = pmap_dbl(.[c("partyPrediction.dryShrubGrass", "partyPrediction.eastForest", "partyPrediction.westForest")], max),
#       across(partyPrediction.dryShrubGrass:partyPrediction.westForest, ~.x == partyPrediction)) %>%
#     select(c("precip_driestMonth_meanAnnAvg_30yr"  ,   "PrecipTempCorr_meanAnnAvg_30yr"      ,
#              "T_warmestMonth_meanAnnAvg_30yr"    ,"avgOrganicCarbonPerc_0_3cm" ,    
#              "vp_meanAnnAvg_30yr", "prcp_meanAnnTotal_30yr", 
#              "swe_meanAnnAvg_30yr", "partyPrediction", "partyPrediction_prob")) %>% 
#     ggplot() + 
#     geom_point(aes(x = avgOrganicCarbonPerc_0_3cm, y = jitter(partyPrediction_prob, 30), color = partyPrediction), alpha = .5) + 
#     theme_minimal() + 
#     theme() + 
#     xlab("Carbon in first 3 cm of soil") + 
#     labs(color = "Ecoregion") + 
#     ylab("Probability") +
#     facet_wrap(~partyPrediction) + 
#     geom_smooth(aes(x = avgOrganicCarbonPerc_0_3cm,  y = partyPrediction_prob),color = "black", se = FALSE) +
#     ylim(c(.5, 1)))   
# 
# (partyTree_1_vp <-  partyTree_1Preds %>%
#     mutate(
#       partyPrediction_prob = pmap_dbl(.[c("partyPrediction.dryShrubGrass", "partyPrediction.eastForest", "partyPrediction.westForest")], max),
#       across(partyPrediction.dryShrubGrass:partyPrediction.westForest, ~.x == partyPrediction)) %>%
#     select(c("precip_driestMonth_meanAnnAvg_30yr"  ,   "PrecipTempCorr_meanAnnAvg_30yr"      ,
#              "T_warmestMonth_meanAnnAvg_30yr"    ,"avgOrganicCarbonPerc_0_3cm" ,    
#              "vp_meanAnnAvg_30yr", "prcp_meanAnnTotal_30yr", 
#              "swe_meanAnnAvg_30yr", "partyPrediction", "partyPrediction_prob")) %>% 
#     ggplot() + 
#     geom_point(aes(x = vp_meanAnnAvg_30yr, y = jitter(partyPrediction_prob, 30), color = partyPrediction), alpha = .5) + 
#     theme_minimal() + 
#     theme() + 
#     xlab("mean annual daily atmospheric vapor pressure") + 
#     labs(color = "Ecoregion") + 
#     ylab("Probability") +
#     facet_wrap(~partyPrediction) + 
#     geom_smooth(aes(x = vp_meanAnnAvg_30yr,  y = partyPrediction_prob),color = "black", se = FALSE) +
#     ylim(c(.5, 1)))   
# 
# (partyTree_1_MAP <-  partyTree_1Preds %>%
#     mutate(
#       partyPrediction_prob = pmap_dbl(.[c("partyPrediction.dryShrubGrass", "partyPrediction.eastForest", "partyPrediction.westForest")], max),
#       across(partyPrediction.dryShrubGrass:partyPrediction.westForest, ~.x == partyPrediction)) %>%
#     select(c("precip_driestMonth_meanAnnAvg_30yr"  ,   "PrecipTempCorr_meanAnnAvg_30yr"      ,
#              "T_warmestMonth_meanAnnAvg_30yr"    ,"avgOrganicCarbonPerc_0_3cm" ,    
#              "vp_meanAnnAvg_30yr", "prcp_meanAnnTotal_30yr", 
#              "swe_meanAnnAvg_30yr", "partyPrediction", "partyPrediction_prob")) %>% 
#     ggplot() + 
#     geom_point(aes(x = prcp_meanAnnTotal_30yr, y = jitter(partyPrediction_prob, 30), color = partyPrediction), alpha = .5) + 
#     theme_minimal() + 
#     theme() + 
#     xlab("mean annual precip") + 
#     labs(color = "Ecoregion") + 
#     ylab("Probability") +
#     facet_wrap(~partyPrediction) + 
#     geom_smooth(aes(x = prcp_meanAnnTotal_30yr,  y = partyPrediction_prob),color = "black", se = FALSE) +
#     ylim(c(.5, 1)))   
# 
# (partyTree_1_swe <-  partyTree_1Preds %>%
#     mutate(
#       partyPrediction_prob = pmap_dbl(.[c("partyPrediction.dryShrubGrass", "partyPrediction.eastForest", "partyPrediction.westForest")], max),
#       across(partyPrediction.dryShrubGrass:partyPrediction.westForest, ~.x == partyPrediction)) %>%
#     select(c("precip_driestMonth_meanAnnAvg_30yr"  ,   "PrecipTempCorr_meanAnnAvg_30yr"      ,
#              "T_warmestMonth_meanAnnAvg_30yr"    ,"avgOrganicCarbonPerc_0_3cm" ,    
#              "vp_meanAnnAvg_30yr", "prcp_meanAnnTotal_30yr", 
#              "swe_meanAnnAvg_30yr", "partyPrediction", "partyPrediction_prob")) %>% 
#     ggplot() + 
#     geom_point(aes(x = swe_meanAnnAvg_30yr, y = jitter(partyPrediction_prob, 30), color = partyPrediction), alpha = .5) + 
#     theme_minimal() + 
#     theme() + 
#     xlab("total annual snowpack SWE") + 
#     labs(color = "Ecoregion") + 
#     ylab("Probability") +
#     facet_wrap(~partyPrediction) + 
#     geom_smooth(aes(x = swe_meanAnnAvg_30yr,  y = partyPrediction_prob),color = "black", se = FALSE) +
#     ylim(c(.5, 1)))   

## arrange all of the figures for this model

ggpubr::annotate_figure(ggarrange(partyTree_1_varImpPlot,
  ggarrange(
    ggplotify::as.grob(gt::as_gtable(gt::gt(data.frame("dryShrubGrass" =  data.frame(partyTree_1_confMat)$Freq[1:3], 
                                                       "eastForest" = data.frame(partyTree_1_confMat)$Freq[4:6],
                                                       "westForest" = data.frame(partyTree_1_confMat)$Freq[7:9], 
                                                       row.names = c("dryShrubGrass", "eastForest", "westForest")), 
                                            rownames_to_stub = TRUE)))#, 
    # ggplotify::as.grob(gt::as_gtable(gt::gt(data.frame("inputs" = c("precip_driestMonth_meanAnnAvg"  ,   "PrecipTempCorr_meanAnnAvg"      ,
    #                                                                 "T_warmestMonth_meanAnnAvg"    ,"avgOrganicCarbonPerc_0_3cm" ,    
    #                                                                 "vp_meanAnnAvg", "prcp_meanAnnTotal", 
    #                                                                 "swe_meanAnnAvg" )
    # ))))
    
  )
  ,
  partyTree_1PredMAP, 
  ggarrange((ggtern::arrangeGrob(partyTree_1_ternGoodPreds)), 
            (ggtern::arrangeGrob(partyTree_1_ternBadPreds))), 
  partyTree_1Pred_mapBadGrass, 
  partyTree_1Pred_mapBadEastForest, 
  partyTree_1Pred_mapBadWestForest, 
  nrow = 6
)#, 
#top = c("Model with depth = 7 and all climate and soil predictors")
) %>% 
  ggexport(
    filename = "./Figures/EcoRegionModelFigures/ModelFigures_depth7_TopSevenPredictors.pdf",
    width = 12, height = 23)

# # Conditional Inference Regression Tree with top 6 most important variables ------------
# ## at this point, none of the predictors are correlated anyway... 
# # try restricting the tree depth (too long right now)
# partyTree_2 <- partykit::ctree(newRegion ~  precip_driestMonth_meanAnnAvg_30yr   + PrecipTempCorr_meanAnnAvg_30yr        +
#                                  T_warmestMonth_meanAnnAvg_30yr    + 
#                                  prcp_meanAnnTotal_30yr + 
#                                  annVPD_max_meanAnnAvg_30yr +
#                                  annVPD_min_meanAnnAvg_30yr 
#                                ,
#                                data = modDat_fit, control = partykit::ctree_control(maxdepth = 7))
# partyTree_2
# 
# # variable importance
# varImp_temp <- data.frame("variable" = names(sort((varimp(partyTree_2)), decreasing = TRUE)),
#                           "importance" = sort((varimp(partyTree_2)), decreasing = TRUE))
# varImp <- varImp_temp  %>%
#   # tibble::rownames_to_column() %>%
#   # dplyr::rename("variable" = rowname) %>%
#   dplyr::arrange(importance) %>%
#   dplyr::mutate(variable = forcats::fct_inorder(variable))
# partyTree_2_varImpPlot <- ggplot(varImp) +
#   geom_segment(aes(x = variable, y = 0, xend = variable, yend = importance),
#                size = 1.5, alpha = 0.7) +
#   geom_point(aes(x = variable, y = importance, col = variable),
#              size = 4, show.legend = F) +
#   coord_flip() +
#   theme_bw()
# 
# # predict categories based on this simple model
# partyTree_2Preds <- cbind(predictionDat, "partyPrediction" = partykit::predict.party(partyTree_2, newdata = predictionDat, type = "response")) %>%
#   cbind("partyPrediction" = partykit::predict.party(partyTree_2, newdata = predictionDat, type = "prob")) 
# partyTree_2Preds$goodPred <- partyTree_2Preds$newRegion == partyTree_2Preds$partyPrediction
# # percentage of misclassifications
# sum(!partyTree_2Preds$goodPred)/nrow(partyTree_2Preds) * 100
# 
# (partyTree_2PredMAP <- ggplot(partyTree_2Preds) +
#     geom_point(aes(Long, Lat, col = partyPrediction), alpha = .1) +
#     ggtitle("model-predicted ecoregion classification -- Model with all possible climate predictors",
#             subtitle = paste0("black dots indicate misclassification, \n fill color indicates which ecoregion a point was misclassified as; \n  ", round((100-sum(!partyTree_2Preds$goodPred)/nrow(partyTree_2Preds) * 100),1), "% classification accuracy" )) +
#     geom_point(data = partyTree_2Preds[partyTree_2Preds$goodPred == FALSE,], aes(Long, Lat, fill = partyPrediction),
#                col = "black"
#                , shape = 21))
# 
# ## confusion matrix
# partyMatrix <- partyTree_2Preds[,c("newRegion", "partyPrediction")]
# partyTree_2_confMat <- caret::confusionMatrix(data = partyTree_2Preds$partyPrediction,
#                                               reference = partyTree_2Preds$newRegion)$table
# 
# ## make ternary plots
# # just good predictions
# (partyTree_2_ternGoodPreds <- partyTree_2Preds %>% 
#     filter(goodPred == TRUE) %>% 
#     ggtern(aes(x=jitter(partyPrediction.dryShrubGrass*100 + 1, 50),
#                y=jitter(partyPrediction.eastForest*100 + 1, 50),
#                z=jitter(partyPrediction.westForest*100 + 1, 50), col = newRegion)) +
#     geom_point(alpha = .2) +
#     Tlab("Eastern Forest") +
#     Llab("Shrub/\nGrass") + 
#     Rlab("Western \nForest") +
#     theme_minimal() + 
#     ggtitle("Accurate Predictions"))
# # just bad predictions
# badDat <- partyTree_2Preds %>% 
#   filter(goodPred == FALSE)
# (partyTree_2_ternBadPreds <- 
#     ggtern( data = badDat, aes(x=jitter(partyPrediction.dryShrubGrass*100 + 1, 50),
#                                y=jitter(partyPrediction.eastForest*100 + 1, 50),
#                                z=jitter(partyPrediction.westForest*100 + 1, 50), col = newRegion)) +
#     geom_point(alpha = .2) +
#     Tlab("Eastern Forest") +
#     Llab("Shrub/\nGrass") + 
#     Rlab("Western \nForest") +
#     theme_minimal() + 
#     ggtitle("Bad Predictions"))
# 
# # map of misclassified points 
# partyTree_2Preds$color <- rgb(red = partyTree_2Preds$partyPrediction.dryShrubGrass, 
#                               green = partyTree_2Preds$partyPrediction.eastForest, 
#                               blue = partyTree_2Preds$partyPrediction.westForest)
# 
# (partyTree_2Pred_mapBadGrass <- ggplot() +
#     geom_point(data = partyTree_2Preds, aes(Long, Lat, col = newRegion), alpha = .1) +
#     ggtitle("Severity of Grass/Shrub misclassification") +
#     geom_point(data = partyTree_2Preds[partyTree_2Preds$goodPred == FALSE & partyTree_2Preds$partyPrediction == "dryShrubGrass",], 
#                aes(Long, Lat, fill = partyPrediction.dryShrubGrass),
#                col = "black" ,
#                shape = 21) +
#     scale_fill_distiller(type = "seq", palette = "Reds")
# )
# (partyTree_2Pred_mapBadEastForest <- ggplot() +
#     geom_point(data = partyTree_2Preds, aes(Long, Lat, col = newRegion), alpha = .1) +
#     ggtitle("Severity of Eastern Forest misclassification") +
#     geom_point(data = partyTree_2Preds[partyTree_2Preds$goodPred == FALSE & partyTree_2Preds$partyPrediction == "eastForest",], 
#                aes(Long, Lat, fill = partyPrediction.eastForest),
#                col = "black" ,
#                shape = 21) +
#     scale_fill_distiller(type = "seq", palette = "Greens")
# ) 
# 
# (partyTree_2Pred_mapBadWestForest <- ggplot() +
#     geom_point(data = partyTree_2Preds, aes(Long, Lat, col = newRegion), alpha = .1) +
#     ggtitle("Severity of Western Forest misclassification") +
#     geom_point(data = partyTree_2Preds[partyTree_2Preds$goodPred == FALSE & partyTree_2Preds$partyPrediction == "westForest",], 
#                aes(Long, Lat, fill = partyPrediction.westForest),
#                col = "black" ,
#                shape = 21) +
#     scale_fill_distiller(type = "seq", palette = "Blues")
# ) 
# 
# ## arrange all of the figures for this model
# 
# ggpubr::annotate_figure(
#   partyTree_2_varImpPlot,
#   ggarrange(
#   ggarrange(
#     ggplotify::as.grob(gt::as_gtable(gt::gt(data.frame("dryShrubGrass" =  data.frame(partyTree_2_confMat)$Freq[1:3], 
#                                                        "eastForest" = data.frame(partyTree_2_confMat)$Freq[4:6],
#                                                        "westForest" = data.frame(partyTree_2_confMat)$Freq[7:9], 
#                                                        row.names = c("dryShrubGrass", "eastForest", "westForest")), 
#                                             rownames_to_stub = TRUE)))#, 
#     # ggplotify::as.grob(gt::as_gtable(gt::gt(data.frame("inputs" = c("precip_driestMonth_meanAnnAvg"  ,   "PrecipTempCorr_meanAnnAvg"      ,
#     #                                                                 "T_warmestMonth_meanAnnAvg"    ,"avgOrganicCarbonPerc_0_3cm" ,    
#     #                                                                 "vp_meanAnnAvg", "prcp_meanAnnTotal" )
#     # ))))
#     # 
#   )
#   ,
#   partyTree_2PredMAP, 
#   ggarrange((ggtern::arrangeGrob(partyTree_2_ternGoodPreds)), 
#             (ggtern::arrangeGrob(partyTree_2_ternBadPreds))), 
#   partyTree_2Pred_mapBadGrass, 
#   partyTree_2Pred_mapBadEastForest, 
#   partyTree_2Pred_mapBadWestForest, 
#   nrow = 6
# )#, 
# #top = c("Model with depth = 7 and all climate and soil predictors")
# ) %>% 
#   ggexport(
#     filename = "./Figures/EcoRegionModelFigures/ModelFigures_depth7_TopSixPredictors.pdf",
#     width = 12, height = 23)

# Conditional Inference Regression Tree with top 5 most important variables ------------
## at this point, none of the predictors are correlated anyway... 
## removed vp instead of prcp (which was technically the least important), since I don't think we'll use vp in the cover models
# try restricting the tree depth (too long right now)
partyTree_3 <- partykit::ctree(newRegion ~  precip_driestMonth_meanAnnAvg_30yr   + PrecipTempCorr_meanAnnAvg_30yr        +
                                 avgOrganicCarbonPerc_0_3cm + 
                                 annVPD_max_meanAnnAvg_30yr + 
                                 prcp_meanAnnTotal_30yr 
                               ,
                               data = modDat_fit, control = partykit::ctree_control(maxdepth = 7))
partyTree_3

# look at correlation btwn predictors
(partyTree_3_corrPlot <- 
    modDat_fit %>% 
    select(precip_driestMonth_meanAnnAvg_30yr   , PrecipTempCorr_meanAnnAvg_30yr        ,
             avgOrganicCarbonPerc_0_3cm , 
             annVPD_max_meanAnnAvg_30yr , 
             prcp_meanAnnTotal_30yr) %>% 
      slice_sample(n = 5e4) %>% 
    #select(-matches("_")) %>% 
    ggpairs( upper = list(continuous = my_fn), lower = list(continuous = GGally::wrap("points", alpha = 0.1, size=0.2)), progress = FALSE))

# variable importance
varImp_temp <- data.frame("variable" = names(sort((varimp(partyTree_3)), decreasing = TRUE)),
                          "importance" = sort((varimp(partyTree_3)), decreasing = TRUE))
varImp <- varImp_temp  %>%
  # tibble::rownames_to_column() %>%
  # dplyr::rename("variable" = rowname) %>%
  dplyr::arrange(importance) %>%
  dplyr::mutate(variable = forcats::fct_inorder(variable))
(partyTree_3_varImpPlot <- ggplot(varImp) +
  geom_segment(aes(x = variable, y = 0, xend = variable, yend = importance),
               size = 1.5, alpha = 0.7) +
  geom_point(aes(x = variable, y = importance, col = variable),
             size = 4, show.legend = F) +
  coord_flip() +
  theme_bw())

# predict categories based on this simple model
partyTree_3Preds <- cbind(predictionDat, "partyPrediction" = partykit::predict.party(partyTree_3, newdata = predictionDat, type = "response")) %>%
  cbind("partyPrediction" = partykit::predict.party(partyTree_3, newdata = predictionDat, type = "prob")) 
partyTree_3Preds$goodPred <- partyTree_3Preds$newRegion == partyTree_3Preds$partyPrediction
# percentage of misclassifications
sum(!partyTree_3Preds$goodPred)/nrow(partyTree_3Preds) * 100

(partyTree_3PredMAP <- ggplot(partyTree_3Preds) +
    geom_point(aes(Long, Lat, col = partyPrediction), alpha = .1) +
    ggtitle("model-predicted ecoregion classification -- Model with all possible climate predictors",
            subtitle = paste0("black dots indicate misclassification, \n fill color indicates which ecoregion a point was misclassified as; \n  ", round((100-sum(!partyTree_3Preds$goodPred)/nrow(partyTree_3Preds) * 100),1), "% classification accuracy" )) +
    geom_point(data = partyTree_3Preds[partyTree_3Preds$goodPred == FALSE,], aes(Long, Lat, fill = partyPrediction),
               col = "black"
               , shape = 21))

## confusion matrix
partyMatrix <- partyTree_3Preds[,c("newRegion", "partyPrediction")]
partyTree_3_confMat <- caret::confusionMatrix(data = partyTree_3Preds$partyPrediction,
                                              reference = partyTree_3Preds$newRegion)$table

## make ternary plots
# just good predictions
(partyTree_3_ternGoodPreds <- partyTree_3Preds %>% 
    filter(goodPred == TRUE) %>% 
    ggtern(aes(x=jitter(partyPrediction.dryShrubGrass*100 + 1, 50),
               y=jitter(partyPrediction.eastForest*100 + 1, 50),
               z=jitter(partyPrediction.westForest*100 + 1, 50), col = newRegion)) +
    geom_point(alpha = .2) +
    Tlab("Eastern Forest") +
    Llab("Shrub/\nGrass") + 
    Rlab("Western \nForest") +
    theme_minimal() + 
    ggtitle("Accurate Predictions"))
# just bad predictions
badDat <- partyTree_3Preds %>% 
  filter(goodPred == FALSE)
(partyTree_3_ternBadPreds <- 
    ggtern( data = badDat, aes(x=jitter(partyPrediction.dryShrubGrass*100 + 1, 50),
                               y=jitter(partyPrediction.eastForest*100 + 1, 50),
                               z=jitter(partyPrediction.westForest*100 + 1, 50), col = newRegion)) +
    geom_point(alpha = .2) +
    Tlab("Eastern Forest") +
    Llab("Shrub/\nGrass") + 
    Rlab("Western \nForest") +
    theme_minimal() + 
    ggtitle("Bad Predictions"))

# map of misclassified points 
partyTree_3Preds$color <- rgb(red = partyTree_3Preds$partyPrediction.dryShrubGrass, 
                              green = partyTree_3Preds$partyPrediction.eastForest, 
                              blue = partyTree_3Preds$partyPrediction.westForest)

(partyTree_3Pred_mapBadGrass <- ggplot() +
    geom_point(data = partyTree_3Preds, aes(Long, Lat, col = newRegion), alpha = .1) +
    ggtitle("Severity of Grass/Shrub misclassification") +
    geom_point(data = partyTree_3Preds[partyTree_3Preds$goodPred == FALSE & partyTree_3Preds$partyPrediction == "dryShrubGrass",], 
               aes(Long, Lat, fill = partyPrediction.dryShrubGrass),
               col = "black" ,
               shape = 21) +
    scale_fill_distiller(type = "seq", palette = "Reds")
)
(partyTree_3Pred_mapBadEastForest <- ggplot() +
    geom_point(data = partyTree_3Preds, aes(Long, Lat, col = newRegion), alpha = .1) +
    ggtitle("Severity of Eastern Forest misclassification") +
    geom_point(data = partyTree_3Preds[partyTree_3Preds$goodPred == FALSE & partyTree_3Preds$partyPrediction == "eastForest",], 
               aes(Long, Lat, fill = partyPrediction.eastForest),
               col = "black" ,
               shape = 21) +
    scale_fill_distiller(type = "seq", palette = "Greens")
) 

(partyTree_3Pred_mapBadWestForest <- ggplot() +
    geom_point(data = partyTree_3Preds, aes(Long, Lat, col = newRegion), alpha = .1) +
    ggtitle("Severity of Western Forest misclassification") +
    geom_point(data = partyTree_3Preds[partyTree_3Preds$goodPred == FALSE & partyTree_3Preds$partyPrediction == "westForest",], 
               aes(Long, Lat, fill = partyPrediction.westForest),
               col = "black" ,
               shape = 21) +
    scale_fill_distiller(type = "seq", palette = "Blues")
) 

## arrange all of the figures for this model

ggpubr::annotate_figure(
                        ggarrange(
                          partyTree_3_varImpPlot,
  ggarrange(
    ggplotify::as.grob(gt::as_gtable(gt::gt(data.frame("dryShrubGrass" =  data.frame(partyTree_3_confMat)$Freq[1:3], 
                                                       "eastForest" = data.frame(partyTree_3_confMat)$Freq[4:6],
                                                       "westForest" = data.frame(partyTree_3_confMat)$Freq[7:9], 
                                                       row.names = c("dryShrubGrass", "eastForest", "westForest")), 
                                            rownames_to_stub = TRUE)))#, 
    # ggplotify::as.grob(gt::as_gtable(gt::gt(data.frame("inputs" = c("precip_driestMonth_meanAnnAvg"  ,   "PrecipTempCorr_meanAnnAvg"      ,
    #                                                                 "T_warmestMonth_meanAnnAvg"    ,"avgOrganicCarbonPerc_0_3cm" ,    
    #                                                                 "prcp_meanAnnTotal" )
    # ))))
    
  )
  ,
  partyTree_3PredMAP, 
  ggarrange((ggtern::arrangeGrob(partyTree_3_ternGoodPreds)), 
            (ggtern::arrangeGrob(partyTree_3_ternBadPreds))), 
  partyTree_3Pred_mapBadGrass, 
  partyTree_3Pred_mapBadEastForest, 
  partyTree_3Pred_mapBadWestForest, 
  nrow = 6
)#, 
#top = c("Model with depth = 7 and all climate and soil predictors")
) %>% 
  ggexport(
    filename = "./Figures/EcoRegionModelFigures/ModelFigures_depth7_TopFivePredictors.pdf",
    width = 12, height = 25)

# Conditional Inference Regression Tree Depth = 7, with top 4 most important variables ------------
## at this point, none of the predictors are correlated anyway... 
## removed vp instead of prcp (which was technically the least important), since I don't think we'll use vp in the cover models
# try restricting the tree depth (too long right now)
partyTree_4 <- partykit::ctree(newRegion ~   precip_driestMonth_meanAnnAvg_30yr   + PrecipTempCorr_meanAnnAvg_30yr        +
                                 avgOrganicCarbonPerc_0_3cm + 
                                 annVPD_max_meanAnnAvg_30yr   ,
                               
                               data = modDat_fit, control = partykit::ctree_control(maxdepth = 7))
partyTree_4

# variable importance
varImp_temp <- data.frame("variable" = names(sort((varimp(partyTree_4)), decreasing = TRUE)),
                          "importance" = sort((varimp(partyTree_4)), decreasing = TRUE))
varImp <- varImp_temp  %>%
  # tibble::rownames_to_column() %>%
  # dplyr::rename("variable" = rowname) %>%
  dplyr::arrange(importance) %>%
  dplyr::mutate(variable = forcats::fct_inorder(variable))
partyTree_4_varImpPlot <- ggplot(varImp) +
  geom_segment(aes(x = variable, y = 0, xend = variable, yend = importance),
               size = 1.5, alpha = 0.7) +
  geom_point(aes(x = variable, y = importance, col = variable),
             size = 4, show.legend = F) +
  coord_flip() +
  theme_bw()

# predict categories based on this simple model
partyTree_4Preds <- cbind(predictionDat, "partyPrediction" = partykit::predict.party(partyTree_4, newdata = predictionDat, type = "response")) %>%
  cbind("partyPrediction" = partykit::predict.party(partyTree_4, newdata = predictionDat, type = "prob")) 
partyTree_4Preds$goodPred <- partyTree_4Preds$newRegion == partyTree_4Preds$partyPrediction
# percentage of misclassifications
sum(!partyTree_4Preds$goodPred)/nrow(partyTree_4Preds) * 100

(partyTree_4PredMAP <- ggplot(partyTree_4Preds) +
    geom_point(aes(Long, Lat, col = partyPrediction), alpha = .1) +
    ggtitle("model-predicted ecoregion classification -- Model with all possible climate predictors",
            subtitle = paste0("black dots indicate misclassification, \n fill color indicates which ecoregion a point was misclassified as; \n  ", round((100-sum(!partyTree_4Preds$goodPred)/nrow(partyTree_4Preds) * 100),1), "% classification accuracy" )) +
    geom_point(data = partyTree_4Preds[partyTree_4Preds$goodPred == FALSE,], aes(Long, Lat, fill = partyPrediction),
               col = "black"
               , shape = 21))

## confusion matrix
partyMatrix <- partyTree_4Preds[,c("newRegion", "partyPrediction")]
partyTree_4_confMat <- caret::confusionMatrix(data = partyTree_4Preds$partyPrediction,
                                              reference = partyTree_4Preds$newRegion)$table

## make ternary plots
# just good predictions
(partyTree_4_ternGoodPreds <- partyTree_4Preds %>% 
    filter(goodPred == TRUE) %>% 
    ggtern(aes(x=jitter(partyPrediction.dryShrubGrass*100 + 1, 50),
               y=jitter(partyPrediction.eastForest*100 + 1, 50),
               z=jitter(partyPrediction.westForest*100 + 1, 50), col = newRegion)) +
    geom_point(alpha = .2) +
    Tlab("Eastern Forest") +
    Llab("Shrub/\nGrass") + 
    Rlab("Western \nForest") +
    theme_minimal() + 
    ggtitle("Accurate Predictions"))
# just bad predictions
badDat <- partyTree_4Preds %>% 
  filter(goodPred == FALSE)
(partyTree_4_ternBadPreds <- 
    ggtern( data = badDat, aes(x=jitter(partyPrediction.dryShrubGrass*100 + 1, 50),
                               y=jitter(partyPrediction.eastForest*100 + 1, 50),
                               z=jitter(partyPrediction.westForest*100 + 1, 50), col = newRegion)) +
    geom_point(alpha = .2) +
    Tlab("Eastern Forest") +
    Llab("Shrub/\nGrass") + 
    Rlab("Western \nForest") +
    theme_minimal() + 
    ggtitle("Bad Predictions"))

# map of misclassified points 
partyTree_4Preds$color <- rgb(red = partyTree_4Preds$partyPrediction.dryShrubGrass, 
                              green = partyTree_4Preds$partyPrediction.eastForest, 
                              blue = partyTree_4Preds$partyPrediction.westForest)

(partyTree_4Pred_mapBadGrass <- ggplot() +
    geom_point(data = partyTree_4Preds, aes(Long, Lat, col = newRegion), alpha = .1) +
    ggtitle("Severity of Grass/Shrub misclassification") +
    geom_point(data = partyTree_4Preds[partyTree_4Preds$goodPred == FALSE & partyTree_4Preds$partyPrediction == "dryShrubGrass",], 
               aes(Long, Lat, fill = partyPrediction.dryShrubGrass),
               col = "black" ,
               shape = 21) +
    scale_fill_distiller(type = "seq", palette = "Reds")
)
(partyTree_4Pred_mapBadEastForest <- ggplot() +
    geom_point(data = partyTree_4Preds, aes(Long, Lat, col = newRegion), alpha = .1) +
    ggtitle("Severity of Eastern Forest misclassification") +
    geom_point(data = partyTree_4Preds[partyTree_4Preds$goodPred == FALSE & partyTree_4Preds$partyPrediction == "eastForest",], 
               aes(Long, Lat, fill = partyPrediction.eastForest),
               col = "black" ,
               shape = 21) +
    scale_fill_distiller(type = "seq", palette = "Greens")
) 

(partyTree_4Pred_mapBadWestForest <- ggplot() +
    geom_point(data = partyTree_4Preds, aes(Long, Lat, col = newRegion), alpha = .1) +
    ggtitle("Severity of Western Forest misclassification") +
    geom_point(data = partyTree_4Preds[partyTree_4Preds$goodPred == FALSE & partyTree_4Preds$partyPrediction == "westForest",], 
               aes(Long, Lat, fill = partyPrediction.westForest),
               col = "black" ,
               shape = 21) +
    scale_fill_distiller(type = "seq", palette = "Blues")
) 

## arrange all of the figures for this model

ggpubr::annotate_figure(
  ggarrange(partyTree_4_varImpPlot,
  ggarrange(
    ggplotify::as.grob(gt::as_gtable(gt::gt(data.frame("dryShrubGrass" =  data.frame(partyTree_4_confMat)$Freq[1:3], 
                                                       "eastForest" = data.frame(partyTree_4_confMat)$Freq[4:6],
                                                       "westForest" = data.frame(partyTree_4_confMat)$Freq[7:9], 
                                                       row.names = c("dryShrubGrass", "eastForest", "westForest")), 
                                            rownames_to_stub = TRUE)))#, 
    # ggplotify::as.grob(gt::as_gtable(gt::gt(data.frame("inputs" = c("precip_driestMonth_meanAnnAvg"  ,   "PrecipTempCorr_meanAnnAvg"      ,
    #                                                                 "T_warmestMonth_meanAnnAvg"    ,"avgOrganicCarbonPerc_0_3cm" ,    
    #                                                                 "prcp_meanAnnTotal" )
    # ))))
    
  )
  ,
  partyTree_4PredMAP, 
  ggarrange((ggtern::arrangeGrob(partyTree_4_ternGoodPreds)), 
            (ggtern::arrangeGrob(partyTree_4_ternBadPreds))), 
  partyTree_4Pred_mapBadGrass, 
  partyTree_4Pred_mapBadEastForest, 
  partyTree_4Pred_mapBadWestForest, 
  nrow = 6
)#, 
#top = c("Model with depth = 7 and all climate and soil predictors")
) %>% 
  ggexport(
    filename = "./Figures/EcoRegionModelFigures/ModelFigures_depth7_TopFourPredictors.pdf",
    width = 12, height = 23)

# MODEL CURRENTLY USING AS BEST MODEL: Conditional Inference Regression Tree Depth = 6, with top 4 most important variables ------------
## at this point, none of the predictors are correlated anyway... 
## removed vp instead of prcp (which was technically the least important), since I don't think we'll use vp in the cover models
# try restricting the tree depth (too long right now)
partyTree_5 <- partykit::ctree(newRegion ~   precip_driestMonth_meanAnnAvg_30yr   + PrecipTempCorr_meanAnnAvg_30yr        +
                                 avgOrganicCarbonPerc_0_3cm + 
                                 annVPD_max_meanAnnAvg_30yr  ,
                               data = modDat_fit, control = partykit::ctree_control(maxdepth = 6))
partyTree_5

# save partyTree_5 model as an rds object to load into sensitivity analysis 
saveRDS(partyTree_5, "./Data_processed/EcoregionModelForTesting.rds")
# variable importance
varImp_temp <- data.frame("variable" = names(sort((varimp(partyTree_5)), decreasing = TRUE)),
                          "importance" = sort((varimp(partyTree_5)), decreasing = TRUE))
varImp <- varImp_temp  %>%
  # tibble::rownames_to_column() %>%
  # dplyr::rename("variable" = rowname) %>%
  dplyr::arrange(importance) %>%
  dplyr::mutate(variable = forcats::fct_inorder(variable))
partyTree_5_varImpPlot <- ggplot(varImp) +
  geom_segment(aes(x = variable, y = 0, xend = variable, yend = importance),
               size = 1.5, alpha = 0.7) +
  geom_point(aes(x = variable, y = importance, col = variable),
             size = 4, show.legend = F) +
  coord_flip() +
  theme_bw()

# predict categories based on this simple model
partyTree_5Preds <- cbind(predictionDat, "partyPrediction" = partykit::predict.party(partyTree_5, newdata = predictionDat, type = "response")) %>%
  cbind("partyPrediction" = partykit::predict.party(partyTree_5, newdata = predictionDat, type = "prob")) 
partyTree_5Preds$goodPred <- partyTree_5Preds$newRegion == partyTree_5Preds$partyPrediction
# percentage of misclassifications
sum(!partyTree_5Preds$goodPred)/nrow(partyTree_5Preds) * 100

(partyTree_5PredMAP <- ggplot(partyTree_5Preds) +
    geom_point(aes(Long, Lat, col = partyPrediction), alpha = .1) +
    ggtitle("model-predicted ecoregion classification -- Model with all possible climate predictors",
            subtitle = paste0("black dots indicate misclassification, \n fill color indicates which ecoregion a point was misclassified as; \n  ", round((100-sum(!partyTree_5Preds$goodPred)/nrow(partyTree_5Preds) * 100),1), "% classification accuracy" )) +
    geom_point(data = partyTree_5Preds[partyTree_5Preds$goodPred == FALSE,], aes(Long, Lat, fill = partyPrediction),
               col = "black"
               , shape = 21))

## confusion matrix
partyMatrix <- partyTree_5Preds[,c("newRegion", "partyPrediction")]
partyTree_5_confMat <- caret::confusionMatrix(data = partyTree_5Preds$partyPrediction,
                                              reference = partyTree_5Preds$newRegion)$table

## make ternary plots
# just good predictions
(partyTree_5_ternGoodPreds <- partyTree_5Preds %>% 
    filter(goodPred == TRUE) %>% 
    ggtern(aes(x=jitter(partyPrediction.dryShrubGrass*100 + 1, 50),
               y=jitter(partyPrediction.eastForest*100 + 1, 50),
               z=jitter(partyPrediction.westForest*100 + 1, 50), col = newRegion)) +
    geom_point(alpha = .2) +
    Tlab("Eastern Forest") +
    Llab("Shrub/\nGrass") + 
    Rlab("Western \nForest") +
    theme_minimal() + 
    ggtitle("Accurate Predictions"))
# just bad predictions
badDat <- partyTree_5Preds %>% 
  filter(goodPred == FALSE)
(partyTree_5_ternBadPreds <- 
    ggtern( data = badDat, aes(x=jitter(partyPrediction.dryShrubGrass*100 + 1, 50),
                               y=jitter(partyPrediction.eastForest*100 + 1, 50),
                               z=jitter(partyPrediction.westForest*100 + 1, 50), col = newRegion)) +
    geom_point(alpha = .2) +
    Tlab("Eastern Forest") +
    Llab("Shrub/\nGrass") + 
    Rlab("Western \nForest") +
    theme_minimal() + 
    ggtitle("Bad Predictions"))

# map of misclassified points 
partyTree_5Preds$color <- rgb(red = partyTree_5Preds$partyPrediction.dryShrubGrass, 
                              green = partyTree_5Preds$partyPrediction.eastForest, 
                              blue = partyTree_5Preds$partyPrediction.westForest)

(partyTree_5Pred_mapBadGrass <- ggplot() +
    geom_point(data = partyTree_5Preds, aes(Long, Lat, col = newRegion), alpha = .1) +
    ggtitle("Severity of Grass/Shrub misclassification") +
    geom_point(data = partyTree_5Preds[partyTree_5Preds$goodPred == FALSE & partyTree_5Preds$partyPrediction == "dryShrubGrass",], 
               aes(Long, Lat, fill = partyPrediction.dryShrubGrass),
               col = "black" ,
               shape = 21) +
    scale_fill_distiller(type = "seq", palette = "Reds")
)
(partyTree_5Pred_mapBadEastForest <- ggplot() +
    geom_point(data = partyTree_5Preds, aes(Long, Lat, col = newRegion), alpha = .1) +
    ggtitle("Severity of Eastern Forest misclassification") +
    geom_point(data = partyTree_5Preds[partyTree_5Preds$goodPred == FALSE & partyTree_5Preds$partyPrediction == "eastForest",], 
               aes(Long, Lat, fill = partyPrediction.eastForest),
               col = "black" ,
               shape = 21) +
    scale_fill_distiller(type = "seq", palette = "Greens")
) 

(partyTree_5Pred_mapBadWestForest <- ggplot() +
    geom_point(data = partyTree_5Preds, aes(Long, Lat, col = newRegion), alpha = .1) +
    ggtitle("Severity of Western Forest misclassification") +
    geom_point(data = partyTree_5Preds[partyTree_5Preds$goodPred == FALSE & partyTree_5Preds$partyPrediction == "westForest",], 
               aes(Long, Lat, fill = partyPrediction.westForest),
               col = "black" ,
               shape = 21) +
    scale_fill_distiller(type = "seq", palette = "Blues")
) 

## arrange all of the figures for this model

ggpubr::annotate_figure(ggarrange(partyTree_5_varImpPlot, 
  ggarrange(
    ggplotify::as.grob(gt::as_gtable(gt::gt(data.frame("dryShrubGrass" =  data.frame(partyTree_5_confMat)$Freq[1:3], 
                                                       "eastForest" = data.frame(partyTree_5_confMat)$Freq[4:6],
                                                       "westForest" = data.frame(partyTree_5_confMat)$Freq[7:9], 
                                                       row.names = c("dryShrubGrass", "eastForest", "westForest")), 
                                            rownames_to_stub = TRUE)))#, 
    # ggplotify::as.grob(gt::as_gtable(gt::gt(data.frame("inputs" = c("precip_driestMonth_meanAnnAvg"  ,   "PrecipTempCorr_meanAnnAvg"      ,
    #                                                                 "T_warmestMonth_meanAnnAvg"    ,"avgOrganicCarbonPerc_0_3cm" ,    
    #                                                                 "prcp_meanAnnTotal" )
    # ))))
    
  )
  ,
  partyTree_5PredMAP, 
  ggarrange((ggtern::arrangeGrob(partyTree_5_ternGoodPreds)), 
            (ggtern::arrangeGrob(partyTree_5_ternBadPreds))), 
  partyTree_5Pred_mapBadGrass, 
  partyTree_5Pred_mapBadEastForest, 
  partyTree_5Pred_mapBadWestForest, 
  nrow = 6
)#, 
#top = c("Model with depth = 7 and all climate and soil predictors")
) %>% 
  ggexport(
    filename = "./Figures/EcoRegionModelFigures/ModelFigures_depth6_TopFourPredictors.pdf",
    width = 12, height = 23)

# Conditional Inference Regression Tree Depth = 5, with top 4 most important variables ------------
## at this point, none of the predictors are correlated anyway... 
## removed vp instead of prcp (which was technically the least important), since I don't think we'll use vp in the cover models
# try restricting the tree depth (too long right now)
partyTree_6 <- partykit::ctree(newRegion ~   precip_driestMonth_meanAnnAvg_30yr   + PrecipTempCorr_meanAnnAvg_30yr        +
                                 avgOrganicCarbonPerc_0_3cm + 
                                 annVPD_max_meanAnnAvg_30yr  ,
                               data = modDat_fit, control = partykit::ctree_control(maxdepth = 5))
partyTree_6

# variable importance
varImp_temp <- data.frame("variable" = names(sort((varimp(partyTree_6)), decreasing = TRUE)),
                          "importance" = sort((varimp(partyTree_6)), decreasing = TRUE))
varImp <- varImp_temp  %>%
  # tibble::rownames_to_column() %>%
  # dplyr::rename("variable" = rowname) %>%
  dplyr::arrange(importance) %>%
  dplyr::mutate(variable = forcats::fct_inorder(variable))
partyTree_6_varImpPlot <- ggplot(varImp) +
  geom_segment(aes(x = variable, y = 0, xend = variable, yend = importance),
               size = 1.5, alpha = 0.7) +
  geom_point(aes(x = variable, y = importance, col = variable),
             size = 4, show.legend = F) +
  coord_flip() +
  theme_bw()

# predict categories based on this simple model
partyTree_6Preds <- cbind(predictionDat, "partyPrediction" = partykit::predict.party(partyTree_6, newdata = predictionDat, type = "response")) %>%
  cbind("partyPrediction" = partykit::predict.party(partyTree_6, newdata = predictionDat, type = "prob")) 
partyTree_6Preds$goodPred <- partyTree_6Preds$newRegion == partyTree_6Preds$partyPrediction
# percentage of misclassifications
sum(!partyTree_6Preds$goodPred)/nrow(partyTree_6Preds) * 100

(partyTree_6PredMAP <- ggplot(partyTree_6Preds) +
    geom_point(aes(Long, Lat, col = partyPrediction), alpha = .1) +
    ggtitle("model-predicted ecoregion classification -- Model with all possible climate predictors",
            subtitle = paste0("black dots indicate misclassification, \n fill color indicates which ecoregion a point was misclassified as; \n  ", round((100-sum(!partyTree_6Preds$goodPred)/nrow(partyTree_6Preds) * 100),1), "% classification accuracy" )) +
    geom_point(data = partyTree_6Preds[partyTree_6Preds$goodPred == FALSE,], aes(Long, Lat, fill = partyPrediction),
               col = "black"
               , shape = 21))

## confusion matrix
partyMatrix <- partyTree_6Preds[,c("newRegion", "partyPrediction")]
partyTree_6_confMat <- caret::confusionMatrix(data = partyTree_6Preds$partyPrediction,
                                              reference = partyTree_6Preds$newRegion)$table

## make ternary plots
# just good predictions
(partyTree_6_ternGoodPreds <- partyTree_6Preds %>% 
    filter(goodPred == TRUE) %>% 
    ggtern(aes(x=jitter(partyPrediction.dryShrubGrass*100 + 1, 50),
               y=jitter(partyPrediction.eastForest*100 + 1, 50),
               z=jitter(partyPrediction.westForest*100 + 1, 50), col = newRegion)) +
    geom_point(alpha = .2) +
    Tlab("Eastern Forest") +
    Llab("Shrub/\nGrass") + 
    Rlab("Western \nForest") +
    theme_minimal() + 
    ggtitle("Accurate Predictions"))
# just bad predictions
badDat <- partyTree_6Preds %>% 
  filter(goodPred == FALSE)
(partyTree_6_ternBadPreds <- 
    ggtern( data = badDat, aes(x=jitter(partyPrediction.dryShrubGrass*100 + 1, 50),
                               y=jitter(partyPrediction.eastForest*100 + 1, 50),
                               z=jitter(partyPrediction.westForest*100 + 1, 50), col = newRegion)) +
    geom_point(alpha = .2) +
    Tlab("Eastern Forest") +
    Llab("Shrub/\nGrass") + 
    Rlab("Western \nForest") +
    theme_minimal() + 
    ggtitle("Bad Predictions"))

# map of misclassified points 
partyTree_6Preds$color <- rgb(red = partyTree_6Preds$partyPrediction.dryShrubGrass, 
                              green = partyTree_6Preds$partyPrediction.eastForest, 
                              blue = partyTree_6Preds$partyPrediction.westForest)

(partyTree_6Pred_mapBadGrass <- ggplot() +
    geom_point(data = partyTree_6Preds, aes(Long, Lat, col = newRegion), alpha = .1) +
    ggtitle("Severity of Grass/Shrub misclassification") +
    geom_point(data = partyTree_6Preds[partyTree_6Preds$goodPred == FALSE & partyTree_6Preds$partyPrediction == "dryShrubGrass",], 
               aes(Long, Lat, fill = partyPrediction.dryShrubGrass),
               col = "black" ,
               shape = 21) +
    scale_fill_distiller(type = "seq", palette = "Reds")
)
(partyTree_6Pred_mapBadEastForest <- ggplot() +
    geom_point(data = partyTree_6Preds, aes(Long, Lat, col = newRegion), alpha = .1) +
    ggtitle("Severity of Eastern Forest misclassification") +
    geom_point(data = partyTree_6Preds[partyTree_6Preds$goodPred == FALSE & partyTree_6Preds$partyPrediction == "eastForest",], 
               aes(Long, Lat, fill = partyPrediction.eastForest),
               col = "black" ,
               shape = 21) +
    scale_fill_distiller(type = "seq", palette = "Greens")
) 

(partyTree_6Pred_mapBadWestForest <- ggplot() +
    geom_point(data = partyTree_6Preds, aes(Long, Lat, col = newRegion), alpha = .1) +
    ggtitle("Severity of Western Forest misclassification") +
    geom_point(data = partyTree_6Preds[partyTree_6Preds$goodPred == FALSE & partyTree_6Preds$partyPrediction == "westForest",], 
               aes(Long, Lat, fill = partyPrediction.westForest),
               col = "black" ,
               shape = 21) +
    scale_fill_distiller(type = "seq", palette = "Blues")
) 

## arrange all of the figures for this model

ggpubr::annotate_figure(ggarrange(partyTree_6_varImpPlot, 
                                  ggarrange(
                                    ggplotify::as.grob(gt::as_gtable(gt::gt(data.frame("dryShrubGrass" =  data.frame(partyTree_6_confMat)$Freq[1:3], 
                                                                                       "eastForest" = data.frame(partyTree_6_confMat)$Freq[4:6],
                                                                                       "westForest" = data.frame(partyTree_6_confMat)$Freq[7:9], 
                                                                                       row.names = c("dryShrubGrass", "eastForest", "westForest")), 
                                                                            rownames_to_stub = TRUE)))#, 
                                    # ggplotify::as.grob(gt::as_gtable(gt::gt(data.frame("inputs" = c("precip_driestMonth_meanAnnAvg"  ,   "PrecipTempCorr_meanAnnAvg"      ,
                                    #                                                                 "T_warmestMonth_meanAnnAvg"    ,"avgOrganicCarbonPerc_0_3cm" ,    
                                    #                                                                 "prcp_meanAnnTotal" )
                                    # ))))
                                    
                                  )
                                  ,
                                  partyTree_6PredMAP, 
                                  ggarrange((ggtern::arrangeGrob(partyTree_6_ternGoodPreds)), 
                                            (ggtern::arrangeGrob(partyTree_6_ternBadPreds))), 
                                  partyTree_6Pred_mapBadGrass, 
                                  partyTree_6Pred_mapBadEastForest, 
                                  partyTree_6Pred_mapBadWestForest, 
                                  nrow = 6
)#, 
#top = c("Model with depth = 7 and all climate and soil predictors")
) %>% 
  ggexport(
    filename = "./Figures/EcoRegionModelFigures/ModelFigures_depth5_TopFourPredictors.pdf",
    width = 12, height = 23)

# Conditional Inference Regression Tree Depth = 5, with top 3 most important variables ------------
## at this point, none of the predictors are correlated anyway... 
## removed vp instead of prcp (which was technically the least important), since I don't think we'll use vp in the cover models
# try restricting the tree depth (too long right now)
partyTree_7 <- partykit::ctree(newRegion ~  precip_driestMonth_meanAnnAvg_30yr   + PrecipTempCorr_meanAnnAvg_30yr        +
                                 T_warmestMonth_meanAnnAvg_30yr ,
                               data = modDat_fit, control = partykit::ctree_control(maxdepth = 5))
partyTree_7

# variable importance
varImp_temp <- data.frame("variable" = names(sort((varimp(partyTree_7)), decreasing = TRUE)),
                          "importance" = sort((varimp(partyTree_7)), decreasing = TRUE))
varImp <- varImp_temp  %>%
  # tibble::rownames_to_column() %>%
  # dplyr::rename("variable" = rowname) %>%
  dplyr::arrange(importance) %>%
  dplyr::mutate(variable = forcats::fct_inorder(variable))
partyTree_7_varImpPlot <- ggplot(varImp) +
  geom_segment(aes(x = variable, y = 0, xend = variable, yend = importance),
               size = 1.5, alpha = 0.7) +
  geom_point(aes(x = variable, y = importance, col = variable),
             size = 4, show.legend = F) +
  coord_flip() +
  theme_bw()

# predict categories based on this simple model
partyTree_7Preds <- cbind(predictionDat, "partyPrediction" = partykit::predict.party(partyTree_7, newdata = predictionDat, type = "response")) %>%
  cbind("partyPrediction" = partykit::predict.party(partyTree_7, newdata = predictionDat, type = "prob")) 
partyTree_7Preds$goodPred <- partyTree_7Preds$newRegion == partyTree_7Preds$partyPrediction
# percentage of misclassifications
sum(!partyTree_7Preds$goodPred)/nrow(partyTree_7Preds) * 100

(partyTree_7PredMAP <- ggplot(partyTree_7Preds) +
    geom_point(aes(Long, Lat, col = partyPrediction), alpha = .1) +
    ggtitle("model-predicted ecoregion classification -- Model with all possible climate predictors",
            subtitle = paste0("black dots indicate misclassification, \n fill color indicates which ecoregion a point was misclassified as; \n  ", round((100-sum(!partyTree_7Preds$goodPred)/nrow(partyTree_7Preds) * 100),1), "% classification accuracy" )) +
    geom_point(data = partyTree_7Preds[partyTree_7Preds$goodPred == FALSE,], aes(Long, Lat, fill = partyPrediction),
               col = "black"
               , shape = 21))

## confusion matrix
partyMatrix <- partyTree_7Preds[,c("newRegion", "partyPrediction")]
partyTree_7_confMat <- caret::confusionMatrix(data = partyTree_7Preds$partyPrediction,
                                              reference = partyTree_7Preds$newRegion)$table

## make ternary plots
# just good predictions
(partyTree_7_ternGoodPreds <- partyTree_7Preds %>% 
    filter(goodPred == TRUE) %>% 
    ggtern(aes(x=jitter(partyPrediction.dryShrubGrass*100 + 1, 50),
               y=jitter(partyPrediction.eastForest*100 + 1, 50),
               z=jitter(partyPrediction.westForest*100 + 1, 50), col = newRegion)) +
    geom_point(alpha = .2) +
    Tlab("Eastern Forest") +
    Llab("Shrub/\nGrass") + 
    Rlab("Western \nForest") +
    theme_minimal() + 
    ggtitle("Accurate Predictions"))
# just bad predictions
badDat <- partyTree_7Preds %>% 
  filter(goodPred == FALSE)
(partyTree_7_ternBadPreds <- 
    ggtern( data = badDat, aes(x=jitter(partyPrediction.dryShrubGrass*100 + 1, 50),
                               y=jitter(partyPrediction.eastForest*100 + 1, 50),
                               z=jitter(partyPrediction.westForest*100 + 1, 50), col = newRegion)) +
    geom_point(alpha = .2) +
    Tlab("Eastern Forest") +
    Llab("Shrub/\nGrass") + 
    Rlab("Western \nForest") +
    theme_minimal() + 
    ggtitle("Bad Predictions"))

# map of misclassified points 
partyTree_7Preds$color <- rgb(red = partyTree_7Preds$partyPrediction.dryShrubGrass, 
                              green = partyTree_7Preds$partyPrediction.eastForest, 
                              blue = partyTree_7Preds$partyPrediction.westForest)

(partyTree_7Pred_mapBadGrass <- ggplot() +
    geom_point(data = partyTree_7Preds, aes(Long, Lat, col = newRegion), alpha = .1) +
    ggtitle("Severity of Grass/Shrub misclassification") +
    geom_point(data = partyTree_7Preds[partyTree_7Preds$goodPred == FALSE & partyTree_7Preds$partyPrediction == "dryShrubGrass",], 
               aes(Long, Lat, fill = partyPrediction.dryShrubGrass),
               col = "black" ,
               shape = 21) +
    scale_fill_distiller(type = "seq", palette = "Reds")
)
(partyTree_7Pred_mapBadEastForest <- ggplot() +
    geom_point(data = partyTree_7Preds, aes(Long, Lat, col = newRegion), alpha = .1) +
    ggtitle("Severity of Eastern Forest misclassification") +
    geom_point(data = partyTree_7Preds[partyTree_7Preds$goodPred == FALSE & partyTree_7Preds$partyPrediction == "eastForest",], 
               aes(Long, Lat, fill = partyPrediction.eastForest),
               col = "black" ,
               shape = 21) +
    scale_fill_distiller(type = "seq", palette = "Greens")
) 

(partyTree_7Pred_mapBadWestForest <- ggplot() +
    geom_point(data = partyTree_7Preds, aes(Long, Lat, col = newRegion), alpha = .1) +
    ggtitle("Severity of Western Forest misclassification") +
    geom_point(data = partyTree_7Preds[partyTree_7Preds$goodPred == FALSE & partyTree_7Preds$partyPrediction == "westForest",], 
               aes(Long, Lat, fill = partyPrediction.westForest),
               col = "black" ,
               shape = 21) +
    scale_fill_distiller(type = "seq", palette = "Blues")
) 

## arrange all of the figures for this model

ggpubr::annotate_figure(ggarrange(partyTree_7_varImpPlot, 
                                  ggarrange(
                                    ggplotify::as.grob(gt::as_gtable(gt::gt(data.frame("dryShrubGrass" =  data.frame(partyTree_7_confMat)$Freq[1:3], 
                                                                                       "eastForest" = data.frame(partyTree_7_confMat)$Freq[4:6],
                                                                                       "westForest" = data.frame(partyTree_7_confMat)$Freq[7:9], 
                                                                                       row.names = c("dryShrubGrass", "eastForest", "westForest")), 
                                                                            rownames_to_stub = TRUE)))#, 
                                    # ggplotify::as.grob(gt::as_gtable(gt::gt(data.frame("inputs" = c("precip_driestMonth_meanAnnAvg"  ,   "PrecipTempCorr_meanAnnAvg"      ,
                                    #                                                                 "T_warmestMonth_meanAnnAvg"    ,"avgOrganicCarbonPerc_0_3cm" ,    
                                    #                                                                 "prcp_meanAnnTotal" )
                                    # ))))
                                    
                                  )
                                  ,
                                  partyTree_7PredMAP, 
                                  ggarrange((ggtern::arrangeGrob(partyTree_7_ternGoodPreds)), 
                                            (ggtern::arrangeGrob(partyTree_7_ternBadPreds))), 
                                  partyTree_7Pred_mapBadGrass, 
                                  partyTree_7Pred_mapBadEastForest, 
                                  partyTree_7Pred_mapBadWestForest, 
                                  nrow = 6
)#, 
#top = c("Model with depth = 7 and all climate and soil predictors")
) %>% 
  ggexport(
    filename = "./Figures/EcoRegionModelFigures/ModelFigures_depth5_TopThreePredictors.pdf",
    width = 12, height = 23)


# # Conditional Inference Regression Tree with tree Depth = 6 and top 7 most important variables ------------
# # try restricting the tree depth (too long right now)
# partyTree_7 <- partykit::ctree(newRegion ~  precip_driestMonth_meanAnnAvg_30yr   + PrecipTempCorr_meanAnnAvg_30yr        +
#                                  T_warmestMonth_meanAnnAvg_30yr    + avgOrganicCarbonPerc_0_3cm + 
#                                  vp_meanAnnAvg_30yr                      +  prcp_meanAnnTotal_30yr                 +
#                                  swe_meanAnnAvg_30yr,
#                                ,
#                                data = modDat_fit, control = partykit::ctree_control(maxdepth = 6))
# partyTree_7
# 
# # variable importance
# varImp_temp <- data.frame("variable" = names(sort((varimp(partyTree_7)), decreasing = TRUE)),
#                           "importance" = sort((varimp(partyTree_7)), decreasing = TRUE))
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
# # predict categories based on this simple model
# partyTree_6Preds <- cbind(predictionDat, "partyPrediction" = partykit::predict.party(partyTree_6, newdata = predictionDat, type = "response")) %>%
#   cbind("partyPrediction" = partykit::predict.party(partyTree_6, newdata = predictionDat, type = "prob")) 
# partyTree_6Preds$goodPred <- partyTree_6Preds$newRegion == partyTree_6Preds$partyPrediction
# # percentage of misclassifications
# sum(!partyTree_6Preds$goodPred)/nrow(partyTree_6Preds) * 100
# 
# (partyTree_6PredMAP <- ggplot(partyTree_6Preds) +
#     geom_point(aes(Long, Lat, col = partyPrediction), alpha = .1) +
#     ggtitle("model-predicted ecoregion classification -- Model with all possible climate predictors",
#             subtitle = paste0("black dots indicate misclassification, \n fill color indicates which ecoregion a point was misclassified as; \n  ", round((100-sum(!partyTree_6Preds$goodPred)/nrow(partyTree_6Preds) * 100),1), "% classification accuracy" )) +
#     geom_point(data = partyTree_6Preds[partyTree_6Preds$goodPred == FALSE,], aes(Long, Lat, fill = partyPrediction),
#                col = "black"
#                , shape = 21))
# 
# ## confusion matrix
# partyMatrix <- partyTree_6Preds[,c("newRegion", "partyPrediction")]
# partyTree_6_confMat <- caret::confusionMatrix(data = partyTree_6Preds$partyPrediction,
#                                               reference = partyTree_6Preds$newRegion)$table
# 
# ## make ternary plots
# # just good predictions
# (partyTree_6_ternGoodPreds <- partyTree_6Preds %>% 
#     filter(goodPred == TRUE) %>% 
#     ggtern(aes(x=jitter(partyPrediction.dryShrubGrass*100 + 1, 50),
#                y=jitter(partyPrediction.eastForest*100 + 1, 50),
#                z=jitter(partyPrediction.westForest*100 + 1, 50), col = newRegion)) +
#     geom_point(alpha = .2) +
#     Tlab("Eastern Forest") +
#     Llab("Shrub/\nGrass") + 
#     Rlab("Western \nForest") +
#     theme_minimal() + 
#     ggtitle("Accurate Predictions"))
# # just bad predictions
# badDat <- partyTree_6Preds %>% 
#   filter(goodPred == FALSE)
# (partyTree_6_ternBadPreds <- 
#     ggtern( data = badDat, aes(x=jitter(partyPrediction.dryShrubGrass*100 + 1, 50),
#                                y=jitter(partyPrediction.eastForest*100 + 1, 50),
#                                z=jitter(partyPrediction.westForest*100 + 1, 50), col = newRegion)) +
#     geom_point(alpha = .2) +
#     Tlab("Eastern Forest") +
#     Llab("Shrub/\nGrass") + 
#     Rlab("Western \nForest") +
#     theme_minimal() + 
#     ggtitle("Bad Predictions"))
# 
# # map of misclassified points 
# partyTree_6Preds$color <- rgb(red = partyTree_6Preds$partyPrediction.dryShrubGrass, 
#                               green = partyTree_6Preds$partyPrediction.eastForest, 
#                               blue = partyTree_6Preds$partyPrediction.westForest)
# 
# (partyTree_6Pred_mapBadGrass <- ggplot() +
#     geom_point(data = partyTree_6Preds, aes(Long, Lat, col = newRegion), alpha = .1) +
#     ggtitle("Severity of Grass/Shrub misclassification") +
#     geom_point(data = partyTree_6Preds[partyTree_6Preds$goodPred == FALSE & partyTree_6Preds$partyPrediction == "dryShrubGrass",], 
#                aes(Long, Lat, fill = partyPrediction.dryShrubGrass),
#                col = "black" ,
#                shape = 21) +
#     scale_fill_distiller(type = "seq", palette = "Reds")
# )
# (partyTree_6Pred_mapBadEastForest <- ggplot() +
#     geom_point(data = partyTree_6Preds, aes(Long, Lat, col = newRegion), alpha = .1) +
#     ggtitle("Severity of Eastern Forest misclassification") +
#     geom_point(data = partyTree_6Preds[partyTree_6Preds$goodPred == FALSE & partyTree_6Preds$partyPrediction == "eastForest",], 
#                aes(Long, Lat, fill = partyPrediction.eastForest),
#                col = "black" ,
#                shape = 21) +
#     scale_fill_distiller(type = "seq", palette = "Greens")
# ) 
# 
# (partyTree_6Pred_mapBadWestForest <- ggplot() +
#     geom_point(data = partyTree_6Preds, aes(Long, Lat, col = newRegion), alpha = .1) +
#     ggtitle("Severity of Western Forest misclassification") +
#     geom_point(data = partyTree_6Preds[partyTree_6Preds$goodPred == FALSE & partyTree_6Preds$partyPrediction == "westForest",], 
#                aes(Long, Lat, fill = partyPrediction.westForest),
#                col = "black" ,
#                shape = 21) +
#     scale_fill_distiller(type = "seq", palette = "Blues")
# ) 
# 
# ## arrange all of the figures for this model
# 
# ggpubr::annotate_figure(ggarrange(
#   ggarrange(
#     ggplotify::as.grob(gt::as_gtable(gt::gt(data.frame("dryShrubGrass" =  data.frame(partyTree_6_confMat)$Freq[1:3], 
#                                                        "eastForest" = data.frame(partyTree_6_confMat)$Freq[4:6],
#                                                        "westForest" = data.frame(partyTree_6_confMat)$Freq[7:9], 
#                                                        row.names = c("dryShrubGrass", "eastForest", "westForest")), 
#                                             rownames_to_stub = TRUE))), 
#     ggplotify::as.grob(gt::as_gtable(gt::gt(data.frame("inputs" = c("precip_driestMonth_meanAnnAvg"  ,   "PrecipTempCorr_meanAnnAvg"      ,
#                                                                     "T_warmestMonth_meanAnnAvg"    ,"avgOrganicCarbonPerc_0_3cm" ,    
#                                                                     "vp_meanAnnAvg", "prcp_meanAnnTotal", 
#                                                                     "swe_meanAnnAvg" )
#     ))))
#     
#   )
#   ,
#   partyTree_6PredMAP, 
#   ggarrange((ggtern::arrangeGrob(partyTree_6_ternGoodPreds)), 
#             (ggtern::arrangeGrob(partyTree_6_ternBadPreds))), 
#   partyTree_6Pred_mapBadGrass, 
#   partyTree_6Pred_mapBadEastForest, 
#   partyTree_6Pred_mapBadWestForest, 
#   nrow = 6
# )#, 
# #top = c("Model with depth = 7 and all climate and soil predictors")
# ) %>% 
#   ggexport(
#     filename = "./Figures/EcoRegionModelFigures/ModelFigures_depth6_TopSevenPredictors.pdf",
#     width = 12, height = 23)

# # Conditional Inference Tree w/ swe, awdd, and soil depth removed --------------
# # try restricting the tree depth (too long right now)
# partyTree_5 <- partykit::ctree(newRegion ~
#                                tmean_meanAnnAvg_30yr + isothermality_meanAnnAvg_30yr + prcp_meanAnnTotal_30yr +
#                                PrecipTempCorr_meanAnnAvg_30yr +  surfaceClay_perc +
#                                avgSandPerc_acrossDepth + avgCoarsePerc_acrossDepth +
#                                avgOrganicCarbonPerc_0_3cm + totalAvailableWaterHoldingCapacity,
#                              data = modDat_fit, control = partykit::ctree_control(maxdepth = 7))
# partyTree_5
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
# #plot(partyTree_2, main="Conditional Inference Tree for ecoregions")
# 
# # predict categories based on this simple model
# partyTree_2Preds <- cbind(predictionDat, "partyPrediction" = partykit::predict.party(partyTree_2, newdata = predictionDat, type = "response")) %>%
#   cbind("partyPrediction" = partykit::predict.party(partyTree_2, newdata = predictionDat, type = "prob")) %>% 
#   cbind(modDat_test %>%
#           dplyr::select(Long, Lat))
# partyTree_2Preds$goodPred <- partyTree_2Preds$newRegion == partyTree_2Preds$partyPrediction
# # percentage of misclassifications
# sum(!partyTree_2Preds$goodPred)/nrow(partyTree_2Preds) * 100
# 
# (partyTree_2PredMAP <- ggplot(partyTree_2Preds) +
#   geom_point(aes(Long, Lat, col = partyPrediction), alpha = .1) +
#   ggtitle("model-predicted ecoregion classification",
#           subtitle = paste0("black dots indicate misclassification, \n fill color indicates which ecoregion a point was misclassified as; \n  ", round((100-sum(!partyTree_2Preds$goodPred)/nrow(partyTree_2Preds) * 100),1), "% classification accuracy" )) +
#   geom_point(data = partyTree_2Preds[partyTree_2Preds$goodPred == FALSE,], aes(Long, Lat, fill = partyPrediction),
#              col = "black"
#              , shape = 21))
# 
# ## confusion matrix
# partyMatrix <- partyTree_2Preds[,c("newRegion", "partyPrediction")]
# partyTree_2_confMat <- caret::confusionMatrix(data = partyTree_2Preds$partyPrediction,
#                        reference = partyTree_2Preds$newRegion)$table
# 
# ## make ternary plots
# (partyTree_2_ternAllPreds <- ggtern(data= partyTree_2Preds, 
#                                     aes(x=jitter(partyPrediction.dryShrubGrass*100 + 1, 50),
#                                         y=jitter(partyPrediction.eastForest*100 + 1, 50),
#                                         z=jitter(partyPrediction.westForest*100 + 1, 50), col = newRegion)) +
#     geom_point(alpha = .2) +
#     Tlab("Eastern Forest") +
#   Llab("Shrub/\nGrass") + 
#   Rlab("Western \nForest") +
#   theme_minimal() + 
#   ggtitle("All Predictions")) 
# # just good predictions
# (partyTree_2_ternGoodPreds <- partyTree_2Preds %>% 
#     filter(goodPred == TRUE) %>% 
#     ggtern(aes(x=jitter(partyPrediction.dryShrubGrass*100 + 1, 50),
#                y=jitter(partyPrediction.eastForest*100 + 1, 50),
#                z=jitter(partyPrediction.westForest*100 + 1, 50), col = newRegion)) +
#     geom_point(alpha = .2) +
#     Tlab("Eastern Forest") +
#     Llab("Shrub/\nGrass") + 
#     Rlab("Western \nForest") +
#     theme_minimal() + 
#     ggtitle("Accurate Predictions"))
# # just bad predictions
# badDat <- partyTree_2Preds %>% 
#   filter(goodPred == FALSE)
# (partyTree_2_ternBadPreds <- 
#     ggtern( data = badDat, aes(x=jitter(partyPrediction.dryShrubGrass*100 + 1, 50),
#                                y=jitter(partyPrediction.eastForest*100 + 1, 50),
#                                z=jitter(partyPrediction.westForest*100 + 1, 50), col = newRegion)) +
#     geom_point(alpha = .2) +
#     Tlab("Eastern Forest") +
#     Llab("Shrub/\nGrass") + 
#     Rlab("Western \nForest") +
#     theme_minimal() + 
#     ggtitle("Bad Predictions"))
# 
# # map of misclassified points 
# partyTree_2Preds$color <- rgb(red = partyTree_2Preds$partyPrediction.dryShrubGrass, 
#                               green = partyTree_2Preds$partyPrediction.eastForest, 
#                               blue = partyTree_2Preds$partyPrediction.westForest)
# 
# (partyTree_2Pred_mapBadGrass <- ggplot() +
#     geom_point(data = partyTree_2Preds, aes(Long, Lat, col = newRegion), alpha = .1) +
#     ggtitle("Severity of Grass/Shrub misclassification") +
#     geom_point(data = partyTree_2Preds[partyTree_2Preds$goodPred == FALSE & partyTree_2Preds$partyPrediction == "dryShrubGrass",], 
#                aes(Long, Lat, fill = partyPrediction.dryShrubGrass),
#                col = "black" ,
#                 shape = 21) +
#     scale_fill_distiller(type = "seq", palette = "Reds")
# )
# (partyTree_2Pred_mapBadEastForest <- ggplot() +
#     geom_point(data = partyTree_2Preds, aes(Long, Lat, col = newRegion), alpha = .1) +
#     ggtitle("Severity of Eastern Forest misclassification") +
#     geom_point(data = partyTree_2Preds[partyTree_2Preds$goodPred == FALSE & partyTree_2Preds$partyPrediction == "eastForest",], 
#                aes(Long, Lat, fill = partyPrediction.eastForest),
#                col = "black" ,
#                shape = 21) +
#     scale_fill_distiller(type = "seq", palette = "Greens")
# ) 
# 
# (partyTree_2Pred_mapBadWestForest <- ggplot() +
#     geom_point(data = partyTree_2Preds, aes(Long, Lat, col = newRegion), alpha = .1) +
#     ggtitle("Severity of Western Forest misclassification") +
#     geom_point(data = partyTree_2Preds[partyTree_2Preds$goodPred == FALSE & partyTree_2Preds$partyPrediction == "westForest",], 
#                aes(Long, Lat, fill = partyPrediction.westForest),
#                col = "black" ,
#                shape = 21) +
#     scale_fill_distiller(type = "seq", palette = "Blues")
# ) 
# 
# ## arrange all of the figures for this model
# 
# ggpubr::annotate_figure(ggarrange(partyTree_2PredMAP, 
#                                   ggarrange((ggtern::arrangeGrob(partyTree_2_ternGoodPreds)), 
#                                              (ggtern::arrangeGrob(partyTree_2_ternBadPreds))), 
#                                   partyTree_2Pred_mapBadGrass, 
#                                   partyTree_2Pred_mapBadEastForest, 
#                                   partyTree_2Pred_mapBadWestForest, 
#                                   ggplotify::as.grob(gt::as_gtable(gt::gt(data.frame("dryShrubGrass" = c(17331, 518, 1053), 
#                                                 "eastForest" = c(176, 8563, 3),
#                                                 "westForest" = c(1408, 19, 8669), 
#                                                 row.names = c("dryShrubGrass", "eastForest", "westForest")), 
#                                                 rownames_to_stub = TRUE))),
#                                   nrow = 6
#                                   ), 
#                         top = "Model with depth = 7 and SWE, AWDD, and Soil Depth removed") %>% 
#   ggexport(
#     filename = "./Figures/EcoRegionModelFigures/ModelFigures_depth7_noSWEnoAWDDnoSoilDepth.pdf",
#     width = 11, height = 24)
# 
# 
# # Conditional Inference Tree w/ swe, awdd, total available water holding capacity and soil depth removed --------------
# # try restricting the tree depth (too long right now)
# partyTree_3 <- partykit::ctree(newRegion ~
#                                  tmean_meanAnnAvg_30yr + isothermality_meanAnnAvg_30yr + prcp_meanAnnTotal_30yr +
#                                  PrecipTempCorr_meanAnnAvg_30yr +  surfaceClay_perc +
#                                  avgSandPerc_acrossDepth + avgCoarsePerc_acrossDepth +
#                                  avgOrganicCarbonPerc_0_3cm ,
#                                data = modDat_fit, control = partykit::ctree_control(maxdepth = 7))
# partyTree_3
# # variable importance
# varImp_temp <- data.frame("variable" = names(sort((varimp(partyTree_3)), decreasing = TRUE)),
#                           "importance" = sort((varimp(partyTree_3)), decreasing = TRUE))
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
# #plot(partyTree_3, main="Conditional Inference Tree for ecoregions")
# 
# # predict categories based on this simple model
# partyTree_3Preds <- cbind(predictionDat, "partyPrediction" = partykit::predict.party(partyTree_3, newdata = predictionDat, type = "response")) %>%
#   cbind("partyPrediction" = partykit::predict.party(partyTree_3, newdata = predictionDat, type = "prob")) %>% 
#   cbind(modDat_test %>%
#           dplyr::select(Long, Lat))
# partyTree_3Preds$goodPred <- partyTree_3Preds$newRegion == partyTree_3Preds$partyPrediction
# # percentage of misclassifications
# sum(!partyTree_3Preds$goodPred)/nrow(partyTree_3Preds) * 100
# 
# (partyTree_3PredMAP <- ggplot(partyTree_3Preds) +
#     geom_point(aes(Long, Lat, col = partyPrediction), alpha = .1) +
#     ggtitle("model-predicted ecoregion classification",
#             subtitle = paste0("black dots indicate misclassification, \n fill color indicates which ecoregion a point was misclassified as; \n  ", round((100-sum(!partyTree_3Preds$goodPred)/nrow(partyTree_3Preds) * 100),1), "% classification accuracy" )) +
#     geom_point(data = partyTree_3Preds[partyTree_3Preds$goodPred == FALSE,], aes(Long, Lat, fill = partyPrediction),
#                col = "black"
#                , shape = 21))
# 
# ## confusion matrix
# partyMatrix <- partyTree_3Preds[,c("newRegion", "partyPrediction")]
# partyTree_3_confMat <- caret::confusionMatrix(data = partyTree_3Preds$partyPrediction,
#                                               reference = partyTree_3Preds$newRegion)$table
# 
# 
# (partyTree_3_ternAllPreds <- ggtern(data= partyTree_3Preds, 
#                                     aes(x=jitter(partyPrediction.dryShrubGrass*100 + 1, 50),
#                                         y=jitter(partyPrediction.eastForest*100 + 1, 50),
#                                         z=jitter(partyPrediction.westForest*100 + 1, 50), col = newRegion)) +
#     geom_point(alpha = .2) +
#     Tlab("Eastern Forest") +
#     Llab("Shrub/\nGrass") + 
#     Rlab("Western \nForest") +
#     theme_minimal() + 
#     ggtitle("All Predictions")) 
# # just good predictions
# (partyTree_3_ternGoodPreds <- partyTree_3Preds %>% 
#     filter(goodPred == TRUE) %>% 
#     ggtern(aes(x=jitter(partyPrediction.dryShrubGrass*100 + 1, 50),
#                y=jitter(partyPrediction.eastForest*100 + 1, 50),
#                z=jitter(partyPrediction.westForest*100 + 1, 50), col = newRegion)) +
#     geom_point(alpha = .2) +
#     Tlab("Eastern Forest") +
#     Llab("Shrub/\nGrass") + 
#     Rlab("Western \nForest") +
#     theme_minimal() + 
#     ggtitle("Accurate Predictions"))
# # just bad predictions
# badDat <- partyTree_3Preds %>% 
#   filter(goodPred == FALSE)
# (partyTree_3_ternBadPreds <- 
#     ggtern( data = badDat, aes(x=jitter(partyPrediction.dryShrubGrass*100 + 1, 50),
#                                y=jitter(partyPrediction.eastForest*100 + 1, 50),
#                                z=jitter(partyPrediction.westForest*100 + 1, 50), col = newRegion)) +
#     geom_point(alpha = .2) +
#     Tlab("Eastern Forest") +
#     Llab("Shrub/\nGrass") + 
#     Rlab("Western \nForest") +
#     theme_minimal() + 
#     ggtitle("Bad Predictions"))
# 
# # map of misclassified points 
# partyTree_3Preds$color <- rgb(red = partyTree_3Preds$partyPrediction.dryShrubGrass, 
#                               green = partyTree_3Preds$partyPrediction.eastForest, 
#                               blue = partyTree_3Preds$partyPrediction.westForest)
# 
# (partyTree_3Pred_mapBadGrass <- ggplot() +
#     geom_point(data = partyTree_3Preds, aes(Long, Lat, col = newRegion), alpha = .1) +
#     ggtitle("Severity of Grass/Shrub misclassification") +
#     geom_point(data = partyTree_3Preds[partyTree_3Preds$goodPred == FALSE & partyTree_3Preds$partyPrediction == "dryShrubGrass",], 
#                aes(Long, Lat, fill = partyPrediction.dryShrubGrass),
#                col = "black" ,
#                shape = 21) +
#     scale_fill_distiller(type = "seq", palette = "Reds")
# )
# (partyTree_3Pred_mapBadEastForest <- ggplot() +
#     geom_point(data = partyTree_3Preds, aes(Long, Lat, col = newRegion), alpha = .1) +
#     ggtitle("Severity of Eastern Forest misclassification") +
#     geom_point(data = partyTree_3Preds[partyTree_3Preds$goodPred == FALSE & partyTree_3Preds$partyPrediction == "eastForest",], 
#                aes(Long, Lat, fill = partyPrediction.eastForest),
#                col = "black" ,
#                shape = 21) +
#     scale_fill_distiller(type = "seq", palette = "Greens")
# ) 
# 
# (partyTree_3Pred_mapBadWestForest <- ggplot() +
#     geom_point(data = partyTree_3Preds, aes(Long, Lat, col = newRegion), alpha = .1) +
#     ggtitle("Severity of Western Forest misclassification") +
#     geom_point(data = partyTree_3Preds[partyTree_3Preds$goodPred == FALSE & partyTree_3Preds$partyPrediction == "westForest",], 
#                aes(Long, Lat, fill = partyPrediction.westForest),
#                col = "black" ,
#                shape = 21) +
#     scale_fill_distiller(type = "seq", palette = "Blues")
# ) 
# 
# ## arrange all of the figures for this model
# 
# ggpubr::annotate_figure(ggarrange(partyTree_3PredMAP, 
#                                   ggarrange((ggtern::arrangeGrob(partyTree_3_ternGoodPreds)), 
#                                             (ggtern::arrangeGrob(partyTree_3_ternBadPreds))), 
#                                   partyTree_3Pred_mapBadGrass, 
#                                   partyTree_3Pred_mapBadEastForest, 
#                                   partyTree_3Pred_mapBadWestForest, 
#                                   ggplotify::as.grob(gt::as_gtable(gt::gt(data.frame("dryShrubGrass" = c(17262, 518, 1122), 
#                                                                                      "eastForest" = c(176, 8558, 8),
#                                                                                      "westForest" = c(1340, 17, 8739), 
#                                                                                      row.names = c("dryShrubGrass", "eastForest", "westForest")), 
#                                                                           rownames_to_stub = TRUE))),
#                                   nrow = 6
# ), 
# top = "Model with depth = 7 and SWE, AWDD, total water-holding capacity and Soil Depth removed") %>% 
#   ggexport(
#     filename = "./Figures/EcoRegionModelFigures/ModelFigures_depth7_noSWEnoAWDDnoWaterHoldingCapacitynoSoilDepth.pdf",
#     width = 11, height = 24)
# 
# # Conditional Inference Tree w/ swe, awdd, total available water holding capacity, sand, and soil depth removed --------------
# # try restricting the tree depth (too long right now)
# partyTree_4 <- partykit::ctree(newRegion ~
#                                  tmean_meanAnnAvg_30yr + isothermality_meanAnnAvg_30yr + prcp_meanAnnTotal_30yr +
#                                  PrecipTempCorr_meanAnnAvg_30yr +  surfaceClay_perc +
#                                   avgCoarsePerc_acrossDepth +
#                                  avgOrganicCarbonPerc_0_3cm ,
#                                data = modDat_fit, control = partykit::ctree_control(maxdepth = 7))
# partyTree_4
# # variable importance
# varImp_temp <- data.frame("variable" = names(sort((varimp(partyTree_4)), decreasing = TRUE)),
#                           "importance" = sort((varimp(partyTree_4)), decreasing = TRUE))
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
# #plot(partyTree_4, main="Conditional Inference Tree for ecoregions")
# 
# # predict categories based on this simple model
# partyTree_4Preds <- cbind(predictionDat, "partyPrediction" = partykit::predict.party(partyTree_4, newdata = predictionDat, type = "response")) %>%
#   cbind("partyPrediction" = partykit::predict.party(partyTree_4, newdata = predictionDat, type = "prob")) %>% 
#   cbind(modDat_test %>%
#           dplyr::select(Long, Lat))
# partyTree_4Preds$goodPred <- partyTree_4Preds$newRegion == partyTree_4Preds$partyPrediction
# # percentage of misclassifications
# sum(!partyTree_4Preds$goodPred)/nrow(partyTree_4Preds) * 100
# 
# (partyTree_4PredMAP <- ggplot(partyTree_4Preds) +
#     geom_point(aes(Long, Lat, col = partyPrediction), alpha = .1) +
#     ggtitle("model-predicted ecoregion classification",
#             subtitle = paste0("black dots indicate misclassification, \n fill color indicates which ecoregion a point was misclassified as; \n  ", round((100-sum(!partyTree_4Preds$goodPred)/nrow(partyTree_4Preds) * 100),1), "% classification accuracy" )) +
#     geom_point(data = partyTree_4Preds[partyTree_4Preds$goodPred == FALSE,], aes(Long, Lat, fill = partyPrediction),
#                col = "black"
#                , shape = 21))
# 
# ## confusion matrix
# partyMatrix <- partyTree_4Preds[,c("newRegion", "partyPrediction")]
# partyTree_4_confMat <- caret::confusionMatrix(data = partyTree_4Preds$partyPrediction,
#                                               reference = partyTree_4Preds$newRegion)$table
# 
# 
# (partyTree_4_ternAllPreds <- ggtern(data= partyTree_4Preds, 
#                                     aes(x=jitter(partyPrediction.dryShrubGrass*100 + 1, 50),
#                                         y=jitter(partyPrediction.eastForest*100 + 1, 50),
#                                         z=jitter(partyPrediction.westForest*100 + 1, 50), col = newRegion)) +
#     geom_point(alpha = .2) +
#     Tlab("Eastern Forest") +
#     Llab("Shrub/\nGrass") + 
#     Rlab("Western \nForest") +
#     theme_minimal() + 
#     ggtitle("All Predictions")) 
# # just good predictions
# (partyTree_4_ternGoodPreds <- partyTree_4Preds %>% 
#     filter(goodPred == TRUE) %>% 
#     ggtern(aes(x=jitter(partyPrediction.dryShrubGrass*100 + 1, 50),
#                y=jitter(partyPrediction.eastForest*100 + 1, 50),
#                z=jitter(partyPrediction.westForest*100 + 1, 50), col = newRegion)) +
#     geom_point(alpha = .2) +
#     Tlab("Eastern Forest") +
#     Llab("Shrub/\nGrass") + 
#     Rlab("Western \nForest") +
#     theme_minimal() + 
#     ggtitle("Accurate Predictions"))
# # just bad predictions
# badDat <- partyTree_4Preds %>% 
#   filter(goodPred == FALSE)
# (partyTree_4_ternBadPreds <- 
#     ggtern( data = badDat, aes(x=jitter(partyPrediction.dryShrubGrass*100 + 1, 50),
#                                y=jitter(partyPrediction.eastForest*100 + 1, 50),
#                                z=jitter(partyPrediction.westForest*100 + 1, 50), col = newRegion)) +
#     geom_point(alpha = .2) +
#     Tlab("Eastern Forest") +
#     Llab("Shrub/\nGrass") + 
#     Rlab("Western \nForest") +
#     theme_minimal() + 
#     ggtitle("Bad Predictions"))
# 
# # map of misclassified points 
# partyTree_4Preds$color <- rgb(red = partyTree_4Preds$partyPrediction.dryShrubGrass, 
#                               green = partyTree_4Preds$partyPrediction.eastForest, 
#                               blue = partyTree_4Preds$partyPrediction.westForest)
# 
# (partyTree_4Pred_mapBadGrass <- ggplot() +
#     geom_point(data = partyTree_4Preds, aes(Long, Lat, col = newRegion), alpha = .1) +
#     ggtitle("Severity of Grass/Shrub misclassification") +
#     geom_point(data = partyTree_4Preds[partyTree_4Preds$goodPred == FALSE & partyTree_4Preds$partyPrediction == "dryShrubGrass",], 
#                aes(Long, Lat, fill = partyPrediction.dryShrubGrass),
#                col = "black" ,
#                shape = 21) +
#     scale_fill_distiller(type = "seq", palette = "Reds")
# )
# (partyTree_4Pred_mapBadEastForest <- ggplot() +
#     geom_point(data = partyTree_4Preds, aes(Long, Lat, col = newRegion), alpha = .1) +
#     ggtitle("Severity of Eastern Forest misclassification") +
#     geom_point(data = partyTree_4Preds[partyTree_4Preds$goodPred == FALSE & partyTree_4Preds$partyPrediction == "eastForest",], 
#                aes(Long, Lat, fill = partyPrediction.eastForest),
#                col = "black" ,
#                shape = 21) +
#     scale_fill_distiller(type = "seq", palette = "Greens")
# ) 
# 
# (partyTree_4Pred_mapBadWestForest <- ggplot() +
#     geom_point(data = partyTree_4Preds, aes(Long, Lat, col = newRegion), alpha = .1) +
#     ggtitle("Severity of Western Forest misclassification") +
#     geom_point(data = partyTree_4Preds[partyTree_4Preds$goodPred == FALSE & partyTree_4Preds$partyPrediction == "westForest",], 
#                aes(Long, Lat, fill = partyPrediction.westForest),
#                col = "black" ,
#                shape = 21) +
#     scale_fill_distiller(type = "seq", palette = "Blues")
# ) 
# 
# ## arrange all of the figures for this model
# 
# ggpubr::annotate_figure(ggarrange(partyTree_4PredMAP, 
#                                   ggarrange((ggtern::arrangeGrob(partyTree_4_ternGoodPreds)), 
#                                             (ggtern::arrangeGrob(partyTree_4_ternBadPreds))), 
#                                   partyTree_4Pred_mapBadGrass, 
#                                   partyTree_4Pred_mapBadEastForest, 
#                                   partyTree_4Pred_mapBadWestForest, 
#                                   ggplotify::as.grob(gt::as_gtable(gt::gt(data.frame("dryShrubGrass" = c(17434, 517, 951), 
#                                                                                      "eastForest" = c(176, 8555, 11),
#                                                                                      "westForest" = c(1572, 14, 8510), 
#                                                                                      row.names = c("dryShrubGrass", "eastForest", "westForest")), 
#                                                                           rownames_to_stub = TRUE))),
#                                   nrow = 6
# ), 
# top = "Model with depth = 7 and SWE, AWDD, total water-holding capacity, sand, and Soil Depth removed") %>% 
#   ggexport(
#     filename = "./Figures/EcoRegionModelFigures/ModelFigures_depth7_noSWEnoAWDDnoWaterHoldingCapacitynoSANDnoSoilDepth.pdf",
#     width = 11, height = 24)
# 
# # Conditional Inference Tree w/ swe, awdd, total available water holding capacity, sand, tmean, and soil depth removed --------------
# # try restricting the tree depth (too long right now)
# partyTree_5 <- partykit::ctree(newRegion ~ isothermality_meanAnnAvg_30yr + prcp_meanAnnTotal_30yr +
#                                  PrecipTempCorr_meanAnnAvg_30yr +  surfaceClay_perc +
#                                  avgCoarsePerc_acrossDepth +
#                                  avgOrganicCarbonPerc_0_3cm ,
#                                data = modDat_fit, control = partykit::ctree_control(maxdepth = 7))
# partyTree_5
# # variable importance
# varImp_temp <- data.frame("variable" = names(sort((varimp(partyTree_5)), decreasing = TRUE)),
#                           "importance" = sort((varimp(partyTree_5)), decreasing = TRUE))
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
# #plot(partyTree_5, main="Conditional Inference Tree for ecoregions")
# 
# # predict categories based on this simple model
# partyTree_5Preds <- cbind(predictionDat, "partyPrediction" = partykit::predict.party(partyTree_5, newdata = predictionDat, type = "response")) %>%
#   cbind("partyPrediction" = partykit::predict.party(partyTree_5, newdata = predictionDat, type = "prob")) %>% 
#   cbind(modDat_test %>%
#           dplyr::select(Long, Lat))
# partyTree_5Preds$goodPred <- partyTree_5Preds$newRegion == partyTree_5Preds$partyPrediction
# # percentage of misclassifications
# sum(!partyTree_5Preds$goodPred)/nrow(partyTree_5Preds) * 100
# 
# (partyTree_5PredMAP <- ggplot(partyTree_5Preds) +
#     geom_point(aes(Long, Lat, col = partyPrediction), alpha = .1) +
#     ggtitle("model-predicted ecoregion classification",
#             subtitle = paste0("black dots indicate misclassification, \n fill color indicates which ecoregion a point was misclassified as; \n  ", round((100-sum(!partyTree_5Preds$goodPred)/nrow(partyTree_5Preds) * 100),1), "% classification accuracy" )) +
#     geom_point(data = partyTree_5Preds[partyTree_5Preds$goodPred == FALSE,], aes(Long, Lat, fill = partyPrediction),
#                col = "black"
#                , shape = 21))
# 
# ## confusion matrix
# partyMatrix <- partyTree_5Preds[,c("newRegion", "partyPrediction")]
# partyTree_5_confMat <- caret::confusionMatrix(data = partyTree_5Preds$partyPrediction,
#                                               reference = partyTree_5Preds$newRegion)$table
# 
# 
# (partyTree_5_ternAllPreds <- ggtern(data= partyTree_5Preds, 
#                                     aes(x=jitter(partyPrediction.dryShrubGrass*100 + 1, 50),
#                                         y=jitter(partyPrediction.eastForest*100 + 1, 50),
#                                         z=jitter(partyPrediction.westForest*100 + 1, 50), col = newRegion)) +
#     geom_point(alpha = .2) +
#     Tlab("Eastern Forest") +
#     Llab("Shrub/\nGrass") + 
#     Rlab("Western \nForest") +
#     theme_minimal() + 
#     ggtitle("All Predictions")) 
# # just good predictions
# (partyTree_5_ternGoodPreds <- partyTree_5Preds %>% 
#     filter(goodPred == TRUE) %>% 
#     ggtern(aes(x=jitter(partyPrediction.dryShrubGrass*100 + 1, 50),
#                y=jitter(partyPrediction.eastForest*100 + 1, 50),
#                z=jitter(partyPrediction.westForest*100 + 1, 50), col = newRegion)) +
#     geom_point(alpha = .2) +
#     Tlab("Eastern Forest") +
#     Llab("Shrub/\nGrass") + 
#     Rlab("Western \nForest") +
#     theme_minimal() + 
#     ggtitle("Accurate Predictions"))
# # just bad predictions
# badDat <- partyTree_5Preds %>% 
#   filter(goodPred == FALSE)
# (partyTree_5_ternBadPreds <- 
#     ggtern( data = badDat, aes(x=jitter(partyPrediction.dryShrubGrass*100 + 1, 50),
#                                y=jitter(partyPrediction.eastForest*100 + 1, 50),
#                                z=jitter(partyPrediction.westForest*100 + 1, 50), col = newRegion)) +
#     geom_point(alpha = .2) +
#     Tlab("Eastern Forest") +
#     Llab("Shrub/\nGrass") + 
#     Rlab("Western \nForest") +
#     theme_minimal() + 
#     ggtitle("Bad Predictions"))
# 
# # map of misclassified points 
# partyTree_5Preds$color <- rgb(red = partyTree_5Preds$partyPrediction.dryShrubGrass, 
#                               green = partyTree_5Preds$partyPrediction.eastForest, 
#                               blue = partyTree_5Preds$partyPrediction.westForest)
# 
# (partyTree_5Pred_mapBadGrass <- ggplot() +
#     geom_point(data = partyTree_5Preds, aes(Long, Lat, col = newRegion), alpha = .1) +
#     ggtitle("Severity of Grass/Shrub misclassification") +
#     geom_point(data = partyTree_5Preds[partyTree_5Preds$goodPred == FALSE & partyTree_5Preds$partyPrediction == "dryShrubGrass",], 
#                aes(Long, Lat, fill = partyPrediction.dryShrubGrass),
#                col = "black" ,
#                shape = 21) +
#     scale_fill_distiller(type = "seq", palette = "Reds")
# )
# (partyTree_5Pred_mapBadEastForest <- ggplot() +
#     geom_point(data = partyTree_5Preds, aes(Long, Lat, col = newRegion), alpha = .1) +
#     ggtitle("Severity of Eastern Forest misclassification") +
#     geom_point(data = partyTree_5Preds[partyTree_5Preds$goodPred == FALSE & partyTree_5Preds$partyPrediction == "eastForest",], 
#                aes(Long, Lat, fill = partyPrediction.eastForest),
#                col = "black" ,
#                shape = 21) +
#     scale_fill_distiller(type = "seq", palette = "Greens")
# ) 
# 
# (partyTree_5Pred_mapBadWestForest <- ggplot() +
#     geom_point(data = partyTree_5Preds, aes(Long, Lat, col = newRegion), alpha = .1) +
#     ggtitle("Severity of Western Forest misclassification") +
#     geom_point(data = partyTree_5Preds[partyTree_5Preds$goodPred == FALSE & partyTree_5Preds$partyPrediction == "westForest",], 
#                aes(Long, Lat, fill = partyPrediction.westForest),
#                col = "black" ,
#                shape = 21) +
#     scale_fill_distiller(type = "seq", palette = "Blues")
# ) 
# 
# ## arrange all of the figures for this model
# 
# ggpubr::annotate_figure(ggarrange(partyTree_5PredMAP, 
#                                   ggarrange((ggtern::arrangeGrob(partyTree_5_ternGoodPreds)), 
#                                             (ggtern::arrangeGrob(partyTree_5_ternBadPreds))), 
#                                   partyTree_5Pred_mapBadGrass, 
#                                   partyTree_5Pred_mapBadEastForest, 
#                                   partyTree_5Pred_mapBadWestForest, 
#                                   ggplotify::as.grob(gt::as_gtable(gt::gt(data.frame("dryShrubGrass" = c(17506, 470, 926), 
#                                                                                      "eastForest" = c(228, 8503, 11),
#                                                                                      "westForest" = c(1494, 17, 8585), 
#                                                                                      row.names = c("dryShrubGrass", "eastForest", "westForest")), 
#                                                                           rownames_to_stub = TRUE))),
#                                   nrow = 6
# ), 
# top = "Model with depth = 7 and SWE, AWDD, total water-holding capacity, sand, tmean, and Soil Depth removed") %>% 
#   ggexport(
#     filename = "./Figures/EcoRegionModelFigures/ModelFigures_depth7_noSWEnoAWDDnoWaterHoldingCapacitynoSANDnoMATnoSoilDepth.pdf",
#     width = 11, height = 24)
# 
# # Conditional Inference Tree w/ swe, awdd, total available water holding capacity, sand, tmean, surface clay, and soil depth removed --------------
# # try restricting the tree depth (too long right now)
# partyTree_6 <- partykit::ctree(newRegion ~ isothermality_meanAnnAvg_30yr + prcp_meanAnnTotal_30yr +
#                                  PrecipTempCorr_meanAnnAvg_30yr + 
#                                  avgCoarsePerc_acrossDepth +
#                                  avgOrganicCarbonPerc_0_3cm ,
#                                data = modDat_fit, control = partykit::ctree_control(maxdepth = 7))
# partyTree_6
# # variable importance
# varImp_temp <- data.frame("variable" = names(sort((varimp(partyTree_6)), decreasing = TRUE)),
#                           "importance" = sort((varimp(partyTree_6)), decreasing = TRUE))
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
# #plot(partyTree_6, main="Conditional Inference Tree for ecoregions")
# 
# # predict categories based on this simple model
# partyTree_6Preds <- cbind(predictionDat, "partyPrediction" = partykit::predict.party(partyTree_6, newdata = predictionDat, type = "response")) %>%
#   cbind("partyPrediction" = partykit::predict.party(partyTree_6, newdata = predictionDat, type = "prob")) %>% 
#   cbind(modDat_test %>%
#           dplyr::select(Long, Lat))
# partyTree_6Preds$goodPred <- partyTree_6Preds$newRegion == partyTree_6Preds$partyPrediction
# # percentage of misclassifications
# sum(!partyTree_6Preds$goodPred)/nrow(partyTree_6Preds) * 100
# 
# (partyTree_6PredMAP <- ggplot(partyTree_6Preds) +
#     geom_point(aes(Long, Lat, col = partyPrediction), alpha = .1) +
#     ggtitle("model-predicted ecoregion classification",
#             subtitle = paste0("black dots indicate misclassification, \n fill color indicates which ecoregion a point was misclassified as; \n  ", round((100-sum(!partyTree_6Preds$goodPred)/nrow(partyTree_6Preds) * 100),1), "% classification accuracy" )) +
#     geom_point(data = partyTree_6Preds[partyTree_6Preds$goodPred == FALSE,], aes(Long, Lat, fill = partyPrediction),
#                col = "black"
#                , shape = 21))
# 
# ## confusion matrix
# partyMatrix <- partyTree_6Preds[,c("newRegion", "partyPrediction")]
# partyTree_6_confMat <- caret::confusionMatrix(data = partyTree_6Preds$partyPrediction,
#                                               reference = partyTree_6Preds$newRegion)$table
# 
# 
# (partyTree_6_ternAllPreds <- ggtern(data= partyTree_6Preds, 
#                                     aes(x=jitter(partyPrediction.dryShrubGrass*100 + 1, 50),
#                                         y=jitter(partyPrediction.eastForest*100 + 1, 50),
#                                         z=jitter(partyPrediction.westForest*100 + 1, 50), col = newRegion)) +
#     geom_point(alpha = .2) +
#     Tlab("Eastern Forest") +
#     Llab("Shrub/\nGrass") + 
#     Rlab("Western \nForest") +
#     theme_minimal() + 
#     ggtitle("All Predictions")) 
# # just good predictions
# (partyTree_6_ternGoodPreds <- partyTree_6Preds %>% 
#     filter(goodPred == TRUE) %>% 
#     ggtern(aes(x=jitter(partyPrediction.dryShrubGrass*100 + 1, 50),
#                y=jitter(partyPrediction.eastForest*100 + 1, 50),
#                z=jitter(partyPrediction.westForest*100 + 1, 50), col = newRegion)) +
#     geom_point(alpha = .2) +
#     Tlab("Eastern Forest") +
#     Llab("Shrub/\nGrass") + 
#     Rlab("Western \nForest") +
#     theme_minimal() + 
#     ggtitle("Accurate Predictions"))
# # just bad predictions
# badDat <- partyTree_6Preds %>% 
#   filter(goodPred == FALSE)
# (partyTree_6_ternBadPreds <- 
#     ggtern( data = badDat, aes(x=jitter(partyPrediction.dryShrubGrass*100 + 1, 50),
#                                y=jitter(partyPrediction.eastForest*100 + 1, 50),
#                                z=jitter(partyPrediction.westForest*100 + 1, 50), col = newRegion)) +
#     geom_point(alpha = .2) +
#     Tlab("Eastern Forest") +
#     Llab("Shrub/\nGrass") + 
#     Rlab("Western \nForest") +
#     theme_minimal() + 
#     ggtitle("Bad Predictions"))
# 
# # map of misclassified points 
# partyTree_6Preds$color <- rgb(red = partyTree_6Preds$partyPrediction.dryShrubGrass, 
#                               green = partyTree_6Preds$partyPrediction.eastForest, 
#                               blue = partyTree_6Preds$partyPrediction.westForest)
# 
# (partyTree_6Pred_mapBadGrass <- ggplot() +
#     geom_point(data = partyTree_6Preds, aes(Long, Lat, col = newRegion), alpha = .1) +
#     ggtitle("Severity of Grass/Shrub misclassification") +
#     geom_point(data = partyTree_6Preds[partyTree_6Preds$goodPred == FALSE & partyTree_6Preds$partyPrediction == "dryShrubGrass",], 
#                aes(Long, Lat, fill = partyPrediction.dryShrubGrass),
#                col = "black" ,
#                shape = 21) +
#     scale_fill_distiller(type = "seq", palette = "Reds")
# )
# (partyTree_6Pred_mapBadEastForest <- ggplot() +
#     geom_point(data = partyTree_6Preds, aes(Long, Lat, col = newRegion), alpha = .1) +
#     ggtitle("Severity of Eastern Forest misclassification") +
#     geom_point(data = partyTree_6Preds[partyTree_6Preds$goodPred == FALSE & partyTree_6Preds$partyPrediction == "eastForest",], 
#                aes(Long, Lat, fill = partyPrediction.eastForest),
#                col = "black" ,
#                shape = 21) +
#     scale_fill_distiller(type = "seq", palette = "Greens")
# ) 
# 
# (partyTree_6Pred_mapBadWestForest <- ggplot() +
#     geom_point(data = partyTree_6Preds, aes(Long, Lat, col = newRegion), alpha = .1) +
#     ggtitle("Severity of Western Forest misclassification") +
#     geom_point(data = partyTree_6Preds[partyTree_6Preds$goodPred == FALSE & partyTree_6Preds$partyPrediction == "westForest",], 
#                aes(Long, Lat, fill = partyPrediction.westForest),
#                col = "black" ,
#                shape = 21) +
#     scale_fill_distiller(type = "seq", palette = "Blues")
# ) 
# 
# ## arrange all of the figures for this model
# 
# ggpubr::annotate_figure(ggarrange(partyTree_6PredMAP, 
#                                   ggarrange((ggtern::arrangeGrob(partyTree_6_ternGoodPreds)), 
#                                             (ggtern::arrangeGrob(partyTree_6_ternBadPreds))), 
#                                   partyTree_6Pred_mapBadGrass, 
#                                   partyTree_6Pred_mapBadEastForest, 
#                                   partyTree_6Pred_mapBadWestForest, 
#                                   ggplotify::as.grob(gt::as_gtable(gt::gt(data.frame("dryShrubGrass" = c(17215, 610, 1077), 
#                                                                                      "eastForest" = c(183, 8540, 19),
#                                                                                      "westForest" = c(1474, 12, 8610), 
#                                                                                      row.names = c("dryShrubGrass", "eastForest", "westForest")), 
#                                                                           rownames_to_stub = TRUE))),
#                                   nrow = 6
# ), 
# top = "Model with depth = 7 and SWE, AWDD, total water-holding capacity, sand, tmean, surface clay, and Soil Depth removed") %>% 
#   ggexport(
#     filename = "./Figures/EcoRegionModelFigures/ModelFigures_depth7_noSWEnoAWDDnoWaterHoldingCapacitynoSANDnoCLAYnoMATnoSoilDepth.pdf",
#     width = 11, height = 24)
# 
# # Conditional Inference Tree w/ swe, awdd, total available water holding capacity, sand, tmean, surface clay, coarse material, and soil depth removed --------------
# # try restricting the tree depth (too long right now)
# partyTree_7 <- partykit::ctree(newRegion ~ isothermality_meanAnnAvg_30yr + prcp_meanAnnTotal_30yr +
#                                  PrecipTempCorr_meanAnnAvg_30yr + 
#                                  avgOrganicCarbonPerc_0_3cm ,
#                                data = modDat_fit, control = partykit::ctree_control(maxdepth = 7))
# partyTree_7
# # variable importance
# varImp_temp <- data.frame("variable" = names(sort((varimp(partyTree_7)), decreasing = TRUE)),
#                           "importance" = sort((varimp(partyTree_7)), decreasing = TRUE))
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
# #plot(partyTree_7, main="Conditional Inference Tree for ecoregions")
# 
# # predict categories based on this simple model
# partyTree_7Preds <- cbind(predictionDat, "partyPrediction" = partykit::predict.party(partyTree_7, newdata = predictionDat, type = "response")) %>%
#   cbind("partyPrediction" = partykit::predict.party(partyTree_7, newdata = predictionDat, type = "prob")) %>% 
#   cbind(modDat_test %>%
#           dplyr::select(Long, Lat))
# partyTree_7Preds$goodPred <- partyTree_7Preds$newRegion == partyTree_7Preds$partyPrediction
# # percentage of misclassifications
# sum(!partyTree_7Preds$goodPred)/nrow(partyTree_7Preds) * 100
# 
# (partyTree_7PredMAP <- ggplot(partyTree_7Preds) +
#     geom_point(aes(Long, Lat, col = partyPrediction), alpha = .1) +
#     ggtitle("model-predicted ecoregion classification",
#             subtitle = paste0("black dots indicate misclassification, \n fill color indicates which ecoregion a point was misclassified as; \n  ", round((100-sum(!partyTree_7Preds$goodPred)/nrow(partyTree_7Preds) * 100),1), "% classification accuracy" )) +
#     geom_point(data = partyTree_7Preds[partyTree_7Preds$goodPred == FALSE,], aes(Long, Lat, fill = partyPrediction),
#                col = "black"
#                , shape = 21))
# 
# ## confusion matrix
# partyMatrix <- partyTree_7Preds[,c("newRegion", "partyPrediction")]
# partyTree_7_confMat <- caret::confusionMatrix(data = partyTree_7Preds$partyPrediction,
#                                               reference = partyTree_7Preds$newRegion)$table
# 
# 
# (partyTree_7_ternAllPreds <- ggtern(data= partyTree_7Preds, 
#                                     aes(x=jitter(partyPrediction.dryShrubGrass*100 + 1, 50),
#                                         y=jitter(partyPrediction.eastForest*100 + 1, 50),
#                                         z=jitter(partyPrediction.westForest*100 + 1, 50), col = newRegion)) +
#     geom_point(alpha = .2) +
#     Tlab("Eastern Forest") +
#     Llab("Shrub/\nGrass") + 
#     Rlab("Western \nForest") +
#     theme_minimal() + 
#     ggtitle("All Predictions")) 
# # just good predictions
# (partyTree_7_ternGoodPreds <- partyTree_7Preds %>% 
#     filter(goodPred == TRUE) %>% 
#     ggtern(aes(x=jitter(partyPrediction.dryShrubGrass*100 + 1, 50),
#                y=jitter(partyPrediction.eastForest*100 + 1, 50),
#                z=jitter(partyPrediction.westForest*100 + 1, 50), col = newRegion)) +
#     geom_point(alpha = .2) +
#     Tlab("Eastern Forest") +
#     Llab("Shrub/\nGrass") + 
#     Rlab("Western \nForest") +
#     theme_minimal() + 
#     ggtitle("Accurate Predictions"))
# # just bad predictions
# badDat <- partyTree_7Preds %>% 
#   filter(goodPred == FALSE)
# (partyTree_7_ternBadPreds <- 
#     ggtern( data = badDat, aes(x=jitter(partyPrediction.dryShrubGrass*100 + 1, 50),
#                                y=jitter(partyPrediction.eastForest*100 + 1, 50),
#                                z=jitter(partyPrediction.westForest*100 + 1, 50), col = newRegion)) +
#     geom_point(alpha = .2) +
#     Tlab("Eastern Forest") +
#     Llab("Shrub/\nGrass") + 
#     Rlab("Western \nForest") +
#     theme_minimal() + 
#     ggtitle("Bad Predictions"))
# 
# # map of misclassified points 
# partyTree_7Preds$color <- rgb(red = partyTree_7Preds$partyPrediction.dryShrubGrass, 
#                               green = partyTree_7Preds$partyPrediction.eastForest, 
#                               blue = partyTree_7Preds$partyPrediction.westForest)
# 
# (partyTree_7Pred_mapBadGrass <- ggplot() +
#     geom_point(data = partyTree_7Preds, aes(Long, Lat, col = newRegion), alpha = .1) +
#     ggtitle("Severity of Grass/Shrub misclassification") +
#     geom_point(data = partyTree_7Preds[partyTree_7Preds$goodPred == FALSE & partyTree_7Preds$partyPrediction == "dryShrubGrass",], 
#                aes(Long, Lat, fill = partyPrediction.dryShrubGrass),
#                col = "black" ,
#                shape = 21) +
#     scale_fill_distiller(type = "seq", palette = "Reds")
# )
# (partyTree_7Pred_mapBadEastForest <- ggplot() +
#     geom_point(data = partyTree_7Preds, aes(Long, Lat, col = newRegion), alpha = .1) +
#     ggtitle("Severity of Eastern Forest misclassification") +
#     geom_point(data = partyTree_7Preds[partyTree_7Preds$goodPred == FALSE & partyTree_7Preds$partyPrediction == "eastForest",], 
#                aes(Long, Lat, fill = partyPrediction.eastForest),
#                col = "black" ,
#                shape = 21) +
#     scale_fill_distiller(type = "seq", palette = "Greens")
# ) 
# 
# (partyTree_7Pred_mapBadWestForest <- ggplot() +
#     geom_point(data = partyTree_7Preds, aes(Long, Lat, col = newRegion), alpha = .1) +
#     ggtitle("Severity of Western Forest misclassification") +
#     geom_point(data = partyTree_7Preds[partyTree_7Preds$goodPred == FALSE & partyTree_7Preds$partyPrediction == "westForest",], 
#                aes(Long, Lat, fill = partyPrediction.westForest),
#                col = "black" ,
#                shape = 21) +
#     scale_fill_distiller(type = "seq", palette = "Blues")
# ) 
# 
# ## arrange all of the figures for this model
# 
# ggpubr::annotate_figure(ggarrange(partyTree_7PredMAP, 
#                                   ggarrange((ggtern::arrangeGrob(partyTree_7_ternGoodPreds)), 
#                                             (ggtern::arrangeGrob(partyTree_7_ternBadPreds))), 
#                                   partyTree_7Pred_mapBadGrass, 
#                                   partyTree_7Pred_mapBadEastForest, 
#                                   partyTree_7Pred_mapBadWestForest, 
#                                   ggplotify::as.grob(gt::as_gtable(gt::gt(data.frame("dryShrubGrass" = c(17215, 327, 1304), 
#                                                                                      "eastForest" = c(389, 8346, 7),
#                                                                                      "westForest" = c(1350, 23, 8723), 
#                                                                                      row.names = c("dryShrubGrass", "eastForest", "westForest")), 
#                                                                           rownames_to_stub = TRUE))),
#                                   nrow = 6
# ), 
# top = "Model with depth = 7 and SWE, AWDD, total water-holding capacity, sand, tmean, surface clay, coarse and Soil Depth removed") %>% 
#   ggexport(
#     filename = "./Figures/EcoRegionModelFigures/ModelFigures_depth7_noSWEnoAWDDnoWaterHoldingCapacitynoSANDnoCLAYnoCOARSEnoMATnoSoilDepth.pdf",
#     width = 11, height = 24)
# 
# # Conditional Inference Tree w/ swe, awdd, total available water holding capacity, sand, tmean, surface clay, coarse material, isothermality, and soil depth removed --------------
# # try restricting the tree depth (too long right now)
# partyTree_8 <- partykit::ctree(newRegion ~  prcp_meanAnnTotal_30yr +
#                                  PrecipTempCorr_meanAnnAvg_30yr + 
#                                  avgOrganicCarbonPerc_0_3cm ,
#                                data = modDat_fit, control = partykit::ctree_control(maxdepth = 7))
# partyTree_8
# # variable importance
# varImp_temp <- data.frame("variable" = names(sort((varimp(partyTree_8)), decreasing = TRUE)),
#                           "importance" = sort((varimp(partyTree_8)), decreasing = TRUE))
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
# #plot(partyTree_8, main="Conditional Inference Tree for ecoregions")
# 
# # predict categories based on this simple model
# partyTree_8Preds <- cbind(predictionDat, "partyPrediction" = partykit::predict.party(partyTree_8, newdata = predictionDat, type = "response")) %>%
#   cbind("partyPrediction" = partykit::predict.party(partyTree_8, newdata = predictionDat, type = "prob")) %>% 
#   cbind(modDat_test %>%
#           dplyr::select(Long, Lat))
# partyTree_8Preds$goodPred <- partyTree_8Preds$newRegion == partyTree_8Preds$partyPrediction
# # percentage of misclassifications
# sum(!partyTree_8Preds$goodPred)/nrow(partyTree_8Preds) * 100
# 
# (partyTree_8PredMAP <- ggplot(partyTree_8Preds) +
#     geom_point(aes(Long, Lat, col = partyPrediction), alpha = .1) +
#     ggtitle("model-predicted ecoregion classification",
#             subtitle = paste0("black dots indicate misclassification, \n fill color indicates which ecoregion a point was misclassified as; \n  ", round((100-sum(!partyTree_8Preds$goodPred)/nrow(partyTree_8Preds) * 100),1), "% classification accuracy" )) +
#     geom_point(data = partyTree_8Preds[partyTree_8Preds$goodPred == FALSE,], aes(Long, Lat, fill = partyPrediction),
#                col = "black"
#                , shape = 21))
# 
# ## confusion matrix
# partyMatrix <- partyTree_8Preds[,c("newRegion", "partyPrediction")]
# partyTree_8_confMat <- caret::confusionMatrix(data = partyTree_8Preds$partyPrediction,
#                                               reference = partyTree_8Preds$newRegion)$table
# 
# 
# (partyTree_8_ternAllPreds <- ggtern(data= partyTree_8Preds, 
#                                     aes(x=jitter(partyPrediction.dryShrubGrass*100 + 1, 50),
#                                         y=jitter(partyPrediction.eastForest*100 + 1, 50),
#                                         z=jitter(partyPrediction.westForest*100 + 1, 50), col = newRegion)) +
#     geom_point(alpha = .2) +
#     Tlab("Eastern Forest") +
#     Llab("Shrub/\nGrass") + 
#     Rlab("Western \nForest") +
#     theme_minimal() + 
#     ggtitle("All Predictions")) 
# # just good predictions
# (partyTree_8_ternGoodPreds <- partyTree_8Preds %>% 
#     filter(goodPred == TRUE) %>% 
#     ggtern(aes(x=jitter(partyPrediction.dryShrubGrass*100 + 1, 50),
#                y=jitter(partyPrediction.eastForest*100 + 1, 50),
#                z=jitter(partyPrediction.westForest*100 + 1, 50), col = newRegion)) +
#     geom_point(alpha = .2) +
#     Tlab("Eastern Forest") +
#     Llab("Shrub/\nGrass") + 
#     Rlab("Western \nForest") +
#     theme_minimal() + 
#     ggtitle("Accurate Predictions"))
# # just bad predictions
# badDat <- partyTree_8Preds %>% 
#   filter(goodPred == FALSE)
# (partyTree_8_ternBadPreds <- 
#     ggtern( data = badDat, aes(x=jitter(partyPrediction.dryShrubGrass*100 + 1, 50),
#                                y=jitter(partyPrediction.eastForest*100 + 1, 50),
#                                z=jitter(partyPrediction.westForest*100 + 1, 50), col = newRegion)) +
#     geom_point(alpha = .2) +
#     Tlab("Eastern Forest") +
#     Llab("Shrub/\nGrass") + 
#     Rlab("Western \nForest") +
#     theme_minimal() + 
#     ggtitle("Bad Predictions"))
# 
# # map of misclassified points 
# partyTree_8Preds$color <- rgb(red = partyTree_8Preds$partyPrediction.dryShrubGrass, 
#                               green = partyTree_8Preds$partyPrediction.eastForest, 
#                               blue = partyTree_8Preds$partyPrediction.westForest)
# 
# (partyTree_8Pred_mapBadGrass <- ggplot() +
#     geom_point(data = partyTree_8Preds, aes(Long, Lat, col = newRegion), alpha = .1) +
#     ggtitle("Severity of Grass/Shrub misclassification") +
#     geom_point(data = partyTree_8Preds[partyTree_8Preds$goodPred == FALSE & partyTree_8Preds$partyPrediction == "dryShrubGrass",], 
#                aes(Long, Lat, fill = partyPrediction.dryShrubGrass),
#                col = "black" ,
#                shape = 21) +
#     scale_fill_distiller(type = "seq", palette = "Reds")
# )
# (partyTree_8Pred_mapBadEastForest <- ggplot() +
#     geom_point(data = partyTree_8Preds, aes(Long, Lat, col = newRegion), alpha = .1) +
#     ggtitle("Severity of Eastern Forest misclassification") +
#     geom_point(data = partyTree_8Preds[partyTree_8Preds$goodPred == FALSE & partyTree_8Preds$partyPrediction == "eastForest",], 
#                aes(Long, Lat, fill = partyPrediction.eastForest),
#                col = "black" ,
#                shape = 21) +
#     scale_fill_distiller(type = "seq", palette = "Greens")
# ) 
# 
# (partyTree_8Pred_mapBadWestForest <- ggplot() +
#     geom_point(data = partyTree_8Preds, aes(Long, Lat, col = newRegion), alpha = .1) +
#     ggtitle("Severity of Western Forest misclassification") +
#     geom_point(data = partyTree_8Preds[partyTree_8Preds$goodPred == FALSE & partyTree_8Preds$partyPrediction == "westForest",], 
#                aes(Long, Lat, fill = partyPrediction.westForest),
#                col = "black" ,
#                shape = 21) +
#     scale_fill_distiller(type = "seq", palette = "Blues")
# ) 
# 
# ## arrange all of the figures for this model
# 
# ggpubr::annotate_figure(ggarrange(partyTree_8PredMAP, 
#                                   ggarrange((ggtern::arrangeGrob(partyTree_8_ternGoodPreds)), 
#                                             (ggtern::arrangeGrob(partyTree_8_ternBadPreds))), 
#                                   partyTree_8Pred_mapBadGrass, 
#                                   partyTree_8Pred_mapBadEastForest, 
#                                   partyTree_8Pred_mapBadWestForest, 
#                                   ggplotify::as.grob(gt::as_gtable(gt::gt(data.frame("dryShrubGrass" = c(17894, 328, 680), 
#                                                                                      "eastForest" = c(325, 8409, 8),
#                                                                                      "westForest" = c(2368, 54, 7674), 
#                                                                                      row.names = c("dryShrubGrass", "eastForest", "westForest")), 
#                                                                           rownames_to_stub = TRUE))),
#                                   nrow = 6
# ), 
# top = "Model with depth = 7 and SWE, AWDD, total water-holding capacity, sand, tmean, surface clay, coarse material, isothermality and Soil Depth removed") %>% 
#   ggexport(
#     filename = "./Figures/EcoRegionModelFigures/ModelFigures_depth7_noSWEnoAWDDnoWaterHoldingCapacitynoSANDnoCLAYnoCOARSEnoMATnoISOTHERMnoSoilDepth.pdf",
#     width = 11, height = 24)
# 
# # Conditional Inference Tree w/ depth = 6 and swe, awdd, total available water holding capacity, sand, tmean, surface clay, coarse material, isothermality, and soil depth removed --------------
# # try restricting the tree depth (too long right now)
# partyTree_9 <- partykit::ctree(newRegion ~  prcp_meanAnnTotal_30yr +
#                                  PrecipTempCorr_meanAnnAvg_30yr + 
#                                  avgOrganicCarbonPerc_0_3cm ,
#                                data = modDat_fit, control = partykit::ctree_control(maxdepth = 6))
# partyTree_9
# # variable importance
# varImp_temp <- data.frame("variable" = names(sort((varimp(partyTree_9)), decreasing = TRUE)),
#                           "importance" = sort((varimp(partyTree_9)), decreasing = TRUE))
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
# #plot(partyTree_9, main="Conditional Inference Tree for ecoregions")
# 
# # predict categories based on this simple model
# partyTree_9Preds <- cbind(predictionDat, "partyPrediction" = partykit::predict.party(partyTree_9, newdata = predictionDat, type = "response")) %>%
#   cbind("partyPrediction" = partykit::predict.party(partyTree_9, newdata = predictionDat, type = "prob")) %>% 
#   cbind(modDat_test %>%
#           dplyr::select(Long, Lat))
# partyTree_9Preds$goodPred <- partyTree_9Preds$newRegion == partyTree_9Preds$partyPrediction
# # percentage of misclassifications
# sum(!partyTree_9Preds$goodPred)/nrow(partyTree_9Preds) * 100
# 
# (partyTree_9PredMAP <- ggplot(partyTree_9Preds) +
#     geom_point(aes(Long, Lat, col = partyPrediction), alpha = .1) +
#     ggtitle("model-predicted ecoregion classification",
#             subtitle = paste0("black dots indicate misclassification, \n fill color indicates which ecoregion a point was misclassified as; \n  ", round((100-sum(!partyTree_9Preds$goodPred)/nrow(partyTree_9Preds) * 100),1), "% classification accuracy" )) +
#     geom_point(data = partyTree_9Preds[partyTree_9Preds$goodPred == FALSE,], aes(Long, Lat, fill = partyPrediction),
#                col = "black"
#                , shape = 21))
# 
# ## confusion matrix
# partyMatrix <- partyTree_9Preds[,c("newRegion", "partyPrediction")]
# partyTree_9_confMat <- caret::confusionMatrix(data = partyTree_9Preds$partyPrediction,
#                                               reference = partyTree_9Preds$newRegion)$table
# 
# 
# (partyTree_9_ternAllPreds <- ggtern(data= partyTree_9Preds, 
#                                     aes(x=jitter(partyPrediction.dryShrubGrass*100 + 1, 50),
#                                         y=jitter(partyPrediction.eastForest*100 + 1, 50),
#                                         z=jitter(partyPrediction.westForest*100 + 1, 50), col = newRegion)) +
#     geom_point(alpha = .2) +
#     Tlab("Eastern Forest") +
#     Llab("Shrub/\nGrass") + 
#     Rlab("Western \nForest") +
#     theme_minimal() + 
#     ggtitle("All Predictions")) 
# # just good predictions
# (partyTree_9_ternGoodPreds <- partyTree_9Preds %>% 
#     filter(goodPred == TRUE) %>% 
#     ggtern(aes(x=jitter(partyPrediction.dryShrubGrass*100 + 1, 50),
#                y=jitter(partyPrediction.eastForest*100 + 1, 50),
#                z=jitter(partyPrediction.westForest*100 + 1, 50), col = newRegion)) +
#     geom_point(alpha = .2) +
#     Tlab("Eastern Forest") +
#     Llab("Shrub/\nGrass") + 
#     Rlab("Western \nForest") +
#     theme_minimal() + 
#     ggtitle("Accurate Predictions"))
# # just bad predictions
# badDat <- partyTree_9Preds %>% 
#   filter(goodPred == FALSE)
# (partyTree_9_ternBadPreds <- 
#     ggtern( data = badDat, aes(x=jitter(partyPrediction.dryShrubGrass*100 + 1, 50),
#                                y=jitter(partyPrediction.eastForest*100 + 1, 50),
#                                z=jitter(partyPrediction.westForest*100 + 1, 50), col = newRegion)) +
#     geom_point(alpha = .2) +
#     Tlab("Eastern Forest") +
#     Llab("Shrub/\nGrass") + 
#     Rlab("Western \nForest") +
#     theme_minimal() + 
#     ggtitle("Bad Predictions"))
# 
# # map of misclassified points 
# partyTree_9Preds$color <- rgb(red = partyTree_9Preds$partyPrediction.dryShrubGrass, 
#                               green = partyTree_9Preds$partyPrediction.eastForest, 
#                               blue = partyTree_9Preds$partyPrediction.westForest)
# 
# (partyTree_9Pred_mapBadGrass <- ggplot() +
#     geom_point(data = partyTree_9Preds, aes(Long, Lat, col = newRegion), alpha = .1) +
#     ggtitle("Severity of Grass/Shrub misclassification") +
#     geom_point(data = partyTree_9Preds[partyTree_9Preds$goodPred == FALSE & partyTree_9Preds$partyPrediction == "dryShrubGrass",], 
#                aes(Long, Lat, fill = partyPrediction.dryShrubGrass),
#                col = "black" ,
#                shape = 21) +
#     scale_fill_distiller(type = "seq", palette = "Reds")
# )
# (partyTree_9Pred_mapBadEastForest <- ggplot() +
#     geom_point(data = partyTree_9Preds, aes(Long, Lat, col = newRegion), alpha = .1) +
#     ggtitle("Severity of Eastern Forest misclassification") +
#     geom_point(data = partyTree_9Preds[partyTree_9Preds$goodPred == FALSE & partyTree_9Preds$partyPrediction == "eastForest",], 
#                aes(Long, Lat, fill = partyPrediction.eastForest),
#                col = "black" ,
#                shape = 21) +
#     scale_fill_distiller(type = "seq", palette = "Greens")
# ) 
# 
# (partyTree_9Pred_mapBadWestForest <- ggplot() +
#     geom_point(data = partyTree_9Preds, aes(Long, Lat, col = newRegion), alpha = .1) +
#     ggtitle("Severity of Western Forest misclassification") +
#     geom_point(data = partyTree_9Preds[partyTree_9Preds$goodPred == FALSE & partyTree_9Preds$partyPrediction == "westForest",], 
#                aes(Long, Lat, fill = partyPrediction.westForest),
#                col = "black" ,
#                shape = 21) +
#     scale_fill_distiller(type = "seq", palette = "Blues")
# ) 
# 
# ## arrange all of the figures for this model
# 
# ggpubr::annotate_figure(ggarrange(partyTree_9PredMAP, 
#                                   ggarrange((ggtern::arrangeGrob(partyTree_9_ternGoodPreds)), 
#                                             (ggtern::arrangeGrob(partyTree_9_ternBadPreds))), 
#                                   partyTree_9Pred_mapBadGrass, 
#                                   partyTree_9Pred_mapBadEastForest, 
#                                   partyTree_9Pred_mapBadWestForest, 
#                                   ggplotify::as.grob(gt::as_gtable(gt::gt(data.frame("dryShrubGrass" = c(17894, 328, 680), 
#                                                                                      "eastForest" = c(325, 8409, 8),
#                                                                                      "westForest" = c(2368, 54, 7674), 
#                                                                                      row.names = c("dryShrubGrass", "eastForest", "westForest")), 
#                                                                           rownames_to_stub = TRUE))),
#                                   nrow = 6
# ), 
# top = "Model with depth = 6 and SWE, AWDD, total water-holding capacity, sand, tmean, surface clay, coarse material, isothermality and Soil Depth removed") %>% 
#   ggexport(
#     filename = "./Figures/EcoRegionModelFigures/ModelFigures_depth6_noSWEnoAWDDnoWaterHoldingCapacitynoSANDnoCLAYnoCOARSEnoMATnoISOTHERMnoSoilDepth.pdf",
#     width = 11, height = 24)
# 
#  
# # Conditional Inference Tree w/ depth = 5 swe, awdd, total available water holding capacity, sand, tmean, surface clay, coarse material, isothermality, and soil depth removed --------------
# # try restricting the tree depth (too long right now)
# partyTree_10 <- partykit::ctree(newRegion ~  prcp_meanAnnTotal_30yr +
#                                  PrecipTempCorr_meanAnnAvg_30yr + 
#                                  avgOrganicCarbonPerc_0_3cm ,
#                                data = modDat_fit, control = partykit::ctree_control(maxdepth = 5))
# partyTree_10
# # variable importance
# varImp_temp <- data.frame("variable" = names(sort((varimp(partyTree_10)), decreasing = TRUE)),
#                           "importance" = sort((varimp(partyTree_10)), decreasing = TRUE))
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
# #plot(partyTree_10, main="Conditional Inference Tree for ecoregions")
# 
# # predict categories based on this simple model
# partyTree_10Preds <- cbind(predictionDat, "partyPrediction" = partykit::predict.party(partyTree_10, newdata = predictionDat, type = "response")) %>%
#   cbind("partyPrediction" = partykit::predict.party(partyTree_10, newdata = predictionDat, type = "prob")) %>% 
#   cbind(modDat_test %>%
#           dplyr::select(Long, Lat))
# partyTree_10Preds$goodPred <- partyTree_10Preds$newRegion == partyTree_10Preds$partyPrediction
# # percentage of misclassifications
# sum(!partyTree_10Preds$goodPred)/nrow(partyTree_10Preds) * 100
# 
# (partyTree_10PredMAP <- ggplot(partyTree_10Preds) +
#     geom_point(aes(Long, Lat, col = partyPrediction), alpha = .1) +
#     ggtitle("model-predicted ecoregion classification",
#             subtitle = paste0("black dots indicate misclassification, \n fill color indicates which ecoregion a point was misclassified as; \n  ", round((100-sum(!partyTree_10Preds$goodPred)/nrow(partyTree_10Preds) * 100),1), "% classification accuracy" )) +
#     geom_point(data = partyTree_10Preds[partyTree_10Preds$goodPred == FALSE,], aes(Long, Lat, fill = partyPrediction),
#                col = "black"
#                , shape = 21))
# 
# ## confusion matrix
# partyMatrix <- partyTree_10Preds[,c("newRegion", "partyPrediction")]
# partyTree_10_confMat <- caret::confusionMatrix(data = partyTree_10Preds$partyPrediction,
#                                               reference = partyTree_10Preds$newRegion)$table
# 
# 
# (partyTree_10_ternAllPreds <- ggtern(data= partyTree_10Preds, 
#                                     aes(x=jitter(partyPrediction.dryShrubGrass*100 + 1, 50),
#                                         y=jitter(partyPrediction.eastForest*100 + 1, 50),
#                                         z=jitter(partyPrediction.westForest*100 + 1, 50), col = newRegion)) +
#     geom_point(alpha = .2) +
#     Tlab("Eastern Forest") +
#     Llab("Shrub/\nGrass") + 
#     Rlab("Western \nForest") +
#     theme_minimal() + 
#     ggtitle("All Predictions")) 
# # just good predictions
# (partyTree_10_ternGoodPreds <- partyTree_10Preds %>% 
#     filter(goodPred == TRUE) %>% 
#     ggtern(aes(x=jitter(partyPrediction.dryShrubGrass*100 + 1, 50),
#                y=jitter(partyPrediction.eastForest*100 + 1, 50),
#                z=jitter(partyPrediction.westForest*100 + 1, 50), col = newRegion)) +
#     geom_point(alpha = .2) +
#     Tlab("Eastern Forest") +
#     Llab("Shrub/\nGrass") + 
#     Rlab("Western \nForest") +
#     theme_minimal() + 
#     ggtitle("Accurate Predictions"))
# # just bad predictions
# badDat <- partyTree_10Preds %>% 
#   filter(goodPred == FALSE)
# (partyTree_10_ternBadPreds <- 
#     ggtern( data = badDat, aes(x=jitter(partyPrediction.dryShrubGrass*100 + 1, 50),
#                                y=jitter(partyPrediction.eastForest*100 + 1, 50),
#                                z=jitter(partyPrediction.westForest*100 + 1, 50), col = newRegion)) +
#     geom_point(alpha = .2) +
#     Tlab("Eastern Forest") +
#     Llab("Shrub/\nGrass") + 
#     Rlab("Western \nForest") +
#     theme_minimal() + 
#     ggtitle("Bad Predictions"))
# 
# # map of misclassified points 
# partyTree_10Preds$color <- rgb(red = partyTree_10Preds$partyPrediction.dryShrubGrass, 
#                               green = partyTree_10Preds$partyPrediction.eastForest, 
#                               blue = partyTree_10Preds$partyPrediction.westForest)
# 
# (partyTree_10Pred_mapBadGrass <- ggplot() +
#     geom_point(data = partyTree_10Preds, aes(Long, Lat, col = newRegion), alpha = .1) +
#     ggtitle("Severity of Grass/Shrub misclassification") +
#     geom_point(data = partyTree_10Preds[partyTree_10Preds$goodPred == FALSE & partyTree_10Preds$partyPrediction == "dryShrubGrass",], 
#                aes(Long, Lat, fill = partyPrediction.dryShrubGrass),
#                col = "black" ,
#                shape = 21) +
#     scale_fill_distiller(type = "seq", palette = "Reds")
# )
# (partyTree_10Pred_mapBadEastForest <- ggplot() +
#     geom_point(data = partyTree_10Preds, aes(Long, Lat, col = newRegion), alpha = .1) +
#     ggtitle("Severity of Eastern Forest misclassification") +
#     geom_point(data = partyTree_10Preds[partyTree_10Preds$goodPred == FALSE & partyTree_10Preds$partyPrediction == "eastForest",], 
#                aes(Long, Lat, fill = partyPrediction.eastForest),
#                col = "black" ,
#                shape = 21) +
#     scale_fill_distiller(type = "seq", palette = "Greens")
# ) 
# 
# (partyTree_10Pred_mapBadWestForest <- ggplot() +
#     geom_point(data = partyTree_10Preds, aes(Long, Lat, col = newRegion), alpha = .1) +
#     ggtitle("Severity of Western Forest misclassification") +
#     geom_point(data = partyTree_10Preds[partyTree_10Preds$goodPred == FALSE & partyTree_10Preds$partyPrediction == "westForest",], 
#                aes(Long, Lat, fill = partyPrediction.westForest),
#                col = "black" ,
#                shape = 21) +
#     scale_fill_distiller(type = "seq", palette = "Blues")
# ) 
# 
# ## arrange all of the figures for this model
# 
# ggpubr::annotate_figure(ggarrange(partyTree_10PredMAP, 
#                                   ggarrange((ggtern::arrangeGrob(partyTree_10_ternGoodPreds)), 
#                                             (ggtern::arrangeGrob(partyTree_10_ternBadPreds))), 
#                                   partyTree_10Pred_mapBadGrass, 
#                                   partyTree_10Pred_mapBadEastForest, 
#                                   partyTree_10Pred_mapBadWestForest, 
#                                   ggplotify::as.grob(gt::as_gtable(gt::gt(data.frame("dryShrubGrass" = c(17894, 328, 680), 
#                                                                                      "eastForest" = c(325, 8409, 8),
#                                                                                      "westForest" = c(2368, 54, 7674), 
#                                                                                      row.names = c("dryShrubGrass", "eastForest", "westForest")), 
#                                                                           rownames_to_stub = TRUE))),
#                                   nrow = 6
# ), 
# top = "Model with depth = 5 and SWE, AWDD, total water-holding capacity, sand, tmean, surface clay, coarse material, isothermality and Soil Depth removed") %>% 
#   ggexport(
#     filename = "./Figures/EcoRegionModelFigures/ModelFigures_depth5_noSWEnoAWDDnoWaterHoldingCapacitynoSANDnoCLAYnoCOARSEnoMATnoISOTHERMnoSoilDepth.pdf",
#     width = 11, height = 24)
# 
# # compare accuracy of plots w/ max depth = 7 ------------------------------
# data.frame("ModelSpecification" = c("newRegion ~  swe_meanAnnAvg_30yr + annWetDegDays_meanAnnAvg_30yr +tmean_meanAnnAvg_30yr + isothermality_meanAnnAvg_30yr + prcp_meanAnnTotal_30yr + PrecipTempCorr_meanAnnAvg_30yr + surfaceClay_perc + avgSandPerc_acrossDepth + avgCoarsePerc_acrossDepth + avgOrganicCarbonPerc_0_3cm + totalAvailableWaterHoldingCapacity",  
# "newRegion ~ tmean_meanAnnAvg_30yr + isothermality_meanAnnAvg_30yr + prcp_meanAnnTotal_30yr + PrecipTempCorr_meanAnnAvg_30yr +  surfaceClay_perc + avgSandPerc_acrossDepth + avgCoarsePerc_acrossDepth + avgOrganicCarbonPerc_0_3cm + totalAvailableWaterHoldingCapacity", 
# "newRegion ~ tmean_meanAnnAvg_30yr + isothermality_meanAnnAvg_30yr + prcp_meanAnnTotal_30yr + PrecipTempCorr_meanAnnAvg_30yr +  surfaceClay_perc + avgSandPerc_acrossDepth + avgCoarsePerc_acrossDepth + avgOrganicCarbonPerc_0_3cm",
# "newRegion ~ tmean_meanAnnAvg_30yr + isothermality_meanAnnAvg_30yr + prcp_meanAnnTotal_30yr + PrecipTempCorr_meanAnnAvg_30yr +  surfaceClay_perc + avgCoarsePerc_acrossDepth + avgOrganicCarbonPerc_0_3cm",
# "newRegion ~ isothermality_meanAnnAvg_30yr + prcp_meanAnnTotal_30yr + PrecipTempCorr_meanAnnAvg_30yr +  surfaceClay_perc + avgCoarsePerc_acrossDepth + avgOrganicCarbonPerc_0_3cm"))
# 
# 
