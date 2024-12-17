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
    select(
       "tmin_meanAnnAvg_30yr"                  ,
       "tmax_meanAnnAvg_30yr"                  , "tmean_meanAnnAvg_30yr"                 ,
       "prcp_meanAnnTotal_30yr"                , "T_warmestMonth_meanAnnAvg_30yr"       , 
       "T_coldestMonth_meanAnnAvg_30yr"        , "precip_wettestMonth_meanAnnAvg_30yr"  , 
       "precip_driestMonth_meanAnnAvg_30yr"    , "precip_Seasonality_meanAnnAvg_30yr"   , 
       "PrecipTempCorr_meanAnnAvg_30yr"       ,  "aboveFreezing_month_meanAnnAvg_30yr"   ,
       "isothermality_meanAnnAvg_30yr"        ,  "annWaterDeficit_meanAnnAvg_30yr"       ,
       "annWetDegDays_meanAnnAvg_30yr"        ,  "annVPD_mean_meanAnnAvg_30yr"           ,
       "annVPD_max_meanAnnAvg_30yr"           ,  "annVPD_min_meanAnnAvg_30yr"            ,
       "annVPD_max_95percentile_30yr"         ,  "annWaterDeficit_95percentile_30yr"     ,
       "annWetDegDays_5percentile_30yr"       ,  "durationFrostFreeDays_5percentile_30yr",
       "durationFrostFreeDays_meanAnnAvg_30yr",  "soilDepth"                             ,
       "surfaceClay_perc"                     ,  "avgSandPerc_acrossDepth"               ,
       "avgCoarsePerc_acrossDepth"            ,  "avgOrganicCarbonPerc_0_3cm"            ,
       "totalAvailableWaterHoldingCapacity" 
    ) %>% 
     rename(
            "prcp" = prcp_meanAnnTotal_30yr,
           "prcp \n TempCorr" = PrecipTempCorr_meanAnnAvg_30yr,  "isothermality" = isothermality_meanAnnAvg_30yr,
           "Wet \n DegDays" = annWetDegDays_meanAnnAvg_30yr) %>%
    cor() ) %>% 
  caret::findCorrelation(cutoff = .7, verbose = TRUE, names = TRUE)
# the findCorrelation() function says we should remove these variables: 
# "tmean_meanAnnAvg_30yr"                  "tmin_meanAnnAvg_30yr"                  
# "annVPD_mean_meanAnnAvg_30yr" 
# "durationFrostFreeDays_meanAnnAvg_30yr"  "aboveFreezing_month_meanAnnAvg_30yr"   
# "durationFrostFreeDays_5percentile_30yr"  "tmax_meanAnnAvg_30yr"                  
# "T_coldestMonth_meanAnnAvg_30yr"       "annVPD_min_meanAnnAvg_30yr"            
# "annVPD_max_meanAnnAvg_30yr"             "T_warmestMonth_meanAnnAvg_30yr"        
# "Wet \n DegDays"                     ///      "annWaterDeficit_95percentile_30yr"     
#  "annWetDegDays_5percentile_30yr"         "prcp"                                  
#  "totalAvailableWaterHoldingCapacity"     "surfaceClay_perc"  

(corrPlot2 <- 
  modDat_fit %>% 
  select(
    #"tmin_meanAnnAvg_30yr"                  ,
    #"tmax_meanAnnAvg_30yr"                  , #"tmean_meanAnnAvg_30yr"                 ,
    #"prcp_meanAnnTotal_30yr"                , #"T_warmestMonth_meanAnnAvg_30yr"       , 
    #"T_coldestMonth_meanAnnAvg_30yr"        
    "precip_wettestMonth_meanAnnAvg_30yr"  , 
    "precip_driestMonth_meanAnnAvg_30yr"    , #"precip_Seasonality_meanAnnAvg_30yr"   , 
    "PrecipTempCorr_meanAnnAvg_30yr"       , #"aboveFreezing_month_meanAnnAvg_30yr"   ,
    "isothermality_meanAnnAvg_30yr"        ,  "annWaterDeficit_meanAnnAvg_30yr"       ,
    #"annWetDegDays_meanAnnAvg_30yr"        ,  #"annVPD_mean_meanAnnAvg_30yr"           ,
    #"annVPD_max_meanAnnAvg_30yr"           ,  #"annVPD_min_meanAnnAvg_30yr"            ,
    "annVPD_max_95percentile_30yr"         ,  #"annWaterDeficit_95percentile_30yr"     ,
    #"annWetDegDays_5percentile_30yr"       ,  #"durationFrostFreeDays_5percentile_30yr",
    #"durationFrostFreeDays_meanAnnAvg_30yr",  
    "soilDepth"                             ,
    #"surfaceClay_perc"                     ,  "avgSandPerc_acrossDepth"               ,
    "avgCoarsePerc_acrossDepth"            ,  "avgOrganicCarbonPerc_0_3cm"            ,
    #"totalAvailableWaterHoldingCapacity" 
  ) %>% 
  rename(#"MAP" =  prcp_meanAnnTotal_30yr, 
    #"T_warmest \nmonth" = T_warmestMonth_meanAnnAvg_30yr, #"T_coldest \nmonth" = T_coldestMonth_meanAnnAvg_30yr,
    "precip_wettest" = precip_wettestMonth_meanAnnAvg_30yr, "precip_driest" = precip_driestMonth_meanAnnAvg_30yr, 
    "P/T corr" = PrecipTempCorr_meanAnnAvg_30yr, "isothermality" = isothermality_meanAnnAvg_30yr,
    "watDef" = annWaterDeficit_meanAnnAvg_30yr, #"Wet \n DegDays" = annWetDegDays_meanAnnAvg_30yr,  
    #"VPD_max" = annVPD_max_meanAnnAvg_30yr,# "VPD_min" = annVPD_min_meanAnnAvg_30yr, 
    "VPD_max_95" = annVPD_max_95percentile_30yr, #"watDef_95" = annWaterDeficit_95percentile_30yr,
    #"wetDegDays_5" = annWetDegDays_5percentile_30yr, 
    #"surfClay" = surfaceClay_perc, 
    #"sand" = avgSandPerc_acrossDepth, 
    "coarse" = avgCoarsePerc_acrossDepth, 
    "carbon" = avgOrganicCarbonPerc_0_3cm, #"waterCap." = totalAvailableWaterHoldingCapacity
    ) %>% 
    slice_sample(n = 5e4) %>% 
    ggpairs( upper = list(continuous = my_fn), lower = list(continuous = GGally::wrap("points", alpha = 0.1, size=0.2)), progress = FALSE))


# Fit a simple regression (multinomial log-normal regression) ----------------
# I think the classification algorithm would be more straightforward to implement??
# following this example:
# https://stats.oarc.ucla.edu/r/dae/multinomial-logistic-regression/
modDat_fit$newRegionFact <- relevel(modDat_fit$newRegion, ref = "dryShrubGrass")
testMod <- nnet::multinom(newRegionFact ~ precip_wettestMonth_meanAnnAvg_30yr  + 
                          precip_driestMonth_meanAnnAvg_30yr    + 
                          PrecipTempCorr_meanAnnAvg_30yr       + 
                          isothermality_meanAnnAvg_30yr        +  annWaterDeficit_meanAnnAvg_30yr       +
                          annVPD_max_95percentile_30yr         +   
                          soilDepth                             +
                          avgCoarsePerc_acrossDepth            +  avgOrganicCarbonPerc_0_3cm ,
                    data = modDat_fit
                  )

summary(testMod)

head(pp <- fitted(testMod))

# get variable importance
# first get z scores
z <- summary(testMod)$coefficients/summary(testMod)$standard.errors
z
# then do a 2-tailed z test
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

# test the predictions of the model
predictionDat <- modDat_test
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
    mutate(
           newRegion_group = as.factor(pmap_chr(.[c("dryShrubGrass", "eastForest", "westForest")], paste0))) %>% 
    select(-dryShrubGrass, -eastForest, -westForest) %>% 
    cbind(predictionDatTemp %>%
            mutate(newRegion_Fact = as.factor(newRegion)) %>% select(dryShrubGrass, eastForest, westForest)) %>% 
    rename("dryShrubGrass_prob" = dryShrubGrass, 
           "eastForest_prob" = eastForest, 
           "westForest_prob" = westForest)
  )
# are these predictions the same as the actual classifications
temp$goodPred <- temp$newRegion_Fact == temp$newRegion_group
sum(!temp$goodPred)/nrow(temp) * 100

(regModPredMAP <- ggplot(temp) +
    geom_point(aes(Long, Lat, col = newRegion_group), alpha = .1) +
    ggtitle("model-predicted ecoregion classification -- Model with all possible uncorrelated predictors",
            subtitle = paste0("ecoregion ~ precip of wettest month + precip of driest month + \n precip/temp correlation + isothermality + \n water deficit + 95th percentile VPD max + \n soil depth + total coarse frac. + surface organic matter
\n black dots indicate misclassification, \n fill color indicates which ecoregion a point was misclassified as; \n  ", 
                              round((100-sum(!temp$goodPred)/nrow(temp) * 100),1), 
                              "% classification accuracy" )) +
    geom_point(data = temp[temp$goodPred == FALSE,], aes(Long, Lat, fill = newRegion_group),
               col = "black"
               , shape = 21))

## confusion matrix
regMod_confMat <- caret::confusionMatrix(data = temp$newRegion_group,
                                               reference = temp$newRegion_Fact)$table

## make ternary plots
# just good predictions
(regMod_ternGoodPreds <- temp %>% 
    filter(goodPred == TRUE) %>% 
    ggtern(aes(#x=jitter(
      dryShrubGrass_prob*100 + 1#, 50)
      ,
               #y=jitter(
      eastForest_prob*100 + 1#, 50)
      ,
               #z=jitter(
      westForest_prob*100 + 1#, 50)
      , col = newRegion)) +
    geom_point(alpha = .2) +
    Tlab("Eastern Forest") +
    Llab("Shrub/\nGrass") + 
    Rlab("Western \nForest") +
    theme_minimal() + 
    ggtitle("Accurate Predictions"))
# just bad predictions
badDat <- temp %>% 
  filter(goodPred == FALSE)
(regMod_ternBadPreds <- 
    ggtern( data = badDat, aes(x=jitter(dryShrubGrass_prob*100 + 1, 50),
                               y=jitter(eastForest_prob*100 + 1, 50),
                               z=jitter(westForest_prob*100 + 1, 50), col = newRegion)) +
    geom_point(alpha = .2) +
    Tlab("Eastern Forest") +
    Llab("Shrub/\nGrass") + 
    Rlab("Western \nForest") +
    theme_minimal() + 
    ggtitle("Bad Predictions"))

# map of misclassified points 
temp$color <- rgb(red = temp$dryShrubGrass_prob, 
                              green = temp$eastForest_prob,
                               blue = temp$westForest_prob)

(regModPred_mapBadGrass <- ggplot() +
    geom_point(data = temp, aes(Long, Lat, col = newRegion), alpha = .1) +
    ggtitle("Severity of Grass/Shrub misclassification") +
    geom_point(data = temp[temp$goodPred == FALSE & temp$newRegion_group == "dryShrubGrass",], 
               aes(Long, Lat, fill =dryShrubGrass_prob),
               col = "black" ,
               shape = 21) +
    scale_fill_distiller(type = "seq", palette = "Reds")
)
(regModPred_mapBadEastForest <- ggplot() +
    geom_point(data = temp, aes(Long, Lat, col = newRegion), alpha = .1) +
    ggtitle("Severity of Eastern Forest misclassification") +
    geom_point(data = temp[temp$goodPred == FALSE & temp$newRegion_group == "eastForest",], 
               aes(Long, Lat, fill = eastForest_prob),
               col = "black" ,
               shape = 21) +
    scale_fill_distiller(type = "seq", palette = "Greens")
) 

(regModPred_mapBadWestForest <- ggplot() +
    geom_point(data = temp, aes(Long, Lat, col = newRegion), alpha = .1) +
    ggtitle("Severity of Western Forest misclassification") +
    geom_point(data = temp[temp$goodPred == FALSE & temp$newRegion_group == "westForest",], 
               aes(Long, Lat, fill = westForest_prob),
               col = "black" ,
               shape = 21) +
    scale_fill_distiller(type = "seq", palette = "Blues")
) 



## arrange all of the figures for this model
ggpubr::annotate_figure(ggarrange(
 # regMod_varImpPlot,
  ggplotify::as.grob(gt::as_gtable(gt::gt(data.frame("dryShrubGrass" =  data.frame(regMod_confMat)$Freq[1:3], 
                                                     "eastForest" = data.frame(regMod_confMat)$Freq[4:6],
                                                     "westForest" = data.frame(regMod_confMat)$Freq[7:9], 
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
  regModPredMAP, 
  ggarrange((ggtern::arrangeGrob(regMod_ternGoodPreds)), 
            (ggtern::arrangeGrob(regMod_ternBadPreds))), 
  regModPred_mapBadGrass, 
  regModPred_mapBadEastForest, 
  regModPred_mapBadWestForest, 
  nrow = 6
)#, 
#top = c("Model with depth = 7 and all climate and soil predictors")
) %>% 
  ggexport(
    filename = "./Figures/EcoRegionModelFigures/ModelFigures_RegressionModelResults.pdf",
    width = 12, height = 23)


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

