#/////////////////// 
# This script tests an approach for estimating vegetation
#cover as a function of climate predictors using Generalized Joint Additive
#Modeling (GJAM) 
# Alice Stears 
# 8/19/24///////////////////


# Load Packages -----------------------------------------------------------

library(tidyverse) 
library(gjam)
library(sf)

# Prepare Data ------------------------------------------------------------

modDat <- readRDS("./Data_processed/CoverData/DataForModels_spatiallyAveraged_withSoils_noSf.rds")


# sub sample for testing purposes
set.seed(1234)
modDat_1 <- modDat %>% 
  #mutate(Lon = st_coordinates(.)[,1], 
        # Lat = st_coordinates(.)[,2])  %>% 
  #st_drop_geometry() %>% 
  filter(!is.na(newRegion))


pred_vars <- c( "swe_meanAnnAvg_30yr"                     ,   "tmin_meanAnnAvg_30yr"                   ,   
                "tmax_meanAnnAvg_30yr"                   ,    "tmean_meanAnnAvg_30yr"                 ,    
                "vp_meanAnnAvg_30yr"                     ,    "prcp_meanAnnTotal_30yr"                ,    
                "T_warmestMonth_meanAnnAvg_30yr"         ,    "T_coldestMonth_meanAnnAvg_30yr"        ,    
                "precip_wettestMonth_meanAnnAvg_30yr"    ,    "precip_driestMonth_meanAnnAvg_30yr"    ,    
                "precip_Seasonality_meanAnnAvg_30yr"     ,    "PrecipTempCorr_meanAnnAvg_30yr"        ,    
                "aboveFreezing_month_meanAnnAvg_30yr"    ,    "isothermality_meanAnnAvg_30yr"         ,    
                "annWaterDeficit_meanAnnAvg_30yr"        ,    "annWetDegDays_meanAnnAvg_30yr"         ,    
                "annVPD_mean_meanAnnAvg_30yr"            ,    "annVPD_max_meanAnnAvg_30yr"            ,    
                "annVPD_min_meanAnnAvg_30yr"             ,    "annVPD_max_95percentile_30yr"          ,    
                "annWaterDeficit_95percentile_30yr"      ,    "annWetDegDays_5percentile_30yr"        ,    
                "durationFrostFreeDays_5percentile_30yr" ,    "durationFrostFreeDays_meanAnnAvg_30yr" ,
                "swe_meanAnn_5yrAnom"                     ,   "tmean_meanAnnAvg_5yrAnom"                ,  
                "tmin_meanAnnAvg_5yrAnom"                ,    "tmax_meanAnnAvg_5yrAnom"                ,   
                "vp_meanAnnAvg_5yrAnom"                  ,    "prcp_meanAnnTotal_5yrAnom"              ,   
                "T_warmestMonth_meanAnnAvg_5yrAnom"      ,    "T_coldestMonth_meanAnnAvg_5yrAnom"      ,   
                "precip_wettestMonth_meanAnnAvg_5yrAnom" ,    "precip_driestMonth_meanAnnAvg_5yrAnom"  ,   
                "precip_Seasonality_meanAnnAvg_5yrAnom"  ,    "PrecipTempCorr_meanAnnAvg_5yrAnom"      ,   
                "aboveFreezing_month_meanAnnAvg_5yrAnom" ,    "isothermality_meanAnnAvg_5yrAnom"       ,   
                "annWaterDeficit_meanAnnAvg_5yrAnom"     ,    "annWetDegDays_meanAnnAvg_5yrAnom"       ,   
                "annVPD_mean_meanAnnAvg_5yrAnom"         ,    "annVPD_min_meanAnnAvg_5yrAnom"          ,   
                "annVPD_max_meanAnnAvg_5yrAnom"          ,    "annVPD_max_95percentile_5yrAnom"        ,   
                "annWaterDeficit_95percentile_5yrAnom"   ,    "annWetDegDays_5percentile_5yrAnom"      ,   
                "durationFrostFreeDays_5percentile_5yrAnom",  "durationFrostFreeDays_meanAnnAvg_5yrAnom", 
                "soilDepth"                          ,        "surfaceClay_perc"                      ,    
                "avgSandPerc_acrossDepth"            ,        "avgCoarsePerc_acrossDepth"             ,    
                "avgOrganicCarbonPerc_acrossDepth"   ,        "totalAvailableWaterHoldingCapacity"  
               )
# removed VPD, since it's highly correlated w/ tmean and prcp

names(pred_vars) <- pred_vars

# modDat$LitterCover_dec <- modDat$LitterCover/100
# ## convert % cover into fractional cover (So they sum to one)
# modDat$TotalCover_dec <- base::rowSums(modDat[,c(ShrubCover_dec, HerbCover_dec, 
#                                                  TotalGramCover_dec, 
#                                                  TotalTreeCover_dec, 
#                                                  LitterCover_dec
#                                                          )] %>% st_drop_geometry())
# 
# # calculate cover in terms of 'fraction' (rather than the 'dec' version, which can sum to >1)
# modDat_temp <- 
#   modDat %>% 
#   mutate(ShrubCover_frac = ShrubCover_dec/TotalCover_dec, 
#          HerbCover_frac = HerbCover_dec/TotalCover_dec, 
#          TotalGramCover_frac = TotalGramCover_dec/TotalCover_dec, 
#          TotalTreeCover_frac = TotalTreeCover_dec/TotalCover_dec, 
#          LitterCover_frac = LitterCover_dec/TotalCover_dec)

## remove rows for data that have NAs in the predictors (lag starts before range of DayMet data)

modDat_1 <- modDat_1 %>% 
  st_drop_geometry() %>% 
  drop_na(names(pred_vars)) %>% 
  select(ShrubCover, TotalTreeCover, TotalHerbaceousCover, CAMCover, BareGroundCover, newRegion, 
         names(pred_vars))

# break into ecoregions
modDat_shrubGrass <- modDat_1 %>% 
  filter(newRegion == 'dryShrubGrass')
modDat_eastTree <- modDat_1 %>% 
  filter(newRegion == 'eastForest')
modDat_westTree <- modDat_1 %>% 
  filter(newRegion == 'westForest')

# df_sample <- {
#   reordered <- slice_sample(modDat_1, prop = 1)
#   sample_group <- 1
#   n_train <- 15000
#   low <- (sample_group - 1)*n_train + 1 # first row (of reordered data) to get
#   high <- low + n_train - 1 # last row
#   if(high > nrow(modDat_1)) {
#     warning(paste0(n_train size due to low sample size (i.e. < , n_train,)))
#     n_train <- n_train*.8
#     low <- (sample_group - 1)*n_train + 1 # first row (of reordered data) to get
#     high <- low + n_train - 1 # last row
#     #stop('trying to sample from rows that dont exist')
#   }
#   reordered[low:high, ]
#   
# }
#   n_test <- 500
# df_test <-  
#   modDat_1 %>% 
#     anti_join(df_sample) %>% 
#     slice_sample(n = n_test)
# 
# 
# # small sample for certain plots
# df_small <- slice_sample(modDat_1, n = 500)

# Try a GJAM model with all data for grass/shrubs, a subsetting list of covariates w/ uniform priors ---------------------------------------------------
# based on this tutorial (https://rpubs.com/clanescher/gjam)
#(https://cran.r-project.org/web/packages/gjam/vignettes/gjamVignette.html#data-types)
# I think the new version of cover [0,inf] is a 'CA'	continuous abundance data type w/in the GJAM framework
# add row names to df
rownames(modDat_eastTree) <- paste0("gridCellNum_", seq(1:nrow(modDat_eastTree)))

# response data
ydata <- modDat_eastTree %>% 
  select(ShrubCover:BareGroundCover) 
names(ydata) <- c("ShrubCover"               ,  "TotalTreeCover",                           
                  "TotalHerbaceousCover"     ,  "CAMCover"      ,                     
                  "BareGroundCover" )

# predictor data
xdata <- modDat_eastTree %>% 
  select(swe_meanAnnAvg_30yr, tmean_meanAnnAvg_30yr,  
           prcp_meanAnnTotal_30yr, PrecipTempCorr_meanAnnAvg_30yr,
           isothermality_meanAnnAvg_30yr, annWetDegDays_meanAnnAvg_30yr, 
         swe_meanAnn_5yrAnom, tmean_meanAnnAvg_5yrAnom, 
         prcp_meanAnnTotal_5yrAnom, PrecipTempCorr_meanAnnAvg_5yrAnom, 
         isothermality_meanAnnAvg_5yrAnom, annWetDegDays_meanAnnAvg_5yrAnom, 
         soilDepth                      ,     surfaceClay_perc           ,              
         avgSandPerc_acrossDepth        ,     avgCoarsePerc_acrossDepth ,               
         avgOrganicCarbonPerc_acrossDepth,     totalAvailableWaterHoldingCapacity
        )
names(xdata) <- c("sweMeanAnnAvg30yr", "tmeanMeanAnnAvg30yr",  
                  "prcpMeanAnnTotal30yr", "PrecipTempCorrMeanAnnAvg30yr",
                  "isothermalityMeanAnnAvg30yr", "annWetDegDaysMeanAnnAvg30yr", 
                  "sweMeanAnn5yrAnom", "tmeanMeanAnnAvg5yrAnom", 
                  "prcpMeanAnnTotal5yrAnom", "PrecipTempCorrMeanAnnAvg5yrAnom", 
                  "isothermalityMeanAnnAvg5yrAnom", "annWetDegDaysMeanAnnAvg5yrAnom", 
                  "soilDepth"                      ,     "surfaceClayPerc"           ,              
                  "avgSandPercAcrossDepth"        ,     "avgCoarsePercAcrossDepth" ,               
                  "avgOrganicCarbonPercAcrossDepth",     "totalAvailableWaterHoldingCapacity" )

# effort 
edata <- list(columns = 1:ncol(ydata), 
              values = rep(1,ncol(ydata)))


# Set up the model --------------------------------------------------------
  
formula <- as.formula(~ sweMeanAnnAvg30yr+ tmeanMeanAnnAvg30yr+  
                      prcpMeanAnnTotal30yr+ PrecipTempCorrMeanAnnAvg30yr +
                      isothermalityMeanAnnAvg30yr+ annWetDegDaysMeanAnnAvg30yr+ 
                      sweMeanAnn5yrAnom+ tmeanMeanAnnAvg5yrAnom+ 
                      prcpMeanAnnTotal5yrAnom+ PrecipTempCorrMeanAnnAvg5yrAnom+ 
                      isothermalityMeanAnnAvg5yrAnom+ annWetDegDaysMeanAnnAvg5yrAnom+ 
                      soilDepth                      +     surfaceClayPerc           +              
                      avgSandPercAcrossDepth        +     avgCoarsePercAcrossDepth +               
                      avgOrganicCarbonPercAcrossDepth+     totalAvailableWaterHoldingCapacity )

# I don't think we should need to do dimension reduction, since we have low S (types of responses) and low n

# set up model priors: 
# start with weakly informative, wide priors 
prior <- gjamPriorTemplate(formula = formula, 
                           xdata = xdata, ydata = ydata, lo = -20, hi = 20)


# make the model list
mlist <- list(ng=2000, # not sure if thisis good
              burnin = 250,  # not sure if this is good
              typeNames = 'CA', 
              betaPrior = prior, 
              effort = edata)


# run the model!  ---------------------------------------------------------
startTime <- Sys.time()
out <- gjam(formula = formula, xdata = xdata, ydata = ydata, modelList = mlist)
endTime <- Sys.time()

# view the results --------------------------------------------------------

gjamPlot(out) 

# create base for plots
base <- ggplot() +
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
base +
  geom_point(aes(x = out$inputs$y[,1],
                 y = out$prediction$ypredMu[,1])) +
  labs(x = "observed", y = "predicted", title = paste0(colnames(out$inputs$y)[1], ", common response")) + 
  geom_abline(aes(slope = 1, intercept = 0), col = "red")
 
base +
  geom_point(aes(x = out$inputs$y[,5],
                 y = out$prediction$ypredMu[,5])) +
  labs(x = "observed", y = "predicted", title = paste0(colnames(out$inputs$y)[5], ", uncommon"))+ 
  geom_abline(aes(slope = 1, intercept = 0), col = "red")

