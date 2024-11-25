#/////////////////// 
# This script tests an approach for estimating vegetation
#cover as a function of climate predictors using multivariate generalized linear 
# modeling, implemented using the mcglm R package
# Alice Stears 
# 11/22/24///////////////////


# Load Packages -----------------------------------------------------------

library(tidyverse) 
# library(devtools)
# install_github("wbonat/mcglm")
library(mcglm)

# Prepare Data ------------------------------------------------------------

modDat <- readRDS(./Data_processed/CoverData/DataForModels_spatiallyAveraged_withSoils_noSf.rds)


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


# try implementing the model  ---------------------------------------------
