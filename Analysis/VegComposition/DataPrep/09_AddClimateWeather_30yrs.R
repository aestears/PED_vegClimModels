#///////////////////
# Adding climate data for generic 30 year period (rather than just for proper previous 30 years) to all data in a given location
# Alice Stears
# 12/10/24
#///////////////////


# load packages -----------------------------------------------------------

library(tidyverse)


# load data ---------------------------------------------------------------
data <- readRDS("./Data_processed/CoverData/DataForModels_spatiallyAveraged_withSoils_noSf.rds")


# re-average climate data -------------------------------------------------

# Problem: We only have climate data from 1981 onward. Currently, climate data
# is predicted for the 30 years prior to when the observation occured, but that
# means that we don't have climate data for any observations occurring before
# 2011. This substantially impairs the extent of our dataset, so I think we
# should use the 'earliest' 30-year period of data (1981 - 2009) for those
# observations prior to 2011.

# get the 30 year mean climate values for each location/year combination
test <- data %>% 
  #slice(500000:600000) %>% 
  group_by(Long,  Lat, Year.x) %>% 
  summarize(swe_meanAnnAvg_CLIM    =  mean(swe_meanAnnAvg_30yr),                    
            tmin_meanAnnAvg_CLIM   = mean(tmin_meanAnnAvg_30yr),                   
            tmax_meanAnnAvg_CLIM   = mean(tmax_meanAnnAvg_30yr),                    
                          tmean_meanAnnAvg_CLIM  = mean(tmean_meanAnnAvg_30yr),                   
                             vp_meanAnnAvg_CLIM     = mean(vp_meanAnnAvg_30yr),                    
                         prcp_meanAnnTotal_CLIM = mean(prcp_meanAnnTotal_30yr),                    
                 T_warmestMonth_meanAnnAvg_CLIM = mean(T_warmestMonth_meanAnnAvg_30yr),            
                 T_coldestMonth_meanAnnAvg_CLIM = mean(T_coldestMonth_meanAnnAvg_30yr),           
            precip_wettestMonth_meanAnnAvg_CLIM = mean(precip_wettestMonth_meanAnnAvg_30yr),       
             precip_driestMonth_meanAnnAvg_CLIM  = mean(precip_driestMonth_meanAnnAvg_30yr),      
             precip_Seasonality_meanAnnAvg_CLIM  = mean(precip_Seasonality_meanAnnAvg_30yr),        
                 PrecipTempCorr_meanAnnAvg_CLIM      = mean(PrecipTempCorr_meanAnnAvg_30yr),      
            aboveFreezing_month_meanAnnAvg_CLIM = mean(aboveFreezing_month_meanAnnAvg_30yr),       
                  isothermality_meanAnnAvg_CLIM       = mean(isothermality_meanAnnAvg_30yr),      
                annWaterDeficit_meanAnnAvg_CLIM     = mean(annWaterDeficit_meanAnnAvg_30yr),       
                  annWetDegDays_meanAnnAvg_CLIM       = mean(annWetDegDays_meanAnnAvg_30yr),      
                    annVPD_mean_meanAnnAvg_CLIM         = mean(annVPD_mean_meanAnnAvg_30yr),       
                     annVPD_max_meanAnnAvg_CLIM          = mean(annVPD_max_meanAnnAvg_30yr),      
                     annVPD_min_meanAnnAvg_CLIM          = mean(annVPD_min_meanAnnAvg_30yr),       
                   annVPD_max_95percentile_CLIM        = mean(annVPD_max_95percentile_30yr),      
              annWaterDeficit_95percentile_CLIM   = mean(annWaterDeficit_95percentile_30yr),       
                 annWetDegDays_5percentile_CLIM      = mean(annWetDegDays_5percentile_30yr),     
         durationFrostFreeDays_5percentile_CLIM = mean(durationFrostFreeDays_5percentile_30yr),     
          durationFrostFreeDays_meanAnnAvg_CLIM  = mean(durationFrostFreeDays_meanAnnAvg_30yr),    
                                     Start_CLIM  = mean(Start_30yr))

# get the values for the first year when there are values (2010)
# vals2010 <- test %>% 
#   filter(Year.x == 2010)

# add these earliest climate values for 2010 to any location that was measured prior to 2010
data_pre2010 <- data %>% 
  filter(Year.x < 2010) %>% 
  left_join(test  %>% select(-Year.x)) %>% 
  drop_na(Start_CLIM) 

# there are many locations where there are multiple years of matches for a
# spatial location (and many where there aren't any matches at all, which I
# dropped w/ the "drop_na" above). For those locations where there are multiple
# options, chose the 'earliest' start year possible

goodYears <- data_pre2010 %>% 
  group_by(Long, Lat, Year.x) %>% 
  summarize(StartClimYear_Best = min(Start_CLIM)) %>% 
  mutate(good = "good")

data_pre2010FINAL <- data_pre2010 %>% 
  left_join(goodYears, by = c("Long", "Lat", "Year.x", "Start_CLIM" = "StartClimYear_Best")) %>% 
  select(-good)

# add these climate values for the 30-year window prior to each sampling location in years after 2010
data_post2010 <- data %>% 
  filter(Year.x >= 2010) %>% 
  left_join(test)

data_all <- rbind(data_pre2010FINAL, data_post2010) %>% 
  select(-Year.y) %>% 
  rename(Year = Year.x)


# save the resulting data -------------------------------------------------
saveRDS(data_all, "./Data_processed/CoverData/DataForModels_finalFINAL.rds")

