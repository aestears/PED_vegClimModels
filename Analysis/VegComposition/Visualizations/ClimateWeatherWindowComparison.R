#///////////////////
# Comparing windows for climate and weather anomalies
# Alice Stears
# 10/15/25
#///////////////////

# load packages -----------------------------------------------------------
library(tidyverse) 


# load data ---------------------------------------------------------------
dat <- 
  readRDS("Documents/Dropbox_static/Work/NAU_USGS_postdoc/PED_vegClimModels/Data_processed/CoverData/dayMetClimateValuesForAnalysis_final.rds")

# compare cDocuments/Dropbox_static/Work/NAU_USGS_postdoc/PED_vegClimModels/Analysis/# compare climate windows -------------------------------------------------
climLong_30yr <- dat %>% 
  #slice(1:100) %>% 
  select(Long,Lat,year, tmin_meanAnnAvg_CLIM:Start_CLIM
         ) %>% 
  rename_with(~ str_remove(.x,"_CLIM"), .cols = tmin_meanAnnAvg_CLIM:durationFrostFreeDays_meanAnnAvg_CLIM) %>% 
  # only chose years after 2011, since those are the years for which we have an entire 30 years of previous data
  filter(year > 2010) %>% 
  pivot_longer(cols = tmin_meanAnnAvg:durationFrostFreeDays_meanAnnAvg, 
                names_to = "variable", 
               values_to = "value_30yr")

climLong_29yr <- dat %>% 
  select(Long,Lat,year, tmin_meanAnnAvg_29yr:Start_29yr
  ) %>% 
  rename_with(~ str_remove(.x,"_29yr"), .cols = tmin_meanAnnAvg_29yr:durationFrostFreeDays_meanAnnAvg_29yr) %>% 
  # only chose years after 2011, since those are the years for which we have an entire 30 years of previous data
  filter(year > 2010) %>% 
    pivot_longer(cols = tmin_meanAnnAvg:durationFrostFreeDays_meanAnnAvg, 
               names_to = "variable", 
               values_to = "value_29yr")

climLong <- climLong_30yr %>% 
 left_join(climLong_29yr)

climLong_sum <- climLong %>% 
  group_by(variable) %>% 
  summarize(avg_30yr = mean(value_30yr, na.rm = TRUE),
            avg_29yr = mean(value_29yr, na.rm = TRUE)) 

# look at difference in actual values of climate w/ different lags (29 yr vs 30 yr)
ggplot() + 
  facet_wrap(~variable, scales = "free") + 
  geom_density(data = climLong, aes(value_30yr), col = "darkorange") + 
  geom_density(data = climLong, aes(value_29yr), col = "darkgreen") + 
 # geom_vline(data = climLong_sum, aes(xintercept = avg_30yr), col = "darkorange") +
  #geom_vline(data = climLong_sum, aes(xintercept = avg_29yr), col = "darkgreen") +
  theme_classic() + 
  ggtitle("Comparison of climate variables calculated \n over 29-year (green) or 30-year (orange) intervals")

# calculate differences between 3 and 2-year versions of weather for the same place and year
climLong <- climLong %>% 
  mutate(Diff_30minus29 = (value_30yr - value_29yr))

# visualize differences
ggplot(climLong) + 
  facet_wrap(~variable, scales = "free") + 
  #geom_point(aes(Long, Lat, col = Diff_30minus29)) + 
  geom_density(aes(Diff_30minus29), col = "darkblue") +
  theme_classic() + 
  ggtitle("Difference between climate variables (30yr - 29yr) for same location in same year")

# bottom line is that there's really not a big difference between the 29 year and 30 year windows for the climate values (especially since in both cases, we allow the window to be as short as 19 or 20 years (for 29 and 30 year windows, respectively))

# compare weather windows (NOT anomalies) -------------------------------------------------
weathLong_3yr <- dat %>% 
  #slice(1:100) %>% 
  select(Long,Lat,year, tmin_meanAnnAvg_3yr:Start_3yr
  ) %>% 
  rename_with(~ str_remove(.x,"_3yr"), .cols = tmin_meanAnnAvg_3yr:durationFrostFreeDays_meanAnnAvg_3yr) %>% 
  # only chose years after 2011, since those are the years for which we have an entire 30 years of previous data
  filter(year > 1990) %>% 
  pivot_longer(cols = tmin_meanAnnAvg:durationFrostFreeDays_meanAnnAvg, 
               names_to = "variable", 
               values_to = "value_3yr")

weathLong_2yr <- dat %>% 
  select(Long,Lat,year, tmin_meanAnnAvg_2yr:Start_2yr
  ) %>% 
  rename_with(~ str_remove(.x,"_2yr"), .cols = tmin_meanAnnAvg_2yr:durationFrostFreeDays_meanAnnAvg_2yr) %>% 
  # only chose years after 2011, since those are the years for which we have an entire 30 years of previous data
  filter(year > 1990) %>% 
  pivot_longer(cols = tmin_meanAnnAvg:durationFrostFreeDays_meanAnnAvg, 
               names_to = "variable", 
               values_to = "value_2yr")

weathLong <- weathLong_3yr %>% 
  left_join(weathLong_2yr)


weathLong_sum <- weathLong %>% 
  group_by(variable) %>% 
  summarize(avg_3yr = mean(value_3yr, na.rm = TRUE),
            avg_2yr = mean(value_2yr, na.rm = TRUE)) 

# look at difference in actual values of weather w/ different lags (2 yr vs 3 yr)
ggplot() + 
  facet_wrap(~variable, scales = "free") + 
  geom_density(data = weathLong, aes(value_3yr), col = "darkorange") + 
  geom_density(data = weathLong, aes(value_2yr), col = "darkgreen") + 
  #geom_vline(data = weathLong_sum, aes(xintercept = avg_3yr), col = "darkorange") +
  #geom_vline(data = weathLong_sum, aes(xintercept = avg_2yr), col = "darkgreen") +
  theme_classic() + 
  ggtitle("Comparison of weather variables (NOT anomalies) calculated \n over 2-year (green) or 3-year (orange) intervals")

# calculate differences between 3 and 2-year versions of weather for the same place and year
weathLong <- weathLong %>% 
  mutate(Diff_3minus2 = (value_3yr - value_2yr))

# visualize differences
ggplot(weathLong) + 
  facet_wrap(~variable, scales = "free") + 
  #geom_point(aes(Long, Lat, col = Diff_3minus2)) + 
  geom_density(aes(Diff_3minus2), col = "darkblue") +
  theme_classic() + 
  ggtitle("Difference between weather variables (NOT anomalies) (3yr - 2 yr) for same location in same year")

# compare weather anomalies -------------------------------------------------
weathLong_3yrAnom <- dat %>% 
  #slice(1:100) %>% 
  select(Long,Lat,year, tmin_meanAnnAvg_3yrAnom:durationFrostFreeDays_meanAnnAvg_3yrAnom
  ) %>% 
  rename_with(~ str_remove(.x,"_3yrAnom"), .cols = tmin_meanAnnAvg_3yrAnom:durationFrostFreeDays_meanAnnAvg_3yrAnom) %>% 
  # only chose years after 2011, since those are the years for which we have an entire 30 years of previous data
  filter(year > 1990) %>% 
  pivot_longer(cols = tmin_meanAnnAvg:durationFrostFreeDays_meanAnnAvg, 
               names_to = "variable", 
               values_to = "value_3yrAnom")

weathLong_2yrAnom <- dat %>% 
  select(Long,Lat,year, tmin_meanAnnAvg_2yrAnom:durationFrostFreeDays_meanAnnAvg_2yrAnom
  ) %>% 
  rename_with(~ str_remove(.x,"_2yrAnom"), .cols = tmin_meanAnnAvg_2yrAnom:durationFrostFreeDays_meanAnnAvg_2yrAnom) %>% 
  # only chose years after 2011, since those are the years for which we have an entire 30 years of previous data
  filter(year > 1990) %>% 
  pivot_longer(cols = tmin_meanAnnAvg:durationFrostFreeDays_meanAnnAvg, 
               names_to = "variable", 
               values_to = "value_2yrAnom")

weathLong <- weathLong_3yrAnom %>% 
  left_join(weathLong_2yrAnom)


weathLong_sum <- weathLong %>% 
  group_by(variable) %>% 
  summarize(avg_3yr = mean(value_3yrAnom, na.rm = TRUE),
            avg_2yr = mean(value_2yrAnom, na.rm = TRUE)) 

# look at difference in actual values of weather w/ different lags (2 yr vs 3 yr)
ggplot() + 
  facet_wrap(~variable, scales = "free") + 
  geom_density(data = weathLong, aes(value_3yrAnom), col = "darkorange") + 
  geom_density(data = weathLong, aes(value_2yrAnom), col = "darkgreen") + 
  #geom_vline(data = weathLong_sum, aes(xintercept = avg_3yr), col = "darkorange") +
  #geom_vline(data = weathLong_sum, aes(xintercept = avg_2yr), col = "darkgreen") +
  theme_classic() + 
  ggtitle("Comparison of weather anomalies calculated \n over 2-year (green) or 3-year (orange) intervals")

# calculate differences between 3 and 2-year versions of weather for the same place and year
weathLong <- weathLong %>% 
  mutate(Diff_3minus2 = (value_3yrAnom - value_2yrAnom))

# visualize differences
ggplot(weathLong) + 
  facet_wrap(~variable, scales = "free") + 
  #geom_point(aes(Long, Lat, col = Diff_3minus2)) + 
  geom_density(aes(Diff_3minus2), col = "darkblue") +
  theme_classic() + 
  ggtitle("Difference between weather anomalies (3yr - 2 yr) for same location in same year")
