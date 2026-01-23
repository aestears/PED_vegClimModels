# biomass data to share w/ Martin 
#w 01/22/26

dat_temp <- readRDS("/Users/astears/Documents/Dropbox_static/Work/NAU_USGS_postdoc/cleanPED/PED_vegClimModels/Data_processed/BiomassQuantityData/dataForAnalysis_fireAndDevelopmentRemoved.rds")

# get only total biomass from GEDI
dat <- dat_temp %>% 
  filter(biomassSource == "GEDI") %>% 
  sf::st_drop_geometry() %>% 
  drop_na(shrubCover_rel)

# clarify names and remove unnecessary columns
dat <- dat %>% 
  select( biomassSource, biomass_MgPerHect, biomassType, year, tmin:Start_CLIM, soilDepth:AWHC, x, y, burnedMoreThan20YearsAgo,
          totalTreeCover_rel:ForbCover_rel
  ) 

# save data 
write.csv(dat, file = "./Data_processed/BiomassQuantityData/GEDIbiomass_modeledCover_clim_soils.csv", row.names = FALSE)

