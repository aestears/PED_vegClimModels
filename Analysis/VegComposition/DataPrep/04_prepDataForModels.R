#///////////////////
# Fitting models to vegetation composition data
# Alice Stears
# 6/26/24
#///////////////////


# load packages -----------------------------------------------------------

library(tidyverse)
library(terra)
#library(betareg)
library(glmmTMB)
library(corrplot)
library(sf)
#library(StepBeta)
#library(lmtest)

# load data ---------------------------------------------------------------

# load vegetation data
vegDat <- readRDS("./Data_processed/CoverData/dataForAnalysis_fireRemoved.rds") 
# load meteorological data
# dayMet <- readRDS("./Data_processed/CoverData/dayMetClimateValuesForAnalysis_final.rds")
# # # remove monthly values 
# # dayMet2 <- dayMet %>% 
# #   select(-names(dayMet)[c(4:22)])
# # make dayMet spatial 
# dayMet3 <- dayMet %>% 
#   st_as_sf(coords = c("Long", "Lat"))
# 

# Add ecoregion data  -----------------------------------------------------
# regions <- sf::st_read(dsn = "./Data_raw/Level2Ecoregions/", layer = "NA_CEC_Eco_Level2") 

# regions_2 <- st_transform(regions, crs = st_crs(vegDat)) %>% 
#   st_make_valid()
# 
# crs(regions_2) == crs(vegDat)
# 
# #mapview(regions_2 %>% slice_sample(n = 10000)) + mapview(modDat_2 %>% slice_sample(n = 10000))
# 
# # match the ecoregion to point data ---------------------------------------
# vegDat_3 <- sf::st_join(vegDat, regions_2)
# 
# # missing for places right on the coast... but not a big deal to not have those
# 
# # group into coarser ecoregions -------------------------------------------
# # make a lookup table
# # (eastern forest) put eastern temperate forests, tropical wet forests and northern forests together
# # (dry shrub and grass) put great plains, southern semiarid highlands, Mediterranean California and north american deserts together 
# # (western forest) put northwestern forested mountains, marine west coast forests together, and temperate sierras together
# 
# ecoReg_lu <- data.frame("NA_L1NAME" = sort(unique(vegDat_3$NA_L1NAME)), 
#                         "newRegion" = c("eastForest", "dryShrubGrass", "westForest",
#                                         "dryShrubGrass", "dryShrubGrass", "eastForest",
#                                         "westForest", "dryShrubGrass", "westForest", 
#                                         "eastForest", NA
#                         ))
# 
# # add to main data.frame 
# newDat <- vegDat_3 %>% 
#   left_join(ecoReg_lu) %>% 
#   mutate(newRegion = as.factor(newRegion), 
#          rowID = 1:nrow(vegDat_3)) 
# # 
# # ggplot(newDat[1:1000000,]) +
# #   geom_sf(aes(col = newRegion))
# # newDat %>% 
# #   filter(Source == "FIA") %>% 
# #   #filter(Year == 2016) %>% 
# #   ggplot() + 
# #   geom_sf(aes(col = TtlTrCv))
# # 
# # # 
# ## save data for further analysis 
saveRDS(vegDat, file = "./Data_processed/CoverData/DataForModels.RDS")
