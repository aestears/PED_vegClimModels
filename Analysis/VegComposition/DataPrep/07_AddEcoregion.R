#///////////////////
# Determining the level-1 ecoregion of each data point
# Alice Stears
# 7/26/24
#///////////////////


# load pacakges -----------------------------------------------------------

library(tidyverse)
library(sf)
library(terra)

# load data ---------------------------------------------------------------

# data ready for model fitting
modDat <- readRDS("./Data_processed/CoverData/DataForModels_spatiallyAveragedWithN_sf.rds")
# get level 1 ecoregions shapefiles
regions <- sf::st_read(dsn = "./Data_raw/Level1Ecoregions/", layer = "NA_CEC_Eco_Level1")

ggplot(regions[200:400,]) + 
  geom_sf(aes(col = NA_L1NAME))
# ensure projections are the same -----------------------------------------
# want them both to have this same crs as below
test_rast <-  rast("./Data_raw/dayMet/rawMonthlyData/orders/70e0da02b9d2d6e8faa8c97d211f3546/Daymet_Monthly_V4R1/data/daymet_v4_prcp_monttl_na_1980.tif")

regions_2 <- st_transform(regions, crs = st_crs(test_rast)) %>% 
  st_make_valid()
crs(test_rast) == crs(regions_2)
# transform modDat to sf to later use st_join
modDat_2 <- st_as_sf(modDat, coords = c("Lon", "Lat"))
modDat_2 <- st_set_crs(modDat_2, value = st_crs(test_rast))

crs(regions_2) == crs(modDat_2)

# match the ecoregion to point data ---------------------------------------
modDat_3 <- sf::st_join(modDat_2, regions_2)
# missing for places right on the coast... but not a big deal to not have those
plot(modDat_3[is.na(modDat_3$NA_L1CODE), "geometry"])
# group into coarser ecoregions -------------------------------------------
# make a lookup table
# (eastern forest) put eastern temperate forests, tropical wet forests and northern forests together
# (dry shrub and grass) put great plains, southern semiarid highlands, Mediterranean California and north american deserts together 
# (western forest) put northwestern forested mountains, marine west coast forests together, and temperate sierras together

ecoReg_lu <- data.frame("NA_L1NAME" = sort(unique(modDat_3$NA_L1NAME)), 
                        "newRegion" = c("eastForest", "dryShrubGrass", "westForest",
                                        "dryShrubGrass", "dryShrubGrass", "eastForest",
                                        "westForest", "dryShrubGrass", "westForest", 
                                        "eastForest", NA
                                        ))

# add to main data.frame 
newDat <- modDat_3 %>% 
  left_join(ecoReg_lu) %>% 
  mutate(newRegion = as.factor(newRegion)) 

ggplot(newDat[1:100000,]) + 
  geom_sf(aes(fill = newRegion))

# Save Data for further analysis ------------------------------------------
saveRDS(newDat, "./Data_processed/CoverData/DataForModels_withEcoregion.rds")

