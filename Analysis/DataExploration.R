#///////////////////
# Importing and exploring potential data sources
# Alice Stears
# 04/05/2024
#///////////////////


# load packages -----------------------------------------------------------
#devtools::install_github("landscape-data-commons/trex", build_vignettes = TRUE)
library(trex)
library(tidyverse)
library(geojsonsf)


# AIM data exploration ----------------------------------------------------
### this is raw AIM data, I think? not actually quite sure... will move to 
# getting data form the Landscape Data Commons, per Gregor's advice
### read in LMF data 
AIM_LMF <- geojson_sf("./data/AIM_BLM_Natl_AIM_LMF_Hub/BLM_Natl_AIM_LMF_Hub_-6152108000941427991.geojson") %>% 
  mutate(Date_visited_new = str_sub(DateVisited,  start = 6, end = 16))
# convert site visit date to POSIXct
AIM_LMF$Date_visited_new <- as.POSIXct(AIM_LMF$Date_visited_new, tz = "GMT", format = "%d %b %Y")

### read in TerrADat data 
AIM_TDat<- geojson_sf("./data/AIM_BLM_Natl_AIM_TerrADat_Hub/BLM_Natl_AIM_TerrADat_Hub_-8322335809691410121.geojson") %>% 
  mutate(Date_visited_new = str_sub(DateVisited,  start = 6, end = 16)) %>% 
  filter(SpeciesState != "AK")
  # get rid of Alaska
# convert site visit date to POSIXct
AIM_TDat$Date_visited_new <- as.POSIXct(AIM_TDat$Date_visited_new, tz = "GMT", format = "%d %b %Y")

ggplot() + 
  geom_sf(data = AIM_LMF, aes(color = as.factor(lubridate::year(Date_visited_new))), pch = 18, alpha = .5) + 
  geom_sf(data = AIM_TDat, aes(color = as.factor(lubridate::year(Date_visited_new))), pch = 1, alpha = .5) +
  guides(fill = guide_none())


# Get AIM Data using trex R package ---------------------------------------
# get "indicators" data, for all plots!  (keys argument)
# indicatorDat <- trex::fetch_ldc(#keys = "17073113365985482017-09-01", key_type = "PrimaryKey",
#                 data_type = "indicators") %>% 
  # mutate(DateVisited = lubridate::as_date(indicatorDat$DateVisited))
  # 

# trim out Alaska
# indicatorDat <- indicatorDat %>% 
#   filter(Latitude_NAD83 < 55)
# # save as a .rds object for later
# saveRDS(indicatorDat, file = "./data/LandscapeDataCommonsDat/IndicatorDat.rds")
indicatorDat <- readRDS(file = "./data/LandscapeDataCommonsDat/IndicatorDat.rds")

# # get species tables 
# speciesDat <- trex::fetch_ldc(#keys = "17073113365985482017-09-01", key_type = "PrimaryKey", 
#   data_type = "species")
# # remove data for plots in Alaska
# speciesDat <- speciesDat %>% 
#   filter(PrimaryKey %in% unique(indicatorDat$PrimaryKey))
# 
# # save as a .rds object for later
# saveRDS(speciesDat, file = "./data/LandscapeDataCommonsDat/SpeciesDat.rds")
speciesDat <- readRDS(file = "./data/LandscapeDataCommonsDat/SpeciesDat.rds")

## what are the years when the sites were visited? 
unique(lubridate::year(indicatorDat$DateVisited))
# visualize the location of plots
ggplot(indicatorDat[1:20000,]) + 
  geom_point(aes(x = Longitude_NAD83, y = Latitude_NAD83, color = TotalFoliarCover))
