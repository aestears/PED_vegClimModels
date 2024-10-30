#///////////////////////////
# Preparing data from FIA to be used in subsequent biomass ~ climate relationships
# Alice Stears
# 30 September 2024
#///////////////////////////


# load packages -----------------------------------------------------------

library(tidyverse)
library(sf)
library(terra)

# Read in RAP cover data --------------------------------------------------

RAPcover <- readRDS("./Data_processed/CoverData/DataForModels.rds") 

RAPcover <- RAPcover[RAPcover$Source == "RAP",]

# Read in RAP biomass data ------------------------------------------------

# years are 1992, 1999, and 2014
RAPbiomass_temp <- read.csv("./Data_raw/RAP_BiomassData/RAP_PeakBiomass/RAP_VegBiomass_gridMetScale.csv")
RAPbiomass <- RAPbiomass_temp %>% 
  rename("Year" = "system.index",
         "HerbaceousBiomass" = "mean",
         "geometry" = ".geo") %>% 
  mutate(Year = as.integer(str_split_i(Year, "_", i = 1))) %>% 
  filter(Year %in% c(1992, 1999, 2014))

# fix geometry column
RAPbiomass$Lat <- as.numeric(str_split_i(
  str_split_i(
    str_split_i(RAPbiomass$geometry, pattern = "\\[", i = 2), 
    pattern = "\\]",
    i = 1
    ),
  pattern = ",",
  i = 2
)
)
RAPbiomass$Lon <- as.numeric(str_split_i(
  str_split_i(
    str_split_i(RAPbiomass$geometry, pattern = "\\[", i = 2), 
    pattern = "\\]",
    i = 1
  ),
  pattern = ",",
  i = 1
)
)


## have to switch to the appropriate crs 
RAPbiomass_sf <- st_as_sf(RAPbiomass, coords = c( "Lon", "Lat"), crs = st_crs("EPSG:4326"))
RAPbiomass_sf <- st_transform(RAPbiomass_sf, st_crs(RAPcover))

# combine biomass and cover data  -----------------------------------------
RAPall <- RAPbiomass_sf %>% 
  st_join(RAPcover, left = TRUE) %>% 
  filter(Year.x == Year.y) %>% 
  # filter to points that only have herbaceous cover
  filter(!is.na(TotalHerbaceousCover)) %>% 
  rename("Year" = Year.x) %>% 
  select(Year, HerbaceousBiomass, StateUnitCode, Lat, Lon, TotalHerbaceousCover, annVPD_mean:geometry) %>% 
  # convert biomass from lbs/acre to kg/hectare
  mutate(HerbaceousBiomass_kgPerHect = HerbaceousBiomass*1.12085)

test <-  rast("./Data_raw/dayMet/rawMonthlyData/orders/70e0da02b9d2d6e8faa8c97d211f3546/Daymet_Monthly_V4R1/data/daymet_v4_prcp_monttl_na_1980.tif")

RAPall_rast <- terra::rasterize(terra::vect(RAPall), field = "HerbaceousBiomass_kgPerHect",
                                y = test, fun = mean, na.rm = TRUE) %>% 
  terra::aggregate(fact = 32, fun = "mean", na.rm = TRUE) %>% 
  terra::crop(ext(-2000000, 2500000, -2000000, 1200000))

plot(RAPall_rast)

ggplot(RAPall[1:10000,]) + 
  geom_sf(aes(col = HerbaceousBiomass_kgPerHect))
