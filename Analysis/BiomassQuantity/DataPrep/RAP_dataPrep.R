#///////////////////////////
# Preparing data from RAP to be used in subsequent biomass ~ climate relationships
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


# save data
saveRDS(RAPall, "./Data_processed/BiomassQuantityData/HerbBiomassCover_withWeatherAndFireFiltering.rds")
RAPall <- readRDS("./Data_processed/BiomassQuantityData/HerbBiomassCover_withWeatherAndFireFiltering.rds")
# visualize data ----------------------------------------------------------

test <-  rast("./Data_raw/dayMet/rawMonthlyData/orders/70e0da02b9d2d6e8faa8c97d211f3546/Daymet_Monthly_V4R1/data/daymet_v4_prcp_monttl_na_1980.tif")

years <- unique(RAPall$Year)
RAPall_rast <- lapply(years, FUN = function(x) {
  temp <- RAPall %>% 
    filter(Year == x) %>% 
    terra::vect() %>% 
    terra::rasterize(field = "HerbaceousBiomass_kgPerHect",
                     y = test, fun = mean, na.rm = TRUE)
  return(temp)
})
names(RAPall_rast) <- years
RAPall_rast <- c(RAPall_rast[[1]], RAPall_rast[[2]], RAPall_rast[[3]])
names(RAPall_rast) <- years

# save RAP data
saveRDS(RAPall_rast, "./Data_processed/BiomassQuantityData/HerbBiomassCover_withWeatherAndFireFiltering_rast.rds")

