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

## set the crs
RAPcover <-   st_transform(RAPcover, crs = ("GEOGCRS[\"NAD83\",\n    DATUM[\"North American Datum 1983\",\n        ELLIPSOID[\"GRS 1980\",6378137,298.257222101,\n            LENGTHUNIT[\"metre\",1]]],\n    PRIMEM[\"Greenwich\",0,\n        ANGLEUNIT[\"degree\",0.0174532925199433]],\n    CS[ellipsoidal,2],\n        AXIS[\"geodetic latitude (Lat)\",north,\n            ORDER[1],\n            ANGLEUNIT[\"degree\",0.0174532925199433]],\n        AXIS[\"geodetic longitude (Lon)\",east,\n            ORDER[2],\n            ANGLEUNIT[\"degree\",0.0174532925199433]],\n    ID[\"EPSG\",4269]]"))
# Read in RAP biomass data ------------------------------------------------

# years are 1992, 1999, and 2014
RAPbiomass_temp <- read.csv("./Data_raw/RAP_BiomassData/RAP_PeakBiomass/RAP_VegBiomass_gridMetScale.csv")
RAPbiomass <- RAPbiomass_temp %>% 
  rename("Year" = "system.index",
         "HerbaceousBiomass" = "mean",
         "geometry" = ".geo") %>% 
  mutate(Year = as.integer(str_split_i(Year, "_", i = 1))) #%>% 
  #filter(Year %in% c(1992, 1999, 2014))

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
  st_join(RAPcover, left = TRUE, join = st_nearest_feature) %>% 
  filter(Year.x == Year.y) %>% 
  # filter to points that only have herbaceous cover
  filter(!is.na(TtlHrbC)) %>% 
  select(Year.x, HerbaceousBiomass, SttUntC, Lat, Lon, TtlHrbC, #annVPD_mean:
         geometry) %>% 
  # convert biomass from lbs/acre to kg/hectare
  mutate(HerbaceousBiomass_kgPerHect = HerbaceousBiomass*1.120851155716688)


# save data
saveRDS(RAPall, "./Data_processed/BiomassQuantityData/HerbBiomassCover_withWeatherAndFireFiltering.rds")
RAPall <- readRDS("./Data_processed/BiomassQuantityData/HerbBiomassCover_withWeatherAndFireFiltering.rds")
# visualize data ----------------------------------------------------------

test <-  rast("./Data_raw/dayMet/rawMonthlyData/orders/70e0da02b9d2d6e8faa8c97d211f3546/Daymet_Monthly_V4R1/data/daymet_v4_prcp_monttl_na_1980.tif")

years <- unique(RAPall$Year.x)
RAPall_rast <- lapply(years, FUN = function(x) {
  temp <- RAPall %>% 
    filter(Year.x == x) %>% 
    terra::vect() %>% 
    terra::rasterize(field = "HerbaceousBiomass_kgPerHect",
                     y = test, fun = mean, na.rm = TRUE)
  return(temp)
})
names(RAPall_rast) <- years
RAPall_rast <- c(RAPall_rast$`1986`, RAPall_rast$`1987`, RAPall_rast$`1988`,  RAPall_rast$`1989`,  RAPall_rast$`1990`, RAPall_rast$`1991`, RAPall_rast$`1992`,
                 RAPall_rast$`1993`, RAPall_rast$`1994`, RAPall_rast$`1995`, RAPall_rast$`1996`, RAPall_rast$`1997`, RAPall_rast$`1998`, RAPall_rast$`1999`, 
                 RAPall_rast$`2000`, RAPall_rast$`2001`, RAPall_rast$`2002`, RAPall_rast$`2003`, RAPall_rast$`2004`, RAPall_rast$`2005`, RAPall_rast$`2006`,
                 RAPall_rast$`2007`, RAPall_rast$`2008`, RAPall_rast$`2009`, RAPall_rast$`2010`, RAPall_rast$`2011`, RAPall_rast$`2012`, RAPall_rast$`2013`, 
                 RAPall_rast$`2014`, RAPall_rast$`2015`, RAPall_rast$`2016`, RAPall_rast$`2017`, RAPall_rast$`2018`, RAPall_rast$`2019`, RAPall_rast$`2020`,
                 RAPall_rast$`2021`, RAPall_rast$`2022`, RAPall_rast$`2023`)
names(RAPall_rast) <- years

# save RAP data
saveRDS(RAPall_rast, "./Data_processed/BiomassQuantityData/HerbBiomassCover_withWeatherAndFireFiltering_rast.rds")

