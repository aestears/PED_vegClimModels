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
# get the dayMet grid 
test_rast <-  rast("./Data_raw/dayMet/rawMonthlyData/orders/70e0da02b9d2d6e8faa8c97d211f3546/Daymet_Monthly_V4R1/data/daymet_v4_prcp_monttl_na_1980.tif") 
# import RAP biomass data and reproject to the dayMet scale 
RAPbiomass_2019 <- terra::rast("./Data_raw/RAP_BiomassData/RAP_PeakBiomass/vegetation-biomass-v3-2019.tif") %>% 
  terra::project(test_rast) %>% 
  terra::resample(test_rast) 
RAPbiomass_2020 <- terra::rast("./Data_raw/RAP_BiomassData/RAP_PeakBiomass/vegetation-biomass-v3-2020.tif") %>% 
  terra::project(test_rast) %>% 
  terra::resample(test_rast) 
RAPbiomass_2021 <- terra::rast("./Data_raw/RAP_BiomassData/RAP_PeakBiomass/vegetation-biomass-v3-2021.tif") %>% 
  terra::project(test_rast) %>% 
  terra::resample(test_rast) 

## sum the two bands (band 1 - annual forb and grass, Band 2 - perennial forb and grass) to total bioamss (in lbs/acre)
RAPbiomass_2019$`TotalHerbaceousBiomass-v3-2019` <- RAPbiomass_2019$`vegetation-biomass-v3-2019_1` + RAPbiomass_2019$`vegetation-biomass-v3-2019_2`
RAPbiomass_2020$`TotalHerbaceousBiomass-v3-2020` <- RAPbiomass_2020$`vegetation-biomass-v3-2020_1` + RAPbiomass_2020$`vegetation-biomass-v3-2020_2`
RAPbiomass_2021$`TotalHerbaceousBiomass-v3-2021` <- RAPbiomass_2021$`vegetation-biomass-v3-2021_1` + RAPbiomass_2021$`vegetation-biomass-v3-2021_2`

# Average values together across three years 
RAPbiomass_2019to2021 <- (RAPbiomass_2019$`TotalHerbaceousBiomass-v3-2019` + RAPbiomass_2020$`TotalHerbaceousBiomass-v3-2020` + RAPbiomass_2021$`TotalHerbaceousBiomass-v3-2021`)/3

rm(RAPbiomass_2019, RAPbiomass_2020, RAPbiomass_2021)
gc()

# Curtail to grass/shrub region -------------------------------------------
# import shapefiles of ecoregion boundaries 
# load ecoregion data
regions <- sf::st_read(dsn = "./Data_raw/Level1Ecoregions/", layer = "NA_CEC_Eco_Level1")

# make sure crs of climate and ecoregion data are the same 
# want them both to have this same crs as below

regions_2 <- st_transform(regions, crs = st_crs(test_rast)) %>% 
  st_make_valid()
crs(test_rast) == crs(regions_2)

## make ecoregions coarser
ecoReg_lu <- data.frame("NA_L1NAME" = sort(unique(regions_2$NA_L1NAME)), 
                        "newRegion" = c(NA, "Forest", "dryShrubGrass", NA, 
                                        "Forest", "dryShrubGrass", "dryShrubGrass", "Forest", 
                                        "Forest", "Forest", NA, "Forest", 
                                        "Forest", "Forest", NA, NA
                        ))

regions_2 <- regions_2 %>% left_join(ecoReg_lu)

# get data only for grassShrub
regions_grassShrub <- regions_2[regions_2$newRegion == "dryShrubGrass" & !is.na(regions_2$newRegion), ]

# use the grassShrub ecoregion to trim the RAP data to only that ecoregion
RAPbiomass_2019to2021_grassShrub <- RAPbiomass_2019to2021 %>% 
  terra::crop(terra::vect(regions_grassShrub)) %>% 
  mask(terra::vect(regions_grassShrub))


# Convert lbs/acre to Mg/hectare ------------------------------------------
RAPbiomass_2019to2021_grassShrub_Mg_hect <- (RAPbiomass_2019to2021_grassShrub$`TotalHerbaceousBiomass-v3-2019`*1.120851155716688)/1000
names(RAPbiomass_2019to2021_grassShrub_Mg_hect) <- "TotalHerbaceousBiomass_Mg_Hect_AvgFrom2019to2021"
# rescale to be on dayMet scale  ------------------------------------------
RAPbiomass_2019to2021_grassShrub_Mg_hect <- RAPbiomass_2019to2021_grassShrub_Mg_hect %>% 
  terra::project(test_rast, method = "bilinear") %>% 
  terra::resample(test_rast, method = "bilinear") %>% 
  terra::crop(ext(-2560750, 3053250, -2091500, 1584500))


# save RAP data
terra::writeRaster(RAPbiomass_2019to2021_grassShrub_Mg_hect, filename = "./Data_processed/BiomassQuantityData/RAP_biomassRaster_totalHerb_Mg_Hect_averagedfrom2019to2021.tif", overwrite = TRUE)

# # years are 1992, 1999, and 2014
# RAPbiomass_temp <- read.csv("./Data_raw/RAP_BiomassData/RAP_PeakBiomass/RAP_VegBiomass_gridMetScale.csv")
# RAPbiomass <- RAPbiomass_temp %>% 
#   rename("Year" = "system.index",
#          "HerbaceousBiomass" = "mean",
#          "geometry" = ".geo") %>% 
#   mutate(Year = as.integer(str_split_i(Year, "_", i = 1))) #%>% 
#   #filter(Year %in% c(1992, 1999, 2014))
# 
# # fix geometry column
# RAPbiomass$Lat <- as.numeric(str_split_i(
#   str_split_i(
#     str_split_i(RAPbiomass$geometry, pattern = "\\[", i = 2), 
#     pattern = "\\]",
#     i = 1
#     ),
#   pattern = ",",
#   i = 2
# )
# )
# RAPbiomass$Lon <- as.numeric(str_split_i(
#   str_split_i(
#     str_split_i(RAPbiomass$geometry, pattern = "\\[", i = 2), 
#     pattern = "\\]",
#     i = 1
#   ),
#   pattern = ",",
#   i = 1
# )
# )
# 
# 
# ## have to switch to the appropriate crs 
# RAPbiomass_sf <- st_as_sf(RAPbiomass, coords = c( "Lon", "Lat"), crs = st_crs("EPSG:4326"))
# RAPbiomass_sf <- st_transform(RAPbiomass_sf, st_crs(RAPcover))

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
RAPall_raster <- RAPbiomass_sf %>% 
  ## convert biomass from lbs/acre to Mg/hectare
  mutate(HerbaceousBiomass_kgPerHect = (HerbaceousBiomass*1.120851155716688)/1000) %>% 
  terra::vect() %>%
  terra::project(test) %>% 
  terra::rasterize(field = "HerbaceousBiomass_kgPerHect", 
                   y = test, fun = mean, na.rm = TRUE, by = "Year")
names(RAPall_raster) <- paste0("year_", names(RAPall_raster))


# save RAP data
terra::writeRaster(RAPall_raster, filename = "./Data_processed/BiomassQuantityData/RAP_biomassRaster.tif", overwrite = TRUE)

