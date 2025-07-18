#///////////////////
# Compiling the complete biomass dataset for use in subsequent modeling
# Alice Stears
# 07/14/2025
#///////////////////

# load packages -----------------------------------------------------------

library(tidyverse)

# load data ---------------------------------------------------------------
# Get total biomass data from Spawn paper 
spawnBiomass <- terra::rast("./Data_raw/BiomassDataSources/aboveground_biomass_carbon_2010.tif") 

# reproject the points to match the crs of the biomass data 

# get raster grid for plotting
test_rast_spawn <-  rast("./Data_raw/dayMet/rawMonthlyData/orders/70e0da02b9d2d6e8faa8c97d211f3546/Daymet_Monthly_V4R1/data/daymet_v4_prcp_monttl_na_1980.tif") %>% 
  #terra::aggregate(fact = 8, fun = "mean") %>% 
  terra::project(crs(spawnBiomass))

spawnBiomass_plot <- spawnBiomass %>% 
  #terra::vect(geom = c("x", "y")) %>% 
  terra::resample(y = test_rast_spawn, 
                  #field = "aboveground_biomass_carbon_2010", 
                  method = "average",
                  #fun = mean,
                  #na.rm = TRUE
  )


# Read in RAP biomass data 
# units: kg/hectare
RAPbiomass_temp <- read.csv("./Data_raw/RAP_BiomassData/RAP_PeakBiomass/RAP_VegBiomass_gridMetScale.csv")
RAPbiomass <- RAPbiomass_temp %>% 
  rename("Year" = "system.index",
         "HerbaceousBiomass" = "mean",
         "geometry" = ".geo") %>% 
  mutate(Year = as.integer(str_split_i(Year, "_", i = 1))) 

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
RAPbiomass_sf <- st_as_sf(RAPbiomass, coords = c( "Lon", "Lat"), crs = st_crs("EPSG:4326"))# rasterize
# change the RAP biomass dataset crs to the 'Spawn' dataset crs
RAPbiomass_sf <- RAPbiomass_sf %>% 
  st_transform(crs = st_crs(spawnBiomass_plot)) %>% # convert biomass from lbs/acre to kg/hectare
  mutate(HerbaceousBiomass_kgPerHect = HerbaceousBiomass*1.120851155716688)

crs(spawnBiomass_plot) == crs(RAPbiomass_plot)

## convert kg/hectare to Mg/hectare
RAPbiomass_plot <- RAPbiomass_plot/1000


(RAPbiomass <- ggplot() + 
    geom_spatraster(data = RAPbiomass_plot  %>% 
                      terra::aggregate(fact = 2, fun = "mean", na.rm = TRUE)) + 
    ggtitle("Biomass from RAP", subtitle = "Herbaceous above-ground biomass in 2010, Mg /ha"))

# Read in FIA biomass data ------------------------------------------------
FIA_biomassCoverDat <- readRDS("./Data_processed/BiomassQuantityData/TreeBiomassCover_withWeatherAndFireFiltering.rds")

# Visualize the spread of the cover/biomass data --------------------------
# get veg data
FIA_biomassCoverDat <- st_sf(FIA_biomassCoverDat, crs = "GEOGCRS[\"NAD83\",\n    DATUM[\"North American Datum 1983\",\n        ELLIPSOID[\"GRS 1980\",6378137,298.257222101,\n            LENGTHUNIT[\"metre\",1]]],\n    PRIMEM[\"Greenwich\",0,\n        ANGLEUNIT[\"degree\",0.0174532925199433]],\n    CS[ellipsoidal,2],\n        AXIS[\"geodetic latitude (Lat)\",north,\n            ORDER[1],\n            ANGLEUNIT[\"degree\",0.0174532925199433]],\n        AXIS[\"geodetic longitude (Lon)\",east,\n            ORDER[2],\n            ANGLEUNIT[\"degree\",0.0174532925199433]],\n    ID[\"EPSG\",4269]]")
# dayMet extent 
test <-  rast("./Data_raw/dayMet/rawMonthlyData/orders/70e0da02b9d2d6e8faa8c97d211f3546/Daymet_Monthly_V4R1/data/daymet_v4_prcp_monttl_na_1980.tif") %>% 
  terra::project(crs(biomassCoverDat))
# add metData to the veg dat 
# make veg data projection the same as raster data
v <- vect(st_drop_geometry(FIA_biomassCoverDat)[,c("Lon", "Lat")], geom = c("Lon", "Lat"), crs=crs(FIA_biomassCoverDat))
y <- project(v, crs(test))
# make sure the veg data is in the appropriate projection
FIA_biomassFoliage_plot <- FIA_biomassCoverDat %>%
  #st_buffer(.01) %>%
  #terra::vect() #%>%
  sf::st_transform(crs(y)) %>% 
  sf::st_transform(crs = st_crs(spawnBiomass_plot)) %>% 
  # st_buffer(.1)
  st_centroid() %>% 
  mutate("Year_char" = as.character(INVYR)) %>% 
  select(INVYR, Year_char, UniquID, Month, Day, Lat:DryBio_branch_trees) %>% 
  filter(Year_char == 2012) %>% 
  #terra::vect(geom = c("x", "y")) %>% 
  terra::rasterize(y = test_rast_spawn, 
                   field = "DryBio_foliage_trees", 
                   #method = "average",
                   fun = mean,
                   na.rm = TRUE
  ) %>% 
  aggregate(fact = 8, fun = "mean", na.rm = TRUE) %>% 
  terra::crop(ext(-130, -65, 25, 50))

## convert from lbs/acre to Mg/hectare
FIA_biomassFoliage_plot <- FIA_biomassFoliage_plot * 1.12 /1000
## plot
(FIAbiomass_foliage <- 
    ggplot() + 
    geom_spatraster(data = FIA_biomassFoliage_plot %>% 
                      terra::aggregate(fact = 2, fun = "mean", na.rm = TRUE)) + 
    ggtitle("Biomass from FIA", subtitle = "Tree foliage biomass in 2012, Mg /ha"))

## tree stem and branch biomass
FIA_biomassStem_plot <- FIA_biomassCoverDat %>%
  #st_buffer(.01) %>%
  #terra::vect() #%>%
  sf::st_transform(crs(y)) %>% 
  sf::st_transform(crs = st_crs(spawnBiomass_plot)) %>% 
  # st_buffer(.1)
  st_centroid() %>% 
  mutate("Year_char" = as.character(INVYR),
         "DryBio_stemAndBranch_trees" = DryBio_stem_trees + DryBio_branch_trees) %>% 
  select(INVYR, Year_char, UniquID, Month, Day, Lat:DryBio_stemAndBranch_trees) %>% 
  filter(Year_char == 2012) %>% 
  #terra::vect(geom = c("x", "y")) %>% 
  terra::rasterize(y = test_rast_spawn, 
                   field = "DryBio_stemAndBranch_trees", 
                   #method = "average",
                   fun = mean,
                   na.rm = TRUE
  ) %>% 
  aggregate(fact = 8, fun = "mean", na.rm = TRUE) %>% 
  terra::crop(ext(-130, -65, 25, 50))

## convert from lbs/acre to Mg/hectare
FIA_biomassStem_plot <- FIA_biomassStem_plot * 1.12 /1000
(FIAbiomass_stemAndBranch <- 
    ggplot() + 
    geom_spatraster(data = FIA_biomassStem_plot %>% 
                      terra::aggregate(fact = 2, fun = "mean", na.rm = TRUE)) + 
    ggtitle("Biomass from FIA", subtitle = "Tree stem and branch biomass in 2012, Mg /ha"))
