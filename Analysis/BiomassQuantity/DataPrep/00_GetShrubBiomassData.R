#///////////////////
# Preparing for and Acquiring shrub bioass data
# Alice Stears
# 10/31/24
#///////////////////


# load packages -----------------------------------------------------------

library(tidyverse)
library(sf)
library(terra)
library(tidyterra)

# load cover data ---------------------------------------------------------

coverDat <- readRDS("./Data_processed/CoverData/DataForModels.rds") %>% 
  st_transform(crs = "EPSG:4326")


# find plots w/ high amount of shrub cover --------------------------------
shrubCover_temp <- coverDat %>% 
  st_drop_geometry %>% 
  mutate(totalCoverPerc  = rowSums(.[c("ShrbCvr", "TtlTrCv", "TtlHrbC")],
                                  na.rm = TRUE)) %>% 
  mutate(ShrubPercent = ShrbCvr/totalCoverPerc*100) %>% 
  st_set_geometry(st_geometry(coverDat))

ggplot(shrubCover_temp[1:10000,]) + 
  geom_sf(aes(fill = ShrbCvr)) + 
  geom_point(aes(x = Lon, y = Lat, color = ShrbCvr))

shrubCov <- shrubCover_temp[shrubCover_temp$ShrubPercent>90 & !is.na(shrubCover_temp$ShrubPercent),]
#11,640 data points
table(shrubCov$Source)

ggplot(shrubCov) + 
  geom_point(aes(x = Lon, y=Lat, col = ShrbCvr)) + 
  facet_wrap(~Year) + 
  ggtitle("Locations with >90% absolute shrub cover by year", 
          subtitle = "56916 plots total -- FIA: 1577; LANDFIRE: 49057; AIM: 4225; RAP: 2057")
  


# save shrub points -------------------------------------------------------
shrubSave <- shrubCov %>% 
  select(Year, Lat, Lon, ShrbCvr, geometry)

st_write(shrubSave, dsn = "./Data_processed/BiomassQuantityData/pointsWithManyShrubs/", 
         layer = "ShrbCvr", driver = "ESRI shapefile", append = TRUE)


# based on total aboveground biomass from Spawn et al., 2020 --------------
# https://daac.ornl.gov/cgi-bin/dsviewer.pl?ds_id=1763
# load raw data
spawnBiomass <- terra::rast("./Data_raw/BiomassDataSources/aboveground_biomass_carbon_2010.tif") 
# reproject the points to match the crs of the biomass data 
shrubSave_spawn <- shrubSave %>% 
  sf::st_transform(to = st_crs(spawnBiomass))
plot(spawnBiomass)

# extract shrub points
spawnBiomass_shrubs <- spawnBiomass %>% 
  terra::extract(shrubSave_spawn, xy = TRUE) 

# get raster grid for plotting
test_rast_spawn <-  rast("./Data_raw/dayMet/rawMonthlyData/orders/70e0da02b9d2d6e8faa8c97d211f3546/Daymet_Monthly_V4R1/data/daymet_v4_prcp_monttl_na_1980.tif") %>% 
  terra::aggregate(fact = 8, fun = "mean") %>% 
  terra::project(crs(spawnBiomass))

spawnBiomass_shrubs_plot <- spawnBiomass_shrubs %>% 
  terra::vect(geom = c("x", "y")) %>% 
  rasterize(y = test_rast_spawn, 
            field = "aboveground_biomass_carbon_2010", 
            fun = mean, na.rm = TRUE) %>% 
  terra::crop(ext(-130, -65, 25, 50))

(spawnShrubPlot <- ggplot() + 
  geom_spatraster(data = spawnBiomass_shrubs_plot, aes(fill = mean), na.rm = TRUE) +
 # geom_point(aes(x = x, y = y, col = aboveground_biomass_carbon_2010)) + 
    scale_fill_grass_c(guide = guide_colorbar(title = "Mg Carbon/ha"))+ 
  ggtitle("Aboveground biomass carbon  (Mg/ha) in 2010 in locations were >90% of cover is shrubs", 
          subtitle = "from Spawn et al., 2020") + 
  theme_minimal()
)
# based on total aboveground biomass from GEDI ----------------------------
# https://daac.ornl.gov/cgi-bin/dsviewer.pl?ds_id=2299
# load raw data
GEDIBiomass <- terra::rast("./Data_raw/BiomassDataSources/GEDI04_B_MW019MW223_02_002_02_R01000M_MU.tif") 
#plot(GEDIBiomass)

# reproject the points to match the crs of the biomass data 
shrubSave_gedi <- shrubSave %>% 
  sf::st_transform(to = st_crs(GEDIBiomass))

# extract shrub points
GEDIBiomass_shrubs <- GEDIBiomass %>% 
  terra::extract(shrubSave_gedi, xy = TRUE) 

# get raster grid for plotting
test_rastGEDI <-  rast("./Data_raw/dayMet/rawMonthlyData/orders/70e0da02b9d2d6e8faa8c97d211f3546/Daymet_Monthly_V4R1/data/daymet_v4_prcp_monttl_na_1980.tif") %>% 
  terra::aggregate(fact = 8, fun = "mean") %>% 
  terra::project(crs(GEDIBiomass))
  
GEDIBiomass_shrubs_plot <- GEDIBiomass_shrubs %>% 
  terra::vect(geom = c("x", "y")) %>% 
  rasterize(y = test_rastGEDI, 
            field = "GEDI04_B_MW019MW223_02_002_02_R01000M_MU", 
            fun = mean, na.rm = TRUE)  %>%
  terra::project(crs(shrubSave)) %>% 
  terra::crop(ext(-130, -65, 25, 50))
 
(GEDIShrubPlot <- ggplot() + 
  geom_spatraster(data = GEDIBiomass_shrubs_plot, aes(fill = mean), na.rm = TRUE) +
    scale_fill_grass_c(guide = guide_colorbar(title = "Mg biomass/ha"))+ 
  ggtitle("Aboveground biomass (Mg/ha)  in locations with >90% of cover is shrubs", 
          subtitle = "from GEDI dataset") + 
  theme_minimal())


