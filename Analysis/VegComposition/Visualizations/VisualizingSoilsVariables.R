#///////////////////
# Visualizing raw cover data from sources
# Alice Stears
# 11/11/2024
#///////////////////


# load packages -----------------------------------------------------------

library(tidyverse)
library(sf)
library(terra)
library(ggpubr)
library(tidyterra)

# load data ---------------------------------------------------------------
# load shapefile version of "big composition dataset"
vegSoils_final <- readRDS("./Data_processed/CoverData/DataForModels_spatiallyAveraged_withSoils_noSf.rds")
# filter for unique soils data (same across years)
soilsFigDat <- vegSoils_final %>% 
  select(Long, Lat, soilDepth:totalAvailableWaterHoldingCapacity) %>% 
  unique() %>% 
# make into an sf data.frame
  st_as_sf(coords = c("Long", "Lat"), crs = crs("PROJCRS[\"unnamed\",\n    BASEGEOGCRS[\"unknown\",\n        DATUM[\"unknown\",\n            ELLIPSOID[\"Spheroid\",6378137,298.257223563,\n                LENGTHUNIT[\"metre\",1,\n                    ID[\"EPSG\",9001]]]],\n        PRIMEM[\"Greenwich\",0,\n            ANGLEUNIT[\"degree\",0.0174532925199433,\n                ID[\"EPSG\",9122]]]],\n    CONVERSION[\"Lambert Conic Conformal (2SP)\",\n        METHOD[\"Lambert Conic Conformal (2SP)\",\n            ID[\"EPSG\",9802]],\n        PARAMETER[\"Latitude of false origin\",42.5,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8821]],\n        PARAMETER[\"Longitude of false origin\",-100,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8822]],\n        PARAMETER[\"Latitude of 1st standard parallel\",25,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8823]],\n        PARAMETER[\"Latitude of 2nd standard parallel\",60,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8824]],\n        PARAMETER[\"Easting at false origin\",0,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8826]],\n        PARAMETER[\"Northing at false origin\",0,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8827]]],\n    CS[Cartesian,2],\n        AXIS[\"easting\",east,\n            ORDER[1],\n            LENGTHUNIT[\"metre\",1,\n                ID[\"EPSG\",9001]]],\n        AXIS[\"northing\",north,\n            ORDER[2],\n            LENGTHUNIT[\"metre\",1,\n                ID[\"EPSG\",9001]]]]"))

# load raster to rasterize cover data
# rasterize
test_rast <-  rast("./Data_raw/dayMet/rawMonthlyData/orders/70e0da02b9d2d6e8faa8c97d211f3546/Daymet_Monthly_V4R1/data/daymet_v4_prcp_monttl_na_1980.tif") %>% 
  terra::aggregate(fact = 16, fun = "mean") %>% 
  terra::project(crs("PROJCRS[\"unnamed\",\n    BASEGEOGCRS[\"unknown\",\n        DATUM[\"unknown\",\n            ELLIPSOID[\"Spheroid\",6378137,298.257223563,\n                LENGTHUNIT[\"metre\",1,\n                    ID[\"EPSG\",9001]]]],\n        PRIMEM[\"Greenwich\",0,\n            ANGLEUNIT[\"degree\",0.0174532925199433,\n                ID[\"EPSG\",9122]]]],\n    CONVERSION[\"Lambert Conic Conformal (2SP)\",\n        METHOD[\"Lambert Conic Conformal (2SP)\",\n            ID[\"EPSG\",9802]],\n        PARAMETER[\"Latitude of false origin\",42.5,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8821]],\n        PARAMETER[\"Longitude of false origin\",-100,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8822]],\n        PARAMETER[\"Latitude of 1st standard parallel\",25,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8823]],\n        PARAMETER[\"Latitude of 2nd standard parallel\",60,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8824]],\n        PARAMETER[\"Easting at false origin\",0,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8826]],\n        PARAMETER[\"Northing at false origin\",0,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8827]]],\n    CS[Cartesian,2],\n        AXIS[\"easting\",east,\n            ORDER[1],\n            LENGTHUNIT[\"metre\",1,\n                ID[\"EPSG\",9001]]],\n        AXIS[\"northing\",north,\n            ORDER[2],\n            LENGTHUNIT[\"metre\",1,\n                ID[\"EPSG\",9001]]]]"))


crs(test_rast) == crs(soilsFigDat)
# For LANDFIRE data -------------------------------------------------------
# soil depth data 
# rasterize data
soilDepth_rast <- soilsFigDat %>% 
  #slice_sample(n = 5e4) %>%
  terra::vect() %>% 
  #terra::set.crs(crs(test_rast)) %>% 
  terra::rasterize(y = test_rast, 
                   field = "soilDepth", 
                   fun = mean, na.rm = TRUE) %>% 
  terra::crop(ext(-2000000, 2500000, -2000000, 1200000
                  ))
(soilDepth_rast_plot <- ggplot() + 
    tidyterra::geom_spatraster(data = soilDepth_rast, aes(fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    scale_fill_viridis_c(option = "A", guide = guide_colorbar(title = "cm"), 
                         limits = c(0,201)) +
    ggtitle("Soil depth"))

## surface clay percentage
surfaceClay_rast <- soilsFigDat %>% 
  #slice_sample(n = 5e4) %>%
  terra::vect() %>% 
  #terra::set.crs(crs(test_rast)) %>% 
  terra::rasterize(y = test_rast, 
                   field = "surfaceClay_perc", 
                   fun = mean, na.rm = TRUE) %>% 
  terra::crop(ext(-2000000, 2500000, -2000000, 1200000
  ))
(surfaceClay_rast_plot <- ggplot() + 
    tidyterra::geom_spatraster(data = surfaceClay_rast, aes(fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    scale_fill_viridis_c(option = "C", guide = guide_colorbar(title = "%"), 
                         limits = c(0,100)) +
    ggtitle("Surface Clay %"))

## average sand percentage across depth
sandPerc_rast <- soilsFigDat %>% 
  #slice_sample(n = 5e4) %>%
  terra::vect() %>% 
  #terra::set.crs(crs(test_rast)) %>% 
  terra::rasterize(y = test_rast, 
                   field = "avgSandPerc_acrossDepth", 
                   fun = mean, na.rm = TRUE) %>% 
  terra::crop(ext(-2000000, 2500000, -2000000, 1200000
  ))
(sandPerc_rast_plot <- ggplot() + 
    tidyterra::geom_spatraster(data = sandPerc_rast, aes(fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    scale_fill_viridis_c(option = "C", guide = guide_colorbar(title = "%"), 
                         limits = c(0,100)) +
    ggtitle("Sand % - weighted avg. across depth"))

## average coarse percentage across depth
coarsePerc_rast <- soilsFigDat %>% 
  #slice_sample(n = 5e4) %>%
  terra::vect() %>% 
  #terra::set.crs(crs(test_rast)) %>% 
  terra::rasterize(y = test_rast, 
                   field = "avgCoarsePerc_acrossDepth", 
                   fun = mean, na.rm = TRUE) %>% 
  terra::crop(ext(-2000000, 2500000, -2000000, 1200000
  ))
(coarsePerc_rast_plot <- ggplot() + 
    tidyterra::geom_spatraster(data = coarsePerc_rast , aes(fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    scale_fill_viridis_c(option = "C", guide = guide_colorbar(title = "%"), 
                         limits = c(0,100)) +
    ggtitle("Coarse % - weighted avg. across depth"))

## average organic carbon percentage across depth
orgPerc_rast <- soilsFigDat %>% 
  #slice_sample(n = 5e4) %>%
  terra::vect() %>% 
  #terra::set.crs(crs(test_rast)) %>% 
  terra::rasterize(y = test_rast, 
                   field = "avgOrganicCarbonPerc_0_3cm", 
                   fun = mean, na.rm = TRUE) %>% 
  terra::crop(ext(-2000000, 2500000, -2000000, 1200000
  ))
(orgPerc_rast_plot <- ggplot() + 
    tidyterra::geom_spatraster(data = orgPerc_rast , aes(fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    scale_fill_viridis_c(option = "D", guide = guide_colorbar(title = "%")#, 
                         #limits = c(0,100)
                         ) +
    ggtitle("Organic Carbon % - first 3 cm"))

## total available water holding capacity
awc_rast <- soilsFigDat %>% 
  #slice_sample(n = 5e4) %>%
  terra::vect() %>% 
  #terra::set.crs(crs(test_rast)) %>% 
  terra::rasterize(y = test_rast, 
                   field = "totalAvailableWaterHoldingCapacity", 
                   fun = mean, na.rm = TRUE) %>% 
  terra::crop(ext(-2000000, 2500000, -2000000, 1200000
  ))
(awc_rast_plot <- ggplot() + 
    tidyterra::geom_spatraster(data = awc_rast , aes(fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    scale_fill_viridis_c(option = "A", guide = guide_colorbar(title = "awc")#, 
                         #limits = c(0,100)
                         ) +
    ggtitle("Total Available Water Holding Capacity"))

# Put all plots together  -------------------------------------------------
#pdf(file = "./Figures/soilsFigDatFigures/TotalTreeFigures.pdf", width = 11, height = 3.75, compress = FALSE)
# tree figures
  ggarrange(soilDepth_rast_plot, surfaceClay_rast_plot, sandPerc_rast_plot, 
            coarsePerc_rast_plot, orgPerc_rast_plot, awc_rast_plot, nrow = 3, ncol = 2) %>% 
  ggexport(
    filename = "./Figures/soilsVariableFigures.pdf", 
    width = 11, height = 12)

d