#///////////////////
# Visualizing raw cover data from sources
# Alice Stears
# 11/24/2024
#///////////////////


# load packages -----------------------------------------------------------

library(tidyverse)
library(sf)
library(terra)
library(ggpubr)
library(tidyterra)
library(ggpubr)

# load data ---------------------------------------------------------------
# load shapefile version of "RAP biomass data
rapBiomass <- readRDS("./Data_processed/BiomassQuantityData/HerbBiomassCover_withWeatherAndFireFiltering_rast.rds")

# load raster to rasterize cover data
# rasterize
test_rast <-  rast("./Data_raw/dayMet/rawMonthlyData/orders/70e0da02b9d2d6e8faa8c97d211f3546/Daymet_Monthly_V4R1/data/daymet_v4_prcp_monttl_na_1980.tif") %>% 
  terra::aggregate(fact = 32, fun = "mean") %>% 
  terra::project(crs("PROJCRS[\"unnamed\",\n    BASEGEOGCRS[\"unknown\",\n        DATUM[\"unknown\",\n            ELLIPSOID[\"Spheroid\",6378137,298.257223563,\n                LENGTHUNIT[\"metre\",1,\n                    ID[\"EPSG\",9001]]]],\n        PRIMEM[\"Greenwich\",0,\n            ANGLEUNIT[\"degree\",0.0174532925199433,\n                ID[\"EPSG\",9122]]]],\n    CONVERSION[\"Lambert Conic Conformal (2SP)\",\n        METHOD[\"Lambert Conic Conformal (2SP)\",\n            ID[\"EPSG\",9802]],\n        PARAMETER[\"Latitude of false origin\",42.5,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8821]],\n        PARAMETER[\"Longitude of false origin\",-100,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8822]],\n        PARAMETER[\"Latitude of 1st standard parallel\",25,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8823]],\n        PARAMETER[\"Latitude of 2nd standard parallel\",60,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8824]],\n        PARAMETER[\"Easting at false origin\",0,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8826]],\n        PARAMETER[\"Northing at false origin\",0,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8827]]],\n    CS[Cartesian,2],\n        AXIS[\"easting\",east,\n            ORDER[1],\n            LENGTHUNIT[\"metre\",1,\n                ID[\"EPSG\",9001]]],\n        AXIS[\"northing\",north,\n            ORDER[2],\n            LENGTHUNIT[\"metre\",1,\n                ID[\"EPSG\",9001]]]]"))


crs(test_rast) == crs(rapBiomass)
# Make plots ------------------------------------------------------
# RAP total herbaceous biomass
# resample to make coarser to ease visualization
rapHerbBiomass_plotRast <- rapBiomass %>%
  terra::aggregate(fact = 16, fun = "mean", na.rm = TRUE) %>% 
  terra::crop(ext(-2000000, 2500000, -2000000, 1200000
  ))
                   
(rapHerbBiomass_plot <- ggplot() + 
    tidyterra::geom_spatraster(data = rapHerbBiomass_plotRast, aes(), na.rm = TRUE) +
    theme_minimal() + 
   facet_wrap(~lyr) + 
   scale_fill_viridis_c(option = "H", guide = guide_colorbar(title = "kg/Hect")#, 
                      #   limits = c(0,4000)
                      ) +
    ggtitle("RAP - Total Herbaceous Biomass"))


# Put all plots together  -------------------------------------------------
#pdf(file = "./Figures/soilsFigDatFigures/TotalTreeFigures.pdf", width = 11, height = 3.75, compress = FALSE)
# tree figures
ggarrange(prcp_totalAnn30yr_plotAll, swe_30yr_plotAll, tmin_30yr_plotAll, 
          tmax_30yr_plotAll, tmean_30yr_plotAll, vp_30yr_plotAll, 
          T_warmestMonth_meanAnnAvg_30yr_plotAll, T_coldestMonth_meanAnnAvg_30yr_plotAll,
          precipSeasonality_rast_plotAll, precipTempCorr_rast_plotAll, 
          aboveFreezeMonth_rast_plotAll, isothermality_rast_plotAll, 
          annWatDef_rast_plotAll, annWetDegDays_rast_plotAll, 
          annVPD_rast_plotAll, frostFreeDays_rast_plotAll,
          ncol = 2, 
          nrow = 8) %>% 
  ggexport(
    filename = "./Figures/climateVariableFigures.pdf", 
    width = 9, height = 25)

