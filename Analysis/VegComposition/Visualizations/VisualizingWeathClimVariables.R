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
# load shapefile version of "big composition dataset"
clim_final <- readRDS("./Data_processed/CoverData/DataForModels_finalFINAL.rds")
# filter for unique soils data (same across years)
climFigDat <- clim_final %>% 
  select(Year, Long, Lat, prcp_annTotal:durationFrostFreeDays_meanAnnAvg_1yr, swe_meanAnnAvg_CLIM:Start_CLIM) %>% 
  unique() %>% 
  # make into an sf data.frame
  st_as_sf(coords = c("Long", "Lat"), crs = crs("PROJCRS[\"unnamed\",\n    BASEGEOGCRS[\"unknown\",\n        DATUM[\"unknown\",\n            ELLIPSOID[\"Spheroid\",6378137,298.257223563,\n                LENGTHUNIT[\"metre\",1,\n                    ID[\"EPSG\",9001]]]],\n        PRIMEM[\"Greenwich\",0,\n            ANGLEUNIT[\"degree\",0.0174532925199433,\n                ID[\"EPSG\",9122]]]],\n    CONVERSION[\"Lambert Conic Conformal (2SP)\",\n        METHOD[\"Lambert Conic Conformal (2SP)\",\n            ID[\"EPSG\",9802]],\n        PARAMETER[\"Latitude of false origin\",42.5,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8821]],\n        PARAMETER[\"Longitude of false origin\",-100,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8822]],\n        PARAMETER[\"Latitude of 1st standard parallel\",25,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8823]],\n        PARAMETER[\"Latitude of 2nd standard parallel\",60,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8824]],\n        PARAMETER[\"Easting at false origin\",0,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8826]],\n        PARAMETER[\"Northing at false origin\",0,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8827]]],\n    CS[Cartesian,2],\n        AXIS[\"easting\",east,\n            ORDER[1],\n            LENGTHUNIT[\"metre\",1,\n                ID[\"EPSG\",9001]]],\n        AXIS[\"northing\",north,\n            ORDER[2],\n            LENGTHUNIT[\"metre\",1,\n                ID[\"EPSG\",9001]]]]"))

# load raster to rasterize cover data
# rasterize
test_rast <-  rast("./Data_raw/dayMet/rawMonthlyData/orders/70e0da02b9d2d6e8faa8c97d211f3546/Daymet_Monthly_V4R1/data/daymet_v4_prcp_monttl_na_1980.tif") %>% 
  terra::aggregate(fact = 32, fun = "mean") %>% 
  terra::project(crs("PROJCRS[\"unnamed\",\n    BASEGEOGCRS[\"unknown\",\n        DATUM[\"unknown\",\n            ELLIPSOID[\"Spheroid\",6378137,298.257223563,\n                LENGTHUNIT[\"metre\",1,\n                    ID[\"EPSG\",9001]]]],\n        PRIMEM[\"Greenwich\",0,\n            ANGLEUNIT[\"degree\",0.0174532925199433,\n                ID[\"EPSG\",9122]]]],\n    CONVERSION[\"Lambert Conic Conformal (2SP)\",\n        METHOD[\"Lambert Conic Conformal (2SP)\",\n            ID[\"EPSG\",9802]],\n        PARAMETER[\"Latitude of false origin\",42.5,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8821]],\n        PARAMETER[\"Longitude of false origin\",-100,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8822]],\n        PARAMETER[\"Latitude of 1st standard parallel\",25,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8823]],\n        PARAMETER[\"Latitude of 2nd standard parallel\",60,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8824]],\n        PARAMETER[\"Easting at false origin\",0,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8826]],\n        PARAMETER[\"Northing at false origin\",0,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8827]]],\n    CS[Cartesian,2],\n        AXIS[\"easting\",east,\n            ORDER[1],\n            LENGTHUNIT[\"metre\",1,\n                ID[\"EPSG\",9001]]],\n        AXIS[\"northing\",north,\n            ORDER[2],\n            LENGTHUNIT[\"metre\",1,\n                ID[\"EPSG\",9001]]]]"))


crs(test_rast) == crs(climFigDat)

# turn the climFigDat data into a terra vector
climFigDat <- climFigDat %>% 
  terra::vect()

# Make plots ------------------------------------------------------
# total annual precipitation data  - over previous 30 years
# rasterize data
  prcpTot_rast <- climFigDat %>% 
    filter(Year == 2014) %>% 
    #slice_sample(n = 5e4) %>%
    
    #terra::vect() %>% 
    #terra::set.crs(crs(test_rast)) %>% 
    terra::rasterize(y = test_rast, 
                     field = "prcp_meanAnnTotal_CLIM", 
                     
                     fun = mean, na.rm = TRUE) %>% 
    terra::crop(ext(-2000000, 2500000, -2000000, 1200000
    ))
  (prcp_totalAnn30yr_plotAll <- ggplot() + 
      tidyterra::geom_spatraster(data = prcpTot_rast, aes(fill = mean), na.rm = TRUE) +
      theme_minimal() + 
      scale_fill_viridis_c(option = "D", guide = guide_colorbar(title = "mm"), 
      limits = c(0,4000)) +
      ggtitle("Annual Total Precip \nmean over previous 30 years - 2014"))


# swe \n mean over previous 30 years
# rasterize data
swe_rast <- climFigDat %>% 
  filter(Year == 2014) %>% 
  #slice_sample(n = 5e4) %>%
  
  #terra::vect() %>% 
  #terra::set.crs(crs(test_rast)) %>% 
  terra::rasterize(y = test_rast, 
                   field = "swe_meanAnnAvg_CLIM", 
                   
                   fun = mean, na.rm = TRUE) %>% 
  terra::crop(ext(-2000000, 2500000, -2000000, 1200000
  ))
(swe_CLIM_plotAll <- ggplot() + 
    tidyterra::geom_spatraster(data = swe_rast, aes(fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    scale_fill_viridis_c(#option = "D", 
      guide = guide_colorbar(title = "mm")#, 
                        #limits = c(0,4000)
      ) +
    ggtitle("SWE  \n mean over previous 30 years - 2014"))

# tmin \n mean over previous 30 years
# rasterize data
tmin_rast <- climFigDat %>% 
  filter(Year == 2014) %>% 
  #slice_sample(n = 5e4) %>%
  
  #terra::vect() %>% 
  #terra::set.crs(crs(test_rast)) %>% 
  terra::rasterize(y = test_rast, 
                   field = "tmin_meanAnnAvg_CLIM", 
                   
                   fun = mean, na.rm = TRUE) %>% 
  terra::crop(ext(-2000000, 2500000, -2000000, 1200000
  ))
(tmin_CLIM_plotAll <- ggplot() + 
    tidyterra::geom_spatraster(data = tmin_rast, aes(fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    scale_fill_viridis_c(#option = "D", 
      guide = guide_colorbar(title = "Deg. C")#, 
      #limits = c(0,4000)
    ) +
    ggtitle("Annual T. min \n mean over previous 30 years - 2014"))

# tmax\n mean over previous 30 years
# rasterize data
tmax_rast <- climFigDat %>% 
  filter(Year == 2014) %>% 
  #slice_sample(n = 5e4) %>%
  
  #terra::vect() %>% 
  #terra::set.crs(crs(test_rast)) %>% 
  terra::rasterize(y = test_rast, 
                   field = "tmax_meanAnnAvg_CLIM", 
                   
                   fun = mean, na.rm = TRUE) %>% 
  terra::crop(ext(-2000000, 2500000, -2000000, 1200000
  ))
(tmax_CLIM_plotAll <- ggplot() + 
    tidyterra::geom_spatraster(data = tmax_rast, aes(fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    scale_fill_viridis_c(#option = "D", 
      guide = guide_colorbar(title = "Deg. C")#, 
      #limits = c(0,4000)
    ) +
    ggtitle("Annual T. max \n mean over previous 30 years - 2014"))

# tmean\n mean over previous 30 years
# rasterize data
tmean_rast <- climFigDat %>% 
  filter(Year == 2014) %>% 
  #slice_sample(n = 5e4) %>%
  
  #terra::vect() %>% 
  #terra::set.crs(crs(test_rast)) %>% 
  terra::rasterize(y = test_rast, 
                   field = "tmean_meanAnnAvg_CLIM", 
                   
                   fun = mean, na.rm = TRUE) %>% 
  terra::crop(ext(-2000000, 2500000, -2000000, 1200000
  ))
(tmean_CLIM_plotAll <- ggplot() + 
    tidyterra::geom_spatraster(data = tmean_rast, aes(fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    scale_fill_viridis_c(#option = "D", 
      guide = guide_colorbar(title = "Deg. C")#, 
      #limits = c(0,4000)
    ) +
    ggtitle("Annual T. mean \n mean over previous 30 years - 2014"))

# vp\n mean over previous 30 years
# rasterize data
vp_rast <- climFigDat %>% 
  filter(Year == 2014) %>% 
  #slice_sample(n = 5e4) %>%
  
  #terra::vect() %>% 
  #terra::set.crs(crs(test_rast)) %>% 
  terra::rasterize(y = test_rast, 
                   field = "vp_meanAnnAvg_CLIM", 
                   
                   fun = mean, na.rm = TRUE) %>% 
  terra::crop(ext(-2000000, 2500000, -2000000, 1200000
  ))
(vp_CLIM_plotAll <- ggplot() + 
    tidyterra::geom_spatraster(data = vp_rast, aes(fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    scale_fill_viridis_c(#option = "D", 
      guide = guide_colorbar(title = "Pascals")#, 
      #limits = c(0,4000)
    ) +
    ggtitle("Annual Mean daily water vapor pressure - \n mean over previous 30 years - 2014"))

# t warmest month\n mean over previous 30 years
# rasterize data
T_warmestMonth_rast <- climFigDat %>% 
  filter(Year == 2014) %>% 
  #slice_sample(n = 5e4) %>%
  
  #terra::vect() %>% 
  #terra::set.crs(crs(test_rast)) %>% 
  terra::rasterize(y = test_rast, 
                   field = "T_warmestMonth_meanAnnAvg_CLIM", 
                   
                   fun = mean, na.rm = TRUE) %>% 
  terra::crop(ext(-2000000, 2500000, -2000000, 1200000
  ))
(T_warmestMonth_meanAnnAvg_CLIM_plotAll <- ggplot() + 
    tidyterra::geom_spatraster(data = T_warmestMonth_rast, aes(fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    scale_fill_viridis_c(#option = "D", 
      guide = guide_colorbar(title = "Deg. C")#, 
      #limits = c(0,4000)
    ) +
    ggtitle("T. of warmest month \n mean over previous 30 years - 2014"))

# t coldest month\n mean over previous 30 years
# rasterize data
T_coldestMonth_rast <- climFigDat %>% 
  filter(Year == 2014) %>% 
  #slice_sample(n = 5e4) %>%
  
  #terra::vect() %>% 
  #terra::set.crs(crs(test_rast)) %>% 
  terra::rasterize(y = test_rast, 
                   field = "T_coldestMonth_meanAnnAvg_CLIM", 
                   
                   fun = mean, na.rm = TRUE) %>% 
  terra::crop(ext(-2000000, 2500000, -2000000, 1200000
  ))
(T_coldestMonth_meanAnnAvg_CLIM_plotAll <- ggplot() + 
    tidyterra::geom_spatraster(data = T_coldestMonth_rast, aes(fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    scale_fill_viridis_c(#option = "D", 
      guide = guide_colorbar(title = "Deg. C")#, 
      #limits = c(0,4000)
    ) +
    ggtitle("T. of coldest month \n mean over previous 30 years - 2014"))

# Precip seasonality \n mean over previous 30 years
# rasterize data
precipSeasonality_rast <- climFigDat %>% 
  filter(Year == 2014) %>% 
  #slice_sample(n = 5e4) %>%
  
  #terra::vect() %>% 
  #terra::set.crs(crs(test_rast)) %>% 
  terra::rasterize(y = test_rast, 
                   field = "precip_Seasonality_meanAnnAvg_CLIM", 
                   
                   fun = mean, na.rm = TRUE) %>% 
  terra::crop(ext(-2000000, 2500000, -2000000, 1200000
  ))
(precipSeasonality_rast_plotAll <- ggplot() + 
    tidyterra::geom_spatraster(data = precipSeasonality_rast, aes(fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    scale_fill_viridis_c(#option = "D", 
      guide = guide_colorbar(title = "Coef. of Var.")#, 
      #limits = c(0,4000)
    ) +
    ggtitle("Precip Seasonality \n mean over previous 30 years - 2014"))

# Precip seasonality \n mean over previous 30 years
# rasterize data
precipTempCorr_rast <- climFigDat %>% 
  filter(Year == 2014) %>% 
  #slice_sample(n = 5e4) %>%
  
  #terra::vect() %>% 
  #terra::set.crs(crs(test_rast)) %>% 
  terra::rasterize(y = test_rast, 
                   field = "PrecipTempCorr_meanAnnAvg_CLIM", 
                   
                   fun = mean, na.rm = TRUE) %>% 
  terra::crop(ext(-2000000, 2500000, -2000000, 1200000
  ))
(precipTempCorr_rast_plotAll <- ggplot() + 
    tidyterra::geom_spatraster(data = precipTempCorr_rast, aes(fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    scale_fill_viridis_c(#option = "D", 
      guide = guide_colorbar(title = "Correlation")#, 
      #limits = c(0,4000)
    ) +
    ggtitle("Precip/Temp Correlation \n mean over previous 30 years - 2014"))

# Above freezing Month \n mean over previous 30 years
# rasterize data
aboveFreezeMonth_rast <- climFigDat %>% 
  filter(Year == 2014) %>% 
  #slice_sample(n = 5e4) %>%
  
  #terra::vect() %>% 
  #terra::set.crs(crs(test_rast)) %>% 
  terra::rasterize(y = test_rast, 
                   field = "aboveFreezing_month_meanAnnAvg_CLIM", 
                   
                   fun = mean, na.rm = TRUE) %>% 
  terra::crop(ext(-2000000, 2500000, -2000000, 1200000
  )) %>% 
  na.omit()
(aboveFreezeMonth_rast_plotAll <- ggplot() + 
    tidyterra::geom_spatraster(data = aboveFreezeMonth_rast, aes(fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    scale_fill_viridis_c(option = "G", 
      guide = guide_colorbar(title = "Month")#, 
      #limits = c(0,4000)
    ) +
    ggtitle("First Month Above Freezing \n mean over previous 30 years - 2014"))

# isothermality \n mean over previous 30 years
# rasterize data
isothermality_rast <- climFigDat %>% 
  filter(Year == 2014) %>% 
  #slice_sample(n = 5e4) %>%
  
  #terra::vect() %>% 
  #terra::set.crs(crs(test_rast)) %>% 
  terra::rasterize(y = test_rast, 
                   field = "isothermality_meanAnnAvg_CLIM", 
                   
                   fun = mean, na.rm = TRUE) %>% 
  terra::crop(ext(-2000000, 2500000, -2000000, 1200000
  )) %>% 
  na.omit()
(isothermality_rast_plotAll <- ggplot() + 
    tidyterra::geom_spatraster(data = isothermality_rast, aes(fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    scale_fill_viridis_c(option = "A", 
                         guide = guide_colorbar(title = "Deg.C/Deg.C")#, 
                         #limits = c(0,4000)
    ) +
    ggtitle("Isothermality \n mean over previous 30 years - 2014"))

# Annual Water Deficit \n mean over previous 30 years
# rasterize data
annWatDef_rast <- climFigDat %>% 
  filter(Year == 2014) %>% 
  #slice_sample(n = 5e4) %>%
  
  #terra::vect() %>% 
  #terra::set.crs(crs(test_rast)) %>% 
  terra::rasterize(y = test_rast, 
                   field = "annWaterDeficit_95percentile_CLIM", 
                   
                   fun = mean, na.rm = TRUE) %>% 
  terra::crop(ext(-2000000, 2500000, -2000000, 1200000
  )) %>% 
  na.omit()
(annWatDef_rast_plotAll <- ggplot() + 
    tidyterra::geom_spatraster(data = annWatDef_rast, aes(fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    scale_fill_viridis_c(option = "A", 
                         guide = guide_colorbar(title = "mm/Deg.C")#, 
                         #limits = c(0,4000)
    ) +
    ggtitle("Ann. Water Deficit \n 95th Percentile over previous 30 years - 2014"))

# Annual wet degree days \n mean over previous 30 years
# rasterize data
annWetDegDays_rast <- climFigDat %>% 
  filter(Year == 2014) %>% 
  #slice_sample(n = 5e4) %>%
  
  #terra::vect() %>% 
  #terra::set.crs(crs(test_rast)) %>% 
  terra::rasterize(y = test_rast, 
                   field = "annWetDegDays_5percentile_CLIM", 
                   
                   fun = mean, na.rm = TRUE) %>% 
  terra::crop(ext(-2000000, 2500000, -2000000, 1200000
  )) %>% 
  na.omit()
(annWetDegDays_rast_plotAll <- ggplot() + 
    tidyterra::geom_spatraster(data = annWetDegDays_rast, aes(fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    scale_fill_viridis_c(option = "A", 
                         guide = guide_colorbar(title = "Degree Days")#, 
                         #limits = c(0,4000)
    ) +
    ggtitle("Ann. Wet Degree Days \n 5th Percentile over previous 30 years - 2014"))

# Annual wet degree days \n mean over previous 30 years
# rasterize data
annVPD_rast <- climFigDat %>% 
  filter(Year == 2014) %>% 
  #slice_sample(n = 5e4) %>%
  
  #terra::vect() %>% 
  #terra::set.crs(crs(test_rast)) %>% 
  terra::rasterize(y = test_rast, 
                   field = "annVPD_max_95percentile_CLIM", 
                   
                   fun = mean, na.rm = TRUE) %>% 
  terra::crop(ext(-2000000, 2500000, -2000000, 1200000
  )) %>% 
  na.omit()
(annVPD_rast_plotAll <- ggplot() + 
    tidyterra::geom_spatraster(data = annVPD_rast, aes(fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    scale_fill_viridis_c(option = "A", 
                         guide = guide_colorbar(title = "milibars")#, 
                         #limits = c(0,4000)
    ) +
    ggtitle("Ann. Max VPD \n 95th Percentile over previous 30 years - 2014"))

# Annual wet degree days \n mean over previous 30 years
# rasterize data
frostFreeDays_rast <- climFigDat %>% 
  filter(Year == 2014) %>% 
  #slice_sample(n = 5e4) %>%
  
  #terra::vect() %>% 
  #terra::set.crs(crs(test_rast)) %>% 
  terra::rasterize(y = test_rast, 
                   field = "durationFrostFreeDays_5percentile_CLIM", 
                   
                   fun = mean, na.rm = TRUE) %>% 
  terra::crop(ext(-2000000, 2500000, -2000000, 1200000
  )) %>% 
  na.omit()
(frostFreeDays_rast_plotAll <- ggplot() + 
    tidyterra::geom_spatraster(data = frostFreeDays_rast, aes(fill = mean), na.rm = TRUE) +
    theme_minimal() + 
    scale_fill_viridis_c(option = "A", 
                         guide = guide_colorbar(title = "days")#, 
                         #limits = c(0,4000)
    ) +
    ggtitle("Frost Free Days \n 5th Percentile over previous 30 years - 2014"))

# Put all plots together  -------------------------------------------------
#pdf(file = "./Figures/soilsFigDatFigures/TotalTreeFigures.pdf", width = 11, height = 3.75, compress = FALSE)
# tree figures
ggarrange(prcp_totalAnn30yr_plotAll, swe_CLIM_plotAll, tmin_CLIM_plotAll, 
          tmax_CLIM_plotAll, tmean_CLIM_plotAll, vp_CLIM_plotAll, 
          T_warmestMonth_meanAnnAvg_CLIM_plotAll, T_coldestMonth_meanAnnAvg_CLIM_plotAll,
          precipSeasonality_rast_plotAll, precipTempCorr_rast_plotAll, 
          aboveFreezeMonth_rast_plotAll, isothermality_rast_plotAll, 
          annWatDef_rast_plotAll, annWetDegDays_rast_plotAll, 
          annVPD_rast_plotAll, frostFreeDays_rast_plotAll,
           ncol = 2, 
          nrow = 8) %>% 
  ggexport(
    filename = "./Figures/climateVariableFigures.pdf", 
    width = 9, height = 25)

