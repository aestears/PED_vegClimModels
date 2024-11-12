#///////////////////
# Visualizing raw cover data from sources
# Alice Stears
# 11/11/2024
#///////////////////


# load packages -----------------------------------------------------------

library(tidyverse)
library(sf)
library(terra)

# load data ---------------------------------------------------------------
# load shapefile version of "big composition dataset"
coverDat <- st_read("./Data_processed/DataForAnalysisPoints", layer = "vegCompPoints")

# load raster to rasterize cover data
# rasterize
test_rast <-  rast("./Data_raw/dayMet/rawMonthlyData/orders/70e0da02b9d2d6e8faa8c97d211f3546/Daymet_Monthly_V4R1/data/daymet_v4_prcp_monttl_na_1980.tif") %>% 
  terra::aggregate(fact = 32, fun = "mean") %>% 
  terra::project(crs(coverDat))

# total tree cover data ---------------------------------------------------
# rasterize data
treeCover_rast <- coverDat %>% 
  #slice_sample(n = 5e4) %>%
  terra::vect() %>% 
  #terra::set.crs(crs(test_rast)) %>% 
  terra::rasterize(y = test_rast, 
                   field = "TtlTrCv", 
                   fun = mean, na.rm = TRUE) %>% 
  terra::crop(ext(-135, -60, 20, 55))
treeCov_plot <- terra::plot(treeCover_rast, main = "Total tree % cover", clip = TRUE, 
            plg = list(title = "Tree % Cover"), 
            col = map.pal("curvature"))

# conifer tree data -------------------------------------------------------
# rasterize data
conifCover_rast <- coverDat %>% 
  #slice_sample(n = 5e4) %>%
  terra::vect() %>% 
  #terra::set.crs(crs(test_rast)) %>% 
  terra::rasterize(y = test_rast, 
                   field = "TtlTrCv", 
                   fun = mean, na.rm = TRUE) %>% 
  terra::crop(ext(-135, -60, 20, 55))
terra::plot(treeCover_rast, main = "Total tree % cover", clip = TRUE, 
            plg = list(title = "Tree % Cover"), 
            col = map.pal("curvature"))

# broad-leaved tree data --------------------------------------------------

# shrub data --------------------------------------------------------------


# graminoid data ----------------------------------------------------------


# C3 grass data -----------------------------------------------------------


# C4 grass data -----------------------------------------------------------


# forb data ---------------------------------------------------------------


# gram+forb data ----------------------------------------------------------


# CAM data ----------------------------------------------------------------




