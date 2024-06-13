#///////////////////
# Rangeland Analysis Platform (RAP) data aquisition and cleaning
# Alice Stears
# 6/5/24
#///////////////////


# load packages -----------------------------------------------------------

library(tidyverse)
library(sf)
library(terra)

# generate random points to sample RAP data with --------------------------
## first, get a shapefile of CONUS
CONUS <- st_read(dsn = "./data/CONUS_extent/", layer = "CONUS_boundary") %>% 
  st_make_valid()%>% 
  st_transform("EPSG:4269") %>% 
  st_set_crs("EPSG:4269") %>% 
  filter(ID == "main")

# subdivide into EPA Level I ecoregions
ecoregions <- st_read(dsn = "./data/Level1Ecoregions/", layer = "NA_CEC_Eco_Level1") %>% 
  st_transform("EPSG:4269") %>%  # reproject to match CONUS
  st_set_crs("EPSG:4269") %>% 
  st_make_valid() %>% 
  filter(NA_L1NAME != "WATER")
# turn of spherical geometery, since it's causing problems
sf_use_s2(FALSE)
# trim the ecoregions to CONUS extent
ecoregions_new <- CONUS %>% 
  st_intersection(ecoregions)

# get the test area
testArea <- st_read(
  dsn = "../shortTermDroughtForescaster/gridSTDF/projects/07_TestOutputForFRESC/FRESC_testBox/", 
  layer = "GriddedDroughtSubset") 
## use a mask to remove agricultural or developed area get the masks from land
#use data from LCMAP (used data from 2021, since it's likely the most
#exclusive...?) from: https://eros.usgs.gov/lcmap/apps/data-downloads. LCMAP
#uses LANDSAT analysis-ready data, just like RAP, so should be on the same grid,
#right??
LCMAP <- terra::rast("./data/LCMAP/LCMAP_CU_2021_V13_LCPRI.tif") #%>% 
 #terra::project(y = "EPSG:4269")
# reproject the ecoregions data according to LCMAP projection
ecoregions_new <- 
  ecoregions_new %>% 
  st_transform(crs(LCMAP))

# reclassify so that 0 = raster cells that are developed (1) or cropland (2)  or water (5) and 1 = any other land use
LCMAP_use <- classify(LCMAP, rcl = matrix(c(1,2,3,4,5,6,7,8,0,0,1,1,0,1,1,1), nrow = 8))

LCMAP_use <- LCMAP_use %>% 
  mask(LCMAP_use, maskvalues = 0)

# save reclassified data
#saveRDS(LCMAP_use, file = "./data/LCMAP/LCMAP_reclassifiedToUse.rds")
LCMAP_use <- readRDS("./data/LCMAP/LCMAP_reclassifiedToUse.rds")
terra::plot(LCMAP_use)
# function to sample spatial points
ecoSample <- function(x, sampSize) {
  x_new <- x %>% 
    spatSample(size = sampSize, method = "random", na.rm = TRUE, replace = FALSE, as.points = TRUE) %>% 
    st_as_sf()
  
  return(x_new)
}

LCMAP_samplePoints <- ecoSample(LCMAP_use, sampSize = 100000)

## save the sample point data
sf::st_write(LCMAP_samplePoints, dsn = "data/RAP_samplePoints/", layer = "randomPoints_100Thousand", 
             driver = "ESRI Shapefile")
# ## subdivide the raster by ecoregions (to make it smaller, and also may be useful for deciding which part to use...?)
# # northern forests (5)
# ecoRast_5 <- LCMAP_use %>% 
#   crop(ecoregions_new[ecoregions_new$NA_L1CODE==5,]) %>% 
#   mask(ecoregions_new[ecoregions_new$NA_L1CODE==5,]) 
# ecoRast_5  <- ecoRast_5 %>% 
#   mask(ecoRast_5, maskvalues = 0)
# 
# # northwestern forested mountains (6)
# ecoRast_6 <- LCMAP_use %>% 
#   crop(ecoregions_new[ecoregions_new$NA_L1CODE==6,]) %>% 
#   mask(ecoregions_new[ecoregions_new$NA_L1CODE==6,])
# ecoRast_6  <- ecoRast_6 %>% 
#   mask(ecoRast_6, maskvalues = 0)
# 
# # marine west coast forest (7)
# ecoRast_7 <- LCMAP_use %>% 
#   crop(ecoregions_new[ecoregions_new$NA_L1CODE==7,]) %>% 
#   mask(ecoregions_new[ecoregions_new$NA_L1CODE==7,])
# ecoRast_7  <- ecoRast_7 %>% 
#   mask(ecoRast_7, maskvalues = 0)
# 
# # eastern temperature forests (8)
# ecoRast_8 <- LCMAP_use %>% 
#   crop(ecoregions_new[ecoregions_new$NA_L1CODE==8,]) %>% 
#   mask(ecoregions_new[ecoregions_new$NA_L1CODE==8,])
# ecoRast_8  <- ecoRast_8 %>% 
#   mask(ecoRast_8, maskvalues = 0)
# 
# # great plains (9)
# ecoRast_9 <- LCMAP_use %>% 
#   crop(ecoregions_new[ecoregions_new$NA_L1CODE==9,]) %>% 
#   mask(ecoregions_new[ecoregions_new$NA_L1CODE==9,])
# ecoRast_9  <- ecoRast_9 %>% 
#   mask(ecoRast_9, maskvalues = 0)
# 
# # north american deserts (10)
# ecoRast_10 <- LCMAP_use %>% 
#   crop(ecoregions_new[ecoregions_new$NA_L1CODE==10,]) %>% 
#   mask(ecoregions_new[ecoregions_new$NA_L1CODE==10,])
# ecoRast_10  <- ecoRast_10 %>% 
#   mask(ecoRast_10, maskvalues = 0)
# 
# # mediterranean California (11)
# ecoRast_11 <- LCMAP_use %>% 
#   crop(ecoregions_new[ecoregions_new$NA_L1CODE==11,]) %>% 
#   mask(ecoregions_new[ecoregions_new$NA_L1CODE==11,])
# ecoRast_11  <- ecoRast_11 %>% 
#   mask(ecoRast_11, maskvalues = 0)
# 
# # southern semiarid highlands (12)
# ecoRast_12 <- LCMAP_use %>% 
#   crop(ecoregions_new[ecoregions_new$NA_L1CODE==12,]) %>% 
#   mask(ecoregions_new[ecoregions_new$NA_L1CODE==12,])
# ecoRast_12  <- ecoRast_12 %>% 
#   mask(ecoRast_12, maskvalues = 0)
# # temperate forests (13)
# ecoRast_13 <- LCMAP_use %>% 
#   crop(ecoregions_new[ecoregions_new$NA_L1CODE==13,]) %>% 
#   mask(ecoregions_new[ecoregions_new$NA_L1CODE==13,])
# ecoRast_13  <- ecoRast_13 %>% 
#   mask(ecoRast_13, maskvalues = 0)
# # tropical wet forests (15)
# ecoRast_15 <- LCMAP_use %>% 
#   crop(ecoregions_new[ecoregions_new$NA_L1CODE==15,]) %>% 
#   mask(ecoregions_new[ecoregions_new$NA_L1CODE==15,])
# ecoRast_15  <- ecoRast_15 %>% 
#   mask(ecoRast_15, maskvalues = 0)
# 
# ecoRast_5_samplePoints <- ecoSample(ecoRast_5, sampSize = 1000)
# ecoRast_6_samplePoints <- ecoSample(ecoRast_6, sampSize = 1000)
# ecoRast_7_samplePoints <- ecoSample(ecoRast_7, sampSize = 1000)
# ecoRast_8_samplePoints <- ecoSample(ecoRast_8, sampSize = 1000)
# ecoRast_9_samplePoints <- ecoSample(ecoRast_9, sampSize = 1000)
# ecoRast_10_samplePoints <- ecoSample(ecoRast_10, sampSize = 1000)
# ecoRast_11_samplePoints <- ecoSample(ecoRast_11, sampSize = 1000)
# ecoRast_12_samplePoints <- ecoSample(ecoRast_12, sampSize = 1000)
# ecoRast_13_samplePoints <- ecoSample(ecoRast_13, sampSize = 1000)
# ecoRast_15_samplePoints <- ecoSample(ecoRast_15, sampSize = 1000)
# 
# plot(ecoRast_9)
# plot(ecoRast_9_samplePoints$geometry, add = TRUE)
# 
# 
# # add all points together 
# samplePoints <- rbind(ecoRast_5_samplePoints, ecoRast_6_samplePoints, ecoRast_7_samplePoints, 
#                       ecoRast_8_samplePoints,  ecoRast_9_samplePoints, ecoRast_10_samplePoints, 
#                       ecoRast_11_samplePoints, ecoRast_12_samplePoints, ecoRast_13_samplePoints, 
#                       ecoRast_15_samplePoints)


# read in RAP points sampled from Google Earth Engine ---------------------
RAPdat <- read.csv("~/Downloads/RAP_VegCover.csv")

RAPdat$LON<- apply(RAPdat, MARGIN = 1, FUN = function(x) 
  stringr::str_extract(string =x[".geo"], pattern = regex("-\\d+.\\d+"))
  )

RAPdat$LAT <- 
  apply(RAPdat, MARGIN = 1, FUN = function(x) 
    #x[".geo"]
  stringr::str_extract(string =x[".geo"], pattern = regex(",\\d+.\\d+")) %>% 
    str_extract(, pattern = regex("\\d+.\\d+"))
)

RAPdat_2 <- st_as_sf(RAPdat, coords = c("LON", "LAT"))
# get data just for one year 
RAPdat_2$Year <- str_sub(RAPdat_2$system.index, start = 1, end = 4)

## randomly select three years (will equal ~300,000 data points, a few less than in the field-collected data)
unique(RAPdat_2$Year)

#indices <- round(runif(n = 3, min = 1, max = length(unique(RAPdat_2$Year))))
indices <- c(14, 7, 29)
randYears <- unique(RAPdat_2$Year)[indices]
RAPdat_3 <- RAPdat_2[RAPdat_2$Year %in% c(randYears), ]
plot(RAPdat_3$geometry)

## looks good! Fewer points in places we filtered out for ag. and developed land uses
## change cover value column titles to be meaningful 

