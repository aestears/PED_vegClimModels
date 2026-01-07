#///////////////////
# Rangeland Analysis Platform (RAP) data aquisition and cleaning
# Alice Stears
# 6/5/24
#///////////////////


# load packages -----------------------------------------------------------

library(tidyverse)
library(sf)
library(terra)
library(rapr)

# generarapr# generate random points to sample RAP data with --------------------------
## first, get a shapefile of CONUS
CONUS <- st_read(dsn = "./Data_raw/CONUS_extent/", layer = "CONUS_boundary") %>% 
  st_make_valid()%>% 
  st_transform("EPSG:4269") %>% 
  st_set_crs("EPSG:4269") %>% 
  filter(ID == "main")

# subdivide into EPA Level I ecoregions
ecoregions <- st_read(dsn = "./Data_raw/Level1Ecoregions/", layer = "NA_CEC_Eco_Level1") %>% 
  st_transform("EPSG:4269") %>%  # reproject to match CONUS
  st_set_crs("EPSG:4269") %>% 
  st_make_valid() %>% 
  filter(NA_L1NAME != "WATER")
# turn of spherical geometery, since it's causing problems
sf_use_s2(FALSE)
# trim the ecoregions to CONUS extent
ecoregions_new <- CONUS %>%
  st_make_valid() %>% 
  st_intersection(ecoregions%>%
                    st_make_valid())
# get ecoregion locations only for grass/shrub
ecoregions_new <- ecoregions_new %>% 
  filter(NA_L1NAME %in% c("GREAT PLAINS", "MEDITERRANEAN CALIFORNIA", "NORTH AMERICAN DESERTS","SOUTHERN SEMIARID HIGHLANDS"))

## use a mask to remove agricultural or developed area get the masks from land
#use data from LCMAP (used data from 2021, since it's likely the most
#exclusive...?) from: https://eros.usgs.gov/lcmap/apps/data-downloads. LCMAP
#uses LANDSAT analysis-ready data, just like RAP, so should be on the same grid

#LCMAP <- terra::rast("./Data_raw/LCMAP/LCMAP_CU_2021_V13_LCPRI.tif") #%>% 
#terra::project(y = "EPSG:4269")
# reproject the ecoregions data according to LCMAP projection
# ecoregions_new <- 
#   ecoregions_new %>% 
#   st_transform(crs(LCMAP))
# 
# # reclassify so that 0 = raster cells that are developed (1) or cropland (2)  or water (5) and 1 = any other land use
# LCMAP_use <- classify(LCMAP, rcl = matrix(c(1,2,3,4,5,6,7,8,0,0,1,1,0,1,1,1), nrow = 8))
# 
# LCMAP_use <- LCMAP_use %>% 
#   mask(LCMAP_use, maskvalues = 0)
# 
# # save reclassified data
#  saveRDS(LCMAP_use, file = "./Data_raw/LCMAP/LCMAP_reclassifiedToUse.rds")
# LCMAP_use <- readRDS("./Data_raw/LCMAP/LCMAP_reclassifiedToUse.rds") 
# 
# ## get dayMet raster that we'll use to generate sampling points
# dayMetGrid <-  rast("./Data_raw/dayMet/rawMonthlyData/orders/70e0da02b9d2d6e8faa8c97d211f3546/Daymet_Monthly_V4R1/data/daymet_v4_prcp_monttl_na_1980.tif") #%>% 
# dayMetGrid <- dayMetGrid %>% terra::project(y = crs(LCMAP_use))
# 
# crs(dayMetGrid) == crs(LCMAP_use)
# 
# ecoregions_new <- ecoregions_new %>% 
#   sf::st_transform(crs = st_crs(LCMAP_use))
# 
# # first, remove raster cells that aren't in the grass/shrub ecoregion (change those values to NA)
# dayMet_crop <- dayMetGrid$daymet_v4_prcp_monttl_na_1980_1 %>% 
#   terra::mask(mask = terra::vect(ecoregions_new)) %>% 
#   terra::crop(y = terra::vect(ecoregions_new))
# 
# # then, remove raster cells that are in developed/undesirable areas (change those values to NA) 
# LCMAP_use_crop <- (LCMAP_use %>% terra::mask(mask = terra::vect(ecoregions_new)) %>% 
#                      terra::crop(y = dayMet_crop) %>% 
#                      terra::resample(y = dayMet_crop))
# 
# dayMet_crop2 <- dayMet_crop * LCMAP_use_crop
# 
# ## then, generate a random point within each dayMet cell 
# # turn the raster into a grid polygon using the sp package
# dayMet_grid <- raster::rasterToPolygons(raster::raster(dayMet_crop2))
# # temp <- st_as_sf(dayMet_grid)
# # plot(temp$geometry[1:10000,])
# 
# # use the sp package to generate a random point within each dayMet grid 
# samplePoints_temp <- sapply(dayMet_grid@polygons, sp::spsample, n = 1, type = "random")
# # take out of 
# samplePoints <- samplePoints_temp %>% lapply(FUN = function(x) {
#   data.frame("test" = 1, "geometry" = st_as_sf(x))
# }) %>% 
#   purrr::list_rbind()
# samplePoints <- st_as_sf(samplePoints)
# st_crs(samplePoints) <- st_crs(dayMet_crop2)
# 
# ## make a rectangle around each point that's 60m square (should encompass the closes 4 RAP cells?)
# samplePoints_squares <- samplePoints %>% 
#   st_buffer(dist = 30, endCapStyle = "SQUARE")

#plot(temp$geometry[1:10])
#points(samplePoints[1:10,"geometry"])
#plot(samplePoints_squares[1:10,"geometry"], add = TRUE)

# ## save the sample point data (w/ buffer squares)
# sf::st_write(samplePoints_squares, dsn = "Data_processed/RAP_samplePoints/", layer = "singlePointPerDayMetCell_withBuffer", 
#              driver = "ESRI Shapefile", overwrite = TRUE, append = FALSE)
# ## save two point rectangles for experimentation purposes
# sf::st_write(samplePoints_squares[1:10,], dsn = "Data_processed/RAP_samplePoints/", layer = "singlePointPerDayMetCell_withBuffer_TEST", 
#              driver = "ESRI Shapefile", overwrite = TRUE, append = FALSE)
# ## save the sample point data
# sf::st_write(samplePoints, dsn = "Data_processed/RAP_samplePoints/", layer = "singlePointPerDayMetCell", 
#              driver = "ESRI Shapefile", overwrite = TRUE, append = FALSE)
# 

samplePoints_squares <- sf::st_read(dsn = "Data_processed/RAP_samplePoints/", layer = "singlePointPerDayMetCell_withBuffer")
samplePoints <- sf::st_read(dsn = "Data_processed/RAP_samplePoints",  layer = "singlePointPerDayMetCell")

## save the upscaled dayMet grid 
# dayMet_13 <- dayMetGrid$daymet_v4_prcp_monttl_na_1980_1 %>% 
#   terra::crop(y = dayMet_crop2) %>% 
#   terra::aggregate(fact = 13, fun = "mean") 
# terra::writeRaster(dayMet_13, filename = "Data_processed/RAP_samplePoints/dayMetGrid_times13_forSampling.tif", overwrite = TRUE)
dayMet_13 <- rast("./Data_processed/RAP_samplePoints/dayMetGrid_times13_forSampling.tif")

## for each samplePoint w/ square buffer, give it an ID indicating which dayMet_13 grid cell it's in
# get dayMet_13 grid 
dayMet_13_grid <- raster::rasterToPolygons(raster::raster(dayMet_13))
dayMet_13_grid2 <- dayMet_13_grid %>% 
  st_as_sf() %>% 
  sf::st_transform(crs = st_crs(samplePoints))

# get uniqueIDs for each each gridcell and assign to sample points
temp <- terra::extract(x = dayMet_13, y = vect(samplePoints), xy = TRUE)
temp$uniqueID <- paste0(temp$x, "_", temp$y)
samplePoints <- samplePoints %>% 
  select(test, geometry) %>% 
  cbind(temp[,"uniqueID"]) %>% 
  rename(uniqueID = `temp....uniqueID..`)

samplePoints_squares <- samplePoints_squares %>% 
  select(test, geometry) %>% 
  cbind(temp$uniqueID) %>% 
  dplyr::rename(uniqueID = temp.uniqueID)

# # test GEE outputs
# # load GEE output
# tempRast <- rast("../../../../../../Downloads/RAP_custom_grid_2000.tif")  %>%
#   terra::project(samplePoints_squares)
# 
# tempsf <- st_read(dsn = "../../../../../../Downloads/RAP_VegCover/", layer = "RAP_VegCover")
# tempsf_point <- st_read(dsn = "../../../../../../Downloads/RAP_VegCover_points/", layer = "RAP_VegCover")
# mapview(tempsf) + mapview(tempsf_point, col.regions = "red")
# 
# # tempRast <- tempRast %>%
# #   terra::project(samplePoints_squares)
# # # get grid of the GEE output
# # tempRast_grid <- raster::rasterToPolygons(raster::raster(tempRast))
# # get the grid of the dayMet aggregated by 4 raster
# dayMet_4 <- dayMetGrid$daymet_v4_prcp_monttl_na_1980_1 %>%
#   terra::aggregate(fact = 13, fun = "mean")  %>%
#   terra::project(samplePoints_squares) %>%
#   terra::crop(ext(c(-1182699.77885738, -1122699.77885738), ylim = c(2021269.29775776, 2041269.29775776)))
# dayMet_4_grid <- raster::rasterToPolygons(raster::raster(dayMet_4))
# 
# # # plot 
# # par(mfrow = c(2,1))
# # plot(tempRast$PFG, xlim = c(-1182699.77885738, -1122699.77885738), ylim = c(2021269.29775776, 2041269.29775776))
# # plot(tempRast_grid, add = TRUE)
# # plot(dayMet_4_grid[1:2,], col = "red", add = TRUE)
# # plot(dayMet_crop2, xlim = c(-1182699.77885738, -1122699.77885738), ylim = c(2021269.29775776, 2041269.29775776))
# # plot(tempRast_grid, add = TRUE)
# # plot(dayMet_4_grid[1:2,], col = "red", add = TRUE)
# # 
# # par(mfrow = c(1,1))
# 
# # distance between centroids of new plots
# tempCentroids <- terra::xyFromCell(dayMet_4, cell = c(1:4)) %>% 
#   as.data.frame() %>% 
#   st_as_sf(coords = c("x", "y"), crs = st_crs(tempRast))
# tempDistances <- st_distance(tempCentroids) %>% 
#   apply(MARGIN = 1, FUN = function(x) {
#     x <- as.numeric(x)
#     min(x[x != 0])
#   })


# Try using rapr R package, since GEE is very frustrating -----------------
# # for each set of points that are in the same dayMetx13 gridcell (sampleIDs that have the same 'uniqueID')
# uniqueIDs <- samplePoints %>% st_drop_geometry() %>% select(uniqueID) %>% unique()
# # make sure everything is in the correct crs ("EPSG:4326" required for get_rap() function)
# samplePoints_squares <- samplePoints_squares %>% 
#   st_transform(crs = "EPSG:4326")
# samplePoints <- samplePoints %>% 
#   st_transform(crs = "EPSG:4326")
# dayMet_13_grid2 <- dayMet_13_grid2 %>% 
#   st_transform(crs = "EPSG:4326")
# 
# for (i in 1:nrow(uniqueIDs)
#      ) {
#   # get unique dayMetx13 gridcell ID 
#   uniqueID_i <- uniqueIDs[i,]
#   # get unique points just in that raster cell 
#   points_i <- samplePoints_squares %>% 
#     filter(uniqueID == uniqueID_i)
#   # get the corresponding raster cell boundary 
#   gridCell_id_i <- st_nearest_feature(points_i[1,], dayMet_13_grid2, sparse = FALSE)
#   gridCell_i <- dayMet_13_grid2[gridCell_id_i,]
#   #mapview(gridCell_i) + mapview(points_i, col.regions = "red")
#   ## get the RAP values that lie within this boundary
#   rap_i <- rapr::get_rap(x = gridCell_i, years = c(2023:2024), 
#                 source = "rap-30m", 
#                 product = "vegetation-cover", 
#                 verbose = FALSE
#                 )
#   ## for each 'point', get teh averaged RAP value w/in that rectangle for each get/year combo (150 of them!)
#   ## create a matrix to hold the information
#   for (j in 1:nrow(points_i)) {
#     points_ij <- points_i[j,]
#     rap_ij <- terra::extract(x = rap_i, y = points_ij, fun = "mean")
#     #rap_ij_test <- terra::crop(x = rap_i, y = points_ij)
#     # save info
#     if(j == 1) {
#       outDat_j <- rap_ij
#     } else {
#       outDat_j <- rbind(outDat_j, rap_ij)
#     }
#   }
#   ## now, average all of the outDat information in the same dayMet gridcell and save! 
#   outDat_i <- as_tibble(outDat_j) %>% 
#     group_by(ID) %>% 
#     summarize(across('vegetation-cover_v3_2023_annual_forb_and_grass':'vegetation-cover_v3_2024_tree', mean))
#   ## add information for the centroid of the given dayMet gridcell
#   outDat_i <- outDat_i %>% 
#     cbind(gridCell_i$geometry)
#     
#   print(paste0("iteration ", i," completed"))
#   
#   if (i == 1) {
#     outDat <- outDat_i
#   } else {
#     outDat <- rbind(outDat, outDat_i)
#     if (i %in% c(seq(from = 1000, to = 22238, by = 1000))) {
#       assign(paste0("tempDatThrough_",i), outDat)
#     } 
#   }
# }
# 
# outDat <- outDat %>% 
#   st_as_sf()
# #mapview(outDat, zcol = 'vegetation-cover_v3_2000_annual_forb_and_grass')

# read in RAP points sampled from Google Earth Engine ---------------------
## get RAP information downloaded from GEE
# file names
RAPnames <- list.files("./Data_processed/RAP_samplePoints/SampledData/")
for (i in 1:length(RAPnames)) {
  ## read in the raster
  tempRast <- rast(x = paste0("./Data_processed/RAP_samplePoints/SampledData/", RAPnames[i])) #%>% 
    ## transform the raster to the crs of samplePoints
    #terra::project(crs(samplePoints))
  ## save the raster
  assign(paste0("RAPraster_", str_extract(RAPnames[i], "[:digit:]{4}")), value = tempRast)
  ## get the values of the raster
  tempValues <- tempRast %>% 
    terra::extract(y = as.data.frame(terra::xyFromCell(tempRast, cell = c(1:94094))),
                   xy = TRUE) %>% drop_na() %>% 
    mutate(Year = str_extract(RAPnames[i], "[:digit:]{4}")) %>% 
    dplyr::select(-ID)
  ## save output 
  if (i == 1) {
    RAPvaluesDF <- tempValues
  } else {
    RAPvaluesDF <- RAPvaluesDF %>% 
      rbind(tempValues)
  }
}

## change cover value column titles to be meaningful 
RAPdat_4 <- RAPvaluesDF %>% 
  rename(Lat = y, 
         Lon = x, 
         AnnualHerbGramCover = AFG,
         PerennialHerbGramCover = PFG,
         BareGroundCover = BGR, 
         LitterCover = LTR, 
         ShrubCover = SHR,
         TotalTreeCover = TRE
  ) %>% 
  mutate(
    UniqueID = 1:nrow(.), 
    StateUnitCounty = NA,
    Plot = NA, 
    PlotCondition = NA,
    date = Year, 
    Lat = Lat, 
    Lon = Lon,
    ShrubCover = ShrubCover,
    HerbCover = NA,
    AnnualHerbGramCover = AnnualHerbGramCover,
    PerennialHerbGramCover = PerennialHerbGramCover,
    TotalGramCover = NA,
    C3GramCover = NA, 
    C4GramCover = NA, 
    AngioTreeCover = NA, 
    ConifTreeCover = NA,
    TotalTreeCover = TotalTreeCover,
    TreeBasalArea_in2 = NA, 
    BareGroundCover = BareGroundCover, 
    LitterCover = LitterCover,
    LitterDepth = NA,
    Source = "RAP"
  )

## save for further analysis! 
write.csv(st_drop_geometry(RAPdat_4), "./Data_raw/RAP_samplePoints/RAPdata_use.csv", row.names = FALSE)
