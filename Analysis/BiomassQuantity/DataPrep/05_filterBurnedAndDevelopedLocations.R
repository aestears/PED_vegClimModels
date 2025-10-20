#///////////////////
# Filtering out biomass ovbservations for burned and developed areas
# Alice Stears
# 08/08/2025
#///////////////////

# load packages -----------------------------------------------------------
library(sbtools)
library(here)
library(assertthat)
library(sf)
library(tigris)
library(tidyverse)

# source internal functions -----------------------------------------------
sapply(paste0("./Functions/",list.files("./Functions/")), source)

#Download combined wildland fire dataset ---------------------------------
# https://www.sciencebase.gov/catalog/item/61707c2ad34ea36449a6b066
##
## script uses sbtools https://github.com/DOI-USGS/sbtools
## a package for interfacing to ScienceBase
## training module: https://owi.usgs.gov/R/training-curriculum/usgs-packages/sbtools-intro/index.html
## downloading the data requires a ScienceBase account

# Destination folders

# use here to get current directory
here::i_am("Analysis/BiomassQuantity/DataPrep/05_filterBurnedAndDevelopedLocations.R")

# input where you will download the Zip files from Science Base
# as well as the unzipped data files
destinationDirectory <- here::here("Data_raw/","combinedWildlandFireDB")
# 
# # Authenticate to ScienceBase
# # uses new authentication method in sbtools>=1.3.1
# initialize_sciencebase_session()
# 
# # Query for the data release
# # use the most recent data release version
# results <- query_sb_text("Combined wildland fire datasets for the United States and certain territories")
# 
# # +input the item Science Base item ID
# combinedWildlandFire_ID <- "61707c2ad34ea36449a6b066"
# combinedWildlandFirePolygon_ID <- "61aa537dd34eb622f699df81"
# combinedWildlandFireRaster_ID <- "61aa5483d34eb622f699df85"
# identifier_exists(combinedWildlandFire_ID)
# 
# # Download item
# item_file_download(combinedWildlandFire_ID,dest_dir=destinationDirectory)
# item_file_download(combinedWildlandFirePolygon_ID,dest_dir=destinationDirectory)
# # item_file_download(combinedWildlandFireRaster_ID,dest_dir=destinationDirectory)
# 
# 
# # Get Monitoring Trends in Burn Severity data -----------------------------
# 
# destinationDirectory <- here::here("Data_raw/","mtbs_perimeter_data")
# 
# # Query perimeters
# 
# ## MTBS fire perimeters (burned area boundaries), 1984-2022
# # will be downloaded directly to Data folder
# MTBS_download <- file.path(destinationDirectory, 'mtbs_perims_DD.shp')
# if (!file.exists(MTBS_download)) {
#   dataSourceLink <-"https://edcintl.cr.usgs.gov/downloads/sciweb1/shared/MTBS_Fire/data/composite_data/burned_area_extent_shapefile/mtbs_perimeter_data.zip"
#   destinationDirectoryZip <- paste0(destinationDirectory, ".zip")
#   download.file(dataSourceLink, destinationDirectoryZip)
#   unzip(destinationDirectoryZip, exdir = destinationDirectory)
#   unlink(destinationDirectoryZip)
#   assert_that(file.exists(MTBS_download))
# }

# Read in Data ----------------------------------------------------------
# mtbs data
mtbs <- st_read(dsn = "./Data_raw/mtbs_perimeter_data/", layer = "mtbs_perims_DD")

#combined wildland fire 
cwf_layers <- st_layers(dsn = "./Data_raw/combinedWildlandFireDB/Fire_Feature_Data_Pro2_8_Geodatabase/Fire_Feature_Data.gdb/")
cwf <- st_read(dsn = "./Data_raw/combinedWildlandFireDB/Fire_Feature_Data_Pro2_8_Geodatabase/Fire_Feature_Data.gdb/", layer = "USGS_Wildland_Fire_Combined_Dataset")
# transform crs to match mtbs
cwf <- cwf %>% 
  st_transform(st_crs(mtbs)) %>% 
  st_sf() %>% 
  st_cast()

## fix those geometries that are "multisurface"
cwf_multi <- cwf[st_geometry_type(cwf) == "MULTISURFACE",]
cwf_nonMulti <- cwf[st_geometry_type(cwf) != "MULTISURFACE",]

# cwf_multi_fixed <- cwf_multi %>% 
# 
#   #st_make_valid() %>% 
#   surf_to_poly()

cwf_new <- #cwf_multi_fixed %>% 
  # rbind(cwf_nonMulti) %>% 
  cwf_nonMulti

## remove fire perimeter data that isn't in CONUS (ie Alaska)
us_states <- tigris::states()

CONUS_states <- us_states %>% 
  dplyr::filter(REGION %in% c(1:4)) %>% 
  dplyr::filter(STUSPS!="HI") %>%
  dplyr::filter(STUSPS!="AK") %>% 
  st_union() %>% 
  st_make_valid() %>% 
  st_transform(crs = st_crs(mtbs))

# trim to CONUS
mtbs_conus <- mtbs %>% 
  st_make_valid() %>% 
  st_intersection(CONUS_states) %>% 
  st_transform(crs = "PROJCRS[\"unnamed\",\n    BASEGEOGCRS[\"unknown\",\n        DATUM[\"unknown\",\n            ELLIPSOID[\"Spheroid\",6378137,298.257223563,\n                LENGTHUNIT[\"metre\",1,\n                    ID[\"EPSG\",9001]]]],\n        PRIMEM[\"Greenwich\",0,\n            ANGLEUNIT[\"degree\",0.0174532925199433,\n                ID[\"EPSG\",9122]]]],\n    CONVERSION[\"Lambert Conic Conformal (2SP)\",\n        METHOD[\"Lambert Conic Conformal (2SP)\",\n            ID[\"EPSG\",9802]],\n        PARAMETER[\"Latitude of false origin\",42.5,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8821]],\n        PARAMETER[\"Longitude of false origin\",-100,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8822]],\n        PARAMETER[\"Latitude of 1st standard parallel\",25,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8823]],\n        PARAMETER[\"Latitude of 2nd standard parallel\",60,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8824]],\n        PARAMETER[\"Easting at false origin\",0,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8826]],\n        PARAMETER[\"Northing at false origin\",0,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8827]]],\n    CS[Cartesian,2],\n        AXIS[\"easting\",east,\n            ORDER[1],\n            LENGTHUNIT[\"metre\",1,\n                ID[\"EPSG\",9001]]],\n        AXIS[\"northing\",north,\n            ORDER[2],\n            LENGTHUNIT[\"metre\",1,\n                ID[\"EPSG\",9001]]]]")

# switch off spherical geometry
sf_use_s2(FALSE)

cwf_conus <- cwf_new %>% 
  st_make_valid() %>% 
  st_intersection(st_make_valid(CONUS_states)) %>% 
  st_transform(crs = "PROJCRS[\"unnamed\",\n    BASEGEOGCRS[\"unknown\",\n        DATUM[\"unknown\",\n            ELLIPSOID[\"Spheroid\",6378137,298.257223563,\n                LENGTHUNIT[\"metre\",1,\n                    ID[\"EPSG\",9001]]]],\n        PRIMEM[\"Greenwich\",0,\n            ANGLEUNIT[\"degree\",0.0174532925199433,\n                ID[\"EPSG\",9122]]]],\n    CONVERSION[\"Lambert Conic Conformal (2SP)\",\n        METHOD[\"Lambert Conic Conformal (2SP)\",\n            ID[\"EPSG\",9802]],\n        PARAMETER[\"Latitude of false origin\",42.5,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8821]],\n        PARAMETER[\"Longitude of false origin\",-100,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8822]],\n        PARAMETER[\"Latitude of 1st standard parallel\",25,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8823]],\n        PARAMETER[\"Latitude of 2nd standard parallel\",60,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8824]],\n        PARAMETER[\"Easting at false origin\",0,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8826]],\n        PARAMETER[\"Northing at false origin\",0,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8827]]],\n    CS[Cartesian,2],\n        AXIS[\"easting\",east,\n            ORDER[1],\n            LENGTHUNIT[\"metre\",1,\n                ID[\"EPSG\",9001]]],\n        AXIS[\"northing\",north,\n            ORDER[2],\n            LENGTHUNIT[\"metre\",1,\n                ID[\"EPSG\",9001]]]]")




# Get biomass/cover/climate/soils data ------------------------------------
biomassDat <- readRDS("./Data_processed/BiomassQuantityData/GEDIbiomass_predictedCover_climateAndSoils.rds")
biomassDat_sf <- biomassDat %>% 
  st_as_sf(coords = c("Long", "Lat"), remove = FALSE)
st_crs(biomassDat_sf) <-  "PROJCRS[\"unnamed\",\n    BASEGEOGCRS[\"unknown\",\n        DATUM[\"unknown\",\n            ELLIPSOID[\"Spheroid\",6378137,298.257223563,\n                LENGTHUNIT[\"metre\",1,\n                    ID[\"EPSG\",9001]]]],\n        PRIMEM[\"Greenwich\",0,\n            ANGLEUNIT[\"degree\",0.0174532925199433,\n                ID[\"EPSG\",9122]]]],\n    CONVERSION[\"Lambert Conic Conformal (2SP)\",\n        METHOD[\"Lambert Conic Conformal (2SP)\",\n            ID[\"EPSG\",9802]],\n        PARAMETER[\"Latitude of false origin\",42.5,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8821]],\n        PARAMETER[\"Longitude of false origin\",-100,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8822]],\n        PARAMETER[\"Latitude of 1st standard parallel\",25,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8823]],\n        PARAMETER[\"Latitude of 2nd standard parallel\",60,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8824]],\n        PARAMETER[\"Easting at false origin\",0,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8826]],\n        PARAMETER[\"Northing at false origin\",0,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8827]]],\n    CS[Cartesian,2],\n        AXIS[\"easting\",east,\n            ORDER[1],\n            LENGTHUNIT[\"metre\",1,\n                ID[\"EPSG\",9001]]],\n        AXIS[\"northing\",north,\n            ORDER[2],\n            LENGTHUNIT[\"metre\",1,\n                ID[\"EPSG\",9001]]]]"

# Identify overlaps between fire boundaries and plots --------------------------
rm(us_states, CONUS_states, cwf_layers, cwf_multi, #cwf_multi_fixed, 
   cwf_nonMulti, cwf_new, mtbs, cwf)
gc()

## divide the plot data into sections and put them into a list
biomassDat_sf$burnedMoreThan20YearsAgo <- FALSE
biomassDat_sf$group <- rep(1:700, 10000)[1:nrow(biomassDat_sf)]
biomassDat_list <- biomassDat_sf %>% 
  base::split(f = biomassDat_sf$group)

biomassDat_noMtbsList <- lapply(names(biomassDat_list), 
                             FUN = function(y) {
                               print(y)
                               z <- biomassDat_list[[y]]
                               plot_mtbs <-  mtbs_conus %>% 
                                 st_contains(z, sparse = FALSE)
                               # rownames are the index of the fire perimeter
                               # column names are the index of the plot 
                               # filter out the locations that have no overlaps
                               mtbs_overlaps <- which(plot_mtbs==TRUE, arr.ind = TRUE) 
                               colnames(mtbs_overlaps) <- c("firePerim", "plot")
                               ## if there are in fact overlaps 
                               if (nrow(mtbs_overlaps)>0) {
                                 # get a list of the overlaps 
                                 overlapOut <- apply(mtbs_overlaps, MARGIN = 1, FUN = function(x) {
                                   # get plot(s)
                                   plotNow <- z[x[2],]
                                   # get fire perimeter
                                   fireNow <- mtbs_conus[x[1],]
                                   ## is the fire year before the year the plot was sampled? 
                                   # fire year
                                   fireYear <- as.numeric(str_extract(fireNow$Ig_Date, pattern = regex("\\d+")))
                                   ## AES made the decision that a fire more than 20 years previous to the sampling was ok?? but can easily change
                                   if (plotNow$year < fireYear) {# if the plot burned after it was sampled, say we can keep this plot
                                     print(paste0("keep plot w/ the index ",x[2]," because it burned in ", fireYear, " and was sampled in ", plotNow$year))
                                   } else if ((plotNow$year-fireYear) > 20) { # if the plot burned more than 20 years ago
                                     print(paste0("label plot w/ the index", x[2]," as burn historically because it burned in ", fireYear, ", at least 20 years before sampling in ", plotNow$year))
                                   } else {
                                     print(paste0("REMOVE plot w/ the index ", x[2]," because it burned in ", fireYear, ", which was soon before sampling in ", plotNow$year))
                                   }
                                 }) # end of internal apply function
                                 
                                 # remove the plots that were burned less than 20 years before sampling
                                 biomassDat_noFire <- z[!(row(z)[,1] %in% 
                                                         unique(as.numeric(overlapOut[str_detect(overlapOut, "REMOVE")] %>% 
                                                                             str_extract(pattern = "[0-9]+")))),]
                                 # label the plots that were burned more than 20 years before sampling
                                 biomassDat_noFire[(row(biomassDat_noFire)[,1] %in% 
                                                   unique(as.numeric(overlapOut[str_detect(overlapOut, "label")] %>% 
                                                                       str_extract(pattern = "[0-9]+")))),]$burnedMoreThan20YearsAgo <- TRUE
                               } else {
                                 biomassDat_noFire <- z
                               }
                               return(biomassDat_noFire)
                             }) # end of lapply

names(biomassDat_noMtbsList) <- as.character(c(1:length(biomassDat_noMtbsList)))
## do the same filtering for cwf data 
biomassDat_noCWFList <- lapply(names(biomassDat_noMtbsList), 
                            FUN = function(y) {
                              print(y)
                              z <- biomassDat_noMtbsList[[y]]
                              plot_cwf <-  cwf_conus %>% 
                                st_contains(z, sparse = FALSE)
                              # rownames are the index of the fire perimeter
                              # column names are the index of the plot 
                              # filter out the locations that have no overlaps
                              cwf_overlaps <- which(plot_cwf==TRUE, arr.ind = TRUE) 
                              colnames(cwf_overlaps) <- c("firePerim", "plot")
                              ## if there are in fact overlaps 
                              if (nrow(cwf_overlaps)>0) {
                                # get a list of the overlaps 
                                overlapOut <- apply(cwf_overlaps, MARGIN = 1, FUN = function(x) {
                                  # get plot(s)
                                  plotNow <- z[x[2],]
                                  # get fire perimeter
                                  fireNow <- cwf_conus[x[1],]
                                  ## is the fire year before the year the plot was sampled? 
                                  # fire year
                                  fireYear <- fireNow$Fire_Year
                                  ## AES made the decision that a fire more than 20 years previous to the sampling was ok?? but can easily change
                                  if (plotNow$year < fireYear) {# if the plot burned after it was sampled, say we can keep this plot
                                    print(paste0("keep plot w/ the index ",x[2]," because it burned in ", fireYear, " and was sampled in ", plotNow$year))
                                  } else if ((plotNow$year - fireYear) > 20) { # if the plot burned more than 20 years ago
                                    print(paste0("label plot w/ the index ", x[2],"as burned historically because it burned in ", fireYear, ", at least 20 years before sampling in ", plotNow$year))
                                  } else {
                                    print(paste0("REMOVE plot w/ the index ", x[2]," because it burned in ", fireYear, ", which was soon before sampling in ", plotNow$year))
                                  }
                                }) # end of internal apply function
                                
                                # remove the plots that were burned less than 20 years before sampling
                                biomassDat_noFire <- z[!(row(z)[,1] %in% 
                                                        unique(as.numeric(overlapOut[str_detect(overlapOut, "REMOVE")] %>% 
                                                                            str_extract(pattern = "[0-9]+")))),]
                                # label the plots that were burned more than 20 years before sampling
                                biomassDat_noFire[(row(biomassDat_noFire)[,1] %in% 
                                                  unique(as.numeric(overlapOut[str_detect(overlapOut, "label")] %>% 
                                                                      str_extract(pattern = "[0-9]+")))),]$burnedMoreThan20YearsAgo <- TRUE
                                
                              } else {
                                biomassDat_noFire <- z
                              }
                              return(biomassDat_noFire)
                            }) # end of lapply


biomassDat_noFireAll <- purrr::reduce(biomassDat_noCWFList, rbind)

## for those plots that have been burned more than 20 years ago, remove values for tree and total biomass 
# remove tree biomass values
biomassDat_noFireAll[biomassDat_noFireAll$burnedMoreThan20YearsAgo == TRUE & biomassDat_noFireAll$biomassSource == "FIA",]$biomass_MgPerHect <- NA
# remove total biomass values
biomassDat_noFireAll[biomassDat_noFireAll$burnedMoreThan20YearsAgo == TRUE & biomassDat_noFireAll$biomassSource == "GEDI",]$biomass_MgPerHect <- NA

plot(biomassDat_noFireAll$Long, biomassDat_noFireAll$Lat)
ggplot(biomassDat_noFireAll) + 
  facet_wrap(~biomassSource) + 
  geom_sf(aes(col = biomassSource))



# remove observations from areas with farming, cities, etc. ---------------
# use a mask to remove agricultural or developed area get the masks from land
#use data from LCMAP (used data from 2021, since it's likely the most
#exclusive...?) from: https://eros.usgs.gov/lcmap/apps/data-downloads. LCMAP
#uses LANDSAT analysis-ready data, just like RAP, so should be on the same grid,
#right??
LCMAP <- terra::rast("./Data_raw/LCMAP/LCMAP_CU_2021_V13_LCPRI.tif") #%>% 
#terra::project(y = "EPSG:4269")
# reproject the LCMAP data according to biomass projection

# reclassify so that 0 = raster cells that are developed (1) or cropland (2)  or water (5) and 1 = any other land use
# make reclassification matrix
rcl_matrix <- as.matrix(data.frame(from = as.numeric(c(1,2,3,4,5,6,7,8)) , to = as.numeric(c(0,0,1,1,0,1,1,1))))
LCMAP_use <- classify(x = LCMAP, rcl = rcl_matrix)

LCMAP_use <- LCMAP_use %>% 
  terra::mask(LCMAP_use, maskvalues = 0)

LCMAP_use <- LCMAP_use %>% 
  terra::project(test_rast)
# save reclassified data
saveRDS(LCMAP_use, file = "./data/LCMAP/LCMAP_reclassifiedToUse.rds")
# LCMAP_use <- readRDS("./Data_raw/LCMAP/LCMAP_reclassifiedToUse.rds")

## remove the biomass points that are inside of the 'excluded' LCMAP land use areas
biomassDat_noFireNoLCMAP <- LCMAP_use %>% 
  terra::extract(biomassDat_noFireAll, bind = TRUE)  %>% 
  st_as_sf()

# plot(biomassDat_noFireNoLCMAP[!is.na(biomassDat_noFireNoLCMAP$LCMAP_CU_2021_V13_LCPRI) & 
#                                 biomassDat_noFireNoLCMAP$biomassSource == "GEDI",]$geometry)

saveRDS(biomassDat_noFireNoLCMAP, "./Data_processed/BiomassQuantityData/dataForAnalysis_fireAndDevelopmentRemoved.rds")
biomassDat_noFireNoLCMAP <- readRDS( "./Data_processed/BiomassQuantityData/dataForAnalysis_fireAndDevelopmentRemoved.rds")

biomassDat_noFireNoLCMAP <- biomassDat_noFireNoLCMAP %>% 
  mutate(totalTreeCover = broadLeavedTreeCover + needleLeavedTreeCover, 
         totalHerbaceousCover = C3GramCover + C4GramCover + forbCover)

# add in un-relativized cover ---------------------------------------------
# read in "un-relativized" cover data (is a data.frame)
unRelCover <- readRDS("./Data_processed/CoverData/ModelPredictionData/ModelPreds_UNRELATIVIZED_ContempClimateData_dayMetScale.rds")
unRelCover <- unRelCover %>% 
  rename(Long = x, 
         Lat  = y) %>% 
  st_as_sf(coords = c("Long", "Lat"),
    crs ="PROJCRS[\"unnamed\",\n    BASEGEOGCRS[\"unknown\",\n        DATUM[\"unknown\",\n            ELLIPSOID[\"Spheroid\",6378137,298.257223563,\n                LENGTHUNIT[\"metre\",1,\n                    ID[\"EPSG\",9001]]]],\n        PRIMEM[\"Greenwich\",0,\n            ANGLEUNIT[\"degree\",0.0174532925199433,\n                ID[\"EPSG\",9122]]]],\n    CONVERSION[\"Lambert Conic Conformal (2SP)\",\n        METHOD[\"Lambert Conic Conformal (2SP)\",\n            ID[\"EPSG\",9802]],\n        PARAMETER[\"Latitude of false origin\",42.5,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8821]],\n        PARAMETER[\"Longitude of false origin\",-100,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8822]],\n        PARAMETER[\"Latitude of 1st standard parallel\",25,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8823]],\n        PARAMETER[\"Latitude of 2nd standard parallel\",60,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8824]],\n        PARAMETER[\"Easting at false origin\",0,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8826]],\n        PARAMETER[\"Northing at false origin\",0,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8827]]],\n    CS[Cartesian,2],\n        AXIS[\"easting\",east,\n            ORDER[1],\n            LENGTHUNIT[\"metre\",1,\n                ID[\"EPSG\",9001]]],\n        AXIS[\"northing\",north,\n            ORDER[2],\n            LENGTHUNIT[\"metre\",1,\n                ID[\"EPSG\",9001]]]]")

# add in the 'un-relativized' cover data (model predictions that have been scaled according to the predicted ecoregion proportion, but haven't been relativized to sum to 100 across all functional groups)
# biomassDat_noFireNoLCMAP <- biomassDat_noFireNoLCMAP %>% 
#   select(-c("predContemp_CONUS_shrub"                      ,
#              "predContemp_CONUS_bareGround"               , "C3_percentage_pred"                          , "C4_percentage_pred"                           ,
#              "forb_percentage_pred"                                ,                             , "totHerb_synth"                                ,
#              "totTree_synth"                              , "totalCover"                                  , "shrub_cover_finalScaled"                      ,
#              "bareGround_cover_finalScaled"               , "totalHerb_cover_finalScaled"                 , "totalTree_cover_finalScaled"                  ,
#              "needleLeavedTree_perc_scaled_synth"         , "broadLeavedTree_perc_scaled_synth"           , "C3_cover_finalScaled"                         ,
#              "C4_cover_finalScaled"                       , "forb_cover_finalScaled"                      , "broadLeaved_cover_finalScaled"                ,
#              "needleLeaved_cover_finalScaled"                                          ))


# unRelCover$roundedLong <- round(unRelCover$Long, 0)
# biomassDat_noFireNoLCMAP$roundedLong <- round(biomassDat_noFireNoLCMAP$Long, 0)
# unRelCover$roundedLat <- round(unRelCover$Lat, 0)
# # biomassDat_noFireNoLCMAP$roundedLat <- round(biomassDat_noFireNoLCMAP$Lat, 0)
# unRelCover$roundedtotalHerb <- round(unRelCover$totalHerb_cover_finalScaled, 0)
# biomassDat_noFireNoLCMAP$roundedtotalHerb <- round(biomassDat_noFireNoLCMAP$totalHerbaceousCover, 0)
# unRelCover$roundedtotalTree <- round(unRelCover$totalTree_cover_finalScaled, 0)
# biomassDat_noFireNoLCMAP$roundedtotalTree <- round(biomassDat_noFireNoLCMAP$totalTreeCover, 0)

biomassDat_noFireNoLCMAP_RAP <- biomassDat_noFireNoLCMAP %>% 
  filter(biomassSource == "RAP") %>% 
  st_join(unRelCover %>% 
              select(NA_L1CODE:newRegion, predContemp_GS_totHerb:roundedtotalTree),
              join = st_nearest_feature
            # by = c("roundedtotalHerb" , "roundedtotalTree")
            )
biomassDat_noFireNoLCMAP_RAP <- biomassDat_noFireNoLCMAP %>% 
  filter(biomassSource == "RAP") %>% 
  st_join(unRelCover %>% 
            select(NA_L1CODE:newRegion, predContemp_GS_totHerb:roundedtotalTree),
          join = st_nearest_feature
          # by = c("roundedtotalHerb" , "roundedtotalTree")
  )
biomassDat_noFireNoLCMAP_FIA <- biomassDat_noFireNoLCMAP %>% 
  filter(biomassSource == "FIA") %>% 
  st_join(unRelCover %>% 
            select(NA_L1CODE:newRegion, predContemp_GS_totHerb:roundedtotalTree),
          join = st_nearest_feature
          # by = c("roundedtotalHerb" , "roundedtotalTree")
  )
biomassDat_noFireNoLCMAP_GEDI <- biomassDat_noFireNoLCMAP %>% 
  filter(biomassSource == "GEDI") %>% 
  st_join(unRelCover %>% 
            select(NA_L1CODE:newRegion, predContemp_GS_totHerb:roundedtotalTree),
          join = st_nearest_feature
          # by = c("roundedtotalHerb" , "roundedtotalTree")
  )

biomassDat_noFireNoLCMAP <- biomassDat_noFireNoLCMAP_GEDI %>% 
  rbind(biomassDat_noFireNoLCMAP_FIA) %>% 
  rbind(biomassDat_noFireNoLCMAP_RAP)

# test2 <- test %>% 
#   filter(roundedtotalTree.x == roundedtotalTree.y,
#          roundedtotalHerb.x == roundedtotalHerb.y)

# update names and remove unneeded columns
biomassDat_noFireNoLCMAP <- biomassDat_noFireNoLCMAP %>% 
  select(climDatSpatial_ID:LCMAP_CU_2021_V13_LCPRI, 
         predContemp_CONUS_shrub:forb_percentage_pred, 
         totHerb_synth, totTree_synth, totalCover, 
         shrub_cover_finalScaled:needleLeaved_cover_finalScaled
         ) %>% 
  rename(NA_L1CODE = NA_L1CODE.x, 
         NA_L1NAME = NA_L1NAME.x, 
         NA_L1KEY = NA_L1KEY.x,
         newRegion = newRegion.x)

saveRDS(biomassDat_noFireNoLCMAP, file = "./Data_processed/BiomassQuantityData/dataForAnalysis_fireAndDevelopmentRemoved.rds")
#biomassDat_noFireNoLCMAP <- readRDS("./Data_processed/BiomassQuantityData/dataForAnalysis_fireAndDevelopmentRemoved.rds")
