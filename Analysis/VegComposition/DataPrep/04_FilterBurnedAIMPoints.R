#///////////////////
# Filtering AIM plots to remove those that burned recently
# Alice Stears
# 7/11/24
# code for acquiring fire permiters and overlaying them on AIM plots adapted from G. Siegmund 
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

# Download combined wildland fire dataset ---------------------------------
# https://www.sciencebase.gov/catalog/item/61707c2ad34ea36449a6b066
##
## script uses sbtools https://github.com/DOI-USGS/sbtools
## a package for interfacing to ScienceBase
## training module: https://owi.usgs.gov/R/training-curriculum/usgs-packages/sbtools-intro/index.html
## downloading the data requires a ScienceBase account 

# Destination folders 

# use here to get current directory
here::i_am("Analysis/VegComposition/DataPrep/04_FilterBurnedAIMPoints.R")

# input where you will download the Zip files from Science Base
# as well as the unzipped data files
destinationDirectory <- here::here("data","combinedWildlandFireDB")

# Authenticate to ScienceBase 
# uses new authentication method in sbtools>=1.3.1
initialize_sciencebase_session()

# Query for the data release 
# use the most recent data release version 
results <- query_sb_text("Combined wildland fire datasets for the United States and certain territories")

# +input the item Science Base item ID 
combinedWildlandFire_ID <- "61707c2ad34ea36449a6b066"
combinedWildlandFirePolygon_ID <- "61aa537dd34eb622f699df81"
combinedWildlandFireRaster_ID <- "61aa5483d34eb622f699df85"
identifier_exists(combinedWildlandFire_ID)

# Download item 
item_file_download(combinedWildlandFire_ID,dest_dir=destinationDirectory)
item_file_download(combinedWildlandFirePolygon_ID,dest_dir=destinationDirectory)
# item_file_download(combinedWildlandFireRaster_ID,dest_dir=destinationDirectory)


# Get Monitoring Trends in Burn Severity data -----------------------------

destinationDirectory <- here::here("data","mtbs_perimeter_data")

# Query perimeters 

## MTBS fire perimeters (burned area boundaries), 1984-2022
# will be downloaded directly to Data folder
MTBS_download <- file.path(destinationDirectory, 'mtbs_perims_DD.shp')
if (!file.exists(MTBS_download)) {
  dataSourceLink <-"https://edcintl.cr.usgs.gov/downloads/sciweb1/shared/MTBS_Fire/data/composite_data/burned_area_extent_shapefile/mtbs_perimeter_data.zip"
  destinationDirectoryZip <- paste0(destinationDirectory, ".zip")
  download.file(dataSourceLink, destinationDirectoryZip)
  unzip(destinationDirectoryZip, exdir = destinationDirectory)
  unlink(destinationDirectoryZip)
  assert_that(file.exists(MTBS_download))
}

# Read in Data ----------------------------------------------------------
# mtbs data
mtbs <- st_read(dsn = "./data/mtbs_perimeter_data/", layer = "mtbs_perims_DD")

#combined wildland fire 
cwf_layers <- st_layers(dsn = "./data/combinedWildlandFireDB/Fire_Feature_Data_Pro2_8_Geodatabase/Fire_Feature_Data.gdb/")
cwf <- st_read(dsn = "./data/combinedWildlandFireDB/Fire_Feature_Data_Pro2_8_Geodatabase/Fire_Feature_Data.gdb/", layer = "USGS_Wildland_Fire_Combined_Dataset")
 # transform crs to match mtbs
cwf <- cwf %>% 
  st_transform(st_crs(mtbs)) %>% 
  st_sf() %>% 
  st_cast()

## fix those geometries that are "multisurface"
cwf_multi <- cwf[st_geometry_type(cwf) == "MULTISURFACE",]
cwf_nonMulti <- cwf[st_geometry_type(cwf) != "MULTISURFACE",]

cwf_multi_fixed <- cwf_multi %>% 
  surf_to_poly()

cwf_new <- cwf_multi_fixed %>% 
  rbind(cwf_nonMulti) 



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
  st_intersection(CONUS_states)

# switch off spherical geometry
sf_use_s2(FALSE)

cwf_conus <- cwf_new %>% 
  st_make_valid() %>% 
  st_intersection(st_make_valid(CONUS_states))


# get AIM plots 
plotDat <- st_read("./data/DataForAnalysisPoints/", "vegCompPoints") %>% 
  st_transform(st_crs(mtbs))


# Identify overlaps between fire boundaries and plots --------------------------
rm(us_states, CONUS_states, cwf_layers, cwf_multi, cwf_multi_fixed, cwf_nonMulti, cwf_new, mtbs, cwf)
gc()

## divide the plot data into sections and put them into a list
plotDat$burnedMoreThan20YearsAgo <- FALSE
plotDat$group <- rep(1:700, 1000)[1:nrow(plotDat)]
plotDat_list <- plotDat %>% 
  base::split(f = plotDat$group)

plotDat_noMtbsList <- lapply(names(plotDat_list), 
                 FUN = function(y) {
                   print(y)
                   z <- plotDat_list[[y]]
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
                       if (plotNow$Year < fireYear) {# if the plot burned after it was sampled, say we can keep this plot
                         print(paste0("keep plot w/ the index ",x[2]," because it burned in ", fireYear, " and was sampled in ", plotNow$Year))
                       } else if ((plotNow$Year-fireYear) > 20) { # if the plot burned more than 20 years ago
                         print(paste0("label plot w/ the index", x[2]," as burn historically because it burned in ", fireYear, ", at least 20 years before sampling in ", plotNow$Year))
                       } else {
                         print(paste0("REMOVE plot w/ the index ", x[2]," because it burned in ", fireYear, ", which was soon before sampling in ", plotNow$Year))
                       }
                     }) # end of internal apply function
                     
                     # remove the plots that were burned less than 20 years before sampling
                     plotDat_noFire <- z[!(row(z)[,1] %in% 
                                             unique(as.numeric(overlapOut[str_detect(overlapOut, "REMOVE")] %>% 
                                                                 str_extract(pattern = "[0-9]+")))),]
                     # label the plots that were burned more than 20 years before sampling
                     plotDat_noFire[(row(plotDat_noFire)[,1] %in% 
                                             unique(as.numeric(overlapOut[str_detect(overlapOut, "label")] %>% 
                                                                 str_extract(pattern = "[0-9]+")))),]$burnedMoreThan20YearsAgo <- TRUE
                   } else {
                     plotDat_noFire <- z
                   }
                   return(plotDat_noFire)
                   }) # end of lapply

names(plotDat_noMtbsList) <- as.character(c(1:length(plotDat_noMtbsList)))
## do the same filtering for cwf data 
plotDat_noCWFList <- lapply(names(plotDat_noMtbsList), 
               FUN = function(y) {
                 print(y)
                 z <- plotDat_noMtbsList[[y]]
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
                     if (plotNow$Year < fireYear) {# if the plot burned after it was sampled, say we can keep this plot
                       print(paste0("keep plot w/ the index ",x[2]," because it burned in ", fireYear, " and was sampled in ", plotNow$Year))
                     } else if ((plotNow$Year - fireYear) > 20) { # if the plot burned more than 20 years ago
                       print(paste0("label plot w/ the index ", x[2],"as burned historically because it burned in ", fireYear, ", at least 20 years before sampling in ", plotNow$Year))
                       } else {
                       print(paste0("REMOVE plot w/ the index ", x[2]," because it burned in ", fireYear, ", which was soon before sampling in ", plotNow$Year))
                     }
                   }) # end of internal apply function
                   
                   # remove the plots that were burned less than 20 years before sampling
                   plotDat_noFire <- z[!(row(z)[,1] %in% 
                                           unique(as.numeric(overlapOut[str_detect(overlapOut, "REMOVE")] %>% 
                                                               str_extract(pattern = "[0-9]+")))),]
                   # label the plots that were burned more than 20 years before sampling
                   plotDat_noFire[(row(plotDat_noFire)[,1] %in% 
                                     unique(as.numeric(overlapOut[str_detect(overlapOut, "label")] %>% 
                                                         str_extract(pattern = "[0-9]+")))),]$burnedMoreThan20YearsAgo <- TRUE
                   
                 } else {
                   plotDat_noFire <- z
                 }
                 return(plotDat_noFire)
               }) # end of lapply
 

plotDat_noFireAll <- purrr::reduce(plotDat_noCWFList, rbind)

saveRDS(plotDat_noFireAll, file = "./data/dataForAnalysis_fireRemoved.rds")
