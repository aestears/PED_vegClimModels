#///////////////////
# Filtering AIM plots to remove those that burned recently
# Alice Stears
# 7/11/24
# code for acquiring fire permiters and overlaying them on AIM plots adapted from G. Siegmund 
#///////////////////


# load packages -----------------------------------------------------------
library(sbtools)
library(here)

# Download combined wildland fire dataset ---------------------------------
# https://www.sciencebase.gov/catalog/item/61707c2ad34ea36449a6b066
##
## script uses sbtools https://github.com/DOI-USGS/sbtools
## a package for interfacing to ScienceBase
## training module: https://owi.usgs.gov/R/training-curriculum/usgs-packages/sbtools-intro/index.html
## downloading the data requires a ScienceBase account 

# - Destination folders ----

# use here to get current directory
here::i_am("Analysis/VegComposition/FilterBurnedAIMPoints.R")

# input where you will download the Zip files from Science Base
# as well as the unzipped data files
destinationDirectory <- here::here("data","combinedWildlandFireDB")

# - Authenticate to ScienceBase ----
# uses new authentication method in sbtools>=1.3.1
initialize_sciencebase_session()

# - Query for the data release  ----
# use the most recent data release version 
results <- query_sb_text("Combined wildland fire datasets for the United States and certain territories")

# - +input the item Science Base item ID ----
combinedWildlandFire_ID <- "61707c2ad34ea36449a6b066"
combinedWildlandFirePolygon_ID <- "61aa537dd34eb622f699df81"
combinedWildlandFireRaster_ID <- "61aa5483d34eb622f699df85"
identifier_exists(combinedWildlandFire_ID)

# - Download item  ----
item_file_download(combinedWildlandFire_ID,dest_dir=destinationDirectory)
item_file_download(combinedWildlandFirePolygon_ID,dest_dir=destinationDirectory)
# item_file_download(combinedWildlandFireRaster_ID,dest_dir=destinationDirectory)

