#///////////////////
# Importing and exploring potential data sources -- NEON cover data
# Alice Stears
# 04/29/2024
#///////////////////


# load packages -----------------------------------------------------------

library(tidyverse)
library(remotes)
#remotes::install_github("admahood/neonPlantEcology")
library(neonPlantEcology)


# Load data ---------------------------------------------------------------

neon <- neonPlantEcology::npe_site_ids(by = 'all')
neon_dat <-  neonPlantEcology::npe_download(sites = neonPlantEcology::npe_site_ids(by = 'all'))
 # I think we get cover from "div_1m2Data" sub-list--data from 1m2 subplots
neon_1mDat <- neon_dat$div_1m2Data
plotSites <- unique(neon_1mDat[,c("decimalLatitude","decimalLongitude")])
mapview(plotSites, xcol = "decimalLongitude", ycol = "decimalLatitude", crs = "epsg:4326")
## adds a tiny bit of data...but not much

# save 1m plot data
saveRDS(neon_1mDat, file = "./data/neon/1m2Data.rds")
