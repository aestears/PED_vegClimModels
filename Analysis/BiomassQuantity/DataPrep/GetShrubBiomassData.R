#///////////////////
# Preparing for and Acquiring shrub bioass data
# Alice Stears
# 10/31/24
#///////////////////


# load packages -----------------------------------------------------------

library(tidyverse)


# load cover data ---------------------------------------------------------

coverDat <- readRDS("./Data_processed/CoverData/DataForModels.rds")


# find plots w/ high amount of shrub cover --------------------------------
shrubCov <- coverDat[coverDat$ShrubCover>80 & !is.na(coverDat$ShrubCover),]
#11,640 data points
table(shrubCov$Source)

ggplot(shrubCov) + 
  geom_point(aes(x = Lon, y=Lat, col = ShrubCover)) + 
  facet_wrap(~Year) + 
  ggtitle("Locations with >80% absolute shrub cover by year", 
          subtitle = "11,640 plots total -- FIA: 1,555; LANDFIRE: 10,027; AIM: 51, RAP: 7")
  
shrubCov_high <- coverDat[coverDat$ShrubCover>90 & !is.na(coverDat$ShrubCover),]
#7,531 data points
table(shrubCov_high$Source)

ggplot(shrubCov_high) + 
  geom_point(aes(x = Lon, y=Lat, col = ShrubCover)) + 
  facet_wrap(~Year) + 
  ggtitle("Locations with >90% absolute shrub cover by year", 
          subtitle = "7,531 plots total -- FIA: 513; LANDFIRE: 7,009; AIM: 9")
