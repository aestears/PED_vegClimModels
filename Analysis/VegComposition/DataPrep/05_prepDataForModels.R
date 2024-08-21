#///////////////////
# Fitting models to vegetation composition data
# Alice Stears
# 6/26/24
#///////////////////


# load packages -----------------------------------------------------------

library(tidyverse)
library(terra)
library(statmod)
#library(betareg)
library(glmmTMB)
library(corrplot)
library(sf)
#library(StepBeta)
#library(lmtest)

# load data ---------------------------------------------------------------

# load vegetation data
vegDat <- readRDS("./data/dataForAnalysis_fireRemoved.rds") 
# load meteorological data
dayMet <- readRDS("./data/dayMet/climateValuesForAnalysis_final.rds")
# remove monthly values 
dayMet2 <- dayMet %>% 
  select(-names(dayMet)[c(4:24)])
# make dayMet spatial 
dayMet3 <- dayMet2 %>% 
  st_as_sf(coords = c("Long", "Lat"))

# add met data to veg data ------------------------------------------------
# make veg data projection the same as raster data
test <-  rast("./data/dayMet/rawMonthlyData/orders/70e0da02b9d2d6e8faa8c97d211f3546/Daymet_Monthly_V4R1/data/daymet_v4_prcp_monttl_na_1980.tif")
#points_sf <- st_read(dsn = "./data/DataForAnalysisPoints/", layer = "vegCompPoints")
v <- vect(st_drop_geometry(vegDat)[,c("Lon", "Lat")], geom = c("Lon", "Lat"), crs=crs(vegDat))
y <- project(v, crs(test)) 
vegDat[,c("Lon", "Lat")] <- st_coordinates(st_as_sf(y))
plot(vegDat$Lon, vegDat$Lat)
vegDat2 <- st_as_sf(vegDat, coords = c("Lon", "Lat"))
vegDat2 <- vegDat2 %>% 
 # st_set_crs(value = st_crs(y)) %>% 
  st_transform(st_crs(y))

dayMet3 <- dayMet3 %>% 
  st_set_crs(value = st_crs(y)) %>% 
  st_transform(st_crs(y))

allDat <- vegDat2 %>% 
  st_buffer(.1) %>% 
  sf::st_join(dayMet3, by = c("Year" = "year", left = TRUE)) %>% 
  filter(Year == year)

#AES missing 600 points... not too sure why?? address later (after ESA)
# 
# # plot those that have missing data ... not sure why? there are points that don't have corresponding climate data
# good <- allDat[!is.na(allDat$tmax_annAvg),]
# 
# bad <- allDat[is.na(allDat$tmax_annAvg),]
# ## add in the clim data for rounded lat/long points
# bad <- bad %>% 
#   select(-names(bad)[25:82]) %>% 
#   mutate(Lat = round(Lat, 1),
#          Lon = round(Lon, 1)) %>% 
#   left_join((climVar %>% 
#                mutate(Lat = round(Lat, 1),
#                       Long = round(Long, 1))), by = c("Year" = "year", "Lon" = "Long", "Lat" = "Lat")) %>% 
#   unique()
# ## are still some messed up variables...will need to figure out later
# badBad <- bad[!is.na(bad$tmin_annAvg),]
# 
# allDat <- rbind(bad, good)


# Convert %covers to proportions of cover for each class ------------------

# fix names
allDat$Lon <- st_coordinates(allDat)[,1]
allDat$Lat <- st_coordinates(allDat)[,2]

allDat  <- allDat %>% 
  mutate(Lon = st_coordinates(st_centroid(allDat))[,1],
         Lat = st_coordinates(st_centroid(allDat))[,2]) %>% 
  st_drop_geometry() %>% 
  rename(UniqueID = UniquID, 
         StateUnitCounty = SttUntC,
         PlotCondition = PltCndt, 
         ShrubCover = ShrbCvr,
         HerbCover = HerbCvr,
         TotalGramCover = TtlGrmC,
         AnnualHerbGramCover = AnnlHGC, 
         PerennialHerbGramCover = PrnnHGC, 
         C3GramCover = C3GrmCv,
         C4GramCover = C4GrmCv,
         AngioTreeCover = AngTrCv, 
         ConifTreeCover = CnfTrCv,
         TotalTreeCover = TtlTrCv, 
         TreeBasalArea = TrBsA_2,
         BareGroundCover = BrGrndC, 
         LitterCover = LttrCvr, 
         LitterDepth = LttrDpt)
  
allDat$TotalCover <- rowSums(allDat[,c("ShrubCover", "HerbCover", "TotalGramCover", "TotalTreeCover")])
allDat <- allDat %>% 
  mutate(ShrubProportion = ShrubCover/TotalCover,
         HerbProportion = HerbCover/TotalCover,
         TotalGramProportion = TotalGramCover/TotalCover, 
         C3GramProportion = C3GramCover/TotalCover,
         C4GramProportion = C4GramCover/TotalCover,
         TotalTreeProportion = TotalTreeCover/TotalCover, 
         ConifTreeProportion = ConifTreeCover/TotalCover, 
         AngioTreeProportion = AngioTreeCover/TotalCover,
         AnnualHerbGramProportion = AnnualHerbGramCover/TotalCover,
         PerennialHerbGramCoverProportion = PerennialHerbGramCover/TotalCover
         )

modDat <- allDat %>% 
  mutate(ShrubCover_dec = ShrubCover/100,
         HerbCover_dec = HerbCover/100,
         TotalGramCover_dec = TotalGramCover/100,
         C3GramCover_dec = C3GramCover/100,
         C4GramCover_dec = C4GramCover/100,
         TotalTreeCover_dec = TotalTreeCover/100,
         ConifTreeCover_dec = ConifTreeCover/100,
         AngioTreeCover_dec = AngioTreeCover/100, 
         AnnualHerbGram_dec = AnnualHerbGramCover/100, 
         PerennialHerbGram_dec = PerennialHerbGramCover/100
         ) %>% 
  st_drop_geometry() 

#adjust to have no 0s and 1s (so glm w/ beta distribution works)
modDat[modDat$ShrubCover_dec == 0 & !is.na(modDat$ShrubCover_dec), "ShrubCover_dec"] <- .0001
modDat[modDat$HerbCover_dec == 0 & !is.na(modDat$HerbCover_dec), "HerbCover_dec"]  <- .0001
modDat[modDat$TotalGramCover_dec == 0 & !is.na(modDat$TotalGramCover_dec),"TotalGramCover_dec"]  <- .0001
modDat[modDat$C3GramCover_dec == 0 & !is.na(modDat$C3GramCover_dec), "C3GramCover_dec"]  <- .0001
modDat[modDat$C4GramCover_dec == 0 & !is.na(modDat$C4GramCover_dec), "C4GramCover_dec"]  <- .0001
modDat[modDat$TotalTreeCover_dec == 0 & !is.na(modDat$TotalTreeCover_dec), "TotalTreeCover_dec"]  <- .0001
modDat[modDat$ConifTreeCover_dec == 0 & !is.na(modDat$ConifTreeCover_dec), "ConifTreeCover_dec"]  <- .0001
modDat[modDat$AngioTreeCover_dec == 0 & !is.na(modDat$AngioTreeCover_dec), "AngioTreeCover_dec"]  <- .0001
modDat[modDat$AnnualHerbGram_dec == 0 & !is.na(modDat$AnnualHerbGram_dec), "AnnualHerbGram_dec"]  <- .0001
modDat[modDat$PerennialHerbGram_dec == 0 & !is.na(modDat$PerennialHerbGram_dec), "PerennialHerbGram_dec"]  <- .0001


modDat[(modDat$ShrubCover_dec==1 | (modDat$ShrubCover_dec>1 & modDat$ShrubCover_dec<1.5)) & !is.na(modDat$ShrubCover_dec), "ShrubCover_dec"] <- .999 
modDat[(modDat$HerbCover_dec==1 | (modDat$HerbCover_dec>1 & modDat$HerbCover_dec<1.5)) & !is.na(modDat$HerbCover_dec), "HerbCover_dec"] <- .999 
modDat[(modDat$TotalGramCover_dec==1 | (modDat$TotalGramCover_dec>1 & modDat$TotalGramCover_dec<1.5)) & !is.na(modDat$TotalGramCover_dec), "TotalGramCover_dec"] <- .999 
modDat[(modDat$C3GramCover_dec==1 | (modDat$C3GramCover_dec>1 & modDat$C3GramCover_dec<1.5)) & !is.na(modDat$C3GramCover_dec), "C3GramCover_dec"] <- .999 
modDat[(modDat$C4GramCover_dec==1 | (modDat$C4GramCover_dec>1 & modDat$C4GramCover_dec<1.5)) & !is.na(modDat$C4GramCover_dec), "C4GramCover_dec"] <- .999 
modDat[(modDat$TotalTreeCover_dec==1 | (modDat$TotalTreeCover_dec>1 & modDat$TotalTreeCover_dec<1.5)) & !is.na(modDat$TotalTreeCover_dec), "TotalTreeCover_dec"] <- .999 
modDat[(modDat$ConifTreeCover_dec==1 | (modDat$ConifTreeCover_dec>1 & modDat$ConifTreeCover_dec<1.5)) & !is.na(modDat$ConifTreeCover_dec), "ConifTreeCover_dec"] <- .999 
modDat[(modDat$AngioTreeCover_dec==1 | (modDat$AngioTreeCover_dec>1 & modDat$AngioTreeCover_dec<1.5)) & !is.na(modDat$AngioTreeCover_dec), "AngioTreeCover_dec"] <- .999
modDat[(modDat$AnnualHerbGram_dec==1 | (modDat$AnnualHerbGram_dec>1 & modDat$AnnualHerbGram_dec<1.5)) & !is.na(modDat$AnnualHerbGram_dec), "AnnualHerbGram_dec"] <- .999
modDat[(modDat$PerennialHerbGram_dec==1 | (modDat$PerennialHerbGram_dec>1 & modDat$PerennialHerbGram_dec<1.5)) & !is.na(modDat$PerennialHerbGram_dec), "PerennialHerbGram_dec"] <- .999 

modDat[modDat$ShrubCover_dec >= 1 & !is.na(modDat$ShrubCover), "ShrubCover_dec"] <- NA
modDat[modDat$HerbCover_dec>=1 & !is.na(modDat$HerbCover), "HerbCover_dec"] <- NA
modDat[modDat$TotalGramCover_dec>=1  & !is.na(modDat$TotalGramCover), "TotalGramCover_dec"] <- NA
modDat[modDat$C3GramCover_dec>=1 & !is.na(modDat$C3GramCover), "C3GramCover_dec"] <- NA
modDat[modDat$C4GramCover_dec>=1 & !is.na(modDat$C4GramCover), "C4GramCover_dec"] <- NA
modDat[modDat$TotalTreeCover_dec>=1  & !is.na(modDat$TotalTreeCover), "TotalTreeCover_dec"] <- NA
modDat[modDat$ConifTreeCover_dec>=1 & !is.na(modDat$ConifTreeCover), "ConifTreeCover_dec"] <- NA
modDat[modDat$AngioTreeCover_dec>=1   & !is.na(modDat$AngioTreeCover), "AngioTreeCover_dec"] <- NA
modDat[modDat$AnnualHerbGram_dec>=1   & !is.na(modDat$AnnualHerbGram), "AnnualHerbGram_dec"] <- NA
modDat[modDat$PerennialHerbGram_dec>=1   & !is.na(modDat$PerennialHerbGram), "PerennialHerbGram_dec"] <- NA


ggplot(data = modDat) + 
  geom_density(aes(ShrubCover_dec), col = "red")+ 
  geom_density(aes(HerbCover_dec), col = "orange")+ 
  geom_density(aes(TotalGramCover_dec), col = "yellow")+ 
  geom_density(aes(C3GramCover_dec), col = "green")+ 
  geom_density(aes(C4GramCover_dec), col = "blue")+ 
  geom_density(aes(TotalTreeCover_dec), col = "purple")+ 
  geom_density(aes(ConifTreeCover_dec), col = "brown")+ 
  geom_density(aes(AngioTreeCover_dec), col = "grey")

## save data for further analysis 
saveRDS(modDat, file = "./data/DataForModels.RDS")
