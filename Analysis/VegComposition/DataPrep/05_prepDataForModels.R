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
vegDat <- readRDS("./Data_processed/CoverData/dataForAnalysis_fireRemoved.rds") 
# load meteorological data
# dayMet <- readRDS("./Data_processed/CoverData/dayMetClimateValuesForAnalysis_final.rds")
# # # remove monthly values 
# # dayMet2 <- dayMet %>% 
# #   select(-names(dayMet)[c(4:22)])
# # make dayMet spatial 
# dayMet3 <- dayMet %>% 
#   st_as_sf(coords = c("Long", "Lat"))
# 

# Add ecoregion data  -----------------------------------------------------
regions <- sf::st_read(dsn = "./Data_raw/Level2Ecoregions/", layer = "NA_CEC_Eco_Level2") 

regions_2 <- st_transform(regions, crs = st_crs(vegDat)) %>% 
  st_make_valid()

crs(regions_2) == crs(vegDat)

#mapview(regions_2 %>% slice_sample(n = 10000)) + mapview(modDat_2 %>% slice_sample(n = 10000))

# match the ecoregion to point data ---------------------------------------
vegDat_3 <- sf::st_join(vegDat, regions_2)

# missing for places right on the coast... but not a big deal to not have those

# group into coarser ecoregions -------------------------------------------
# make a lookup table
# (eastern forest) put eastern temperate forests, tropical wet forests and northern forests together
# (dry shrub and grass) put great plains, southern semiarid highlands, Mediterranean California and north american deserts together 
# (western forest) put northwestern forested mountains, marine west coast forests together, and temperate sierras together

ecoReg_lu <- data.frame("NA_L1NAME" = sort(unique(vegDat_3$NA_L1NAME)), 
                        "newRegion" = c("eastForest", "dryShrubGrass", "westForest",
                                        "dryShrubGrass", "dryShrubGrass", "eastForest",
                                        "westForest", "dryShrubGrass", "westForest", 
                                        "eastForest", NA
                        ))

# add to main data.frame 
newDat <- vegDat_3 %>% 
  left_join(ecoReg_lu) %>% 
  mutate(newRegion = as.factor(newRegion), 
         rowID = 1:nrow(vegDat_3)) 
# 
# ggplot(newDat[1:100000,]) + 
#   geom_sf(aes(fill = newRegion))



# For forest sites, remove data from RAP  ---------------------------------
rowsToRemove <- newDat[newDat$Source == "RAP" & newDat$newRegion %in% c("westForest", "eastForest"), ]

newDat2 <- newDat[!(newDat$rowID %in% rowsToRemove$rowID),]

# # add met data to veg data ------------------------------------------------
# # make veg data projection the same as raster data
# test <-  terra::rast("./Data_raw/dayMet/rawMonthlyData/orders/70e0da02b9d2d6e8faa8c97d211f3546/Daymet_Monthly_V4R1/data/daymet_v4_prcp_monttl_na_1980.tif")
# #points_sf <- st_read(dsn = "./data/DataForAnalysisPoints/", layer = "vegCompPoints")
# v <- vect(st_drop_geometry(vegDat)[,c("Lon", "Lat")], geom = c("Lon", "Lat"), crs=crs(vegDat))
# y <- project(v, crs(test)) 
# #vegDat[,c("Lon", "Lat")] <- st_coordinates(st_as_sf(y))
# #plot(vegDat$Lon, vegDat$Lat)
# vegDat2 <- st_as_sf(vegDat, coords = c("Lon", "Lat"))
# vegDat2 <- vegDat2 %>% 
#  # st_set_crs(value = st_crs(y)) %>% 
#   st_transform(st_crs(y))
# 
# dayMet3 <- dayMet3 %>% 
#   st_set_crs(value = st_crs(y)) %>% 
#   st_transform(st_crs(y))
# 
# rm(dayMet2, test, v, vegDat, y, dayMet)
# gc()
# 
# vegDat2 <- vegDat2 %>% 
#   mutate(vegRowID = c(1:nrow(vegDat2)))
# dayMet3 <- dayMet3 %>% 
#   mutate(dayMetRowID = c(1:nrow(dayMet3)))
#   
# vegDatTemp <- vegDat2 %>% 
#   select(geometry, Year, vegRowID)
# 
# dayMet3Temp <- dayMet3 %>% 
#   select(geometry, year, dayMetRowID) 
# 
# allDat <- vegDatTemp %>% 
#   st_buffer(.01) %>% 
#   sf::st_join(dayMet3Temp, by = c("Year" = "year"), left = TRUE#, join = st_nearest_feature
#               ) %>% 
#   filter(Year == year)
# 
# # add back in actual climate and veg data
# allDatNew <- allDat %>% 
#   left_join(vegDat2 %>% 
#               st_drop_geometry() %>% 
#               select(-Year), by = "vegRowID") %>% 
#   left_join((dayMet3 %>% 
#               st_drop_geometry() %>% 
#               select(-year)), by = "dayMetRowID") %>% 
#   filter(!duplicated(vegRowID))
#   
# allDat <- allDatNew  %>% 
#   rename(StateUnitCode = SttUntC, # fix names 
#          PlotCondition = PltCndt,
#          ShrubCover = ShrbCvr,
#          TotalTreeCover = TtlTrCv, 
#          TotalHerbaceousCover = TtlHrbC, 
#          CAMCover  = CAMCovr, 
#          BareGroundCover = BrGrndC, 
#          ForbCover_prop = FrbCvr_,
#          C3Cover_prop = C3GrmC_,
#          C4Cover_prop = C4GrmC_,
#          BroadleavedTreeCover_prop = AngTrC_,
#          NeedleLeavedTreeCover_prop = CnfTrC_,
#          #AnnualHerbaceousCover = AnnlHGC,
#          #PerennialHerbaceousCover = PrnnHGC,,
#          #C3GramCover = C3GrmCv,
#          #C4GramCover = C4GrmCv,
#          #BroadLeavedTreeCover = AngTrCv,
#          #NeedleLeavedTreeCover = CnfTrCv,
#          #TotalTreeCover = TtlTrCv,
#          TotalTreeBasalArea_m2 = TrBsA_2,
#          BareGroundCover = BrGrndC,
#          LitterCover = LttrCvr,
#          LitterDepth = LttrDpt,
#          #ForbCover_prop = HrbCvr_,
#          #C3GramCover_prop = C3GrmC_,
#          #C4GramCover_prop = C4GrmC_,
#          #BroadLeavedTreeCover_prop = AngTrC_,
#          #NeedleLeavedTreeCover_prop = CnfTrC_
#          ) 
#   # calculate proportion of C3/C4/Herb and Broad-leaved and Needle-leaved trees
#   
# 
# # Convert %covers to proportions of cover for each class ------------------
# 
# # fix names
# # allDat  <- allDat %>% 
# #   mutate(Lon = st_coordinates(st_centroid(allDat))[,1],
# #          Lat = st_coordinates(st_centroid(allDat))[,2]) %>% 
# #   st_drop_geometry() %>% 
# #   rename(UniqueID = UniquID, 
# #          StateUnitCounty = SttUntC,
# #          PlotCondition = PltCndt, 
# #          ShrubCover = ShrbCvr,
# #          HerbCover = HerbCvr,
# #          TotalGramCover = TtlGrmC,
# #          AnnualHerbGramCover = AnnlHGC, 
# #          PerennialHerbGramCover = PrnnHGC, 
# #          C3GramCover = C3GrmCv,
# #          C4GramCover = C4GrmCv,
# #          AngioTreeCover = AngTrCv, 
# #          ConifTreeCover = CnfTrCv,
# #          TotalTreeCover = TtlTrCv, 
# #          TreeBasalArea = TrBsA_2,
# #          BareGroundCover = BrGrndC, 
# #          LitterCover = LttrCvr, 
# #          LitterDepth = LttrDpt)
# #   
# # # allDat$TotalCover <- rowSums(allDat[,c("ShrubCover", "HerbCover", "TotalGramCover", "TotalTreeCover")])
# # # allDat <- allDat %>% 
# # #   mutate(ShrubProportion = ShrubCover/TotalCover,
# # #          HerbProportion = HerbCover/TotalCover,
# # #          TotalGramProportion = TotalGramCover/TotalCover, 
# # #          C3GramProportion = C3GramCover/TotalCover,
# # #          C4GramProportion = C4GramCover/TotalCover,
# # #          TotalTreeProportion = TotalTreeCover/TotalCover, 
# # #          ConifTreeProportion = ConifTreeCover/TotalCover, 
# # #          AngioTreeProportion = AngioTreeCover/TotalCover,
# # #          AnnualHerbGramProportion = AnnualHerbGramCover/TotalCover,
# # #          PerennialHerbGramCoverProportion = PerennialHerbGramCover/TotalCover
# # #          )
# # # 
# # # modDat <- allDat %>% 
# # #   mutate(ShrubCover_dec = ShrubCover/100,
# # #          HerbCover_dec = HerbCover/100,
# # #          TotalGramCover_dec = TotalGramCover/100,
# # #          C3GramCover_dec = C3GramCover/100,
# # #          C4GramCover_dec = C4GramCover/100,
# # #          TotalTreeCover_dec = TotalTreeCover/100,
# # #          ConifTreeCover_dec = ConifTreeCover/100,
# # #          AngioTreeCover_dec = AngioTreeCover/100, 
# # #          AnnualHerbGram_dec = AnnualHerbGramCover/100, 
# # #          PerennialHerbGram_dec = PerennialHerbGramCover/100,
# # #          BareGroundCover_dec = BareGroundCover/100,
# # #          LitterCover_dec = LitterCover/100
# # #          ) %>% 
# # #   st_drop_geometry() 
# # # 
# # # #adjust to have no 0s and 1s (so glm w/ beta distribution works)
# # # modDat[modDat$ShrubCover_dec == 0 & !is.na(modDat$ShrubCover_dec), "ShrubCover_dec"] <- .0001
# # # modDat[modDat$HerbCover_dec == 0 & !is.na(modDat$HerbCover_dec), "HerbCover_dec"]  <- .0001
# # # modDat[modDat$TotalGramCover_dec == 0 & !is.na(modDat$TotalGramCover_dec),"TotalGramCover_dec"]  <- .0001
# # # modDat[modDat$C3GramCover_dec == 0 & !is.na(modDat$C3GramCover_dec), "C3GramCover_dec"]  <- .0001
# # # modDat[modDat$C4GramCover_dec == 0 & !is.na(modDat$C4GramCover_dec), "C4GramCover_dec"]  <- .0001
# # # modDat[modDat$TotalTreeCover_dec == 0 & !is.na(modDat$TotalTreeCover_dec), "TotalTreeCover_dec"]  <- .0001
# # # modDat[modDat$ConifTreeCover_dec == 0 & !is.na(modDat$ConifTreeCover_dec), "ConifTreeCover_dec"]  <- .0001
# # # modDat[modDat$AngioTreeCover_dec == 0 & !is.na(modDat$AngioTreeCover_dec), "AngioTreeCover_dec"]  <- .0001
# # # modDat[modDat$AnnualHerbGram_dec == 0 & !is.na(modDat$AnnualHerbGram_dec), "AnnualHerbGram_dec"]  <- .0001
# # # modDat[modDat$PerennialHerbGram_dec == 0 & !is.na(modDat$PerennialHerbGram_dec), "PerennialHerbGram_dec"]  <- .0001
# # # modDat[modDat$BareGroundCover_dec == 0 & !is.na(modDat$BareGroundCover_dec), "BareGroundCover_dec"]  <- .0001
# # # modDat[modDat$LitterCover_dec == 0 & !is.na(modDat$LitterCover_dec), "LitterCover_dec"]  <- .0001
# # # 
# # # 
# # # modDat[(modDat$ShrubCover_dec==1 | (modDat$ShrubCover_dec>1 & modDat$ShrubCover_dec<1.5)) & !is.na(modDat$ShrubCover_dec), "ShrubCover_dec"] <- .999 
# # # modDat[(modDat$HerbCover_dec==1 | (modDat$HerbCover_dec>1 & modDat$HerbCover_dec<1.5)) & !is.na(modDat$HerbCover_dec), "HerbCover_dec"] <- .999 
# # # modDat[(modDat$TotalGramCover_dec==1 | (modDat$TotalGramCover_dec>1 & modDat$TotalGramCover_dec<1.5)) & !is.na(modDat$TotalGramCover_dec), "TotalGramCover_dec"] <- .999 
# # # modDat[(modDat$C3GramCover_dec==1 | (modDat$C3GramCover_dec>1 & modDat$C3GramCover_dec<1.5)) & !is.na(modDat$C3GramCover_dec), "C3GramCover_dec"] <- .999 
# # # modDat[(modDat$C4GramCover_dec==1 | (modDat$C4GramCover_dec>1 & modDat$C4GramCover_dec<1.5)) & !is.na(modDat$C4GramCover_dec), "C4GramCover_dec"] <- .999 
# # # modDat[(modDat$TotalTreeCover_dec==1 | (modDat$TotalTreeCover_dec>1 & modDat$TotalTreeCover_dec<1.5)) & !is.na(modDat$TotalTreeCover_dec), "TotalTreeCover_dec"] <- .999 
# # # modDat[(modDat$ConifTreeCover_dec==1 | (modDat$ConifTreeCover_dec>1 & modDat$ConifTreeCover_dec<1.5)) & !is.na(modDat$ConifTreeCover_dec), "ConifTreeCover_dec"] <- .999 
# # # modDat[(modDat$AngioTreeCover_dec==1 | (modDat$AngioTreeCover_dec>1 & modDat$AngioTreeCover_dec<1.5)) & !is.na(modDat$AngioTreeCover_dec), "AngioTreeCover_dec"] <- .999
# # # modDat[(modDat$AnnualHerbGram_dec==1 | (modDat$AnnualHerbGram_dec>1 & modDat$AnnualHerbGram_dec<1.5)) & !is.na(modDat$AnnualHerbGram_dec), "AnnualHerbGram_dec"] <- .999
# # # modDat[(modDat$PerennialHerbGram_dec==1 | (modDat$PerennialHerbGram_dec>1 & modDat$PerennialHerbGram_dec<1.5)) & !is.na(modDat$PerennialHerbGram_dec), "PerennialHerbGram_dec"] <- .999 
# # # modDat[(modDat$BareGroundCover_dec==1 | (modDat$BareGroundCover_dec>1 & modDat$BareGroundCover_dec<1.5)) & !is.na(modDat$BareGroundCover_dec), "BareGroundCover_dec"] <- .999 
# # # modDat[(modDat$LitterCover_dec==1 | (modDat$LitterCover_dec>1 & modDat$LitterCover_dec<1.5)) & !is.na(modDat$LitterCover_dec), "LitterCover_dec"] <- .999 
# # # 
# # # modDat[modDat$ShrubCover_dec >= 1 & !is.na(modDat$ShrubCover), "ShrubCover_dec"] <- NA
# # # modDat[modDat$HerbCover_dec>=1 & !is.na(modDat$HerbCover), "HerbCover_dec"] <- NA
# # # modDat[modDat$TotalGramCover_dec>=1  & !is.na(modDat$TotalGramCover), "TotalGramCover_dec"] <- NA
# # # modDat[modDat$C3GramCover_dec>=1 & !is.na(modDat$C3GramCover), "C3GramCover_dec"] <- NA
# # # modDat[modDat$C4GramCover_dec>=1 & !is.na(modDat$C4GramCover), "C4GramCover_dec"] <- NA
# # # modDat[modDat$TotalTreeCover_dec>=1  & !is.na(modDat$TotalTreeCover), "TotalTreeCover_dec"] <- NA
# # # modDat[modDat$ConifTreeCover_dec>=1 & !is.na(modDat$ConifTreeCover), "ConifTreeCover_dec"] <- NA
# # # modDat[modDat$AngioTreeCover_dec>=1   & !is.na(modDat$AngioTreeCover), "AngioTreeCover_dec"] <- NA
# # # modDat[modDat$AnnualHerbGram_dec>=1   & !is.na(modDat$AnnualHerbGram), "AnnualHerbGram_dec"] <- NA
# # # modDat[modDat$PerennialHerbGram_dec>=1   & !is.na(modDat$PerennialHerbGram), "PerennialHerbGram_dec"] <- NA
# # # modDat[modDat$BareGroundCover_dec>=1   & !is.na(modDat$BareGroundCover_dec), "BareGroundCover_dec"] <- NA
# # # modDat[modDat$LitterCover_dec>=1   & !is.na(modDat$LitterCover_dec), "LitterCover_dec"] <- NA
# # # 
# # 
# # ggplot(data = modDat) + 
# #   geom_density(aes(ShrubCover_dec), col = "red")+ 
# #   geom_density(aes(HerbCover_dec), col = "orange")+ 
# #   geom_density(aes(TotalGramCover_dec), col = "yellow")+ 
# #   geom_density(aes(C3GramCover_dec), col = "green")+ 
# #   geom_density(aes(C4GramCover_dec), col = "blue")+ 
# #   geom_density(aes(TotalTreeCover_dec), col = "purple")+ 
# #   geom_density(aes(ConifTreeCover_dec), col = "brown")+ 
# #   geom_density(aes(AngioTreeCover_dec), col = "grey") + 
# #   geom_density(aes(PerennialHerbGram_dec), col = "black") + 
# #   geom_density(aes(AnnualHerbGram_dec), col = "pink") + 
# #   geom_density(aes(BareGroundCover_dec), col = "turquoise") + 
# #   geom_density(aes(LitterCover_dec), col = "lightblue") 
# #   
# 
# ## save data for further analysis 
saveRDS(newDat2, file = "./Data_processed/CoverData/DataForModels.RDS")
