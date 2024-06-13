#///////////////////
# Combining vegetation composition data from different sources into one dataset
# Alice Stears
# 6/7/24
#///////////////////


# load packages -----------------------------------------------------------

library(tidyverse)


# load FIA data -----------------------------------------------------------

# vegetation data 
FIA_veg <- read.csv("./data/FIA/vegetationComposition_use.csv") %>% 
  mutate(Source = "FIA")
# ground cover data 
FIA_groundCover <- read.csv("./data/FIA/groundCover_use.csv")
# litter data 
FIA_litter <- read.csv("./data/FIA/LitterDuffFuel_use.csv")
# tree data
FIA_tree <- readRDS("./data/FIA/TREEtable_use.rds")

## add datasets together 
FIA_all <- FIA_veg %>% 
  left_join(FIA_groundCover) %>% 
  left_join(FIA_litter) %>% 
  left_join(FIA_tree) %>% 
  select(PLT_CN, STATECD, UNITCD, COUNTYCD, PLOT, INVYR, CONDID, Forbs_AerialCover, 
         Graminoid_AerialCover, Shrub_AerialCover, TallyTree_AerialCover, 
         NonTallyTree_AerialCover, PCTBARE_RMRS, SLOPE, ASPECT, STATENAME, 
         LAT, LON, Source, Litter_PctCover, BareGround_PctCover, MEASYEAR, 
         DuffDepth, LitterDepth, FuelDepth, basalArea_allGroups_in2,
         basalArea_Angiosperms_perc, basalArea_Gymnosperms_perc, LitterDepth) %>%  #remove columns we don't need
  transmute(UniqueID = PLT_CN, 
         StateUnitCounty = paste0(STATECD, "_", UNITCD, "_", COUNTYCD),
         Plot = PLOT, 
         PlotCondition = CONDID,
         date = INVYR, 
         Lat = LAT, 
         Lon = LON,
         ShrubCover = Shrub_AerialCover,
         HerbCover = Forbs_AerialCover,
         TotalGramCover = Graminoid_AerialCover,
         C3GramCover = NA, 
         C4GramCover = NA, 
         AngioTreeCover = TallyTree_AerialCover * basalArea_Angiosperms_perc, 
         ConifTreeCover = TallyTree_AerialCover * basalArea_Gymnosperms_perc,
         TotalTreeCover = TallyTree_AerialCover + NonTallyTree_AerialCover,
         TreeBasalArea_in2 = basalArea_allGroups_in2, 
         BareGroundCover_temp = BareGround_PctCover, 
         PCTBARE_RMRS = PCTBARE_RMRS, 
         LitterCover = Litter_PctCover,
         LitterDepth = LitterDepth,
         Source = "FIA"
         ) # update/change some names

# average columns for BareGroundCover and PCTBARE_RMRS
FIA_all$BareGroundCover  <- apply(FIA_all[,c("BareGroundCover_temp", "PCTBARE_RMRS")], MARGIN = 1, FUN = function(x) 
  mean(x, na.rm = TRUE))
FIA_all <- FIA_all %>% 
  select(-BareGroundCover_temp, -PCTBARE_RMRS) %>% 
  mutate(BareGroundCover = replace(BareGroundCover, BareGroundCover == 999, NA)) %>% 
  select(UniqueID, StateUnitCounty, Plot,  PlotCondition, date,  Lat,  Lon,
         ShrubCover, HerbCover,  TotalGramCover, C3GramCover,  C4GramCover, 
         AngioTreeCover ,   ConifTreeCover, TotalTreeCover, TreeBasalArea_in2, 
         BareGroundCover,   LitterCover,  LitterDepth, Source)
# load LANDFIRE data ------------------------------------------------------

LANDFIRE_veg <- read.csv("./data/LANDFIRE_LFRDB/coverDat_USE.csv") %>% 
  mutate(Source = "LANDFIRE")

# select columns we want
LANDFIRE_all <- LANDFIRE_veg %>% 
  mutate(date = as.POSIXct(paste0(MM,"-", DD, "-",YYYY), tz = "UTC", format = "%m-%d-%Y")) %>% 
  select(-LFX, -LFY, -LFCoordSys, -LFZone, -YYYY, -MM, -DD, -DDD) %>% 
  transmute(UniqueID = EventID, 
         StateUnitCounty = NA,
         Plot = NA,
         PlotCondition = NA, 
         date = date,
         Lat = Lat,
         Lon = Long,
         ShrubCover = LFShrubCov, 
         HerbCover = LFHerbCov, 
         TotalGramCover = C3_LFAbsCov + C4_LFAbsCov,
         C3GramCover = C3_LFAbsCov, 
         C4GramCover = C4_LFAbsCov, 
         AngioTreeCover = AngioTree_LFAbsCov, 
         ConifTreeCover = ConifTree_LFAbsCov,
         TotalTreeCover = AngioTree_LFAbsCov + ConifTree_LFAbsCov,
         TreeBasalArea_in2 = NA, 
         BareGroundCover = NA, 
         LitterCover = NA,
         LitterDepth = NA,
         Source = "LANDFIRE"
         ) 

# load Landscape Data Commons data ----------------------------------------

LDC_veg <- read.csv("./data/LandscapeDataCommonsDat/coverDat_use.csv") %>% 
  mutate(Source = "LDC")

# select columns we want
LDC_all <- LDC_veg %>% 
  mutate(date = as.POSIXct(DateVisited, tz = "UTC", format = "%Y-%m-%d")) %>% 
  #select(-LFX, -LFY, -LFCoordSys, -LFZone, -YYYY, -MM, -DD, -DDD) %>% 
  transmute(UniqueID = PrimaryKey, 
            StateUnitCounty = NA,
            Plot = NA,
            PlotCondition = NA, 
            date = date,
            Lat = Latitude_NAD83,
            Lon = Longitude_NAD83,
            ShrubCover = AH_ShrubCover, 
            HerbCover = AH_ForbCover, 
            TotalGramCover = AH_C3TotalCover + AH_C4TotalCover,
            C3GramCover = AH_C3TotalCover, 
            C4GramCover = AH_C4TotalCover, 
            AngioTreeCover = AH_AngioTotalCover, 
            ConifTreeCover = AH_ConifTotalCover,
            TotalTreeCover = AH_AngioTotalCover + AH_ConifTotalCover,
            TreeBasalArea_in2 = NA, 
            BareGroundCover = BareSoilCover, 
            LitterCover = FH_TotalLitterCover,
            LitterDepth = NA,
            Source = "LDC"
              ) 

# add datasets together ---------------------------------------------------
dat_all <- FIA_all %>% 
  rbind(LANDFIRE_all, LDC_all)

# plot of all data points
plot(dat_all$Lon, dat_all$Lat, col = as.factor(dat_all$Source))
ggplot(dat_all) +
  geom_point(aes(Lon, Lat, col = Source), alpha = .5)

# plot of data that has c3/c4 and broad leaf vs. conifer
dat_all %>% 
  filter(!is.na(ConifTreeCover) & 
           !is.na(AngioTreeCover) &
           !is.na(C3GramCover) & 
           !is.na(C4GramCover)) %>% 
  ggplot() +
  geom_point(aes(Lon, Lat, col = Source), alpha = .5) + 
  scale_color_manual(values = c("forestgreen", "cornflowerblue"))

# plot of data that have litter cover
dat_all %>% 
filter(!is.na(LitterCover)) %>% 
  ggplot() +
  geom_point(aes(Lon, Lat, col = Source), alpha = .5)

# plot of data that have litter depth
dat_all %>% 
  filter(!is.na(LitterDepth)) %>% 
  ggplot() +
  geom_point(aes(Lon, Lat, col = Source), alpha = .5) 

# plot of data that have bare ground cover
dat_all %>% 
  filter(!is.na(BareGroundCover)) %>% 
  ggplot() +
  geom_point(aes(Lon, Lat, col = Source), alpha = .5) 

## save dataset for further analysis
write.csv(dat_all, file = "./")
