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
         basalArea_Angiosperms_perc, basalArea_Gymnosperms_perc) %>%  #remove columns we don't need
  transmute(UniqueID = PLT_CN, 
         StateUnitCountyPlot = paste0(STATECD, "_", UNITCD, "_", COUNTYCD),
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
         BareGroundCover = NA, 
         LitterCover = NA
         ) # update/change some names

#AES need to add in litter and ground cover data 
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
         Long = Long,
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
         LitterCover = NA
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
            Long = Longitude_NAD83,
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
            LitterCover = FH_TotalLitterCover
              ) 

# add datasets together ---------------------------------------------------


