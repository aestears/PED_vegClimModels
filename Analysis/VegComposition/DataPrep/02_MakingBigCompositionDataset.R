#///////////////////
# Combining vegetation composition data from different sources into one dataset
# Alice Stears
# 6/7/24
#///////////////////


# load packages -----------------------------------------------------------

library(tidyverse)
library(sf)
library(maps)

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
         Month = NA,
         Day = NA,
         Year = INVYR,
         Lat = LAT, 
         Lon = LON,
         ShrubCover = Shrub_AerialCover,
         HerbCover = Forbs_AerialCover,
         AnnualHerbGramCover = NA,
         PerennialHerbGramCover = NA,
         TotalGramCover = Graminoid_AerialCover,
         C3GramCover = NA, 
         C4GramCover = NA, 
         AngioTreeCover = TallyTree_AerialCover * basalArea_Angiosperms_perc/100, 
         ConifTreeCover = TallyTree_AerialCover * basalArea_Gymnosperms_perc/100,
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
  select(UniqueID, StateUnitCounty, Plot,  PlotCondition, Month, Day, Year, Lat,  Lon,
         ShrubCover, HerbCover,  TotalGramCover, AnnualHerbGramCover, PerennialHerbGramCover,
         C3GramCover,  C4GramCover, 
         AngioTreeCover ,   ConifTreeCover, TotalTreeCover, TreeBasalArea_in2, 
         BareGroundCover,   LitterCover,  LitterDepth, Source)
# load LANDFIRE data ------------------------------------------------------

LANDFIRE_veg <- read.csv("./data/LANDFIRE_LFRDB/coverDat_USE.csv") %>% 
  mutate(Source = "LANDFIRE")
## remove values that don't have dates
LANDFIRE_veg <- 
  LANDFIRE_veg %>% 
  filter(!is.na(YYYY)) 

# select columns we want
LANDFIRE_all <- LANDFIRE_veg %>% 
  mutate(date = as.POSIXct(paste0(MM,"-", DD, "-",YYYY), tz = "UTC", format = "%m-%d-%Y")) %>% 
  transmute(UniqueID = EventID, 
         StateUnitCounty = NA,
         Plot = NA,
         PlotCondition = NA, 
         Month = MM,
         Day = DD,
         Year = YYYY,
         Lat = Lat,
         Lon = Long,
         ShrubCover = LFShrubCov, 
         HerbCover = LFHerbCov, 
         AnnualHerbGramCover = NA,
         PerennialHerbGramCover = NA,
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

# ggplot(LANDFIRE_all[LANDFIRE_all$TotalGramCover < 150,]) +
#   geom_point(aes(Lon, Lat, col = TotalGramCover))

# load Landscape Data Commons data ----------------------------------------

LDC_veg <- read.csv("./data/LandscapeDataCommonsDat/coverDat_use.csv") %>% 
  mutate(Source = "LDC") %>% 
  filter(!is.na(DateVisited))

# select columns we want
LDC_all <- LDC_veg %>% 
  mutate(date = as.POSIXct(DateVisited, tz = "UTC", format = "%Y-%m-%d")) %>% 
  #select(-LFX, -LFY, -LFCoordSys, -LFZone, -YYYY, -MM, -DD, -DDD) %>% 
  transmute(UniqueID = PrimaryKey, 
            StateUnitCounty = NA,
            Plot = NA,
            PlotCondition = NA, 
            Month = lubridate::month(date),
            Day = lubridate::day(date),
            Year = lubridate::year(DateVisited),
            Lat = Latitude_NAD83,
            Lon = Longitude_NAD83,
            ShrubCover = AH_ShrubCover, 
            HerbCover = AH_ForbCover, 
            AnnualHerbGramCover = NA,
            PerennialHerbGramCover = NA,
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

# ggplot(FIA_all) + 
#    geom_point(aes(Lon, Lat, col = HerbCover))

# load RAP data -----------------------------------------------------------
RAP_all <- read.csv("./data/RAP_samplePoints/RAPdata_use.csv") %>% 
  mutate(Year = date, 
         Month = NA, 
         Day = NA) %>% 
  select(UniqueID, StateUnitCounty, Plot,  PlotCondition, Month, Day, Year, Lat,  Lon,
         ShrubCover, HerbCover,  TotalGramCover, AnnualHerbGramCover, PerennialHerbGramCover,
         C3GramCover,  C4GramCover, 
         AngioTreeCover ,   ConifTreeCover, TotalTreeCover, TreeBasalArea_in2, 
         BareGroundCover,   LitterCover,  LitterDepth, Source)


# add datasets together ---------------------------------------------------
dat_all <- FIA_all %>% 
  rbind(LANDFIRE_all, LDC_all, RAP_all)
## save dataset for further analysis
#write.csv(dat_all, file = "./data/DataForAnalysis.csv", row.names = FALSE)
dat_all <- read.csv("./data/DataForAnalysis.csv")

## make a shapefile of the sample points also and save
dat_all_sf_full <- st_as_sf(dat_all, coords = c("Lon", "Lat")) %>% 
  select(geometry) %>% 
  #unique() %>% 
  sf::st_set_crs("EPSG:4326") %>% 
  cbind(dat_all)
dat_all_sf <- dat_all_sf_full %>% 
  unique()

#st_write(dat_all_sf, dsn = "./data/DataForAnalysisPoints", layer = "vegCompPoints", driver = "ESRI Shapefile", append = FALSE)


## trim data to only that collected after 1979 
dat_all <- dat_all %>% 
  filter(Year > 1978)
# plot of all data points
plot(dat_all$Lon, dat_all$Lat, col = as.factor(dat_all$Source))
ggplot(dat_all) +
  geom_point(aes(Lon, Lat, col = Source), alpha = .5)

baseMap <- st_as_sf(map("state", plot = FALSE, fill = TRUE), crs = st_crs(dat_all_new)) %>% 
  rename("geometry" = "geom")

# plot AIM data 
pdf("./figures/LDC_dataextent.pdf")
dat_all_new %>% 
  filter(Source == "LDC") %>% 
  ggplot() +
  geom_sf(data = baseMap) + 
  geom_sf(alpha = .5, col = "cornflowerblue") +
  ggtitle("Landscape Data Commons (AIM + a bit more)", 
          subtitle = "Cov vars: angio. trees, gymno. trees, shrubs, herbs, C3 grams, C4 grams, bare ground, litter") + 
  theme_minimal()
dev.off()

# plot FIA data 
pdf("./figures/FIA_dataextent.pdf")
dat_all_new %>% 
  filter(Source == "FIA") %>% 
  ggplot() +
  geom_sf(data = baseMap) + 
  geom_sf(alpha = .5, col = "tomato") +
  ggtitle("Forest Inventory and Analysis plots", 
          subtitle = "Cover variables: angio. trees, gymno. trees, shrubs, herbs, grams, (litter), (bare ground)") + 
  theme_minimal()
dev.off()

# plot LANDFIRE data 
pdf("./figures/LANDFIRE_dataextent.pdf")
dat_all_new %>% 
  filter(Source == "LANDFIRE") %>% 
  ggplot() +
  geom_sf(data = baseMap) + 
  geom_sf(alpha = .5, col = "forestgreen") +
  ggtitle("LANDFIRE reference database plots", 
          subtitle = "Cover variables: angio. trees, gymno. trees, shrubs, herbs, C3 grams, C4 grams") + 
  theme_minimal()
dev.off()

# plot LANDFIRE data 
pdf("./figures/RAP_dataextent.pdf")
dat_all_new %>% 
  filter(Source == "RAP") %>% 
  ggplot() +
  geom_sf(data = baseMap) + 
  geom_sf(alpha = .3, col = "orchid") +
  ggtitle("RAP points -- 300,000 point sampled from undeveloped, non-ag. area", 
          subtitle = "Cover variables: all trees, shrubs, ann. herbaceous, perenn. herbaceous, bare ground, litter") + 
  theme_minimal()
dev.off()


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

