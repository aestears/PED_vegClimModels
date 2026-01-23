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
FIA_veg <- read.csv("./Data_raw/FIA/vegetationComposition_use.csv") %>% 
  mutate(Source = "FIA")
# ground cover data 
FIA_groundCover <- read.csv("./Data_raw/FIA/groundCover_use.csv")
# litter data 
FIA_litter <- read.csv("./Data_raw/FIA/LitterDuffFuel_use.csv")
# tree data
FIA_tree <- readRDS("./Data_raw/FIA/TREEtable_use.rds")

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
         ForbCover = Forbs_AerialCover,
         AnnualHerbGramCover = NA,
         PerennialHerbGramCover = NA,
         TotalGramCover = Graminoid_AerialCover,
         C3GramCover = NA, 
         C4GramCover = NA, 
         AngioTreeCover = TallyTree_AerialCover * basalArea_Angiosperms_perc/100, 
         ConifTreeCover = TallyTree_AerialCover * basalArea_Gymnosperms_perc/100,
         TotalTreeCover = TallyTree_AerialCover, #+ NonTallyTree_AerialCover,
         #CAMCover = NA,
         TotalHerbaceousCover = Graminoid_AerialCover + Forbs_AerialCover,
         TreeBasalArea_in2 = basalArea_allGroups_in2, 
         BareGroundCover_temp = BareGround_PctCover, 
         PCTBARE_RMRS = PCTBARE_RMRS, 
         LitterCover = Litter_PctCover,
         LitterDepth = LitterDepth,
         Source = "FIA",
         AngioTreeCover_prop = basalArea_Angiosperms_perc/100,
         ConifTreeCover_prop = basalArea_Gymnosperms_perc/100
         )  %>% # update/change some names
  mutate(#AngioTreeCover_prop = AngioTreeCover/TotalTreeCover, 
         #ConifTreeCover_prop = ConifTreeCover/TotalTreeCover,
         C4GramCover_prop = C4GramCover/TotalHerbaceousCover,
         C3GramCover_prop = C3GramCover/TotalHerbaceousCover,
         ForbCover_prop = ForbCover/TotalHerbaceousCover)
# ggplot(FIA_all) +
#   geom_point(aes(Lon, Lat, col = AngioTreeCover))
# average columns for BareGroundCover and PCTBARE_RMRS
FIA_all$BareGroundCover  <- apply(FIA_all[,c("BareGroundCover_temp", "PCTBARE_RMRS")], MARGIN = 1, FUN = function(x) 
  mean(x, na.rm = TRUE))
FIA_all <- FIA_all %>% 
  select(-BareGroundCover_temp, -PCTBARE_RMRS) %>% 
  mutate(BareGroundCover = replace(BareGroundCover, BareGroundCover == 999, NA)) %>% 
  select(UniqueID, StateUnitCounty, Plot,  PlotCondition, Month, Day, Year, Lat,  Lon,
         ShrubCover, ForbCover,  TotalGramCover, AnnualHerbGramCover, PerennialHerbGramCover,
         C3GramCover,  C4GramCover, 
         AngioTreeCover ,   ConifTreeCover, TotalTreeCover, #CAMCover, 
         TotalHerbaceousCover, TreeBasalArea_in2, 
         BareGroundCover,   LitterCover,  LitterDepth, Source, AngioTreeCover_prop, 
         ConifTreeCover_prop, C4GramCover_prop,C3GramCover_prop, ForbCover_prop)

## trim values for cover to be only from 2011 or later 
FIA_all <- FIA_all %>% 
  filter(Year > 2010) 

# load LANDFIRE data ------------------------------------------------------

LANDFIRE_veg <- read.csv("./Data_raw//LANDFIRE_LFRDB/coverDat_USE.csv") %>% 
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
         ShrubCover = LFShrubCovAdj, 
         ForbCover = Forb_LFAbsCov,#LFHerbCovAdj * (Forb_LFAbsCov/100), ## the "LFHerbCovAdj" cover from LANDFIRE is herbaceous, not forbs! 
         AnnualHerbGramCover = NA,
         PerennialHerbGramCover = NA,
         TotalGramCover = NA,
         C3GramCover = C3_LFAbsCov,#((C3_LFAbsCov/100)*LFHerbCovAdj), # the proportion of herbaceous cover that is 
         C4GramCover = C4_LFAbsCov,#((C4_LFAbsCov/100)*LFHerbCovAdj), 
         AngioTreeCover = AngioTree_LFAbsCov,#(AngioTree_LFAbsCov/100)*LFTreeCovAdj, 
         ConifTreeCover = ConifTree_LFAbsCov,#(ConifTree_LFAbsCov/100)*LFTreeCovAdj,
         TotalTreeCover = LFTreeCovAdj,#AngioTree_LFRelCov + ConifTree_LFRelCov,
         #CAMCover = CAM_LFRelCov,
         #TotalHerbaceousCover_OLD = ((C3_LFRelCov/100)*LFHerbCovAdj) + ((C4_LFRelCov/100)*LFHerbCovAdj) + (LFHerbCovAdj * (Forb_LFRelCov/100)),
         TotalHerbaceousCover = LFHerbCovAdj,
         TreeBasalArea_in2 = NA, 
         BareGroundCover = NA, 
         LitterCover = NA,
         LitterDepth = NA,
         Source = "LANDFIRE",
         # LFHerbCovAdj = LFHerbCovAdj,
         #   C3_LFAbsCov, 
         #   C4_LFAbsCov,
         # Forb_LFAbsCov
         )  %>% # update/change some names
  mutate(AngioTreeCover_prop = AngioTreeCover/(AngioTreeCover + ConifTreeCover), 
         ConifTreeCover_prop = ConifTreeCover/(AngioTreeCover + ConifTreeCover),
         C4GramCover_prop = C4GramCover/(C4GramCover + C3GramCover + ForbCover),
         C3GramCover_prop = C3GramCover/(C4GramCover + C3GramCover + ForbCover),
         ForbCover_prop = ForbCover/(C4GramCover + C3GramCover + ForbCover))

# LANDFIRE_all %>% 
#   ggplot() + 
#   #xlim(c(0,100)) + 
#   geom_density(aes(TotalHerbaceousCover)) + 
#   geom_density(aes(ForbCover), col = "green") + 
#   geom_density(aes(C4GramCover), col = "red") + 
#   geom_density(aes(C3GramCover), col = "blue") + 
#   # geom_density(aes(TotalTreeCover)) + 
#   # geom_density(aes(AngioTreeCover), col = "green") + 
#   # geom_density(aes(ConifTreeCover), col = "red") + 
#   #geom_density(aes(ForbCover + C3GramCover + C4GramCover), col = "purple") #+
#   #geom_density(aes(LFHerbCovAdj), col = "purple", lty = 2) +
#   geom_density(aes((C3GramCover_prop )* 100), col = "red", lty = 2) + 
#   geom_density(aes((C4GramCover_prop)* 100), col = "blue", lty = 2) +
#   geom_density(aes((ForbCover_prop)*100), col = "purple", lty = 2)# +
#geom_density(aes((ForbCover_prop + C3GramCover_prop + C4GramCover_prop)*100), col = "grey", lty = 2)
  
# ggarrange(
# LANDFIRE_all %>%
#   ggplot() +
#   stat_summary_2d(aes(x = Lon, y = Lat, z = TotalHerbaceousCover), fun = mean, binwidth = .1) +
#   scale_fill_viridis_c(option = "A", guide = guide_colorbar(title = "% cover"),
#                        limits = c(0,100)) +
#   ggtitle("TotalHerbaceousCover"),
# LANDFIRE_all %>% 
#   ggplot() + 
#   stat_summary_2d(aes(x = Lon, y = Lat, z = C4GramCover), fun = mean, binwidth = .1) + 
#   scale_fill_viridis_c(option = "A", guide = guide_colorbar(title = "% cover"), 
#                        #limits = c(0,100)
#                        ) + 
#   ggtitle("ForbCover"),
# LANDFIRE_all %>% 
#   ggplot() + 
#   stat_summary_2d(aes(x = Lon, y = Lat, z = ForbCover_prop), fun = mean, binwidth = .1) + 
#   scale_fill_viridis_c(option = "A", guide = guide_colorbar(title = "% cover"), 
#                        limits = c(0,1)) + 
#   ggtitle("ForbCover_prop"),
# LANDFIRE_all %>% 
#   ggplot() + 
#   stat_summary_2d(aes(x = Lon, y = Lat, z = Forb_LFAbsCov), fun = mean, binwidth = .1) + 
#   scale_fill_viridis_c(option = "A", guide = guide_colorbar(title = "% cover"), 
#                        limits = c(0,100)) + 
#   ggtitle("Forb_LFAbsCov"),
# ncol = 1)
# 
# ggarrange(
#   LANDFIRE_all %>% 
#     ggplot() + 
#     stat_summary_2d(aes(x = Lon, y = Lat, z = ForbCover_prop), fun = mean, binwidth = .1) + 
#     scale_fill_viridis_c(option = "A", guide = guide_colorbar(title = "% cover"), 
#                          limits = c(0,1)) + 
#     ggtitle("ForbCover_prop"),
#   LANDFIRE_all %>% 
#     ggplot() + 
#     stat_summary_2d(aes(x = Lon, y = Lat, z = C3GramCover_prop), fun = mean, binwidth = .1) + 
#     scale_fill_viridis_c(option = "A", guide = guide_colorbar(title = "% cover"), 
#                          limits = c(0,1)) + 
#     ggtitle("C3GramCover_prop"),
#   LANDFIRE_all %>% 
#     ggplot() + 
#     stat_summary_2d(aes(x = Lon, y = Lat, z = C4GramCover_prop), fun = mean, binwidth = .1) + 
#     scale_fill_viridis_c(option = "A", guide = guide_colorbar(title = "% cover"), 
#                          limits = c(0,1)
#     ) + 
#     ggtitle("C4GramCover_prop"),
#   ncol = 1)

# landfire_long <- LANDFIRE_all %>% 
#   pivot_longer(cols = c(ShrubCover:ForbCover, TotalGramCover:CAMCover), 
#                names_to = "coverType", 
#                values_to = "relativeCover")
# ggplot(landfire_long) +
#   geom_histogram(aes(relativeCover, fill = coverType)) + 
#   facet_wrap(~coverType) + 
#   theme_minimal()
# load Landscape Data Commons data ----------------------------------------

LDC_veg <- read.csv("./Data_raw//LandscapeDataCommonsDat/coverDat_use.csv") %>% 
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
            ForbCover = AH_ForbCover, 
            AnnualHerbGramCover = NA,
            PerennialHerbGramCover = NA,
            TotalGramCover = TotalFoliarCover * (C3_hits_proportionOfAllSpecies + C4_hits_proportionOfAllSpecies),
            C3GramCover = TotalFoliarCover * C3_hits_proportionOfAllSpecies, 
            C4GramCover = TotalFoliarCover * C4_hits_proportionOfAllSpecies, 
            AngioTreeCover =  TotalFoliarCover * Angio_hits_proportionOfAllSpp, 
            ConifTreeCover = TotalFoliarCover * Conif_hits_proportionOfAllSpp,
            TotalTreeCover = TotalFoliarCover * (Angio_hits_proportionOfAllSpp + Conif_hits_proportionOfAllSpp),
            #CAMCover = TotalFoliarCover * cam_hits_proportionOfAllSpp,
            TotalHerbaceousCover = TotalFoliarCover * (C3_hits_proportionOfAllSpecies + C4_hits_proportionOfAllSpecies) + AH_ForbCover,
            TreeBasalArea_in2 = NA, 
            BareGroundCover = BareSoilCover, 
            LitterCover = FH_TotalLitterCover,
            LitterDepth = NA,
            Source = "LDC"
              )  %>% # update/change some names
  mutate(AngioTreeCover_prop = AngioTreeCover/TotalTreeCover, 
         ConifTreeCover_prop = ConifTreeCover/TotalTreeCover,
         C4GramCover_prop = C4GramCover/TotalHerbaceousCover,
         C3GramCover_prop = C3GramCover/TotalHerbaceousCover,
         ForbCover_prop = ForbCover/TotalHerbaceousCover)

# ggplot(FIA_all) + 
#    geom_point(aes(Lon, Lat, col = ForbCover))

# load RAP data -----------------------------------------------------------
RAP_all <- read.csv("./Data_raw/RAP_samplePoints/RAPdata_use.csv") %>% 
  mutate(Year = date, 
         Month = NA, 
         Day = NA, 
         CAMCover = NA, 
         TotalHerbaceousCover = AnnualHerbGramCover + PerennialHerbGramCover, 
         ForbCover = NA) %>% 
  select(UniqueID, StateUnitCounty, Plot,  PlotCondition, Month, Day, Year, Lat,  Lon,
         ShrubCover, ForbCover,  TotalGramCover, AnnualHerbGramCover, PerennialHerbGramCover,
         C3GramCover,  C4GramCover, 
         AngioTreeCover ,   ConifTreeCover, TotalTreeCover, TotalHerbaceousCover, #CAMCover, 
         TreeBasalArea_in2, 
         BareGroundCover,   LitterCover,  LitterDepth, Source)  %>% 
  mutate(AngioTreeCover_prop = AngioTreeCover/TotalTreeCover, 
         ConifTreeCover_prop = ConifTreeCover/TotalTreeCover,
         C4GramCover_prop = C4GramCover/TotalHerbaceousCover,
         C3GramCover_prop = C3GramCover/TotalHerbaceousCover,
         ForbCover_prop = ForbCover/TotalHerbaceousCover)



# fix coordinate reference systems of each data source so they're  the same --------
# transform the data into sf, and transform if necessary to WGS 84 ("EPSG: 4326")
FIA_all <- FIA_all %>% 
  st_as_sf(coords = c("Lon", "Lat"), crs = "EPSG:4269") %>% 
  st_transform(crs = "EPSG:4326")

LANDFIRE_all <- LANDFIRE_all %>% 
  sf::st_as_sf( coords = c("Lon", "Lat"), crs = "EPSG:4326")

RAP_all <- RAP_all %>% 
  sf::st_as_sf(coords = c("Lon", "Lat"), crs = "EPSG:4326")

LDC_all <- LDC_all %>% 
  sf::st_as_sf(coords = c("Lon", "Lat"), crs = "EPSG:4269") %>% 
  st_transform(crs = "EPSG:4326")
  
# update Lat and Lon
LDC_all <- LDC_all %>% 
  mutate(Lon = sf::st_coordinates(.)[,1],
         Lat = sf::st_coordinates(.)[,2])

RAP_all <- RAP_all %>% 
  mutate(Lon = sf::st_coordinates(.)[,1],
         Lat = sf::st_coordinates(.)[,2])

LANDFIRE_all <- LANDFIRE_all %>% 
  mutate(Lon = sf::st_coordinates(.)[,1],
         Lat = sf::st_coordinates(.)[,2])

FIA_all <- FIA_all %>% 
  mutate(Lon = sf::st_coordinates(.)[,1],
         Lat = sf::st_coordinates(.)[,2])

# ggplot(dat_all) + 
#   facet_wrap(~Source) + 
#   geom_sf(aes(col = TotalTreeCover))
# plot(LDC_all$Lon, LDC_all$Lat)
# points(RAP_all$Lon, RAP_all$Lat, col = "red")
# points(FIA_all$Lon, FIA_all$Lat, col = "green")
# points(LANDFIRE_all$Lon, LANDFIRE_all$Lat, col = "blue")
# add datasets together ---------------------------------------------------
dat_all <- FIA_all %>% 
  rbind(LANDFIRE_all, LDC_all, RAP_all)


# # Calculate Total Herbaceous Cover (where necessary) ----------------------
# dat_all <- dat_all %>% 
#   mutate(TotalHerbaceousCover_A =  pmap_dbl(.[c("AnnualHerbGramCover","PerennialHerbGramCover")], sum), # captures RAP data
#          TotalHerbaceousCover_B = pmap_dbl(.[c("ForbCover", "TotalGramCover")], sum)
#          ) %>% 
#   mutate(TotalHerbaceousCover = pmap_dbl(.[c("TotalHerbaceousCover_A", "TotalHerbaceousCover_B")], sum, na.rm = TRUE)) %>% 
#   select(-TotalHerbaceousCover_A, -TotalHerbaceousCover_B)
# 

## deal w/ the fact that the total herbaceous data is >100 in many cases
## for now, simply truncate to be max 100
dat_all[dat_all$TotalHerbaceousCover >100 & !is.na(dat_all$TotalHerbaceousCover), "TotalHerbaceousCover"] <- 100
hist(dat_all$TotalHerbaceousCover)

# Breaking herbaceous and trees into proportions ----------------------
# 
# dat_all$ConifTreeCover_prop <- NA
# 
# dat_all[dat_all$TotalTreeCover>0 & !is.na(dat_all$TotalTreeCover), "ConifTreeCover_prop"] <- 
#   dat_all[dat_all$TotalTreeCover>0 & !is.na(dat_all$TotalTreeCover),"ConifTreeCover"] /  
#   dat_all[dat_all$TotalTreeCover>0 & !is.na(dat_all$TotalTreeCover),"TotalTreeCover"]
# 
# 
# hist(dat_all$ConifTreeCover_prop)
# dat_all <- dat_all %>% 
#   mutate(ForbCover_prop = pmap_dbl(.[c("ForbCover", "TotalHerbaceousCover")], 
#                                    function(ForbCover, TotalHerbaceousCover, ...) {
#                                      ForbCover/TotalHerbaceousCover
#                                    } ),
#          C3GramCover_prop = pmap_dbl(.[c("C3GramCover", "TotalHerbaceousCover")], 
#                                      function(C3GramCover, TotalHerbaceousCover, ...) {
#                                        C3GramCover/TotalHerbaceousCover
#                                      } ),
#          C4GramCover_prop = pmap_dbl(.[c("C4GramCover", "TotalHerbaceousCover")], 
#                                     function(C4GramCover, TotalHerbaceousCover, ...) {
#                                       C4GramCover/TotalHerbaceousCover
#                                     } ),
#          AngioTreeCover_prop = pmap_dbl(.[c("AngioTreeCover", "TotalTreeCover")], 
#                                       function(AngioTreeCover, TotalTreeCover, ...) {
#                                         AngioTreeCover/TotalTreeCover
#                                       } ),
#          ConifTreeCover_prop = pmap_dbl(.[c("ConifTreeCover", "TotalTreeCover")], 
#                                         function(ConifTreeCover, TotalTreeCover, ...) {
#                                           ConifTreeCover/TotalTreeCover
#                                         } )
#          )


## If the total herbaceous cover is 0, we can assume that forb, graminoid, c3, and c4 cover is also 0
dat_all[dat_all$TotalHerbaceousCover == 0 & !is.na(dat_all$TotalHerbaceousCover), 
        c("ForbCover", "TotalGramCover","C3GramCover", "C4GramCover")] <- 0

## If the total tree cover is 0, we can assume that conif and braod leaved tree cover is also 0
dat_all[dat_all$TotalTreeCover == 0 & !is.na(dat_all$TotalTreeCover), 
        c("AngioTreeCover", "ConifTreeCover")] <- 0

## fix "NaN" values caused by instances where there are NO herbaceous or tree cover (is a divide by zero problem)
# also, change all values for proportion cover to NA when the corresponding umbrella variable is NA (can't have a proportion of no cover)
dat_all[dat_all$TotalTreeCover == 0 & !is.na(dat_all$TotalTreeCover) &  dat_all$AngioTreeCover ==0 & !is.na(dat_all$AngioTreeCover), "AngioTreeCover_prop"] <- NA

dat_all[dat_all$TotalTreeCover == 0 & !is.na(dat_all$TotalTreeCover) & dat_all$ConifTreeCover ==0 & !is.na(dat_all$ConifTreeCover), "ConifTreeCover_prop"] <- NA

dat_all[dat_all$TotalHerbaceousCover == 0 & !is.na(dat_all$TotalHerbaceousCover) &  dat_all$ForbCover ==0 & !is.na(dat_all$ForbCover), "ForbCover_prop"] <- NA

dat_all[dat_all$TotalHerbaceousCover == 0 & !is.na(dat_all$TotalHerbaceousCover) &  dat_all$C3GramCover ==0 & !is.na(dat_all$C3GramCover), "C3GramCover_prop"] <- NA

dat_all[dat_all$TotalHerbaceousCover == 0 & !is.na(dat_all$TotalHerbaceousCover) &  dat_all$C4GramCover ==0 & !is.na(dat_all$C4GramCover), "C4GramCover_prop"] <- NA


# make extra sure that, when Total tree Cover is NA, then associated level 2 proportion value is NA also 
dat_all[is.na(dat_all$TotalTreeCover) , "AngioTreeCover_prop"] <- NA

dat_all[is.na(dat_all$TotalTreeCover), "ConifTreeCover_prop"] <- NA

dat_all[is.na(dat_all$TotalHerbaceousCover), "ForbCover_prop"] <- NA

dat_all[is.na(dat_all$TotalHerbaceousCover), "C3GramCover_prop"] <- NA

dat_all[is.na(dat_all$TotalHerbaceousCover), "C4GramCover_prop"] <- NA

test <- dat_all$C4GramCover_prop + dat_all$C3GramCover_prop + dat_all$ForbCover_prop
hist(test)
test2 <- dat_all$AngioTreeCover_prop + dat_all$ConifTreeCover_prop
hist(test2)

dat_all %>% 
  filter(!is.na(TotalHerbaceousCover)) %>% 
  ggplot() + 
  facet_wrap(~Year) + 
  geom_point(aes(Lon, Lat))

## save dataset for further analysis
write.csv(dat_all %>% st_drop_geometry() , file = "./Data_processed/CoverData/ForAnalysis.csv", row.names = FALSE)
#dat_all <- read.csv("./Data_processed/CoverData/DataForAnalysis.csv")

## make a shapefile of the sample points also and save
# dat_all_sf_full <- st_as_sf(dat_all, coords = c("Lon", "Lat")) %>% 
#   select(geometry) %>% 
#   #unique() %>% 
#   sf::st_set_crs("EPSG:4326") %>% 
#   cbind(dat_all)
dat_all_sf <- dat_all

st_write(dat_all_sf, dsn = "./Data_processed/CoverData/DataForAnalysisPoints", layer = "vegCompPoints", driver = "ESRI Shapefile", append = FALSE)

