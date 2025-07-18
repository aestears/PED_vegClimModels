#///////////////////////////
# Preparing data from FIA to be used in subsequent biomass ~ climate relationships
# Alice Stears
# 30 September 2024
#///////////////////////////

# load packages -----------------------------------------------------------

library(tidyverse)
library(terra)
library(sf)
library(tidyterra)

# get metadata/plot-level info ---------------------------------------------------------------
#First, get 
file <- "./Data_raw/FIA/CSV_FIADB_ENTIRE/"

# get state codes
stateCodes <- FIESTA::ref_statecd %>% 
  rename(STATECD = VALUE, STATENAME = MEANING, STATEABB = ABBR) %>% 
  select(-c(RS, RSCD, REGION, REGION_SPGRPCD))

#plot table gives location of plot, m/d/y of sampling
PLOT <- read.csv(paste0(file, "ENTIRE_PLOT.csv"))

#The Conditions table (COND) gives you forest type, slope, aspect, disturbance
#codes and treatment codes (filter for sites without fire or thinning treatments
# --a "condition" is a stand, so there can be multiple conditions within the same
#plot, although there can also only be one
#-- have to look at the help manual to get the key for what codes actually
#mean), and other plot-level things
#-- want to filter out water: (COND_STATUS_CD) exclude codes #3 ('noncensus water') and #4 ('census water')
#--want to filter by disturbance code (DSTRBCD...) -- exclude fire (general fire = 30, ground fire damage = 31)?
#exclude grazing? (#46); exclude disease damage (understory = #21, trees =
##22)?; human damage (#80)?; landslide (#91); avalanche track (#92); volcanic
#blast zone (#93)
#-- we probably also want to filter by "treatment" (TRTCD__ column): 10 = cutting; 
# 20 = "site preparation" (slashing, burning, etc.); 
# 30 = artificial regeneration after treatment; 40 = natural regeneration after treatment; 
# 50 = other silvicultural practices
#-- should also maybe exclude things based on PRESNFCD column (present nonforest code)
# 10 = agricultural land; 11 = cropland; 12 = improved pasture; 31 = cultural; 30 = developed;
# 32 = rights of way
COND_temp <- read.csv(paste0(file, "ENTIRE_COND.csv")) 
#filter out unwanted plots/conditions
COND_temp2 <- COND_temp %>% 
  filter(!(COND_STATUS_CD %in% c(3,4))) %>% # filter out water
  filter(!(DSTRBCD1 %in% c(30, 31, 46, 21, 22, 80, 91, 92, 93))) %>% 
  filter(!(DSTRBCD2 %in% c(30, 31, 46, 21, 22, 80, 91, 92, 93))) %>% 
  filter(!(DSTRBCD3 %in% c(30, 31, 46, 21, 22, 80, 91, 92, 93))) %>% # remove disturbances
  filter(!(TRTCD1 %in% c(10, 20, 30, 40, 50))) %>% 
  filter(!(TRTCD2 %in% c(10, 20, 30, 40, 50))) %>% 
  filter(!(TRTCD3 %in% c(10, 20, 30, 40, 50))) %>% # remove forest treatments
  filter(!(PRESNFCD %in% c(10, 11, 12, 31, 30, 32))) # remove agricultural uses

# add in lat long information for plots from PLOT dataset
COND <- COND_temp2 %>% 
  left_join(PLOT[,c("STATECD", "UNITCD", "COUNTYCD", "PLOT", "INVYR", "LAT", "LON")]) %>% 
  left_join(stateCodes) %>% # add state names
  filter(!(STATEABB %in% c("VI", "PR", "PW", "MP", "MH", "GU", "FM", "AS", "HI", "AK"))) # remove data for Alaska and islands


# Get tree biomass data from the TREE table -------------------------------
# previously generated in "./Analysis/VegComposition/DataPrep/01_FIA_DataWrangling.R"
TREE <- readRDS(file = "./Data_raw/FIA/TREEtable.RDS")

# add group information for TREEs that are "Tree broadleaf" and "Tree evergreen"
TREE[TREE$SCIENTIFIC_NAME == "Tree broadleaf", "group"] <- "Angiosperms"
TREE[TREE$SCIENTIFIC_NAME == "Tree evergreen", "group"] <- "Gymnosperms"
TREE[TREE$SCIENTIFIC_NAME == "Tree unknown", "group"] <- "Unknown"


# group by subplot (values summed across each subplot, grouped by species group)
TREE_SubPlot <- TREE %>% 
  mutate(basalArea_in2 = pi*(DIA/2)^2) %>% # calculate the basal area of each tree (in square inches)
  group_by(PLT_CN, INVYR, STATECD, UNITCD, COUNTYCD, PLOT, SUBP, CONDID, STATUSCD, group) %>% 
  summarize(HeightAvg_ft = mean(HT),
            DiaAvg_in = mean(DIA),
            basalAreaSum_in2 = sum(basalArea_in2, na.rm = TRUE),
            Carbon_AG_subPlot_lbPerAcre = sum(CARBON_AG), # no na.rm in this or follow rows, 
            #since there are usually NAs for biomass values when there is not a 
            #species name... so those plots would have an incomplete plot-level 
            #sum of plot-level biomass if we were to remove NAs 
            Carbon_BG_subPlot_lbPerAcre = sum(CARBON_BG)/0.0415417126, 
            DryBio_stem_subPlot_lbPerAcre = sum(DRYBIO_STEM)/0.0415417126, # sum of biomass in a subplot: biomass in lbs per subplot (which is 1809.557 ft^2, or 0.0415417126 acres)
            DryBio_foliage_subPlot_lbPerAcre = sum(DRYBIO_FOLIAGE)/0.0415417126, # sum of biomass in a subplot: biomass in lbs  per subplot (which is 1809.557 ft^2, or 0.0415417126 acres), so change to lbs/acre
            DryBio_branch_subPlot_lbPerAcre = sum(DRYBIO_BRANCH)/0.0415417126 # sum of biomass in a subplot:biomass in lbs  per subplot (which is 1809.557 ft^2, or 0.0415417126 acres), so change to lbs/acre
  )

# group by plot (values averaged within each plot--averages of summed values in each subplot w/in a plot--, grouped by species group, units: lb/acre)
TREE_Plot <- TREE_SubPlot %>% 
  group_by(PLT_CN, INVYR, STATECD, UNITCD, COUNTYCD, PLOT, CONDID, STATUSCD, group) %>% 
  summarize(Height_subpAvg_plotAvg_ft = mean(HeightAvg_ft),
            Dia_subpAvg_plotAvg_in = mean(DiaAvg_in),
            basalArea_subpSum_plotAvg_in2 = mean(basalAreaSum_in2, na.rm = TRUE),
            Carbon_AG_subpSum_plotAvg = mean(Carbon_AG_subPlot_lbPerAcre), 
            Carbon_BG_subpSum_plotAvg = mean(Carbon_BG_subPlot_lbPerAcre), 
            DryBio_stem_subpSum_plotAvg = mean(DryBio_stem_subPlot_lbPerAcre), 
            DryBio_foliage_subpSum_plotAvg = mean(DryBio_foliage_subPlot_lbPerAcre),
            DryBio_branch_subpSum_plotAvg = mean(DryBio_branch_subPlot_lbPerAcre)
  ) %>% ## add location information and filter for plots we don't want
  mutate(PLT_CN = as.double(PLT_CN)) %>% 
  left_join(COND[,c("PLT_CN", "INVYR", "STATECD", "UNITCD", "COUNTYCD", "PLOT", 
                    "CONDID", "PCTBARE_RMRS", "SLOPE", "ASPECT", "STATENAME", "LAT", "LON")]) %>% 
  filter(!is.na(LAT))

TREE_Plot %>% 
filter(INVYR == 2012 & !is.na(Carbon_AG_subpSum_plotAvg)) %>% 
  ggplot() + 
  geom_point(aes(x = LON, y = LAT, col = Carbon_AG_subpSum_plotAvg))

## make into a simpler format for us to use
TREE_use <- TREE_Plot %>% 
  ungroup() %>% 
  filter(STATUSCD == 1) %>% # remove dead trees 
  select(PLT_CN, INVYR, STATECD, UNITCD, COUNTYCD, PLOT, CONDID, group, LAT, 
         LON, basalArea_subpSum_plotAvg_in2, 
         Carbon_AG_subpSum_plotAvg, Carbon_BG_subpSum_plotAvg, 
         DryBio_stem_subpSum_plotAvg, 
         DryBio_foliage_subpSum_plotAvg,
         DryBio_branch_subpSum_plotAvg) %>% # select only basal area variable
  pivot_wider(values_from = c(basalArea_subpSum_plotAvg_in2:DryBio_branch_subpSum_plotAvg), names_from = group) 
  # rename("basalArea_Angiosperms_in2" = "Angiosperms", 
  #        "basalArea_Gymnosperms_in2" = "Gymnosperms",
  #        "basalArea_UnknownGroup_in2" = "Unknown",
  #        "basalArea_Pteridophytes_in2" = "Pteridophytes"
  # ) %>% 
  ## the basal area values in TREE_Plots d.fs do not have zeros, so the NAs in these biomass columns are true zeros 



# calculate total plot-level basal area (averaged across subplots))
TREE_use$basalArea_allGroups_in2 = rowSums(TREE_use[,c("basalArea_subpSum_plotAvg_in2_Angiosperms", 
                                                       "basalArea_subpSum_plotAvg_in2_Gymnosperms", 
                                                       "basalArea_subpSum_plotAvg_in2_Unknown", 
                                                       "basalArea_subpSum_plotAvg_in2_Pteridophytes")], na.rm = TRUE)

# calculate the proportion of the biomass that corresponds to each functional group 
TREE_use$basalArea_Angiosperms_perc = TREE_use$basalArea_subpSum_plotAvg_in2_Angiosperms/TREE_use$basalArea_allGroups_in2*100
TREE_use$basalArea_Gymnosperms_perc = TREE_use$basalArea_subpSum_plotAvg_in2_Gymnosperms/TREE_use$basalArea_allGroups_in2*100
TREE_use$basalArea_UnknownGroup_perc = TREE_use$basalArea_subpSum_plotAvg_in2_Unknown/TREE_use$basalArea_allGroups_in2*100
TREE_use$basalArea_Pteridophytes_perc = TREE_use$basalArea_subpSum_plotAvg_in2_Pteridophytes/TREE_use$basalArea_allGroups_in2*100

plot(TREE_use$LON, TREE_use$LAT, col = as.factor(TREE_use$basalArea_UnknownGroup_perc))
ggplot(TREE_use) +
  geom_point(aes(LON, LAT, col = basalArea_allGroups_in2))

# Get vegetation cover data -----------------------------------------------
# previously generated in "./Analysis/VegComposition/DataPrep/05_prepDataForModels.R" 
#(includes weather data, and burned plots have been filtered out)
vegDat <- readRDS(file = "./Data_processed/CoverData/DataForModels.RDS")

# Add cover and biomass data together -------------------------------------

biomassCoverDat <- TREE_use %>% 
  mutate(StateUnitCode =paste0(STATECD, "_", UNITCD, "_", COUNTYCD)) %>% 
    full_join((vegDat %>% filter(Source == "FIA"))
            , by = c("StateUnitCode" = "SttUntC", "PLOT" = "Plot", "CONDID" = "PltCndt", "INVYR" = "Year")) %>% 
## get just the columns that we need for tree biomass and tree cover 
select(UniquID, StateUnitCode, INVYR, Month, Day, Lat, Lon, 
       Carbon_AG_subpSum_plotAvg_Angiosperms, Carbon_AG_subpSum_plotAvg_Gymnosperms,
       Carbon_BG_subpSum_plotAvg_Angiosperms, Carbon_BG_subpSum_plotAvg_Gymnosperms,
       DryBio_stem_subpSum_plotAvg_Angiosperms, DryBio_stem_subpSum_plotAvg_Gymnosperms,
       DryBio_foliage_subpSum_plotAvg_Angiosperms, DryBio_foliage_subpSum_plotAvg_Gymnosperms,
       DryBio_branch_subpSum_plotAvg_Angiosperms, DryBio_branch_subpSum_plotAvg_Gymnosperms,
       AngTrCv, CnfTrCv,
       TtlTrCv, burnedMoreThan20YearsAgo, #annVPD_mean:
       geometry) %>% 
  # remove data for any plots that don't have tree cover data (since that's what we're interested in)
filter(!is.na(TtlTrCv)) %>% 
  # because we're working with tree data, remove data for any plots that have burned at all 
filter(burnedMoreThan20YearsAgo == FALSE) %>% 
  mutate(Carbon_AG_trees = pmap_dbl(.[8:9], sum, na.rm = TRUE),
         Carbon_BG_trees =  pmap_dbl(.[10:11], sum, na.rm = TRUE),
         DryBio_stem_trees =  pmap_dbl(.[12:13], sum, na.rm = TRUE),
         DryBio_foliage_trees =  pmap_dbl(.[14:15], sum, na.rm = TRUE),
         DryBio_branch_trees =  pmap_dbl(.[16:17], sum, na.rm = TRUE))

## save the data 
saveRDS(biomassCoverDat, "./Data_processed/BiomassQuantityData/TreeBiomassCover_withWeatherAndFireFiltering.rds")
biomassCoverDat <- readRDS("./Data_processed/BiomassQuantityData/TreeBiomassCover_withWeatherAndFireFiltering.rds")

# Visualize the spread of the cover/biomass data --------------------------
# get veg data
biomassCoverDat <- st_sf(biomassCoverDat, crs = "GEOGCRS[\"NAD83\",\n    DATUM[\"North American Datum 1983\",\n        ELLIPSOID[\"GRS 1980\",6378137,298.257222101,\n            LENGTHUNIT[\"metre\",1]]],\n    PRIMEM[\"Greenwich\",0,\n        ANGLEUNIT[\"degree\",0.0174532925199433]],\n    CS[ellipsoidal,2],\n        AXIS[\"geodetic latitude (Lat)\",north,\n            ORDER[1],\n            ANGLEUNIT[\"degree\",0.0174532925199433]],\n        AXIS[\"geodetic longitude (Lon)\",east,\n            ORDER[2],\n            ANGLEUNIT[\"degree\",0.0174532925199433]],\n    ID[\"EPSG\",4269]]")
# dayMet extent 
test <-  rast("./Data_raw/dayMet/rawMonthlyData/orders/70e0da02b9d2d6e8faa8c97d211f3546/Daymet_Monthly_V4R1/data/daymet_v4_prcp_monttl_na_1980.tif") %>% 
  terra::project(crs(biomassCoverDat))
# add metData to the veg dat 
# make veg data projection the same as raster data
v <- vect(st_drop_geometry(biomassCoverDat)[,c("Lon", "Lat")], geom = c("Lon", "Lat"), crs=crs(biomassCoverDat))
y <- project(v, crs(test))
# make sure the veg data is in the appropriate projection
biomassCoverDat <- biomassCoverDat %>%
  #st_buffer(.01) %>%
  #terra::vect() #%>%
  sf::st_transform(crs(y)) %>% 
  # st_buffer(.1)
  st_centroid() %>% 
  mutate("Year_char" = as.character(INVYR)) %>% 
  select(INVYR, Year_char, UniquID, Month, Day, Lat:DryBio_branch_trees)


# make sure the raster data is in the appropriate projection
test <- test %>%
  terra::project(crs(y))
st_crs(biomassCoverDat) == st_crs(test)

## visualize above ground carbon data
biomassCoverDat_temp <- biomassCoverDat %>% 
  select(c("INVYR", "DryBio_foliage_trees", "geometry")) %>% 
  st_as_sf() %>% 
  st_cast("POINT")

years <-as.matrix(unique(biomassCoverDat_temp$INVYR))
biomassCoverDat_rast <- apply(years, MARGIN = 1, function(x) {
  rast <- terra::rasterize(terra::vect(biomassCoverDat_temp[biomassCoverDat_temp$INVYR == x,]), field = "DryBio_foliage_trees",
                   y = test, fun = mean, na.rm = TRUE) %>% 
    terra::aggregate(fact = 12, fun = "mean", na.rm = TRUE) %>% 
    terra::crop(ext(-2000000, 2500000, -2000000, 1200000))
  return(rast)
})
names(biomassCoverDat_rast) <- years
biomassCoverDat_rast <- biomassCoverDat_rast[order(years)]
biomassCoverDat_rastAll <- rast(biomassCoverDat_rast)

par(mfrow = c(1,1))
ggplot() + 
  geom_spatraster(data = biomassCoverDat_rastAll) + 
  ggtitle("Tree foliar biomass") + 
  scale_fill_viridis_c(option = "turbo", na.value = "white") + 
  facet_wrap(~lyr)

## visualize stem + foliage + branch biomass ( a lot of plots don't have stem and branch, only foliage, which is why there are fewer data points)
biomassDryBio_temp <- biomassCoverDat %>% 
  mutate(DryBio_stemFoliageBranch =(DryBio_stem_trees + DryBio_foliage_trees + DryBio_branch_trees)) %>% 
  select(INVYR, DryBio_stemFoliageBranch, geometry) %>% 
  st_as_sf() %>% 
  st_cast("POINT")

years <-as.matrix(unique(biomassDryBio_temp$INVYR))

biomassDryBio_rast <- apply(years, MARGIN = 1, function(x) {
  rast <- terra::rasterize(terra::vect(biomassDryBio_temp[biomassDryBio_temp$INVYR == x,]), 
                           field = "DryBio_stemFoliageBranch",
                           y = test, fun = mean, na.rm = TRUE) %>% 
    terra::aggregate(fact = 12, fun = "mean", na.rm = TRUE) %>% 
    terra::crop(ext(-2000000, 2500000, -2000000, 1200000))
  return(rast)
})
names(biomassDryBio_rast) <- years
biomassDryBio_rast <- biomassDryBio_rast[order(years)]
biomassDryBio_rast <- rast(biomassDryBio_rast)

ggplot() + 
  geom_spatraster(data = biomassDryBio_rast) + 
  ggtitle("Tree biomass in stem + foliage + branch data from FIA") + 
  scale_fill_viridis_c(option = "turbo", na.value = "white") + 
  facet_wrap(~lyr)



# Get Tiled Biomass Data --------------------------------------------------
FIAbiomass <- st_read(dsn = "./Data_raw/FIA_Forest_Biomass_Estimates_1873/data/CONUSbiohex2020/", layer = "CONUSbiohex2020")
# get dayMet extent
# dayMet extent 
test <-  rast("./Data_raw/dayMet/rawMonthlyData/orders/70e0da02b9d2d6e8faa8c97d211f3546/Daymet_Monthly_V4R1/data/daymet_v4_prcp_monttl_na_1980.tif") 

# change the crs of the FIA biomass to be the same as dayMet
FIAbiomass_2 <- FIAbiomass %>% 
  sf::st_transform(crs = crs(test)) %>% 
  select(#CRM_LIVE, DRYBIOT_LI, 
         JENK_LIVE, EST_SAMPLE, AVG_INVYR) %>% 
  # rasterize
  terra::vect() %>% 
  terra::rasterize(y = test, field = "JENK_LIVE", fun = "mean")

FIAbiomass_years <- FIAbiomass %>% 
  sf::st_transform(crs = crs(test)) %>% 
  select(#CRM_LIVE, DRYBIOT_LI, 
    JENK_LIVE, EST_SAMPLE, AVG_INVYR) %>% 
  # rasterize
  terra::vect() %>% 
  terra::rasterize(y = test, field = "AVG_INVYR", fun = "mean")

FIAbiomass_2 <- c(FIAbiomass_2, FIAbiomass_years)
  
## fields we want 
# "CRM_LIVE"	CRM_LIVE	Mg ha-1	Aboveground biomass of live trees (≥2.54 cm diameter) on forest land per hectare of sampled area using FIA component ratio method (CRM).
# "DRYBIOT_LI"	DRYBIOT_LIVE	Mg ha-1	Aboveground biomass of live trees (≥2.54 cm diameter) on forest land per hectare of sampled area using retired FIA regional methods.
# "JENK_LIVE"	JENK_LIVE	Mg ha-1	Aboveground biomass of live trees (≥2.54 cm diameter) on forest land per hectare of sampled area using the Jenkins equations.
# "EST_SAMPLE"	EST_SAMPLED_HA	ha	Sampled hectares in the hexagon.
# "AVG_INVYR"	AVG_INVYR	year	Average of year of field sampling

## I think we want the Jenkins equations?? https://research.fs.usda.gov/treesearch/42790 indicates they are better than CRM? 

## save
terra::writeRaster(FIAbiomass_2, filename = "./Data_processed/BiomassQuantityData/FIA_biomassRaster.tif")


