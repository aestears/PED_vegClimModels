#///////////////////
# Importing and exploring potential data sources - Forest Inventory and Analysis Data
# Alice Stears
# 04/10/2024
#///////////////////


# load packages -----------------------------------------------------------

library(tidyverse)
library(FIESTA)

# Read in data ------------------------------------------------------------

file <- "./data/FIA/CSV_FIADB_ENTIRE/"

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

## I think we get the actual cover data by species from the 
# "Phase 2 Vegetation Subplot Species Table" (P2VEG_SUBPLOT_SPP)
VEG_temp <- read.csv(paste0(file, "ENTIRE_P2VEG_SUBPLOT_SPP.csv"))
# subset this down to the plots/conditions/years we actually want (from the COND table)
VEG <- VEG_temp %>% 
  left_join(COND, by = c("INVYR", "STATECD", "UNITCD", "COUNTYCD", "PLOT", "CONDID")) %>% 
  filter(!is.na(STATENAME))
## there are only species-level data for 11 states ... 
# why? (CA, CO, AZ, ID, NM, MT, NV, OR, UT, WA and WY)

## Vegetation structure in each subplot (the same as above, but with cover by functional group/layer)
# this dataset has many more states (nearly all?)--will likely have to use this 
# then, which should be fine, although we'll have to break out tree types using the TREE table I guess...
VEG_fgroup <- read.csv(paste0(file, "ENTIRE_P2VEG_SUBP_STRUCTURE.csv"))
# make into a wide format (with a column for each functional group), aggregate
# by subplot, then average across subplots in a plot
VEG_fgroup_PlotAvgs <- VEG_fgroup %>% 
  # subset only to layer "5" (the "aerial cover" across all layers)
  filter(LAYER==5) %>% 
  select(-LAYER) %>% 
  pivot_wider(names_from = GROWTH_HABIT_CD, 
              values_from = COVER_PCT#,
              # names_glue = ("_AerialCover")
              ) %>% 
  group_by( PLT_CN, STATECD, UNITCD, COUNTYCD, PLOT, INVYR, SUBP, CONDID, 
            # don't want to use CN, since that is a unique # for each row in the 
            # veg structure species-level dataset
           MODIFIED_BY, MODIFIED_DATE, MODIFIED_IN_INSTANCE, CYCLE, SUBCYCLE) %>% 
  summarize(Forbs_AerialCover = sum(FB, na.rm = TRUE), 
            Graminoid_AerialCover = sum(GR, na.rm = TRUE),
            NonTallyTree_AerialCover = sum(NT, na.rm = TRUE), 
            Shrub_AerialCover = sum(SH, na.rm = TRUE),
            TallyTree_AerialCover = sum(TT, na.rm = TRUE)) %>% 
  group_by(PLT_CN, STATECD, UNITCD, COUNTYCD, PLOT, INVYR, CONDID, MODIFIED_BY, # group by plot (exclude the SUBP (subplot) column)
           MODIFIED_DATE, MODIFIED_IN_INSTANCE, CYCLE, SUBCYCLE) %>% 
  summarize(Forbs_AerialCover = mean(Forbs_AerialCover),  # average across subplots
            Graminoid_AerialCover = mean(Graminoid_AerialCover),
            NonTallyTree_AerialCover = mean(NonTallyTree_AerialCover), 
            Shrub_AerialCover = mean(Shrub_AerialCover),
            TallyTree_AerialCover = mean(TallyTree_AerialCover)) %>% 
  # add in the LatLong info from the COND dataset, and filter for the plots we want
 left_join(COND[,c("PLT_CN", "INVYR", "STATECD", "UNITCD", "COUNTYCD", "PLOT", 
                   "CONDID", "PCTBARE_RMRS", "SLOPE", "ASPECT", "STATENAME", "LAT", "LON")]) %>% 
  filter(!is.na(LAT))

# get litter data and add to plot-level veg. composition data -------------
# got some bare ground % cover information from COND$PCTBARE_RMRS (only for sites in the RMRS)
# get additional ground cover data from the "GRND_CVR" table (only from Oregon, California, and Washington)
GRND_CVR <- read.csv(paste0(file, "ENTIRE_GRND_CVR.csv"))
# turn into a wide dataset
GRND_CVR_PlotAvgs <- GRND_CVR %>% 
  pivot_wider(names_from = GRND_CVR_TYP,
              values_from = CVR_PCT)  %>% 
  group_by( PLT_CN, STATECD, UNITCD, COUNTYCD, PLOT, INVYR,
            # don't want to use CN, since that is a unique # for each row in the 
            # veg structure species-level dataset
            # also don't include subplot or transect, because we want to average values for the entire plot 
            MODIFIED_BY,  MODIFIED_IN_INSTANCE, CYCLE, SUBCYCLE) %>% 
  summarize(Litter_PctCover = mean(LITT, na.rm = TRUE), 
            Rock_PctCover = mean(ROCK, na.rm = TRUE),
            Wood_PctCover = mean(WOOD, na.rm = TRUE),
            BareGround_PctCover = mean(BARE, na.rm = TRUE),
            VegBasalArea_PctCover = mean(BAVE, na.rm = TRUE),
            Moss_PctCover = mean(MOSS, na.rm = TRUE),
            NonInventoriedConditionClassLand_PctCover = mean(NOIN, na.rm = TRUE),
            Lichen_PctCover = mean(LICH, na.rm = TRUE),
            Nonsampled_PctCover = mean(NONS, na.rm = TRUE),
            Crypto_PctCover = mean(CRYP, na.rm = TRUE),
            VolcanicMaterial_PctCover = mean(TEPH, na.rm = TRUE),
            Road_PctCover = mean(ROAD, na.rm = TRUE),
            Water_PctCover = mean(WATE, na.rm = TRUE),
            Developed_PctCover = mean(DEVP, na.rm = TRUE),
            Ash_PctCover = mean(ASH, na.rm = TRUE),
            TempIceSnow_PctCover = mean(TRIS, na.rm = TRUE)+ mean(TEIS, na.rm = TRUE),
            PermIceSnow_PctCover = mean(PEIS, na.rm = TRUE) ) %>% 
  ungroup() %>% 
  # get rid of NaN values from calculating the mean of only NAs (plots that have no values for a given cover class)
  mutate(Litter_PctCover = replace(Litter_PctCover, is.nan(Litter_PctCover),0),
         Rock_PctCover = replace(Rock_PctCover, is.nan(Rock_PctCover),0),
         Wood_PctCover = replace(Wood_PctCover, is.nan(Wood_PctCover),0),
         BareGround_PctCover = replace(BareGround_PctCover, is.nan(BareGround_PctCover),0),
         VegBasalArea_PctCover = replace(VegBasalArea_PctCover, is.nan(VegBasalArea_PctCover),0),
         Moss_PctCover = replace(Moss_PctCover, is.nan(Moss_PctCover),0),
         Lichen_PctCover = replace(Lichen_PctCover, is.nan(Lichen_PctCover),0)) %>% 
# add plot-level information from the COND table
  # add in the LatLong info from the COND dataset, and filter for the plots we want
  left_join(COND[,c("PLT_CN", "INVYR", "STATECD", "UNITCD", "COUNTYCD", "PLOT", 
                    "CONDID", "PCTBARE_RMRS", "SLOPE", "ASPECT", "STATENAME", "LAT", "LON")]) %>% 
  filter(!is.na(LAT))
  