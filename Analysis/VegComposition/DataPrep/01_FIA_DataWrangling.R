#///////////////////
# Importing and exploring potential data sources - Forest Inventory and Analysis Data
# Alice Stears
# 04/10/2024
#///////////////////


# load packages -----------------------------------------------------------

library(tidyverse)
library(FIESTA)

# Read in data ------------------------------------------------------------

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

ggplot(COND %>% 
         filter(is.na(COND_NONSAMPLE_REASN_CD))
       ) + 
  facet_wrap(~INVYR) + 
  geom_point(aes(LON, LAT))

# get vegetation data -----------------------------------------------------
# Vegetation
#structure in each subplot (the same as above, but with cover by functional
#group/layer) this dataset has many more states (nearly all?)--will likely have
#to use this then, which should be fine, although we'll have to break out tree
#types using the TREE table I guess...
VEG_fgroup <- read.csv(paste0(file, "ENTIRE_P2VEG_SUBP_STRUCTURE.csv"))
# make into a wide format (with a column for each functional group), aggregate
# by subplot, then average across subplots in a plot
VEG_fgroup_PlotAvgs <- VEG_fgroup %>% 
  # subset only to layer "5" (the "aerial cover", which sums across all layers)
  filter(LAYER==5) %>% 
  select(-LAYER) %>% 
  # pivot_wider(names_from = GROWTH_HABIT_CD, 
  #             values_from = COVER_PCT#,
  #             # names_glue = ("_AerialCover")
  #             ) %>% 
  # group_by( PLT_CN, STATECD, UNITCD, COUNTYCD, PLOT, INVYR, SUBP, CONDID, 
  #           # don't want to use CN, since that is a unique # for each row in the 
  #           # veg structure species-level dataset
  #          MODIFIED_BY, MODIFIED_DATE, MODIFIED_IN_INSTANCE, CYCLE, SUBCYCLE) %>% 
  # summarize(Forbs_AerialCover = sum(FB, na.rm = TRUE), 
  #           Graminoid_AerialCover = sum(GR, na.rm = TRUE),
  #           NonTallyTree_AerialCover = sum(NT, na.rm = TRUE), 
  #           Shrub_AerialCover = sum(SH, na.rm = TRUE),
  #           TallyTree_AerialCover = sum(TT, na.rm = TRUE)) %>% 
  # ungroup() %>% 
  # group_by(PLT_CN, STATECD, UNITCD, COUNTYCD, PLOT, INVYR, CONDID, # group by plot (exclude the SUBP (subplot) column)
  #           CYCLE, SUBCYCLE) %>% 
  # summarize(Forbs_AerialCover = mean(Forbs_AerialCover),  # average across subplots
  #           Graminoid_AerialCover = mean(Graminoid_AerialCover),
  #           NonTallyTree_AerialCover = mean(NonTallyTree_AerialCover), 
  #           Shrub_AerialCover = mean(Shrub_AerialCover),
  #           TallyTree_AerialCover = mean(TallyTree_AerialCover)) %>% 
  # add in the LatLong info from the COND dataset, and filter for the plots we want
 left_join(COND[,c("PLT_CN", "INVYR", "STATECD", "UNITCD", "COUNTYCD", "PLOT", 
                   "CONDID", "PCTBARE_RMRS", "SLOPE", "ASPECT", "STATENAME", "LAT", "LON")]) %>% 
  filter(!is.na(LAT))

ggplot(VEG_fgroup_PlotAvgs %>% 
         filter(GROWTH_HABIT_CD == "SS")) + 
  facet_wrap(~INVYR) + 
  stat_summary_2d(aes(x = LON, y = LAT, z = COVER_PCT), fun = mean, binwidth = .25) + 
  scale_fill_viridis_c(option = "A", guide = guide_colorbar(title = "% cover"), 
limits = c(0,100)) 
  # geom_point(aes(LON, LAT, col = COVER_PCT))

## add in the c3/c4 and angio/conifer data from the species-level plot data (not present for many states, but have some)
# test <- VEG_fgroup_PlotAvgs %>% 
#   full_join(bySppCoverData)
## save plot-level veg data
#write.csv(VEG_fgroup_PlotAvgs, file = "./data/FIA/vegetationComposition_use.csv", row.names = FALSE)
VEG_fgroup_PlotAvgs <- read.csv("./Data_raw/FIA/vegetationComposition_use.csv")

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

## save plot-level ground cover data
write.csv(GRND_CVR_PlotAvgs, file = "./Data_raw/FIA/groundCover_use.csv", row.names = FALSE)

 ## get litter depth information
DWM_DUFF_LITTER_FUEL <- read.csv(paste0(file,"ENTIRE_DWM_DUFF_LITTER_FUEL.csv"))
 
DWM_DUFF_LITTER_FUEL_PlotAvgs <- DWM_DUFF_LITTER_FUEL %>% 
  ## calculate average depths for each plot
  group_by(PLT_CN, INVYR, STATECD, COUNTYCD, PLOT, MEASYEAR, CONDID) %>% 
  summarize(DuffDepth = mean(DUFFDEP, na.rm = TRUE),
            LitterDepth = mean(LITTDEP, na.rm = TRUE),
            FuelDepth = mean(FUELDEP, na.rm = TRUE)) %>% 
  # filter down to the plots we want
  left_join(COND[,c("PLT_CN", "INVYR", "STATECD", "UNITCD", "COUNTYCD", "PLOT", 
                    "CONDID", "PCTBARE_RMRS", "SLOPE", "ASPECT", "STATENAME", "LAT", "LON")]) %>% 
  filter(!is.na(LAT))
## save plot-level litter data
write.csv(DWM_DUFF_LITTER_FUEL_PlotAvgs, file = "./Data_raw//FIA/LitterDuffFuel_use.csv", row.names = FALSE)



# Get TREE data -----------------------------------------------------------
# too big to do in one chunk, need to do it by region
TREE_1 <- FIESTA::DBgetCSV("TREE", states = unique(COND$STATECD)[1:5])
TREE_2 <- FIESTA::DBgetCSV("TREE", states = unique(COND$STATECD)[6:10])
TREE_3 <- FIESTA::DBgetCSV("TREE", states = unique(COND$STATECD)[11:15])
TREE_4 <- FIESTA::DBgetCSV("TREE", states = unique(COND$STATECD)[16:20])
TREE_5 <- FIESTA::DBgetCSV("TREE", states = unique(COND$STATECD)[21:25])
TREE_6 <- FIESTA::DBgetCSV("TREE", states = unique(COND$STATECD)[26:30])
TREE_7 <- FIESTA::DBgetCSV("TREE", states = unique(COND$STATECD)[30:35])
TREE_8 <- FIESTA::DBgetCSV("TREE", states = unique(COND$STATECD)[36:40])
TREE_9 <- FIESTA::DBgetCSV("TREE", states = unique(COND$STATECD)[40:45])
TREE_10 <- FIESTA::DBgetCSV("TREE", states = unique(COND$STATECD)[46:48])

# get list of tree species codes
sppCodes <- FIESTA::ref_species
#sppCodes$E_SPGRPCD - sppCodes$W_SPGRPCD
# use the "taxonlookup" R package to get the families for each tree species
sppCodes_temp <- taxonlookup::lookup_table(
  species_list = unique(sppCodes$SCIENTIFIC_NAME)
                          , by_species = TRUE) 
sppCodes_temp$ScientificName <- row.names(sppCodes_temp)

sppCodes <- sppCodes %>% 
  left_join(sppCodes_temp, by = c("SCIENTIFIC_NAME" = "ScientificName"))
# add 'group' information for those that don't have it
sppCodes[sppCodes$GENUS %in% c("Reynoldsia", "Feijoa", "Exorrhiza", "Gulubia", 
                               "Trukia", "Neolaugeria", "Nesoluma", "Hyeronima",
                               "Guamia", "Carmona", "Munroidendron"), "group"] <- "Angiosperms"

#Pteridophytes
#Genus = 
sppCodes[sppCodes$GENUS %in% c("Acoelorraphe", "Family Arecaceae", "Howeia"), 
         "group"] <- "Pteridophytes"

#Gymnosperms
sppCodes[sppCodes$GENUS %in% c("Cupressocyparis"), 
         "group"] <- "Gymnosperms"


sppCodes[sppCodes$SCIENTIFIC_NAME == "Tree broadleaf","group"] <- "Angiosperms"

sppCodes[sppCodes$SCIENTIFIC_NAME == "Tree evergreen","group"] <- "Gymnosperms"

## add species data to the tree table
TREE_1NEW <- TREE_1 %>% 
  left_join(sppCodes[,c("SPCD", "SCIENTIFIC_NAME", "family", "group")]) %>% 
  select(PLT_CN, INVYR, STATECD, UNITCD, COUNTYCD, PLOT, SUBP, TREE, CONDID, 
         STATUSCD, SPCD, DIA, DIAHTCD, HT, HTCD, ACTUALHT, TREECLCD, CCLCD, 
         TREEGRCD, CARBON_AG, CARBON_BG, DRYBIO_STEM, DRYBIO_FOLIAGE, DRYBIO_BRANCH, SCIENTIFIC_NAME, group)
TREE_2NEW <- TREE_2 %>% 
  left_join(sppCodes[,c("SPCD", "SCIENTIFIC_NAME", "family", "group")])%>% 
  select(PLT_CN, INVYR, STATECD, UNITCD, COUNTYCD, PLOT, SUBP, TREE, CONDID, 
         STATUSCD, SPCD, DIA, DIAHTCD, HT, HTCD, ACTUALHT, TREECLCD, CCLCD, 
         TREEGRCD, CARBON_AG, CARBON_BG, DRYBIO_STEM, DRYBIO_FOLIAGE, DRYBIO_BRANCH, SCIENTIFIC_NAME, group)
TREE_3NEW <- TREE_3 %>% 
  left_join(sppCodes[,c("SPCD", "SCIENTIFIC_NAME", "family", "group")])%>% 
  select(PLT_CN, INVYR, STATECD, UNITCD, COUNTYCD, PLOT, SUBP, TREE, CONDID, 
         STATUSCD, SPCD, DIA, DIAHTCD, HT, HTCD, ACTUALHT, TREECLCD, CCLCD, 
         TREEGRCD, CARBON_AG, CARBON_BG, DRYBIO_STEM, DRYBIO_FOLIAGE, DRYBIO_BRANCH, SCIENTIFIC_NAME, group)
TREE_4NEW <- TREE_4 %>% 
  left_join(sppCodes[,c("SPCD", "SCIENTIFIC_NAME", "family", "group")])%>% 
  select(PLT_CN, INVYR, STATECD, UNITCD, COUNTYCD, PLOT, SUBP, TREE, CONDID, 
         STATUSCD, SPCD, DIA, DIAHTCD, HT, HTCD, ACTUALHT, TREECLCD, CCLCD, 
         TREEGRCD, CARBON_AG, CARBON_BG, DRYBIO_STEM, DRYBIO_FOLIAGE, DRYBIO_BRANCH, SCIENTIFIC_NAME, group)
TREE_5NEW <- TREE_5 %>% 
  left_join(sppCodes[,c("SPCD", "SCIENTIFIC_NAME", "family", "group")])%>% 
  select(PLT_CN, INVYR, STATECD, UNITCD, COUNTYCD, PLOT, SUBP, TREE, CONDID, 
         STATUSCD, SPCD, DIA, DIAHTCD, HT, HTCD, ACTUALHT, TREECLCD, CCLCD, 
         TREEGRCD, CARBON_AG, CARBON_BG, DRYBIO_STEM, DRYBIO_FOLIAGE, DRYBIO_BRANCH, SCIENTIFIC_NAME, group)
TREE_6NEW <- TREE_6 %>% 
  left_join(sppCodes[,c("SPCD", "SCIENTIFIC_NAME", "family", "group")])%>% 
  select(PLT_CN, INVYR, STATECD, UNITCD, COUNTYCD, PLOT, SUBP, TREE, CONDID, 
         STATUSCD, SPCD, DIA, DIAHTCD, HT, HTCD, ACTUALHT, TREECLCD, CCLCD, 
         TREEGRCD, CARBON_AG, CARBON_BG, DRYBIO_STEM, DRYBIO_FOLIAGE, DRYBIO_BRANCH, SCIENTIFIC_NAME, group)
TREE_7NEW <- TREE_7 %>% 
  left_join(sppCodes[,c("SPCD", "SCIENTIFIC_NAME", "family", "group")])%>% 
  select(PLT_CN, INVYR, STATECD, UNITCD, COUNTYCD, PLOT, SUBP, TREE, CONDID, 
         STATUSCD, SPCD, DIA, DIAHTCD, HT, HTCD, ACTUALHT, TREECLCD, CCLCD, 
         TREEGRCD, CARBON_AG, CARBON_BG, DRYBIO_STEM, DRYBIO_FOLIAGE, DRYBIO_BRANCH, SCIENTIFIC_NAME, group)
TREE_8NEW <- TREE_8 %>% 
  left_join(sppCodes[,c("SPCD", "SCIENTIFIC_NAME", "family", "group")])%>% 
  select(PLT_CN, INVYR, STATECD, UNITCD, COUNTYCD, PLOT, SUBP, TREE, CONDID, 
         STATUSCD, SPCD, DIA, DIAHTCD, HT, HTCD, ACTUALHT, TREECLCD, CCLCD, 
         TREEGRCD, CARBON_AG, CARBON_BG, DRYBIO_STEM, DRYBIO_FOLIAGE, DRYBIO_BRANCH, SCIENTIFIC_NAME, group)
TREE_9NEW <- TREE_9 %>% 
  left_join(sppCodes[,c("SPCD", "SCIENTIFIC_NAME", "family", "group")])%>% 
  select(PLT_CN, INVYR, STATECD, UNITCD, COUNTYCD, PLOT, SUBP, TREE, CONDID, 
         STATUSCD, SPCD, DIA, DIAHTCD, HT, HTCD, ACTUALHT, TREECLCD, CCLCD, 
         TREEGRCD, CARBON_AG, CARBON_BG, DRYBIO_STEM, DRYBIO_FOLIAGE, DRYBIO_BRANCH, SCIENTIFIC_NAME, group)
TREE_10NEW <- TREE_10 %>% 
  left_join(sppCodes[,c("SPCD", "SCIENTIFIC_NAME", "family", "group")])%>% 
  select(PLT_CN, INVYR, STATECD, UNITCD, COUNTYCD, PLOT, SUBP, TREE, CONDID, 
         STATUSCD, SPCD, DIA, DIAHTCD, HT, HTCD, ACTUALHT, TREECLCD, CCLCD, 
         TREEGRCD, CARBON_AG, CARBON_BG, DRYBIO_STEM, DRYBIO_FOLIAGE, DRYBIO_BRANCH, SCIENTIFIC_NAME, group)

TREE <- rbind(TREE_1NEW, TREE_2NEW, TREE_3NEW, TREE_4NEW, TREE_5NEW, TREE_6NEW,
              TREE_7NEW, TREE_8NEW, TREE_9NEW, TREE_10NEW)
#saveRDS(TREE, file = "./data_raw/FIA/TREEtable.RDS")
# TREE <- readRDS(file = "./data_raw/FIA/TREEtable.RDS")

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
            Carbon_AG_sum = sum(CARBON_AG), 
            Carbon_BG_sum = sum(CARBON_BG), 
            DryBio_stem_sum = sum(DRYBIO_STEM), 
            DryBio_foliage_sum = sum(DRYBIO_FOLIAGE),
            DryBio_branch_sum = sum(DRYBIO_BRANCH)
  ) %>% ## add location information and filter for plots we don't want
  mutate(PLT_CN = as.double(PLT_CN)) %>% 
  left_join(COND[,c("PLT_CN", "INVYR", "STATECD", "UNITCD", "COUNTYCD", "PLOT", 
                    "CONDID", "PCTBARE_RMRS", "SLOPE", "ASPECT", "STATENAME", "LAT", "LON")]) %>% 
  filter(!is.na(LAT))

ggplot(TREE_SubPlot) + 
  facet_wrap(~group) + 
  geom_point(aes(LON, LAT, col = basalAreaSum_in2), pch = 1)



# group by plot (values averaged within each plot--averages of summed values in each subplot w/in a plot--, grouped by species group)
TREE_Plot <- TREE_SubPlot %>% 
  #select(PLT_CN, INVYR, STATECD, UNITCD, COUNTYCD, PLOT, CONDID, PCTBARE_RMRS, 
         #SLOPE, ASPECT, STATENAME, LAT, LON, group) %>% 
  group_by(PLT_CN, INVYR, STATECD, UNITCD, COUNTYCD, PLOT, CONDID, STATUSCD, group) %>% 
  summarize(Height_subpAvg_plotAvg_ft = mean(HeightAvg_ft, na.rm = TRUE),
            Dia_subpAvg_plotAvg_in = mean(DiaAvg_in),
            basalArea_subpSum_plotAvg_in2 = mean(basalAreaSum_in2, na.rm = TRUE),
            Carbon_AG_subpSum_plotAvg = mean(Carbon_AG_sum, na.rm = TRUE), 
            Carbon_BG_subpSum_plotAvg = mean(Carbon_BG_sum, na.rm = TRUE), 
            DryBio_stem_subpSum_plotAvg = mean(DryBio_stem_sum, na.rm = TRUE), 
            DryBio_foliage_subpSum_plotAvg = mean(DryBio_foliage_sum, na.rm = TRUE),
            DryBio_branch_subpSum_plotAvg = mean(DryBio_branch_sum, na.rm = TRUE)
  ) %>% ## add location information and filter for plots we don't want
  mutate(PLT_CN = as.double(PLT_CN)) %>% 
  left_join(COND[,c("PLT_CN", "INVYR", "STATECD", "UNITCD", "COUNTYCD", "PLOT", 
                    "CONDID", "PCTBARE_RMRS", "SLOPE", "ASPECT", "STATENAME", "LAT", "LON")]) %>% 
  filter(!is.na(LAT))


ggplot(TREE_Plot) + 
  facet_wrap(~group) + 
  geom_point(aes(LON, LAT, col = basalArea_subpSum_plotAvg_in2), pch = 1)

## make into a simpler format for us to use
TREE_use <- TREE_Plot %>% 
  ungroup() %>% 
  filter(STATUSCD == 1) %>% # remove dead trees 
  select(PLT_CN, INVYR, STATECD, UNITCD, COUNTYCD, PLOT, CONDID, group, LAT, 
         LON, basalArea_subpSum_plotAvg_in2) %>% # select only basal area variable
  pivot_wider(values_from = basalArea_subpSum_plotAvg_in2, names_from = group) %>% 
  rename("basalArea_Angiosperms_in2" = "Angiosperms", 
         "basalArea_Gymnosperms_in2" = "Gymnosperms",
         "basalArea_UnknownGroup_in2" = "Unknown",
         "basalArea_Pteridophytes_in2" = "Pteridophytes") %>%
  ## the basal area values in TREE_Plots d.fs do not have zeros, so the NAs in these biomass columns are true zeros
  mutate(basalArea_Angiosperms_in2 = replace(basalArea_Angiosperms_in2, is.na(basalArea_Angiosperms_in2), 0),
         basalArea_Gymnosperms_in2 = replace(basalArea_Gymnosperms_in2, is.na(basalArea_Gymnosperms_in2), 0),
         basalArea_UnknownGroup_in2 = replace(basalArea_UnknownGroup_in2, is.na(basalArea_UnknownGroup_in2), 0),
         basalArea_Pteridophytes_in2 = replace(basalArea_Pteridophytes_in2, is.na(basalArea_Pteridophytes_in2), 0))

ggplot(TREE_use[,c("LAT", "LON", "INVYR", "basalArea_Angiosperms_in2")]) + 
  geom_point(aes(LON, LAT, col = basalArea_Angiosperms_in2), pch = 1)


# calculate total plot-level basal area (averaged across subplots))
TREE_use$basalArea_allGroups_in2 = rowSums(TREE_use[,c("basalArea_Angiosperms_in2", "basalArea_Gymnosperms_in2", "basalArea_UnknownGroup_in2", "basalArea_Pteridophytes_in2")])
# calculate the proportion of the biomass that corresponds to each functional group 
TREE_use$basalArea_Angiosperms_perc = TREE_use$basalArea_Angiosperms_in2/TREE_use$basalArea_allGroups_in2*100
TREE_use$basalArea_Gymnosperms_perc = TREE_use$basalArea_Gymnosperms_in2/TREE_use$basalArea_allGroups_in2*100
TREE_use$basalArea_UnknownGroup_perc = TREE_use$basalArea_UnknownGroup_in2/TREE_use$basalArea_allGroups_in2*100
TREE_use$basalArea_Pteridophytes_perc = TREE_use$basalArea_Pteridophytes_in2/TREE_use$basalArea_allGroups_in2*100


plot(TREE_use$LON, TREE_use$LAT, col = as.factor(TREE_use$basalArea_Gymnosperms_perc))
ggplot(TREE_use) +
  geom_point(aes(LON, LAT, col = basalArea_Gymnosperms_perc)) + 
  facet_wrap(~INVYR)

## save data
saveRDS(TREE_use, "./Data_raw//FIA/TREEtable_use.rds")

