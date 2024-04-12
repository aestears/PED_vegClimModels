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
## there are only species-level data for 11 states that we're interested in... 
# why? (CA, CO, AZ, ID, NM, MT, NV, OR, UT, WA and WY)

## Vegetation structure in each subplot (the same as above, but with cover by functional group)
# this dataset has many more states (nearly all?)--will likely have to use this 
# then, which should be fine, although we'll have to break out tree types using the TREE table I guess...
VEG_fgroup <- read.csv(paste0(file, "ENTIRE_P2VEG_SUBP_STRUCTURE.csv"))
