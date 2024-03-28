#///////////////////
# Importing and exploring potential data sources
# Alice Stears
# 04/05/2024
#///////////////////


# load packages -----------------------------------------------------------
#devtools::install_github("landscape-data-commons/trex", build_vignettes = TRUE)
library(trex)
library(tidyverse)
library(geojsonsf)


# AIM data exploration ----------------------------------------------------
### this is raw AIM data, I think? not actually quite sure... will move to 
# getting data form the Landscape Data Commons, per Gregor's advice
### read in LMF data 
AIM_LMF <- geojson_sf("./data/AIM_BLM_Natl_AIM_LMF_Hub/BLM_Natl_AIM_LMF_Hub_-6152108000941427991.geojson") %>% 
  mutate(Date_visited_new = str_sub(DateVisited,  start = 6, end = 16))
# convert site visit date to POSIXct
AIM_LMF$Date_visited_new <- as.POSIXct(AIM_LMF$Date_visited_new, tz = "GMT", format = "%d %b %Y")

### read in TerrADat data 
AIM_TDat<- geojson_sf("./data/AIM_BLM_Natl_AIM_TerrADat_Hub/BLM_Natl_AIM_TerrADat_Hub_-8322335809691410121.geojson") %>% 
  mutate(Date_visited_new = str_sub(DateVisited,  start = 6, end = 16)) %>% 
  filter(SpeciesState != "AK")
  # get rid of Alaska
# convert site visit date to POSIXct
AIM_TDat$Date_visited_new <- as.POSIXct(AIM_TDat$Date_visited_new, tz = "GMT", format = "%d %b %Y")

ggplot() + 
  geom_sf(data = AIM_LMF, aes(color = as.factor(lubridate::year(Date_visited_new))), pch = 18, alpha = .5) + 
  geom_sf(data = AIM_TDat, aes(color = as.factor(lubridate::year(Date_visited_new))), pch = 1, alpha = .5) +
  guides(fill = guide_none())


# Get AIM Data using trex R package ---------------------------------------
# acronyms in LDC data: "FH" = "%First Hit" (the percentage of the first hit that had that species??)
# "HGT" = height 
# "AH" = "all hits"? maybe? 

# get "indicators" data, for all plots!  (keys argument)
# indicatorDat <- trex::fetch_ldc(#keys = "17073113365985482017-09-01", key_type = "PrimaryKey",
#                 data_type = "indicators") %>% 
  # mutate(DateVisited = lubridate::as_date(indicatorDat$DateVisited))
  # 

# trim out Alaska
# indicatorDat <- indicatorDat %>% 
#   filter(Latitude_NAD83 < 55)
# # save as a .rds object for later
# saveRDS(indicatorDat, file = "./data/LandscapeDataCommonsDat/IndicatorDat.rds")
indicatorDat <- readRDS(file = "./data/LandscapeDataCommonsDat/IndicatorDat.rds")

# # get species tables 
# speciesDat <- trex::fetch_ldc(#keys = "17073113365985482017-09-01", key_type = "PrimaryKey", 
#   data_type = "species")
# # remove data for plots in Alaska
# speciesDat <- speciesDat %>% 
#   filter(PrimaryKey %in% unique(indicatorDat$PrimaryKey))
# 
# # save as a .rds object for later
# saveRDS(speciesDat, file = "./data/LandscapeDataCommonsDat/SpeciesDat.rds")
speciesDat <- readRDS(file = "./data/LandscapeDataCommonsDat/SpeciesDat.rds")

## what are the years when the sites were visited? 
unique(lubridate::year(indicatorDat$DateVisited))
# visualize the location of plots
ggplot(indicatorDat[1:20000,]) + 
  geom_point(aes(x = Longitude_NAD83, y = Latitude_NAD83, color = TotalFoliarCover))

# what do the distributions of percent cover data look like? 
# total foliar cove
hist(indicatorDat$TotalFoliarCover) # pretty normally distributed between 0 and 100

# I think other functional group cover values are all very right skewed
hist(indicatorDat$AH_AnnGrassCover) # very right skewed
hist(indicatorDat$AH_PerenGrassCover) # very right skewed
hist(indicatorDat$AH_PerenForbCover)

# do cover values ever sum above 100%?
summedCover <- indicatorDat %>% 
  select(AH_ForbCover, AH_GrassCover, AH_ShrubCover) %>% 
  rowSums()
hist(summedCover)

  

# Calculate % Cover by c3 vs c4 grass -------------------------------------
# can just aggregate from Species-level data (double check that this is accurate, with shrubs, for example!)
shrubTest <- speciesDat %>% 
  filter(GrowthHabitSub == "Shrub") %>% 
  group_by(PrimaryKey) %>% 
  summarize("AH_shrub_sum" = sum(AH_SpeciesCover, na.rm = TRUE))
# compare to actual data in indicatorDat
testTest <- indicatorDat %>% 
  select(PrimaryKey, AH_ShrubCover) %>% 
  left_join(shrubTest)

# The values are the same! Can proceed w/ DIY grouping of grasses by C3 vs. 
# C4 using by aggregating the speciesDat data frame
# get list of grass species for which we need C3/C4 data
grassSpp <- speciesDat %>% 
  filter(GrowthHabitSub == "Graminoid" & !is.na(AH_SpeciesCover)) %>% 
  select(Species) %>% 
  unique()

## problem... there are quite a few "species" that are just called "perennial 
# grass" which I obviously can't assign a photosynthetic pathway to. Should I 
# just remove plots that have these "species"? There are quite a few of these 
# observations that have a large % cover, so I definitely don't want to just throw them out
# find the 'bad quadrats' that have these weird species names so that we can remove them 
PG_data <- speciesDat %>% 
  filter(str_detect(speciesDat$Species, pattern = "^PG"))
# get plot names that have >2% of weird species names 
PG_data <- PG_data %>% 
  filter(AH_SpeciesCover > 2)
badQuads <- unique(PG_data$PrimaryKey)                   

# remove those plots from the data frame of grass species
grassDat <- speciesDat %>% 
  filter(GrowthHabitSub == "Graminoid" &         
           !is.na(AH_SpeciesCover) &
    !(PrimaryKey %in% badQuads))

# remove observations for weird species names w/ % cover less than 2%
grassDat <- grassDat %>% 
  filter(!str_detect(Species, pattern = "^PG"))

##
# also issues with "names" starting with "AG" (which I think means Annual Grass)
AG_data <- speciesDat %>% 
  filter(str_detect(speciesDat$Species, pattern = "^AG"))
# get plot names that have >2% of weird species names 
AG_data <- AG_data %>% 
  filter(AH_SpeciesCover > 2)
badQuads <- unique(AG_data$PrimaryKey)                   

# remove those plots from the data frame of grass species
grassDat <- grassDat %>% 
  filter(GrowthHabitSub == "Graminoid" &    
           !(PrimaryKey %in% badQuads))

# remove observations for weird species names w/ % cover less than 2%
grassDat <- grassDat %>% 
  filter(!str_detect(Species, pattern = "^AG"))

## now get the unique names in the dataset
grassNames <- unique(grassDat$Species) %>% 
  data.frame() 

names(grassNames) <- "Species"
# get the species names lookup table from BLM 
taxCodes <- read.csv("./data/USDA_plants_SppNameTable.csv")
# deal w/ synonym issues (try to, anyway)
uniqueSymbols <- unique(taxCodes$AcceptedSymbol)
for (i in 1:length(uniqueSymbols)) {
  temp <- taxCodes[taxCodes$AcceptedSymbol == uniqueSymbols[i],]
  temp$Status <- NA
  temp[temp$SynonymSymbol != "","Status"] <- "Synonym"
  temp[temp$SynonymSymbol == "","Status"] <- "Accepted"
  if (i == 1) {
    taxCodes_new <- temp
  } else {
    taxCodes_new <- rbind(taxCodes_new, temp)
  }
}


# save the synonyms data
#write.csv(taxCodes_new, "./data/USDA_plants_SppNameTable_synonymNames.csv", row.names = TRUE)

# now, get photosynthetic pathway data
# trim to just grasses in the names dataset
# get just names in the dataset w/out synonyms
grassNames_data <- left_join(grassNames, taxCodes_new[taxCodes_new$Status == "Accepted",], by = c("Species"= "AcceptedSymbol"))
# get just names in the dataset with synonyms
grassNames_all <- left_join(grassNames, taxCodes_new,  by = c("Species"= "AcceptedSymbol"))

#write.csv(grassNames_all, "./data/USDA_plants_SppNameTable_allGrassInDataset.csv", row.names = FALSE)

# read in dataset w/ photosynthetic pathway information
grassNames_photo <- read.csv("./data/USDA_plants_SppNameTable_allGrassInDataset.csv")

# use to get species latin names and photosynthetic pathways
grassDat2 <- left_join(grassDat, grassNames_photo[grassNames_photo$Status == "Accepted",], 
                      by = c("Species"= "Species")) %>% 
  select(-SynonymSymbol)



