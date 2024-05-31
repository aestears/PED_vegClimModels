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
library(taxonlookup)

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
#"AH" means % cover for any hits on the transect; "FH" means % cover for the first hit of the transect
# To get "total foliar cover", we want the first hit % cover... to get "species cover," we'd want to use "any hit" cover data? 
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
badQuads_1 <- unique(PG_data$PrimaryKey)                   

# remove those plots from the data frame of grass species
grassDat <- speciesDat %>% 
  filter(GrowthHabitSub == "Graminoid" &         
           !is.na(AH_SpeciesCover) &
           !(PrimaryKey %in% badQuads_1))

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
badQuads_2 <- unique(AG_data$PrimaryKey)                   

# remove those plots from the data frame of grass species
grassDat <- grassDat %>% 
  filter(GrowthHabitSub == "Graminoid" &    
           !(PrimaryKey %in% badQuads_2))

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

C3_cover <- grassDat2 %>% 
  filter(PhotosyntheticPathway == "C3") %>% 
  group_by(PrimaryKey) %>% 
  summarize(AH_C3TotalCover = sum(AH_SpeciesCover, na.rm = TRUE),
            Hgt_C3_avg = mean(Hgt_Species_Avg, na.rm = TRUE), 
            DateVisited = unique(DateVisited))
  
C4_cover <- grassDat2 %>% 
  filter(PhotosyntheticPathway == "C4") %>% 
  group_by(PrimaryKey) %>% 
  summarize(AH_C4TotalCover = sum(AH_SpeciesCover, na.rm = TRUE),
            Hgt_C4_avg = mean(Hgt_Species_Avg, na.rm = TRUE), 
            DateVisited = unique(DateVisited))


# investigate tree data? ---------------------------------------------------
#There are actually 27,946 tree observations, so we should calculate decid vs coniferous tree data too
# add tree and shrub species names
taxTrees <- read.csv("./data/USDA_plants_SppNameTable_TREE.csv") %>% 
  rename("AcceptedSymbol" = Accepted.Symbol,
         "SynonymSymbol" = Synonym,
         "ScientificName" = Scientific.Name,
         "CommonName" = Common.Name) %>% 
  mutate(Status = "Accepted")
taxShrub <- read.csv("./data/USDA_plants_SppNameTable_SHRUB.csv")  %>% 
  rename("AcceptedSymbol" = Accepted.Symbol,
         "SynonymSymbol" = Synonym,
         "ScientificName" = Scientific.Name,
         "CommonName" = Common.Name) %>% 
  mutate(Status = "Accepted")

taxCodes_newNew <- rbind(taxCodes_new, taxTrees, taxShrub)
#filter to accepted names and remove duplicates

taxCodes_use <- taxCodes_newNew[taxCodes_newNew$Status=="Accepted",] 
# shorten the name 
taxCodes_use$ScientificName <- apply(X = str_split(taxCodes_use$ScientificName, " ", simplify = TRUE)[,1:2], 
        MARGIN = 1, 
      FUN = function(x) 
        paste(x[1],  x[2])
      )
# remove duplicates
taxCodes_use <- unique(taxCodes_use[,c("AcceptedSymbol", "ScientificName")])

#add species names to species-level data
speciesDat_new <- speciesDat %>% 
  left_join(taxCodes_use, by = c("Species" = "AcceptedSymbol"))
# got most, but not all species names, but I think should be ok...
# filter for tree data
treeDat <- speciesDat_new %>% 
  filter(GrowthHabitSub == "Tree")
# fix the missing tree names that we can
treeDat[treeDat$Species == "ABIES", "ScientificName"] <- "Abies"
treeDat[treeDat$Species == "JUNIP", "ScientificName"] <- "Juniperus"
treeDat[treeDat$Species == "PICEA", "ScientificName"] <- "Picea"
treeDat[treeDat$Species %in% c("PINSCO", "PINUS", "PINUSTR"), "ScientificName"] <- "Pinus"

# remove data for plots that have species we don't know that have a high % cover
# get plot names that have >10% of missing species names 
badTree <- treeDat %>% 
  filter(is.na(ScientificName)) %>% 
  filter(AH_SpeciesCover > 10)
badQuads_tree <- unique(badTree$PrimaryKey)                   

# remove those plots from the data frame of grass species
treeDat <- treeDat %>% 
  filter(!(PrimaryKey %in% badQuads_tree))

# remove observations for weird species names w/ % cover less than 10%
treeDat <- treeDat %>% 
  filter(!is.na(ScientificName))

# get coniferous vs deciduous data 
treeGroups <- taxonlookup::lookup_table(species_list = unique(treeDat$ScientificName), by_species = TRUE)
treeGroups$Species <- row.names(treeGroups)

treeDat <- left_join(treeDat, treeGroups[,c("Species", "group")], by = c("ScientificName"="Species"))
#fix missing names
treeDat[treeDat$ScientificName %in% c("Hesperocyparis sargentii", "Hesperocyparis macnabiana"), "group"] <- "Gymnosperms"

## agregate by angiosperm vs gymnosperm
conifDat <- treeDat %>% 
  filter(group == "Gymnosperms") %>% 
  group_by(PrimaryKey) %>% 
  summarize(AH_ConifTotalCover = sum(AH_SpeciesCover, na.rm = TRUE),
            Hgt_Conif_avg = mean(Hgt_Species_Avg, na.rm = TRUE), 
            DateVisited = unique(DateVisited))

angioDat <- treeDat %>% 
  filter(group == "Angiosperms") %>% 
  group_by(PrimaryKey) %>% 
  summarize(AH_AngioTotalCover = sum(AH_SpeciesCover, na.rm = TRUE),
            Hgt_Angio_avg = mean(Hgt_Species_Avg, na.rm = TRUE), 
            DateVisited = unique(DateVisited))




# add together 
test <- full_join(C3_cover, C4_cover, by = c("PrimaryKey", "DateVisited")) %>% 
  full_join(conifDat, by = c("PrimaryKey", "DateVisited")) %>% 
  full_join(angioDat, by = c("PrimaryKey", "DateVisited"))

## add in shrub and forb data from "Indicators" d.f
datAll <- indicatorDat %>% 
  select(rid, PrimaryKey, DBKey, ProjectKey, DateVisited, Latitude_NAD83, Longitude_NAD83,
         BareSoilCover, TotalFoliarCover, AH_ForbCover, AH_ShrubCover,
         FH_CyanobacteriaCover, FH_DepSoilCover, FH_DuffCover, FH_EmbLitterCover,
         FH_HerbLitterCover, FH_LichenCover, FH_MossCover, FH_RockCover, FH_TotalLitterCover,
         FH_WoodyLitterCover) %>% 
  full_join(test, by = "PrimaryKey") %>% 
  select(-DateVisited.y) %>% 
  rename("DateVisited" = "DateVisited.x")


# save data to file 
write.csv(datAll, 
          file = "./data/LandscapeDataCommonsDat/coverDat_use.csv",
          row.names = FALSE)

