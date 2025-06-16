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
indicatorDat <- readRDS(file = "./Data_raw/LandscapeDataCommonsDat/IndicatorDat.rds")
indicatorDat$Year <- lubridate::year(indicatorDat$DateVisited)

## remove indicator Data points from plots that are croplands, cropland/natural vegetation mosiacs, permanent Snow and Ice, Urban and Built-up Lands, and Water Bodies
indicatorDat <- indicatorDat %>% 
  filter(!(modis_landcover %in% c("Croplands", "Cropland/Natural Vegetation Mosaics", "Permanent Snow and Ice",
                                "Urban and Built-up Lands", "Water Bodies")))


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
speciesDat_temp <- readRDS(file = "./Data_raw/LandscapeDataCommonsDat/SpeciesDat.rds")
# aggregate the species data by year that it was sampled (sampled once per year, usually...)
speciesDat <- speciesDat_temp %>% 
  mutate(Year = lubridate::year(DateVisited))

## what are the years when the sites were visited? 
unique(lubridate::year(indicatorDat$DateVisited))
# visualize the location of plots
ggplot(indicatorDat[1:20000,]) + 
  geom_point(aes(x = Longitude_NAD83, y = Latitude_NAD83, color = TotalFoliarCover))

# # what do the distributions of percent cover data look like? 
# # total foliar cover
# hist(indicatorDat$TotalFoliarCover) # pretty normally distributed between 0 and 100
# 
# # I think other functional group cover values are all very right skewed
# hist(indicatorDat$AH_AnnGrassCover) # very right skewed
# hist(indicatorDat$AH_PerenGrassCover) # very right skewed
# hist(indicatorDat$AH_PerenForbCover)

# # do cover values ever sum above 100%?
# summedCover <- indicatorDat %>% 
#   select(AH_ForbCover, AH_GrassCover, AH_ShrubCover) %>% 
#   rowSums()
# hist(summedCover)

# Calculate % Cover by c3 vs c4 grass -------------------------------------
# # can just aggregate from Species-level data (double check that this is accurate, with shrubs, for example!)
# shrubTest <- speciesDat %>% 
#   filter(GrowthHabitSub == "Shrub") %>% 
#   group_by(PrimaryKey) %>% 
#   summarize("AH_shrub_sum" = sum(AH_SpeciesCover, na.rm = TRUE))
# 
# # compare to actual data in indicatorDat
# testTest <- indicatorDat %>% 
#   select(PrimaryKey, AH_ShrubCover) %>% 
#   left_join(shrubTest)

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
taxCodes <- read.csv("./Data_raw/USDA_plants_SppNameTable.csv")
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
taxCodes_new <- read.csv("./Data_raw/USDA_plants_SppNameTable_synonymNames.csv")
# now, get photosynthetic pathway data
# trim to just grasses in the names dataset
# get just names in the dataset w/out synonyms
grassNames_data <- left_join(grassNames, taxCodes_new[taxCodes_new$Status == "Accepted",], by = c("Species"= "AcceptedSymbol"))
# get just names in the dataset with synonyms
grassNames_all <- left_join(grassNames, taxCodes_new,  by = c("Species"= "AcceptedSymbol"))

#write.csv(grassNames_all, "./data/USDA_plants_SppNameTable_allGrassInDataset.csv", row.names = FALSE)

# read in dataset w/ photosynthetic pathway information
grassNames_photo <- read.csv("./Data_raw//USDA_plants_SppNameTable_allGrassInDataset.csv") %>% 
  select(Species, PhotosyntheticPathway) %>% 
  unique() %>% 
  filter(PhotosyntheticPathway %in% c("C3", "C4"))

# use to get species latin names and photosynthetic pathways
grassDat2 <- left_join(grassDat, grassNames_photo, 
                       by = c("Species"= "Species")) 

grassCoverTemp <- grassDat2 %>% 
  group_by(PrimaryKey, PhotosyntheticPathway, Year) %>% 
  summarize(n_hits = sum(AH_SpeciesCover_n)) %>% 
  pivot_wider(names_from =  PhotosyntheticPathway, 
              values_from = n_hits) %>% 
  mutate(AH_C3_n_hits = sum(c(C3, 0), na.rm = TRUE),
         AH_C4_n_hits = sum(c(C4, 0), na.rm = TRUE)) %>% 
  select(-C3, -C4) %>% 
  rename(NA_area = "NA") 



# remove plots for which >15% of the "graminoids" did not have a photosynthetic pathway 
grassCover <- grassCoverTemp[grassCoverTemp$NA_area <= 15 | is.na(grassCoverTemp$NA_area), ]
grassCover <- grassCover %>% 
  select(-NA_area)

## get total #n for all plots
totalGram_nDat <- speciesDat %>% 
  #filter(GrowthHabitSub == "Graminoid") %>% 
  group_by(PrimaryKey, Year) %>% 
  summarize("total_allSppCover_n" = sum(AH_SpeciesCover_n, na.rm = TRUE))
# add this information to the C3/C4 count data
grassCover_final <- grassCover %>% 
  left_join(totalGram_nDat) %>% 
  ## calculate the percentage of hits that are C3 vs C4
  mutate(C3_hits_proportionOfAllSpecies = AH_C3_n_hits/total_allSppCover_n, 
         C4_hits_proportionOfAllSpecies = AH_C4_n_hits/total_allSppCover_n) %>% 
  # drop data afor plots that don't have total plot 'n' values
  filter(!is.na(total_allSppCover_n))


# Calculate perennial vs annual cover for forbs and grams -----------------
# ## calculate perennial vs annual graminoid % cover
# gramAnnPenCover <- speciesDat %>% 
#   filter(GrowthHabitSub == "Graminoid" | 
#            GrowthHabitSub == "Sedge" | 
#            GrowthHabitSub == "Rush") %>% 
#   group_by(PrimaryKey, Duration, Year) %>% 
#   summarize(Cover = sum(AH_SpeciesCover, na.rm = TRUE)) %>% 
#   pivot_wider(names_from = Duration, 
#               values_from = Cover) %>% 
#   mutate(AH_GramPerenTotalCover = sum(c(Perennial, 0), na.rm = TRUE), 
#          AH_GramAnnTotalCover = sum(c(Annual, 0), na.rm = TRUE)) %>% 
#   select(-`NA`, -Annual, -Perennial)
# 
# ## calculate perennial vs annual forb % cover
# forbAnnPenCover <- speciesDat %>% 
#   filter(GrowthHabitSub == "Forb" | 
#            GrowthHabitSub == "Forb/herb") %>% 
#   group_by(PrimaryKey, Duration, Year) %>% 
#   summarize(Cover = sum(AH_SpeciesCover, na.rm = TRUE)) %>% 
#   pivot_wider(names_from = Duration, 
#               values_from = Cover) %>% 
#   mutate(AH_ForbPerenTotalCover = sum(c(Perennial, Biennial, 0), na.rm = TRUE), 
#          AH_ForbAnnTotalCover = sum(c(Annual, 0), na.rm = TRUE)) %>% 
#   select(-`NA`, -Annual, -Perennial, -Biennial, -PerennialÂ)

# investigate tree data ---------------------------------------------------
#There are actually 27,946 tree observations, so we should calculate decid vs coniferous tree data too
# add tree and shrub species names
taxTrees <- read.csv("./Data_raw/USDA_plants_SppNameTable_TREE.csv") %>% 
  rename("AcceptedSymbol" = Accepted.Symbol,
         "SynonymSymbol" = Synonym,
         "ScientificName" = Scientific.Name,
         "CommonName" = Common.Name) %>% 
  mutate(Status = "Accepted")
taxShrub <- read.csv("./Data_raw/USDA_plants_SppNameTable_SHRUB.csv")  %>% 
  rename("AcceptedSymbol" = Accepted.Symbol,
         "SynonymSymbol" = Synonym,
         "ScientificName" = Scientific.Name,
         "CommonName" = Common.Name) %>% 
  mutate(Status = "Accepted")

taxCodes_newNew <- rbind(taxCodes_new[,c("AcceptedSymbol","SynonymSymbol","ScientificName","CommonName","Status"   )], taxTrees, taxShrub)
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

# remove those plots from the data frame of tree species
treeDat <- treeDat %>% 
  filter(!(PrimaryKey %in% badQuads_tree))

# remove observations for weird species names w/ % cover less than 10%
treeDat <- treeDat %>% 
  filter(!is.na(ScientificName))

# remove observations that have no cover associated with them (not sure why these observations were included? )
treeDat <- treeDat %>% 
  filter(!is.na(AH_SpeciesCover))

# get coniferous vs deciduous data 
treeGroups <- taxonlookup::lookup_table(species_list = unique(treeDat$ScientificName), by_species = TRUE)
treeGroups$Species <- row.names(treeGroups)

treeDat <- left_join(treeDat, treeGroups[,c("Species", "group")], by = c("ScientificName"="Species"))
#fix missing names
treeDat[treeDat$ScientificName %in% c("Hesperocyparis sargentii", "Hesperocyparis macnabiana"), "group"] <- "Gymnosperms"


treeDat<- treeDat %>% 
  group_by(PrimaryKey, Year, group) %>% 
  summarize(n_hits = sum(AH_SpeciesCover_n)) %>% 
  pivot_wider(values_from = n_hits, names_from = group) %>% 
  mutate(AH_ConifTotalCover = sum(c(Gymnosperms, 0), na.rm = TRUE),
         AH_AngioTotalCover = sum(c(Angiosperms, 0), na.rm = TRUE)) %>% 
  select(-Angiosperms, -Gymnosperms)

## get total #n for all plots
totalTree_nDat <- speciesDat %>% 
  #filter(GrowthHabitSub == "Tree") %>% 
  group_by(PrimaryKey, Year) %>% 
  summarize("total_speciesCover_n" = sum(AH_SpeciesCover_n, na.rm = TRUE))
# add this information to the C3/C4 count data
treeCover_final <- treeDat %>% 
  left_join(totalTree_nDat) %>% 
  ## calculate the percentage of hits that are C3 vs C4
  mutate(Conif_hits_proportionOfAllSpp = AH_ConifTotalCover/total_speciesCover_n, 
         Angio_hits_proportionOfAllSpp = AH_AngioTotalCover/total_speciesCover_n) %>% 
  # drop data afor plots that don't have total plot 'n' values
  filter(!is.na(total_speciesCover_n))
  
  
# calculate shrub and CAM species ---------------------------------------------------
# data.frame that has species-level data as well as species names (speciesDat_new)
# speciesDat_new
# get family information
speciesDat_families <- taxonlookup::lookup_table(species_list = unique(speciesDat_new$ScientificName), by_species = TRUE)
speciesDat_families$Species <- row.names(speciesDat_families)
speciesDat_new <- left_join(speciesDat_new, speciesDat_families, by = c("ScientificName" = "Species"))
# narrow down to families that are likely to be CAM
# source (first pass): https://doi.org/10.1093/aob/mcad135
CAM_species <- speciesDat_new %>%
  filter(family %in% c("Crassulaceae", "Bromeliaceae", "Cactaceae",
                       "Anacampserotaceae", "Portulacaceae", "Talinaceae",
                       "Didiereaceae", "Halophytaceae", "Basellaceae")) %>% 
  mutate(PrimaryKey_Year = paste(PrimaryKey, Year))
# %>% 
  #filter(!is.na(AH_SpeciesCover))
# if there are NAs in the AH_SpeciesCover column, remove observations for that plot 
badPlots_cam <- CAM_species %>% 
  #filter(is.na(AH_SpeciesCover)) %>% 
 # select(PrimaryKey, DBKey, Year) %>% 
  group_by(PrimaryKey, DBKey, Year) %>% 
  summarize(n_NAs = sum(is.na(AH_SpeciesCover_n)),
            n_NotNAs = sum(!is.na(AH_SpeciesCover_n))) %>% 
  mutate(percentNAs = n_NAs/(n_NAs + n_NotNAs)) %>% 
  filter(percentNAs > .33333) %>% 
  select(PrimaryKey, DBKey, Year) %>% 
  unique() %>% 
  mutate(PrimaryKey_Year = paste(PrimaryKey, Year))

CAM_species <- CAM_species %>% 
  filter(!(PrimaryKey_Year %in% badPlots_cam$PrimaryKey_Year))

# calculate the number of hits for CAM species in each plot 
CAM_species <- CAM_species %>% 
  group_by(PrimaryKey, Year) %>% 
  summarize(CAM_n_hits = sum(AH_SpeciesCover_n)) %>% 
  filter(!is.na(CAM_n_hits)) %>% 
left_join(indicatorDat %>% select(PrimaryKey, DBKey, Latitude_NAD83, Longitude_NAD83))

# get total #n for all plots
totalSpecies_nDat <- speciesDat %>% 
  group_by(PrimaryKey, Year) %>% 
  summarize("total_speciesCover_n" = sum(AH_SpeciesCover_n, na.rm = TRUE))
# add this information to the C3/C4 count data
CAMCover_final <- CAM_species  %>% 
  left_join(totalSpecies_nDat) %>% 
  ## calculate the percentage of hits that are C3 vs C4
  mutate(CAM_hits_proportionOfAllSpp = CAM_n_hits/total_speciesCover_n) %>% 
  # drop data afor plots that don't have total plot 'n' values
  filter(!is.na(total_speciesCover_n))


# ggplot() + 
#   geom_point(data = indicatorDat, aes(Longitude_NAD83, Latitude_NAD83), col = "grey") + 
#   geom_point(data = CAMCover_final,  aes(Longitude_NAD83, Latitude_NAD83, col = CAM_hits_proportionOfAllSpp))

## shrubs
# get shrub species names 
shrub_species <- speciesDat_new %>% 
  filter(GrowthHabitSub  %in% c(#"Succulent", 
    "Shrub", "SubShrub", "Sub-Shrub", "Subshrub", 
                                "Shrub-Shrub")) %>% 
    filter(!(family %in% c("Crassulaceae", "Bromeliaceae", "Cactaceae",
                         "Anacampserotaceae", "Portulacaceae", "Talinaceae",
                         "Didiereaceae", "Halophytaceae", "Basellaceae"))) %>% 
  mutate(PrimaryKey_Year = paste(PrimaryKey, Year))
# %>% 
#filter(!is.na(AH_SpeciesCover))
# if there are NAs in the AH_SpeciesCover column, remove observations for that plot 
badPlots_shrub <- shrub_species %>% 
  #filter(is.na(AH_SpeciesCover)) %>% 
  # select(PrimaryKey, DBKey, Year) %>% 
  group_by(PrimaryKey, DBKey, Year) %>% 
  summarize(n_NAs = sum(is.na(AH_SpeciesCover_n)),
            n_NotNAs = sum(!is.na(AH_SpeciesCover_n))) %>% 
  mutate(percentNAs = n_NAs/(n_NAs + n_NotNAs)) %>% 
  filter(percentNAs > .33) %>% 
  select(PrimaryKey, DBKey, Year) %>% 
  unique() %>% 
  mutate(PrimaryKey_Year = paste(PrimaryKey, Year))

shrub_species <- shrub_species %>% 
  filter(!(shrub_species$PrimaryKey_Year %in% badPlots_shrub$PrimaryKey_Year))

# calculate the number of hits for shrub species in each plot 
shrub_species <- shrub_species %>% 
  group_by(PrimaryKey, Year) %>% 
  summarize(shrub_n_hits = sum(AH_SpeciesCover_n)) %>% 
  filter(!is.na(shrub_n_hits)) %>% 
  left_join(indicatorDat %>% select(PrimaryKey, DBKey, Latitude_NAD83, Longitude_NAD83))# select(PrimaryKey, DBKey, Year) %>% 
  
# add this information to the shrub count data
shrubCover_final <- shrub_species  %>% 
  left_join(totalSpecies_nDat) %>% 
  ## calculate the percentage of hits that are shrub
  mutate(shrub_hits_proportionOfAllSpp = shrub_n_hits/total_speciesCover_n) %>% 
  # drop data afor plots that don't have total plot 'n' values
  filter(!is.na(total_speciesCover_n))

## assume that if there are no species of a group recorded in a plot, the cover for that group is 0 

# Add all data pieces together  -------------------------------------------
# add together 
test <- grassCover_final %>% 
  full_join(treeCover_final, by = c("PrimaryKey", "Year")) %>% 
  #full_join(forbAnnPenCover, by = c("PrimaryKey", "Year")) %>% 
  full_join(CAMCover_final, by = c("PrimaryKey", "Year")) %>% 
  full_join(shrubCover_final, by = c("PrimaryKey", "Year"))

## add in shrub and forb data from "Indicators" d.f
datAll <- indicatorDat %>% 
  mutate(Year = lubridate::year(DateVisited)) %>% 
  select(rid, PrimaryKey, DBKey, ProjectKey, DateVisited, Latitude_NAD83, Longitude_NAD83,
         BareSoilCover, TotalFoliarCover, AH_ForbCover, AH_ShrubCover, 
         FH_CyanobacteriaCover, FH_DepSoilCover, FH_DuffCover, FH_EmbLitterCover,
         FH_HerbLitterCover, FH_LichenCover, FH_MossCover, FH_RockCover, FH_TotalLitterCover,
         FH_WoodyLitterCover, Year) %>% 
  full_join(test, by = c("PrimaryKey","Year") )

## for trees, CAM, shrub, and grass (calculated by species), we have the proportion of hits that are each given type.... want to convert to the amount of the total cover (rather than relative % of hits)
datAll <- datAll %>% 
  mutate(C3_coverByHand = C3_hits_proportionOfAllSpecies * TotalFoliarCover, 
         C4_coverByHand = C4_hits_proportionOfAllSpecies * TotalFoliarCover, 
         ConifTree_coverByHand = Conif_hits_proportionOfAllSpp * TotalFoliarCover, 
         AngioTree_coverByHand = Angio_hits_proportionOfAllSpp * TotalFoliarCover, 
         CAM_coverByHand = CAM_hits_proportionOfAllSpp* TotalFoliarCover, 
         Shrub_coverByHand = shrub_hits_proportionOfAllSpp * TotalFoliarCover
  )

# save data to file 
write.csv(datAll, 
          file = "./Data_raw//LandscapeDataCommonsDat/coverDat_use.csv",
          row.names = FALSE)

