#///////////////////
# Importing and exploring potential data sources - LANDFIRE reference data
# Alice Stears
# 04/15/2024
#///////////////////

# load packages -----------------------------------------------------------

library(tidyverse)
library(mdbr)
library(mapview)
#devtools::install_github("wcornwell/taxonlookup")
library(taxonlookup)

#Read in Data ------------------------------------------------------------
# there is an Microsoft access database for each of six regions of the US try
# loading the dtStands table: "This table contains lifeform cover and height
# data within the sampled unit. Table 9: Lifeform cover and height data within
# sample unit"
# nc_dat <- mdbr::read_mdb("./data/LANDFIRE_LFRDB/NC_Public_LFRDB_LF2.0.0/NC_Public_LFRDB_LF2.0.0.accdb",
#                          table = "dtStands")
# # get site locations: dtPoints
#   #This table contains location information for each sampled unit.
#   #Table 5: Sample unit location information
# nc_plots <- mdbr::read_mdb("./data/LANDFIRE_LFRDB/NC_Public_LFRDB_LF2.0.0/NC_Public_LFRDB_LF2.0.0.accdb",
#                            table = "dtPoints")
# # add info for data source
# nc_source <- mdbr::read_mdb("./data/LANDFIRE_LFRDB/NC_Public_LFRDB_LF2.0.0/NC_Public_LFRDB_LF2.0.0.accdb", 
#                             table = "dtVisits")
# # add plot data to the data
# nc_dat <- nc_dat %>%
#   left_join(nc_plots) %>% 
#   left_join(nc_source)
# 
# # loading the dtStands table: "This table contains lifeform cover and height
# # data within the sampled unit. Table 9: Lifeform cover and height data within
# # sample unit"
# ne_dat <- mdbr::read_mdb("./data/LANDFIRE_LFRDB/NE_Public_LFRDB_LF2.0.0/NE_Public_LFRDB_LF2.0.0.accdb",
#                          table = "dtStands")
# # get site locations: dtPoints
# #This table contains location information for each sampled unit.
# #Table 5: Sample unit location information
# ne_plots <- mdbr::read_mdb("./data/LANDFIRE_LFRDB/NE_Public_LFRDB_LF2.0.0/NE_Public_LFRDB_LF2.0.0.accdb",
#                            table = "dtPoints")
# # add info for data source
# ne_source <- mdbr::read_mdb("./data/LANDFIRE_LFRDB/NE_Public_LFRDB_LF2.0.0/NE_Public_LFRDB_LF2.0.0.accdb", 
#                             table = "dtVisits")
# # add plot data to the data
# ne_dat <- ne_dat %>%
#   left_join(ne_plots) %>% 
#   left_join(ne_source)
# 
# 
# # loading the dtStands table: "This table contains lifeform cover and height
# # data within the sampled unit. Table 9: Lifeform cover and height data within
# # sample unit"
# nw_dat <- mdbr::read_mdb("./data/LANDFIRE_LFRDB/NW_Public_LFRDB_LF2.0.0/NW_Public_LFRDB_LF2.0.0.accdb",
#                          table = "dtStands")
# #get site locations: dtPoints This table contains location information for each
# #sampled unit. Table 5: Sample unit location information doesn't have location
# #information for a lot of plots?? why?? Just have an 'x' in the location method
# #column, which means their location is unknown? not very helpful...
# nw_plots <- mdbr::read_mdb("./data/LANDFIRE_LFRDB/NW_Public_LFRDB_LF2.0.0/NW_Public_LFRDB_LF2.0.0.accdb",
#                            table = "dtPoints")
# # add info for data source
# nw_source <- mdbr::read_mdb("./data/LANDFIRE_LFRDB/NW_Public_LFRDB_LF2.0.0/NW_Public_LFRDB_LF2.0.0.accdb", 
#                             table = "dtVisits")
# 
# # add plot data to the data
# nw_dat <- nw_dat %>%
#   left_join(nw_plots)%>% 
#   left_join(nw_source, by = "EventID")
# 
# 
# # loading the dtStands table: "This table contains lifeform cover and height
# # data within the sampled unit. Table 9: Lifeform cover and height data within
# # sample unit"
# sc_dat <- mdbr::read_mdb("./data/LANDFIRE_LFRDB/SC_Public_LFRDB_LF2.0.0/SC_Public_LFRDB_LF2.0.0.accdb",
#                          table = "dtStands")
# # get site locations: dtPoints
# #This table contains location information for each sampled unit.
# #Table 5: Sample unit location information
# sc_plots <- mdbr::read_mdb("./data/LANDFIRE_LFRDB/SC_Public_LFRDB_LF2.0.0/SC_Public_LFRDB_LF2.0.0.accdb",
#                            table = "dtPoints")
# # add info for data source
# sc_source <- mdbr::read_mdb("./data/LANDFIRE_LFRDB/SC_Public_LFRDB_LF2.0.0/SC_Public_LFRDB_LF2.0.0.accdb", 
#                             table = "dtVisits")
# 
# #
# # add plot data to the data
# sc_dat <- sc_dat %>%
#   left_join(sc_plots) %>% 
#   left_join(sc_source)
# 
# # loading the dtStands table: "This table contains lifeform cover and height
# # data within the sampled unit. Table 9: Lifeform cover and height data within
# # sample unit"
# se_dat <- mdbr::read_mdb("./data/LANDFIRE_LFRDB/SE_Public_LFRDB_LF2.0.0/SE_Public_LFRDB_LF2.0.0.accdb",
#                          table = "dtStands")
# # get site locations: dtPoints
# #This table contains location information for each sampled unit.
# #Table 5: Sample unit location information
# se_plots <- mdbr::read_mdb("./data/LANDFIRE_LFRDB/SE_Public_LFRDB_LF2.0.0/SE_Public_LFRDB_LF2.0.0.accdb",
#                            table = "dtPoints")
# # add info for data source
# se_source <- mdbr::read_mdb("./data/LANDFIRE_LFRDB/SE_Public_LFRDB_LF2.0.0/SE_Public_LFRDB_LF2.0.0.accdb", 
#                             table = "dtVisits")
# # add plot data to the data
# se_dat <- se_dat %>%
#   left_join(se_plots) %>% 
#   left_join(se_source)
# 
# 
# # loading the dtStands table: "This table contains lifeform cover and height
# # data within the sampled unit. Table 9: Lifeform cover and height data within
# # sample unit"
# sw_dat <- mdbr::read_mdb("./data/LANDFIRE_LFRDB/SW_Public_LFRDB_LF2.0.0/SW_Public_LFRDB_LF2.0.0.accdb",
#                          table = "dtStands")
# # get site locations: dtPoints
# #This table contains location information for each sampled unit.
# #Table 5: Sample unit location information
# sw_plots <- mdbr::read_mdb("./data/LANDFIRE_LFRDB/SW_Public_LFRDB_LF2.0.0/SW_Public_LFRDB_LF2.0.0.accdb",
#                            table = "dtPoints")
# # add info for data source
# sw_source <- mdbr::read_mdb("./data/LANDFIRE_LFRDB/SW_Public_LFRDB_LF2.0.0/SW_Public_LFRDB_LF2.0.0.accdb", 
#                             table = "dtVisits")
# # add plot data to the data
# sw_dat <- sw_dat %>%
#   left_join(sw_plots) %>% 
#   left_join(sw_source)
# 
# ## put all the data into one table
# dat <- rbind(nc_dat, ne_dat, nw_dat, sc_dat, se_dat, sw_dat) %>%
#   filter(!is.na(Long))
# 
# # get codes for "SourceID"
# LUsource <- mdbr::read_mdb("./data/LANDFIRE_LFRDB/NC_Public_LFRDB_LF2.0.0/NC_Public_LFRDB_LF2.0.0.accdb", 
#                               table = "lutdtVisitsSourceID")
# # add source information to the d.f
# dat <- dat %>% 
#   left_join(LUsource, by = "SourceID")
# 
# ## save data for later
# write.csv(dat, file = "./data/LANDFIRE_LFRDB/coverDat_all.csv", row.names = FALSE)

dat <- read.csv("./data/LANDFIRE_LFRDB/coverDat_all.csv")

# what are the data sources? 
table(dat$Protocol)

mapview(unique(dat[,c("Long","Lat")]), xcol = "Long", ycol = "Lat", crs = "EPSG:4326", map.types = "OpenStreetMap")


#Get Species-level cover data (For tree type and c3/c4 grams) ------------
#loading the dtSpecies table: "This table lists and characterizes the plant
#species reported on the sampled unit."
nc_spp <- mdbr::read_mdb("./data/LANDFIRE_LFRDB/NC_Public_LFRDB_LF2.0.0/NC_Public_LFRDB_LF2.0.0.accdb",
                         table = "dtSpecies")
# add plot data to the data
nc_spp <- nc_spp %>%
  left_join(nc_plots) %>%
  left_join(nc_source)

#loading the dtSpecies table: "This table lists and characterizes the plant
#species reported on the sampled unit."
ne_spp <- mdbr::read_mdb("./data/LANDFIRE_LFRDB/NE_Public_LFRDB_LF2.0.0/NE_Public_LFRDB_LF2.0.0.accdb",
                         table = "dtSpecies")
# add plot data to the data
ne_spp <- ne_spp %>%
  left_join(ne_plots) %>%
  left_join(ne_source)

#loading the dtSpecies table: "This table lists and characterizes the plant
#species reported on the sampled unit."
nw_spp <- mdbr::read_mdb("./data/LANDFIRE_LFRDB/NW_Public_LFRDB_LF2.0.0/NW_Public_LFRDB_LF2.0.0.accdb",
                         table = "dtSpecies")
# add plot data to the data
nw_spp <- nw_spp %>%
  left_join(nw_plots) %>%
  left_join(nw_source)

#loading the dtSpecies table: "This table lists and characterizes the plant
#species reported on the sampled unit."
sc_spp <- mdbr::read_mdb("./data/LANDFIRE_LFRDB/SC_Public_LFRDB_LF2.0.0/SC_Public_LFRDB_LF2.0.0.accdb",
                         table = "dtSpecies")
# add plot data to the data
sc_spp <- sc_spp %>%
  left_join(sc_plots) %>%
  left_join(sc_source)

#loading the dtSpecies table: "This table lists and characterizes the plant
#species reported on the sampled unit."
se_spp <- mdbr::read_mdb("./data/LANDFIRE_LFRDB/SE_Public_LFRDB_LF2.0.0/SE_Public_LFRDB_LF2.0.0.accdb",
                         table = "dtSpecies")
# add plot data to the data
se_spp <- se_spp %>%
  left_join(se_plots) %>%
  left_join(se_source)

#loading the dtSpecies table: "This table lists and characterizes the plant
#species reported on the sampled unit."
sw_spp <- mdbr::read_mdb("./data/LANDFIRE_LFRDB/SW_Public_LFRDB_LF2.0.0/SW_Public_LFRDB_LF2.0.0.accdb",
                         table = "dtSpecies")
# add plot data to the data
sw_spp <- sw_spp %>%
  left_join(sw_plots) %>%
  left_join(sw_source)

sppDat <- rbind(nc_spp, ne_spp, nw_spp, sc_spp, se_spp, sw_spp)

# add source information to the d.f
sppDat <- sppDat %>%
  left_join(LUsource, by = "SourceID")

## save data for later
write.csv(sppDat, file = "./data/LANDFIRE_LFRDB/speciesCoverDat_all.csv", row.names = FALSE)
## use "sppDat$LFAbsCov", the absolute cover derived by LANDFIRE from the source dataset

#### get data for c3 vs c4 grasses ####
# load graminoid species names
gramSpp <- unique(sppDat[sppDat$Lifeform == "G",c("Item", "SciName")])
names(gramSpp) <- c("sppCode_dataset", "SciName_dataset")
# load table of USDA PLANTS names
gramPhotoDat <- read.csv("./data/USDA_plants_SppNameTable_synonymNames.csv")
# add species names to dataset (from USDA PLANTS)
gramDat <- left_join(gramSpp, gramPhotoDat, by = c("sppCode_dataset" = "AcceptedSymbol"))
# get data about photosynthetic pathway that we already have
pathwaysDat <- read.csv("./data/USDA_plants_SppNameTable_allGrassInDataset.csv")
test <- left_join(gramDat, pathwaysDat, by = c("sppCode_dataset" = "Species", "ScientificName", "Status", "SynonymSymbol", "CommonName"))
# write so that I can update photosynthetic pathway info
write.csv(test, "./data/LANDFIRE_c3c4Names_temp.csv", row.names = FALSE)
# read back in the updated data
pathway <- read.csv("./data/LANDFIRE_c3c4Names.csv") %>% 
    select(sppCode_dataset, SciName_dataset, PhotosyntheticPathway) %>% 
  unique()

## add the updated photosynthetic pathway information onto the graminoid data
# get graminoid data
graminoidData <- sppDat[sppDat$Lifeform == "G",]
# add photosynthesis data
graminoidData <- left_join(graminoidData, pathway, by = c("Item" = "sppCode_dataset", 
                                                          "SciName" = "SciName_dataset"))
# break into C3 vs C4 (data by species observation, not summed by plot yet)
C3Dat_spp <- graminoidData[graminoidData$PhotosyntheticPathway=="C3" & !is.na(graminoidData$PhotosyntheticPathway),]
C4Dat_spp <- graminoidData[graminoidData$PhotosyntheticPathway=="C4" & !is.na(graminoidData$PhotosyntheticPathway),]

## now, sum % cover by plot 
C3Dat <- C3Dat_spp %>% 
  group_by(EventID) %>% 
  summarize(C3_LFAbsCov = sum(LFAbsCov),
            C3_LFRelCov = sum(LFRelCov), 
            C3_LFHgt = mean(LFHgt),
            Lat = mean(Lat), 
            Long = mean(Long),
            LFX = mean(LFX), 
            LFY = mean(LFY), 
            LFZone = unique(LFZone),
            AKRID = unique(AKRID),
            VPU = unique(VPU),
            GeoArea = unique(GeoArea),
            LocMeth = unique(LocMeth),
            YYYY = mean(YYYY), 
            MM = unique(MM),
            DD = unique(DD),
            DDD = unique(DDD),
            Type = unique(Type), 
            SourceID = unique(SourceID))

C4Dat <- C4Dat_spp %>% 
  group_by(EventID) %>% 
  summarize(C4_LFAbsCov = sum(LFAbsCov),
            C4_LFRelCov = sum(LFRelCov), 
            C4_LFHgt = mean(LFHgt),
            Lat = mean(Lat), 
            Long = mean(Long),
            LFX = mean(LFX), 
            LFY = mean(LFY), 
            LFZone = unique(LFZone),
            AKRID = unique(AKRID),
            VPU = unique(VPU),
            GeoArea = unique(GeoArea),
            LocMeth = unique(LocMeth),
            YYYY = mean(YYYY), 
            MM = unique(MM),
            DD = unique(DD),
            DDD = unique(DDD),
            Type = unique(Type), 
            SourceID = unique(SourceID))

#### get data for deciduous vs coniferous trees ####
## decided to do this taxonomically (i.e. gymnosperms vs angiosperms)
#can  get taxonomic data using the "taxonlookup" R package\

# get unique tree species names
treeNames <- unique(sppDat[sppDat$Lifeform == "T",c("Item", "SciName")])
names(treeNames) <- c("sppCode_dataset", "SciName_dataset")
# lookup species names 
treeGroups <- taxonlookup::lookup_table(species_list = treeNames$SciName_dataset, by_species = TRUE)
treeGroups$Species <- row.names(treeGroups)

# get the data for all trees, and add the "group" data
treeDat <- sppDat[sppDat$Lifeform == "T" & !is.na(sppDat$Lifeform), ] %>% 
  left_join(treeGroups, by = c("SciName" = "Species"))
# "pinyon pine", "Hesperocyparis arizonica", "Unknown conifer", "Unknown conifer
# tree", "Unknown conifer tree 1", "Hesperocyparis forbesii", "Hesperocyparis
# macnabiana", "Pinaceae", "Hesperocyparis sargentii", "Hesperocyparis
# macrocarpa", "Hesperocyparis pigmaea"-- need a "gymnosperm" label
treeDat[treeDat$SciName %in% c("pinyon pine", "Pinyon pine", "Hesperocyparis arizonica", 
                               "Unknown conifer", "Unknown conifer tree", "Unknown conifer tree 1", 
                               "Hesperocyparis forbesii", "Hesperocyparis macnabiana", 	
                               "Hesperocyparis macnabiana", "Pinaceae", "Hesperocyparis sargentii", 
                               "Hesperocyparis macrocarpa", "Hesperocyparis pigmaea"), "group"] <- "Gymnosperms"

# "Unknown deciduous tree", "Juglandaceae" -- needs an "angiosperm" label
treeDat[treeDat$SciName %in% c("Unknown deciduous tree", "Juglandaceae"), "group"] <- "Angiosperms"

# break into C3 vs C4 (data by species observation, not summed by plot yet)
AngioTreeDat_spp <- treeDat[treeDat$group=="Angiosperms" & !is.na(treeDat$group),]
ConifTreeDat_spp <- treeDat[treeDat$group=="Gymnosperms" & !is.na(treeDat$group),]

## now, sum % cover by plot 
AngioTreeDat <- AngioTreeDat_spp %>% 
  group_by(EventID) %>% 
  summarize(AngioTree_LFAbsCov = sum(LFAbsCov),
            AngioTree_LFRelCov = sum(LFRelCov), 
            AngioTree_LFHgt = mean(LFHgt),
            Lat = mean(Lat), 
            Long = mean(Long),
            LFX = mean(LFX), 
            LFY = mean(LFY), 
            LFZone = unique(LFZone),
            AKRID = unique(AKRID),
            VPU = unique(VPU),
            GeoArea = unique(GeoArea),
            LocMeth = unique(LocMeth),
            YYYY = mean(YYYY), 
            MM = unique(MM),
            DD = unique(DD),
            DDD = unique(DDD),
            Type = unique(Type), 
            SourceID = unique(SourceID))

ConifTreeDat <- ConifTreeDat_spp %>% 
  group_by(EventID) %>% 
  summarize(ConifTree_LFAbsCov = sum(LFAbsCov),
            ConifTree_LFRelCov = sum(LFRelCov), 
            ConifTree_LFHgt = mean(LFHgt),
            Lat = mean(Lat), 
            Long = mean(Long),
            LFX = mean(LFX), 
            LFY = mean(LFY), 
            LFZone = unique(LFZone),
            AKRID = unique(AKRID),
            VPU = unique(VPU),
            GeoArea = unique(GeoArea),
            LocMeth = unique(LocMeth),
            YYYY = mean(YYYY), 
            MM = unique(MM),
            DD = unique(DD),
            DDD = unique(DDD),
            Type = unique(Type), 
            SourceID = unique(SourceID))

#### put species-level calculations together with other functional group data ####
temp <- full_join(C3Dat, C4Dat) %>% 
  full_join(AngioTreeDat) %>% 
  full_join(ConifTreeDat) %>% 
  mutate(MM = as.integer(MM), 
         DD = as.integer(DD),
         DDD = as.integer(DDD))

datAll <- dat %>% 
  select(-VPU, -GeoArea, -LocMeth, -Photo1, -Photo2, -Photo3, -Photo4, 
         -PublicAccess, -AgencyCd, -Locality, -SourceCitation, -Website) %>% 
  left_join(temp)

## hand calculated tree absolute cover data seems to track quite well with the functional-group data, with a few exceptions

# visualize differences in % cover data between groups
ggplot(datAll) +
  geom_density(aes(LFConiferTreeCov), col = "red") +
  geom_density(aes(ConifTree_LFAbsCov), col = "darkgreen")


ggplot(datAll) +
  geom_density(aes(ConifTree_LFAbsCov), col = "darkgreen") +
  geom_density(aes(AngioTree_LFAbsCov), col = "green") + 
  geom_density(aes(C3_LFAbsCov), col = "orange") + 
  geom_density(aes(C4_LFAbsCov), col = "red") +
  geom_density(aes(LFShrubCov), col = "blue") +
  geom_density(aes(LFHerbCov), col = "purple") +
  theme_dark() +
  xlim(0,200)

## trim down the dataset just to the variables we really want for analysis
datLF_use <- datAll  %>% 
  select(EventID, Lat, Long, LFX, LFY, LFCoordSys, LFZone, YYYY, MM, DD, DDD,
         LFShrubCov, LFHerbCov, C3_LFAbsCov, C4_LFAbsCov, AngioTree_LFAbsCov, ConifTree_LFAbsCov)

# remove rows for plots that don't have all functional groups measured (can figure out by finding which have NAs in the 'dat' data frame)
goodPlots <- datAll %>% 
 filter(!is.na(LFTreeCov) & 
           !is.na(LFShrubCov) & 
           !is.na(LFHerbCov)) %>% 
  select(EventID)


## subset the datLF_use dataframe to have only plots where all functional groups were measured (have accurate zeros)
datLF_use  <- datLF_use %>% 
  filter(EventID %in% goodPlots$EventID)
# replace NAs w/ zeros 
datLF_use[is.na(datLF_use$C3_LFAbsCov) , "C3_LFAbsCov"] <- 0
datLF_use[is.na(datLF_use$C4_LFAbsCov) , "C4_LFAbsCov"] <- 0
datLF_use[is.na(datLF_use$ConifTree_LFAbsCov) , "ConifTree_LFAbsCov"] <- 0
datLF_use[is.na(datLF_use$AngioTree_LFAbsCov) , "AngioTree_LFAbsCov"] <- 0

#save data to file 
write.csv(datLF_use,"./data/LANDFIRE_LFRDB/coverDat_USE.csv", row.names = FALSE)

ggplot(datLF_use) +
  geom_density(aes(ConifTree_LFAbsCov), col = "darkgreen") +
  geom_density(aes(AngioTree_LFAbsCov), col = "green") + 
  geom_density(aes(C3_LFAbsCov), col = "orange") + 
  geom_density(aes(C4_LFAbsCov), col = "red") +
  geom_density(aes(LFShrubCov), col = "blue") +
  geom_density(aes(LFHerbCov), col = "purple") +
  theme_dark() +
  xlim(0,200)

test <- sf::st_as_sf(datLF_use, coords = c("Long", "Lat"), crs = "EPSG:4326")
# get basemap
library(maps)
usa1 <- sf::st_as_sf(map('usa', plot = FALSE, fill = TRUE))

usa2 <- sf::st_transform(
  usa1,
  dst = st_crs(test)
)

# visualize the data spatially to ensure it makes sense
ggplot(test) + 
  geom_sf(data = usa2, aes()) +
  geom_sf(aes(alpha= C4_LFAbsCov)) 
  