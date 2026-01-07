#///////////////////
# Averaging values from plots w/in each dayMet gridcell
# Alice Stears
# 8/19/24
#///////////////////


# clear environment -------------------------------------------------------

rm(list=ls()) 

# Load packages -----------------------------------------------------------

library(tidyverse)
library(sf)
library(terra)
library(exactextractr)
library(stars)
library(lwgeom)
library(tidyterra)
library(ggpubr)


# Load data ---------------------------------------------------------------

# get veg data
dat_temp <- readRDS("./Data_processed/CoverData/DataForModels.RDS") 
# dayMet extent 
test <-  rast("./Data_raw/dayMet/rawMonthlyData/orders/70e0da02b9d2d6e8faa8c97d211f3546/Daymet_Monthly_V4R1/data/daymet_v4_prcp_monttl_na_1980.tif") %>% 
  terra::project(crs(dat_temp))

layerNames <- c("ShrubCover", "TotalHerbaceousCover",  "TotalTreeCover", "C3GramCover_prop", "C4GramCover_prop",
                "ForbCover_prop", "AngioTreeCover_prop", "ConifTreeCover_prop",
                "BareGroundCover")

# add met data to veg data ------------------------------------------------
# make veg data projection the same as raster data
v <- vect(st_drop_geometry(dat_temp)[,c("Lon", "Lat")], geom = c("Lon", "Lat"), crs=crs(dat_temp))
y <- project(v, crs(test))
# make sure the veg data is in the appropriate projection
dat <- dat_temp %>%
  #st_buffer(.01) %>%
  #terra::vect() #%>%
  sf::st_transform(crs(y)) %>% 
  # st_buffer(.1)
  st_centroid() %>% 
  mutate("Year_char" = as.character(Year)) %>% 
  select(Year, Year_char, UniquID:Day, ShrbCvr:TtlHrbC, BrGrndC, LttrCvr, LttrDpt,TrBsA_2, AngTrC_:Lat, Source, newRegion, geometry, rowID)


# make sure the raster data is in the appropriate projection
test <- test %>%
  terra::project(crs(y))
st_crs(dat) == st_crs(test)

## rename the columns in 'dat' 
# fix names
dat  <- dat %>%
  rename(UniqueID = UniquID,
         StateUnitCounty = SttUntC,
         PlotCondition = PltCndt,
         ShrubCover = ShrbCvr,
         TotalHerbaceousCover = TtlHrbC,
         TotalTreeCover = TtlTrCv,
         TotalGramCover = TtlGrmC,
         AnnualHerbGramCover = AnnlHGC,
         PerennialHerbGramCover = PrnnHGC,
         C3GramCover = C3GrmCv,
         C3GramCover_prop = C3GrmC_,
         C4GramCover = C4GrmCv,
         C4GramCover_prop = C4GrmC_,
         ForbCover = ForbCvr,
         ForbCover_prop = FrbCvr_,
         AngioTreeCover = AngTrCv,
         AngioTreeCover_prop = AngTrC_,
         ConifTreeCover = CnfTrCv,
         ConifTreeCover_prop = CnfTrC_,
         TreeBasalArea = TrBsA_2,
         BareGroundCover = BrGrndC,
         LitterCover = LttrCvr,
         LitterDepth = LttrDpt)

dat <- ungroup(dat)

dat <- dat %>% 
  filter(Year >= 2000) #%>% 
#slice_sample(n = 100000)
# dat %>% 
#   st_drop_geometry()  %>% 
#   filter(!is.na(TotalHerbaceousCover)) %>% 
#   ggplot() + 
#   facet_wrap(~Year) + 
#   geom_point(aes(x = Lon, y = Lat))

## for RAP data, remove the points left over on the very far east coast that somehow are left over after the filtering process to remove forests
dat <- dat[!(dat$Source == "RAP" & 
                 dat$Lon > -85),]

dat2 <- dat
# # calculate the average distance between points in each dataset/year --------
# yearDataset <- unique(dat2 %>%
#                         st_drop_geometry() %>% 
#                         select(Year, Source))
# dataTypes <- c("ShrubCover",  "ForbCover", "TotalGramCover", "C3GramCover", "C4GramCover", "AngioTreeCover", "ConifTreeCover", "TotalTreeCover", "TotalHerbaceousCover", "BareGroundCover")
# 
# dat2_avgDist <- apply(X = yearDataset, MARGIN = 1, FUN = function(x) {
#   lapply(X = dataTypes, FUN = function(y) {
#     ## get the distance between each point in the given data source/year combination
#     tempDat <- dat2 %>% 
#       filter((Year == x[1]) & (Source == x[2])) %>% 
#       filter(!is.na(.data[[y]])) %>% 
#       sf::st_distance() 
#     ## replace zeros on the diagonal of the matrix with NAs 
#     diag(tempDat) <- NA
#     # for each observation (row), find the minimum value (distance to nearest neighbor)
#     tempDat2 <- apply(X = tempDat, MARGIN = 1, FUN = function(x) {min(x, na.rm = TRUE)})
#     ## calculate the mean distance between points in meters
#     avgDist <- mean(tempDat2, na.rm = TRUE)
#     ## save output
#     return(data.frame(Year = x[1], Source = x[2], DataType = y, avgDist = avgDist))
#   }
#   )
# })
# 
# dat2_avgDist_df <- list_rbind(lapply(dat2_avgDist, FUN = purrr::list_rbind))
# # save intermediate data
# saveRDS(dat2_avgDist_df, "./Data_processed/CoverData/tableOfSamplingDensity.rds")
# 
# ## get the distribution of values w/in each dataset (how much does the average distance between points vary across years)
# dat2_avgDist_df %>% 
#   filter(is.finite(avgDist)) %>% 
#   ggplot() + 
#   facet_wrap(~Source, scales = "free") +
#   geom_density(aes(x = avgDist, colour = Source)) + 
#   geom_vline(data = dat2_avgDist_df3, aes(xintercept = meanDist, col = Source), linetype = 1) + 
#   geom_vline(data = dat2_avgDist_df3, aes(xintercept = medianDist, col = Source), linetype = 3) +
#   ggtitle("Distribution of annual average distance \n between data points, by data source", 
#           subtitle = "Solid line = mean value \n Dashed line = median value") +
#   theme_classic()
# ## median distance between points in each dataset (across all years) 
# dat2_avgDist_df2 <- dat2_avgDist_df %>% 
#   filter(is.finite(avgDist)) %>% 
#   group_by(Source, DataType) %>% 
#   reframe(meanDist = mean(avgDist, na.rm = TRUE),
#           medianDist = median(avgDist, na.rm = TRUE)) %>% 
#   pivot_wider(names_from = Source, 
#               values_from = c(meanDist, medianDist))
# 
# ## get median distance between points across all data types and years 
# dat2_avgDist_df3 <- dat2_avgDist_df %>% 
#   filter(is.finite(avgDist)) %>% 
#   group_by(Source) %>% 
#   reframe(meanDist = mean(avgDist, na.rm = TRUE),
#           medianDist = median(avgDist, na.rm = TRUE))
# OPTION to filter out some of the RAP data  ------------------------------
# dat2_noRAP <- dat2 %>%
#   filter(Source != "RAP")
# 
# dat2_RAPonly <- dat2 %>%
#   filter(Source == "RAP")
# 
# # chose 1000 random locations of the ~3750 in the RAP dataset
# set.seed("12011993")
# RAPlocations_want <- dat2_RAPonly %>%
#   st_drop_geometry() %>%
#   select(Lon, Lat) %>%
#   unique() %>%
#   slice_sample(n = 1000)
# dat2_RAPonly_want <- dat2_RAPonly %>%
#   filter((Lon %in% RAPlocations_want$Lon) &
#            (Lat %in% RAPlocations_want$Lat))
# 
# # dat2_RAPonly %>%
# #   ggplot() +
# #   geom_point(aes(x = Lon, y = Lat), pch = 19) +
# #   geom_point(data = dat2_RAPonly_want, aes(x = Lon, y = Lat), col = "red", pch = 20)
# 
# # add the sampled RAP data onto the unsampled RAP data
# dat2 <- dat2_noRAP %>%
#   rbind(dat2_RAPonly_want)

# # OPTION to filter out some of the LANDFIRE data  ------------------------------
dat2_noLF <- dat2 %>%
  filter(Source != "LANDFIRE")

dat2_LFonly <- dat2 %>%
  filter(Source == "LANDFIRE")

## aggregate the dayMet grid up by 4
test_big <- test %>%
  terra::aggregate(fact = 3, fun = "mean") 

# area of these cells (in m)
#plot(test_big$daymet_v4_prcp_monttl_na_1980_1)
# terra::cellSize(test)
# terra::cellSize(test_big)

## add the aggregated dayMet ID to each LF observation
dat2_LFonly_cellID <- dat2_LFonly %>%
  cbind(terra::extract(x = test_big$daymet_v4_prcp_monttl_na_1980_1, y = dat2_LFonly, cells = TRUE, ID = FALSE, xy = TRUE)[,2:4])
dat2_LFonly_cellID$uniqueRowID <- 1:nrow(dat2_LFonly_cellID)
# combine year and cellID info into one column
dat2_LFonly_cellID$cellID_year <- paste0(dat2_LFonly_cellID$cell, "_", dat2_LFonly_cellID$Year)

## if, for a given data type in a given year, there are > 1 observation in an aggregated grid cell, average the values together
testTest <- lapply(X = layerNames, FUN = function(x) {
  ## get data just for the focal cover type
  temp <- dat2_LFonly_cellID[,c("Year", "Lat", "Lon", x, "cell", "uniqueRowID", "cellID_year")] %>%
    st_drop_geometry() %>%
    drop_na()
  if (nrow(temp) > 0) {
    ## get counts of the number of observations within an aggregated cell in the same year
    temp2 <- temp %>%
      group_by(Year, cell, cellID_year) %>%
      summarise(cell_n = n())
    # get names of those cells/years that only have one observations
    singleCellYears <- temp2[temp2$cell_n==1, c("cellID_year")]
    # get names of those cells/years that have more than one observation
    dupCellsYears <- temp2[temp2$cell_n!=1, c("cellID_year")]
    
    ## keep all of the cells that have only 1 observation in a given year
    out_singleObs <- temp %>%
      filter(cellID_year %in% singleCellYears$cellID_year)
    
    ## for those cells that have >1 observation in a given year, average across those observations
    # names of duplicated rows
    dupCellKeepIDs <- apply(dupCellsYears, MARGIN = 1, FUN = function(y) {
      temp3 <- temp %>%
        filter(cellID_year == y["cellID_year"]
        ) %>%
        summarize(Year = mean(Year), 
                  Lat = mean(Lat), 
                  Lon = mean(Lon), 
                  x = mean(.data[[x]]), 
                  cell = mean(cell), 
                  uniqueRowID = NA, 
                  cellID_year = unique(cellID_year)) 
      names(temp3)[4] <- x
      #slice_sample(n = 1) %>%
      #select(uniqueRowID)
      return(temp3)
    }
    )
    out_multObs <- dupCellKeepIDs %>% 
      purrr::list_rbind()
    # temp %>%
    # filter(uniqueRowID %in% dupCellKeepIDs)
    
    ## put together the single observation cells and the duplicate observation cells and save
    out <- out_singleObs %>%
      rbind(out_multObs)
  }  
  
})

# get data that will be joined to the filtered data 
tempTemp <- dat2_LFonly_cellID %>% select(Year, cell, cellID_year, newRegion) %>% 
  st_drop_geometry() %>% unique() %>% group_by(Year, cell, cellID_year) %>% 
  summarise(newRegion = paste0(.data[["newRegion"]], collapse = "_")) %>% 
  mutate(newRegion = replace(newRegion, newRegion == "dryShrubGrass_eastForest", "dryShrubGrass"), 
         newRegion = replace(newRegion, newRegion == "dryShrubGrass_NA", "dryShrubGrass"), 
         newRegion = replace(newRegion, newRegion == "dryShrubGrass_westForest", "dryShrubGrass"), 
         newRegion = replace(newRegion, newRegion == "eastForest_dryShrubGrass", "dryShrubGrass"), 
         newRegion = replace(newRegion, newRegion == "eastForest_NA", "eastForest"),  
         newRegion = replace(newRegion, newRegion == "NA_dryShrubGrass", "dryShrubGrass"), 
         newRegion = replace(newRegion, newRegion == "NA_eastForest", "eastForest"), 
         newRegion = replace(newRegion, newRegion == "westForest_dryShrubGrass", "dryShrubGrass")
         )
                                       
dat2_LFonly_filtered <- testTest[[1]] %>%
  full_join(testTest[[2]], by = c("Year", "Lat", "Lon", "cell", "uniqueRowID", "cellID_year")) %>%
  full_join(testTest[[3]], by = c("Year", "Lat", "Lon", "cell", "uniqueRowID", "cellID_year")) %>%
  full_join(testTest[[4]], by = c("Year", "Lat", "Lon", "cell", "uniqueRowID", "cellID_year")) %>%
  full_join(testTest[[5]], by = c("Year", "Lat", "Lon", "cell", "uniqueRowID", "cellID_year")) %>%
  full_join(testTest[[6]], by = c("Year", "Lat", "Lon", "cell", "uniqueRowID", "cellID_year")) %>%
  full_join(testTest[[7]], by = c("Year", "Lat", "Lon", "cell", "uniqueRowID", "cellID_year")) %>%
  full_join(testTest[[8]], by = c("Year", "Lat", "Lon", "cell", "uniqueRowID", "cellID_year")) %>%
  #full_join(testTest[[9]], by = c("Year", "Lat", "Lon", "cell", "uniqueRowID", "cellID_year")) %>%
  left_join(tempTemp, by = c("Year", "cell", "cellID_year"
                                                    )) %>%
  mutate(BareGroundCover = NA, 
         Year_char = as.character(Year), 
         UniqueID = NA, 
         StateUnitCounty = NA, 
         Plot = NA, 
         PlotCondition = NA, 
         Month = NA, 
         Day = NA,
         burnedMoreThan20YearsAgo = NA,
         group = NA, 
         NA_L2CODE = NA,
         NA_L2NAME = NA,
         NA_L1CODE = NA,              
         NA_L1NAME = NA,
         NA_L2KEY = NA,
         NA_L1KEY = NA,
         Shape_Leng = NA,
         Shape_Area = NA, 
         Source = "LANDFIRE") %>% 
  sf::st_as_sf(coords = c("Lon", "Lat"), remove = FALSE, crs = sf::st_crs(dat2)) %>% 
  mutate(rowID = NA) %>% 
  select(names(dat2_noLF %>% select(-c(ForbCover, TotalGramCover, AnnualHerbGramCover, PerennialHerbGramCover, C3GramCover, C4GramCover,
                                       AngioTreeCover, ConifTreeCover, TreeBasalArea, LitterCover, LitterDepth))), 'cell') 

 # add the sampled LF data onto the un-sampled LF data
dat2 <- dat2_noLF %>%
  select(-c(ForbCover, TotalGramCover, AnnualHerbGramCover, PerennialHerbGramCover, C3GramCover, C4GramCover,
            AngioTreeCover, ConifTreeCover, TreeBasalArea, LitterCover, LitterDepth)) %>%
  rbind(dat2_LFonly_filtered %>% select(-cell))

# # # OPTION to filter out some of the AIM data  ------------------------------
# dat2_noAIM <- dat2 %>%
#   filter(Source != "LDC")
# 
# dat2_AIMonly <- dat2 %>%
#   filter(Source == "LDC")
# 
# ## aggregate the dayMet grid up by 13
# test_big <- test %>%
#   terra::aggregate(fact = 13, fun = "mean") 
# 
# # area of these cells (in m)
# #plot(test_big$daymet_v4_prcp_monttl_na_1980_1)
# # terra::cellSize(test)
# # terra::cellSize(test_big)
# 
# ## add the aggregated dayMet ID to each AIM observation
# dat2_AIMonly_cellID <- dat2_AIMonly %>%
#   cbind(terra::extract(x = test_big$daymet_v4_prcp_monttl_na_1980_1, y = dat2_AIMonly, cells = TRUE, ID = FALSE, xy = TRUE)[,2:4])
# dat2_AIMonly_cellID$uniqueRowID <- 1:nrow(dat2_AIMonly_cellID)
# # combine year and cellID info into one column
# dat2_AIMonly_cellID$cellID_year <- paste0(dat2_AIMonly_cellID$cell, "_", dat2_AIMonly_cellID$Year)
# 
# ## if, for a given data type in a given year, there are > 1 observation in an aggregated grid cell, average the values together
# testTest <- lapply(X = layerNames, FUN = function(x) {
#   ## get data just for the focal cover type
#   temp <- dat2_AIMonly_cellID[,c("Year", "Lat", "Lon", x, "cell", "uniqueRowID", "cellID_year")] %>%
#     st_drop_geometry() %>%
#     drop_na()
#   if (nrow(temp) > 0) {
#     ## get counts of the number of observations within an aggregated cell in the same year
#     temp2 <- temp %>%
#       group_by(Year, cell, cellID_year) %>%
#       summarise(cell_n = n())
#     # get names of those cells/years that only have one observations
#     singleCellYears <- temp2[temp2$cell_n==1, c("cellID_year")]
#     # get names of those cells/years that have more than one observation
#     dupCellsYears <- temp2[temp2$cell_n!=1, c("cellID_year")]
#     
#     ## keep all of the cells that have only 1 observation in a given year
#     out_singleObs <- temp %>%
#       filter(cellID_year %in% singleCellYears$cellID_year)
#     
#     ## for those cells that have >1 observation in a given year, average across those observations
#     # names of duplicated rows
#     dupCellKeepIDs <- apply(dupCellsYears, MARGIN = 1, FUN = function(y) {
#       temp3 <- temp %>%
#         filter(cellID_year == y["cellID_year"]
#         ) %>%
#         summarize(Year = mean(Year), 
#                   Lat = mean(Lat), 
#                   Lon = mean(Lon), 
#                   x = mean(.data[[x]]), 
#                   cell = mean(cell), 
#                   uniqueRowID = NA, 
#                   cellID_year = unique(cellID_year)) 
#       names(temp3)[4] <- x
#       #slice_sample(n = 1) %>%
#       #select(uniqueRowID)
#       return(temp3)
#     }
#     )
#     out_multObs <- dupCellKeepIDs %>% 
#       purrr::list_rbind()
#     # temp %>%
#     # filter(uniqueRowID %in% dupCellKeepIDs)
#     
#     ## put together the single observation cells and the duplicate observation cells and save
#     out <- out_singleObs %>%
#       rbind(out_multObs)
#   }  
#   
# })
# 
# # get data that will be joined to the filtered data 
# tempTemp <- dat2_AIMonly_cellID %>% select(Year, cell, cellID_year, newRegion) %>% 
#   st_drop_geometry() %>% unique() %>% group_by(Year, cell, cellID_year) %>% 
#   summarise(newRegion = paste0(.data[["newRegion"]], collapse = "_")) %>% 
#   mutate(newRegion = replace(newRegion, newRegion == "dryShrubGrass_eastForest", "dryShrubGrass"), 
#          newRegion = replace(newRegion, newRegion == "dryShrubGrass_NA", "dryShrubGrass"), 
#          newRegion = replace(newRegion, newRegion == "dryShrubGrass_westForest", "dryShrubGrass"), 
#          newRegion = replace(newRegion, newRegion == "eastForest_dryShrubGrass", "dryShrubGrass"), 
#          newRegion = replace(newRegion, newRegion == "eastForest_NA", "eastForest"),  
#          newRegion = replace(newRegion, newRegion == "NA_dryShrubGrass", "dryShrubGrass"), 
#          newRegion = replace(newRegion, newRegion == "NA_eastForest", "eastForest"), 
#          newRegion = replace(newRegion, newRegion == "westForest_dryShrubGrass", "dryShrubGrass")
#   )
# 
# dat2_AIMonly_filtered <- testTest[[1]] %>%
#   full_join(testTest[[2]], by = c("Year", "Lat", "Lon", "cell", "uniqueRowID", "cellID_year")) %>%
#   full_join(testTest[[3]], by = c("Year", "Lat", "Lon", "cell", "uniqueRowID", "cellID_year")) %>%
#   full_join(testTest[[4]], by = c("Year", "Lat", "Lon", "cell", "uniqueRowID", "cellID_year")) %>%
#   full_join(testTest[[5]], by = c("Year", "Lat", "Lon", "cell", "uniqueRowID", "cellID_year")) %>%
#   full_join(testTest[[6]], by = c("Year", "Lat", "Lon", "cell", "uniqueRowID", "cellID_year")) %>%
#   full_join(testTest[[7]], by = c("Year", "Lat", "Lon", "cell", "uniqueRowID", "cellID_year")) %>%
#   full_join(testTest[[8]], by = c("Year", "Lat", "Lon", "cell", "uniqueRowID", "cellID_year")) %>%
#   #full_join(testTest[[9]], by = c("Year", "Lat", "Lon", "cell", "uniqueRowID", "cellID_year")) %>%
#   left_join(tempTemp, by = c("Year", "cell", "cellID_year"
#   )) %>%
#   mutate(BareGroundCover = NA, 
#          Year_char = as.character(Year), 
#          UniqueID = NA, 
#          StateUnitCounty = NA, 
#          Plot = NA, 
#          PlotCondition = NA, 
#          Month = NA, 
#          Day = NA,
#          burnedMoreThan20YearsAgo = NA,
#          group = NA, 
#          NA_L2CODE = NA,
#          NA_L2NAME = NA,
#          NA_L1CODE = NA,              
#          NA_L1NAME = NA,
#          NA_L2KEY = NA,
#          NA_L1KEY = NA,
#          Shape_Leng = NA,
#          Shape_Area = NA, 
#          Source = "LDC") %>% 
#   sf::st_as_sf(coords = c("Lat", "Lon"), remove = FALSE, crs = sf::st_crs(dat2)) %>% 
#   select(names(dat2_noAIM), 'cell') 
# 
# # add the sampled AIM data onto the un-sampled AIM data
# dat2 <- dat2_noAIM %>%
#   rbind(dat2_AIMonly_filtered %>% select(-cell))
# 

# # Calculate distance between 'coarisified' LANDFIRE data ---------------------------
# yearDataset <- unique(dat2 %>%
#                         st_drop_geometry() %>%
#                         filter(Source == "LANDFIRE") %>% 
#                         select(Year, Source))
# dataTypes <- c("ShrubCover",  "ForbCover_prop", "C3GramCover_prop", "C4GramCover_prop", 
#                "AngioTreeCover_prop", "ConifTreeCover_prop", "TotalTreeCover", "TotalHerbaceousCover", "BareGroundCover")
# 
# dat2_avgDist_postFilter <- apply(X = yearDataset, MARGIN = 1, FUN = function(x) {
#   lapply(X = dataTypes, FUN = function(y) {
#     ## get the distance between each point in the given data source/year combination
#     tempDat <- dat2 %>%
#       filter((Year == x[[1]]) & (Source == "LANDFIRE")) %>%
#       filter(!is.na(.data[[y]])) %>%
#       sf::st_distance()
#     ## replace zeros on the diagonal of the matrix with NAs
#     diag(tempDat) <- NA
#     # for each observation (row), find the minimum value (distance to nearest neighbor)
#     tempDat2 <- apply(X = tempDat, MARGIN = 1, FUN = function(x) {min(x, na.rm = TRUE)})
#     ## calculate the mean distance between points in meters
#     avgDist <- mean(tempDat2, na.rm = TRUE)
#     ## save output
#     return(data.frame(Year = x[1], Source = x[2], DataType = y, avgDist = avgDist))
#   }
#   )
# })
# 
# dat2_avgDist_postFilter_df <- list_rbind(lapply(dat2_avgDist_postFilter, FUN = purrr::list_rbind))
# 
# ## median distance between points in each dataset (across all years)
# dat2_avgDist_postFilter_df2 <- dat2_avgDist_postFilter_df %>%
#   filter(is.finite(avgDist)) %>%
#   group_by(Source, DataType) %>%
#   reframe(meanDist = mean(avgDist, na.rm = TRUE),
#           medianDist = median(avgDist, na.rm = TRUE)) %>%
#   pivot_wider(names_from = Source,
#               values_from = c(meanDist, medianDist))
# 
# ## get median distance between points across all data types and years
# dat2_avgDist_postFilter_df3 <- dat2_avgDist_postFilter_df %>%
#   filter(is.finite(avgDist)) %>%
#   group_by(Source) %>%
#   reframe(meanDist = mean(avgDist, na.rm = TRUE),
#           medianDist = median(avgDist, na.rm = TRUE))

# compare sample size per dataset before and after 'coarsifying' ----------
# before coarsifying
dat %>% 
  st_drop_geometry() %>% 
  group_by(#Year, 
    Source) %>% 
  summarize(n_totTree = sum(!is.na(TotalTreeCover)), 
            n_conifTree = sum(!is.na(ConifTreeCover_prop)),
            n_angioTree = sum(!is.na(AngioTreeCover_prop)), 
            n_shrub = sum(!is.na(ShrubCover)),
            n_bareGround = sum(!is.na(BareGroundCover)),
            n_forb = sum(!is.na(ForbCover_prop)),
            n_c3 = sum(!is.na(C3GramCover_prop)), 
            n_c4 = sum(!is.na(C4GramCover_prop)), 
            n_totHerb = sum(!is.na(TotalHerbaceousCover)))
# after coarsifying

dat2 %>% 
  st_drop_geometry() %>% 
  group_by(#Year, 
    Source) %>% 
  summarize(n_totTree = sum(!is.na(TotalTreeCover)), 
            n_conifTree = sum(!is.na(ConifTreeCover_prop)),
            n_angioTree = sum(!is.na(AngioTreeCover_prop)), 
            n_shrub = sum(!is.na(ShrubCover)),
            n_bareGround = sum(!is.na(BareGroundCover)),
            n_forb = sum(!is.na(ForbCover_prop)),
            n_c3 = sum(!is.na(C3GramCover_prop)), 
            n_c4 = sum(!is.na(C4GramCover_prop)), 
            n_totHerb = sum(!is.na(TotalHerbaceousCover)))

dat2 %>% 
  ggplot() + 
  facet_wrap(~Source) + 
  geom_point(aes(Lon, Lat, col = Source))

# Now do spatial averaging -- Get the 'name' of the daymet cell that each observation lies within --------
crs(dat2) == crs(test)
dat2_cellID <- dat2 %>% 
  cbind(terra::extract(x = test$daymet_v4_prcp_monttl_na_1980_1, y = dat2, cells = TRUE, ID = FALSE, xy = TRUE)[,2:4])

dat2_avgsBySource <- dat2_cellID %>% 
  st_drop_geometry() %>% 
  group_by(Year, cell, x, y 
           ,Source
  ) %>% 
  #summarize(across(all_of(layerNames), list(mean = mean, n = sum(!is.na(.)))), na.rm = TRUE)
  summarize(ShrubCover_n = sum(!is.na(ShrubCover)),
            ShrubCover_sd = sd(ShrubCover, na.rm = TRUE),
            ShrubCover = mean(ShrubCover, na.rm = TRUE), 
            TotalHerbaceousCover_n = sum(!is.na(TotalHerbaceousCover)),
            TotalHerbaceousCover_sd = sd(TotalHerbaceousCover, na.rm = TRUE),
            TotalHerbaceousCover = mean(TotalHerbaceousCover, na.rm = TRUE), 
            #TotalHerbaceousCover_n2 = n(),
            TotalTreeCover_n = sum(!is.na(TotalTreeCover)),
            TotalTreeCover_sd = sd(TotalTreeCover, na.rm = TRUE),
            TotalTreeCover = mean(TotalTreeCover, na.rm = TRUE), 
            C3GramCover_prop_n = sum(!is.na(C3GramCover_prop)),
            C3GramCover_prop_sd = sd(C3GramCover_prop, na.rm = TRUE),
            C3GramCover_prop = mean(C3GramCover_prop, na.rm = TRUE), 
            C4GramCover_prop_n = sum(!is.na(C4GramCover_prop)),
            C4GramCover_prop_sd = sd(C4GramCover_prop, na.rm = TRUE),
            C4GramCover_prop = mean(C4GramCover_prop, na.rm = TRUE), 
            ForbCover_prop_n = sum(!is.na(ForbCover_prop)),
            ForbCover_prop_sd = sd(ForbCover_prop, na.rm = TRUE),
            ForbCover_prop = mean(ForbCover_prop, na.rm = TRUE), 
            AngioTreeCover_prop_n = sum(!is.na(AngioTreeCover_prop)),
            AngioTreeCover_prop_sd = sd(AngioTreeCover_prop, na.rm = TRUE),
            AngioTreeCover_prop = mean(AngioTreeCover_prop, na.rm = TRUE), 
            ConifTreeCover_prop_n = sum(!is.na(ConifTreeCover_prop)),
            ConifTreeCover_prop_sd = sd(ConifTreeCover_prop, na.rm = TRUE),
            ConifTreeCover_prop = mean(ConifTreeCover_prop, na.rm = TRUE), 
            BareGroundCover_n = sum(!is.na(BareGroundCover)),
            BareGroundCover_sd = sd(BareGroundCover, na.rm = TRUE),
            BareGroundCover = mean(BareGroundCover, na.rm = TRUE)
  )


## get averages in all 
dat2_avgs <- dat2_cellID %>% 
  #filter(cell == 32829762) %>% 
  st_drop_geometry() %>% 
  group_by(Year, cell, x, y 
           #,Source
  ) %>% 
  #summarize(across(all_of(layerNames), list(mean = mean, n = sum(!is.na(.)))), na.rm = TRUE)
  summarize(ShrubCover_n = sum(!is.na(ShrubCover)),
            ShrubCover_sd = sd(ShrubCover, na.rm = TRUE),
            ShrubCover = mean(ShrubCover, na.rm = TRUE), 
            TotalHerbaceousCover_n = sum(!is.na(TotalHerbaceousCover)),
            TotalHerbaceousCover_sd = sd(TotalHerbaceousCover, na.rm = TRUE),
            TotalHerbaceousCover = mean(TotalHerbaceousCover, na.rm = TRUE), 
            #TotalHerbaceousCover_n2 = n(),
            TotalTreeCover_n = sum(!is.na(TotalTreeCover)),
            TotalTreeCover_sd = sd(TotalTreeCover, na.rm = TRUE),
            TotalTreeCover = mean(TotalTreeCover, na.rm = TRUE), 
            C3GramCover_prop_n = sum(!is.na(C3GramCover_prop)),
            C3GramCover_prop_sd = sd(C3GramCover_prop, na.rm = TRUE),
            C3GramCover_prop = mean(C3GramCover_prop, na.rm = TRUE), 
            C4GramCover_prop_n = sum(!is.na(C4GramCover_prop)),
            C4GramCover_prop_sd = sd(C4GramCover_prop, na.rm = TRUE),
            C4GramCover_prop = mean(C4GramCover_prop, na.rm = TRUE), 
            ForbCover_prop_n = sum(!is.na(ForbCover_prop)),
            ForbCover_prop_sd = sd(ForbCover_prop, na.rm = TRUE),
            ForbCover_prop = mean(ForbCover_prop, na.rm = TRUE), 
            AngioTreeCover_prop_n = sum(!is.na(AngioTreeCover_prop)),
            AngioTreeCover_prop_sd = sd(AngioTreeCover_prop, na.rm = TRUE),
            AngioTreeCover_prop = mean(AngioTreeCover_prop, na.rm = TRUE), 
            ConifTreeCover_prop_n = sum(!is.na(ConifTreeCover_prop)),
            ConifTreeCover_prop_sd = sd(ConifTreeCover_prop, na.rm = TRUE),
            ConifTreeCover_prop = mean(ConifTreeCover_prop, na.rm = TRUE), 
            BareGroundCover_n = sum(!is.na(BareGroundCover)),
            BareGroundCover_sd = sd(BareGroundCover, na.rm = TRUE),
            BareGroundCover = mean(BareGroundCover, na.rm = TRUE)
  )

# save data ---------------------------------------------------------------
# data pre-spatial averaging
saveRDS(dat2, "./Data_processed/CoverData/data_beforeSpatialAveraging_sampledLANDFIRE.rds")
# spatially averaged data
saveRDS(dat2_avgs, "./Data_processed/CoverData/spatiallyAverageData_intermediate_test5_sampledLANDFIRE.rds")
# visualizations  ---------------------------
# mapview(dat2_cellID[dat2_cellID$Source == "RAP" & is.finite(dat2_cellID$ShrubCover) & dat2_cellID$Year == 2010,])

## No. of observations per cell
# plots show the maximum # of obs / cell across all years of sampling
# histograms show all values 

averagingFigs <- lapply(X = layerNames, FUN = function(x) {
  
  if (x %in% c("ShrubCover", "TotalHerbaceousCover", "TotalTreeCover","BareGroundCover")) {
    cov_label <- "% Cover"
    cov_title_fig <- paste0("% ",x, ", after spatial averaging process \n(dayMet grid aggregated x 4)")
    cov_title_hist <- paste0("% ",x, ", after spatial averaging process")
  } else {
    cov_label <- "prop."
    cov_title_fig <- paste0(x, ", after spatial averaging process \n(dayMet grid aggregated x 4)")
    cov_title_hist <- paste0(x, ", after spatial averaging process")
  } 
  dat2_n <- dat2_avgs %>% 
    filter(is.finite(x)) %>% 
    sf::st_as_sf(coords = c("x", "y"), crs = crs(dat2_cellID)) %>% 
    sf::st_transform(crs(test)) %>% 
    terra::vect() %>% 
    terra::rasterize(y = test, field = paste0(x,"_n"), fun = max, na.rm = TRUE) %>% 
    terra::aggregate(fact = 4, fun = "max", na.rm = TRUE) %>% 
    terra::crop(ext(-130, -60, 20, 50))
  
  n_figs <- ggarrange(
    ggplot() +
      geom_spatraster(data = log(dat2_n), aes(), na.rm = TRUE) +
      theme_minimal() +
      scale_fill_viridis_c(option = "D", guide = guide_colorbar(title = "log(#of plots)"),
                           #limits = c(1,315)
      ) +
      #facet_wrap(~lyr) +
      ggtitle(paste0("Maximum # of plots in a grid cell/year combo. with \n",x, " data (log-transformed)"))+
      theme_minimal()
    ,
    dat2_avgs %>% 
      filter(is.finite(x)) %>% 
      ggplot() + 
      geom_density(aes((.data[[paste0(x,"_n")]]))) + 
      xlim(c(0,50)) + 
      geom_text(aes(x = 40, y = 1, label = paste0("max No. of plots = ",max(.data[[paste0(x,"_n")]])))) + 
      ggtitle(paste0("Maximum # of plots in a grid/cell combo. \nwith",x," data (curtailed)")),
    #geom_density(aes(log(ShrubCover_n))),
    ncol = 2, widths = c(1,.5)
  )
  
  ## Cover Values 
  dat2_cov <- dat2_avgs %>% 
    filter(is.finite(x)) %>% 
    sf::st_as_sf(coords = c("x", "y"), crs = crs(dat2_cellID)) %>% 
    terra::vect() %>% 
    terra::rasterize(y = test, field = x, fun = mean, na.rm = TRUE) %>% 
    terra::aggregate(fact = 4, fun = "mean", na.rm = TRUE)  %>% 
    terra::crop(ext(-130, -60, 20, 50))
  
  cov_figs <- ggarrange(
    ggplot() +
      geom_spatraster(data = (dat2_cov), aes(), na.rm = TRUE) +
      theme_minimal() +
      scale_fill_viridis_c(option = "H", guide = guide_colorbar(title = cov_label),
                           #limits = c(1,315)
      ) +
      #facet_wrap(~lyr) +
      ggtitle(cov_title_fig)+
      theme_minimal()
    ,
    dat2_avgs %>% 
      filter(is.finite(x)) %>% 
      ggplot() + 
      geom_density(aes(.data[[x]])) + 
      #xlim(c(0,50)) + 
      ggtitle(cov_title_hist),
    #geom_density(aes(log(ShrubCover_n))),
    ncol = 2, widths = c(1,.5)
  )
  
  
  ## SD of cover values within a cell
  dat2_sd <- dat2_avgs %>% 
    filter(is.finite(x)) %>% 
    sf::st_as_sf(coords = c("x", "y"), crs = crs(dat2_cellID)) %>%  
    terra::vect() %>% 
    terra::rasterize(y = test, field = paste0(x, "_sd"), fun = mean, na.rm = TRUE) %>% 
    terra::aggregate(fact = 4, fun = "mean", na.rm = TRUE) %>% 
    terra::crop(ext(-130, -60, 20, 50))
  
  sd_figs <- ggarrange(
    ggplot() +
      geom_spatraster(data = (dat2_sd), aes(), na.rm = TRUE) +
      theme_minimal() +
      scale_fill_viridis_c(option = "F", guide = guide_colorbar(title = "sd"),
                           #limits = c(1,315)
      ) +
      #facet_wrap(~lyr) +
      ggtitle(paste0("Stnd. Dev. of ",x," within a grid cell \n(dayMet grid aggregated x 4)"))+
      theme_minimal()
    ,
    dat2_avgs %>% 
      filter(is.finite(x)) %>% 
      ggplot() + 
      geom_density(aes(.data[[paste0(x, "_sd")]])) + 
      #xlim(c(0,50)) + 
      ggtitle(paste0("% ",x,", after spatial averaging process")),
    #geom_density(aes(log(ShrubCover_n))),
    ncol = 2, widths = c(1,.5)
  )
  
  
  ## add all together
  allFigs <- ggarrange(
    cov_figs,
    sd_figs,
    n_figs,
    ncol = 1
  )
  return(allFigs)
}
)

# save figures

bitmap(file = "./Figures/CoverDatFigures/SpatiallyAveragedCoverData_filteredRAP_LF.bmp", 
       width = 14, height = 100, res = 200)
ggarrange(
  averagingFigs[[1]],
  averagingFigs[[2]],
  averagingFigs[[3]],
  averagingFigs[[4]],
  averagingFigs[[5]],
  averagingFigs[[6]],
  averagingFigs[[7]],
  averagingFigs[[8]],
  averagingFigs[[9]],
  ncol = 1
)
dev.off()
