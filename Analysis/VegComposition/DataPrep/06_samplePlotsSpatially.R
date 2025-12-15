#///////////////////
# Averaging values from plots w/in each dayMet gridcell
# Alice Stears
# 8/19/24
#///////////////////


# Load packages -----------------------------------------------------------

library(tidyverse)
library(sf)
library(terra)
library(exactextractr)
library(stars)
library(tidyterra)

# Load data ---------------------------------------------------------------

# get veg data
dat <- readRDS("./Data_processed/CoverData/DataForModels.RDS") 
# dayMet extent 
test <-  rast("./Data_raw/dayMet/rawMonthlyData/orders/70e0da02b9d2d6e8faa8c97d211f3546/Daymet_Monthly_V4R1/data/daymet_v4_prcp_monttl_na_1980.tif") %>% 
  terra::project(crs(dat))

layerNames <- c("ShrubCover", "TotalHerbaceousCover",  "TotalTreeCover", "C3GramCover_prop", "C4GramCover_prop",
                "ForbCover_prop", "AngioTreeCover_prop", "ConifTreeCover_prop",
                "BareGroundCover")
# %>% 
#   st_transform(crs(test)) %>% 
#   st_drop_geometry() %>% 
#   select(Year, UniquID:Day, Lat:annVPD_max_meanAnnAvg_1yrAnom) %>% 
#   mutate(Year_fac = as.factor(Year)) #%>% 
#   #st_buffer(.01)

## make sure the projections are the same
# add met data to veg data ------------------------------------------------
# make veg data projection the same as raster data
v <- vect(st_drop_geometry(dat)[,c("Lon", "Lat")], geom = c("Lon", "Lat"), crs=crs(dat))
y <- project(v, crs(test))
# make sure the veg data is in the appropriate projection
dat2 <- dat %>%
  #st_buffer(.01) %>%
  #terra::vect() #%>%
  sf::st_transform(crs(y)) %>% 
  # st_buffer(.1)
  st_centroid() %>% 
  mutate("Year_char" = as.character(Year)) %>% 
  select(Year, Year_char, UniquID:Day, Lat:geometry)


# make sure the raster data is in the appropriate projection
test <- test %>%
  terra::project(crs(y))
st_crs(dat2) == st_crs(test)

#(xTest = st_rasterize(dat2[,"ShrubCover"], st_as_stars(st_bbox(test), nx = ncol(test), ny = nrow(test), values = NA_real_)))

## rename the columns in 'dat' 
# fix names
dat2  <- dat2 %>%
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

dat2 <- ungroup(dat2)

dat2 <- dat2 %>% 
  filter(Year >= 2000) #%>% 
#slice_sample(n = 100000)
# dat2 %>% 
#   st_drop_geometry()  %>% 
#   filter(!is.na(TotalHerbaceousCover)) %>% 
#   ggplot() + 
#   facet_wrap(~Year) + 
#   geom_point(aes(x = Lon, y = Lat))

## for RAP data, remove the points left over on the very far east coast that somehow are left over after the filtering process to remove forests
dat2 <- dat2[!(dat2$Source == "RAP" & 
                 dat2$Lon > -85),]

# calculate the average distance between points in each dataset/year --------
yearDataset <- unique(dat2 %>%
                        st_drop_geometry() %>% 
                        select(Year, Source))
dataTypes <- c("ShrubCover",  "ForbCover", "TotalGramCover", "C3GramCover", "C4GramCover", "AngioTreeCover", "ConifTreeCover", "TotalTreeCover", "TotalHerbaceousCover", "BareGroundCover")

dat2_avgDist <- apply(X = yearDataset[1:3,], MARGIN = 1, FUN = function(x) {
  lapply(X = dataTypes, FUN = function(y) {
    ## get the distance between each point in the given data source/year combination
    tempDat <- dat2 %>% 
      filter((Year == x[1]) & (Source == x[2])) %>% 
      filter(!is.na(.data[[y]])) %>% 
      sf::st_distance() 
    ## replace zeros on the diagonal of the matrix with NAs 
    diag(tempDat) <- NA
    # for each observation (row), find the minimum value (distance to nearest neighbor)
    tempDat2 <- apply(X = tempDat, MARGIN = 1, FUN = function(x) {min(x, na.rm = TRUE)})
    ## calculate the mean distance between points in meters
    avgDist <- mean(tempDat2, na.rm = TRUE)
    ## save output
    return(data.frame(Year = x[1], Source = x[2], DataType = y, avgDist = avgDist))
  }
  )
})

dat2_avgDist_df <- list_rbind(lapply(dat2_avgDist, FUN = purrr::list_rbind))
# save intermediate data
saveRDS(dat2_avgDist_df, "./Data_processed/CoverData/tableOfSamplingDensity.rds")

## median distance between points in each dataset (across all years) 
dat2_avgDist_df2 <- dat2_avgDist_df %>% 
  filter(is.finite(avgDist)) %>% 
  group_by(Source, DataType) %>% 
  reframe(meanDist = mean(avgDist, na.rm = TRUE),
          medianDist = median(avgDist, na.rm = TRUE)) %>% 
  pivot_wider(names_from = Source, 
              values_from = c(meanDist, medianDist))

## get median distance between points across all data types and years 
dat2_avgDist_df3 <- dat2_avgDist_df %>% 
  filter(is.finite(avgDist)) %>% 
  group_by(Source) %>% 
  reframe(meanDist = mean(avgDist, na.rm = TRUE),
          medianDist = median(avgDist, na.rm = TRUE))
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
  terra::aggregate(fact = 4, fun = "mean")
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

## if, for a given data type in a given year, there are > 1 observation in an aggregated grid cell, arbitrarily chose 1
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
  sf::st_as_sf(coords = c("Lat", "Lon"), remove = FALSE, crs = sf::st_crs(dat2)) %>% 
  select(names(dat2_noLF %>% select(-c(ForbCover, TotalGramCover, AnnualHerbGramCover, PerennialHerbGramCover, C3GramCover, C4GramCover,
                                       AngioTreeCover, ConifTreeCover, TreeBasalArea, LitterCover, LitterDepth))), 'cell') 

 # add the sampled LF data onto the un-sampled LF data
dat2 <- dat2_noLF %>%
  select(-c(ForbCover, TotalGramCover, AnnualHerbGramCover, PerennialHerbGramCover, C3GramCover, C4GramCover,
            AngioTreeCover, ConifTreeCover, TreeBasalArea, LitterCover, LitterDepth)) %>%
  rbind(dat2_LFonly_filtered %>% select(-cell))

# ggarrange(
# dat2 %>%
#   filter(Source == "LANDFIRE" & Year == 2000) %>%
#   ggplot() +
#   geom_sf() +
#   ggtitle("filtered data"),
# dat2 %>%
#   filter(Source == "LANDFIRE" & Year == 2000) %>%
#   ggplot() +
#   geom_sf() +
#   ggtitle("unfiltered data")
# )

# Now do spatial averaging -- Get the 'name' of the daymet cell that each observation lies within --------
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

# ###/////////////
# # rasterize values --------------------------------------------------------
# # get the names of the columns w/ data we want to rasterize
# layerNames <- c("ShrubCover", "TotalHerbaceousCover",  "TotalTreeCover", "C3GramCover_prop", "C4GramCover_prop",
#                   "ForbCover_prop", "AngioTreeCover_prop", "ConifTreeCover_prop",
#                   "BareGroundCover")
# 
# years <- sort(unique(dat2$Year))
# 
# dat2 %>% 
#   filter(!is.na(TotalHerbaceousCover)) %>% 
#   ggplot() + 
#   facet_wrap(~Year) + 
#   geom_point(aes(Lon, Lat))
# 
# # rasterize and average cover values
# test2_a <- lapply(layerNames[1:2], FUN = function(x) {
#   temp_a <- terra::rasterize(dat2[dat2$Year %in% c(1940:1985),], y = test, field = x, fun = mean, na.rm = TRUE#function(x) mean(x, na.rm = TRUE)
#                              , by = "Year")
#   temp_b <- terra::rasterize(dat2[dat2$Year %in% c(1986:2023),], y = test, field = x, fun = mean, na.rm = TRUE#function(x) mean(x, na.rm = TRUE)
#                              , by = "Year")
#   temp <- c(temp_a, temp_b)
#   
#   names(temp) <- paste0("ID_",years)
#   
#   # re-write NaN values as NA to save space
#   temp[is.nan(temp)] <- NA
#   return(temp)
#   # temp <-  dat2 %>% 
#   #   select("Lat", "Lon", "Year", all_of(x)) %>% 
#   #   drop_na() %>% 
#   #   terra::vect()
#   #   
#   #   #dat2[!is.na(dat2[,x]), c("Lat", "Lon", "Year", x)]
#   # 
#   # temp2 <- lapply(years, FUN = function(y) {
#   #   # rasterize the points
#   #   temp3 <-  temp[temp$Year == y,] %>% 
#   #      terra::rasterize(y = test,#test, 
#   #                      field = x, 
#   #                      #by = "Year",
#   #                      fun = mean, na.rm = TRUE) %>% 
#   #   #re-extract as points at the centroid of each raster cell 
#   #   terra::extract(y = centroidPoints, xy = TRUE) %>% 
#   #     drop_na()
#   # }
#   # )
#   # 
#   # 
# }
# )
# 
# 
# test2_b <- lapply(layerNames[3], FUN = function(x) {
#   temp_a <- terra::rasterize(dat2[dat2$Year %in% c(1940:1985),], y = test, field = x, fun = mean, na.rm = TRUE#function(x) mean(x, na.rm = TRUE)
#                              , by = "Year")
#   temp_b <- terra::rasterize(dat2[dat2$Year %in% c(1986:2023),], y = test, field = x, fun = mean, na.rm = TRUE#function(x) mean(x, na.rm = TRUE)
#                              , by = "Year")
#   temp <- c(temp_a, temp_b)
#   
#   names(temp) <- paste0("ID_",years)
#   
#   # re-write NaN values as NA to save space
#   temp[is.nan(temp)] <- NA
#   return(temp)
# }
# )
# 
# test2_ab <- c(test2_a, test2_b)
# rm(test2_a, test2_b)
# gc()
# 
# test2_c <- lapply(layerNames[4], FUN = function(x) {
#   temp_a <- terra::rasterize(dat2[dat2$Year %in% c(1940:1985),], y = test, field = x, fun = mean, na.rm = TRUE#function(x) mean(x, na.rm = TRUE)
#                              , by = "Year")
#   temp_b <- terra::rasterize(dat2[dat2$Year %in% c(1986:2023),], y = test, field = x, fun = mean, na.rm = TRUE#function(x) mean(x, na.rm = TRUE)
#                              , by = "Year")
#   temp <- c(temp_a, temp_b)
#   
#   names(temp) <- paste0("ID_",years)
#   
#   # re-write NaN values as NA to save space
#   temp[is.nan(temp)] <- NA
#   return(temp)
# }
# )
# test2_abc <- c(test2_ab, test2_c)
# rm(test2_ab, test2_c,dat, v, y)
# 
# gc()
# 
# test2_d <- lapply(layerNames[5], FUN = function(x) {
#   temp_a <- terra::rasterize(dat2[dat2$Year %in% c(1940:1985),], y = test, field = x, fun = mean, na.rm = TRUE#function(x) mean(x, na.rm = TRUE)
#                              , by = "Year")
#   temp_b <- terra::rasterize(dat2[dat2$Year %in% c(1986:2023),], y = test, field = x, fun = mean, na.rm = TRUE#function(x) mean(x, na.rm = TRUE)
#                              , by = "Year")
#   temp <- c(temp_a, temp_b)
#   
#   names(temp) <- paste0("ID_",years)
#   
#   # re-write NaN values as NA to save space
#   temp[is.nan(temp)] <- NA
#   return(temp)
# }
# )
# test2_abcd <- c(test2_abc, test2_d)
# rm(test2_abc, test2_d)
# gc()
# 
# test2_e <- lapply(layerNames[6], FUN = function(x) {
#   temp_a <- terra::rasterize(dat2[dat2$Year %in% c(1940:1985),], y = test, field = x, fun = mean, na.rm = TRUE#function(x) mean(x, na.rm = TRUE)
#                              , by = "Year")
#   temp_b <- terra::rasterize(dat2[dat2$Year %in% c(1986:2023),], y = test, field = x, fun = mean, na.rm = TRUE#function(x) mean(x, na.rm = TRUE)
#                              , by = "Year")
#   temp <- c(temp_a, temp_b)
#   
#   names(temp) <- paste0("ID_",years)
#   
#   # re-write NaN values as NA to save space
#   temp[is.nan(temp)] <- NA
#   return(temp)
# }
# )
# 
# test2_abcde <- c(test2_abcd, test2_e)
# rm(test2_abcd, test2_e)
# gc()
# 
# test2_f <- lapply(layerNames[7], FUN = function(x) {
#   temp_a <- terra::rasterize(dat2[dat2$Year %in% c(1940:1985),], y = test, field = x, fun = mean, na.rm = TRUE#function(x) mean(x, na.rm = TRUE)
#                            , by = "Year")
#   temp_b <- terra::rasterize(dat2[dat2$Year %in% c(1986:2023),], y = test, field = x, fun = mean, na.rm = TRUE#function(x) mean(x, na.rm = TRUE)
#                              , by = "Year")
#  temp <- c(temp_a, temp_b)
#   
#  names(temp) <- paste0("ID_",years)
#  
#  # re-write NaN values as NA to save space
#  temp[is.nan(temp)] <- NA
#   return(temp)
# }
# )
# 
# test2_abcdef <- c(test2_abcde, test2_f)
# 
#  # terra:::saveRDS(test2_f, file = "./Data_processed/CoverData/IntermediateAnalysisFiles/rasterForSpatialAveraging_f.rds")
#  # test2_f <- terra:::readRDS(file = "./Data_processed/CoverData/IntermediateAnalysisFiles/rasterForSpatialAveraging_f.rds")
# 
# rm(test2_abcde, test2_f)
# gc()
# 
# test2_g <- lapply(layerNames[8], FUN = function(x) {
#   temp_a <- terra::rasterize(dat2[dat2$Year %in% c(1940:1985),], y = test, field = x, fun = mean, na.rm = TRUE#function(x) mean(x, na.rm = TRUE)
#                              , by = "Year")
#   temp_b <- terra::rasterize(dat2[dat2$Year %in% c(1986:2023),], y = test, field = x, fun = mean, na.rm = TRUE#function(x) mean(x, na.rm = TRUE)
#                              , by = "Year")
#   temp <- c(temp_a, temp_b)
#   
#   names(temp) <- paste0("ID_",years)
#   
#   # re-write NaN values as NA to save space
#   temp[is.nan(temp)] <- NA
#   return(temp)
# }
# )
# 
# test2_abcdefg <- c(test2_abcdef, test2_g)
# 
# #terra:::saveRDS(test2_g, file = "./Data_processed/CoverData/IntermediateAnalysisFiles/rasterForSpatialAveraging_g.rds")
# #test2_g <- terra:::readRDS(file = "./Data_processed/CoverData/IntermediateAnalysisFiles/rasterForSpatialAveraging_g.rds")
# 
# rm(test2_abcdef, test2_g)
# gc()
# 
# test2_h <- lapply(layerNames[9], FUN = function(x) {
#   temp_a <- terra::rasterize(dat2[dat2$Year %in% c(1940:1985),], y = test, field = x, fun = mean, na.rm = TRUE#function(x) mean(x, na.rm = TRUE)
#                              , by = "Year")
#   temp_b <- terra::rasterize(dat2[dat2$Year %in% c(1986:2023),], y = test, field = x, fun = mean, na.rm = TRUE#function(x) mean(x, na.rm = TRUE)
#                              , by = "Year")
#   temp <- c(temp_a, temp_b)
#   
#   names(temp) <- paste0("ID_",years)
#   
#   # re-write NaN values as NA to save space
#   temp[is.nan(temp)] <- NA 
#   
#   names(temp) <- paste0("ID_",years)
#   return(temp)
# }
# )
# 
# test2 <- c(test2_abcdefg, test2_h)
# rm(test2_abcdefg, test2_h)
# gc()
# 
# names(test2) <- layerNames
# 
# 
# # save the output ---------------------------------------------------------
# for (i in 1:length(layerNames)){
#   terra::saveRDS(test2[[i]], file = paste0("./Data_processed/CoverData/IntermediateAnalysisFiles/rasterForSpatialAveraging_",layerNames[i],".rds"))
# }
# # ## read back in 
# # test2_new <- vector(length = length(layerNames), mode = "list")
# # for (i in 1:length(layerNames)){
# #   test2_new[[i]] <- terra:::readRDS(file = paste0("./Data_processed/CoverData/IntermediateAnalysisFiles/rasterForSpatialAveraging_",layerNames[i],".rds")) %>% 
# #     rast()
# #   }
# 
# 
# # library(tidyterra)
# # plotDat <- test2$NeedleLeavedTreeCover_prop %>%
# #   terra::mean(na.rm = TRUE) %>%
# #   terra::aggregate(fact = 8, fun = "mean", na.rm = TRUE)
# # (treeCov_plot_AVG <- ggplot() +
# #     geom_spatraster(data = plotDat, aes(), na.rm = TRUE) +
# #     theme_minimal() +
# #     scale_fill_viridis_c(option = "H", guide = guide_colorbar(title = "% cover"),
# #                          limits = c(0,1)) +
# #     ggtitle("Averaged Data", subtitle = "Tree % cover" ))
# 
# # # save for later
# # lapply(layerNames, FUN = function(x) {
# #   saveRDS(test2[[x]]*1, paste0("./Data_processed/CoverData/spatialSamplesInt_",x,".rds"))
# # })
# 
# # # read back in saved data
# # testTest <- lapply(layerNames, FUN = function(x) {
# #  readRDS(paste0("./Data_processed/CoverData/spatialSamplesInt_",x,".rds")) %>%
# #     rast()
# # }
# # )
# # names(testTest) <- layerNames
# # test2 <- testTest
# 
# 
# # ExtractValuesBackToPoints -----------------------------------------------
# 
# centroidPoints <- xyFromCell(test$daymet_v4_prcp_monttl_na_1980_1, cell = 1:ncell(test$daymet_v4_prcp_monttl_na_1980_1)) %>% 
#   #as.data.frame() %>% 
#   #st_as_sf(coords = c("x", "y")) %>% 
#   vect(crs = terra::crs(test)) 
# crs(centroidPoints) == crs(dat2) 
#   
# 
# # extract the points from the raster back to the data.frame 
# test3 <- lapply(names(test2)[1:length(names(test2))], FUN = function(y) {
#   print(y)
#   temp <- lapply(1:length(unique(dat2$Year)), FUN = function(x) {
#     
#     # is the raster for this data/year all NAs? 
#     if (any(as.numeric(terra::global(test2[[y]][[x]], fun = "notNA")[,1]) > 0)) { # if there is data
#       print(x)
#        layer_i <- test2[[y]][[x]] %>% 
#         terra::extract(y = centroidPoints, xy = TRUE) %>% # get data just for the points
#         filter(!is.na(.data[[names(test2[[y]])[x]]])) 
#       layer_i$Year <- sort(unique(dat2$Year))[x]
#       return(layer_i)
#     } else {
#       message(paste0("year ", sort(unique(dat2$Year))[[x]], " (index ", x,") has no data"))
#     }
#     
#   }) %>% 
#     list_rbind 
#   
#   temp2 <- temp %>% 
#     transmute(value = base::rowSums(temp %>% select(any_of(names(test2[[1]]))), na.rm = TRUE), 
#               ID = ID, 
#               x = x,
#               y = y, 
#               Year = Year)
#   # save this iteration of 'y' as a .csv just in case 
#   write.csv(x = temp2, file = paste0("./Data_processed/CoverData/IntermediateAnalysisFiles/rasterToPoints_",y,".csv"))
#   return(temp2)
# }
# )
# # 
# # test3 <- lapply(names(test2)[1:length(names(test2))], FUN = function(y) {
# #   temp <- read.csv(paste0("./Data_processed/CoverData/IntermediateAnalysisFiles/rasterToPoints_",y,".csv"))
# #   return(temp[,2:6])
# # }
# # )
# 
# names(test3) <- layerNames
# 
# # save output! 
# saveRDS(test3, "./Data_processed/CoverData/spatiallyAverageData_intermediate.rds")
# 
# #test3 <- readRDS("./Data_processed/CoverData/spatiallyAverageData_intermediate.rds")
# # treeCover_rast <- test3$NeedleLeavedTreeCover_prop %>% 
# #   st_as_sf(coords = c('x', 'y')) %>% 
# #   #slice_sample(n = 5e4) %>%
# #   terra::vect() %>% 
# #   #terra::set.crs(crs(test_rast)) %>% 
# #   terra::rasterize(y = test2$NeedleLeavedTreeCover_prop, 
# #                    field = "value", 
# #                    fun = mean, na.rm = TRUE) %>% 
# #   terra::aggregate(fact = 16, fun = "mean", na.rm = TRUE) %>% 
# #   terra::crop(ext(-2560750, 3253250, -2090500, 1284500))
# # (treeCov_plot_AVG <- ggplot() + 
# #     geom_spatraster(data = treeCover_rast, aes(fill = mean), na.rm = TRUE) +
# #     theme_minimal() + 
# #     scale_fill_viridis_c(option = "H", guide = guide_colorbar(title = "% cover"), 
# #                          limits = c(0,1)) +
# #     ggtitle("Averaged Data", subtitle = "Tree % cover" ))
# 
# 
# # put together into a data.frame 
# test4 <- lapply(layerNames, function(x) {
#   temp <- test3[[x]] %>% 
#     select(-ID) 
#   names(temp)[1] <- x
#   return(temp)
# }) 
# test5 <- test4[[1]] %>% 
#   full_join(test4[[2]], by = c("x", "y", "Year")) %>% 
#   full_join(test4[[3]], by = c("x", "y", "Year")) %>% 
#   full_join(test4[[4]], by = c("x", "y", "Year")) %>% 
#   full_join(test4[[5]], by = c("x", "y", "Year")) %>% 
#   full_join(test4[[6]], by = c("x", "y", "Year")) %>% 
#   full_join(test4[[7]], by = c("x", "y", "Year")) %>% 
#   full_join(test4[[8]], by = c("x", "y", "Year")) %>% 
#   full_join(test4[[9]], by = c("x", "y", "Year")) #%>% 
#   #full_join(test4[[10]], by = c("x", "y", "Year"))
#   
# 
# rm(test3, test4)
# gc()
# 
# plot(test5$x, test5$y)

# saveRDS(test5, "./Data_processed/CoverData/spatiallyAverageData_intermediate_test5.rds")
# rm(test5)
# gc()
# 
# test5_totalTree <- test5 %>% 
#   sf::st_as_sf(coords = c("x", "y"), crs = crs(test)) %>% 
#   terra::vect() %>% 
#   terra::rasterize(y = test, field = "TotalTreeCover", fun = mean, na.rm = TRUE) %>% 
#   terra::aggregate(fact = 2, fun = "max", na.rm = TRUE) %>% 
#   terra::crop(ext(-140, -60, 20, 55))
# 
# ggarrange(
#   ggplot() +
#   geom_spatraster(data = test5_totalTree, aes(), na.rm = TRUE) +
#   theme_minimal() +
#  # scale_fill_viridis_c(option = "H", guide = guide_colorbar(title = "% cover"),
#                        #limits = c(1,315)) +
#   #facet_wrap(~lyr) +
#   ggtitle("Maximum # of plots in a grid cell with Total Tree Cover data", subtitle = "full range of values" )+
#   theme_minimal(),
#   ggplot()
#   
# )
# 
#   
# test5_totalTree <- test5 %>% 
#   sf::st_as_sf(coords = c("x", "y"), crs = crs(test)) %>% 
#   terra::vect() %>% 
#   terra::rasterize(y = test, field = "TotalTreeCover", fun = mean, na.rm = TRUE) %>% 
#   terra::aggregate(fact = 2, fun = "max", na.rm = TRUE) %>% 
#   terra::crop(ext(-140, -60, 20, 55))


# save data ---------------------------------------------------------------
# data pre-spatial averaging
saveRDS(dat2, "./Data_processed/CoverData/data_beforeSpatialAveraging_sampledRAP.rds")
# spatially averaged data
saveRDS(dat2_avgs, "./Data_processed/CoverData/spatiallyAverageData_intermediate_test5_sampledLANDFIRE.rds")
# visualizations  ---------------------------

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
    sf::st_as_sf(coords = c("x", "y"), crs = crs(test)) %>% 
    terra::vect() %>% 
    terra::rasterize(y = test, field = paste0(x,"_n"), fun = max, na.rm = TRUE) %>% 
    terra::aggregate(fact = 4, fun = "max", na.rm = TRUE) %>% 
    terra::crop(ext(-130, -65, 25, 50))
  
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
    sf::st_as_sf(coords = c("x", "y"), crs = crs(test)) %>% 
    terra::vect() %>% 
    terra::rasterize(y = test, field = x, fun = mean, na.rm = TRUE) %>% 
    terra::aggregate(fact = 4, fun = "mean", na.rm = TRUE) %>% 
    terra::crop(ext(-130, -65, 25, 50))
  
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
    sf::st_as_sf(coords = c("x", "y"), crs = crs(test)) %>% 
    terra::vect() %>% 
    terra::rasterize(y = test, field = paste0(x, "_sd"), fun = mean, na.rm = TRUE) %>% 
    terra::aggregate(fact = 4, fun = "mean", na.rm = TRUE) %>% 
    terra::crop(ext(-130, -65, 25, 50))
  
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
