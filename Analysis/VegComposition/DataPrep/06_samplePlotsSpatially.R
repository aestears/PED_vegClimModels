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

# Load data ---------------------------------------------------------------

# get veg data
dat <- readRDS("./Data_processed/CoverData/DataForModels.RDS") 
# dayMet extent 
test <-  rast("./Data_raw/dayMet/rawMonthlyData/orders/70e0da02b9d2d6e8faa8c97d211f3546/Daymet_Monthly_V4R1/data/daymet_v4_prcp_monttl_na_1980.tif") %>% 
  terra::project(crs(dat))

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

# dat2 %>% 
#   st_drop_geometry()  %>% 
#   filter(!is.na(TotalHerbaceousCover)) %>% 
#   ggplot() + 
#   facet_wrap(~Year) + 
#   geom_point(aes(x = Lon, y = Lat))


# Get the 'name' of the daymet cell that each observation lies wit --------
dat2 <- dat2 %>% 
  filter(Year >= 1980) #%>% 
  #slice_sample(n = 100000)

dat2_cellID <- dat2 %>% 
  cbind(terra::extract(x = test$daymet_v4_prcp_monttl_na_1980_1, y = dat2, cells = TRUE, ID = FALSE, xy = TRUE)[,2:4])

layerNames <- c("ShrubCover", "TotalHerbaceousCover",  "TotalTreeCover", "C3GramCover_prop", "C4GramCover_prop",
                "ForbCover_prop", "AngioTreeCover_prop", "ConifTreeCover_prop",
                "BareGroundCover")

dat2Test_avgsBySource <- dat2_cellID %>% 
  st_drop_geometry() %>% 
  group_by(Year, cell, x, y 
           ,Source
           ) %>% 
  #summarize(across(all_of(layerNames), list(mean = mean, n = sum(!is.na(.)))), na.rm = TRUE)
  summarize(ShrubCover_n = sum(!is.na(ShrubCover)),
            ShrubCover = mean(ShrubCover, na.rm = TRUE), 
            TotalHerbaceousCover_n = sum(!is.na(TotalHerbaceousCover)),
            TotalHerbaceousCover = mean(TotalHerbaceousCover, na.rm = TRUE), 
            TotalHerbaceousCover_n2 = n(),
            TotalTreeCover_n = sum(!is.na(TotalTreeCover)),
            TotalTreeCover = mean(TotalTreeCover, na.rm = TRUE), 
            C3GramCover_prop_n = sum(!is.na(C3GramCover_prop)),
            C3GramCover_prop = mean(C3GramCover_prop, na.rm = TRUE), 
            C4GramCover_prop_n = sum(!is.na(C4GramCover_prop)),
            C4GramCover_prop = mean(C4GramCover_prop, na.rm = TRUE), 
            ForbCover_prop_n = sum(!is.na(ForbCover_prop)),
            ForbCover_prop = mean(ForbCover_prop, na.rm = TRUE), 
            AngioTreeCover_prop_n = sum(!is.na(AngioTreeCover_prop)),
            AngioTreeCover_prop = mean(AngioTreeCover_prop, na.rm = TRUE), 
            ConifTreeCover_prop_n = sum(!is.na(ConifTreeCover_prop)),
            ConifTreeCover_prop = mean(ConifTreeCover_prop, na.rm = TRUE), 
            BareGroundCover_n = sum(!is.na(BareGroundCover)),
            BareGroundCover = mean(BareGroundCover, na.rm = TRUE)
            )

dat2Test_avgs <- dat2_cellID %>% 
  st_drop_geometry() %>% 
  group_by(Year, cell, x, y 
           #,Source
  ) %>% 
  #summarize(across(all_of(layerNames), list(mean = mean, n = sum(!is.na(.)))), na.rm = TRUE)
  summarize(ShrubCover_n = sum(!is.na(ShrubCover)),
            ShrubCover = mean(ShrubCover, na.rm = TRUE), 
            TotalHerbaceousCover_n = sum(!is.na(TotalHerbaceousCover)),
            TotalHerbaceousCover = mean(TotalHerbaceousCover, na.rm = TRUE), 
            TotalHerbaceousCover_n2 = n(),
            TotalTreeCover_n = sum(!is.na(TotalTreeCover)),
            TotalTreeCover = mean(TotalTreeCover, na.rm = TRUE), 
            C3GramCover_prop_n = sum(!is.na(C3GramCover_prop)),
            C3GramCover_prop = mean(C3GramCover_prop, na.rm = TRUE), 
            C4GramCover_prop_n = sum(!is.na(C4GramCover_prop)),
            C4GramCover_prop = mean(C4GramCover_prop, na.rm = TRUE), 
            ForbCover_prop_n = sum(!is.na(ForbCover_prop)),
            ForbCover_prop = mean(ForbCover_prop, na.rm = TRUE), 
            AngioTreeCover_prop_n = sum(!is.na(AngioTreeCover_prop)),
            AngioTreeCover_prop = mean(AngioTreeCover_prop, na.rm = TRUE), 
            ConifTreeCover_prop_n = sum(!is.na(ConifTreeCover_prop)),
            ConifTreeCover_prop = mean(ConifTreeCover_prop, na.rm = TRUE), 
            BareGroundCover_n = sum(!is.na(BareGroundCover)),
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


dat2_totalTree <- dat2_avgs %>% 
  sf::st_as_sf(coords = c("x", "y"), crs = crs(test)) %>% 
  terra::vect() %>% 
  terra::rasterize(y = test, field = "TotalTreeCover", fun = mean, na.rm = TRUE) %>% 
  terra::aggregate(fact = 2, fun = "max", na.rm = TRUE) %>% 
  terra::crop(ext(-140, -60, 20, 55))

diff <-  test5_totalTree - dat2_totalTree
bigPosDiffs <- terra::app(x = diff, fun = function(x) {
  x>10
})
bigNegDiffs <- terra::app(x = diff, fun = function(x) {
  x<10
})


ggarrange(
  ggplot() +
  geom_spatraster(data = test5_totalTree, aes(), na.rm = TRUE) +
  theme_minimal() +
 scale_fill_viridis_c(option = "H", guide = guide_colorbar(title = "% cover"),
 limits = c(1,100)) +
  ggtitle("Old dataset" )+
  theme_minimal(),
 ggplot() +
   geom_spatraster(data = dat2_totalTree, aes(), na.rm = TRUE) +
   theme_minimal() +
   scale_fill_viridis_c(option = "H", guide = guide_colorbar(title = "% cover"),
   limits = c(1,100)) +
   ggtitle("New dataset" )+
   theme_minimal(),
 ggplot() +
   geom_spatraster(data = log(test5_totalTree - dat2_totalTree), aes(), na.rm = TRUE) +
   theme_minimal() +
   scale_fill_viridis_c(option = "H", guide = guide_colorbar(title = "% cover")
                        #,limits = c(-60,60)
                        ) +
   ggtitle("Old - New" )+
   theme_minimal()
 ,
 ncol = 1
)
