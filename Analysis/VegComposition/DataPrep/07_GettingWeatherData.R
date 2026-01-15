#///////////////////
# Getting weather data to add to vegetation composition data
# Alice Stears
# 6/17/24
#///////////////////


# load packages -----------------------------------------------------------

library(tidyverse)
library(sf)
library(daymetr)
library(terra)
library(future.apply)
library(parallel)

# Load data ---------------------------------------------------------------

## read in spatially averaged veg data to get locations (are some duplicated locates b/c plots have data from multiple years)
test5 <- readRDS("./Data_processed/CoverData/spatiallyAverageData_intermediate_test5_sampledLANDFIRE.rds")

years <- unique(test5$Year)

# get sf point data
points_sf <- test5 %>%
  sf::st_as_sf(coords = c("x", "y"), crs = crs("EPSG:4269")) %>% 
  st_transform( crs =  "GEOGCRS[\"NAD83\",\n    DATUM[\"North American Datum 1983\",\n        ELLIPSOID[\"GRS 1980\",6378137,298.257222101,\n            LENGTHUNIT[\"metre\",1]]],\n    PRIMEM[\"Greenwich\",0,\n        ANGLEUNIT[\"degree\",0.0174532925199433]],\n    CS[ellipsoidal,2],\n        AXIS[\"geodetic latitude (Lat)\",north,\n            ORDER[1],\n            ANGLEUNIT[\"degree\",0.0174532925199433]],\n        AXIS[\"geodetic longitude (Lon)\",east,\n            ORDER[2],\n            ANGLEUNIT[\"degree\",0.0174532925199433]],\n    ID[\"EPSG\",4269]]")

#mapview(points_sf[points_sf$Year == 2010,])

# ## get monthly data downloaded from online
#test <- terra::rast("./Data_raw/dayMet/rawMonthlyData/orders/70e0da02b9d2d6e8faa8c97d211f3546/Daymet_Monthly_V4R1/data/dayMet_v4_prcp_monttl_na_1980.tif")
rastNames <- list.files("./Data_raw/dayMet/rawMonthlyData/orders/70e0da02b9d2d6e8faa8c97d211f3546/Daymet_Monthly_V4R1/data/")
#reproject points to same crs as rasters
test <-  rast("./Data_raw/dayMet/rawMonthlyData/orders/70e0da02b9d2d6e8faa8c97d211f3546/Daymet_Monthly_V4R1/data/dayMet_v4_prcp_monttl_na_1980.tif")
points_sf <- points_sf %>%
  sf::st_transform(crs(test))
# # 
# # load monthly total precip values and make into a raster stack
# for (i in 1:length(rastNames[str_detect(string = rastNames,
#                                         pattern = "prcp_monttl_na_.....tif$")])){
# 
#   name_i <- rastNames[str_detect(string = rastNames,
#                                  pattern = "prcp_monttl_na_.....tif$")][i]
#   temp_rast <- rast(paste0("./Data_raw/dayMet/rawMonthlyData/orders/70e0da02b9d2d6e8faa8c97d211f3546/Daymet_Monthly_V4R1/data/", name_i))
# 
#   # get the data for the locations we want
#   temp_points <-
#     temp_rast %>%
#     terra::extract(points_sf)
# 
#   # make column for year and change column names to month value only
#   temp_points$year <- as.numeric(str_extract(names(temp_points)[2], pattern = "\\d{4}"))
#   names(temp_points)[2:13] <- c("prcp_Jan", "prcp_Feb", "prcp_March", "prcp_April",
#                                 "prcp_May", "prcp_June", "prcp_July", "prcp_Aug",
#                                 "prcp_Sept", "prcp_Oct", "prcp_Nov", "prcp_Dec")
#   temp_points <- temp_points %>%
#     select(year, prcp_Jan:prcp_Dec) %>%
#     cbind(st_coordinates(points_sf)) %>%
#     rename(Long = X, Lat = Y)
# 
#   if (i == 1 ){
#     prcpPoints <- temp_points
#   } else {
#     prcpPoints <- rbind(prcpPoints, temp_points)
#   }
# }
# 
# 
# # load monthly average tmax values and make into a raster stack
# for (i in 1:length(rastNames[str_detect(string = rastNames,
#                                         pattern = "tmax_monavg_na_.....tif$")])){
# 
#   name_i <- rastNames[str_detect(string = rastNames,
#                                  pattern = "tmax_monavg_na_.....tif$")][i]
#   temp_rast <- rast(paste0("./Data_raw/dayMet/rawMonthlyData/orders/70e0da02b9d2d6e8faa8c97d211f3546/Daymet_Monthly_V4R1/data/", name_i))
#   # get the data for the locations we want
#   temp_points <-
#     temp_rast %>%
#     terra::extract(points_sf)
# 
#   # make column for year and change column names to month value only
#   temp_points$year <- as.numeric(str_extract(names(temp_points)[2], pattern = "\\d{4}"))
#   names(temp_points)[2:13] <- c("tmax_Jan", "tmax_Feb", "tmax_March", "tmax_April",
#                                 "tmax_May", "tmax_June", "tmax_July", "tmax_Aug",
#                                 "tmax_Sept", "tmax_Oct", "tmax_Nov", "tmax_Dec")
#   temp_points <- temp_points %>%
#     select(year, tmax_Jan:tmax_Dec) %>%
#     cbind(st_coordinates(points_sf)) %>%
#     rename(Long = X, Lat = Y)
# 
#   if (i == 1 ){
#     tmaxPoints <- temp_points
#   } else {
#     tmaxPoints <- rbind(tmaxPoints, temp_points)
#   }
# }
# 
# # load monthly average tmin values and make into a raster stack
# for (i in 1:length(rastNames[str_detect(string = rastNames,
#                                         pattern = "tmin_monavg_na_.....tif$")])){
# 
#   name_i <- rastNames[str_detect(string = rastNames,
#                                  pattern = "tmin_monavg_na_.....tif$")][i]
#   temp_rast <- rast(paste0("./Data_raw/dayMet/rawMonthlyData/orders/70e0da02b9d2d6e8faa8c97d211f3546/Daymet_Monthly_V4R1/data/", name_i))
#   # get the data for the locations we want
#   temp_points <-
#     temp_rast %>%
#     terra::extract(points_sf)
# 
#   # make column for year and change column names to month value only
#   temp_points$year <- as.numeric(str_extract(names(temp_points)[2], pattern = "\\d{4}"))
#   names(temp_points)[2:13] <- c("tmin_Jan", "tmin_Feb", "tmin_March", "tmin_April",
#                                 "tmin_May", "tmin_June", "tmin_July", "tmin_Aug",
#                                 "tmin_Sept", "tmin_Oct", "tmin_Nov", "tmin_Dec")
#   temp_points <- temp_points %>%
#     select(year, tmin_Jan:tmin_Dec) %>%
#     cbind(st_coordinates(points_sf)) %>%
#     rename(Long = X, Lat = Y)
# 
#   if (i == 1 ){
#     tminPoints <- temp_points
#   } else {
#     tminPoints <- rbind(tminPoints, temp_points)
#   }
# }
# 
# ## add all variables together (they are in the same order, so can cbind)
# # allMetDat <- tmaxPoints %>%
# #   cbind(tminPoints %>% select(-year, -Long, -Lat)) %>%
# #   #cbind(vpPoints %>% select(-year, -Long, -Lat)) %>%
# #   cbind(prcpPoints %>% select(-year, -Long, -Lat))
# tminPoints <- tminPoints %>%
#   unique()
# tmaxPoints <- tmaxPoints %>%
#   unique()
# prcpPoints <- prcpPoints %>%
#   unique()
# 
# allMetDat <- tmaxPoints %>%
#   left_join(tminPoints, by = c("year", "Long", "Lat")) %>%
#   #cbind(vpPoints %>% select("year", "Long", "Lat")) %>%
#   left_join(prcpPoints, by = c("year", "Long", "Lat"))
# 
# 
# # save data
# write.csv(allMetDat, file = "./Data_processed/CoverData/dayMet_intermediate/sampledDataForAnalysis_COVER.csv", row.names = FALSE)
# #allMetDat <- read.csv("./Data_processed/CoverData/dayMet_intermediate/sampledDataForAnalysis_COVER.csv")
# 
# # get annual climate data -------------------------------------------------
# 
# # get annual data downloaded from online
# #test <- terra::rast("./Data_raw/dayMet/rawMonthlyData/orders/70e0da02b9d2d6e8faa8c97d211f3546/Daymet_Monthly_V4R1/Data_raw/dayMet_v4_prcp_monttl_na_1980.tif")
# rastNames2 <- list.files("./Data_raw/dayMet/yearly/")
# 
# # load annual total precip values and make into a raster stack
# for (i in 1:length(rastNames2[str_detect(string = rastNames2,
#                                          pattern = "prcp_annttl_na_.....tif$")])){
# 
#   name_i <- rastNames2[str_detect(string = rastNames2,
#                                   pattern = "prcp_annttl_na_.....tif$")][i]
#   temp_rast <- rast(paste0("./Data_raw/dayMet/yearly/", name_i))
# 
#   # get the data for the locations we want
#   temp_points <-
#     temp_rast %>%
#     terra::extract(points_sf)
# 
#   # make column for year and change column names to month value only
#   temp_points$year <- as.numeric(str_extract(names(temp_points)[2], pattern = "\\d{4}"))
#   names(temp_points)[2] <- c("prcp_annTotal")
#   temp_points <- temp_points %>%
#     select(year, prcp_annTotal) %>%
#     cbind(st_coordinates(points_sf)) %>%
#     rename(Long = X, Lat = Y)
# 
#   if (i == 1 ){
#     prcpPoints_ann <- temp_points
#   } else {
#     prcpPoints_ann <- rbind(prcpPoints_ann, temp_points)
#   }
# }
# 
# # load annual tmax ann avg values and make into a raster stack
# for (i in 1:length(rastNames2[str_detect(string = rastNames2,
#                                          pattern = "tmax_annavg_na_.....tif$")])){
# 
#   name_i <- rastNames2[str_detect(string = rastNames2,
#                                   pattern = "tmax_annavg_na_.....tif$")][i]
#   temp_rast <- rast(paste0("./Data_raw/dayMet/yearly/", name_i))
# 
#   # get the data for the locations we want
#   temp_points <-
#     temp_rast %>%
#     terra::extract(points_sf)
# 
#   # make column for year and change column names to month value only
#   temp_points$year <- as.numeric(str_extract(names(temp_points)[2], pattern = "\\d{4}"))
#   names(temp_points)[2] <- c("tmax_annAvg")
#   temp_points <- temp_points %>%
#     select(year, tmax_annAvg) %>%
#     cbind(st_coordinates(points_sf)) %>%
#     rename(Long = X, Lat = Y)
# 
#   if (i == 1 ){
#     tmaxPoints_ann <- temp_points
#   } else {
#     tmaxPoints_ann <- rbind(tmaxPoints_ann, temp_points)
#   }
# }
# 
# # load annual tmin ann avg values and make into a raster stack
# for (i in 1:length(rastNames2[str_detect(string = rastNames2,
#                                          pattern = "tmin_annavg_na_.....tif$")])){
# 
#   name_i <- rastNames2[str_detect(string = rastNames2,
#                                   pattern = "tmin_annavg_na_.....tif$")][i]
#   temp_rast <- rast(paste0("./Data_raw/dayMet/yearly/", name_i))
# 
#   # get the data for the locations we want
#   temp_points <-
#     temp_rast %>%
#     terra::extract(points_sf)
# 
#   # make column for year and change column names to month value only
#   temp_points$year <- as.numeric(str_extract(names(temp_points)[2], pattern = "\\d{4}"))
#   names(temp_points)[2] <- c("tmin_annAvg")
#   temp_points <- temp_points %>%
#     select(year, tmin_annAvg) %>%
#     cbind(st_coordinates(points_sf)) %>%
#     rename(Long = X, Lat = Y)
# 
#   if (i == 1 ){
#     tminPoints_ann <- temp_points
#   } else {
#     tminPoints_ann <- rbind(tminPoints_ann, temp_points)
#   }
# }
# 
# tminPoints_ann <- tminPoints_ann %>%
#   unique()
# tmaxPoints_ann <- tmaxPoints_ann %>%
#   unique()
# prcpPoints_ann <- prcpPoints_ann %>%
#   unique()
# # join together
# annMetDat <- prcpPoints_ann %>%
#   #left_join(swePoints_ann) %>%
#   left_join(tminPoints_ann, by = c("year", "Long", "Lat")) %>%
#   left_join(tmaxPoints_ann, by = c("year", "Long", "Lat"))# %>%
#   # left_join(vpPoints_ann %>% select(-year, -Long, -Lat))
# 
# # save data
# write.csv(annMetDat, file = "./Data_processed/CoverData/dayMet_intermediate/sampledDataForAnalysis_Annual_COVER.csv", row.names = FALSE)
# #annMetDat <- read.csv("./Data_processed/CoverData/dayMet_intermediate/sampledDataForAnalysis_Annual_COVER.csv")
# 
# 
# # add annual data to the monthly data (will use later in processing)
# allMetDat2 <- allMetDat %>%
#   cbind(annMetDat %>% select(-"year", -"Lat", -"Long"))
# 
# # drop values w/ NAs (coastal locations)
# allMetDat2 <- allMetDat2 %>%
#   drop_na()
# 
# 
# # calculating climate variables for models -------------------------------
# climVar <- allMetDat2 %>%
#  #slice(23507:23909) %>%
#   mutate(totalAnnPrecip = rowSums(.[c("prcp_Jan", "prcp_Feb", "prcp_March", "prcp_April", "prcp_May", "prcp_June", "prcp_July", "prcp_Aug", "prcp_Sept", "prcp_Oct" ,"prcp_Nov", "prcp_Dec")]), # total annual precipitation
#          #maxAnnSwe = rowSums(.[28:39]), # total annual swe
#          T_warmestMonth = pmap_dbl(.[c("tmax_Jan", "tmax_Feb", "tmax_March", "tmax_April", "tmax_May", "tmax_June", "tmax_July", "tmax_Aug", "tmax_Sept", "tmax_Oct",  "tmax_Nov",  "tmax_Dec")], max), # temperature of warmest month
#          T_coldestMonth = pmap_dbl(.[c("tmin_Jan", "tmin_Feb", "tmin_March", "tmin_April", "tmin_May", "tmin_June", "tmin_July", "tmin_Aug", "tmin_Sept", "tmin_Oct",  "tmin_Nov",  "tmin_Dec")], min), # temperature of coldest month
#          #Tmin_annAvgOfMonthly_OLD = rowSums(.[c("tmin_Jan", "tmin_Feb", "tmin_March", "tmin_April", "tmin_May", "tmin_June", "tmin_July", "tmin_Aug", "tmin_Sept", "tmin_Oct",  "tmin_Nov",  "tmin_Dec")])/12,
#          ##
#          Tmin_annAvgOfMonthly = pmap_dbl(.[c("tmin_Jan", "tmin_Feb", "tmin_March", "tmin_April", "tmin_May", "tmin_June", "tmin_July", "tmin_Aug", "tmin_Sept", "tmin_Oct",  "tmin_Nov",  "tmin_Dec")],
#                                          .f = function(tmin_Jan, tmin_Feb, tmin_March, tmin_April, tmin_May, tmin_June, tmin_July, tmin_Aug, tmin_Sept, tmin_Oct,  tmin_Nov,  tmin_Dec) {
#                                            return(mean(c(
#                                               tmin_Jan * 31/31, tmin_Feb * 28.5/31, tmin_March * 31/31, tmin_April * 30/31,
#                                               tmin_May * 31/31, tmin_June * 30/31, tmin_July * 31/31, tmin_Aug * 31/31,
#                                               tmin_Sept * 30/31, tmin_Oct * 31/31,  tmin_Nov * 30/31,  tmin_Dec * 31/31
#                                            )
#                                            )) # in degrees C
#                                          }),
#          ##
#          #Tmax_annAvgOfMonthly_OLD = rowSums(.[c("tmax_Jan", "tmax_Feb", "tmax_March", "tmax_April", "tmax_May", "tmax_June", "tmax_July", "tmax_Aug", "tmax_Sept", "tmax_Oct",  "tmax_Nov",  "tmax_Dec")])/12,
#          ##
#          Tmax_annAvgOfMonthly = pmap_dbl(.[c("tmax_Jan", "tmax_Feb", "tmax_March", "tmax_April", "tmax_May", "tmax_June", "tmax_July", "tmax_Aug", "tmax_Sept", "tmax_Oct",  "tmax_Nov",  "tmax_Dec")],
#                                          .f = function(tmax_Jan, tmax_Feb, tmax_March, tmax_April, tmax_May, tmax_June, tmax_July, tmax_Aug, tmax_Sept, tmax_Oct,  tmax_Nov,  tmax_Dec) {
#                                            return(mean(c(
#                                              tmax_Jan * 31/31, tmax_Feb * 28.5/31, tmax_March * 31/31, tmax_April * 30/31,
#                                              tmax_May * 31/31, tmax_June * 30/31, tmax_July * 31/31, tmax_Aug * 31/31,
#                                              tmax_Sept * 30/31, tmax_Oct * 31/31,  tmax_Nov * 30/31,  tmax_Dec * 31/31
#                                            )
#                                            )) # in degrees C
#                                          }),
#          ##
#          precip_wettestMonth = pmap_dbl(.[c("prcp_Jan", "prcp_Feb", "prcp_March", "prcp_April", "prcp_May", "prcp_June", "prcp_July", "prcp_Aug", "prcp_Sept", "prcp_Oct" ,"prcp_Nov", "prcp_Dec")],
#                                         max), # precip of wettest month
#          precip_driestMonth = pmap_dbl(.[c("prcp_Jan", "prcp_Feb", "prcp_March", "prcp_April", "prcp_May", "prcp_June", "prcp_July", "prcp_Aug", "prcp_Sept", "prcp_Oct" ,"prcp_Nov", "prcp_Dec")],
#                                        min), # precip of driest month
#          precip_Seasonality = pmap_dbl(.[c("prcp_Jan", "prcp_Feb", "prcp_March", "prcp_April", "prcp_May", "prcp_June", "prcp_July", "prcp_Aug", "prcp_Sept", "prcp_Oct" ,"prcp_Nov", "prcp_Dec")],   # coefficient of variation (sd/mean) of precipitation
#                                        .f = function(prcp_Jan, prcp_Feb, prcp_March, prcp_April, prcp_May, prcp_June, prcp_July, prcp_Aug, prcp_Sept, prcp_Oct ,prcp_Nov, prcp_Dec, ...)
#                                        {temp <- c(prcp_Jan, prcp_Feb, prcp_March, prcp_April, prcp_May, prcp_June, prcp_July, prcp_Aug, prcp_Sept, prcp_Oct ,prcp_Nov, prcp_Dec)
#                                        sd(temp)/mean(temp)
#                                        }
#          ),
#          PrecipTempCorr = pmap_dbl(.[c("tmax_Jan", "tmax_Feb", "tmax_March", "tmax_April", "tmax_May", "tmax_June", "tmax_July", "tmax_Aug", "tmax_Sept", "tmax_Oct",  "tmax_Nov",  "tmax_Dec",
#                                        "prcp_Jan", "prcp_Feb", "prcp_March", "prcp_April", "prcp_May", "prcp_June", "prcp_July", "prcp_Aug", "prcp_Sept", "prcp_Oct" ,"prcp_Nov", "prcp_Dec")], #correlation of monthly temp and precip
#                                    .f = function(tmax_Jan, tmax_Feb, tmax_March, tmax_April, tmax_May, tmax_June, tmax_July, tmax_Aug, tmax_Sept, tmax_Oct,  tmax_Nov,  tmax_Dec,
#                                                  prcp_Jan, prcp_Feb, prcp_March, prcp_April, prcp_May, prcp_June, prcp_July, prcp_Aug, prcp_Sept, prcp_Oct ,prcp_Nov, prcp_Dec, ...) {
#                                      cor(y = c(tmax_Jan, tmax_Feb, tmax_March, tmax_April, tmax_May, tmax_June, tmax_July, tmax_Aug, tmax_Sept, tmax_Oct,  tmax_Nov,  tmax_Dec),
#                                          x = c(prcp_Jan, prcp_Feb, prcp_March, prcp_April, prcp_May, prcp_June, prcp_July, prcp_Aug, prcp_Sept, prcp_Oct ,prcp_Nov, prcp_Dec))
#                                    }),
#          aboveFreezing_month = pmap_dbl(.[c("tmin_Jan", "tmin_Feb", "tmin_March", "tmin_April", "tmin_May", "tmin_June", "tmin_July", "tmin_Aug", "tmin_Sept", "tmin_Oct",  "tmin_Nov",  "tmin_Dec")], # month when temp gets above freezing (when tmin > 0 degrees C, so no freeze at night )
#                                         .f = function(tmin_Jan, tmin_Feb, tmin_March, tmin_April, tmin_May, tmin_June, tmin_July, tmin_Aug, tmin_Sept, tmin_Oct,  tmin_Nov,  tmin_Dec) {
#                                           temp <- c(tmin_Jan, tmin_Feb, tmin_March, tmin_April, tmin_May, tmin_June, tmin_July, tmin_Aug, tmin_Sept, tmin_Oct,  tmin_Nov,  tmin_Dec)
#                                           which(temp > 0)[1] # in degrees C
#                                         }),
#          lastAboveFreezing_month = pmap_dbl(.[c("tmin_Jan", "tmin_Feb", "tmin_March", "tmin_April", "tmin_May", "tmin_June", "tmin_July", "tmin_Aug", "tmin_Sept", "tmin_Oct",  "tmin_Nov",  "tmin_Dec")], # month when temp gets above freezing (when tmin > 0 degrees C, so no freeze at night )
#                                         .f = function(tmin_Jan, tmin_Feb, tmin_March, tmin_April, tmin_May, tmin_June, tmin_July, tmin_Aug, tmin_Sept, tmin_Oct,  tmin_Nov,  tmin_Dec) {
#                                           temp <- c(tmin_Jan, tmin_Feb, tmin_March, tmin_April, tmin_May, tmin_June, tmin_July, tmin_Aug, tmin_Sept, tmin_Oct,  tmin_Nov,  tmin_Dec)
#                                           temp2 <- which(temp > 0) # in degrees C
#                                           if(length(temp2)>0) {
#                                             return(max(temp2))
#                                           } else {
#                                               return(NA)
#                                             }
#                                         }),
# 
#          isothermality = pmap_dbl(.[c("tmax_Jan", "tmax_Feb", "tmax_March", "tmax_April", "tmax_May", "tmax_June", "tmax_July", "tmax_Aug", "tmax_Sept", "tmax_Oct",  "tmax_Nov",  "tmax_Dec",
#                                       "tmin_Jan", "tmin_Feb", "tmin_March", "tmin_April", "tmin_May", "tmin_June", "tmin_July", "tmin_Aug", "tmin_Sept", "tmin_Oct",  "tmin_Nov",  "tmin_Dec")], # isothermality
#                                   .f = function(tmax_Jan, tmax_Feb, tmax_March, tmax_April, tmax_May, tmax_June, tmax_July, tmax_Aug, tmax_Sept, tmax_Oct,  tmax_Nov,  tmax_Dec,
#                                                 tmin_Jan, tmin_Feb, tmin_March, tmin_April, tmin_May, tmin_June, tmin_July, tmin_Aug, tmin_Sept, tmin_Oct,  tmin_Nov,  tmin_Dec, ...) {
#                                     tmins <- c(tmin_Jan, tmin_Feb, tmin_March, tmin_April, tmin_May, tmin_June, tmin_July, tmin_Aug, tmin_Sept, tmin_Oct,  tmin_Nov,  tmin_Dec)
#                                     tmaxes <- c(tmax_Jan, tmax_Feb, tmax_March, tmax_April, tmax_May, tmax_June, tmax_July, tmax_Aug, tmax_Sept, tmax_Oct,  tmax_Nov,  tmax_Dec)
#                                     tMaxMax <- max(c(tmax_Jan, tmax_Feb, tmax_March, tmax_April, tmax_May, tmax_June, tmax_July, tmax_Aug, tmax_Sept, tmax_Oct,  tmax_Nov,  tmax_Dec))
#                                     tMinMin <- min(c(tmin_Jan, tmin_Feb, tmin_March, tmin_April, tmin_May, tmin_June, tmin_July, tmin_Aug, tmin_Sept, tmin_Oct,  tmin_Nov,  tmin_Dec))
#                                     mean(tmaxes-tmins)/(tMaxMax-tMinMin) * 100
#                                   }),
# 
#   ) %>%
#   mutate(
#     # calculate the duration of frost-free days (in our case here, Frost-free
#     # days = (doy of first day of the first month when tmin is >0) - (doy of
#     # last day of the lost month when tmin >0))
#     # first month when tmin is above freezing is "aboveFreezing_month" in the previous d.f.
#     # last month when tmin is above freezing is "lastAboveFreezing_month" in the previous d.f.
#     durationFrostFreeDays =
#       # DOY of last day of last frost-free month (just give the 30th, since it
#       # probably isn't a bit deal if we use the 30th rather than the 31st in
#       # months when there is a 31st)
#       lubridate::yday(as.Date(paste0(lastAboveFreezing_month, "/30/2024"),
#                               format = "%m/%d/%Y")) -
#       # DOY of first day of first frost-free month
#       lubridate::yday(as.Date(paste0("0",aboveFreezing_month, "/01/2024"),
#                                                     format = "%m/%d/%Y"))
#     )
# 
# # constants for SVP calculation
# #calculate SVP according to Williams et al NatCC 2012 supplementary material -  units haPa
# a0<-6.107799961
# a1<-0.4436518521
# a2<-0.01428945805
# a3<-0.0002650648471
# a4<-0.000003031240396
# a5<-0.00000002034080948
# a6<-0.00000000006136820929
# ## calculating vapor pressure deficit, annual water deficit, and wet degree days (based on code from Adam Noel)
# 
# climVar2 <- allMetDat2%>%
#   #slice(23507:23909) %>%
#   # approximation of mean temp (just avg. of max and min, which I realize is not totally accurate)
#   mutate(tmean_Jan = (tmax_Jan + tmin_Jan)/2,
#          tmean_Feb = (tmax_Feb + tmin_Feb)/2,
#          tmean_March = (tmax_March + tmin_March)/2,
#          tmean_April = (tmax_April + tmin_April)/2,
#          tmean_May = (tmax_May + tmin_May)/2,
#          tmean_June = (tmax_June + tmin_June)/2,
#          tmean_July = (tmax_July + tmin_July)/2,
#          tmean_Aug = (tmax_Aug + tmin_Aug)/2,
#          tmean_Sept = (tmax_Sept + tmin_Sept)/2,
#          tmean_Oct = (tmax_Oct + tmin_Oct)/2,
#          tmean_Nov = (tmax_Nov + tmin_Nov)/2,
#          tmean_Dec = (tmax_Dec + tmin_Dec)/2,
#   ) %>%
#   mutate(
#     # monthly water deficit
#     awd_Jan = tmean_Jan*2 - prcp_Jan,
#     awd_Feb = tmean_Feb*2 - prcp_Feb,
#     awd_March = tmean_March*2 - prcp_March,
#     awd_April = tmean_April*2 - prcp_April,
#     awd_May = tmean_May*2 - prcp_May,
#     awd_June = tmean_June*2 - prcp_June,
#     awd_July = tmean_July*2 - prcp_July,
#     awd_Aug = tmean_Aug*2 - prcp_Aug,
#     awd_Sept = tmean_Sept*2 - prcp_Sept,
#     awd_Oct = tmean_Oct*2 - prcp_Oct,
#     awd_Nov = tmean_Nov*2 - prcp_Nov,
#     awd_Dec = tmean_Dec*2 - prcp_Dec,
#     # monthly wet degree days
#     ##aes
#     awdd_Jan =  ifelse(tmean_Jan*2 < prcp_Jan, tmean_Jan*30, NA),
#     awdd_Feb =  ifelse(tmean_Feb*2 < prcp_Feb, tmean_Feb*30, NA),
#     awdd_March =   ifelse(tmean_March*2 < prcp_March, tmean_March*30, NA),
#     awdd_April =  ifelse(tmean_April*2 < prcp_April, tmean_April*30, NA),
#     awdd_May = ifelse(tmean_May*2 < prcp_May, tmean_May*30, NA),
#     awdd_June = ifelse(tmean_June*2 < prcp_June, tmean_June*30, NA),
#     awdd_July = ifelse(tmean_July*2 < prcp_July, tmean_July*30, NA),
#     awdd_Aug = ifelse(tmean_Aug*2 < prcp_Aug, tmean_Aug*30, NA),
#     awdd_Sept = ifelse(tmean_Sept*2 < prcp_Sept, tmean_Sept*30, NA),
#     awdd_Oct = ifelse(tmean_Oct*2 < prcp_Oct, tmean_Oct*30, NA),
#     awdd_Nov = ifelse(tmean_Nov*2 < prcp_Nov, tmean_Nov*30, NA),
#     awdd_Dec = ifelse(tmean_Dec*2 < prcp_Dec, tmean_Dec*30, NA),
#     # calculate VPD in milibars (100Pa = 1mb, vp is in Pa)
#     # #(0.6108 * exp(17.27 * TMAX / (TMAX + 237.3))
#     # VPD_Jan = (6.11*exp((17.27*tmean_Jan)/(237.3 + tmean_Jan))) -  (vp_Jan/100),
#     # VPD_Feb = (6.11*exp((17.27*tmean_Feb)/(237.3 + tmean_Feb))) -  (vp_Feb/100),
#     # VPD_March = (6.11*exp((17.27*tmean_March)/(237.3 + tmean_March))) - (vp_March/100),
#     # VPD_April = (6.11*exp((17.27*tmean_April)/(237.3 + tmean_April)))- (vp_April/100),
#     # VPD_May =   (6.11*exp((17.27*tmean_May)/(237.3 + tmean_May))) - (vp_May/100),
#     # VPD_June =  (6.11*exp((17.27*tmean_June)/(237.3 + tmean_June))) - (vp_June/100) ,
#     # VPD_July =  (6.11*exp((17.27*tmean_July)/(237.3 + tmean_July))) - (vp_July/100),
#     # VPD_Aug =   (6.11*exp((17.27*tmean_Aug)/(237.3 + tmean_Aug))) -  (vp_Aug/100),
#     # VPD_Sept =  (6.11*exp((17.27*tmean_Sept)/(237.3 + tmean_Sept)))- (vp_Sept/100),
#     # VPD_Oct =   (6.11*exp((17.27*tmean_Oct)/(237.3 + tmean_Oct))) - (vp_Oct/100),
#     # VPD_Nov =   (6.11*exp((17.27*tmean_Nov)/(237.3 + tmean_Nov))) - (vp_Nov/100),
#     # VPD_Dec =   (6.11*exp((17.27*tmean_Dec)/(237.3 + tmean_Dec))) - (vp_Dec/100),
#     # second try for VPD (based on https://static-content.springer.com/esm/art%3A10.1038%2Fnclimate1693/MediaObjects/41558_2013_BFnclimate1693_MOESM272_ESM.pdf)
#     # units are Pascals
#     VPD_Jan = ((( a0+ tmean_Jan*(a1+ tmean_Jan *(a2+ tmean_Jan *(a3+ tmean_Jan *(a4	+ tmean_Jan *(a5	+ tmean_Jan *a6)))))))*100 -  (tmean_Jan))/1000,
#     VPD_Feb = ((( a0+ tmean_Feb*(a1+ tmean_Feb *(a2+ tmean_Feb *(a3+ tmean_Feb *(a4	+ tmean_Feb *(a5	+ tmean_Feb *a6)))))))*100 -  (tmean_Feb))/1000,
#     VPD_March = ((( a0+ tmean_March*(a1+ tmean_March *(a2+ tmean_March *(a3+ tmean_March *(a4	+ tmean_March *(a5	+ tmean_March *a6)))))))*100 -  (tmean_March))/1000,
#     VPD_April = ((( a0+ tmean_April*(a1+ tmean_April *(a2+ tmean_April *(a3+ tmean_April *(a4	+ tmean_April *(a5	+ tmean_April *a6)))))))*100 -  (tmean_April))/1000,
#     VPD_May =   ((( a0+ tmean_May*(a1+ tmean_May *(a2+ tmean_May *(a3+ tmean_May *(a4	+ tmean_May *(a5	+ tmean_May *a6)))))))*100 -  (tmean_May))/1000,
#     VPD_June =  ((( a0+ tmean_June*(a1+ tmean_June *(a2+ tmean_June *(a3+ tmean_June *(a4	+ tmean_June *(a5	+ tmean_June *a6)))))))*100 -  (tmean_June))/1000,
#     VPD_July =  ((( a0+ tmean_July*(a1+ tmean_July *(a2+ tmean_July *(a3+ tmean_July *(a4	+ tmean_July *(a5	+ tmean_July *a6)))))))*100 -  (tmean_July))/1000,
#     VPD_Aug =   ((( a0+ tmean_Aug*(a1+ tmean_Aug *(a2+ tmean_Aug *(a3+ tmean_Aug *(a4	+ tmean_Aug *(a5	+ tmean_Aug *a6)))))))*100 -  (tmean_Aug))/1000,
#     VPD_Sept =  ((( a0+ tmean_Sept*(a1+ tmean_Sept *(a2+ tmean_Sept *(a3+ tmean_Sept *(a4	+ tmean_Sept *(a5	+ tmean_Sept *a6)))))))*100 -  (tmean_Sept))/1000,
#     VPD_Oct =   ((( a0+ tmean_Oct*(a1+ tmean_Oct *(a2+ tmean_Oct *(a3+ tmean_Oct *(a4	+ tmean_Oct *(a5	+ tmean_Oct *a6)))))))*100 -  (tmean_Oct))/1000,
#     VPD_Nov =   ((( a0+ tmean_Nov*(a1+ tmean_Nov *(a2+ tmean_Nov *(a3+ tmean_Nov *(a4	+ tmean_Nov *(a5	+ tmean_Nov *a6)))))))*100 -  (tmean_Nov))/1000,
#     VPD_Dec =   ((( a0+ tmean_Dec*(a1+ tmean_Dec *(a2+ tmean_Dec *(a3+ tmean_Dec *(a4	+ tmean_Dec *(a5	+ tmean_Dec *a6)))))))*100 -  (tmean_Dec))/1000
#     ) %>%
#   #calculate annual values
#   transmute(#keep = c("year", "Long", "Lat"),
#   #mutate(
#     # annual water deficit (mm of water over degrees celsius)(sum across all months?)
#     tmean = pmap_dbl(.[c("tmean_Jan", "tmean_Feb", "tmean_March", "tmean_April", "tmean_May", "tmean_June", "tmean_July", "tmean_Aug", "tmean_Sept", "tmean_Oct" ,"tmean_Nov", "tmean_Dec")],
#                        .f = function(tmean_Jan, tmean_Feb, tmean_March, tmean_April, tmean_May, tmean_June, tmean_July, tmean_Aug, tmean_Sept, tmean_Oct ,tmean_Nov, tmean_Dec, ...) {
#                          temp <- sum(tmean_Jan* 31/31, tmean_Feb* 28.5/31, tmean_March* 31/31, tmean_April * 30/31,
#                                      tmean_May * 31/31, tmean_June * 30/31, tmean_July * 31/31, tmean_Aug * 31/31,
#                                      tmean_Sept * 30/31, tmean_Oct * 31/31, tmean_Nov * 30/31, tmean_Dec * 31/31)/12
#                          return(temp)
#                        }),
#     # tmean_OLD = pmap_dbl(.[c("tmean_Jan", "tmean_Feb", "tmean_March", "tmean_April", "tmean_May", "tmean_June", "tmean_July", "tmean_Aug", "tmean_Sept", "tmean_Oct" ,"tmean_Nov", "tmean_Dec")],
#     #                      .f = function(tmean_Jan, tmean_Feb, tmean_March, tmean_April, tmean_May, tmean_June, tmean_July, tmean_Aug, tmean_Sept, tmean_Oct ,tmean_Nov, tmean_Dec, ...) {
#     #                        temp <- sum(tmean_Jan, tmean_Feb, tmean_March, tmean_April,
#     #                                    tmean_May, tmean_June, tmean_July, tmean_Aug,
#     #                                    tmean_Sept, tmean_Oct, tmean_Nov, tmean_Dec)/12
#     #                        return(temp)
#     #                      }),
#     # annual water deficit (mm of water over degrees celsius)(sum across all months?)
#     annWaterDeficit = pmap_dbl(.[c("awd_Jan", "awd_Feb", "awd_March", "awd_April", "awd_May", "awd_June", "awd_July", "awd_Aug", "awd_Sept", "awd_Oct" ,"awd_Nov", "awd_Dec")],
#                                .f = function(awd_Jan, awd_Feb, awd_March, awd_April, awd_May, awd_June, awd_July, awd_Aug, awd_Sept, awd_Oct ,awd_Nov, awd_Dec, ...){
#                                  temp <- c(awd_Jan, awd_Feb, awd_March, awd_April, awd_May, awd_June, awd_July, awd_Aug, awd_Sept, awd_Oct ,awd_Nov, awd_Dec)
#                                  sum(temp[temp>0])
#                                }
#     ),
#     # annual wet degree days (temp*days) (sum only positive values)
#     annWetDegDays = pmap_dbl(.[c("awdd_Jan", "awdd_Feb", "awdd_March", "awdd_April", "awdd_May", "awdd_June", "awdd_July", "awdd_Aug", "awdd_Sept", "awdd_Oct" ,"awdd_Nov", "awdd_Dec")],
#                              .f = function(awdd_Jan, awdd_Feb, awdd_March, awdd_April, awdd_May, awdd_June, awdd_July, awdd_Aug, awdd_Sept, awdd_Oct ,awdd_Nov, awdd_Dec, ...)
#                              {
#                                temp <- c(awdd_Jan, awdd_Feb, awdd_March, awdd_April, awdd_May, awdd_June, awdd_July, awdd_Aug, awdd_Sept, awdd_Oct ,awdd_Nov, awdd_Dec)
#                                sum(temp[temp>0], na.rm = TRUE)
#                              }
#     ),
#     # annual average vapor pressure deficit (in milibars) ()
#         annVPD_mean = pmap_dbl(.[c("VPD_Jan", "VPD_Feb", "VPD_March","VPD_April" ,"VPD_May","VPD_June", "VPD_July","VPD_Aug","VPD_Sept","VPD_Oct","VPD_Nov","VPD_Dec")],
#                            .f = function(VPD_Jan, VPD_Feb, VPD_March,VPD_April ,VPD_May,VPD_June, VPD_July,VPD_Aug,VPD_Sept,VPD_Oct,VPD_Nov,VPD_Dec) {
#       mean(c(VPD_Jan* 31/31, VPD_Feb * 28.5/31, VPD_March * 31/31, VPD_April* 30/31,
#              VPD_May * 31/31, VPD_June * 30/31, VPD_July * 31/31, VPD_Aug * 31/31,
#              VPD_Sept * 30/31, VPD_Oct * 31/31, VPD_Nov * 30/31, VPD_Dec * 31/31))
#     }),
#     # annVPD_mean_OLD = pmap_dbl(.[c("VPD_Jan", "VPD_Feb", "VPD_March","VPD_April" ,"VPD_May","VPD_June", "VPD_July","VPD_Aug","VPD_Sept","VPD_Oct","VPD_Nov","VPD_Dec")],
#     #                            .f = function(VPD_Jan, VPD_Feb, VPD_March,VPD_April ,VPD_May,VPD_June, VPD_July,VPD_Aug,VPD_Sept,VPD_Oct,VPD_Nov,VPD_Dec) {
#     #                              mean(VPD_Jan, VPD_Feb, VPD_March , VPD_April,
#     #                                     VPD_May , VPD_June , VPD_July , VPD_Aug ,
#     #                                     VPD_Sept , VPD_Oct , VPD_Nov, VPD_Dec)
#     #                            }),
#     # annual maximum vapor pressure deficit (in milibars)
#     annVPD_max = pmap_dbl(.[c("VPD_Jan", "VPD_Feb", "VPD_March","VPD_April" ,"VPD_May","VPD_June", "VPD_July","VPD_Aug","VPD_Sept","VPD_Oct","VPD_Nov","VPD_Dec")], max),
#     # annual minimum vapor pressure deficit (in milibars)
#     annVPD_min = pmap_dbl(.[c("VPD_Jan", "VPD_Feb", "VPD_March","VPD_April" ,"VPD_May","VPD_June", "VPD_July","VPD_Aug","VPD_Sept","VPD_Oct","VPD_Nov","VPD_Dec")], min)
#   )
# 
# 
# climVar <- cbind(climVar, climVar2)
# plot(climVar$tmin_annAvg, climVar$tmean)
# plot(climVar$tmax_annAvg, climVar$tmean)
# 
# ## recalculate tmean as the average of annual average tmax and annual average tmean
# # climVar <- climVar %>%
# #   mutate(tmean = (tmin_annAvg + tmax_annAvg)/2)
# 
# rm(climVar2, tminPoints, tminPoints_ann, tmaxPoints, tmaxPoints_ann, prcpPoints, prcpPoints_ann)
# gc()
# 
# # save for subsequent use
# saveRDS(climVar, file = "./Data_processed/CoverData/dayMet_intermediate/climateValuesForAnalysis_monthly.rds")
# climVar <- readRDS(file="./Data_processed/CoverData/dayMet_intermediate/climateValuesForAnalysis_monthly.rds")
# 
# 
# # fix issue w/ prcp_seasonality and prcpTempCorr --------------------------
# # precip seasonality
# # Plot annual precip against precip seasonality to see what the values look like as it approaches 0…
# # climVar %>%
# #   slice_sample(n = 500000) %>%
# #   ggplot() +
# #   geom_point(aes(x = totalAnnPrecip, y = precip_Seasonality), alpha = .3) +
# #   geom_smooth(aes(y = precip_Seasonality, x = totalAnnPrecip))
# # as precip gets closer to 0, seasonality goes up (the average is 2), so that's what I'll change the NA values to
# climVar[is.na(climVar$precip_Seasonality), "precip_Seasonality"] <- 2
# 
# # precip temp corr
# #It would make sense to change this to 0 (as we approach 0 precip, it seems likely that precip and temp are uncorrelated)
# # climVar %>%
# #   slice_sample(n = 500000) %>%
# #   ggplot() +
# #   geom_point(aes(x = tmean, y = PrecipTempCorr), alpha = .3) +
# #   geom_smooth(aes(y = PrecipTempCorr, x = tmean))
# 
# #with low precip, the correlation is close to zero, but actually a bit below... will change to -.25
# climVar[is.na(climVar$PrecipTempCorr), "PrecipTempCorr"] <- -.25
# 
# # calculate sliding window inter-annual climate means ----------------------
# 
# ## calculate MAP and MAT over past years (a sliding window?)
# # function
# slidingMetMeans <- function(inDat, start, end) {
#   endActual <- end-1 # subtract one so that we're actually looking at the 30, 10, 5, etc. years previous to the "end" year
#   outDat <- inDat %>%
#     filter(year %in% c(start:endActual)) %>%
#     group_by(Long, Lat) %>%
#     summarize(#sweMax_meanAnnAvg = mean(swe_annAvg),
#               tmin_meanAnnAvg = mean(tmin_annAvg),
#               tmax_meanAnnAvg = mean(tmax_annAvg),
#               tmean_meanAnnAvg = mean(tmean),
#               #vp_meanAnnAvg = mean(vp_annAvg),
#               prcp_meanAnnTotal = mean(prcp_annTotal),
#               T_warmestMonth_meanAnnAvg = mean(T_warmestMonth), # temperature of warmest month
#               T_coldestMonth_meanAnnAvg = mean(T_coldestMonth), # temperature of coldest month
#               precip_wettestMonth_meanAnnAvg = mean(precip_wettestMonth), # precip of wettest month
#               precip_driestMonth_meanAnnAvg = mean(precip_driestMonth), # precip of driest month
#               precip_Seasonality_meanAnnAvg = mean(precip_Seasonality),
#               PrecipTempCorr_meanAnnAvg = mean(PrecipTempCorr),
#               aboveFreezing_month_meanAnnAvg = mean(aboveFreezing_month),
#               isothermality_meanAnnAvg = mean(isothermality),
#               annWaterDeficit_meanAnnAvg = mean(annWaterDeficit),
#               annWetDegDays_meanAnnAvg = mean(annWetDegDays),
#               annVPD_mean_meanAnnAvg = mean(annVPD_mean),
#               annVPD_max_meanAnnAvg = mean(annVPD_max),
#               annVPD_min_meanAnnAvg = mean(annVPD_min),
#               annVPD_max_95percentile = unname(quantile(annVPD_max, probs = 0.95, na.rm = TRUE)),
#               annWaterDeficit_95percentile = unname(quantile(annWaterDeficit, probs = 0.95, na.rm = TRUE)),
#               annWetDegDays_5percentile = unname(quantile(annWetDegDays, probs = 0.05, na.rm = TRUE)),
#               durationFrostFreeDays_5percentile = unname(quantile(durationFrostFreeDays, probs = 0.05, na.rm = TRUE)),
#               durationFrostFreeDays_meanAnnAvg = unname(mean(durationFrostFreeDays))
#     )
#   return(outDat)
# }
# 
# 
# ## for last 30 year window
# #set up parallel processing for this part of the workflow
# detectCores() # there are 16 cores
# #plan(multicore, workers = detectCores()-6)
# 
# # run parallel process
# endDats <- as.matrix(c(2011:2023))
# 
# annMeans <- #future_lapply
#   mclapply(X = endDats, #MARGIN = 1,
#            FUN = function(x)
#              slidingMetMeans(inDat = climVar[climVar$year %in% c(as.numeric(x-31):(as.numeric(x)-1)),]
#                              , start = as.numeric(x-31), end = as.numeric(x))
#   )
# 
# ## for years prior to 2011 and as early as 2000, calculate the average over any previous years (which will be less than 30, as few as 20, but is ok...)
# endDats2 <- as.matrix(c(2000:2010))
# annMeans_2 <-
#   mclapply(X = endDats2,
#            FUN = function(x)
#              slidingMetMeans(inDat = climVar[climVar$year %in% c(1980:(as.numeric(x)-1)),]
#                              , start = 1980, end = as.numeric(x))
#   )
# 
# # put together into one list
# annMeans_all <- c(annMeans, annMeans_2)
# 
# names(annMeans_all) <- c(2011:2023, 2000:2010)
# annMeans_30yr_temp1 <- lapply(endDats, function(x) {
#   temp <- cbind(annMeans_all[[as.character(x)]], x)
#   temp$Start <- x-31
#   names(temp) <- c(names(annMeans_all[[1]]), "End", "Start")
#   return(temp)
# })
# annMeans_30yr_temp2 <- lapply(endDats2, function(x) {
#   temp <- cbind(annMeans_all[[as.character(x)]], x)
#   temp$Start <- 1980
#   names(temp) <- c(names(annMeans_all[[1]]), "End", "Start")
#   return(temp)
# })
# 
# 
# annMeans_30yr <- data.table::rbindlist(c(annMeans_30yr_temp1, annMeans_30yr_temp2))
# 
# names(annMeans_30yr)[3:26] <- paste0(names(annMeans_30yr)[3:26], "_30yr")
# saveRDS(annMeans_30yr, "./Data_processed/CoverData/dayMet_intermediate/annMeans_30yrs.rds")
# #annMeans_30yr <- readRDS("./Data_processed/CoverData/dayMet_intermediate/annMeans_30yrs.rds")
# rm(annMeans_30yr_temp1, annMeans_30yr_temp2)
# gc()
# 
# # for last 3-year window
# # start in 2000, since we only have climate data starting then anyway
# endDats <- as.matrix(c(2000:2023))
# annMeans <-
#   mclapply(X = endDats,
#            FUN = function(x)
#              slidingMetMeans(inDat = climVar[climVar$year %in% c(as.numeric(x-4):(as.numeric(x)-1)),]
#                              , start = as.numeric(x-4), end = as.numeric(x))
#   )
# 
# # annMeans <- apply(endDats, MARGIN = 1, FUN = function(x)
# #   slidingMetMeans(inDat = climVar, start = as.numeric(x-4), end = as.numeric(x))
# # )
# names(annMeans) <- c(2000:2023)
# 
# annMeans_3yr <- lapply(endDats, function(x) {
#   temp <- cbind(annMeans[[as.character(x)]], x)
#   temp$Start <- x-4
#   names(temp) <- c(names(annMeans[[1]]), "End", "Start")
#   return(temp)
# })
# 
# annMeans_3yr <- data.table::rbindlist(annMeans_3yr)
# names(annMeans_3yr)[3:26] <- paste0(names(annMeans_3yr)[3:26], "_3yr")
# saveRDS(annMeans_3yr, "./Data_processed/CoverData/dayMet_intermediate/annMeans_3yrs.rds")
# #annMeans_3yr <- readRDS("./Data_processed/CoverData/dayMet_intermediate/annMeans_3yrs.rds")
# 
# rm(annMeans)
# gc()
# 
# 
# ## add lagged data to the main climate value data.frame
# test <- climVar %>%
#   select(-c(tmax_Jan:tmax_Dec, tmin_Jan:prcp_Dec)) %>%
#   #filter(year == 2020) %>%
#   #slice(1:100) %>%
#   left_join(annMeans_30yr, by = c("year" = "End_30yr",
#                                   "Long" = "Long",
#                                   "Lat" = "Lat")) %>%
#   left_join(annMeans_3yr, by = c("year" = "End_3yr",
#                                   "Long" = "Long",
#                                   "Lat" = "Lat"))
# 
# 
# 
# rm(annMeans_30yr, annMeans_3yr)
# gc()
# 
# # save intermediate data
# saveRDS(test, "./Data_processed/CoverData/dayMet_intermediate/climVars_AnnualMeansAndLaggedValues.rds")
# #test <- readRDS("./Data_processed/CoverData/dayMet_intermediate/climVars_AnnualMeansAndLaggedValues.rds")
# 
# # for those years including and after 2010, use the averages over the previous
# # 30 years for climate. for those years before 2010, use the averages over any
# # previous years starting w/ 1980 -- we calculated this previously, but rename here for clarity
# 
# testNew <- test %>%
#   rename("tmin_meanAnnAvg_CLIM" = tmin_meanAnnAvg_30yr,
#          "tmin_meanAnnAvg_CLIM" = tmin_meanAnnAvg_30yr                  ,
#          "tmax_meanAnnAvg_CLIM" = tmax_meanAnnAvg_30yr                  ,
#          "tmean_meanAnnAvg_CLIM" = tmean_meanAnnAvg_30yr                 ,
#          "prcp_meanAnnTotal_CLIM" = prcp_meanAnnTotal_30yr                ,
#          "T_warmestMonth_meanAnnAvg_CLIM" = T_warmestMonth_meanAnnAvg_30yr        ,
#          "T_coldestMonth_meanAnnAvg_CLIM" = T_coldestMonth_meanAnnAvg_30yr    ,
#          "precip_wettestMonth_meanAnnAvg_CLIM" = precip_wettestMonth_meanAnnAvg_30yr   ,
#          "precip_driestMonth_meanAnnAvg_CLIM" = precip_driestMonth_meanAnnAvg_30yr    ,
#          "precip_Seasonality_meanAnnAvg_CLIM" = precip_Seasonality_meanAnnAvg_30yr  ,
#          "PrecipTempCorr_meanAnnAvg_CLIM" = PrecipTempCorr_meanAnnAvg_30yr        ,
#          "aboveFreezing_month_meanAnnAvg_CLIM" = aboveFreezing_month_meanAnnAvg_30yr   ,
#          "isothermality_meanAnnAvg_CLIM" = isothermality_meanAnnAvg_30yr     ,
#          "annWaterDeficit_meanAnnAvg_CLIM" = annWaterDeficit_meanAnnAvg_30yr     ,
#          "annWetDegDays_meanAnnAvg_CLIM" = annWetDegDays_meanAnnAvg_30yr        ,
#          "annVPD_mean_meanAnnAvg_CLIM" = annVPD_mean_meanAnnAvg_30yr    ,
#          "annVPD_max_meanAnnAvg_CLIM" = annVPD_max_meanAnnAvg_30yr            ,
#          "annVPD_min_meanAnnAvg_CLIM" = annVPD_min_meanAnnAvg_30yr            ,
#          "annVPD_max_95percentile_CLIM" = annVPD_max_95percentile_30yr,
#          "annWaterDeficit_95percentile_CLIM" = annWaterDeficit_95percentile_30yr     ,
#          "annWetDegDays_5percentile_CLIM" = annWetDegDays_5percentile_30yr        ,
#          "durationFrostFreeDays_5percentile_CLIM" = durationFrostFreeDays_5percentile_30yr,
#          "durationFrostFreeDays_meanAnnAvg_CLIM" = durationFrostFreeDays_meanAnnAvg_30yr ,
#          "Start_CLIM" = Start_30yr)
# 
# rm(climVar)
# gc()
# 
# # calculate anomalies -----------------------------------------------------
# 
# anomDat_3yr <- testNew %>%
#   transmute(
#     # compare 5 yr values to 30 yr values
#     # swe as % difference
#     #swe_meanAnn_3yrAnom = swe_meanAnnAvg_CLIM - swe_meanAnnAvg_3yr/swe_meanAnnAvg_CLIM,
#     # tmean as absolute difference
#     tmean_meanAnnAvg_3yrAnom = tmean_meanAnnAvg_CLIM - tmean_meanAnnAvg_3yr,
#     # tmin as absolute difference
#     tmin_meanAnnAvg_3yrAnom = tmin_meanAnnAvg_CLIM - tmin_meanAnnAvg_3yr,
#     # tmax as absolute difference
#     tmax_meanAnnAvg_3yrAnom = tmax_meanAnnAvg_CLIM - tmax_meanAnnAvg_3yr,
#     # vp as % difference
#     #vp_meanAnnAvg_3yrAnom = (vp_meanAnnAvg_CLIM - vp_meanAnnAvg_3yr)/vp_meanAnnAvg_CLIM,
#     # prcp as % difference
#     prcp_meanAnnTotal_3yrAnom = (prcp_meanAnnTotal_CLIM - prcp_meanAnnTotal_3yr)/prcp_meanAnnTotal_CLIM,
#     # t warmest month as absolute difference
#     T_warmestMonth_meanAnnAvg_3yrAnom = T_warmestMonth_meanAnnAvg_CLIM - T_warmestMonth_meanAnnAvg_3yr,
#     # t coldest month as absolute difference
#     T_coldestMonth_meanAnnAvg_3yrAnom = T_coldestMonth_meanAnnAvg_CLIM - T_coldestMonth_meanAnnAvg_3yr,
#     # precip wettest month as % difference
#     precip_wettestMonth_meanAnnAvg_3yrAnom = (precip_wettestMonth_meanAnnAvg_CLIM - precip_wettestMonth_meanAnnAvg_3yr)/precip_wettestMonth_meanAnnAvg_CLIM,
#     # precip driest month as % difference
#     precip_driestMonth_meanAnnAvg_3yrAnom = (precip_driestMonth_meanAnnAvg_CLIM - precip_driestMonth_meanAnnAvg_3yr)/precip_driestMonth_meanAnnAvg_CLIM,
#     # precip seasonality as % difference
#     precip_Seasonality_meanAnnAvg_3yrAnom = (precip_Seasonality_meanAnnAvg_CLIM - precip_Seasonality_meanAnnAvg_3yr)/precip_Seasonality_meanAnnAvg_CLIM,
#     # precip tempCorr as absolute difference
#     PrecipTempCorr_meanAnnAvg_3yrAnom = PrecipTempCorr_meanAnnAvg_CLIM - PrecipTempCorr_meanAnnAvg_3yr,
#     # above Freezing month as absolute difference
#     aboveFreezing_month_meanAnnAvg_3yrAnom = aboveFreezing_month_meanAnnAvg_CLIM - aboveFreezing_month_meanAnnAvg_3yr,
#     # isothermailty as absolute difference
#     isothermality_meanAnnAvg_3yrAnom = isothermality_meanAnnAvg_CLIM - isothermality_meanAnnAvg_3yr,
#     # annual water deficit as % difference
#     annWaterDeficit_meanAnnAvg_3yrAnom = ((annWaterDeficit_meanAnnAvg_CLIM+.0001) - annWaterDeficit_meanAnnAvg_3yr)/(annWaterDeficit_meanAnnAvg_CLIM+.0001),
#     # wet degree days as % difference
#     annWetDegDays_meanAnnAvg_3yrAnom = (annWetDegDays_meanAnnAvg_CLIM - annWetDegDays_meanAnnAvg_3yr)/annWetDegDays_meanAnnAvg_CLIM,
#     # mean VPD as absolute difference
#     annVPD_mean_meanAnnAvg_3yrAnom = (annVPD_mean_meanAnnAvg_CLIM - annVPD_mean_meanAnnAvg_3yr),
#     # min VPD as absolute difference
#     annVPD_min_meanAnnAvg_3yrAnom = (annVPD_min_meanAnnAvg_CLIM - annVPD_min_meanAnnAvg_3yr),
#     # max VPD as absolute difference
#     annVPD_max_meanAnnAvg_3yrAnom = (annVPD_max_meanAnnAvg_CLIM - annVPD_max_meanAnnAvg_3yr),
#     # 95th percentile of max VPD as absolute difference
#     annVPD_max_95percentile_3yrAnom = (annVPD_max_95percentile_CLIM - annVPD_max_95percentile_3yr),
#     # 95th percentile of annual water deficit as % difference
#     annWaterDeficit_95percentile_3yrAnom = ((annWaterDeficit_95percentile_CLIM + .0001) - annWaterDeficit_95percentile_3yr)/(annWaterDeficit_95percentile_CLIM + .0001),
#     # 5th percentile of annual wet degree days as % difference
#     annWetDegDays_5percentile_3yrAnom = ((annWetDegDays_5percentile_CLIM + .0001) - annWetDegDays_5percentile_3yr)/(annWetDegDays_5percentile_CLIM + .0001),
#     # 5th percentile of frost-free days as absolute difference
#     durationFrostFreeDays_5percentile_3yrAnom = (durationFrostFreeDays_5percentile_CLIM - durationFrostFreeDays_5percentile_3yr),
#     # mean of frost free days as absolute difference
#     durationFrostFreeDays_meanAnnAvg_3yrAnom = (durationFrostFreeDays_meanAnnAvg_CLIM - durationFrostFreeDays_meanAnnAvg_3yr)
#   )
# 
# climDat <- cbind(testNew, #anomDat_10yr,
#                  anomDat_3yr
# )
# 
# 
# # remove years for which there are no climate averages (any time before 2000) --------------------
# climDatNew <- climDat[climDat$year > 1999,]
# 
# # fix issue w/ anomaly of precip driest month -----------------------------
# 
# climDatNew[(climDatNew$precip_driestMonth_meanAnnAvg_CLIM == 0 & !is.na(climDatNew$precip_driestMonth_meanAnnAvg_CLIM))
#         & (climDatNew$precip_driestMonth_meanAnnAvg_3yr == 0 & !is.na(climDatNew$precip_driestMonth_meanAnnAvg_3yr)),
#         c("precip_driestMonth_meanAnnAvg_3yrAnom")] <- 0
# climDatNew[(climDatNew$precip_driestMonth_meanAnnAvg_29yr == 0 & !is.na(climDatNew$precip_driestMonth_meanAnnAvg_29yr))
#            & (climDatNew$precip_driestMonth_meanAnnAvg_2yr == 0 & !is.na(climDatNew$precip_driestMonth_meanAnnAvg_2yr)),
#            c("precip_driestMonth_meanAnnAvg_2yrAnom")] <- 0
# 
# rm(climDat, allMetDat, allMetDat2, annMetDat)
# gc()
# 
# # save climate values for analysis
# saveRDS(climDatNew, "./Data_processed/CoverData/dayMetClimateValuesForAnalysis_final.rds")
# #climDatNew <- readRDS("./Data_processed/CoverData/dayMetClimateValuesForAnalysis_final.rds")
# 
# 
# # Determine accuracy of 'fixes' for precip seasonality and precipT --------
# # precipSeasonalityDat <- climDatNew %>%
# #   select(year, Long, Lat, precip_Seasonality_meanAnnAvg_CLIM, precip_Seasonality_meanAnnAvg_2yr,
# #          precip_Seasonality_meanAnnAvg_3yr, precip_Seasonality_meanAnnAvg_29yr,
# #          precip_Seasonality_meanAnnAvg_3yrAnom, precip_Seasonality_meanAnnAvg_2yrAnom)
# #
# # precipSeasonalityDat %>%
# #   filter(year %in% c(2020:2023)) %>%
# #   ggplot() +
# #   geom_point(aes(Long, Lat, col = precip_Seasonality_meanAnnAvg_CLIM))

# Now, add the climate data to the spatially averaged cover observations -----------------------------------------------
climDatNew <- readRDS( "./Data_processed/CoverData/dayMetClimateValuesForAnalysis_final.rds")
## assign a 'unique ID' to each location (So the same location has the same ID across years)

climDatNew$locID <- paste0(climDatNew$Lat, "_", climDatNew$Long)
climDatNew$uniqueID <- c(1:nrow(climDatNew))

# make points for "locID" in a single year into an sf data.frame, then convert to NAD83
climSF <- climDatNew %>% 
  dplyr::filter(year == as.integer(2011)) %>% 
  sf::st_as_sf(coords = c("Long", "Lat"), crs =  crs(points_sf)) %>% 
  sf::st_transform(crs = crs("EPSG:4269")) %>% 
  select(locID)

# udpate CRS of test5 
test6 <- test5 %>%
  #slice_sample(n = 1000) %>% 
  #rename(Lon = "x", Lat = "y") %>% 
  sf::st_as_sf(coords = c("x", "y"), crs = crs("EPSG:4269")) %>% 
  st_transform( crs =  "GEOGCRS[\"NAD83\",\n    DATUM[\"North American Datum 1983\",\n        ELLIPSOID[\"GRS 1980\",6378137,298.257222101,\n            LENGTHUNIT[\"metre\",1]]],\n    PRIMEM[\"Greenwich\",0,\n        ANGLEUNIT[\"degree\",0.0174532925199433]],\n    CS[ellipsoidal,2],\n        AXIS[\"geodetic latitude (Lat)\",north,\n            ORDER[1],\n            ANGLEUNIT[\"degree\",0.0174532925199433]],\n        AXIS[\"geodetic longitude (Lon)\",east,\n            ORDER[2],\n            ANGLEUNIT[\"degree\",0.0174532925199433]],\n    ID[\"EPSG\",4269]]")

st_crs(test6) == st_crs(climSF)

#mapview::mapview(test6) + mapview::mapview(climSF, col.regions = "red")
# plot(climSF$geometry)
# points(test6$geometry, col = "red")

#Add in the 'locID' column from the climSF data.frame
test7 <- test6 %>%
  dplyr::ungroup() %>% 
  #   #slice_sample(n = 1000) %>% 
  #   rename(Lon = x, Lat = y) %>% 
  #   sf::st_as_sf(coords = c("Lon", "Lat"), crs = st_crs(dat2)) %>%
  #   st_transform(crs(test)) %>% 
  mutate("x" = st_coordinates(.)[,1],
         "y" = st_coordinates(.)[,2]) %>% 
  #st_buffer(400) %>% 
  sf::st_join(climSF, join = st_nearest_feature)

# add back in all climate data based on the "locID"
allDat_avg <- test7 %>% 
  left_join(climDatNew, by = c("locID", "Year" = "year"))

## save the data
saveRDS(allDat_avg, "./Data_processed/CoverData/DataForModels_spatiallyAveraged_sf_sampledLANDFIRE.rds")
saveRDS(st_drop_geometry(allDat_avg), "./Data_processed/CoverData/DataForModels_spatiallyAveraged_NoSf_sampledLANDFIRE.rds")


allDat_avg %>%
  filter(Year == 2013) %>%
  ggplot() +
  facet_wrap(~Year) +
  geom_point(aes(x = x, y = y, color = tmean))

