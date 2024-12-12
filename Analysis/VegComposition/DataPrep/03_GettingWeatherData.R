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

# Load data ---------------------------------------------------------------

# ## get veg data to get locations (are some duplicated locates b/c plots have data from multiple years)
# vegDat <- read.csv("./data/DataForAnalysis.csv") #%>% 
# #   select(Lat, Lon) %>% 
# #   unique()
# # points <- vegDat %>% 
# #   select(UniqueID, Lat, Lon) %>% 
# #   unique()
# # write.csv(points, "./data/dayMet/dayMetPoints.csv", row.names = FALSE)
# years <- unique(vegDat$Year)
# # get sf point data
# points_sf <- st_read(dsn = "./data/DataForAnalysisPoints/", layer = "vegCompPoints")
# 
# 
# ## get monthly data downloaded from online
# #test <- terra::rast("./data/dayMet/rawMonthlyData/orders/70e0da02b9d2d6e8faa8c97d211f3546/Daymet_Monthly_V4R1/data/daymet_v4_prcp_monttl_na_1980.tif")
# rastNames <- list.files("./data/dayMet/rawMonthlyData/orders/70e0da02b9d2d6e8faa8c97d211f3546/Daymet_Monthly_V4R1/data/")
# #reproject points to same crs as rasters
# test <-  rast("./data/dayMet/rawMonthlyData/orders/70e0da02b9d2d6e8faa8c97d211f3546/Daymet_Monthly_V4R1/data/daymet_v4_prcp_monttl_na_1980.tif")
# points_sf <- points_sf %>% 
#   sf::st_transform(crs(test))
# 
# # load monthly total precip values and make into a raster stack
# for (i in 1:length(rastNames[str_detect(string = rastNames, 
#                                         pattern = "prcp_monttl_na_.....tif$")])){
#   
#   name_i <- rastNames[str_detect(string = rastNames, 
#                                  pattern = "prcp_monttl_na_.....tif$")][i]
#   temp_rast <- rast(paste0("./data/dayMet/rawMonthlyData/orders/70e0da02b9d2d6e8faa8c97d211f3546/Daymet_Monthly_V4R1/data/", name_i))
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
# # load monthly average sweMax values and make into a raster stack
# for (i in 1:length(rastNames[str_detect(string = rastNames, 
#                                         pattern = "swe_monavg_na_.....tif$")])){
#   
#   name_i <- rastNames[str_detect(string = rastNames, 
#                                  pattern = "swe_monavg_na_.....tif$")][i]
#   temp_rast <- rast(paste0("./data/dayMet/rawMonthlyData/orders/70e0da02b9d2d6e8faa8c97d211f3546/Daymet_Monthly_V4R1/data/", name_i))
#   
#   # get the data for the locations we want
#   temp_points <- 
#     temp_rast %>% 
#     terra::extract(points_sf)
#   
#   # make column for year and change column names to month value only
#   temp_points$year <- as.numeric(str_extract(names(temp_points)[2], pattern = "\\d{4}"))
#   names(temp_points)[2:13] <- c("swe_Jan", "swe_Feb", "swe_March", "swe_April", 
#                                 "swe_May", "swe_June", "swe_July", "swe_Aug", 
#                                 "swe_Sept", "swe_Oct", "swe_Nov", "swe_Dec")
#   temp_points <- temp_points %>% 
#     select(year, swe_Jan:swe_Dec) %>% 
#     cbind(st_coordinates(points_sf)) %>% 
#     rename(Long = X, Lat = Y)
#   
#   if (i == 1 ){
#     swePoints <- temp_points
#   } else {
#     swePoints <- rbind(swePoints, temp_points)
#   }
# }
# 
# # load monthly average tmax values and make into a raster stack
# for (i in 1:length(rastNames[str_detect(string = rastNames, 
#                                         pattern = "tmax_monavg_na_.....tif$")])){
#   
#   name_i <- rastNames[str_detect(string = rastNames, 
#                                  pattern = "tmax_monavg_na_.....tif$")][i]
#   temp_rast <- rast(paste0("./data/dayMet/rawMonthlyData/orders/70e0da02b9d2d6e8faa8c97d211f3546/Daymet_Monthly_V4R1/data/", name_i))
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
#   temp_rast <- rast(paste0("./data/dayMet/rawMonthlyData/orders/70e0da02b9d2d6e8faa8c97d211f3546/Daymet_Monthly_V4R1/data/", name_i))
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
# # load monthly average vapor pressure (?) values and make into a raster stack
# for (i in 1:length(rastNames[str_detect(string = rastNames, 
#                                         pattern = "vp_monavg_na_.....tif$")])){
#   
#   name_i <- rastNames[str_detect(string = rastNames, 
#                                  pattern = "vp_monavg_na_.....tif$")][i]
#   temp_rast <- rast(paste0("./data/dayMet/rawMonthlyData/orders/70e0da02b9d2d6e8faa8c97d211f3546/Daymet_Monthly_V4R1/data/", name_i))
#   # get the data for the locations we want
#   temp_points <- 
#     temp_rast %>% 
#     terra::extract(points_sf)
#   
#   # make column for year and change column names to month value only
#   temp_points$year <- as.numeric(str_extract(names(temp_points)[2], pattern = "\\d{4}"))
#   names(temp_points)[2:13] <- c("vp_Jan", "vp_Feb", "vp_March", "vp_April", "vp_May", 
#                                 "vp_June", "vp_July", "vp_Aug", "vp_Sept", "vp_Oct", "vp_Nov", "vp_Dec")
#   temp_points <- temp_points %>% 
#     select(year, vp_Jan:vp_Dec) %>% 
#     cbind(st_coordinates(points_sf)) %>% 
#     rename(Long = X, Lat = Y)
#   
#   if (i == 1 ){
#     vpPoints <- temp_points
#   } else {
#     vpPoints <- rbind(vpPoints, temp_points)
#   }
# }
# 
# 
# ## add all variables together
# allMetDat <- tmaxPoints %>% 
#   left_join(tminPoints) %>% 
#   left_join(swePoints) %>% 
#   left_join(vpPoints) %>% 
#   left_join(prcpPoints)

# save data
#write.csv(allMetDat, file = "./data/dayMet/sampledDataForAnalysis.csv", row.names = FALSE)
allMetDat <- read.csv("./Data_raw/dayMet/sampledDataForAnalysis.csv")

# get annual climate data -------------------------------------------------

# # get annual data downloaded from online
# #test <- terra::rast("./data/dayMet/rawMonthlyData/orders/70e0da02b9d2d6e8faa8c97d211f3546/Daymet_Monthly_V4R1/data/daymet_v4_prcp_monttl_na_1980.tif")
# rastNames2 <- list.files("./Data_raw/dayMet/yearly/")
# 
# # load annual total precip values and make into a raster stack
# for (i in 1:length(rastNames2[str_detect(string = rastNames2,
#                                          pattern = "prcp_annttl_na_.....tif$")])){
# 
#   name_i <- rastNames2[str_detect(string = rastNames2,
#                                   pattern = "prcp_annttl_na_.....tif$")][i]
#   temp_rast <- rast(paste0("./data/dayMet/yearly/", name_i))
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
# # load annual total swe values and make into a raster stack
# for (i in 1:length(rastNames2[str_detect(string = rastNames2,
#                                          pattern = "swe_annavg_na_.....tif$")])){
# 
#   name_i <- rastNames2[str_detect(string = rastNames2,
#                                   pattern = "swe_annavg_na_.....tif$")][i]
#   temp_rast <- rast(paste0("./data/dayMet/yearly/", name_i))
# 
#   # get the data for the locations we want
#   temp_points <-
#     temp_rast %>%
#     terra::extract(points_sf)
# 
#   # make column for year and change column names to month value only
#   temp_points$year <- as.numeric(str_extract(names(temp_points)[2], pattern = "\\d{4}"))
#   names(temp_points)[2] <- c("swe_annAvg")
#   temp_points <- temp_points %>%
#     select(year, swe_annAvg) %>%
#     cbind(st_coordinates(points_sf)) %>%
#     rename(Long = X, Lat = Y)
# 
#   if (i == 1 ){
#     swePoints_ann <- temp_points
#   } else {
#     swePoints_ann <- rbind(swePoints_ann, temp_points)
#   }
# }
# 
# # load annual tmax ann avg values and make into a raster stack
# for (i in 1:length(rastNames2[str_detect(string = rastNames2,
#                                          pattern = "tmax_annavg_na_.....tif$")])){
# 
#   name_i <- rastNames2[str_detect(string = rastNames2,
#                                   pattern = "tmax_annavg_na_.....tif$")][i]
#   temp_rast <- rast(paste0("./data/dayMet/yearly/", name_i))
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
#   temp_rast <- rast(paste0("./data/dayMet/yearly/", name_i))
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
# # load annual vp ann avg values and make into a raster stack
# for (i in 1:length(rastNames2[str_detect(string = rastNames2,
#                                          pattern = "vp_annavg_na_.....tif$")])){
# 
#   name_i <- rastNames2[str_detect(string = rastNames2,
#                                   pattern = "vp_annavg_na_.....tif$")][i]
#   temp_rast <- rast(paste0("./data/dayMet/yearly/", name_i))
# 
#   # get the data for the locations we want
#   temp_points <-
#     temp_rast %>%
#     terra::extract(points_sf)
# 
#   # make column for year and change column names to month value only
#   temp_points$year <- as.numeric(str_extract(names(temp_points)[2], pattern = "\\d{4}"))
#   names(temp_points)[2] <- c("vp_annAvg")
#   temp_points <- temp_points %>%
#     select(year, vp_annAvg) %>%
#     cbind(st_coordinates(points_sf)) %>%
#     rename(Long = X, Lat = Y)
# 
#   if (i == 1 ){
#     vpPoints_ann <- temp_points
#   } else {
#     vpPoints_ann <- rbind(vpPoints_ann, temp_points)
#   }
# }
# 
# # join together
# annMetDat <- prcpPoints_ann %>%
#   left_join(swePoints_ann) %>%
#   left_join(tminPoints_ann) %>%
#   left_join(tmaxPoints_ann) %>%
#   left_join(vpPoints_ann)

# save data
#write.csv(annMetDat, file = "./data/dayMet/sampledDataForAnalysis_Annual.csv", row.names = FALSE)
annMetDat <- read.csv("./Data_raw/dayMet/sampledDataForAnalysis_Annual.csv")

# add annual data to the monthly data (will use later in processing)
allMetDat2 <- allMetDat %>% 
  left_join(annMetDat)

# calculating climate variables for models -------------------------------
climVar <- allMetDat2 %>% 
  mutate(totalAnnPrecip = rowSums(.[52:63]), # total annual precipitation
         maxAnnSwe = rowSums(.[28:39]), # total annual swe
         T_warmestMonth = pmap_dbl(.[2:13], max), # temperature of warmest month
         T_coldestMonth = pmap_dbl(.[16:27], min), # temperature of coldest month
         Tmin_annAvgOfMonthly = rowSums(.[16:27])/12,
         Tmax_annAvgOfMonthly = rowSums(.[2:13])/12,
         meanAnnVp = rowMeans(.[40:51]), # annual mean vapor pressure
         precip_wettestMonth = pmap_dbl(.[52:63], max), # precip of wettest month
         precip_driestMonth = pmap_dbl(.[52:63], min), # precip of driest month
         precip_Seasonality = pmap_dbl(.[52:63],   # coefficient of variation (sd/mean) of precipitation
                                       .f = function(prcp_Jan, prcp_Feb, prcp_March, prcp_April, prcp_May, prcp_June, prcp_July, prcp_Aug, prcp_Sept, prcp_Oct ,prcp_Nov, prcp_Dec, ...) 
                                       {temp <- c(prcp_Jan, prcp_Feb, prcp_March, prcp_April, prcp_May, prcp_June, prcp_July, prcp_Aug, prcp_Sept, prcp_Oct ,prcp_Nov, prcp_Dec)
                                       sd(temp)/mean(temp)
                                       }
         ),
         PrecipTempCorr = pmap_dbl(.[c(2:13,52:63)], #correlation of monthly temp and precip
                                   .f = function(tmax_Jan, tmax_Feb, tmax_March, tmax_April, tmax_May, tmax_June, tmax_July, tmax_Aug, tmax_Sept, tmax_Oct,  tmax_Nov,  tmax_Dec,
                                                 prcp_Jan, prcp_Feb, prcp_March, prcp_April, prcp_May, prcp_June, prcp_July, prcp_Aug, prcp_Sept, prcp_Oct ,prcp_Nov, prcp_Dec, ...) {
                                     cor(y = c(tmax_Jan, tmax_Feb, tmax_March, tmax_April, tmax_May, tmax_June, tmax_July, tmax_Aug, tmax_Sept, tmax_Oct,  tmax_Nov,  tmax_Dec), 
                                         x = c(prcp_Jan, prcp_Feb, prcp_March, prcp_April, prcp_May, prcp_June, prcp_July, prcp_Aug, prcp_Sept, prcp_Oct ,prcp_Nov, prcp_Dec))
                                   }),
         aboveFreezing_month = pmap_dbl(.[16:27], # month when temp gets above freezing (when tmin > 0 degrees C, so no freeze at night )
                                        .f = function(tmin_Jan, tmin_Feb, tmin_March, tmin_April, tmin_May, tmin_June, tmin_July, tmin_Aug, tmin_Sept, tmin_Oct,  tmin_Nov,  tmin_Dec) {
                                          temp <- c(tmin_Jan, tmin_Feb, tmin_March, tmin_April, tmin_May, tmin_June, tmin_July, tmin_Aug, tmin_Sept, tmin_Oct,  tmin_Nov,  tmin_Dec)
                                          which(temp > 0)[1] # in degrees C
                                        }),
         lastAboveFreezing_month = pmap_dbl(.[16:27], # month when temp gets above freezing (when tmin > 0 degrees C, so no freeze at night )
                                        .f = function(tmin_Jan, tmin_Feb, tmin_March, tmin_April, tmin_May, tmin_June, tmin_July, tmin_Aug, tmin_Sept, tmin_Oct,  tmin_Nov,  tmin_Dec) {
                                          temp <- c(tmin_Jan, tmin_Feb, tmin_March, tmin_April, tmin_May, tmin_June, tmin_July, tmin_Aug, tmin_Sept, tmin_Oct,  tmin_Nov,  tmin_Dec)
                                          temp2 <- which(temp > 0) # in degrees C
                                          if(length(temp2)>0) {
                                            return(max(temp2))
                                          } else {
                                              return(NA)
                                            }
                                        }),
         
         isothermality = pmap_dbl(.[c(2:13,16:27)], # isothermality
                                  .f = function(tmax_Jan, tmax_Feb, tmax_March, tmax_April, tmax_May, tmax_June, tmax_July, tmax_Aug, tmax_Sept, tmax_Oct,  tmax_Nov,  tmax_Dec,
                                                tmin_Jan, tmin_Feb, tmin_March, tmin_April, tmin_May, tmin_June, tmin_July, tmin_Aug, tmin_Sept, tmin_Oct,  tmin_Nov,  tmin_Dec, ...) {
                                    tmins <- c(tmin_Jan, tmin_Feb, tmin_March, tmin_April, tmin_May, tmin_June, tmin_July, tmin_Aug, tmin_Sept, tmin_Oct,  tmin_Nov,  tmin_Dec)
                                    tmaxes <- c(tmax_Jan, tmax_Feb, tmax_March, tmax_April, tmax_May, tmax_June, tmax_July, tmax_Aug, tmax_Sept, tmax_Oct,  tmax_Nov,  tmax_Dec)
                                    tMaxMax <- max(c(tmax_Jan, tmax_Feb, tmax_March, tmax_April, tmax_May, tmax_June, tmax_July, tmax_Aug, tmax_Sept, tmax_Oct,  tmax_Nov,  tmax_Dec))
                                    tMinMin <- min(c(tmin_Jan, tmin_Feb, tmin_March, tmin_April, tmin_May, tmin_June, tmin_July, tmin_Aug, tmin_Sept, tmin_Oct,  tmin_Nov,  tmin_Dec))
                                    mean(tmins-tmaxes)/(tMaxMax-tMinMin) * 100
                                  }),
         
  ) %>% 
  mutate(
    # calculate the duration of frost-free days (in our case here, Frost-free
    # days = (doy of first day of the first month when tmin is >0) - (doy of
    # last day of the lost month when tmin >0))
    # first month when tmin is above freezing is "aboveFreezing_month" in the previous d.f.
    # last month when tmin is above freezing is "lastAboveFreezing_month" in the previous d.f.
    durationFrostFreeDays = 
      # DOY of last day of last frost-free month (just give the 30th, since it
      # probably isn't a bit deal if we use the 30th rather than the 31st in
      # months when there is a 31st)
      lubridate::yday(as.Date(paste0(lastAboveFreezing_month, "/30/2024"), 
                              format = "%m/%d/%Y")) - 
      # DOY of first day of first frost-free month
      lubridate::yday(as.Date(paste0("0",aboveFreezing_month, "/01/2024"), 
                                                    format = "%m/%d/%Y"))
    )

# constants for SVP calculation 
#calculate SVP according to Williams et al NatCC 2012 supplementary material -  units haPa
a0<-6.107799961
a1<-0.4436518521
a2<-0.01428945805
a3<-0.0002650648471
a4<-0.000003031240396
a5<-0.00000002034080948
a6<-0.00000000006136820929
## calculating vapor pressure deficit, annual water deficit, and wet degree days (based on code from Adam Noel)
climVar2 <- allMetDat2%>% 
  # approximation of mean temp (just avg. of max and min, which I realize is not totally accurate)
  mutate(tmean_Jan = (tmax_Jan + tmin_Jan)/2,
         tmean_Feb = (tmax_Feb + tmin_Feb)/2,
         tmean_March = (tmax_March + tmin_March)/2,
         tmean_April = (tmax_April + tmin_April)/2,
         tmean_May = (tmax_May + tmin_May)/2,
         tmean_June = (tmax_June + tmin_June)/2,
         tmean_July = (tmax_July + tmin_July)/2,
         tmean_Aug = (tmax_Aug + tmin_Aug)/2,
         tmean_Sept = (tmax_Sept + tmin_Sept)/2,
         tmean_Oct = (tmax_Oct + tmin_Oct)/2,
         tmean_Nov = (tmax_Nov + tmin_Nov)/2,
         tmean_Dec = (tmax_Dec + tmin_Dec)/2,
  ) %>% 
  mutate(
    # monthly water deficit 
    awd_Jan = tmean_Jan*2 - prcp_Jan,
    awd_Feb = tmean_Feb*2 - prcp_Feb,
    awd_March = tmean_March*2 - prcp_March,
    awd_April = tmean_April*2 - prcp_April,
    awd_May = tmean_May*2 - prcp_May,
    awd_June = tmean_June*2 - prcp_June, 
    awd_July = tmean_July*2 - prcp_July,
    awd_Aug = tmean_Aug*2 - prcp_Aug, 
    awd_Sept = tmean_Sept*2 - prcp_Sept, 
    awd_Oct = tmean_Oct*2 - prcp_Oct,   
    awd_Nov = tmean_Nov*2 - prcp_Nov,   
    awd_Dec = tmean_Dec*2 - prcp_Dec,   
    # monthly wet degree days
    ##aes 
    awdd_Jan =  ifelse(tmean_Jan*2 < prcp_Jan, tmean_Jan*30, NA),
    awdd_Feb =  ifelse(tmean_Feb*2 < prcp_Feb, tmean_Feb*30, NA),
    awdd_March =   ifelse(tmean_March*2 < prcp_March, tmean_March*30, NA),
    awdd_April =  ifelse(tmean_April*2 < prcp_April, tmean_April*30, NA),
    awdd_May = ifelse(tmean_May*2 < prcp_May, tmean_May*30, NA),
    awdd_June = ifelse(tmean_June*2 < prcp_June, tmean_June*30, NA),
    awdd_July = ifelse(tmean_July*2 < prcp_July, tmean_July*30, NA), 
    awdd_Aug = ifelse(tmean_Aug*2 < prcp_Aug, tmean_Aug*30, NA),
    awdd_Sept = ifelse(tmean_Sept*2 < prcp_Sept, tmean_Sept*30, NA),
    awdd_Oct = ifelse(tmean_Oct*2 < prcp_Oct, tmean_Oct*30, NA),
    awdd_Nov = ifelse(tmean_Nov*2 < prcp_Nov, tmean_Nov*30, NA),
    awdd_Dec = ifelse(tmean_Dec*2 < prcp_Dec, tmean_Dec*30, NA),
    # calculate VPD in milibars (100Pa = 1mb, vp is in Pa)
    # #(0.6108 * exp(17.27 * TMAX / (TMAX + 237.3))
    # VPD_Jan = (6.11*exp((17.27*tmean_Jan)/(237.3 + tmean_Jan))) -  (vp_Jan/100),
    # VPD_Feb = (6.11*exp((17.27*tmean_Feb)/(237.3 + tmean_Feb))) -  (vp_Feb/100),
    # VPD_March = (6.11*exp((17.27*tmean_March)/(237.3 + tmean_March))) - (vp_March/100),
    # VPD_April = (6.11*exp((17.27*tmean_April)/(237.3 + tmean_April)))- (vp_April/100),
    # VPD_May =   (6.11*exp((17.27*tmean_May)/(237.3 + tmean_May))) - (vp_May/100),
    # VPD_June =  (6.11*exp((17.27*tmean_June)/(237.3 + tmean_June))) - (vp_June/100) ,
    # VPD_July =  (6.11*exp((17.27*tmean_July)/(237.3 + tmean_July))) - (vp_July/100),
    # VPD_Aug =   (6.11*exp((17.27*tmean_Aug)/(237.3 + tmean_Aug))) -  (vp_Aug/100),
    # VPD_Sept =  (6.11*exp((17.27*tmean_Sept)/(237.3 + tmean_Sept)))- (vp_Sept/100),
    # VPD_Oct =   (6.11*exp((17.27*tmean_Oct)/(237.3 + tmean_Oct))) - (vp_Oct/100),
    # VPD_Nov =   (6.11*exp((17.27*tmean_Nov)/(237.3 + tmean_Nov))) - (vp_Nov/100),
    # VPD_Dec =   (6.11*exp((17.27*tmean_Dec)/(237.3 + tmean_Dec))) - (vp_Dec/100),
    # second try for VPD (based on https://static-content.springer.com/esm/art%3A10.1038%2Fnclimate1693/MediaObjects/41558_2013_BFnclimate1693_MOESM272_ESM.pdf)
    # units are Pascals 
    VPD_Jan = ((( a0+ tmean_Jan*(a1+ tmean_Jan *(a2+ tmean_Jan *(a3+ tmean_Jan *(a4	+ tmean_Jan *(a5	+ tmean_Jan *a6)))))))*100 -  (tmean_Jan))/1000,
    VPD_Feb = ((( a0+ tmean_Feb*(a1+ tmean_Feb *(a2+ tmean_Feb *(a3+ tmean_Feb *(a4	+ tmean_Feb *(a5	+ tmean_Feb *a6)))))))*100 -  (tmean_Feb))/1000,
    VPD_March = ((( a0+ tmean_March*(a1+ tmean_March *(a2+ tmean_March *(a3+ tmean_March *(a4	+ tmean_March *(a5	+ tmean_March *a6)))))))*100 -  (tmean_March))/1000,
    VPD_April = ((( a0+ tmean_April*(a1+ tmean_April *(a2+ tmean_April *(a3+ tmean_April *(a4	+ tmean_April *(a5	+ tmean_April *a6)))))))*100 -  (tmean_April))/1000,
    VPD_May =   ((( a0+ tmean_May*(a1+ tmean_May *(a2+ tmean_May *(a3+ tmean_May *(a4	+ tmean_May *(a5	+ tmean_May *a6)))))))*100 -  (tmean_May))/1000,
    VPD_June =  ((( a0+ tmean_June*(a1+ tmean_June *(a2+ tmean_June *(a3+ tmean_June *(a4	+ tmean_June *(a5	+ tmean_June *a6)))))))*100 -  (tmean_June))/1000,
    VPD_July =  ((( a0+ tmean_July*(a1+ tmean_July *(a2+ tmean_July *(a3+ tmean_July *(a4	+ tmean_July *(a5	+ tmean_July *a6)))))))*100 -  (tmean_July))/1000,
    VPD_Aug =   ((( a0+ tmean_Aug*(a1+ tmean_Aug *(a2+ tmean_Aug *(a3+ tmean_Aug *(a4	+ tmean_Aug *(a5	+ tmean_Aug *a6)))))))*100 -  (tmean_Aug))/1000,
    VPD_Sept =  ((( a0+ tmean_Sept*(a1+ tmean_Sept *(a2+ tmean_Sept *(a3+ tmean_Sept *(a4	+ tmean_Sept *(a5	+ tmean_Sept *a6)))))))*100 -  (tmean_Sept))/1000,
    VPD_Oct =   ((( a0+ tmean_Oct*(a1+ tmean_Oct *(a2+ tmean_Oct *(a3+ tmean_Oct *(a4	+ tmean_Oct *(a5	+ tmean_Oct *a6)))))))*100 -  (tmean_Oct))/1000,
    VPD_Nov =   ((( a0+ tmean_Nov*(a1+ tmean_Nov *(a2+ tmean_Nov *(a3+ tmean_Nov *(a4	+ tmean_Nov *(a5	+ tmean_Nov *a6)))))))*100 -  (tmean_Nov))/1000,
    VPD_Dec =   ((( a0+ tmean_Dec*(a1+ tmean_Dec *(a2+ tmean_Dec *(a3+ tmean_Dec *(a4	+ tmean_Dec *(a5	+ tmean_Dec *a6)))))))*100 -  (tmean_Dec))/1000
    ) %>% 
  #calculate annual values
  transmute(#keep = c("year", "Long", "Lat"),
    # annual water deficit (mm of water over degrees celsius)(sum across all months?)
    tmean = rowMeans(.[c("tmean_Jan", "tmean_Feb", "tmean_March", "tmean_April", "tmean_May", "tmean_June", "tmean_July", "tmean_Aug", "tmean_Sept", "tmean_Oct" ,"tmean_Nov", "tmean_Dec")]
    ),
    # annual water deficit (mm of water over degrees celsius)(sum across all months?)
    annWaterDeficit = pmap_dbl(.[c("awd_Jan", "awd_Feb", "awd_March", "awd_April", "awd_May", "awd_June", "awd_July", "awd_Aug", "awd_Sept", "awd_Oct" ,"awd_Nov", "awd_Dec")], 
                               .f = function(awd_Jan, awd_Feb, awd_March, awd_April, awd_May, awd_June, awd_July, awd_Aug, awd_Sept, awd_Oct ,awd_Nov, awd_Dec, ...){
                                 temp <- c(awd_Jan, awd_Feb, awd_March, awd_April, awd_May, awd_June, awd_July, awd_Aug, awd_Sept, awd_Oct ,awd_Nov, awd_Dec)
                                 sum(temp[temp>0])
                               }
    ),
    # annual wet degree days (temp*days) (sum only positive values)
    annWetDegDays = pmap_dbl(.[c("awdd_Jan", "awdd_Feb", "awdd_March", "awdd_April", "awdd_May", "awdd_June", "awdd_July", "awdd_Aug", "awdd_Sept", "awdd_Oct" ,"awdd_Nov", "awdd_Dec")],
                             .f = function(awdd_Jan, awdd_Feb, awdd_March, awdd_April, awdd_May, awdd_June, awdd_July, awdd_Aug, awdd_Sept, awdd_Oct ,awdd_Nov, awdd_Dec, ...) 
                             {
                               temp <- c(awdd_Jan, awdd_Feb, awdd_March, awdd_April, awdd_May, awdd_June, awdd_July, awdd_Aug, awdd_Sept, awdd_Oct ,awdd_Nov, awdd_Dec)
                               sum(temp[temp>0], na.rm = TRUE)
                             }
    ),
    # annual average vapor pressure deficit (in milibars) ()
    annVPD_mean = rowMeans(.[c("VPD_Jan", "VPD_Feb", "VPD_March","VPD_April" ,"VPD_May","VPD_June", "VPD_July","VPD_Aug","VPD_Sept","VPD_Oct","VPD_Nov","VPD_Dec")]),
    # annual maximum vapor pressure deficit (in milibars) 
    annVPD_max = pmap_dbl(.[c("VPD_Jan", "VPD_Feb", "VPD_March","VPD_April" ,"VPD_May","VPD_June", "VPD_July","VPD_Aug","VPD_Sept","VPD_Oct","VPD_Nov","VPD_Dec")], max),
    # annual minimum vapor pressure deficit (in milibars) 
    annVPD_min = pmap_dbl(.[c("VPD_Jan", "VPD_Feb", "VPD_March","VPD_April" ,"VPD_May","VPD_June", "VPD_July","VPD_Aug","VPD_Sept","VPD_Oct","VPD_Nov","VPD_Dec")], min)
  )

# 
#   
#   plot(climVar2[,c("VPD_2_Jan")] ~ 
#          climVar2[,c("tmax_Jan")], col =  "purple",
#        xlim = c(-15,40), 
#        ylim = c(0, 2.5)
#   )
#   points(climVar2[,c("VPD_2_Feb")]  ~ 
#                   climVar2[,c("tmax_Feb")], col = "blue"
#   )
#   points(climVar2[,c("VPD_2_March")]  ~ 
#             climVar2[,c("tmax_March")], col = "turquoise"
#   )
#   points(climVar2[,c("VPD_2_April")]  ~ 
#            climVar2[,c("tmax_April")], col = "green"
#   )
#   points(climVar2[,c("VPD_2_May")]  ~ 
#            climVar2[,c("tmax_May")], col = "yellow"
#   )
#   points(climVar2[,c("VPD_2_June")]  ~ 
#            climVar2[,c("tmax_June")], col = "orange"
#   )
#   points(climVar2[,c("VPD_2_July")]  ~ 
#            climVar2[,c("tmax_July")], col = "red"
#   )
#   points(climVar2[,c("VPD_2_Aug")]  ~ 
#            climVar2[,c("tmax_Aug")], col = "brown"
#   )
#   points(climVar2[,c("VPD_2_Sept")]  ~ 
#            climVar2[,c("tmax_Sept")], col = "grey"
#   )
#   points(climVar2[,c("VPD_2_Oct")]  ~ 
#            climVar2[,c("tmax_Oct")], col = "wheat"
#   )
#   points(climVar2[,c("VPD_2_Nov")]  ~ 
#            climVar2[,c("tmax_Nov")], col = "pink"
#   )
#   points(climVar2[,c("VPD_2_Dec")]  ~ 
#            climVar2[,c("tmax_Dec")], col = "magenta"
#   )
#   
       
climVar <- cbind(climVar, climVar2)
rm(climVar2)


# save for subsequent use
#saveRDS(climVar, file = "./Data_raw/dayMet/climateValuesForAnalysis_monthly.rds")
climVar <- readRDS(file="./Data_raw/dayMet/climateValuesForAnalysis_monthly.rds")

rm(allMetDat, allMetDat2, annMetDat)
# calculate sliding window inter-annual climate means ----------------------

## calculate MAP and MAT over past years (a sliding window?)
# function
slidingMetMeans <- function(inDat, start, end) {
  endActual <- end-1 # subtract one so that we're actually looking at the 30, 10, 5, etc. years previous to the "end" year
  outDat <- inDat %>% 
    filter(year %in% c(start:endActual)) %>% 
    group_by(Long, Lat) %>% 
    summarize(sweMax_meanAnnAvg = mean(swe_annAvg),
              tmin_meanAnnAvg = mean(tmin_annAvg),
              tmax_meanAnnAvg = mean(tmax_annAvg),
              tmean_meanAnnAvg = mean(tmean),
              vp_meanAnnAvg = mean(vp_annAvg),
              prcp_meanAnnTotal = mean(prcp_annTotal),
              T_warmestMonth_meanAnnAvg = mean(T_warmestMonth), # temperature of warmest month
              T_coldestMonth_meanAnnAvg = mean(T_coldestMonth), # temperature of coldest month
              precip_wettestMonth_meanAnnAvg = mean(precip_wettestMonth), # precip of wettest month
              precip_driestMonth_meanAnnAvg = mean(precip_driestMonth), # precip of driest month
              precip_Seasonality_meanAnnAvg = mean(precip_Seasonality),
              PrecipTempCorr_meanAnnAvg = mean(PrecipTempCorr),
              aboveFreezing_month_meanAnnAvg = mean(aboveFreezing_month),
              isothermality_meanAnnAvg = mean(isothermality),
              annWaterDeficit_meanAnnAvg = mean(annWaterDeficit),
              annWetDegDays_meanAnnAvg = mean(annWetDegDays),
              annVPD_mean_meanAnnAvg = mean(annVPD_mean),
              annVPD_max_meanAnnAvg = mean(annVPD_max),
              annVPD_min_meanAnnAvg = mean(annVPD_min),
              annVPD_max_95percentile = unname(quantile(annVPD_max, probs = 0.95, na.rm = TRUE)),
              annWaterDeficit_95percentile = unname(quantile(annWaterDeficit, probs = 0.95, na.rm = TRUE)),
              annWetDegDays_5percentile = unname(quantile(annWetDegDays, probs = 0.05, na.rm = TRUE)),
              durationFrostFreeDays_5percentile = unname(quantile(durationFrostFreeDays, probs = 0.05, na.rm = TRUE)),
              durationFrostFreeDays_meanAnnAvg = unname(mean(durationFrostFreeDays))
    )
  return(outDat)
}

#testDat <- climVar[runif(10000,min=0, max = nrow(climVar)),]
# for last 30-year window
endDats <- as.matrix(c(2010:2023))
annMeans <- apply(as.matrix(endDats[1:2]), MARGIN = 1, FUN = function(x)
  slidingMetMeans(inDat = climVar
                    , start = as.numeric(x-30), end = as.numeric(x))
)
annMeans_2 <- apply(as.matrix(endDats[3:5]), MARGIN = 1, FUN = function(x)
  slidingMetMeans(inDat = climVar
                  , start = as.numeric(x-30), end = as.numeric(x))
)
annMeans_3 <- apply(as.matrix(endDats[6:8]), MARGIN = 1, FUN = function(x)
  slidingMetMeans(inDat = climVar
                  , start = as.numeric(x-30), end = as.numeric(x))
)
annMeans_4 <- apply(as.matrix(endDats[9:11]), MARGIN = 1, FUN = function(x)
  slidingMetMeans(inDat = climVar
                  , start = as.numeric(x-30), end = as.numeric(x))
)
annMeans_5 <- apply(as.matrix(endDats[12:14]), MARGIN = 1, FUN = function(x)
  slidingMetMeans(inDat = climVar
                  , start = as.numeric(x-30), end = as.numeric(x))
)

# put together into one list
annMeans_all <- c(annMeans, annMeans_2, annMeans_3, annMeans_4, annMeans_5)
names(annMeans_all) <- c(2010:2023)
annMeans_30yr <- lapply(endDats, function(x) {
  temp <- cbind(annMeans_all[[as.character(x)]], x)
  temp$Start <- x-30
  names(temp) <- c(names(annMeans_all[[1]]), "End", "Start")
  return(temp)
})

annMeans_30yr <- data.table::rbindlist(annMeans_30yr)

names(annMeans_30yr)[3:28] <- paste0(names(annMeans_30yr)[3:28], "_30yr")
saveRDS(annMeans_30yr, "./Data_processed/CoverData/dayMet_intermediate/annMeans_30yrs.rds")
annMeans_30yr <- readRDS("./Data_processed/CoverData/dayMet_intermediate/annMeans_30yrs.rds")

## for windows that are specific to the year (for 2011 onward, use 30 year
## window; for years prior, use the longest window possible (starting in 1980))
endDats <- as.matrix(c(1980:2009))
annMeans <- apply(as.matrix(endDats[1:2]), MARGIN = 1, FUN = function(x)
  slidingMetMeans(inDat = climVar
                  , start = 1980, end = as.numeric(x))
)
annMeans_2 <- apply(as.matrix(endDats[3:5]), MARGIN = 1, FUN = function(x)
  slidingMetMeans(inDat = climVar
                  , start = 1980, end = as.numeric(x))
)
annMeans_3 <- apply(as.matrix(endDats[6:8]), MARGIN = 1, FUN = function(x)
  slidingMetMeans(inDat = climVar
                  , start = 1980, end = as.numeric(x))
)
annMeans_4 <- apply(as.matrix(endDats[9:11]), MARGIN = 1, FUN = function(x)
  slidingMetMeans(inDat = climVar
                  , start = 1980, end = as.numeric(x))
)
annMeans_5 <- apply(as.matrix(endDats[12:14]), MARGIN = 1, FUN = function(x)
  slidingMetMeans(inDat = climVar
                  , start = 1980 , end = as.numeric(x))
)
annMeans_6 <- apply(as.matrix(endDats[15:17]), MARGIN = 1, FUN = function(x)
  slidingMetMeans(inDat = climVar
                  , start = 1980 , end = as.numeric(x))
)
annMeans_7 <- apply(as.matrix(endDats[18:20]), MARGIN = 1, FUN = function(x)
  slidingMetMeans(inDat = climVar
                  , start = 1980 , end = as.numeric(x))
)
annMeans_8 <- apply(as.matrix(endDats[21:23]), MARGIN = 1, FUN = function(x)
  slidingMetMeans(inDat = climVar
                  , start = 1980 , end = as.numeric(x))
)
annMeans_9 <- apply(as.matrix(endDats[24:26]), MARGIN = 1, FUN = function(x)
  slidingMetMeans(inDat = climVar
                  , start = 1980 , end = as.numeric(x))
)
annMeans_10 <- apply(as.matrix(endDats[27:28]), MARGIN = 1, FUN = function(x)
  slidingMetMeans(inDat = climVar
                  , start = 1980 , end = as.numeric(x))
)
annMeans_11 <- apply(as.matrix(endDats[29:30]), MARGIN = 1, FUN = function(x)
  slidingMetMeans(inDat = climVar
                  , start = 1980 , end = as.numeric(x))
)
# put together into one list
annMeans_all <- c(annMeans, annMeans_2, annMeans_3, annMeans_4, annMeans_5,
                  annMeans_6, annMeans_7, annMeans_8, annMeans_9, annMeans_10,
                  annMeans_11)
names(annMeans_all) <- c(1980:2009)
annMeans_ShorterLagsForClimate<- lapply(endDats, function(x) {
  temp <- cbind(annMeans_all[[as.character(x)]], x)
  temp$Start <- 1980
  names(temp) <- c(names(annMeans_all[[1]]), "End", "Start")
  return(temp)
})

annMeans_ShorterLagsForClimate <- data.table::rbindlist(annMeans_ShorterLagsForClimate)

names(annMeans_ShorterLagsForClimate)[3:28] <- paste0(names(annMeans_ShorterLagsForClimate)[3:28], "_trimClim")
saveRDS(annMeans_ShorterLagsForClimate, "./Data_processed/CoverData/dayMet_intermediate/annMeans_forShorterLagsPriorTo2009.rds")

## for last 10-year window
endDats <- as.matrix(c(1990:2023))
annMeans <- apply(endDats, MARGIN = 1, FUN = function(x)
  slidingMetMeans(inDat = climVar, start = as.numeric(x-10), end = as.numeric(x))
)
names(annMeans) <- c(1990:2023)
annMeans_10yr <- lapply(endDats, function(x) {
  temp <- cbind(annMeans[[as.character(x)]], x)
  temp$Start <- x-10
  names(temp) <- c(names(annMeans[[1]]), "End", "Start")
  return(temp)
})

annMeans_10yr <- data.table::rbindlist(annMeans_10yr)
names(annMeans_10yr)[3:28] <- paste0(names(annMeans_10yr)[3:28], "_10yr")
saveRDS(annMeans_10yr, "./Data_processed/CoverData/dayMet_intermediate/annMeans_10yrs.rds")
annMeans_10yr <- readRDS("./Data_processed/CoverData/dayMet_intermediate/annMeans_10yrs.rds")

# for last 5-year window
endDats <- as.matrix(c(1985:2023))
annMeans <- apply(endDats, MARGIN = 1, FUN = function(x)
  slidingMetMeans(inDat = climVar, start = as.numeric(x-5), end = as.numeric(x))
)
names(annMeans) <- c(1985:2023)
annMeans_5yr <- lapply(endDats, function(x) {
  temp <- cbind(annMeans[[as.character(x)]], x)
  temp$Start <- x-5
  names(temp) <- c(names(annMeans[[1]]), "End", "Start")
  return(temp)
})

annMeans_5yr <- data.table::rbindlist(annMeans_5yr)
names(annMeans_5yr)[3:28] <- paste0(names(annMeans_5yr)[3:28], "_5yr")
saveRDS(annMeans_5yr, "./Data_processed/CoverData/dayMet_intermediate/annMeans_5yrs.rds")
annMeans_5yr <- readRDS("./Data_processed/CoverData/dayMet_intermediate/annMeans_5yrs.rds")

# for last 1-year window
endDats <- as.matrix(c(1981:2023))
annMeans <- apply(endDats, MARGIN = 1, FUN = function(x)
  slidingMetMeans(inDat = climVar, start = as.numeric(x-1), end = as.numeric(x))
)
names(annMeans) <- c(1981:2023)
annMeans_1yr <- lapply(endDats, function(x) {
  temp <- cbind(annMeans[[as.character(x)]], x)
  temp$Start <- x-1
  names(temp) <- c(names(annMeans[[1]]), "End", "Start")
  return(temp)
})

annMeans_1yr <- data.table::rbindlist(annMeans_1yr)
names(annMeans_1yr)[3:28] <- paste0(names(annMeans_1yr)[3:28], "_1yr")
saveRDS(annMeans_1yr, "./Data_processed/CoverData/dayMet_intermediate/annMeans_1yrs.rds")
annMeans_1yr <- readRDS("./Data_processed/CoverData/dayMet_intermediate/annMeans_1yrs.rds")

## add lagged data to the main climate value data.frame
test <- climVar %>% 
  select(-c(tmax_Jan:tmax_Dec, tmin_Jan:prcp_Dec)) %>% 
  #filter(year == 2020) %>% 
  #slice(1:100) %>% 
  left_join(annMeans_30yr, by = c("year" = "End_30yr", 
                                  "Long" = "Long", 
                                  "Lat" = "Lat")) %>% 
  left_join(annMeans_10yr, by = c("year" = "End_10yr", 
                                  "Long" = "Long", 
                                  "Lat" = "Lat")
  )

rm(annMeans_30yr, annMeans_10yr)
gc()

test2 <- test %>% 
  left_join(annMeans_5yr, by = c("year" = "End_5yr", 
                                 "Long" = "Long", 
                                 "Lat" = "Lat"))
 
rm(test, annMeans_30yr, annMeans_10yr, annMeans_5yr, annMeans, annMeans_2, 
   annMeans_3, annMeans_4, annMeans_5, annMeans_6, annMeans_7, annMeans_8,
   annMeans_9, annMeans_10, annMeans_11, annMeans_all, climVar)
gc()

test3 <- test2 %>%
  left_join(annMeans_1yr, by = c("year" = "End_1yr",
                                 "Long" = "Long",
                                 "Lat" = "Lat"))

rm(annMeans_1yr, test2)
annMeans_ShorterLagsForClimate <- readRDS("./Data_processed/CoverData/dayMet_intermediate/annMeans_forShorterLagsPriorTo2009.rds")

test4 <- test3 %>% 
  left_join(annMeans_ShorterLagsForClimate, by = c("year" = "End_trimClim",
                                                   "Long" = "Long",
                                                   "Lat" = "Lat"))

# save intermediate data 
saveRDS(test4, "./Data_processed/CoverData/dayMet_intermediate/climVars_AnnualMeansAndLaggedValues.rds")
test4 <- readRDS("./Data_processed/CoverData/dayMet_intermediate/climVars_AnnualMeansAndLaggedValues.rds")
# for those years including and after 2010, use the averages over the previous
# 30 years for climate. for those years before 2010, use the averages over any
# previous years starting w/ 1980

rm(test3, annMeans_ShorterLagsForClimate)
gc()

datPost2009 <- test4 %>% 
  filter(year > 2009) %>% 
  mutate("swe_meanAnnAvg_CLIM" = swe_meanAnnAvg_30yr ,   
         "tmin_meanAnnAvg_CLIM" = tmin_meanAnnAvg_30yr                  ,  
         "tmax_meanAnnAvg_CLIM" = tmax_meanAnnAvg_30yr                  ,     
         "tmean_meanAnnAvg_CLIM" = tmean_meanAnnAvg_30yr                 ,    
         "vp_meanAnnAvg_CLIM" = vp_meanAnnAvg_30yr                    ,  
         "prcp_meanAnnTotal_CLIM" = prcp_meanAnnTotal_30yr                ,     
         "T_warmestMonth_meanAnnAvg_CLIM" = T_warmestMonth_meanAnnAvg_30yr        ,    
         "T_coldestMonth_meanAnnAvg_CLIM" = T_coldestMonth_meanAnnAvg_30yr    , 
         "precip_wettestMonth_meanAnnAvg_CLIM" = precip_wettestMonth_meanAnnAvg_30yr   ,     
         "precip_driestMonth_meanAnnAvg_CLIM" = precip_driestMonth_meanAnnAvg_30yr    ,    
         "precip_Seasonality_meanAnnAvg_CLIM" = precip_Seasonality_meanAnnAvg_30yr  ,  
         "PrecipTempCorr_meanAnnAvg_CLIM" = PrecipTempCorr_meanAnnAvg_30yr        ,     
         "aboveFreezing_month_meanAnnAvg_CLIM" = aboveFreezing_month_meanAnnAvg_30yr   ,    
               "isothermality_meanAnnAvg_CLIM" = isothermality_meanAnnAvg_30yr     , 
             "annWaterDeficit_meanAnnAvg_CLIM" = annWaterDeficit_meanAnnAvg_30yr     ,     
               "annWetDegDays_meanAnnAvg_CLIM" = annWetDegDays_meanAnnAvg_30yr        ,    
                 "annVPD_mean_meanAnnAvg_CLIM" = annVPD_mean_meanAnnAvg_30yr    , 
                  "annVPD_max_meanAnnAvg_CLIM" = annVPD_max_meanAnnAvg_30yr            ,     
                  "annVPD_min_meanAnnAvg_CLIM" = annVPD_min_meanAnnAvg_30yr            ,    
                "annVPD_max_95percentile_CLIM" = annVPD_max_95percentile_30yr, 
           "annWaterDeficit_95percentile_CLIM" = annWaterDeficit_95percentile_30yr     ,     
              "annWetDegDays_5percentile_CLIM" = annWetDegDays_5percentile_30yr        ,    
      "durationFrostFreeDays_5percentile_CLIM" = durationFrostFreeDays_5percentile_30yr,  
       "durationFrostFreeDays_meanAnnAvg_CLIM" = durationFrostFreeDays_meanAnnAvg_30yr ,     
                                  "Start_CLIM" = Start_30yr)
saveRDS(datPost2009, "./Data_processed/CoverData/dayMet_intermediate/climVars_AnnualMeansAndLaggedValues_post2009.rds")
rm(datPost2009)
gc()

datPre2009 <- test4 %>% 
  filter(year <=  2009) %>% 
  mutate("swe_meanAnnAvg_CLIM" = sweMax_meanAnnAvg_trimClim ,   
         "tmin_meanAnnAvg_CLIM" = tmin_meanAnnAvg_trimClim                  ,  
         "tmax_meanAnnAvg_CLIM" = tmax_meanAnnAvg_trimClim                  ,     
         "tmean_meanAnnAvg_CLIM" = tmean_meanAnnAvg_trimClim                 ,    
         "vp_meanAnnAvg_CLIM" = vp_meanAnnAvg_trimClim                    ,  
         "prcp_meanAnnTotal_CLIM" = prcp_meanAnnTotal_trimClim                ,     
         "T_warmestMonth_meanAnnAvg_CLIM" = T_warmestMonth_meanAnnAvg_trimClim        ,    
         "T_coldestMonth_meanAnnAvg_CLIM" = T_coldestMonth_meanAnnAvg_trimClim    , 
         "precip_wettestMonth_meanAnnAvg_CLIM" = precip_wettestMonth_meanAnnAvg_trimClim   ,     
         "precip_driestMonth_meanAnnAvg_CLIM" = precip_driestMonth_meanAnnAvg_trimClim    ,    
         "precip_Seasonality_meanAnnAvg_CLIM" = precip_Seasonality_meanAnnAvg_trimClim  ,  
         "PrecipTempCorr_meanAnnAvg_CLIM" = PrecipTempCorr_meanAnnAvg_trimClim        ,     
         "aboveFreezing_month_meanAnnAvg_CLIM" = aboveFreezing_month_meanAnnAvg_trimClim   ,    
         "isothermality_meanAnnAvg_CLIM" = isothermality_meanAnnAvg_trimClim     , 
         "annWaterDeficit_meanAnnAvg_CLIM" = annWaterDeficit_meanAnnAvg_trimClim       ,     
         "annWetDegDays_meanAnnAvg_CLIM" =   annWetDegDays_meanAnnAvg_trimClim         ,    
         "annVPD_mean_meanAnnAvg_CLIM" =     annVPD_mean_meanAnnAvg_trimClim    , 
         "annVPD_max_meanAnnAvg_CLIM" = annVPD_max_meanAnnAvg_trimClim            ,     
         "annVPD_min_meanAnnAvg_CLIM" = annVPD_min_meanAnnAvg_trimClim          ,    
         "annVPD_max_95percentile_CLIM" = annVPD_max_95percentile_trimClim, 
         "annWaterDeficit_95percentile_CLIM" = annWaterDeficit_95percentile_trimClim     ,     
         "annWetDegDays_5percentile_CLIM" = annWetDegDays_5percentile_trimClim        ,    
         "durationFrostFreeDays_5percentile_CLIM" = durationFrostFreeDays_5percentile_trimClim,  
         "durationFrostFreeDays_meanAnnAvg_CLIM" = durationFrostFreeDays_meanAnnAvg_trimClim ,     
         "Start_CLIM" = Start_trimClim)

rm(test4)
gc()
saveRDS(datPre2009, "./Data_processed/CoverData/dayMet_intermediate/climVars_AnnualMeansAndLaggedValues_pre2009.rds")

# read back in post 2009 data 
datPost2009 <- readRDS( "./Data_processed/CoverData/dayMet_intermediate/climVars_AnnualMeansAndLaggedValues_post2009.rds")
# join pre and post 2009 data back together and save for later 
test5 <- datPre2009 %>% 
  select(-c(swe_meanAnnAvg_30yr:Start_30yr), -c(sweMax_meanAnnAvg_trimClim:Start_trimClim)) %>% 
  rbind(datPost2009 %>% select(-c(swe_meanAnnAvg_30yr:Start_30yr), -c(sweMax_meanAnnAvg_trimClim:Start_trimClim)))
 
rm(datPost2009, datPre2009)
gc()
# save data 
saveRDS(test5, "./Data_processed/CoverData/dayMet_intermediate/climVars_AnnualMeansAndLaggedValues.rds")
test5 <- readRDS("./Data_processed/CoverData/dayMet_intermediate/climVars_AnnualMeansAndLaggedValues.rds")
#### calculate anomalies ####
# i.e. how do the 10 yr. lagged values compare to the 30yr lagged values? 5 yr? previous yr? 
# compare 10 yr values to 30 yr values

rm(test2, annMeans_1yr)

anomDat_10yr <- test5 %>% 
  transmute(
    # compare 10 yr values to 30 yr values
    # swe as % difference
    sweMax_meanAnn_10yrAnom = ((swe_meanAnnAvg_CLIM - swe_meanAnnAvg_10yr)/swe_meanAnnAvg_CLIM),
    # tmean as absolute difference
    tmean_meanAnnAvg_10yrAnom = tmean_meanAnnAvg_CLIM - tmean_meanAnnAvg_10yr,
    # tmin as absolute difference
    tmin_meanAnnAvg_10yrAnom = tmin_meanAnnAvg_CLIM - tmin_meanAnnAvg_10yr,
    # tmax as absolute difference
    tmax_meanAnnAvg_10yrAnom = tmax_meanAnnAvg_CLIM - tmax_meanAnnAvg_10yr,
    # vp as % difference
    vp_meanAnnAvg_10yrAnom = (vp_meanAnnAvg_CLIM - vp_meanAnnAvg_10yr)/vp_meanAnnAvg_CLIM,
    # prcp as % difference
    prcp_meanAnnTotal_10yrAnom = (prcp_meanAnnTotal_CLIM - prcp_meanAnnTotal_10yr)/prcp_meanAnnTotal_CLIM,
    # t warmest month as absolute difference
    T_warmestMonth_meanAnnAvg_10yrAnom = T_warmestMonth_meanAnnAvg_CLIM - T_warmestMonth_meanAnnAvg_10yr,
    # t coldest month as absolute difference
    T_coldestMonth_meanAnnAvg_10yrAnom = T_coldestMonth_meanAnnAvg_CLIM - T_coldestMonth_meanAnnAvg_10yr,
    # precip wettest month as % difference
    precip_wettestMonth_meanAnnAvg_10yrAnom = (precip_wettestMonth_meanAnnAvg_CLIM - precip_wettestMonth_meanAnnAvg_10yr)/precip_wettestMonth_meanAnnAvg_CLIM,
    # precip driest month as % difference
    precip_driestMonth_meanAnnAvg_10yrAnom = (precip_driestMonth_meanAnnAvg_CLIM - precip_driestMonth_meanAnnAvg_10yr)/precip_driestMonth_meanAnnAvg_CLIM,
    # precip seasonality as % difference
    precip_Seasonality_meanAnnAvg_10yrAnom = (precip_Seasonality_meanAnnAvg_CLIM - precip_Seasonality_meanAnnAvg_10yr)/precip_Seasonality_meanAnnAvg_CLIM,
    # precip tempCorr as absolute difference
    PrecipTempCorr_meanAnnAvg_10yrAnom = PrecipTempCorr_meanAnnAvg_CLIM - PrecipTempCorr_meanAnnAvg_10yr,
    # above Freezing month as absolute difference
    aboveFreezing_month_meanAnnAvg_10yrAnom = aboveFreezing_month_meanAnnAvg_CLIM - aboveFreezing_month_meanAnnAvg_10yr,
    # isothermailty as % difference
    isothermality_meanAnnAvg_10yrAnom = isothermality_meanAnnAvg_CLIM - isothermality_meanAnnAvg_10yr,
    # annual water deficit as % difference
    annWaterDeficit_meanAnnAvg_10yrAnom = (annWaterDeficit_meanAnnAvg_CLIM - annWaterDeficit_meanAnnAvg_10yr)/annWaterDeficit_meanAnnAvg_CLIM,
    # wet degree days as % difference
    annWetDegDays_meanAnnAvg_10yrAnom = (annWetDegDays_meanAnnAvg_CLIM - annWetDegDays_meanAnnAvg_10yr)/annWetDegDays_meanAnnAvg_CLIM,
    # mean VPD as absolute difference
    annVPD_mean_meanAnnAvg_10yrAnom = (annVPD_mean_meanAnnAvg_CLIM - annVPD_mean_meanAnnAvg_10yr),
    # min VPD as absolute difference
    annVPD_min_meanAnnAvg_10yrAnom = (annVPD_min_meanAnnAvg_CLIM - annVPD_min_meanAnnAvg_10yr),
    # max VPD as absolute difference
    annVPD_max_meanAnnAvg_10yrAnom = (annVPD_max_meanAnnAvg_CLIM - annVPD_max_meanAnnAvg_10yr),
    # 95th percentile of max VPD as absolute difference 
    annVPD_max_95percentile_10yrAnom = (annVPD_max_95percentile_CLIM - annVPD_max_95percentile_10yr),
    # 95th percentile of annual water deficit as % difference
    annWaterDeficit_95percentile_10yrAnom = (annWaterDeficit_95percentile_CLIM - annWaterDeficit_95percentile_10yr)/annWaterDeficit_95percentile_CLIM,
    # 5th percentile of annual wet degree days as % difference 
    annWetDegDays_5percentile_10yrAnom = (annWetDegDays_5percentile_CLIM - annWetDegDays_5percentile_10yr)/annWetDegDays_5percentile_CLIM,
    # 10th percentile of frost-free days as absolute difference 
    durationFrostFreeDays_5percentile_10yrAnom = (durationFrostFreeDays_5percentile_CLIM - durationFrostFreeDays_5percentile_10yr),
    # mean of frost free days as absolute difference
    durationFrostFreeDays_meanAnnAvg_10yrAnom = (durationFrostFreeDays_meanAnnAvg_CLIM - durationFrostFreeDays_meanAnnAvg_10yr)
  )

anomDat_5yr <- test5 %>% 
  transmute(
    # compare 5 yr values to 30 yr values
    # swe as % difference
    swe_meanAnn_5yrAnom = swe_meanAnnAvg_CLIM - swe_meanAnnAvg_5yr/swe_meanAnnAvg_CLIM,
    # tmean as absolute difference
    tmean_meanAnnAvg_5yrAnom = tmean_meanAnnAvg_CLIM - tmean_meanAnnAvg_5yr,
    # tmin as absolute difference
    tmin_meanAnnAvg_5yrAnom = tmin_meanAnnAvg_CLIM - tmin_meanAnnAvg_5yr,
    # tmax as absolute difference
    tmax_meanAnnAvg_5yrAnom = tmax_meanAnnAvg_CLIM - tmax_meanAnnAvg_5yr,
    # vp as % difference
    vp_meanAnnAvg_5yrAnom = (vp_meanAnnAvg_CLIM - vp_meanAnnAvg_5yr)/vp_meanAnnAvg_CLIM,
    # prcp as % difference
    prcp_meanAnnTotal_5yrAnom = (prcp_meanAnnTotal_CLIM - prcp_meanAnnTotal_5yr)/prcp_meanAnnTotal_CLIM,
    # t warmest month as absolute difference
    T_warmestMonth_meanAnnAvg_5yrAnom = T_warmestMonth_meanAnnAvg_CLIM - T_warmestMonth_meanAnnAvg_5yr,
    # t coldest month as absolute difference
    T_coldestMonth_meanAnnAvg_5yrAnom = T_coldestMonth_meanAnnAvg_CLIM - T_coldestMonth_meanAnnAvg_5yr,
    # precip wettest month as % difference
    precip_wettestMonth_meanAnnAvg_5yrAnom = (precip_wettestMonth_meanAnnAvg_CLIM - precip_wettestMonth_meanAnnAvg_5yr)/precip_wettestMonth_meanAnnAvg_CLIM,
    # precip driest month as % difference
    precip_driestMonth_meanAnnAvg_5yrAnom = (precip_driestMonth_meanAnnAvg_CLIM - precip_driestMonth_meanAnnAvg_5yr)/precip_driestMonth_meanAnnAvg_CLIM,
    # precip seasonality as % difference
    precip_Seasonality_meanAnnAvg_5yrAnom = (precip_Seasonality_meanAnnAvg_CLIM - precip_Seasonality_meanAnnAvg_5yr)/precip_Seasonality_meanAnnAvg_CLIM,
    # precip tempCorr as absolute difference
    PrecipTempCorr_meanAnnAvg_5yrAnom = PrecipTempCorr_meanAnnAvg_CLIM - PrecipTempCorr_meanAnnAvg_5yr,
    # above Freezing month as absolute difference
    aboveFreezing_month_meanAnnAvg_5yrAnom = aboveFreezing_month_meanAnnAvg_CLIM - aboveFreezing_month_meanAnnAvg_5yr,
    # isothermailty as % difference
    isothermality_meanAnnAvg_5yrAnom = isothermality_meanAnnAvg_CLIM - isothermality_meanAnnAvg_5yr,    
    # annual water deficit as % difference
    annWaterDeficit_meanAnnAvg_5yrAnom = (annWaterDeficit_meanAnnAvg_CLIM - annWaterDeficit_meanAnnAvg_5yr)/annWaterDeficit_meanAnnAvg_CLIM,
    # wet degree days as % difference
    annWetDegDays_meanAnnAvg_5yrAnom = (annWetDegDays_meanAnnAvg_CLIM - annWetDegDays_meanAnnAvg_5yr)/annWetDegDays_meanAnnAvg_CLIM,
    # mean VPD as absolute difference
    annVPD_mean_meanAnnAvg_5yrAnom = (annVPD_mean_meanAnnAvg_CLIM - annVPD_mean_meanAnnAvg_5yr),
    # min VPD as absolute difference
    annVPD_min_meanAnnAvg_5yrAnom = (annVPD_min_meanAnnAvg_CLIM - annVPD_min_meanAnnAvg_5yr),
    # max VPD as absolute difference
    annVPD_max_meanAnnAvg_5yrAnom = (annVPD_max_meanAnnAvg_CLIM - annVPD_max_meanAnnAvg_5yr),
    # 95th percentile of max VPD as absolute difference 
    annVPD_max_95percentile_5yrAnom = (annVPD_max_95percentile_CLIM - annVPD_max_95percentile_5yr),
    # 95th percentile of annual water deficit as % difference
    annWaterDeficit_95percentile_5yrAnom = (annWaterDeficit_95percentile_CLIM - annWaterDeficit_95percentile_5yr)/annWaterDeficit_95percentile_CLIM,
    # 5th percentile of annual wet degree days as % difference 
    annWetDegDays_5percentile_5yrAnom = (annWetDegDays_5percentile_CLIM - annWetDegDays_5percentile_5yr)/annWetDegDays_5percentile_CLIM,
    # 5th percentile of frost-free days as absolute difference 
    durationFrostFreeDays_5percentile_5yrAnom = (durationFrostFreeDays_5percentile_CLIM - durationFrostFreeDays_5percentile_5yr),
    # mean of frost free days as absolute difference
    durationFrostFreeDays_meanAnnAvg_5yrAnom = (durationFrostFreeDays_meanAnnAvg_CLIM - durationFrostFreeDays_meanAnnAvg_5yr)
  )


anomDat_1yr_temp <- test5 %>% 
  # remove values for 10 and 5 yr. lags (that aren't anomalies)
  select(-c(prcp_annTotal:annVPD_min, swe_meanAnnAvg_10yr:Start_10yr, swe_meanAnnAvg_5yr:Start_5yr))

anomDat_1yr <- anomDat_1yr_temp %>% 
  transmute(
    # compare 1 yr values to 30 yr values
    # sweMax as % difference
    swe_meanAnn_1yrAnom = ((swe_meanAnnAvg_CLIM - swe_meanAnnAvg_1yr)/swe_meanAnnAvg_CLIM),
    # tmean as absolute difference
    tmean_meanAnnAvg_1yrAnom = tmean_meanAnnAvg_CLIM - tmean_meanAnnAvg_1yr,
    # tmin as absolute difference
    tmin_meanAnnAvg_1yrAnom = tmin_meanAnnAvg_CLIM - tmin_meanAnnAvg_1yr,
    # tmax as absolute difference
    tmax_meanAnnAvg_1yrAnom = tmax_meanAnnAvg_CLIM - tmax_meanAnnAvg_1yr,
    # vp as % difference
    vp_meanAnnAvg_1yrAnom = (vp_meanAnnAvg_CLIM - vp_meanAnnAvg_1yr)/vp_meanAnnAvg_CLIM,
    # prcp as % difference
    prcp_meanAnnTotal_1yrAnom = (prcp_meanAnnTotal_CLIM - prcp_meanAnnTotal_1yr)/prcp_meanAnnTotal_CLIM,
    # t warmest month as absolute difference
    T_warmestMonth_meanAnnAvg_1yrAnom = T_warmestMonth_meanAnnAvg_CLIM - T_warmestMonth_meanAnnAvg_1yr,
    # t coldest month as absolute difference
    T_coldestMonth_meanAnnAvg_1yrAnom = T_coldestMonth_meanAnnAvg_CLIM - T_coldestMonth_meanAnnAvg_1yr,
    # precip wettest month as % difference
    precip_wettestMonth_meanAnnAvg_1yrAnom = (precip_wettestMonth_meanAnnAvg_CLIM - precip_wettestMonth_meanAnnAvg_1yr)/precip_wettestMonth_meanAnnAvg_CLIM,
    # precip driest month as % difference
    precip_driestMonth_meanAnnAvg_1yrAnom = (precip_driestMonth_meanAnnAvg_CLIM - precip_driestMonth_meanAnnAvg_1yr)/precip_driestMonth_meanAnnAvg_CLIM,
    # precip seasonality as % difference
    precip_Seasonality_meanAnnAvg_1yrAnom = (precip_Seasonality_meanAnnAvg_CLIM - precip_Seasonality_meanAnnAvg_1yr)/precip_Seasonality_meanAnnAvg_CLIM,
    # precip tempCorr as absolute difference
    PrecipTempCorr_meanAnnAvg_1yrAnom = PrecipTempCorr_meanAnnAvg_CLIM - PrecipTempCorr_meanAnnAvg_1yr,
    # above Freezing month as absolute difference
    aboveFreezing_month_meanAnnAvg_1yrAnom = aboveFreezing_month_meanAnnAvg_CLIM - aboveFreezing_month_meanAnnAvg_1yr,
    # isothermailty as % difference
    isothermality_meanAnnAvg_1yrAnom = isothermality_meanAnnAvg_CLIM - isothermality_meanAnnAvg_1yr,
    # annual water deficit as % difference
    annWaterDeficit_meanAnnAvg_1yrAnom = (annWaterDeficit_meanAnnAvg_CLIM - annWaterDeficit_meanAnnAvg_1yr)/annWaterDeficit_meanAnnAvg_CLIM,
    # wet degree days as % difference
    annWetDegDays_meanAnnAvg_1yrAnom = (annWetDegDays_meanAnnAvg_CLIM - annWetDegDays_meanAnnAvg_1yr)/annWetDegDays_meanAnnAvg_CLIM,
    # mean VPD as absolute difference
    annVPD_mean_meanAnnAvg_1yrAnom = (annVPD_mean_meanAnnAvg_CLIM - annVPD_mean_meanAnnAvg_1yr),
    # min VPD as absolute difference
    annVPD_min_meanAnnAvg_1yrAnom = (annVPD_min_meanAnnAvg_CLIM - annVPD_min_meanAnnAvg_1yr),
    # max VPD as absolute difference
    annVPD_max_meanAnnAvg_1yrAnom = (annVPD_max_meanAnnAvg_CLIM - annVPD_max_meanAnnAvg_1yr),
    # 95th percentile of max VPD as absolute difference 
    annVPD_max_95percentile_1yrAnom = (annVPD_max_95percentile_CLIM - annVPD_max_95percentile_1yr),
    # 95th percentile of annual water deficit as % difference
    annWaterDeficit_95percentile_1yrAnom = (annWaterDeficit_95percentile_CLIM - annWaterDeficit_95percentile_1yr)/annWaterDeficit_95percentile_CLIM,
    # 5th percentile of annual wet degree days as % difference 
    annWetDegDays_5percentile_1yrAnom = (annWetDegDays_5percentile_CLIM - annWetDegDays_5percentile_1yr)/annWetDegDays_5percentile_CLIM,
    # 10th percentile of frost-free days as absolute difference 
    durationFrostFreeDays_5percentile_1yrAnom = (durationFrostFreeDays_5percentile_CLIM - durationFrostFreeDays_5percentile_1yr),
    # mean of frost free days as absolute difference
    durationFrostFreeDays_meanAnnAvg_1yrAnom = (durationFrostFreeDays_meanAnnAvg_CLIM - durationFrostFreeDays_meanAnnAvg_1yr)
    )

climDat <- cbind(test5, anomDat_10yr, anomDat_5yr#, anomDat_1yr
                 ) %>% 
  select(-c(swe_meanAnnAvg_1yr:Start_1yr))
# save climate values for analysis 
#write.csv(climDat, "./data/dayMet/climateValuesForAnalysis_final.csv", row.names = FALSE)
saveRDS(climDat, "./Data_processed/CoverData/dayMetClimateValuesForAnalysis_final.rds")

# #plot MAP and MAT for sites for most recent 30-year period
# plotDat <- annMeans_30yr %>% 
#   filter(End_30yr == 2022)
# 
# ggplot(plotDat) +
#   geom_point(aes(y = prcp_meanAnnTotal_30yr, x = tmin_meanAnnAvg_30yr))
# 
# ggplot(plotDat) +
#   geom_point(aes(x = Long, y = Lat, col = tmax_meanAnnAvg_30yr))
