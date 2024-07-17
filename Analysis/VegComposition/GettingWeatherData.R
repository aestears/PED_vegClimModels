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

## get veg data to get locations (are some duplicated locates b/c plots have data from multiple years)
vegDat <- read.csv("./data/DataForAnalysis.csv") #%>% 
#   select(Lat, Lon) %>% 
#   unique()
# points <- vegDat %>% 
#   select(UniqueID, Lat, Lon) %>% 
#   unique()
# write.csv(points, "./data/dayMet/dayMetPoints.csv", row.names = FALSE)
years <- unique(vegDat$Year)
# get sf point data
points_sf <- st_read(dsn = "./data/DataForAnalysisPoints/", layer = "vegCompPoints")


## get monthly data downloaded from online
#test <- terra::rast("./data/dayMet/rawMonthlyData/orders/70e0da02b9d2d6e8faa8c97d211f3546/Daymet_Monthly_V4R1/data/daymet_v4_prcp_monttl_na_1980.tif")
rastNames <- list.files("./data/dayMet/rawMonthlyData/orders/70e0da02b9d2d6e8faa8c97d211f3546/Daymet_Monthly_V4R1/data/")
#reproject points to same crs as rasters
test <-  rast("./data/dayMet/rawMonthlyData/orders/70e0da02b9d2d6e8faa8c97d211f3546/Daymet_Monthly_V4R1/data/daymet_v4_prcp_monttl_na_1980.tif")
points_sf <- points_sf %>% 
  sf::st_transform(crs(test))

# load monthly total precip values and make into a raster stack
for (i in 1:length(rastNames[str_detect(string = rastNames, 
                       pattern = "prcp_monttl_na_.....tif$")])){
  
    name_i <- rastNames[str_detect(string = rastNames, 
                         pattern = "prcp_monttl_na_.....tif$")][i]
    temp_rast <- rast(paste0("./data/dayMet/rawMonthlyData/orders/70e0da02b9d2d6e8faa8c97d211f3546/Daymet_Monthly_V4R1/data/", name_i))
    
    # get the data for the locations we want
    temp_points <- 
      temp_rast %>% 
      terra::extract(points_sf)
    
    # make column for year and change column names to month value only
    temp_points$year <- as.numeric(str_extract(names(temp_points)[2], pattern = "\\d{4}"))
    names(temp_points)[2:13] <- c("prcp_Jan", "prcp_Feb", "prcp_March", "prcp_April", 
                                  "prcp_May", "prcp_June", "prcp_July", "prcp_Aug", 
                                  "prcp_Sept", "prcp_Oct", "prcp_Nov", "prcp_Dec")
    temp_points <- temp_points %>% 
      select(year, prcp_Jan:prcp_Dec) %>% 
      cbind(st_coordinates(points_sf)) %>% 
      rename(Long = X, Lat = Y)
    
    if (i == 1 ){
      prcpPoints <- temp_points
    } else {
      prcpPoints <- rbind(prcpPoints, temp_points)
    }
}

# load monthly average swe values and make into a raster stack
for (i in 1:length(rastNames[str_detect(string = rastNames, 
                                        pattern = "swe_monavg_na_.....tif$")])){
  
  name_i <- rastNames[str_detect(string = rastNames, 
                                 pattern = "swe_monavg_na_.....tif$")][i]
  temp_rast <- rast(paste0("./data/dayMet/rawMonthlyData/orders/70e0da02b9d2d6e8faa8c97d211f3546/Daymet_Monthly_V4R1/data/", name_i))
  
  # get the data for the locations we want
  temp_points <- 
    temp_rast %>% 
    terra::extract(points_sf)
  
  # make column for year and change column names to month value only
  temp_points$year <- as.numeric(str_extract(names(temp_points)[2], pattern = "\\d{4}"))
  names(temp_points)[2:13] <- c("swe_Jan", "swe_Feb", "swe_March", "swe_April", 
                                "swe_May", "swe_June", "swe_July", "swe_Aug", 
                                "swe_Sept", "swe_Oct", "swe_Nov", "swe_Dec")
  temp_points <- temp_points %>% 
    select(year, swe_Jan:swe_Dec) %>% 
    cbind(st_coordinates(points_sf)) %>% 
    rename(Long = X, Lat = Y)
  
  if (i == 1 ){
    swePoints <- temp_points
  } else {
    swePoints <- rbind(swePoints, temp_points)
  }
}

# load monthly average tmax values and make into a raster stack
for (i in 1:length(rastNames[str_detect(string = rastNames, 
                                        pattern = "tmax_monavg_na_.....tif$")])){
  
  name_i <- rastNames[str_detect(string = rastNames, 
                                 pattern = "tmax_monavg_na_.....tif$")][i]
  temp_rast <- rast(paste0("./data/dayMet/rawMonthlyData/orders/70e0da02b9d2d6e8faa8c97d211f3546/Daymet_Monthly_V4R1/data/", name_i))
  # get the data for the locations we want
  temp_points <- 
    temp_rast %>% 
    terra::extract(points_sf)
  
  # make column for year and change column names to month value only
  temp_points$year <- as.numeric(str_extract(names(temp_points)[2], pattern = "\\d{4}"))
  names(temp_points)[2:13] <- c("tmax_Jan", "tmax_Feb", "tmax_March", "tmax_April", 
                                "tmax_May", "tmax_June", "tmax_July", "tmax_Aug", 
                                "tmax_Sept", "tmax_Oct", "tmax_Nov", "tmax_Dec")
  temp_points <- temp_points %>% 
    select(year, tmax_Jan:tmax_Dec) %>% 
    cbind(st_coordinates(points_sf)) %>% 
    rename(Long = X, Lat = Y)
  
  if (i == 1 ){
    tmaxPoints <- temp_points
  } else {
    tmaxPoints <- rbind(tmaxPoints, temp_points)
  }
}

# load monthly average tmin values and make into a raster stack
for (i in 1:length(rastNames[str_detect(string = rastNames, 
                                        pattern = "tmin_monavg_na_.....tif$")])){
  
  name_i <- rastNames[str_detect(string = rastNames, 
                                 pattern = "tmin_monavg_na_.....tif$")][i]
  temp_rast <- rast(paste0("./data/dayMet/rawMonthlyData/orders/70e0da02b9d2d6e8faa8c97d211f3546/Daymet_Monthly_V4R1/data/", name_i))
  # get the data for the locations we want
  temp_points <- 
    temp_rast %>% 
    terra::extract(points_sf)
  
  # make column for year and change column names to month value only
  temp_points$year <- as.numeric(str_extract(names(temp_points)[2], pattern = "\\d{4}"))
  names(temp_points)[2:13] <- c("tmin_Jan", "tmin_Feb", "tmin_March", "tmin_April", 
                                "tmin_May", "tmin_June", "tmin_July", "tmin_Aug", 
                                "tmin_Sept", "tmin_Oct", "tmin_Nov", "tmin_Dec")
  temp_points <- temp_points %>% 
    select(year, tmin_Jan:tmin_Dec) %>% 
    cbind(st_coordinates(points_sf)) %>% 
    rename(Long = X, Lat = Y)
  
  if (i == 1 ){
    tminPoints <- temp_points
  } else {
    tminPoints <- rbind(tminPoints, temp_points)
  }
}

# load monthly average vapor pressure (?) values and make into a raster stack
for (i in 1:length(rastNames[str_detect(string = rastNames, 
                                        pattern = "vp_monavg_na_.....tif$")])){
  
  name_i <- rastNames[str_detect(string = rastNames, 
                                 pattern = "vp_monavg_na_.....tif$")][i]
  temp_rast <- rast(paste0("./data/dayMet/rawMonthlyData/orders/70e0da02b9d2d6e8faa8c97d211f3546/Daymet_Monthly_V4R1/data/", name_i))
  # get the data for the locations we want
  temp_points <- 
    temp_rast %>% 
    terra::extract(points_sf)
  
  # make column for year and change column names to month value only
  temp_points$year <- as.numeric(str_extract(names(temp_points)[2], pattern = "\\d{4}"))
  names(temp_points)[2:13] <- c("vp_Jan", "vp_Feb", "vp_March", "vp_April", "vp_May", 
                                "vp_June", "vp_July", "vp_Aug", "vp_Sept", "vp_Oct", "vp_Nov", "vp_Dec")
  temp_points <- temp_points %>% 
    select(year, vp_Jan:vp_Dec) %>% 
    cbind(st_coordinates(points_sf)) %>% 
    rename(Long = X, Lat = Y)
  
  if (i == 1 ){
    vpPoints <- temp_points
  } else {
    vpPoints <- rbind(vpPoints, temp_points)
  }
}


## add all variables together
allMetDat <- tmaxPoints %>% 
  left_join(tminPoints) %>% 
  left_join(swePoints) %>% 
  left_join(vpPoints) %>% 
  left_join(prcpPoints)

# save data
#write.csv(allMetDat, file = "./data/dayMet/sampledDataForAnalysis.csv", row.names = FALSE)
allMetDat <- read.csv("./data/dayMet/sampledDataForAnalysis.csv")

# ## get weather data from DayMet (using dayMetR package)
# download_daymet_batch(
#   file_location = "./data/dayMet/dayMetPoints.csv",
#   start = 1980, 
#   end = 2023,
#   internal = FALSE, 
#   path = "./data/dayMet/monthly/"
# ) 
# 
# daymetr::download_daymet_ncss(
#     #location = st_bbox(points_sf[1,]),
#     location = c(41.7, -121.4, 41.6, -121.3),
#     start = 1980, 
#     end = 1980, 
#     path = "./data/dayMet/",
#     param = "vp",
#     frequency = "monthly"
# )
# 
# # download monthly
# download_daymet_ncss(location = c(41.7, -121.4, 41.6, -121.3),
#                      start = 1980,
#                      end = 1980,
#                      frequency = "monthly",
#                      param = c("tmin","tmax"),
#                      path = "./data/dayMet/",
#                      silent = TRUE)
# 
# # convert nc files to geotiff
# nc2tif(tempdir())


# get annual climate data -------------------------------------------------

## get monthly data downloaded from online
#test <- terra::rast("./data/dayMet/rawMonthlyData/orders/70e0da02b9d2d6e8faa8c97d211f3546/Daymet_Monthly_V4R1/data/daymet_v4_prcp_monttl_na_1980.tif")
rastNames2 <- list.files("./data/dayMet/yearly/")

# load annual total precip values and make into a raster stack
for (i in 1:length(rastNames2[str_detect(string = rastNames2, 
                                        pattern = "prcp_annttl_na_.....tif$")])){
  
  name_i <- rastNames2[str_detect(string = rastNames2, 
                                 pattern = "prcp_annttl_na_.....tif$")][i]
  temp_rast <- rast(paste0("./data/dayMet/yearly/", name_i))
  
  # get the data for the locations we want
  temp_points <- 
    temp_rast %>% 
    terra::extract(points_sf)
  
  # make column for year and change column names to month value only
  temp_points$year <- as.numeric(str_extract(names(temp_points)[2], pattern = "\\d{4}"))
  names(temp_points)[2] <- c("prcp_annTotal")
  temp_points <- temp_points %>% 
    select(year, prcp_annTotal) %>% 
    cbind(st_coordinates(points_sf)) %>% 
    rename(Long = X, Lat = Y)
  
  if (i == 1 ){
    prcpPoints_ann <- temp_points
  } else {
    prcpPoints_ann <- rbind(prcpPoints_ann, temp_points)
  }
}

# load annual total swe values and make into a raster stack
for (i in 1:length(rastNames2[str_detect(string = rastNames2, 
                                         pattern = "swe_annavg_na_.....tif$")])){
  
  name_i <- rastNames2[str_detect(string = rastNames2, 
                                  pattern = "swe_annavg_na_.....tif$")][i]
  temp_rast <- rast(paste0("./data/dayMet/yearly/", name_i))
  
  # get the data for the locations we want
  temp_points <- 
    temp_rast %>% 
    terra::extract(points_sf)
  
  # make column for year and change column names to month value only
  temp_points$year <- as.numeric(str_extract(names(temp_points)[2], pattern = "\\d{4}"))
  names(temp_points)[2] <- c("swe_annAvg")
  temp_points <- temp_points %>% 
    select(year, swe_annAvg) %>% 
    cbind(st_coordinates(points_sf)) %>% 
    rename(Long = X, Lat = Y)
  
  if (i == 1 ){
    swePoints_ann <- temp_points
  } else {
    swePoints_ann <- rbind(swePoints_ann, temp_points)
  }
}

# load annual tmax ann avg values and make into a raster stack
for (i in 1:length(rastNames2[str_detect(string = rastNames2, 
                                         pattern = "tmax_annavg_na_.....tif$")])){
  
  name_i <- rastNames2[str_detect(string = rastNames2, 
                                  pattern = "tmax_annavg_na_.....tif$")][i]
  temp_rast <- rast(paste0("./data/dayMet/yearly/", name_i))
  
  # get the data for the locations we want
  temp_points <- 
    temp_rast %>% 
    terra::extract(points_sf)
  
  # make column for year and change column names to month value only
  temp_points$year <- as.numeric(str_extract(names(temp_points)[2], pattern = "\\d{4}"))
  names(temp_points)[2] <- c("tmax_annAvg")
  temp_points <- temp_points %>% 
    select(year, tmax_annAvg) %>% 
    cbind(st_coordinates(points_sf)) %>% 
    rename(Long = X, Lat = Y)
  
  if (i == 1 ){
    tmaxPoints_ann <- temp_points
  } else {
    tmaxPoints_ann <- rbind(tmaxPoints_ann, temp_points)
  }
}

# load annual tmin ann avg values and make into a raster stack
for (i in 1:length(rastNames2[str_detect(string = rastNames2, 
                                         pattern = "tmin_annavg_na_.....tif$")])){
  
  name_i <- rastNames2[str_detect(string = rastNames2, 
                                  pattern = "tmin_annavg_na_.....tif$")][i]
  temp_rast <- rast(paste0("./data/dayMet/yearly/", name_i))
  
  # get the data for the locations we want
  temp_points <- 
    temp_rast %>% 
    terra::extract(points_sf)
  
  # make column for year and change column names to month value only
  temp_points$year <- as.numeric(str_extract(names(temp_points)[2], pattern = "\\d{4}"))
  names(temp_points)[2] <- c("tmin_annAvg")
  temp_points <- temp_points %>% 
    select(year, tmin_annAvg) %>% 
    cbind(st_coordinates(points_sf)) %>% 
    rename(Long = X, Lat = Y)
  
  if (i == 1 ){
    tminPoints_ann <- temp_points
  } else {
    tminPoints_ann <- rbind(tminPoints_ann, temp_points)
  }
}

# load annual vp ann avg values and make into a raster stack
for (i in 1:length(rastNames2[str_detect(string = rastNames2, 
                                         pattern = "vp_annavg_na_.....tif$")])){
  
  name_i <- rastNames2[str_detect(string = rastNames2, 
                                  pattern = "vp_annavg_na_.....tif$")][i]
  temp_rast <- rast(paste0("./data/dayMet/yearly/", name_i))
  
  # get the data for the locations we want
  temp_points <- 
    temp_rast %>% 
    terra::extract(points_sf)
  
  # make column for year and change column names to month value only
  temp_points$year <- as.numeric(str_extract(names(temp_points)[2], pattern = "\\d{4}"))
  names(temp_points)[2] <- c("vp_annAvg")
  temp_points <- temp_points %>% 
    select(year, vp_annAvg) %>% 
    cbind(st_coordinates(points_sf)) %>% 
    rename(Long = X, Lat = Y)
  
  if (i == 1 ){
    vpPoints_ann <- temp_points
  } else {
    vpPoints_ann <- rbind(vpPoints_ann, temp_points)
  }
}

# join together
annMetDat <- prcpPoints_ann %>% 
  left_join(swePoints_ann) %>% 
  left_join(tminPoints_ann) %>% 
  left_join(tmaxPoints_ann) %>% 
  left_join(vpPoints_ann)

# save data
#write.csv(annMetDat, file = "./data/dayMet/sampledDataForAnalysis_Annual.csv", row.names = FALSE)
annMetDat <- read.csv("./data/dayMet/sampledDataForAnalysis_Annual.csv")

# add annual data to the monthly data (will use later in processing)
allMetDat2 <- allMetDat %>% 
  left_join(annMetDat)

# calculating climate variables for models -------------------------------
climVar <- allMetDat2 %>% 
  mutate(totalAnnPrecip = rowSums(.[52:63]), # total annual precipitation
         totalAnnSwe = rowSums(.[28:39]), # total annual swe
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
         
         isothermality = pmap_dbl(.[c(2:13,16:27)], # isothermality
                                  .f = function(tmax_Jan, tmax_Feb, tmax_March, tmax_April, tmax_May, tmax_June, tmax_July, tmax_Aug, tmax_Sept, tmax_Oct,  tmax_Nov,  tmax_Dec,
                                                tmin_Jan, tmin_Feb, tmin_March, tmin_April, tmin_May, tmin_June, tmin_July, tmin_Aug, tmin_Sept, tmin_Oct,  tmin_Nov,  tmin_Dec, ...) {
                                    tmins <- c(tmin_Jan, tmin_Feb, tmin_March, tmin_April, tmin_May, tmin_June, tmin_July, tmin_Aug, tmin_Sept, tmin_Oct,  tmin_Nov,  tmin_Dec)
                                    tmaxes <- c(tmax_Jan, tmax_Feb, tmax_March, tmax_April, tmax_May, tmax_June, tmax_July, tmax_Aug, tmax_Sept, tmax_Oct,  tmax_Nov,  tmax_Dec)
                                    tMaxMax <- max(c(tmax_Jan, tmax_Feb, tmax_March, tmax_April, tmax_May, tmax_June, tmax_July, tmax_Aug, tmax_Sept, tmax_Oct,  tmax_Nov,  tmax_Dec))
                                    tMinMin <- min(c(tmin_Jan, tmin_Feb, tmin_March, tmin_April, tmin_May, tmin_June, tmin_July, tmin_Aug, tmin_Sept, tmin_Oct,  tmin_Nov,  tmin_Dec))
                                    mean(tmins-tmaxes)/(tMaxMax-tMinMin) * 100
                                  })
         )

# save for subsequent use
#saveRDS(climVar, file = "./data/dayMet/climateValuesForAnalysis_monthly.rds")
climVar <- readRDS(file="./data/dayMet/climateValuesForAnalysis_monthly.rds")

# calculate sliding window inter-annual climate means ----------------------

## calculate MAP and MAT over past years (a sliding window?)
# function
slidingMetMeans <- function(inDat, start, end) {
  outDat <- inDat %>% 
    filter(year %in% c(start:end)) %>% 
    group_by(Long, Lat) %>% 
    summarize(swe_meanAnnAvg = mean(swe_annAvg),
              tmin_meanAnnAvg = mean(tmin_annAvg),
              tmax_meanAnnAvg = mean(tmax_annAvg),
              vp_meanAnnAvg = mean(vp_annAvg),
              prcp_meanAnnTotal = mean(prcp_annTotal),
              T_warmestMonth_meanAnnAvg = mean(T_warmestMonth), # temperature of warmest month
              T_coldestMonth_meanAnnAvg = mean(T_coldestMonth), # temperature of coldest month
              precip_wettestMonth_meanAnnAvg = mean(precip_wettestMonth), # precip of wettest month
              precip_driestMonth_meanAnnAvg = mean(precip_driestMonth), # precip of driest month
              precip_Seasonality_meanAnnAvg = mean(precip_Seasonality),
              PrecipTempCorr_meanAnnAvg = mean(PrecipTempCorr),
              aboveFreezing_month_meanAnnAvg = mean(aboveFreezing_month),
              isothermality_meanAnnAvg = mean(isothermality)
              )
  return(outDat)
}

# for last 30-year window
endDats <- as.matrix(c(2010:2023))
annMeans <- apply(endDats, MARGIN = 1, FUN = function(x)
  slidingMetMeans(inDat = climVar, start = as.numeric(x-30), end = as.numeric(x))
  )
names(annMeans) <- c(2010:2023)
annMeans_30yr <- lapply(endDats, function(x) {
 temp <- cbind(annMeans[[as.character(x)]], x)
 temp$Start <- x-30
  names(temp) <- c(names(annMeans[[1]]), "End", "Start")
  return(temp)
})

annMeans_30yr <- data.table::rbindlist(annMeans_30yr)
names(annMeans_30yr)[3:17] <- paste0(names(annMeans_30yr)[3:17], "_30yr")

# for last 10-year window
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
names(annMeans_10yr)[3:17] <- paste0(names(annMeans_10yr)[3:17], "_10yr")

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
names(annMeans_5yr)[3:17] <- paste0(names(annMeans_5yr)[3:17], "_5yr")

## add lagged data to the main climate value data.frame
test <- climVar %>% 
  #filter(year == 2020) %>% 
  #slice(1:100) %>% 
  left_join(annMeans_30yr, by = c("year" = "End_30yr", 
                               "Long" = "Long", 
                               "Lat" = "Lat")) %>% 
  left_join(annMeans_10yr, by = c("year" = "End_10yr", 
                                  "Long" = "Long", 
                                  "Lat" = "Lat")
              ) %>% 
  left_join(annMeans_5yr, by = c("year" = "End_5yr", 
                                 "Long" = "Long", 
                                 "Lat" = "Lat"))

# save climate values for analysis 
write.csv(test, "./data/dayMet/climateValuesForAnalysis_final.csv", row.names = FALSE)

#plot MAP and MAT for sites for most recent 30-year period
plotDat <- annMeans_30yr %>% 
  filter(End_30yr == 2022)

ggplot(plotDat) +
  geom_point(aes(y = prcp_meanAnnTotal_30yr, x = tmin_meanAnnAvg_30yr))

ggplot(plotDat) +
  geom_point(aes(x = Long, y = Lat, col = tmax_meanAnnAvg_30yr))
