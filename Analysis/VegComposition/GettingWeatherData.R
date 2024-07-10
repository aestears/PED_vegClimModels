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

ggplot(test2) + 
  geom_sf(aes(col = daymet_v4_prcp_monttl_na_1980_1))
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

# calculating climate variables for models -------------------------------
climVar <- allMetDat %>% 
  mutate(totalAnnPrecip = rowSums(.[52:63]),
         totalAnnSwe = rowSums(.[28:39]),
         meanAnnTmax = rowMeans(.[2:13]),
         meanAnnTmin = rowMeans(.[16:27]),
         annMaxTmax = pmap_dbl(.[2:13], max),
         annMinTmin = pmap_dbl(.[16:27], min),
         meanAnnVp = rowMeans(.[40:51])) %>% 
  mutate(tempAmp = annMaxTmax - annMinTmin #the difference between annMaxTmax and annMinTmin
           ) #%>% 
  #select(year, Long, Lat, totalAnnPrecip, totalAnnSwe, meanAnnTmax, meanAnnTmin, annMaxTmax, annMinTmin, meanAnnVp) 

# determine the months in which the temperature is highest, and in which precip is highest
# 
# 
# climVar$tMaxMax_Month <- list_c(apply(climVar[1:100000,2:13], MARGIN = 1, FUN = function(x)
#   as.character(paste(lubridate::month(1:12, label = TRUE)[which(x == max(x))]))
#   ))
# 
# climVar$precipMax_Month <-  apply(climVar[,52:63], MARGIN = 1, FUN = function(x)
#   as.character(paste(lubridate::month(1:12, label = TRUE)[which(x == max(x))]))
# )
# climVar$tMaxMax_Month <- unlist(climVar$tMaxMax_Month)

## calculate MAP and MAT over the last 30 years (a sliding window?)
slidingMetMeans <- function(inDat, start, end) {
  outDat <- inDat %>% 
    filter(year %in% c(start:end)) %>% 
    group_by(Long, Lat) %>% 
    summarize(swe_meanAnnualAverage = mean(swe_annAvg),
              tmin_meanAnnualAverage = mean(tmin_annAvg),
              tmax_meanAnnualAverage = mean(tmax_annAvg),
              vp_meanAnnualAverage = mean(vp_annAvg),
              prcp_meanAnnualTotal = mean(prcp_annTotal))
  return(outDat)
}

endDats <- as.matrix(c(2010:2023))
annMeans <- apply(endDats, MARGIN = 1, FUN = function(x)
  slidingMetMeans(inDat = annMetDat, start = as.numeric(x-30), end = as.numeric(x))
  )
names(annMeans) <- c(2010:2023)
annMeans_2 <- lapply(endDats, function(x) {
 temp <- cbind(annMeans[[as.character(x)]], x)
 temp$Start <- x-30
  names(temp) <- c(names(annMeans[[1]]), "End", "Start")
  return(temp)
})

annMeans_3 <- data.table::rbindlist(annMeans_2)

## add lagged data to the main climate value data.frame
test <- climVar %>% 
  #filter(year == 2020) %>% 
  #slice(1:100) %>% 
  left_join(annMeans_3, by = c("year" = "End", 
                               "Long" = "Long", 
                               "Lat" = "Lat"))

# save climate values for analysis 
write.csv(test, "./data/dayMet/climateValuesForAnalysis_final.csv", row.names = FALSE)

#plot MAP and MAT for sites for most recent 30-year period
plotDat <- annMeans_3 %>% 
  filter(End == 2022)

ggplot(plotDat) +
  geom_point(aes(y = prcp_meanAnnualTotal, x = tmin_meanAnnualAverage))

ggplot(plotDat) +
  geom_point(aes(x = Long, y = Lat, col = tmax_meanAnnualAverage))
