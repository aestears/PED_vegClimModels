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
write.csv(allMetDat, file = "./data/dayMet/sampledDataForAnalysis.csv", row.names = FALSE)

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
