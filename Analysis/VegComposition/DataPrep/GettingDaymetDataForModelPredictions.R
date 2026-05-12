#///////////////
# Acquiring daymet data across CONUS and calcuating climate variables for use in creating maps of odel prediction
# Alice Stears
# 20 March 2026 
#///////////////

# load packages -----------------------------------------------------------

library(tidyverse)
library(sf)
library(daymetr)
library(terra)
library(future.apply)
library(parallel)
library(USAboundaries)

# is this a test run? 
test <- TRUE
# do I need to download the data
downloadRawData <- TRUE
# set the size of the bins for the dayMet data
binSize <- 100000
# Get points for all locations in dayMet centroids ------------------------
# raster w/ dayMet grid
temp_rast <- rast(paste0("./Data_raw/dayMet/rawMonthlyData/orders/70e0da02b9d2d6e8faa8c97d211f3546/Daymet_Monthly_V4R1/data/daymet_v4_prcp_monttl_na_1980.tif"))

# mask the raster to only have values in CONUS
cropped_states_2 <- us_states(resolution = "low")
cropped_states_2 <- suppressMessages(cropped_states_2 %>%
                                       dplyr::filter(name!="Hawaii") %>%
                                       dplyr::filter(name!="Alaska") %>%
                                       dplyr::filter(name!="Puerto Rico") %>%
                                       dplyr::filter(name!="American Samoa") %>%
                                       dplyr::filter(name!="Guam") %>%
                                       dplyr::filter(name!="Commonwealth of the Northern Mariana Islands") %>%
                                       dplyr::filter(name!="United States Virgin Islands") %>%
                                       sf::st_transform(sf::st_crs(temp_rast))) 

temp_rast_mask <- temp_rast %>% 
  terra::crop(y = cropped_states_2) %>% 
  terra::mask(mask = terra::vect(cropped_states_2)) 

# get points at the centroid of each cell
temp_rast_points <- temp_rast_mask %>% 
  terra::xyFromCell(1:ncell(temp_rast_mask)) %>% 
  cbind(terra::values(temp_rast_mask$daymet_v4_prcp_monttl_na_1980_1)) 

# make the points into an sf object
dayMet_points <- st_as_sf(as.data.frame(temp_rast_points), coords = c("x", "y"))

### subset data for testing (if required) 
if (test) {
  dayMet_points <- 
    dayMet_points %>% 
    slice(1:100)
}

# slice the points into bins (100000 points each)
dayMet_points$sliceID <- rep(1:1000000, each = binSize, length.out = nrow(dayMet_points))

# now start a huge loop where we go through an iteration for each slice of the point data
for (z in 1:length(unique(dayMet_points$sliceID))) {
  # subset the points into data for the current slice
  dayMet_points_z <- dayMet_points[dayMet_points$sliceID == z,]
  # Acquire weather data and calculate variables ----------------------------
  ## calculating climate for 2023; so I only need climate data starting in 1992 (2023-31 = 1992)
  # get names of rasters (starts in 1980, only need data from )
  if (downloadRawData) {
    rastNames <- list.files("./Data_raw/dayMet/rawMonthlyData/orders/70e0da02b9d2d6e8faa8c97d211f3546/Daymet_Monthly_V4R1/data/")
    
    # load monthly total precip values and make into a raster stack
    for (i in 14:length(rastNames[str_detect(string = rastNames,
                                             pattern = "prcp_monttl_na_.....tif$")])){
      
      name_i <- rastNames[str_detect(string = rastNames,
                                     pattern = "prcp_monttl_na_.....tif$")][i]
      temp_rast <- rast(paste0("./Data_raw/dayMet/rawMonthlyData/orders/70e0da02b9d2d6e8faa8c97d211f3546/Daymet_Monthly_V4R1/data/", name_i))
      
      # get the data for the locations we want
      temp_points <-
        temp_rast %>%
        terra::extract(dayMet_points_z)
      
      # make column for year and change column names to month value only
      temp_points$year <- as.numeric(str_extract(names(temp_points)[2], pattern = "\\d{4}"))
      names(temp_points)[2:13] <- c("prcp_Jan", "prcp_Feb", "prcp_March", "prcp_April",
                                    "prcp_May", "prcp_June", "prcp_July", "prcp_Aug",
                                    "prcp_Sept", "prcp_Oct", "prcp_Nov", "prcp_Dec")
      temp_points <- temp_points %>%
        select(year, prcp_Jan:prcp_Dec) %>%
        cbind(st_coordinates(dayMet_points_z)) %>%
        rename(Long = X, Lat = Y)
      
      if (i == 14 ){
        prcpPoints <- temp_points
      } else {
        prcpPoints <- rbind(prcpPoints, temp_points)
      }
    }
    
    
    # load monthly average tmax values and make into a raster stack
    for (i in 14:length(rastNames[str_detect(string = rastNames,
                                             pattern = "tmax_monavg_na_.....tif$")])){
      
      name_i <- rastNames[str_detect(string = rastNames,
                                     pattern = "tmax_monavg_na_.....tif$")][i]
      temp_rast <- rast(paste0("./Data_raw/dayMet/rawMonthlyData/orders/70e0da02b9d2d6e8faa8c97d211f3546/Daymet_Monthly_V4R1/data/", name_i))
      # get the data for the locations we want
      temp_points <-
        temp_rast %>%
        terra::extract(dayMet_points_z)
      
      # make column for year and change column names to month value only
      temp_points$year <- as.numeric(str_extract(names(temp_points)[2], pattern = "\\d{4}"))
      names(temp_points)[2:13] <- c("tmax_Jan", "tmax_Feb", "tmax_March", "tmax_April",
                                    "tmax_May", "tmax_June", "tmax_July", "tmax_Aug",
                                    "tmax_Sept", "tmax_Oct", "tmax_Nov", "tmax_Dec")
      temp_points <- temp_points %>%
        select(year, tmax_Jan:tmax_Dec) %>%
        cbind(st_coordinates(dayMet_points_z)) %>%
        rename(Long = X, Lat = Y)
      
      if (i == 14 ){
        tmaxPoints <- temp_points
      } else {
        tmaxPoints <- rbind(tmaxPoints, temp_points)
      }
    }
    
    # load monthly average tmin values and make into a raster stack
    for (i in 14:length(rastNames[str_detect(string = rastNames,
                                             pattern = "tmin_monavg_na_.....tif$")])){
      
      name_i <- rastNames[str_detect(string = rastNames,
                                     pattern = "tmin_monavg_na_.....tif$")][i]
      temp_rast <- rast(paste0("./Data_raw/dayMet/rawMonthlyData/orders/70e0da02b9d2d6e8faa8c97d211f3546/Daymet_Monthly_V4R1/data/", name_i))
      # get the data for the locations we want
      temp_points <-
        temp_rast %>%
        terra::extract(dayMet_points_z)
      
      # make column for year and change column names to month value only
      temp_points$year <- as.numeric(str_extract(names(temp_points)[2], pattern = "\\d{4}"))
      names(temp_points)[2:13] <- c("tmin_Jan", "tmin_Feb", "tmin_March", "tmin_April",
                                    "tmin_May", "tmin_June", "tmin_July", "tmin_Aug",
                                    "tmin_Sept", "tmin_Oct", "tmin_Nov", "tmin_Dec")
      temp_points <- temp_points %>%
        select(year, tmin_Jan:tmin_Dec) %>%
        cbind(st_coordinates(dayMet_points_z)) %>%
        rename(Long = X, Lat = Y)
      
      if (i == 14 ){
        tminPoints <- temp_points
      } else {
        tminPoints <- rbind(tminPoints, temp_points)
      }
    }
    
    ## add all variables together (they are in the same order, so can cbind)
    # allMetDat <- tmaxPoints %>%
    #   cbind(tminPoints %>% select(-year, -Long, -Lat)) %>%
    #   #cbind(vpPoints %>% select(-year, -Long, -Lat)) %>%
    #   cbind(prcpPoints %>% select(-year, -Long, -Lat))
    tminPoints <- tminPoints %>%
      unique()
    tmaxPoints <- tmaxPoints %>%
      unique()
    prcpPoints <- prcpPoints %>%
      unique()
    
    allMetDat <- tmaxPoints %>%
      left_join(tminPoints, by = c("year", "Long", "Lat")) %>%
      #cbind(vpPoints %>% select("year", "Long", "Lat")) %>%
      left_join(prcpPoints, by = c("year", "Long", "Lat"))

    # save data
    if (!test) {
      write.csv(allMetDat, file = paste0("./Data_processed/CoverData/dayMet_intermediate/WallToWall/sampledDataForAnalysis_COVER_slice",z ,".csv"), row.names = FALSE)
      #allMetDat <- read.csv("./Data_processed/CoverData/dayMet_intermediate/WallToWall/sampledDataForAnalysis_COVER.csv")
    }
    
    # get annual climate data -------------------------------------------------
    
    # get annual data downloaded from online
    #test <- terra::rast("./Data_raw/dayMet/rawMonthlyData/orders/70e0da02b9d2d6e8faa8c97d211f3546/Daymet_Monthly_V4R1/Data_raw/dayMet_v4_prcp_monttl_na_1980.tif")
    rastNames2 <- list.files("./Data_raw/dayMet/yearly/")
    
    # load annual total precip values and make into a raster stack
    for (i in 14:length(rastNames2[str_detect(string = rastNames2,
                                              pattern = "prcp_annttl_na_.....tif$")])){
      
      name_i <- rastNames2[str_detect(string = rastNames2,
                                      pattern = "prcp_annttl_na_.....tif$")][i]
      temp_rast <- rast(paste0("./Data_raw/dayMet/yearly/", name_i))
      
      # get the data for the locations we want
      temp_points <-
        temp_rast %>%
        terra::extract(dayMet_points_z)
      
      # make column for year and change column names to month value only
      temp_points$year <- as.numeric(str_extract(names(temp_points)[2], pattern = "\\d{4}"))
      names(temp_points)[2] <- c("prcp_annTotal")
      temp_points <- temp_points %>%
        select(year, prcp_annTotal) %>%
        cbind(st_coordinates(dayMet_points_z)) %>%
        rename(Long = X, Lat = Y)
      
      if (i == 14 ){
        prcpPoints_ann <- temp_points
      } else {
        prcpPoints_ann <- rbind(prcpPoints_ann, temp_points)
      }
    }
    
    # load annual tmax ann avg values and make into a raster stack
    for (i in 14:length(rastNames2[str_detect(string = rastNames2,
                                              pattern = "tmax_annavg_na_.....tif$")])){
      
      name_i <- rastNames2[str_detect(string = rastNames2,
                                      pattern = "tmax_annavg_na_.....tif$")][i]
      temp_rast <- rast(paste0("./Data_raw/dayMet/yearly/", name_i))
      
      # get the data for the locations we want
      temp_points <-
        temp_rast %>%
        terra::extract(dayMet_points_z)
      
      # make column for year and change column names to month value only
      temp_points$year <- as.numeric(str_extract(names(temp_points)[2], pattern = "\\d{4}"))
      names(temp_points)[2] <- c("tmax_annAvg")
      temp_points <- temp_points %>%
        select(year, tmax_annAvg) %>%
        cbind(st_coordinates(dayMet_points_z)) %>%
        rename(Long = X, Lat = Y)
      
      if (i == 14 ){
        tmaxPoints_ann <- temp_points
      } else {
        tmaxPoints_ann <- rbind(tmaxPoints_ann, temp_points)
      }
    }
    
    # load annual tmin ann avg values and make into a raster stack
    for (i in 14:length(rastNames2[str_detect(string = rastNames2,
                                              pattern = "tmin_annavg_na_.....tif$")])){
      
      name_i <- rastNames2[str_detect(string = rastNames2,
                                      pattern = "tmin_annavg_na_.....tif$")][i]
      temp_rast <- rast(paste0("./Data_raw/dayMet/yearly/", name_i))
      
      # get the data for the locations we want
      temp_points <-
        temp_rast %>%
        terra::extract(dayMet_points_z)
      
      # make column for year and change column names to month value only
      temp_points$year <- as.numeric(str_extract(names(temp_points)[2], pattern = "\\d{4}"))
      names(temp_points)[2] <- c("tmin_annAvg")
      temp_points <- temp_points %>%
        select(year, tmin_annAvg) %>%
        cbind(st_coordinates(dayMet_points_z)) %>%
        rename(Long = X, Lat = Y)
      
      if (i == 14 ){
        tminPoints_ann <- temp_points
      } else {
        tminPoints_ann <- rbind(tminPoints_ann, temp_points)
      }
    }
    
    tminPoints_ann <- tminPoints_ann %>%
      unique()
    tmaxPoints_ann <- tmaxPoints_ann %>%
      unique()
    prcpPoints_ann <- prcpPoints_ann %>%
      unique()
    # join together
    annMetDat <- prcpPoints_ann %>%
      #left_join(swePoints_ann) %>%
      left_join(tminPoints_ann, by = c("year", "Long", "Lat")) %>%
      left_join(tmaxPoints_ann, by = c("year", "Long", "Lat"))# %>%
    # left_join(vpPoints_ann %>% select(-year, -Long, -Lat))
    
    if (!test) {
      # save data
      write.csv(annMetDat, file = paste0("./Data_processed/CoverData/dayMet_intermediate/WallToWall/sampledDataForAnalysis_Annual_COVER_slice",z ,".csv"), row.names = FALSE)
      #annMetDat <- read.csv("./Data_processed/CoverData/dayMet_intermediate/WallToWall/sampledDataForAnalysis_Annual_COVER.csv")
    }
  } else {
    # download annual met data
    annMetDat <- read.csv( file = paste0("./Data_processed/CoverData/dayMet_intermediate/WallToWall/sampledDataForAnalysis_Annual_COVER_slice",z ,".csv"))
    # download monthly met data
    allMetDat <- read.csv( file = paste0("./Data_processed/CoverData/dayMet_intermediate/WallToWall/sampledDataForAnalysis_COVER_slice",z ,".csv"))
  }
  
  # add annual data to the monthly data (will use later in processing)
  allMetDat2 <- allMetDat %>%
    cbind(annMetDat %>% select(-"year", -"Lat", -"Long"))
  
  # drop values w/ NAs (coastal locations)
  allMetDat2 <- allMetDat2 %>%
    drop_na()
  
  # calculating climate variables for models -------------------------------
  climVar <- allMetDat2 %>%
    #slice(23507:23909) %>%
    mutate(totalAnnPrecip = rowSums(.[c("prcp_Jan", "prcp_Feb", "prcp_March", "prcp_April", "prcp_May", "prcp_June", "prcp_July", "prcp_Aug", "prcp_Sept", "prcp_Oct" ,"prcp_Nov", "prcp_Dec")]), # total annual precipitation
           #maxAnnSwe = rowSums(.[28:39]), # total annual swe
           T_warmestMonth = pmap_dbl(.[c("tmax_Jan", "tmax_Feb", "tmax_March", "tmax_April", "tmax_May", "tmax_June", "tmax_July", "tmax_Aug", "tmax_Sept", "tmax_Oct",  "tmax_Nov",  "tmax_Dec")], max), # temperature of warmest month
           T_coldestMonth = pmap_dbl(.[c("tmin_Jan", "tmin_Feb", "tmin_March", "tmin_April", "tmin_May", "tmin_June", "tmin_July", "tmin_Aug", "tmin_Sept", "tmin_Oct",  "tmin_Nov",  "tmin_Dec")], min), # temperature of coldest month
           #Tmin_annAvgOfMonthly_OLD = rowSums(.[c("tmin_Jan", "tmin_Feb", "tmin_March", "tmin_April", "tmin_May", "tmin_June", "tmin_July", "tmin_Aug", "tmin_Sept", "tmin_Oct",  "tmin_Nov",  "tmin_Dec")])/12,
           ##
           Tmin_annAvgOfMonthly = pmap_dbl(.[c("tmin_Jan", "tmin_Feb", "tmin_March", "tmin_April", "tmin_May", "tmin_June", "tmin_July", "tmin_Aug", "tmin_Sept", "tmin_Oct",  "tmin_Nov",  "tmin_Dec")],
                                           .f = function(tmin_Jan, tmin_Feb, tmin_March, tmin_April, tmin_May, tmin_June, tmin_July, tmin_Aug, tmin_Sept, tmin_Oct,  tmin_Nov,  tmin_Dec) {
                                             return(mean(c(
                                               tmin_Jan * 31/31, tmin_Feb * 28.5/31, tmin_March * 31/31, tmin_April * 30/31,
                                               tmin_May * 31/31, tmin_June * 30/31, tmin_July * 31/31, tmin_Aug * 31/31,
                                               tmin_Sept * 30/31, tmin_Oct * 31/31,  tmin_Nov * 30/31,  tmin_Dec * 31/31
                                             )
                                             )) # in degrees C
                                           }),
           ##
           #Tmax_annAvgOfMonthly_OLD = rowSums(.[c("tmax_Jan", "tmax_Feb", "tmax_March", "tmax_April", "tmax_May", "tmax_June", "tmax_July", "tmax_Aug", "tmax_Sept", "tmax_Oct",  "tmax_Nov",  "tmax_Dec")])/12,
           ##
           Tmax_annAvgOfMonthly = pmap_dbl(.[c("tmax_Jan", "tmax_Feb", "tmax_March", "tmax_April", "tmax_May", "tmax_June", "tmax_July", "tmax_Aug", "tmax_Sept", "tmax_Oct",  "tmax_Nov",  "tmax_Dec")],
                                           .f = function(tmax_Jan, tmax_Feb, tmax_March, tmax_April, tmax_May, tmax_June, tmax_July, tmax_Aug, tmax_Sept, tmax_Oct,  tmax_Nov,  tmax_Dec) {
                                             return(mean(c(
                                               tmax_Jan * 31/31, tmax_Feb * 28.5/31, tmax_March * 31/31, tmax_April * 30/31,
                                               tmax_May * 31/31, tmax_June * 30/31, tmax_July * 31/31, tmax_Aug * 31/31,
                                               tmax_Sept * 30/31, tmax_Oct * 31/31,  tmax_Nov * 30/31,  tmax_Dec * 31/31
                                             )
                                             )) # in degrees C
                                           }),
           ##
           precip_wettestMonth = pmap_dbl(.[c("prcp_Jan", "prcp_Feb", "prcp_March", "prcp_April", "prcp_May", "prcp_June", "prcp_July", "prcp_Aug", "prcp_Sept", "prcp_Oct" ,"prcp_Nov", "prcp_Dec")],
                                          max), # precip of wettest month
           precip_driestMonth = pmap_dbl(.[c("prcp_Jan", "prcp_Feb", "prcp_March", "prcp_April", "prcp_May", "prcp_June", "prcp_July", "prcp_Aug", "prcp_Sept", "prcp_Oct" ,"prcp_Nov", "prcp_Dec")],
                                         min), # precip of driest month
           precip_Seasonality = pmap_dbl(.[c("prcp_Jan", "prcp_Feb", "prcp_March", "prcp_April", "prcp_May", "prcp_June", "prcp_July", "prcp_Aug", "prcp_Sept", "prcp_Oct" ,"prcp_Nov", "prcp_Dec")],   # coefficient of variation (sd/mean) of precipitation
                                         .f = function(prcp_Jan, prcp_Feb, prcp_March, prcp_April, prcp_May, prcp_June, prcp_July, prcp_Aug, prcp_Sept, prcp_Oct ,prcp_Nov, prcp_Dec, ...)
                                         {temp <- c(prcp_Jan, prcp_Feb, prcp_March, prcp_April, prcp_May, prcp_June, prcp_July, prcp_Aug, prcp_Sept, prcp_Oct ,prcp_Nov, prcp_Dec)
                                         sd(temp)/mean(temp)
                                         }
           ),
           PrecipTempCorr = pmap_dbl(.[c("tmax_Jan", "tmax_Feb", "tmax_March", "tmax_April", "tmax_May", "tmax_June", "tmax_July", "tmax_Aug", "tmax_Sept", "tmax_Oct",  "tmax_Nov",  "tmax_Dec",
                                         "prcp_Jan", "prcp_Feb", "prcp_March", "prcp_April", "prcp_May", "prcp_June", "prcp_July", "prcp_Aug", "prcp_Sept", "prcp_Oct" ,"prcp_Nov", "prcp_Dec")], #correlation of monthly temp and precip
                                     .f = function(tmax_Jan, tmax_Feb, tmax_March, tmax_April, tmax_May, tmax_June, tmax_July, tmax_Aug, tmax_Sept, tmax_Oct,  tmax_Nov,  tmax_Dec,
                                                   prcp_Jan, prcp_Feb, prcp_March, prcp_April, prcp_May, prcp_June, prcp_July, prcp_Aug, prcp_Sept, prcp_Oct ,prcp_Nov, prcp_Dec, ...) {
                                       cor(y = c(tmax_Jan, tmax_Feb, tmax_March, tmax_April, tmax_May, tmax_June, tmax_July, tmax_Aug, tmax_Sept, tmax_Oct,  tmax_Nov,  tmax_Dec),
                                           x = c(prcp_Jan, prcp_Feb, prcp_March, prcp_April, prcp_May, prcp_June, prcp_July, prcp_Aug, prcp_Sept, prcp_Oct ,prcp_Nov, prcp_Dec))
                                     }),
           aboveFreezing_month = pmap_dbl(.[c("tmin_Jan", "tmin_Feb", "tmin_March", "tmin_April", "tmin_May", "tmin_June", "tmin_July", "tmin_Aug", "tmin_Sept", "tmin_Oct",  "tmin_Nov",  "tmin_Dec")], # month when temp gets above freezing (when tmin > 0 degrees C, so no freeze at night )
                                          .f = function(tmin_Jan, tmin_Feb, tmin_March, tmin_April, tmin_May, tmin_June, tmin_July, tmin_Aug, tmin_Sept, tmin_Oct,  tmin_Nov,  tmin_Dec) {
                                            temp <- c(tmin_Jan, tmin_Feb, tmin_March, tmin_April, tmin_May, tmin_June, tmin_July, tmin_Aug, tmin_Sept, tmin_Oct,  tmin_Nov,  tmin_Dec)
                                            which(temp > 0)[1] # in degrees C
                                          }),
           lastAboveFreezing_month = pmap_dbl(.[c("tmin_Jan", "tmin_Feb", "tmin_March", "tmin_April", "tmin_May", "tmin_June", "tmin_July", "tmin_Aug", "tmin_Sept", "tmin_Oct",  "tmin_Nov",  "tmin_Dec")], # month when temp gets above freezing (when tmin > 0 degrees C, so no freeze at night )
                                              .f = function(tmin_Jan, tmin_Feb, tmin_March, tmin_April, tmin_May, tmin_June, tmin_July, tmin_Aug, tmin_Sept, tmin_Oct,  tmin_Nov,  tmin_Dec) {
                                                temp <- c(tmin_Jan, tmin_Feb, tmin_March, tmin_April, tmin_May, tmin_June, tmin_July, tmin_Aug, tmin_Sept, tmin_Oct,  tmin_Nov,  tmin_Dec)
                                                temp2 <- which(temp > 0) # in degrees C
                                                if(length(temp2)>0) {
                                                  return(max(temp2))
                                                } else {
                                                  return(NA)
                                                }
                                              }),
           
           isothermality = pmap_dbl(.[c("tmax_Jan", "tmax_Feb", "tmax_March", "tmax_April", "tmax_May", "tmax_June", "tmax_July", "tmax_Aug", "tmax_Sept", "tmax_Oct",  "tmax_Nov",  "tmax_Dec",
                                        "tmin_Jan", "tmin_Feb", "tmin_March", "tmin_April", "tmin_May", "tmin_June", "tmin_July", "tmin_Aug", "tmin_Sept", "tmin_Oct",  "tmin_Nov",  "tmin_Dec")], # isothermality
                                    .f = function(tmax_Jan, tmax_Feb, tmax_March, tmax_April, tmax_May, tmax_June, tmax_July, tmax_Aug, tmax_Sept, tmax_Oct,  tmax_Nov,  tmax_Dec,
                                                  tmin_Jan, tmin_Feb, tmin_March, tmin_April, tmin_May, tmin_June, tmin_July, tmin_Aug, tmin_Sept, tmin_Oct,  tmin_Nov,  tmin_Dec, ...) {
                                      tmins <- c(tmin_Jan, tmin_Feb, tmin_March, tmin_April, tmin_May, tmin_June, tmin_July, tmin_Aug, tmin_Sept, tmin_Oct,  tmin_Nov,  tmin_Dec)
                                      tmaxes <- c(tmax_Jan, tmax_Feb, tmax_March, tmax_April, tmax_May, tmax_June, tmax_July, tmax_Aug, tmax_Sept, tmax_Oct,  tmax_Nov,  tmax_Dec)
                                      tMaxMax <- max(c(tmax_Jan, tmax_Feb, tmax_March, tmax_April, tmax_May, tmax_June, tmax_July, tmax_Aug, tmax_Sept, tmax_Oct,  tmax_Nov,  tmax_Dec))
                                      tMinMin <- min(c(tmin_Jan, tmin_Feb, tmin_March, tmin_April, tmin_May, tmin_June, tmin_July, tmin_Aug, tmin_Sept, tmin_Oct,  tmin_Nov,  tmin_Dec))
                                      mean(tmaxes-tmins)/(tMaxMax-tMinMin) * 100
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
    #slice(23507:23909) %>%
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
      #mutate(
      # annual water deficit (mm of water over degrees celsius)(sum across all months?)
      tmean = pmap_dbl(.[c("tmean_Jan", "tmean_Feb", "tmean_March", "tmean_April", "tmean_May", "tmean_June", "tmean_July", "tmean_Aug", "tmean_Sept", "tmean_Oct" ,"tmean_Nov", "tmean_Dec")],
                       .f = function(tmean_Jan, tmean_Feb, tmean_March, tmean_April, tmean_May, tmean_June, tmean_July, tmean_Aug, tmean_Sept, tmean_Oct ,tmean_Nov, tmean_Dec, ...) {
                         temp <- sum(tmean_Jan* 31/31, tmean_Feb* 28.5/31, tmean_March* 31/31, tmean_April * 30/31,
                                     tmean_May * 31/31, tmean_June * 30/31, tmean_July * 31/31, tmean_Aug * 31/31,
                                     tmean_Sept * 30/31, tmean_Oct * 31/31, tmean_Nov * 30/31, tmean_Dec * 31/31)/12
                         return(temp)
                       }),
      # tmean_OLD = pmap_dbl(.[c("tmean_Jan", "tmean_Feb", "tmean_March", "tmean_April", "tmean_May", "tmean_June", "tmean_July", "tmean_Aug", "tmean_Sept", "tmean_Oct" ,"tmean_Nov", "tmean_Dec")],
      #                      .f = function(tmean_Jan, tmean_Feb, tmean_March, tmean_April, tmean_May, tmean_June, tmean_July, tmean_Aug, tmean_Sept, tmean_Oct ,tmean_Nov, tmean_Dec, ...) {
      #                        temp <- sum(tmean_Jan, tmean_Feb, tmean_March, tmean_April,
      #                                    tmean_May, tmean_June, tmean_July, tmean_Aug,
      #                                    tmean_Sept, tmean_Oct, tmean_Nov, tmean_Dec)/12
      #                        return(temp)
      #                      }),
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
      annVPD_mean = pmap_dbl(.[c("VPD_Jan", "VPD_Feb", "VPD_March","VPD_April" ,"VPD_May","VPD_June", "VPD_July","VPD_Aug","VPD_Sept","VPD_Oct","VPD_Nov","VPD_Dec")],
                             .f = function(VPD_Jan, VPD_Feb, VPD_March,VPD_April ,VPD_May,VPD_June, VPD_July,VPD_Aug,VPD_Sept,VPD_Oct,VPD_Nov,VPD_Dec) {
                               mean(c(VPD_Jan* 31/31, VPD_Feb * 28.5/31, VPD_March * 31/31, VPD_April* 30/31,
                                      VPD_May * 31/31, VPD_June * 30/31, VPD_July * 31/31, VPD_Aug * 31/31,
                                      VPD_Sept * 30/31, VPD_Oct * 31/31, VPD_Nov * 30/31, VPD_Dec * 31/31))
                             }),
      # annVPD_mean_OLD = pmap_dbl(.[c("VPD_Jan", "VPD_Feb", "VPD_March","VPD_April" ,"VPD_May","VPD_June", "VPD_July","VPD_Aug","VPD_Sept","VPD_Oct","VPD_Nov","VPD_Dec")],
      #                            .f = function(VPD_Jan, VPD_Feb, VPD_March,VPD_April ,VPD_May,VPD_June, VPD_July,VPD_Aug,VPD_Sept,VPD_Oct,VPD_Nov,VPD_Dec) {
      #                              mean(VPD_Jan, VPD_Feb, VPD_March , VPD_April,
      #                                     VPD_May , VPD_June , VPD_July , VPD_Aug ,
      #                                     VPD_Sept , VPD_Oct , VPD_Nov, VPD_Dec)
      #                            }),
      # annual maximum vapor pressure deficit (in milibars)
      annVPD_max = pmap_dbl(.[c("VPD_Jan", "VPD_Feb", "VPD_March","VPD_April" ,"VPD_May","VPD_June", "VPD_July","VPD_Aug","VPD_Sept","VPD_Oct","VPD_Nov","VPD_Dec")], max),
      # annual minimum vapor pressure deficit (in milibars)
      annVPD_min = pmap_dbl(.[c("VPD_Jan", "VPD_Feb", "VPD_March","VPD_April" ,"VPD_May","VPD_June", "VPD_July","VPD_Aug","VPD_Sept","VPD_Oct","VPD_Nov","VPD_Dec")], min)
    )
  
  
  climVar <- cbind(climVar, climVar2)
  # plot(climVar$tmin_annAvg, climVar$tmean)
  # plot(climVar$tmax_annAvg, climVar$tmean)
  
  ## recalculate tmean as the average of annual average tmax and annual average tmean
  # climVar <- climVar %>%
  #   mutate(tmean = (tmin_annAvg + tmax_annAvg)/2)
  
  rm(climVar2, tminPoints, tminPoints_ann, tmaxPoints, tmaxPoints_ann, prcpPoints, prcpPoints_ann)
  gc()
  
  # save for subsequent use
  if (!test) {
    saveRDS(climVar, file = paste0("./Data_processed/CoverData/dayMet_intermediate/WallToWall/climateValuesForAnalysis_monthly_slice",z ,".csv"))
    #climVar <- readRDS(file="./Data_processed/CoverData/dayMet_intermediate/WallToWall/climateValuesForAnalysis_monthly.rds")
  }
  
  #fix issue w/ prcp_seasonality and prcpTempCorr --------------------------
  #precip seasonality
  #Plot annual precip against precip seasonality to see what the values look like as it approaches 0…
  # climVar %>%
  #   slice_sample(n = 500000) %>%
  #   ggplot() +
  #   geom_point(aes(x = totalAnnPrecip, y = precip_Seasonality), alpha = .3) +
  #   geom_smooth(aes(y = precip_Seasonality, x = totalAnnPrecip))
  # as precip gets closer to 0, seasonality goes up (the average is 2), so that's what I'll change the NA values to
  climVar[is.na(climVar$precip_Seasonality), "precip_Seasonality"] <- 2
  
  # precip temp corr
  #It would make sense to change this to 0 (as we approach 0 precip, it seems likely that precip and temp are uncorrelated)
  # climVar %>%
  #   slice_sample(n = 500000) %>%
  #   ggplot() +
  #   geom_point(aes(x = tmean, y = PrecipTempCorr), alpha = .3) +
  #   geom_smooth(aes(y = PrecipTempCorr, x = tmean))
  
  #with low precip, the correlation is close to zero, but actually a bit below... will change to -.25
  climVar[is.na(climVar$PrecipTempCorr), "PrecipTempCorr"] <- -.25
  
  # calculate sliding window inter-annual climate means ----------------------
  
  ## calculate MAP and MAT over past years (a sliding window?)
  # function
  slidingMetMeans <- function(inDat, start, end) {
    endActual <- end-1 # subtract one so that we're actually looking at the 30, 10, 5, etc. years previous to the "end" year
    outDat <- inDat %>%
      filter(year %in% c(start:endActual)) %>%
      group_by(Long, Lat) %>%
      summarize(#sweMax_meanAnnAvg = mean(swe_annAvg),
        tmin_meanAnnAvg = mean(tmin_annAvg),
        tmax_meanAnnAvg = mean(tmax_annAvg),
        tmean_meanAnnAvg = mean(tmean),
        #vp_meanAnnAvg = mean(vp_annAvg),
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
  
  
  ## for last 30 year window
  #set up parallel processing for this part of the workflow
  detectCores() # there are 16 cores
  #plan(multicore, workers = detectCores()-6)
  
  # run parallel process
  endDats <- as.matrix(c(2023))
  
  annMeans <- #future_lapply
    mclapply(X = endDats, #MARGIN = 1,
             FUN = function(x)
               slidingMetMeans(inDat = climVar[climVar$year %in% c(as.numeric(x-31):(as.numeric(x)-1)),]
                               , start = as.numeric(x-31), end = as.numeric(x))
    )
  
  # put together into one list
  annMeans_all <- c(annMeans)
  
  names(annMeans_all) <- c(2023)
  annMeans_30yr_temp1 <- lapply(endDats, function(x) {
    temp <- cbind(annMeans_all[[as.character(x)]], x)
    temp$Start <- x-31
    names(temp) <- c(names(annMeans_all[[1]]), "End", "Start")
    return(temp)
  })
  
  annMeans_30yr <- data.table::rbindlist(c(annMeans_30yr_temp1))
  
  names(annMeans_30yr)[3:26] <- paste0(names(annMeans_30yr)[3:26], "_30yr")
  if (!test) {
    saveRDS(annMeans_30yr, paste0("./Data_processed/CoverData/dayMet_intermediate/WallToWall/annMeans_30yrs_slice",z ,".csv"))
    #annMeans_30yr <- readRDS("./Data_processed/CoverData/dayMet_intermediate/WallToWall/annMeans_30yrs.rds")
  }
  
  ## add lagged data to the main climate value data.frame
  testLag <- climVar %>%
    select(-c(tmax_Jan:tmax_Dec, tmin_Jan:prcp_Dec)) %>%
    #filter(year == 2020) %>%
    #slice(1:100) %>%
    left_join(annMeans_30yr, by = c("year" = "End_30yr",
                                    "Long" = "Long",
                                    "Lat" = "Lat")) 
  
  if (!test) {
    # save intermediate data
    saveRDS(testLag, paste0("./Data_processed/CoverData/dayMet_intermediate/WallToWall/climVars_AnnualMeansAndLaggedValues_slice",z ,".csv"))
    #test <- readRDS("./Data_processed/CoverData/dayMet_intermediate/climVars_AnnualMeansAndLaggedValues.rds")
  }
  
  # for those years including and after 2010, use the averages over the previous
  # 30 years for climate. for those years before 2010, use the averages over any
  # previous years starting w/ 1980 -- we calculated this previously, but rename here for clarity
  
  testNew <- testLag %>%
    rename("tmin_meanAnnAvg_CLIM" = tmin_meanAnnAvg_30yr,
           "tmin_meanAnnAvg_CLIM" = tmin_meanAnnAvg_30yr                  ,
           "tmax_meanAnnAvg_CLIM" = tmax_meanAnnAvg_30yr                  ,
           "tmean_meanAnnAvg_CLIM" = tmean_meanAnnAvg_30yr                 ,
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
  
  rm(climVar)
  gc()
  
  climDatNew <- testNew 

  if (!test) {
    # save climate values for analysis
    
    saveRDS(climDatNew, paste0("./Data_processed/CoverData/dayMet_intermediate/WallToWall/dayMetClimateValuesForAnalysis_final_slice",z ,".csv"))
    #climDatNew <- readRDS("./Data_processed/CoverData/dayMetClimateValuesForAnalysis_final.rds")
  }
# remove values for the current slice and proceed to the next   
rm(allMetDat, allMetDat2, annMeans, annMeans_30yr, annMeans_30yr_temp1, annMeans_all, annMetDat, climDatNew, testLag, testNew)
gc()
}

# Now add data from different slices together  ---------------------------------
# get file names 
climDatNames <- list.files("./Data_processed/CoverData/dayMet_intermediate/WallToWall/", pattern = "dayMetClimateValuesForAnalysis_final_slice")

listOut <- apply(as.matrix(climDatNames), MARGIN = 1, FUN = function(x) {
  # get data fore each slice and remove any data that aren't for 2023
 readRDS(paste0("./Data_processed/CoverData/dayMet_intermediate/WallToWall/",x)) %>% 
           filter(year == 2023)
  #return(get(x))
}) %>% 
  purrr::list_rbind()

# save for further analysis 
listOut %>% 
  select(-precip_driestMonth_meanAnnAvg_3yrAnom, -precip_driestMonth_meanAnnAvg_2yrAnom) %>% 
  saveRDS("./Data_processed/WallToWallClimateData/DayMetData_allCONUS_2023ClimateValues.rds")

# save as a raster w/ layers for each variable ----------------------------
listOut <- readRDS("./Data_processed/WallToWallClimateData/DayMetData_allCONUS_2023ClimateValues.rds")

# data averaged over 1992 to 2022 -- data used as predictors for cover data collected in 2023
# turn into a raster 
testOutVect <- listOut %>% 
  select(year, Long, Lat, tmin_meanAnnAvg_CLIM:Start_CLIM) %>% 
    vect(geom = c("Long", "Lat"), crs = crs(temp_rast)) 

testOutRast <- lapply(c("tmin_meanAnnAvg_CLIM"                   ,"tmax_meanAnnAvg_CLIM"  ,                
                        "tmean_meanAnnAvg_CLIM"                  ,"prcp_meanAnnTotal_CLIM",                 "T_warmestMonth_meanAnnAvg_CLIM"      ,  
                        "T_coldestMonth_meanAnnAvg_CLIM"         ,"precip_wettestMonth_meanAnnAvg_CLIM"    ,"precip_driestMonth_meanAnnAvg_CLIM"  ,  
                        "precip_Seasonality_meanAnnAvg_CLIM"     ,"PrecipTempCorr_meanAnnAvg_CLIM"         ,"aboveFreezing_month_meanAnnAvg_CLIM" ,  
                        "isothermality_meanAnnAvg_CLIM"          ,"annWaterDeficit_meanAnnAvg_CLIM"        ,"annWetDegDays_meanAnnAvg_CLIM"       ,  
                        "annVPD_mean_meanAnnAvg_CLIM"            ,"annVPD_max_meanAnnAvg_CLIM"             ,"annVPD_min_meanAnnAvg_CLIM"          ,  
                        "annVPD_max_95percentile_CLIM"           ,"annWaterDeficit_95percentile_CLIM"      ,"annWetDegDays_5percentile_CLIM"      ,  
                        "durationFrostFreeDays_5percentile_CLIM" ,"durationFrostFreeDays_meanAnnAvg_CLIM"), 
                      FUN = function(x) {
                        tempRast <- testOutVect %>% 
                          rasterize(y = temp_rast_mask, field = (x))
                        return(tempRast)
                      })
names(testOutRast) <- c("tmin_meanAnnAvg_CLIM"                   ,"tmax_meanAnnAvg_CLIM"  ,                
                        "tmean_meanAnnAvg_CLIM"                  ,"prcp_meanAnnTotal_CLIM",                 "T_warmestMonth_meanAnnAvg_CLIM"      ,  
                        "T_coldestMonth_meanAnnAvg_CLIM"         ,"precip_wettestMonth_meanAnnAvg_CLIM"    ,"precip_driestMonth_meanAnnAvg_CLIM"  ,  
                        "precip_Seasonality_meanAnnAvg_CLIM"     ,"PrecipTempCorr_meanAnnAvg_CLIM"         ,"aboveFreezing_month_meanAnnAvg_CLIM" ,  
                        "isothermality_meanAnnAvg_CLIM"          ,"annWaterDeficit_meanAnnAvg_CLIM"        ,"annWetDegDays_meanAnnAvg_CLIM"       ,  
                        "annVPD_mean_meanAnnAvg_CLIM"            ,"annVPD_max_meanAnnAvg_CLIM"             ,"annVPD_min_meanAnnAvg_CLIM"          ,  
                        "annVPD_max_95percentile_CLIM"           ,"annWaterDeficit_95percentile_CLIM"      ,"annWetDegDays_5percentile_CLIM"      ,  
                        "durationFrostFreeDays_5percentile_CLIM" ,"durationFrostFreeDays_meanAnnAvg_CLIM")

testOutRast_final <- c(testOutRast[[1]], testOutRast[[2]], testOutRast[[3]],testOutRast[[4]],testOutRast[[5]],testOutRast[[6]],testOutRast[[7]],testOutRast[[8]],
                       testOutRast[[9]],testOutRast[[10]],testOutRast[[11]],testOutRast[[12]],testOutRast[[13]],testOutRast[[14]],testOutRast[[15]],testOutRast[[16]],
                       testOutRast[[17]],testOutRast[[18]],testOutRast[[19]],testOutRast[[20]],testOutRast[[21]],testOutRast[[22]])

terra::set.names(testOutRast_final, value = c("tmin_meanAnnAvg_CLIM"                   ,"tmax_meanAnnAvg_CLIM"  ,                
                                     "tmean_meanAnnAvg_CLIM"                  ,"prcp_meanAnnTotal_CLIM",                 "T_warmestMonth_meanAnnAvg_CLIM"      ,  
                                     "T_coldestMonth_meanAnnAvg_CLIM"         ,"precip_wettestMonth_meanAnnAvg_CLIM"    ,"precip_driestMonth_meanAnnAvg_CLIM"  ,  
                                     "precip_Seasonality_meanAnnAvg_CLIM"     ,"PrecipTempCorr_meanAnnAvg_CLIM"         ,"aboveFreezing_month_meanAnnAvg_CLIM" ,  
                                     "isothermality_meanAnnAvg_CLIM"          ,"annWaterDeficit_meanAnnAvg_CLIM"        ,"annWetDegDays_meanAnnAvg_CLIM"       ,  
                                     "annVPD_mean_meanAnnAvg_CLIM"            ,"annVPD_max_meanAnnAvg_CLIM"             ,"annVPD_min_meanAnnAvg_CLIM"          ,  
                                     "annVPD_max_95percentile_CLIM"           ,"annWaterDeficit_95percentile_CLIM"      ,"annWetDegDays_5percentile_CLIM"      ,  
                                     "durationFrostFreeDays_5percentile_CLIM" ,"durationFrostFreeDays_meanAnnAvg_CLIM")
)

# save raster
terra::writeRaster(x = testOutRast_final, filename = "./Data_processed/WallToWallClimateData/DayMetData_allCONUS_2023ClimateValues_raster.tif")


# Add soils information --------------------------------------------------
if (sum(list.files("./Data_processed/") == "SoilsRaster.rds") == 0) {
  # if there is NOT an .rds file called SoilsRaster, do the following
  # load gridded soils data from Daniel (currently an old version, will be updated w/ SOLUS100 data)
  gridClay <- terra::rast(x = "./Data_raw/soilsDB_new/claytotal_PED-CONUS4km_SOLUS100.nc") %>% 
    terra::project(y = crs(vegDat))
  names(gridClay) <- c(clayPerc_2cm,
                       "clayPerc_7cm"  ,            
                       "clayPerc_15cm" , 
                       "clayPerc_25cm", 
                       "clayPerc_35cm" ,
                       "clayPerc_50cm" , 
                       "clayPerc_70cm" , 
                       "clayPerc_90cm" , 
                       "clayPerc_125cm", 
                       "clayPerc_176cm") 
  
  gridSand <- terra::rast(x = "./Data_raw/soilsDB_new/sandtotal_PED-CONUS4km_SOLUS100.nc") %>% 
    terra::project(y = crs(vegDat) )
  names(gridSand) <- c("sandPerc_2cm" ,         
                       "sandPerc_7cm",           
                       "sandPerc_15cm" ,        
                       "sandPerc_25cm",         
                       "sandPerc_35cm",
                       "sandPerc_50cm",   
                       "sandPerc_70cm" ,          
                       "sandPerc_90cm",          
                       "sandPerc_125cm",
                       "sandPerc_176cm")
  
  gridSilt <- terra::rast(x = "./Data_raw/soilsDB_new/silttotal_PED-CONUS4km_SOLUS100.nc") %>% 
    terra::project(y = crs(vegDat)) 
  names(gridSilt) <- c(
    "siltPerc_2cm",
    "siltPerc_7cm",
    "siltPerc_15cm",
    "siltPerc_25cm",
    "siltPerc_35cm",
    "siltPerc_50cm",
    "siltPerc_70cm",
    "siltPerc_90cm",
    "siltPerc_125cm",
    "siltPerc_176cm")
  
  gridDensity <- terra::rast(x = "./Data_raw/soilsDB_new/dbovendry_PED-CONUS4km_SOLUS100.nc") %>% 
    terra::project(y = crs(vegDat))
  names(gridDensity) <- c(
    "density_gcm3_2cm" ,
    "density_gcm3_7cm",
    "density_gcm3_15cm",
    "density_gcm3_25cm",
    "density_gcm3_35cm"  ,
    "density_gcm3_50cm",
    "density_gcm3_70cm",
    "density_gcm3_90cm",
    "density_gcm3_125cm",
    "density_gcm3_176cm")
  
  gridThickness <- terra::rast(x = "./Data_raw/soilsDB_new/hzthk_PED-CONUS4km_SOLUS100.nc") %>% 
    terra::project(y = crs(vegDat)) 
  names(gridThickness) <- c(
    "horizonThickness_cm_2cm" , 
    "horizonThickness_cm_7cm",
    "horizonThickness_cm_15cm", 
    "horizonThickness_cm_25cm", 
    "horizonThickness_cm_35cm", 
    "horizonThickness_cm_50cm", 
    "horizonThickness_cm_70cm", 
    "horizonThickness_cm_90cm",
    "horizonThickness_cm_125cm" ,
    "horizonThickness_cm_176cm")
  
  gridCoarse <- terra::rast(x = "./Data_raw/soilsDB_new/fragvol_PED-CONUS4km_SOLUS100.nc") %>% 
    terra::project(y = crs(vegDat))
  names(gridCoarse) <- c(
    "coarsePerc_2cm" ,
    "coarsePerc_7cm",
    "coarsePerc_15cm",
    "coarsePerc_25cm",
    "coarsePerc_35cm",
    "coarsePerc_50cm",
    "coarsePerc_70cm",
    "coarsePerc_90cm",
    "coarsePerc_125cm",
    "coarsePerc_176cm")
  
  gridCarbon <- terra::rast(x = "./Data_raw/soilsDB_new/soc_PED-CONUS4km_SOLUS100.nc") %>% 
    terra::project(y = crs(vegDat))
  names(gridCarbon) <- c(
    "organicCarbonPerc_2cm",
    "organicCarbonPerc_7cm",
    "organicCarbonPerc_15cm" ,
    "organicCarbonPerc_25cm" ,
    "organicCarbonPerc_35cm" ,
    "organicCarbonPerc_50cm" ,
    "organicCarbonPerc_70cm" ,
    "organicCarbonPerc_90cm" ,
    "organicCarbonPerc_125cm" ,
    "organicCarbonPerc_176cm")
  
  # stack into a single raster
  soilRast <- c(gridClay, gridSand, gridSilt, gridCoarse, gridDensity, gridThickness, gridCarbon)
  saveRDS(soilRast, file = "./Data_processed/SoilsRaster.rds")
} else {
  # otherwise, read in the file
  soilRast <- readRDS("./Data_processed/SoilsRaster.rds")
}


# sample soils data for veg. points ---------------------------------------
# sample raster to get values for the points in the cover dataset
listOut_vect <- listOut %>% 
  st_as_sf(coords = c("Long", "Lat"), crs = crs(temp_rast), remove = FALSE) %>% 
  terra::vect()
vegSoils_df <- soilRast %>% 
  terra::extract(y = listOut_vect #%>% dplyr::select(-x,-y)
                , xy = TRUE, bind = TRUE) %>% 
  as.data.frame()

# calculate soils variables w/ cover data ---------------------------------
names(vegSoils_df)[c(length(names(vegSoils_df))-1, length(names(vegSoils_df)))] <- c("x_UTM", "y_UTM")
vegSoils_new <- 
  vegSoils_df %>% 
  dplyr::mutate(
    # Soil depth 
    soilDepth = pmap_dbl(.[c("horizonThickness_cm_2cm" , "horizonThickness_cm_7cm" , "horizonThickness_cm_15cm" , 
                             "horizonThickness_cm_25cm" , "horizonThickness_cm_35cm" , "horizonThickness_cm_50cm" , 
                             "horizonThickness_cm_70cm" , "horizonThickness_cm_90cm" , "horizonThickness_cm_125cm" , 
                             "horizonThickness_cm_176cm")], sum, na.rm = TRUE),
    #Surface clay (influences how much moisture can get into the profile)
    surfaceClay_perc = clayPerc_2cm) %>% 
  mutate(soilDepth = replace(soilDepth, is.na(horizonThickness_cm_2cm), values = NA)) %>% 
  mutate( 
    # Sand average across depths (avg. weighted by width of layer)
    avgSandPerc_acrossDepth = pmap_dbl(.[c("horizonThickness_cm_2cm" , "horizonThickness_cm_7cm" , "horizonThickness_cm_15cm" , 
                                           "horizonThickness_cm_25cm" , "horizonThickness_cm_35cm" , "horizonThickness_cm_50cm" , 
                                           "horizonThickness_cm_70cm" , "horizonThickness_cm_90cm" , "horizonThickness_cm_125cm" , 
                                           "horizonThickness_cm_176cm", "sandPerc_2cm", "sandPerc_7cm" , "sandPerc_15cm",
                                           "sandPerc_25cm" , "sandPerc_35cm", "sandPerc_50cm" , "sandPerc_70cm", "sandPerc_90cm" ,
                                           "sandPerc_125cm", "sandPerc_176cm", "soilDepth")], 
                                       function(horizonThickness_cm_2cm , horizonThickness_cm_7cm , horizonThickness_cm_15cm , 
                                                horizonThickness_cm_25cm , horizonThickness_cm_35cm , horizonThickness_cm_50cm , 
                                                horizonThickness_cm_70cm , horizonThickness_cm_90cm , horizonThickness_cm_125cm , 
                                                horizonThickness_cm_176cm, sandPerc_2cm, sandPerc_7cm , sandPerc_15cm,
                                                sandPerc_25cm , sandPerc_35cm, sandPerc_50cm , sandPerc_70cm, sandPerc_90cm ,
                                                sandPerc_125cm,sandPerc_176cm, soilDepth) {
                                         y <- sum(c(sandPerc_2cm *  horizonThickness_cm_2cm/soilDepth, 
                                                    sandPerc_7cm *    horizonThickness_cm_7cm/soilDepth, 
                                                    sandPerc_15cm *   horizonThickness_cm_15cm/soilDepth, 
                                                    sandPerc_25cm *   horizonThickness_cm_25cm/soilDepth, 
                                                    sandPerc_35cm *   horizonThickness_cm_35cm/soilDepth, 
                                                    sandPerc_50cm *   horizonThickness_cm_50cm/soilDepth, 
                                                    sandPerc_70cm *   horizonThickness_cm_70cm/soilDepth, 
                                                    sandPerc_90cm *   horizonThickness_cm_90cm/soilDepth, 
                                                    sandPerc_125cm *  horizonThickness_cm_125cm/soilDepth, 
                                                    sandPerc_176cm *  horizonThickness_cm_176cm/soilDepth), 
                                                  na.rm = TRUE)/1 
                                         # following weighted average formula here: weighted average = sum(x * weight)/sum(weights)
                                         return(y)
                                       }
    ),
    # Coarse fragments average across depths (avg. weighted by width of layer)
    avgCoarsePerc_acrossDepth = pmap_dbl(.[c("horizonThickness_cm_2cm" , "horizonThickness_cm_7cm" , "horizonThickness_cm_15cm" , 
                                             "horizonThickness_cm_25cm" , "horizonThickness_cm_35cm" , "horizonThickness_cm_50cm" , 
                                             "horizonThickness_cm_70cm" , "horizonThickness_cm_90cm" , "horizonThickness_cm_125cm" , 
                                             "horizonThickness_cm_176cm", "coarsePerc_2cm", "coarsePerc_7cm" , "coarsePerc_15cm",
                                             "coarsePerc_25cm" , "coarsePerc_35cm", "coarsePerc_50cm" , "coarsePerc_70cm", "coarsePerc_90cm" ,
                                             "coarsePerc_125cm","coarsePerc_176cm", "soilDepth")], 
                                         function(horizonThickness_cm_2cm , horizonThickness_cm_7cm , horizonThickness_cm_15cm , 
                                                  horizonThickness_cm_25cm , horizonThickness_cm_35cm , horizonThickness_cm_50cm , 
                                                  horizonThickness_cm_70cm , horizonThickness_cm_90cm , horizonThickness_cm_125cm , 
                                                  horizonThickness_cm_176cm, coarsePerc_2cm, coarsePerc_7cm , coarsePerc_15cm,
                                                  coarsePerc_25cm , coarsePerc_35cm, coarsePerc_50cm , coarsePerc_70cm, coarsePerc_90cm ,
                                                  coarsePerc_125cm,coarsePerc_176cm, soilDepth) {
                                           y <- sum(c(coarsePerc_2cm *  horizonThickness_cm_2cm/soilDepth, 
                                                      coarsePerc_7cm *    horizonThickness_cm_7cm/soilDepth, 
                                                      coarsePerc_15cm *   horizonThickness_cm_15cm/soilDepth, 
                                                      coarsePerc_25cm *   horizonThickness_cm_25cm/soilDepth, 
                                                      coarsePerc_35cm *   horizonThickness_cm_35cm/soilDepth, 
                                                      coarsePerc_50cm *   horizonThickness_cm_50cm/soilDepth, 
                                                      coarsePerc_70cm *   horizonThickness_cm_70cm/soilDepth, 
                                                      coarsePerc_90cm *   horizonThickness_cm_90cm/soilDepth, 
                                                      coarsePerc_125cm *  horizonThickness_cm_125cm/soilDepth, 
                                                      coarsePerc_176cm *  horizonThickness_cm_176cm/soilDepth), 
                                                    na.rm = TRUE)/1 
                                           # following weighted average formula here: weighted average = sum(x * weight)/sum(weights)
                                           return(y)
                                         }
    ), 
    # soil organic carbon average across depths (avg. weighted by width of layer)
    avgOrganicCarbonPerc_0_3cm = organicCarbonPerc_2cm
  )

# # total profile available water-holding capacity
temp <- vegSoils_new %>% 
  mutate(clayPerc_2cm = clayPerc_2cm/100,
         clayPerc_7cm = clayPerc_7cm/100,
         clayPerc_15cm = clayPerc_15cm/100,
         clayPerc_25cm = clayPerc_25cm/100,
         clayPerc_35cm = clayPerc_35cm/100,
         clayPerc_50cm = clayPerc_50cm/100,
         clayPerc_70cm = clayPerc_70cm/100,
         clayPerc_90cm = clayPerc_90cm/100,
         clayPerc_125cm = clayPerc_125cm/100,
         clayPerc_176cm = clayPerc_176cm/100,
         sandPerc_2cm = sandPerc_2cm/100,
         sandPerc_7cm = sandPerc_7cm/100,
         sandPerc_15cm = sandPerc_15cm/100,
         sandPerc_25cm = sandPerc_25cm/100,
         sandPerc_35cm = sandPerc_35cm/100,
         sandPerc_50cm = sandPerc_50cm/100,
         sandPerc_70cm = sandPerc_70cm/100,
         sandPerc_90cm = sandPerc_90cm/100,
         sandPerc_125cm = sandPerc_125cm/100,
         sandPerc_176cm = sandPerc_176cm/100,
         coarsePerc_2cm = coarsePerc_2cm/100,
         coarsePerc_7cm = coarsePerc_7cm/100,
         coarsePerc_15cm = coarsePerc_15cm/100,
         coarsePerc_25cm = coarsePerc_25cm/100,
         coarsePerc_35cm = coarsePerc_35cm/100,
         coarsePerc_50cm = coarsePerc_50cm/100,
         coarsePerc_70cm = coarsePerc_70cm/100,
         coarsePerc_90cm = coarsePerc_90cm/100,
         coarsePerc_125cm = coarsePerc_125cm/100,
         coarsePerc_176cm = coarsePerc_176cm/100) #%>% 
#slice(1:3) 
rm(listOut, listOut_vect, vegSoils_df)
gc()

## save intermediate data
saveRDS(temp, "./Data_processed/WallToWallClimateData/DayMetData_allCONUS_2023ClimateValues_WithSoilsINTERMEDIATE.rds")
temp <- readRDS("./Data_processed/WallToWallClimateData/DayMetData_allCONUS_2023ClimateValues_WithSoilsINTERMEDIATE.rds")

# calculate # # intermediate value 'p' 
# loop through b/c R keeps crashing
# assign IDs to groups
temp$clumpID <- rep_len(1:100, length.out = nrow(temp))
temp$uniqueID <- 1:nrow(temp)
vegSoils_new$uniqueID <- 1:nrow(vegSoils_new)
for (i in 1:100) {
  temp_temp <- temp %>% 
    filter(clumpID == i)
  
  vegSoil_p <- pmap(.l = temp_temp[,c("sandPerc_2cm", "sandPerc_7cm", "sandPerc_15cm", 
                                 "sandPerc_25cm", "sandPerc_35cm", "sandPerc_50cm", 
                                 "sandPerc_70cm", "sandPerc_90cm" ,"sandPerc_125cm", 
                                 "sandPerc_176cm",
                                 "clayPerc_2cm", "clayPerc_7cm" , "clayPerc_15cm", 
                                 "clayPerc_25cm", "clayPerc_35cm", "clayPerc_50cm", 
                                 "clayPerc_70cm", "clayPerc_90cm" ,"clayPerc_125cm", 
                                 "clayPerc_176cm",
                                 "coarsePerc_2cm", "coarsePerc_7cm" , "coarsePerc_15cm", 
                                 "coarsePerc_25cm", "coarsePerc_35cm", "coarsePerc_50cm", 
                                 "coarsePerc_70cm", "coarsePerc_90cm" ,"coarsePerc_125cm", 
                                 "coarsePerc_176cm")], 
                    function (sandPerc_2cm, sandPerc_7cm, sandPerc_15cm, 
                              sandPerc_25cm, sandPerc_35cm, sandPerc_50cm, 
                              sandPerc_70cm, sandPerc_90cm ,sandPerc_125cm, 
                              sandPerc_176cm,
                              clayPerc_2cm, clayPerc_7cm , clayPerc_15cm, 
                              clayPerc_25cm, clayPerc_35cm, clayPerc_50cm, 
                              clayPerc_70cm, clayPerc_90cm ,clayPerc_125cm, 
                              clayPerc_176cm,
                              coarsePerc_2cm, coarsePerc_7cm , coarsePerc_15cm, 
                              coarsePerc_25cm, coarsePerc_35cm, coarsePerc_50cm, 
                              coarsePerc_70cm, coarsePerc_90cm ,coarsePerc_125cm, 
                              coarsePerc_176cm) {
                      p <- rSOILWAT2::ptf_estimate(
                        sand = c(sandPerc_2cm,sandPerc_7cm , sandPerc_15cm,
                                 sandPerc_25cm , sandPerc_35cm, sandPerc_50cm , sandPerc_70cm, sandPerc_90cm ,
                                 sandPerc_125cm,sandPerc_176cm),
                        clay = c(clayPerc_2cm,clayPerc_7cm , clayPerc_15cm,
                                 clayPerc_25cm , clayPerc_35cm, clayPerc_50cm , clayPerc_70cm, clayPerc_90cm ,
                                 clayPerc_125cm,clayPerc_176cm),
                        fcoarse = c(coarsePerc_2cm, coarsePerc_7cm , coarsePerc_15cm,
                                    coarsePerc_25cm , coarsePerc_35cm, coarsePerc_50cm , coarsePerc_70cm, coarsePerc_90cm ,
                                    coarsePerc_125cm,coarsePerc_176cm),
                        swrc_name = "Campbell1974",
                        ptf_name = "Cosby1984"
                      )
                    }
  )

  # calculate intermediate value 'tmp'
  # reference "temp" data frame (which has the raw soil variables), as well as vegSoil_p, a list which has matrices for p calculated above
  vegSoil_tmp <- purrr::map(.x = c(1:nrow(temp_temp)
  ), 
  function (n) {
    print(n)
    tmp <- rSOILWAT2::swrc_swp_to_vwc(
      c(-1.5, -0.033), ##AES should I change this? not totally clear what these values indicate 
      fcoarse = unlist(as.vector(temp_temp[n,c("coarsePerc_2cm" ,                           
                                          "coarsePerc_7cm" ,  "coarsePerc_15cm",                        
                                          "coarsePerc_25cm",  "coarsePerc_35cm",                        
                                          "coarsePerc_50cm",  "coarsePerc_70cm",                        
                                          "coarsePerc_90cm",  "coarsePerc_125cm",                        
                                          "coarsePerc_176cm")])),
      swrc = list(name = "Campbell1974", swrcp = vegSoil_p[[n]])
    )
  }
  )
  
  
  #   # calculate final value 'awc' 
  vegSoil_awc <- purrr::map(.x = c(1:nrow(temp_temp)), 
                            function (n) {
                              awc <- temp_temp[n,c("horizonThickness_cm_2cm"  ,                 
                                              "horizonThickness_cm_7cm"  ,                  "horizonThickness_cm_15cm"    ,              
                                              "horizonThickness_cm_25cm" ,                  "horizonThickness_cm_35cm"    ,              
                                              "horizonThickness_cm_50cm" ,                  "horizonThickness_cm_70cm"    ,              
                                              "horizonThickness_cm_90cm" ,                  "horizonThickness_cm_125cm"   ,              
                                              "horizonThickness_cm_176cm")] * as.vector(diff(vegSoil_tmp[[n]])
                                              )
                              #AES I assume that I sum these values across the entire profile to get "total profile awc"??
                              totAWC <- sum(awc, na.rm = TRUE)
                            }
  )
  
  if (i == 1) {
    outDF <- data.frame("uniqueID" = temp_temp$uniqueID,
               "totalAvailableWaterHoldingCapacity" = unlist(vegSoil_awc)
    )
  } else {
    outDF <- outDF %>% 
      rbind(data.frame("uniqueID" = temp_temp$uniqueID,
                       "totalAvailableWaterHoldingCapacity" = unlist(vegSoil_awc)))
    
  }
}

saveRDS(outDF, "./Data_processed/WallToWallClimateData/DayMetData_allCONUS_2023ClimateValues_WithSoilsINTERMEDIATE_2.rds")
outDF <- readRDS("./Data_processed/WallToWallClimateData/DayMetData_allCONUS_2023ClimateValues_WithSoilsINTERMEDIATE_2.rds")

vegSoils_new <- vegSoils_new %>% 
  left_join(outDF)


# remove unnecessary soils variables 
vegSoils_final <- vegSoils_new %>% 
  select(-c(clayPerc_2cm:organicCarbonPerc_176cm))

# 
# ## drop data points for which there are no climate variables (prior to 2000)
# vegSoils_final <- vegSoils_final %>% 
#   filter(!is.na(durationFrostFreeDays_meanAnnAvg_3yrAnom))
# 
# prepare final dataset
# give each row a unique ID
vegSoils_final <- 
  vegSoils_final %>% 
  mutate(uniqueID = seq(1:nrow(.)))

# give coordinates
test <- vegSoils_final %>% 
  #slice_sample(n = 1000) %>% 
  st_as_sf(coords = c("x_UTM", "y_UTM"), crs = st_crs(temp_rast)) #%>% 
#st_drop_geometry() %>% 
#st_buffer(50) %>% 
# sf::st_join(CAMcoverDat %>% select(CAMCover, Year.x, geometry) %>% st_buffer(100), by = c("Year" = "Year.x"))

#test <- test %>% 
#filter(Year == Year.x)
uniqueID_dups <- duplicated(test$uniqueID)
test <- test[!uniqueID_dups,]

## add back data w/ appropriate Long and Lat info 
#test <- readRDS("./Data_processed/WallToWallClimateData/DayMetData_allCONUS_2023ClimateValues_withSoils.rds")
test2 <- test %>% 
  left_join(vegSoils_df) %>% 
  select(names(test), Long, Lat)
   
# change the geometry to correspond to the original Lat/Long values, not the soilRaster values
test3 <- test2 %>% 
  st_drop_geometry() %>% 
  st_as_sf(coords = c("Long", "Lat"), crs = crs(test2))



# save data
# save for further analysis 
test3 %>% 
  #select(-precip_driestMonth_meanAnnAvg_3yrAnom, -precip_driestMonth_meanAnnAvg_2yrAnom) %>% 
  saveRDS("./Data_processed/WallToWallClimateData/DayMetData_allCONUS_2023ClimateValues_withSoils.rds")

