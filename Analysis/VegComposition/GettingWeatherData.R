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

# get annual climate data -------------------------------------------------

## get annual data downloaded from online
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

## calculating vapor pressure deficit, annual water deficit, and wet degree days (based on code from Adam Noel)
climVar2 <- allMetDat2 %>% 
  # approximation of mean temp (just avg. of max and min, which I realize is not totally accurate)
  mutate(tmean_Jan = (tmax_Jan - tmin_Jan)/2,
         tmean_Feb = (tmax_Feb - tmin_Feb)/2,
         tmean_March = (tmax_March - tmin_March)/2,
         tmean_April = (tmax_April - tmin_April)/2,
         tmean_May = (tmax_May - tmin_May)/2,
         tmean_June = (tmax_June - tmin_June)/2,
         tmean_July = (tmax_July - tmin_July)/2,
         tmean_Aug = (tmax_Aug - tmin_Aug)/2,
         tmean_Sept = (tmax_Sept - tmin_Sept)/2,
         tmean_Oct = (tmax_Oct - tmin_Oct)/2,
         tmean_Nov = (tmax_Nov - tmin_Nov)/2,
         tmean_Dec = (tmax_Dec - tmin_Dec)/2,
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
    VPD_Jan = (6.11*exp((17.27*tmean_Jan)/(237.3 + tmean_Jan))) -  (vp_Jan/100),
    VPD_Feb = (6.11*exp((17.27*tmean_Feb)/(237.3 + tmean_Feb))) -  (vp_Feb/100),
    VPD_March = (6.11*exp((17.27*tmean_March)/(237.3 + tmean_March))) - (vp_March/100),
    VPD_April = (6.11*exp((17.27*tmean_April)/(237.3 + tmean_April)))- (vp_April/100),
    VPD_May =   (6.11*exp((17.27*tmean_May)/(237.3 + tmean_May))) - (vp_May/100),
    VPD_June =  (6.11*exp((17.27*tmean_June)/(237.3 + tmean_June))) - (vp_June/100) ,
    VPD_July =  (6.11*exp((17.27*tmean_July)/(237.3 + tmean_July))) - (vp_July/100),
    VPD_Aug =   (6.11*exp((17.27*tmean_Aug)/(237.3 + tmean_Aug))) -  (vp_Aug/100),
    VPD_Sept =  (6.11*exp((17.27*tmean_Sept)/(237.3 + tmean_Sept)))- (vp_Sept/100),
    VPD_Oct =   (6.11*exp((17.27*tmean_Oct)/(237.3 + tmean_Oct))) - (vp_Oct/100),
    VPD_Nov =   (6.11*exp((17.27*tmean_Nov)/(237.3 + tmean_Nov))) - (vp_Nov/100),
    VPD_Dec =   (6.11*exp((17.27*tmean_Dec)/(237.3 + tmean_Dec))) - (vp_Dec/100) 
  ) %>% 
  #calculate annual values
  transmute(#keep = c("year", "Long", "Lat"),
         # annual water deficit (mm of water over degrees celsius)(sum across all months?)
         annWaterDeficit = pmap_dbl(.[81:92], 
                                    .f = function(awd_Jan, awd_Feb, awd_March, awd_April, awd_May, awd_June, awd_July, awd_Aug, awd_Sept, awd_Oct ,awd_Nov, awd_Dec, ...){
                                      temp <- c(awd_Jan, awd_Feb, awd_March, awd_April, awd_May, awd_June, awd_July, awd_Aug, awd_Sept, awd_Oct ,awd_Nov, awd_Dec)
                                        sum(temp[temp>0])
                                       }
         ),
         # annual wet degree days (temp*days) (sum only positive values)
         annWetDegDays = pmap_dbl(.[93:104],
                                  .f = function(awdd_Jan, awdd_Feb, awdd_March, awdd_April, awdd_May, awdd_June, awdd_July, awdd_Aug, awdd_Sept, awdd_Oct ,awdd_Nov, awdd_Dec, ...) 
                                  {
                                    temp <- c(awdd_Jan, awdd_Feb, awdd_March, awdd_April, awdd_May, awdd_June, awdd_July, awdd_Aug, awdd_Sept, awdd_Oct ,awdd_Nov, awdd_Dec)
                                    sum(temp[temp>0], na.rm = TRUE)
                                  }
                                  ),
         # annual average vapor pressure deficit (in milibars) ()
         annVPD_mean = rowMeans(.[105:116]),
         # annual maximum vapor pressure deficit (in milibars) 
         annVPD_max = pmap_dbl(.[105:116], max),
         # annual maximum vapor pressure deficit (in milibars) 
         annVPD_min = pmap_dbl(.[105:116], min)
  )


climVar <- cbind(climVar, climVar2)
rm(climVar2)


# save for subsequent use
#saveRDS(climVar, file = "./data/dayMet/climateValuesForAnalysis_monthly.rds")
climVar <- readRDS(file="./data/dayMet/climateValuesForAnalysis_monthly.rds")

# calculate sliding window inter-annual climate means ----------------------

## calculate MAP and MAT over past years (a sliding window?)
# function
slidingMetMeans <- function(inDat, start, end) {
  endActual <- end-1 # subtract one so that we're actually looking at the 30, 10, 5, etc. years previous to the "end" year
  outDat <- inDat %>% 
    filter(year %in% c(start:endActual)) %>% 
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
              isothermality_meanAnnAvg = mean(isothermality),
              annWaterDeficit_meanAnnAvg = mean(annWaterDeficit),
              annWetDegDays_meanAnnAvg = mean(annWetDegDays),
              annVPD_mean_meanAnnAvg = mean(annVPD_mean),
              annVPD_max_meanAnnAvg = mean(annVPD_max),
              annVPD_min_meanAnnAvg = mean(annVPD_min)
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

names(annMeans_30yr)[3:22] <- paste0(names(annMeans_30yr)[3:22], "_30yr")

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
names(annMeans_10yr)[3:22] <- paste0(names(annMeans_10yr)[3:22], "_10yr")

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
names(annMeans_5yr)[3:22] <- paste0(names(annMeans_5yr)[3:22], "_5yr")

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
names(annMeans_1yr)[3:22] <- paste0(names(annMeans_1yr)[3:22], "_1yr")

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

          
test2 <- test %>% 
  left_join(annMeans_5yr, by = c("year" = "End_5yr", 
                                                 "Long" = "Long", 
                                                 "Lat" = "Lat"))

test3 <- test2 %>% 
  left_join(annMeans_1yr, by = c("year" = "End_1yr", 
                                 "Long" = "Long", 
                                 "Lat" = "Lat"))

test3 <- test3 %>% 
  select(-c(swe_meanAnn_10yrAnom:isothermality_meanAnnAvg_1yrAnom))

## calculate tmean values
test4 <- test3 %>% 
  mutate(tMean_annAvg = rowMeans(.[c("tmin_annAvg", "tmax_annAvg")], na.rm = TRUE),
         tMean_meanAnnAvg_30yr = rowMeans(.[c("tmin_meanAnnAvg_30yr", "tmax_meanAnnAvg_30yr")], na.rm = TRUE),
         tMean_meanAnnAvg_10yr = rowMeans(.[c("tmin_meanAnnAvg_10yr", "tmax_meanAnnAvg_10yr")], na.rm = TRUE),
         tMean_meanAnnAvg_5yr = rowMeans(.[c("tmin_meanAnnAvg_5yr", "tmax_meanAnnAvg_5yr")], na.rm = TRUE),
         tMean_meanAnnAvg_1yr = rowMeans(.[c("tmin_meanAnnAvg_1yr", "tmax_meanAnnAvg_1yr")], na.rm = TRUE),)
### calculate anomalies 
# i.e. how do the 10 yr. lagged values compare to the 30yr lagged values? 5 yr? previous yr? 
# compare 10 yr values to 30 yr values

anomDat_10yr <- test4 %>% 
transmute(
    # compare 10 yr values to 30 yr values
    # swe as % difference
    swe_meanAnn_10yrAnom = ((swe_meanAnnAvg_30yr - swe_meanAnnAvg_10yr)/swe_meanAnnAvg_30yr),
    # tMean as absolute difference
    tMean_meanAnnAvg_10yrAnom = tMean_meanAnnAvg_30yr - tMean_meanAnnAvg_10yr,
    # tmin as absolute difference
    tmin_meanAnnAvg_10yrAnom = tmin_meanAnnAvg_30yr - tmin_meanAnnAvg_10yr,
    # tmax as absolute difference
    tmax_meanAnnAvg_10yrAnom = tmax_meanAnnAvg_30yr - tmax_meanAnnAvg_10yr,
    # vp as % difference
    vp_meanAnnAvg_10yrAnom = (vp_meanAnnAvg_30yr - vp_meanAnnAvg_10yr)/vp_meanAnnAvg_30yr,
    # prcp as % difference
    prcp_meanAnnTotal_10yrAnom = (prcp_meanAnnTotal_30yr - prcp_meanAnnTotal_10yr)/prcp_meanAnnTotal_30yr,
    # t warmest month as absolute difference
    T_warmestMonth_meanAnnAvg_10yrAnom = T_warmestMonth_meanAnnAvg_30yr - T_warmestMonth_meanAnnAvg_10yr,
    # t coldest month as absolute difference
    T_coldestMonth_meanAnnAvg_10yrAnom = T_coldestMonth_meanAnnAvg_30yr - T_coldestMonth_meanAnnAvg_10yr,
    # precip wettest month as % difference
    precip_wettestMonth_meanAnnAvg_10yrAnom = (precip_wettestMonth_meanAnnAvg_30yr - precip_wettestMonth_meanAnnAvg_10yr)/precip_wettestMonth_meanAnnAvg_30yr,
    # precip driest month as % difference
    precip_driestMonth_meanAnnAvg_10yrAnom = (precip_driestMonth_meanAnnAvg_30yr - precip_driestMonth_meanAnnAvg_10yr)/precip_driestMonth_meanAnnAvg_30yr,
    # precip seasonality as % difference
    precip_Seasonality_meanAnnAvg_10yrAnom = (precip_Seasonality_meanAnnAvg_30yr - precip_Seasonality_meanAnnAvg_10yr)/precip_Seasonality_meanAnnAvg_30yr,
    # precip tempCorr as absolute difference
    PrecipTempCorr_meanAnnAvg_10yrAnom = PrecipTempCorr_meanAnnAvg_30yr - PrecipTempCorr_meanAnnAvg_10yr,
    # above Freezing month as absolute difference
    aboveFreezing_month_meanAnnAvg_10yrAnom = aboveFreezing_month_meanAnnAvg_30yr - aboveFreezing_month_meanAnnAvg_10yr,
    # isothermailty as % difference
    isothermality_meanAnnAvg_10yrAnom = isothermality_meanAnnAvg_30yr - isothermality_meanAnnAvg_10yr,
    # annual water deficit as % difference
    annWaterDeficit_meanAnnAvg_10yrAnom = (annWaterDeficit_meanAnnAvg_30yr - annWaterDeficit_meanAnnAvg_10yr)/annWaterDeficit_meanAnnAvg_30yr,
    # wet degree days as % difference
    annWetDegDays_meanAnnAvg_10yrAnom = (annWetDegDays_meanAnnAvg_30yr - annWetDegDays_meanAnnAvg_10yr)/annWetDegDays_meanAnnAvg_30yr,
    # mean VPD as absolute difference
    annVPD_mean_meanAnnAvg_10yrAnom = (annVPD_mean_meanAnnAvg_30yr - annVPD_mean_meanAnnAvg_10yr),
    # min VPD as absolute difference
    annVPD_min_meanAnnAvg_10yrAnom = (annVPD_min_meanAnnAvg_30yr - annVPD_min_meanAnnAvg_10yr),
    # max VPD as absolute difference
    annVPD_max_meanAnnAvg_10yrAnom = (annVPD_max_meanAnnAvg_30yr - annVPD_max_meanAnnAvg_10yr),
  )

anomDat_5yr <- test4 %>% 
      transmute(
    # compare 5 yr values to 30 yr values
        # swe as % difference
        swe_meanAnn_5yrAnom = ((swe_meanAnnAvg_30yr - swe_meanAnnAvg_5yr)/swe_meanAnnAvg_30yr),
        # tMean as absolute difference
        tMean_meanAnnAvg_5yrAnom = tMean_meanAnnAvg_30yr - tMean_meanAnnAvg_5yr,
        # tmin as absolute difference
        tmin_meanAnnAvg_5yrAnom = tmin_meanAnnAvg_30yr - tmin_meanAnnAvg_5yr,
        # tmax as absolute difference
        tmax_meanAnnAvg_5yrAnom = tmax_meanAnnAvg_30yr - tmax_meanAnnAvg_5yr,
        # vp as % difference
        vp_meanAnnAvg_5yrAnom = (vp_meanAnnAvg_30yr - vp_meanAnnAvg_5yr)/vp_meanAnnAvg_30yr,
        # prcp as % difference
        prcp_meanAnnTotal_5yrAnom = (prcp_meanAnnTotal_30yr - prcp_meanAnnTotal_5yr)/prcp_meanAnnTotal_30yr,
        # t warmest month as absolute difference
        T_warmestMonth_meanAnnAvg_5yrAnom = T_warmestMonth_meanAnnAvg_30yr - T_warmestMonth_meanAnnAvg_5yr,
        # t coldest month as absolute difference
        T_coldestMonth_meanAnnAvg_5yrAnom = T_coldestMonth_meanAnnAvg_30yr - T_coldestMonth_meanAnnAvg_5yr,
        # precip wettest month as % difference
        precip_wettestMonth_meanAnnAvg_5yrAnom = (precip_wettestMonth_meanAnnAvg_30yr - precip_wettestMonth_meanAnnAvg_5yr)/precip_wettestMonth_meanAnnAvg_30yr,
        # precip driest month as % difference
        precip_driestMonth_meanAnnAvg_5yrAnom = (precip_driestMonth_meanAnnAvg_30yr - precip_driestMonth_meanAnnAvg_5yr)/precip_driestMonth_meanAnnAvg_30yr,
        # precip seasonality as % difference
        precip_Seasonality_meanAnnAvg_5yrAnom = (precip_Seasonality_meanAnnAvg_30yr - precip_Seasonality_meanAnnAvg_5yr)/precip_Seasonality_meanAnnAvg_30yr,
        # precip tempCorr as absolute difference
        PrecipTempCorr_meanAnnAvg_5yrAnom = PrecipTempCorr_meanAnnAvg_30yr - PrecipTempCorr_meanAnnAvg_5yr,
        # above Freezing month as absolute difference
        aboveFreezing_month_meanAnnAvg_5yrAnom = aboveFreezing_month_meanAnnAvg_30yr - aboveFreezing_month_meanAnnAvg_5yr,
        # isothermailty as % difference
        isothermality_meanAnnAvg_5yrAnom = isothermality_meanAnnAvg_30yr - isothermality_meanAnnAvg_5yr,    
        # annual water deficit as % difference
        annWaterDeficit_meanAnnAvg_5yrAnom = (annWaterDeficit_meanAnnAvg_30yr - annWaterDeficit_meanAnnAvg_5yr)/annWaterDeficit_meanAnnAvg_30yr,
        # wet degree days as % difference
        annWetDegDays_meanAnnAvg_5yrAnom = (annWetDegDays_meanAnnAvg_30yr - annWetDegDays_meanAnnAvg_5yr)/annWetDegDays_meanAnnAvg_30yr,
        # mean VPD as absolute difference
        annVPD_mean_meanAnnAvg_5yrAnom = (annVPD_mean_meanAnnAvg_30yr - annVPD_mean_meanAnnAvg_5yr),
        # min VPD as absolute difference
        annVPD_min_meanAnnAvg_5yrAnom = (annVPD_min_meanAnnAvg_30yr - annVPD_min_meanAnnAvg_5yr),
        # max VPD as absolute difference
        annVPD_max_meanAnnAvg_5yrAnom = (annVPD_max_meanAnnAvg_30yr - annVPD_max_meanAnnAvg_5yr)
  )


anomDat_1yr_temp <- test4 %>% 
  # remove values for 10 and 5 yr. lags (that aren't anomalies)
  select(-c(prcp_annTotal:isothermality, swe_meanAnnAvg_10yr:isothermality_meanAnnAvg_10yr, swe_meanAnnAvg_5yr:isothermality_meanAnnAvg_5yr))

anomDat_1yr <- anomDat_1yr_temp %>% 
  transmute(
# compare 1 yr values to 30 yr values
    # swe as % difference
    swe_meanAnn_1yrAnom = ((swe_meanAnnAvg_30yr - swe_meanAnnAvg_1yr)/swe_meanAnnAvg_30yr),
    # tMean as absolute difference
    tMean_meanAnnAvg_1yrAnom = tMean_meanAnnAvg_30yr - tMean_meanAnnAvg_1yr,
    # tmin as absolute difference
    tmin_meanAnnAvg_1yrAnom = tmin_meanAnnAvg_30yr - tmin_meanAnnAvg_1yr,
    # tmax as absolute difference
    tmax_meanAnnAvg_1yrAnom = tmax_meanAnnAvg_30yr - tmax_meanAnnAvg_1yr,
    # vp as % difference
    vp_meanAnnAvg_1yrAnom = (vp_meanAnnAvg_30yr - vp_meanAnnAvg_1yr)/vp_meanAnnAvg_30yr,
    # prcp as % difference
    prcp_meanAnnTotal_1yrAnom = (prcp_meanAnnTotal_30yr - prcp_meanAnnTotal_1yr)/prcp_meanAnnTotal_30yr,
    # t warmest month as absolute difference
    T_warmestMonth_meanAnnAvg_1yrAnom = T_warmestMonth_meanAnnAvg_30yr - T_warmestMonth_meanAnnAvg_1yr,
    # t coldest month as absolute difference
    T_coldestMonth_meanAnnAvg_1yrAnom = T_coldestMonth_meanAnnAvg_30yr - T_coldestMonth_meanAnnAvg_1yr,
    # precip wettest month as % difference
    precip_wettestMonth_meanAnnAvg_1yrAnom = (precip_wettestMonth_meanAnnAvg_30yr - precip_wettestMonth_meanAnnAvg_1yr)/precip_wettestMonth_meanAnnAvg_30yr,
    # precip driest month as % difference
    precip_driestMonth_meanAnnAvg_1yrAnom = (precip_driestMonth_meanAnnAvg_30yr - precip_driestMonth_meanAnnAvg_1yr)/precip_driestMonth_meanAnnAvg_30yr,
    # precip seasonality as % difference
    precip_Seasonality_meanAnnAvg_1yrAnom = (precip_Seasonality_meanAnnAvg_30yr - precip_Seasonality_meanAnnAvg_1yr)/precip_Seasonality_meanAnnAvg_30yr,
    # precip tempCorr as absolute difference
    PrecipTempCorr_meanAnnAvg_1yrAnom = PrecipTempCorr_meanAnnAvg_30yr - PrecipTempCorr_meanAnnAvg_1yr,
    # above Freezing month as absolute difference
    aboveFreezing_month_meanAnnAvg_1yrAnom = aboveFreezing_month_meanAnnAvg_30yr - aboveFreezing_month_meanAnnAvg_1yr,
    # isothermailty as % difference
    isothermality_meanAnnAvg_1yrAnom = isothermality_meanAnnAvg_30yr - isothermality_meanAnnAvg_1yr,
    # annual water deficit as % difference
    annWaterDeficit_meanAnnAvg_1yrAnom = (annWaterDeficit_meanAnnAvg_30yr - annWaterDeficit_meanAnnAvg_1yr)/annWaterDeficit_meanAnnAvg_30yr,
    # wet degree days as % difference
    annWetDegDays_meanAnnAvg_1yrAnom = (annWetDegDays_meanAnnAvg_30yr - annWetDegDays_meanAnnAvg_1yr)/annWetDegDays_meanAnnAvg_30yr,
    # mean VPD as absolute difference
    annVPD_mean_meanAnnAvg_1yrAnom = (annVPD_mean_meanAnnAvg_30yr - annVPD_mean_meanAnnAvg_1yr),
    # min VPD as absolute difference
    annVPD_min_meanAnnAvg_1yrAnom = (annVPD_min_meanAnnAvg_30yr - annVPD_min_meanAnnAvg_1yr),
    # max VPD as absolute difference
    annVPD_max_meanAnnAvg_1yrAnom = (annVPD_max_meanAnnAvg_30yr - annVPD_max_meanAnnAvg_1yr))

climDat <- cbind(test3, anomDat_10yr, anomDat_5yr, anomDat_1yr)
# save climate values for analysis 
#write.csv(climDat, "./data/dayMet/climateValuesForAnalysis_final.csv", row.names = FALSE)
#saveRDS(climDat, "./data/dayMet/climateValuesForAnalysis_final.rds")

#plot MAP and MAT for sites for most recent 30-year period
plotDat <- annMeans_30yr %>% 
  filter(End_30yr == 2022)

ggplot(plotDat) +
  geom_point(aes(y = prcp_meanAnnTotal_30yr, x = tmin_meanAnnAvg_30yr))

ggplot(plotDat) +
  geom_point(aes(x = Long, y = Lat, col = tmax_meanAnnAvg_30yr))
