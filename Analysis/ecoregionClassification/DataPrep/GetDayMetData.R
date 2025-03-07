#///////////////////
# Getting weather data for ecoregion classification models
# Alice Stears
# 12/12/24
#///////////////////


# load packages -----------------------------------------------------------

library(tidyverse)
library(sf)
library(daymetr)
library(terra)

# Load data ---------------------------------------------------------------

## get veg data to get locations (are some duplicated locates b/c plots have data from multiple years)
#vegDat <- read.csv("./Data_processed/CoverData/DataForAnalysis.csv") #%>%
#   dplyr::select(Lat, Lon) %>%
#   unique()
# points <- vegDat %>%
#   dplyr::select(UniqueID, Lat, Lon) %>%
#   unique()
# write.csv(points, "./data/dayMet/dayMetPoints.csv", row.names = FALSE)
#years <- unique(vegDat$Year)
# get sf point data
#points_sf <- st_read(dsn = "./Data_processed/CoverData/DataForAnalysisPoints/", layer = "vegCompPoints")
# load the CONUS extent 
CONUS <- st_read("./Data_raw/CONUS_extent/", layer = "CONUS_boundary")
## get monthly data downloaded from online
# get point centroids
test <- terra::rast("./Data_raw/dayMet/rawMonthlyData/orders/70e0da02b9d2d6e8faa8c97d211f3546/Daymet_Monthly_V4R1/data/daymet_v4_prcp_monttl_na_1980.tif")
# change the crs of conus boundary shapefile
CONUS <- CONUS %>% 
  st_transform(crs(test)) %>% 
  vect()
  
points_sf <- test %>% 
  terra::crop(CONUS) %>% 
  terra::mask(CONUS) %>% 
  terra::crds() %>% 
  data.frame() %>% 
  slice_sample(n = 800000) %>% 
  terra::vect(geom = c("x", "y"))

rastNames <- list.files("./Data_raw/dayMet/rawMonthlyData/orders/70e0da02b9d2d6e8faa8c97d211f3546/Daymet_Monthly_V4R1/data/")


# load values if they aren't already loaded 
if(sum(list.files("./Data_raw/dayMet/") == "ECOREGIONsampledDataForAnalysis.csv") <1) {
  # load monthly total precip values and make into a raster stack
  for (i in 1:32#length(rastNames[str_detect(string = rastNames,
       #pattern = "prcp_monttl_na_.....tif$")])
  ){
    
    name_i <- rastNames[str_detect(string = rastNames,
                                   pattern = "prcp_monttl_na_.....tif$")][i]
    temp_rast <- rast(paste0("./Data_raw/dayMet/rawMonthlyData/orders/70e0da02b9d2d6e8faa8c97d211f3546/Daymet_Monthly_V4R1/data/", name_i))
    
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
      dplyr::select(year, prcp_Jan:prcp_Dec) %>%
      cbind(terra::crds(points_sf)) %>%
      rename(Long = x, Lat = y)
    
    if (i == 1 ){
      prcpPoints <- temp_points
    } else {
      prcpPoints <- rbind(prcpPoints, temp_points)
    }
  }
  
  # load monthly average tmax values and make into a raster stack
  for (i in 1:32#length(rastNames[str_detect(string = rastNames, pattern = "tmax_monavg_na_.....tif$")])
  ){
    
    name_i <- rastNames[str_detect(string = rastNames,
                                   pattern = "tmax_monavg_na_.....tif$")][i]
    temp_rast <- rast(paste0("./Data_raw/dayMet/rawMonthlyData/orders/70e0da02b9d2d6e8faa8c97d211f3546/Daymet_Monthly_V4R1/data/", name_i))
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
      dplyr::select(year, tmax_Jan:tmax_Dec) %>%
      cbind(terra::crds(points_sf)) %>%
      rename(Long = x, Lat = y)
    
    if (i == 1 ){
      tmaxPoints <- temp_points
    } else {
      tmaxPoints <- rbind(tmaxPoints, temp_points)
    }
  }
  
  # load monthly average tmin values and make into a raster stack
  for (i in 1:32#length(rastNames[str_detect(string = rastNames, pattern = "tmin_monavg_na_.....tif$")])
  ){
    
    name_i <- rastNames[str_detect(string = rastNames,
                                   pattern = "tmin_monavg_na_.....tif$")][i]
    temp_rast <- rast(paste0("./Data_raw/dayMet/rawMonthlyData/orders/70e0da02b9d2d6e8faa8c97d211f3546/Daymet_Monthly_V4R1/data/", name_i))
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
      dplyr::select(year, tmin_Jan:tmin_Dec) %>%
      cbind(terra::crds(points_sf)) %>%
      rename(Long = x, Lat = y)
    
    if (i == 1 ){
      tminPoints <- temp_points
    } else {
      tminPoints <- rbind(tminPoints, temp_points)
    }
  }
  
  ## add all variables together
  allMetDat <- tmaxPoints %>%
    left_join(tminPoints) %>%
    left_join(prcpPoints)
  
  # save data
  #write.csv(allMetDat, file = "./data/dayMet/sampledDataForAnalysis.csv", row.names = FALSE)
  saveRDS(allMetDat, "./Data_raw/dayMet/ECOREGIONsampledDataForAnalysis.csv")
  
} else (
  allMetDat <- readRDS("./Data_raw/dayMet/ECOREGIONsampledDataForAnalysis.csv")
)
# 
# 
# ggplot(allMetDat %>% slice_sample(n = 500000)) + 
#   geom_point(aes(x = Long, y = Lat))
# get annual climate data -------------------------------------------------

# get annual data downloaded from online
#test <- terra::rast("./data/dayMet/rawMonthlyData/orders/70e0da02b9d2d6e8faa8c97d211f3546/Daymet_Monthly_V4R1/data/daymet_v4_prcp_monttl_na_1980.tif")
rastNames2 <- list.files("./Data_raw/dayMet/yearly/")

 if (sum(list.files("./Data_raw/dayMet/") == "ECOREGIONsampledDataForAnalysis_Annual.rds") <1 ) {# load annual total precip values and make into a raster stack
for (i in 1:32#length(rastNames2[str_detect(string = rastNames2, pattern = "prcp_annttl_na_.....tif$")])
     ){

  name_i <- rastNames2[str_detect(string = rastNames2,
                                  pattern = "prcp_annttl_na_.....tif$")][i]
  temp_rast <- rast(paste0("./Data_raw/dayMet/yearly/", name_i))

  # get the data for the locations we want
  temp_points <-
    temp_rast %>%
    terra::extract(points_sf)

  # make column for year and change column names to month value only
  temp_points$year <- as.numeric(str_extract(names(temp_points)[2], pattern = "\\d{4}"))
  names(temp_points)[2] <- c("prcp_annTotal")
  temp_points <- temp_points %>%
    dplyr::select(year, prcp_annTotal) %>%
    cbind(terra::crds(points_sf)) %>%
    rename(Long = x, Lat = y)

  if (i == 1 ){
    prcpPoints_ann <- temp_points
  } else {
    prcpPoints_ann <- rbind(prcpPoints_ann, temp_points)
  }
}

# load annual tmax ann avg values and make into a raster stack
for (i in 1:32#length(rastNames2[str_detect(string = rastNames2, pattern = "tmax_annavg_na_.....tif$")])
     ){

  name_i <- rastNames2[str_detect(string = rastNames2,
                                  pattern = "tmax_annavg_na_.....tif$")][i]
  temp_rast <- rast(paste0("./Data_raw//dayMet/yearly/", name_i))

  # get the data for the locations we want
  temp_points <-
    temp_rast %>%
    terra::extract(points_sf)

  # make column for year and change column names to month value only
  temp_points$year <- as.numeric(str_extract(names(temp_points)[2], pattern = "\\d{4}"))
  names(temp_points)[2] <- c("tmax_annAvg")
  temp_points <- temp_points %>%
    dplyr::select(year, tmax_annAvg) %>%
    cbind(terra::crds(points_sf)) %>%
    rename(Long = x, Lat = y)

  if (i == 1 ){
    tmaxPoints_ann <- temp_points
  } else {
    tmaxPoints_ann <- rbind(tmaxPoints_ann, temp_points)
  }
}

# load annual tmin ann avg values and make into a raster stack
for (i in 1:32#length(rastNames2[str_detect(string = rastNames2,  pattern = "tmin_annavg_na_.....tif$")])
     ){

  name_i <- rastNames2[str_detect(string = rastNames2,
                                  pattern = "tmin_annavg_na_.....tif$")][i]
  temp_rast <- rast(paste0("./Data_raw//dayMet/yearly/", name_i))

  # get the data for the locations we want
  temp_points <-
    temp_rast %>%
    terra::extract(points_sf)

  # make column for year and change column names to month value only
  temp_points$year <- as.numeric(str_extract(names(temp_points)[2], pattern = "\\d{4}"))
  names(temp_points)[2] <- c("tmin_annAvg")
  temp_points <- temp_points %>%
    dplyr::select(year, tmin_annAvg) %>%
    cbind(terra::crds(points_sf)) %>%
    rename(Long = x, Lat = y)

  if (i == 1 ){
    tminPoints_ann <- temp_points
  } else {
    tminPoints_ann <- rbind(tminPoints_ann, temp_points)
  }
}

# join together
annMetDat <- prcpPoints_ann %>%
  #left_join(swePoints_ann) %>%
  left_join(tminPoints_ann) %>%
  left_join(tmaxPoints_ann) #%>%
  #left_join(vpPoints_ann)

# save data
saveRDS(annMetDat, file = "./Data_raw/dayMet/ECOREGIONsampledDataForAnalysis_Annual.rds")
 } else {
   annMetDat <- readRDS("./Data_raw/dayMet/ECOREGIONsampledDataForAnalysis_Annual.rds")
}

# add annual data to the monthly data (will use later in processing)
allMetDat2 <- allMetDat %>% 
  left_join(annMetDat)

# calculating climate variables for models -------------------------------
climVar <- allMetDat2 %>% 
  #slice_sample(n = 10) %>% 
  mutate(totalAnnPrecip = rowSums(.[c("prcp_Jan", "prcp_Feb", "prcp_March", 
                                      "prcp_April", "prcp_May", "prcp_June", 
                                      "prcp_July", "prcp_Aug", "prcp_Sept", 
                                      "prcp_Oct" ,"prcp_Nov", "prcp_Dec")]), # total annual precipitation
         #maxAnnSwe = rowSums(.[28:39]), # total annual swe
         T_warmestMonth = pmap_dbl(.[c("tmax_Jan" , "tmax_Feb",  "tmax_March","tmax_April", "tmax_May",     
                                     "tmax_June", "tmax_July",  "tmax_Aug", "tmax_Sept" , "tmax_Oct", "tmax_Nov",     
                                      "tmax_Dec")], max), # temperature of warmest month
         T_coldestMonth = pmap_dbl(.[c("tmin_Jan" , "tmin_Feb",  "tmin_March","tmin_April", "tmin_May",     
                                       "tmin_June", "tmin_July",  "tmin_Aug", "tmin_Sept" , "tmin_Oct", "tmin_Nov",     
                                       "tmin_Dec")], min), # temperature of coldest month
         Tmin_annAvgOfMonthly = rowSums(.[c("tmin_Jan" , "tmin_Feb",  "tmin_March","tmin_April", "tmin_May",     
                                            "tmin_June", "tmin_July",  "tmin_Aug", "tmin_Sept" , "tmin_Oct", "tmin_Nov",     
                                            "tmin_Dec")])/12,
         Tmax_annAvgOfMonthly = rowSums(.[c("tmax_Jan" , "tmax_Feb",  "tmax_March","tmax_April", "tmax_May",     
                                            "tmax_June", "tmax_July",  "tmax_Aug", "tmax_Sept" , "tmax_Oct", "tmax_Nov",     
                                            "tmax_Dec")])/12,
         precip_wettestMonth = pmap_dbl(.[c("prcp_Jan", "prcp_Feb", "prcp_March", 
                                            "prcp_April", "prcp_May", "prcp_June", 
                                            "prcp_July", "prcp_Aug", "prcp_Sept", 
                                            "prcp_Oct" ,"prcp_Nov", "prcp_Dec")], max), # precip of wettest month
         precip_driestMonth = pmap_dbl(.[c("prcp_Jan", "prcp_Feb", "prcp_March", 
                                           "prcp_April", "prcp_May", "prcp_June", 
                                           "prcp_July", "prcp_Aug", "prcp_Sept", 
                                           "prcp_Oct" ,"prcp_Nov", "prcp_Dec")], min), # precip of driest month
         precip_Seasonality = pmap_dbl(.[c("prcp_Jan", "prcp_Feb", "prcp_March", 
                                           "prcp_April", "prcp_May", "prcp_June", 
                                           "prcp_July", "prcp_Aug", "prcp_Sept", 
                                           "prcp_Oct" ,"prcp_Nov", "prcp_Dec")],   # coefficient of variation (sd/mean) of precipitation
                                       .f = function(prcp_Jan, prcp_Feb, prcp_March, prcp_April, prcp_May, prcp_June, prcp_July, prcp_Aug, prcp_Sept, prcp_Oct ,prcp_Nov, prcp_Dec, ...) 
                                       {temp <- c(prcp_Jan, prcp_Feb, prcp_March, prcp_April, prcp_May, prcp_June, prcp_July, prcp_Aug, prcp_Sept, prcp_Oct ,prcp_Nov, prcp_Dec)
                                      ( sd(temp)/mean(temp))*100
                                       }
         ),
         PrecipTempCorr = pmap_dbl(.[c(c("tmax_Jan" , "tmax_Feb",  "tmax_March","tmax_April", "tmax_May",     
                                         "tmax_June", "tmax_July",  "tmax_Aug", "tmax_Sept" , "tmax_Oct", "tmax_Nov",     
                                         "tmax_Dec", "prcp_Jan", "prcp_Feb", "prcp_March", 
                                              "prcp_April", "prcp_May", "prcp_June", 
                                              "prcp_July", "prcp_Aug", "prcp_Sept", 
                                              "prcp_Oct" ,"prcp_Nov", "prcp_Dec"))], #correlation of monthly temp and precip
                                   .f = function(tmax_Jan, tmax_Feb, tmax_March, tmax_April, tmax_May, tmax_June, tmax_July, tmax_Aug, tmax_Sept, tmax_Oct,  tmax_Nov,  tmax_Dec,
                                                 prcp_Jan, prcp_Feb, prcp_March, prcp_April, prcp_May, prcp_June, prcp_July, prcp_Aug, prcp_Sept, prcp_Oct ,prcp_Nov, prcp_Dec, ...) {
                                     cor(y = c(tmax_Jan, tmax_Feb, tmax_March, tmax_April, tmax_May, tmax_June, tmax_July, tmax_Aug, tmax_Sept, tmax_Oct,  tmax_Nov,  tmax_Dec), 
                                         x = c(prcp_Jan, prcp_Feb, prcp_March, prcp_April, prcp_May, prcp_June, prcp_July, prcp_Aug, prcp_Sept, prcp_Oct ,prcp_Nov, prcp_Dec))
                                   }),
         aboveFreezing_month = pmap_dbl(.[c("tmin_Jan" , "tmin_Feb",  "tmin_March","tmin_April", "tmin_May",     
                                            "tmin_June", "tmin_July",  "tmin_Aug", "tmin_Sept" , "tmin_Oct", "tmin_Nov",     
                                            "tmin_Dec")], # month when temp gets above freezing (when tmin > 0 degrees C, so no freeze at night )
                                        .f = function(tmin_Jan, tmin_Feb, tmin_March, tmin_April, tmin_May, tmin_June, tmin_July, tmin_Aug, tmin_Sept, tmin_Oct,  tmin_Nov,  tmin_Dec) {
                                          temp <- c(tmin_Jan, tmin_Feb, tmin_March, tmin_April, tmin_May, tmin_June, tmin_July, tmin_Aug, tmin_Sept, tmin_Oct,  tmin_Nov,  tmin_Dec)
                                          which(temp > 0)[1] # in degrees C
                                        }),
         lastAboveFreezing_month = pmap_dbl(.[c("tmin_Jan" , "tmin_Feb",  "tmin_March","tmin_April", "tmin_May",     
                                                "tmin_June", "tmin_July",  "tmin_Aug", "tmin_Sept" , "tmin_Oct", "tmin_Nov",     
                                                "tmin_Dec")], # month when temp gets above freezing (when tmin > 0 degrees C, so no freeze at night )
                                            .f = function(tmin_Jan, tmin_Feb, tmin_March, tmin_April, tmin_May, tmin_June, tmin_July, tmin_Aug, tmin_Sept, tmin_Oct,  tmin_Nov,  tmin_Dec) {
                                              temp <- c(tmin_Jan, tmin_Feb, tmin_March, tmin_April, tmin_May, tmin_June, tmin_July, tmin_Aug, tmin_Sept, tmin_Oct,  tmin_Nov,  tmin_Dec)
                                              temp2 <- which(temp > 0) # in degrees C
                                              if(length(temp2)>0) {
                                                return(max(temp2))
                                              } else {
                                                return(NA)
                                              }
                                            }),
         
         isothermality = pmap_dbl(.[c(c("tmax_Jan" , "tmax_Feb",  "tmax_March","tmax_April", "tmax_May",     
                                        "tmax_June", "tmax_July",  "tmax_Aug", "tmax_Sept" , "tmax_Oct", "tmax_Nov",     
                                        "tmax_Dec", 
                                        "tmin_Jan" , "tmin_Feb",  "tmin_March","tmin_April", "tmin_May",     
                                             "tmin_June", "tmin_July",  "tmin_Aug", "tmin_Sept" , "tmin_Oct", "tmin_Nov",     
                                             "tmin_Dec"))], # isothermality
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

# # multiply the precip seasonality by 100 (b/c I guess I calculated it wrong)
# climVar <- climVar %>% 
#   mutate(precip_Seasonality = precip_Seasonality/100)


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
saveRDS(climVar, file = "./Data_raw/dayMet/ECOREGIONclimateValuesForAnalysis_monthly.rds")
climVar <- readRDS(file="./Data_raw/dayMet/ECOREGIONclimateValuesForAnalysis_monthly.rds")

rm(allMetDat, allMetDat2, annMetDat)
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

#testDat <- climVar[runif(10000,min=0, max = nrow(climVar)),]
# for last 30-year window
endDats <- as.matrix(c(2011))
annMeans <- apply(as.matrix(endDats[1]), MARGIN = 1, FUN = function(x)
  slidingMetMeans(inDat = climVar
                  , start = as.numeric(x-30), end = as.numeric(x))
)

# put together into one list
annMeans_all <- c(annMeans)
names(annMeans_all) <- c(2011)
annMeans_30yr <- lapply(endDats, function(x) {
  temp <- cbind(annMeans_all[[as.character(x)]], x)
  temp$Start <- x-30
  names(temp) <- c(names(annMeans_all[[1]]), "End", "Start")
  return(temp)
})

annMeans_30yr <- data.table::rbindlist(annMeans_30yr)

names(annMeans_30yr)[3:26] <- paste0(names(annMeans_30yr)[3:26], "_30yr")
saveRDS(annMeans_30yr, "./Data_processed/CoverData/dayMet_intermediate/ECOREGIONSannMeans_30yrs.rds")
annMeans_30yr <- readRDS("./Data_processed/CoverData/dayMet_intermediate/ECOREGIONSannMeans_30yrs.rds")

## add lagged data to the main climate value data.frame
test <- climVar %>% 
  dplyr::select(-c(tmax_Jan:tmax_Dec, tmin_Jan:prcp_Dec)) %>% 
  #filter(year == 2020) %>% 
  #slice(1:100) %>% 
  left_join(annMeans_30yr, by = c("year" = "End_30yr", 
                                  "Long" = "Long", 
                                  "Lat" = "Lat"))

rm(annMeans_30yr, annMeans_10yr)
gc()

# save intermediate data 
saveRDS(test, "./Data_processed/CoverData/dayMet_intermediate/ECOREGIONSclimVars_AnnualMeansAndLaggedValues.rds")
test <- readRDS("./Data_processed/CoverData/dayMet_intermediate/ECOREGIONSclimVars_AnnualMeansAndLaggedValues.rds")
# for those years including and after 2010, use the averages over the previous
# 30 years for climate. for those years before 2010, use the averages over any
# previous years starting w/ 1980

