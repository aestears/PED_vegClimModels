#///////////////////
# Averaging values from plots w/in 
# Alice Stears
# 8/19/24
# code for acquiring fire perimiters and overlaying them on AIM plots adapted from G. Siegmund 
#///////////////////


# Load packages -----------------------------------------------------------

library(tidyverse)
library(sf)
library(terra)
library(exactextractr)

# Load data ---------------------------------------------------------------
dat <- readRDS("./Data_processed/DataForModels.rds") %>% 
  st_drop_geometry()
# dayMet extent 
test <-  rast("./Data_raw/dayMet/rawMonthlyData/orders/70e0da02b9d2d6e8faa8c97d211f3546/Daymet_Monthly_V4R1/data/daymet_v4_prcp_monttl_na_1980.tif")

# rasterize values --------------------------------------------------------
dat2 <- terra::vect(dat, geom = c("Lon", "Lat"), crs = terra::crs(test))

# get the names of the columns w/ data we want to rasterize
layerNames <- dat%>% 
  st_drop_geometry() %>% 
  select(c(ShrubCover, TotalTreeCover, TotalHerbaceousCover, CAMCover, BareGroundCover, ForbCover_prop:NeedleLeavedTreeCover_prop)) %>% 
  names() 


# fdat# function to rasterize and average cover values
test2_a <- lapply(layerNames[1:2], FUN = function(x) {
  terra::rasterize(dat2, y = test, field = x, fun = mean, na.rm = TRUE#function(x) mean(x, na.rm = TRUE)
                     , by = "Year")
}
)

test2_b <- lapply(layerNames[3:4], FUN = function(x) {
  terra::rasterize(dat2, y = test, field = x, fun = mean, na.rm = TRUE#function(x) mean(x, na.rm = TRUE)
                   , by = "Year")
}
)

test2_ab <- c(test2_a, test2_b)
rm(test2_a, test2_b)

test2_c <- lapply(layerNames[5:6], FUN = function(x) {
  terra::rasterize(dat2, y = test, field = x, fun = mean, na.rm = TRUE#function(x) mean(x, na.rm = TRUE)
                   , by = "Year")
}
)
test2_abc <- c(test2_ab, test2_c)
rm(test2_ab, test2_c)

test2_d <- lapply(layerNames[7], FUN = function(x) {
  terra::rasterize(dat2, y = test, field = x, fun = mean, na.rm = TRUE#function(x) mean(x, na.rm = TRUE)
                   , by = "Year")
}
)
test2_abcd <- c(test2_abc, test2_d)
rm(test2_abc, test2_d)

test2_e <- lapply(layerNames[8], FUN = function(x) {
  terra::rasterize(dat2, y = test, field = x, fun = mean, na.rm = TRUE#function(x) mean(x, na.rm = TRUE)
                   , by = "Year")
}
)

test2_abcde <- c(test2_abcd, test2_e)
rm(test2_abcd, test2_e)
gc()

test2_f <- lapply(layerNames[9], FUN = function(x) {
  terra::rasterize(dat2, y = test, field = x, fun = mean, na.rm = TRUE#function(x) mean(x, na.rm = TRUE)
                   , by = "Year")
}
)

test2_abcdef <- c(test2_abcde, test2_f)
rm(test2_abcde, test2_f)
gc()

test2_g <- lapply(layerNames[10], FUN = function(x) {
  terra::rasterize(dat2, y = test, field = x, fun = mean, na.rm = TRUE#function(x) mean(x, na.rm = TRUE)
                   , by = "Year")
}
)

test2 <- c(test2_abcdef, test2_g)
rm(test2_abcdef, test2_g)
gc()

names(test2) <- layerNames

saveRDS(test2, "./Data_processed/spatialSamplesInt_1.rds")



# ExtractValuesBackToPoints -----------------------------------------------

centroidPoints <- xyFromCell(test2[[2]], cell = 1:ncell(test2[[2]])) %>% 
  as.data.frame() %>% 
  st_as_sf(coords = c("x", "y")) %>% 
  vect()
  
names(test2$NeedleLeavedTreeCover_prop)[44] <- "test"
# extract the points from the raster back to the data.frame 
test3 <- lapply(names(test2), FUN = function(y) {
  print(y)
  temp <- lapply(1:length(unique(dat$Year)), FUN = function(x) {
    
    # is the raster for this data/year all NAs? 
    if (any(as.numeric(terra::global(test2[[y]][[x]], fun = "notNA")[,1]) > 0)) { # if there is data
      print(x)
       layer_i <- test2[[y]][[x]] %>% 
        terra::extract(y = centroidPoints, xy = TRUE) %>% # get data just for the points
        filter(!is.na(.data[[names(test2[[y]])[x]]])) 
      layer_i$Year <- sort(unique(dat$Year))[x]
      return(layer_i)
    } else {
      message(paste0("year ", sort(unique(dat$Year))[[x]], " (index ", x,") has no data"))
    }
    
  }) %>% 
    list_rbind 
  
  temp2 <- temp %>% 
    transmute(value = base::rowSums(temp %>% select(any_of(names(test2[[1]]))), na.rm = TRUE), 
              ID = ID, 
              x = x,
              y = y, 
              Year = Year)
}
)
  
names(test3) <- layerNames
# save output! 
saveRDS(test3, "./Data_processed/spatiallyAverageData_intermediate.rds")
#test3 <- readRDS("./Data_processed/spatiallyAverageData_intermediate.rds")

# put together into a data.frame 
test4 <- lapply(layerNames, function(x) {
  temp <- test3[[x]] %>% 
    select(-ID) 
  names(temp)[1] <- x
  return(temp)
}) 
test5 <- test4[[1]] %>% 
  full_join(test4[[2]], by = c("x", "y", "Year")) %>% 
  full_join(test4[[3]], by = c("x", "y", "Year")) %>% 
  full_join(test4[[4]], by = c("x", "y", "Year")) %>% 
  full_join(test4[[5]], by = c("x", "y", "Year")) %>% 
  full_join(test4[[6]], by = c("x", "y", "Year")) %>% 
  full_join(test4[[7]], by = c("x", "y", "Year")) %>% 
  full_join(test4[[8]], by = c("x", "y", "Year")) %>% 
  full_join(test4[[9]], by = c("x", "y", "Year")) %>% 
  full_join(test4[[10]], by = c("x", "y", "Year"))
  

rm(test3, test4)
gc()


# add back in climate data  -----------------------------------------------

climDat <- readRDS( "./Data_processed/dayMetClimateValuesForAnalysis_final.rds")
## assign a 'unique ID' to each location (So the same location has the same ID across years)
uniqueLocs <- unique(climDat[,c("Lat", "Long")])
uniqueLocs$locID <- seq(from = 1, to = nrow(uniqueLocs), by = 1)
climDat  <- climDat %>% 
  left_join(uniqueLocs)

# make points for "locID" in a single year into a raster
climSF <- climDat %>% 
  dplyr::filter(year == as.integer(2011)) %>% 
  sf::st_as_sf(coords = c("Long", "Lat"), crs = st_crs(test)) %>% 
  select(locID)


#allDat_avg <- 
  test7 <- test5 %>% 
  rename(Lon = x, Lat = y) %>% 
  sf::st_as_sf(coords = c("Lon", "Lat"), crs = st_crs(test)) %>%
  st_buffer(400) %>% 
  sf::st_join(climSF, join = st_nearest_feature)
  
# add back in all climate data based on the "locID"
allDat_avg <- test7 %>% 
  left_join(climDat, by = c("locID", "Year" = "year"))

## save the data
saveRDS(allDat_avg, "./Data_processed/DataForModels_spatiallyAveraged_sf.rds")
saveRDS(st_drop_geometry(allDat_avg), "./Data_processed/DataForModels_spatiallyAveraged_NoSf.rds")

# Testing -----------------------------------------------------------------
  # determine if the results are accurate 
test6 <- vect(test5, geom = c("x", "y"), crs = crs(test))
temp <- terra::rasterize(test6, y = test, field = "ShrubCover", fun = function(x) mean(x, na.rm = TRUE)) %>% 
  terra::aggregate(fact = 32, fun = function(x) mean(x, na.rm = TRUE))
  

  # compare to raw data 
  
temp2 <- 
  rasterize(dat2, y = test, field = "ShrubCover", fun = function(x) mean(x, na.rm = TRUE)) %>% 
  terra::aggregate(fact = 32, fun = function(x) mean(x, na.rm = TRUE) )


par(mfrow = c(1,2))
plot(temp, 
     xlim = c(-2000000, 2500000), 
     ylim = c(-2000000, 1000000))
plot(temp2, 
     xlim = c(-2000000, 2500000), 
     ylim = c(-2000000, 1000000))

## Look the same! 

