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
dat <- readRDS("./data/DataForModels.rds")
# dayMet extent 
test <-  rast("./data/dayMet/rawMonthlyData/orders/70e0da02b9d2d6e8faa8c97d211f3546/Daymet_Monthly_V4R1/data/daymet_v4_prcp_monttl_na_1980.tif")

# get unique clim data
clim <- dat %>% 
  select(Lat, Lon, Year, annVPD_mean:annVPD_max_meanAnnAvg_1yrAnom) %>% 
  unique()
# rasterize values --------------------------------------------------------
dat2 <- vect(dat, geom = c("Lon", "Lat"), crs = crs(test))

# get the names of the columns w/ data we want to rasterize
layerNames <- dat %>% 
  select(ShrubCover_dec:LitterCover_dec) %>% 
  names()

# function to rasterize and average cover values
test2 <- lapply(layerNames, FUN = function(x) {
  rasterize(dat2, y = test, field = x, fun = function(x) mean(x, na.rm = TRUE), by = "Year")
}
)
names(test2) <- layerNames

# ExtractValuesBackToPoints -----------------------------------------------

centroidPoints <- xyFromCell(test2[[2]], cell = 1:ncell(test2[[2]])) %>% 
  as.data.frame() %>% 
  st_as_sf(coords = c("x", "y")) %>% 
  vect()
  
# extract the points from the raster back to the data.frame 
test3 <- lapply(names(test2), FUN = function(y) {
  print(y)
  temp <- lapply(1:length(unique(dat$Year)), FUN = function(x) {
    
    # is the raster for this data/year all NAs? 
    if (any(as.numeric(terra::global(test2[[y]][[x]], fun = "notNA")[,1]) > 0)) { # if there is data
      print(x)
       layer_i <- test2[[y]][[x]] %>% 
        terra::extract(y = centroidPoints, xy = TRUE) %>% 
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
#saveRDS(test3, "./data/spatiallyAverageData_intermediate.rds")
test3 <- readRDS("./data/spatiallyAverageData_intermediate.rds")
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
  full_join(test4[[10]], by = c("x", "y", "Year")) %>% 
  full_join(test4[[11]], by = c("x", "y", "Year")) %>% 
  full_join(test4[[12]], by = c("x", "y", "Year")) 
  

## add back in climate data 
allDat_avg <- test5 %>% 
  rename(Lon = x, Lat = y) %>% 
  sf::st_as_sf(coords = c("Lon", "Lat"), crs = st_crs(test)) %>% 
  st_buffer(400) %>% 
  sf::st_join(clim %>% sf::st_as_sf(coords = c("Lon", "Lat"), crs = st_crs(test)) %>% sf::st_buffer(1000), by = c("Year")) %>% 
  filter(Year.x == Year.y)
##AES need to figure out how to do this better... maybe rasterize both layers, add together, and then extract points again?? 


plot(test$daymet_v4_prcp_monttl_na_1980_1)
points(clim[1:1000,"Lon"], clim[1:1000, "Lat"], add = TRUE, col = "orange")
points(test5$x, test5$y, col = "green")


ggplot() +
  geom_point(data = clim, aes(x = Lon, y = Lat), col = "red") +
  geom_point(data = test6[!is.na(test6$AngioTreeCover_dec),], aes(x = Lon, y = Lat, col = AngioTreeCover_dec), alpha = .5) 
  
# %>% 
#   list_rbind() %>% 
#   select(-ID) %>% 
#   pivot_wider(names_from = Variable, 
#               values_from = value)


  # determine if the results are accurate 
test6 <- vect(test5, geom = c("x", "y"), crs = crs(test))
temp <- terra::rasterize(test6, y = test, field = "ShrubCover_dec", fun = function(x) mean(x, na.rm = TRUE)) %>% 
  terra::aggregate(fact = 32, fun = function(x) mean(x, na.rm = TRUE))
  

  # compare to raw data 
  
temp2 <- 
  rasterize(dat2, y = test, field = "ShrubCover_dec", fun = function(x) mean(x, na.rm = TRUE)) %>% 
  terra::aggregate(fact = 32, fun = function(x) mean(x, na.rm = TRUE) )

par(mfrow = c(1,2))
plot(temp, 
     xlim = c(-2000000, 2500000), 
     ylim = c(-2000000, 1000000))
plot(temp2, 
     xlim = c(-2000000, 2500000), 
     ylim = c(-2000000, 1000000))

## Look the same! 

