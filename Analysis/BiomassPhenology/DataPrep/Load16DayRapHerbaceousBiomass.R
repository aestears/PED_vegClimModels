#///////////////////
# Loading 16 day herbaceous biomass data from RAP
# Alice Stears
# 10/30/24
#///////////////////


# load packages -----------------------------------------------------------

library(tidyverse)


# read in data ------------------------------------------------------------
fileNames <- list.files("./Data_raw/RAP_BiomassData/RAP_biomassPhenology/")

for (i in 2:length(fileNames)) {
  datTemp <- read.csv(paste0("./Data_raw/RAP_BiomassData/RAP_biomassPhenology/",fileNames[i]))
  # tidy up dataset 
  datTemp2 <- datTemp %>% 
    mutate(Year = as.integer(str_sub(system.index, start = 1, end = 4)),
           DOY = as.integer(str_sub(system.index, start = 5, end = 7)),
           index = str_sub(system.index, start = 9, end = 28),
           Lat = as.numeric(str_split_i(
             str_split_i(
               str_split_i(.geo, pattern = "\\[", i = 2), 
               pattern = "\\]",
               i = 1
             ),
             pattern = ",",
             i = 2
           )
           ),
           Lon = as.numeric(str_split_i(
             str_split_i(
               str_split_i(.geo, pattern = "\\[", i = 2), 
               pattern = "\\]",
               i = 1
             ),
             pattern = ",",
             i = 1
           )
           ))
  
  sites <- as.matrix(unique(datTemp2$index)[1:1000])
  years <- as.matrix(unique(datTemp2$Year))
  
  # identify the DOY of peak and trough of each location in each year
  dates <- apply(sites, MARGIN = 1, FUN = function(x) {
    print(x)
    temp2 <- apply(years, MARGIN = 1, FUN = function(y) {
      print(y)
      # get data just for this year and plot
      temp <- datTemp2[datTemp2$index == x & datTemp2$Year == y,]
      # make sure that the DOY is in order
      temp <- temp[order(temp$DOY),]
      # remove dates w/ NAs in mean
      temp <- temp[!is.na(temp$mean),]
      
      ggplot(temp) +
        geom_line(aes(x = DOY, y = mean))
      
      ## if all of the values are the same, then escape to the next y (the data is messed up somehow)
      if (length(unique(temp$mean)) != 1) {
        ## first stab at determining whether the time series is bimodal
        
        # calculate the difference in biomass from previous measurement date
        temp$trend <- temp$mean - lag(temp$mean)
        # does the trend go up for more than 3 time steps after it first goes
        # down for three time steps? #AES the 3 time step thing is arbitrary...
        # can change if so
        whichPositive <- temp$trend>0
        whichNegative <- temp$trend<0
        # what is the DOY of the overall max... 
        # identify the DOY of the max 
          max <- temp[which(temp$mean == max(temp$mean, na.rm = TRUE)),"DOY"]
          # identify the DOY of the min
          min <- temp[which(temp$mean == min(temp$mean, na.rm = TRUE)),"DOY"]
          # identify the DOY when biomass exceeds 80th percentile 
          # the the DOY must be earlier than the date of the max
          above80 <- temp[which(temp$mean > max(temp$mean, na.rm = TRUE)*.8),"DOY"]
          above80_DOY <- min(above80)
          # identify the DOY when biomass goes below the 80th percentile
          # the the DOY must be later than the date of the max
          below80 <- temp[which(temp$mean < max(temp$mean, na.rm = TRUE)*.8),"DOY"]
          below80_DOY <- below80[which(below80 > max[1])[1]]
          # identify the DOY when biomass exceeds 20th percentile 
          # the the DOY must be later than the date of the min
          above20 <- temp[which(temp$mean > max(temp$mean, na.rm = TRUE)*.2),"DOY"]
          above20_DOY <- min(above20)
          # identify the DOY when biomass goes below the 20th percentile
          # the the DOY must be later than the date of the max
          below20 <- temp[which(temp$mean < max(temp$mean, na.rm = TRUE)*.2),"DOY"]
          below20_DOY <- below20[which(below20 > max[1])[1]]
          if (below20_DOY)
            
            
            return(data.frame("Site" = x,
                              "Year" = y,
                              "modality" = "unimodal",
                              "max_DOY" = paste(max, collapse = "_"),
                              "min_DOY" = paste(min, collapse = "_"), 
                              "above80_DOY" = above80_DOY,
                              "below80_DOY" = below80_DOY,
                              "above20_DOY" = above20_DOY, 
                              "below20_DOY" = below20_DOY
            )
            )
          }
        
        }
      
    )
    return(list_rbind(temp2))
  }) %>% 
    list_rbind()

  
}

str(dates)

