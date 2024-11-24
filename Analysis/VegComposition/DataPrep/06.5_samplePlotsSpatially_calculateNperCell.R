#///////////////////
# Averaging values from plots w/in each dayMet gridcell
# Alice Stears
# 8/19/24
#///////////////////


# Load packages -----------------------------------------------------------

library(tidyverse)
library(sf)
library(terra)
library(exactextractr)
library(stars)
library(tidyterra)
library(ggpubr)

# Load data ---------------------------------------------------------------

# get veg data
dat <- readRDS("./Data_processed/CoverData/DataForModels.rds") 
# dayMet extent 
test <-  rast("./Data_raw/dayMet/rawMonthlyData/orders/70e0da02b9d2d6e8faa8c97d211f3546/Daymet_Monthly_V4R1/data/daymet_v4_prcp_monttl_na_1980.tif") %>% 
  terra::project(crs(dat))

# %>% 
#   st_transform(crs(test)) %>% 
#   st_drop_geometry() %>% 
#   select(Year, UniquID:Day, Lat:annVPD_max_meanAnnAvg_1yrAnom) %>% 
#   mutate(Year_fac = as.factor(Year)) #%>% 
#   #st_buffer(.01)

## make sure the projections are the same
# add met data to veg data ------------------------------------------------
# make veg data projection the same as raster data
v <- vect(st_drop_geometry(dat)[,c("Lon", "Lat")], geom = c("Lon", "Lat"), crs=crs(dat))
y <- project(v, crs(test))
# make sure the veg data is in the appropriate projection
dat2 <- dat %>%
  #st_buffer(.01) %>%
  sf::st_transform(crs(y)) %>% 
  # st_buffer(.1)
  st_centroid() %>% 
  mutate("Year_char" = as.character(Year)) %>% 
  select(Year, Year_char, UniquID:Day, Lat:geometry)# %>% 
#terra::vect()


# make sure the raster data is in the appropriate projection
test <- test %>%
  terra::project(crs(y))
st_crs(dat) == st_crs(test)

#(xTest = st_rasterize(dat2[,"ShrubCover"], st_as_stars(st_bbox(test), nx = ncol(test), ny = nrow(test), values = NA_real_)))

# rasterize values --------------------------------------------------------
# get the names of the columns w/ data we want to rasterize
layerNames <- dat %>% 
  st_drop_geometry() %>% 
  dplyr::select(c(ShrubCover, TotalTreeCover, TotalHerbaceousCover, CAMCover, BareGroundCover, 
                  ForbCover_prop:NeedleLeavedTreeCover_prop)) %>% 
  names() 
years <- sort(unique(dat$Year))


## for now, only look at 5 first tier cover variables
# dat# function to rasterize and average cover values
test2_a <- lapply(layerNames[1:2], FUN = function(x) {
  temp <- terra::rasterize(dat2, y = test$daymet_v4_prcp_monttl_na_1980_1, field = x, fun = function(x) {
   tempTemp <- sum(!is.na(x))
    return(tempTemp)
    },
    #, na.rm = TRUE#function(x) mean(x, na.rm = TRUE)
                           by = "Year") 
  names(temp) <- paste0("ID_", years)
  return(temp)
}
)

test2_b <- lapply(layerNames[3:4], FUN = function(x) {
  temp <- terra::rasterize(dat2, y = test$daymet_v4_prcp_monttl_na_1980_1, field = x, fun = function(x) {
    tempTemp <- sum(!is.na(x))
    return(tempTemp)
  },
  #, na.rm = TRUE#function(x) mean(x, na.rm = TRUE)
  by = "Year") 
  
  names(temp) <- paste0("ID_",years)
  return(temp)
}
)

test2_ab <- c(test2_a, test2_b)
rm(test2_a, test2_b)
gc()

test2_c <- lapply(layerNames[5], FUN = function(x) {
  temp <- terra::rasterize(dat2, y = test$daymet_v4_prcp_monttl_na_1980_1, field = x, fun = function(x) {
    tempTemp <- sum(!is.na(x))
    return(tempTemp)
  },
  #, na.rm = TRUE#function(x) mean(x, na.rm = TRUE)
  by = "Year") 

  names(temp) <- paste0("ID_",years)
  return(temp)
}
)
test2 <- c(test2_ab, test2_c)
rm(test2_ab, test2_c)
gc()

names(test2) <- layerNames[1:5]
# 
# test2_c <- lapply(layerNames[5:6], FUN = function(x) {
#   temp <- terra::rasterize(dat2, y = test, field = x, fun = length#function(x) mean(x, na.rm = TRUE)
#                            , by = "Year")
#   
#   names(temp) <- paste0("ID_",years)
#   return(temp)
# }
# )
# test2_abc <- c(test2_ab, test2_c)
# rm(test2_ab, test2_c)
# gc()
# 
# test2_d <- lapply(layerNames[7], FUN = function(x) {
#   temp <- terra::rasterize(dat2, y = test, field = x, fun = length#function(x) mean(x, na.rm = TRUE)
#                            , by = "Year")
#   
#   names(temp) <- paste0("ID_",years)
#   return(temp)
# }
# )
# test2_abcd <- c(test2_abc, test2_d)
# rm(test2_abc, test2_d)
# gc()
# 
# test2_e <- lapply(layerNames[8], FUN = function(x) {
#   temp <- terra::rasterize(dat2, y = test, field = x, fun = length#function(x) mean(x, na.rm = TRUE)
#                            , by = "Year")
#   
#   names(temp) <- paste0("ID_",years)
#   return(temp)
# }
# )
# 
# test2_abcde <- c(test2_abcd, test2_e)
# rm(test2_abcd, test2_e)
# gc()
# 
# test2_f <- lapply(layerNames[9], FUN = function(x) {
#   temp <- terra::rasterize(dat2, y = test, field = x, fun = length#function(x) mean(x, na.rm = TRUE)
#                            , by = "Year")
#   
#   names(temp) <- paste0("ID_",years)
#   return(temp)
# }
# )
# 
# test2_abcdef <- c(test2_abcde, test2_f)
# rm(test2_abcde, test2_f)
# gc()
# 
# test2_g <- lapply(layerNames[10], FUN = function(x) {
#   temp <- terra::rasterize(dat2, y = test, field = x, fun = length#function(x) mean(x, na.rm = TRUE)
#                            , by = "Year")
#   
#   names(temp) <- paste0("ID_",years)
#   return(temp)
# }
# )


# Visualize results -------------------------------------------------------
## shrub
temp <- test2[[1]] %>% 
  terra::aggregate(fact = 8, fun = "max", na.rm = TRUE) %>% 
  terra::crop(ext(-2060750, 2855250, -2395500, 1284500)) %>% 
  terra::app(fun = function(x) {
    if(sum(!is.na(x))>0 ) {
      if(max(x, na.rm = TRUE) > 0) {
        tempTemp <- max(x, na.rm = TRUE)
      } else {
        tempTemp <- NA
      }
    } else {
    tempTemp <- NA
    }
    return(tempTemp)
    })


(shrubN_plot_full <- ggplot() + 
    geom_spatraster(data = temp, aes(), na.rm = TRUE) +
    theme_minimal() + 
    scale_fill_viridis_c(option = "H", guide = guide_colorbar(title = "% cover"), 
                         limits = c(1,315)) +
    #facet_wrap(~lyr) +
    ggtitle("Maximum # of plots in a grid cell with Shrub Cover data", subtitle = "full range of values" )+ 
    theme_minimal())


(shrubN_plot_trimmed <- ggplot() + 
    geom_spatraster(data = temp, aes(), na.rm = TRUE) +
    theme_minimal() + 
    scale_fill_viridis_c(option = "H", guide = guide_colorbar(title = "trimmed scale"), 
                         limits = c(1,5)) +
    #facet_wrap(~lyr) +
    ggtitle("Maximum # of plots in a grid cell with Shrub Cover data", subtitle = "truncated" ) + 
    theme_minimal())

temp2 <- data.frame(terra::values(temp), "order" = 1:nrow(terra::values(temp)))
(shrubN_hist_full <- ggplot(temp2) + 
    geom_histogram(aes(lyr.1))+ 
    theme_minimal()  + 
    xlab("Number of plots") + 
    ylab("Frequency")#+ 
    #xlim(0,315) +
    #ylim(0,60000)
)
(shrubN_hist_trimmed <- ggplot(temp2) + 
    geom_histogram(aes(lyr.1)) + 
    xlim(c(0.5,5)) + 
    #ylim(0,7500) +
    theme_minimal() + 
    xlab("Number of plots") + 
    ylab("Frequency")
)

# total trees 

temp <- test2[[2]] %>% 
  terra::aggregate(fact = 8, fun = "max", na.rm = TRUE) %>% 
  terra::crop(ext(-2060750, 2855250, -2395500, 1284500)) %>% 
  terra::app(fun = function(x) {
    if(sum(!is.na(x))>0 ) {
      if(max(x, na.rm = TRUE) > 0) {
        tempTemp <- max(x, na.rm = TRUE)
      } else {
        tempTemp <- NA
      }
    } else {
      tempTemp <- NA
    }
    return(tempTemp)
  })


(treeN_plot_full <- ggplot() + 
    geom_spatraster(data = temp, aes(), na.rm = TRUE) +
    theme_minimal() + 
    scale_fill_viridis_c(option = "H", guide = guide_colorbar(title = "% cover"), 
                         limits = c(1,24)) +
    #facet_wrap(~lyr) +
    ggtitle("Maximum # of plots in a grid cell with Tree Cover data", subtitle = "full range of values" )+ 
    theme_minimal())


(treeN_plot_trimmed <- ggplot() + 
    geom_spatraster(data = temp, aes(), na.rm = TRUE) +
    theme_minimal() + 
    scale_fill_viridis_c(option = "H", guide = guide_colorbar(title = "trimmed scale"), 
                         limits = c(1,5)) +
    #facet_wrap(~lyr) +
    ggtitle("Maximum # of plots in a grid cell with Tree Cover data", subtitle = "truncated" ) + 
    theme_minimal())

temp2 <- data.frame(terra::values(temp), "order" = 1:nrow(terra::values(temp)))
(TreeN_hist_full <- ggplot(temp2) + 
    geom_histogram(aes(lyr.1), binwidth = 1)+ 
    theme_minimal()  + 
    xlab("Number of plots") + 
    ylab("Frequency")
)
(TreeN_hist_trimmed <- ggplot(temp2) + 
    geom_histogram(aes(lyr.1), binwidth = 1) + 
    xlim(c(0.5,5))+ 
    theme_minimal() + 
    xlab("Number of plots") + 
    ylab("Frequency")
)


# total herbaceous cover

temp <- test2[[3]] %>% 
  terra::aggregate(fact = 8, fun = "max", na.rm = TRUE) %>% 
  terra::crop(ext(-2060750, 2855250, -2395500, 1284500)) %>% 
  terra::app(fun = function(x) {
    if(sum(!is.na(x))>0 ) {
      if(max(x, na.rm = TRUE) > 0) {
        tempTemp <- max(x, na.rm = TRUE)
      } else {
        tempTemp <- NA
      }
    } else {
      tempTemp <- NA
    }
    return(tempTemp)
  })


(herbN_plot_full <- ggplot() + 
    geom_spatraster(data = temp, aes(), na.rm = TRUE) +
    theme_minimal() + 
    scale_fill_viridis_c(option = "H", guide = guide_colorbar(title = "% cover"), 
                         limits = c(1,320)) +
    #facet_wrap(~lyr) +
    ggtitle("Maximum # of plots in a grid cell with Total Herbaceous Cover data", subtitle = "full range of values" )+ 
    theme_minimal())


(herbN_plot_trimmed <- ggplot() + 
    geom_spatraster(data = temp, aes(), na.rm = TRUE) +
    theme_minimal() + 
    scale_fill_viridis_c(option = "H", guide = guide_colorbar(title = "trimmed scale"), 
                         limits = c(1,5)) +
    #facet_wrap(~lyr) +
    ggtitle("Maximum # of plots in a grid cell with Total Herbaceous Cover data", subtitle = "truncated" ) + 
    theme_minimal())

temp2 <- data.frame(terra::values(temp), "order" = 1:nrow(terra::values(temp)))
(herbN_hist_full <- ggplot(temp2) + 
    geom_histogram(aes(lyr.1), binwidth = 1)+ 
    theme_minimal()  + 
    xlab("Number of plots") + 
    ylab("Frequency")
)
(herbN_hist_trimmed <- ggplot(temp2) + 
    geom_histogram(aes(lyr.1), binwidth = 1) + 
    xlim(c(0.5,5))+ 
    theme_minimal() + 
    xlab("Number of plots") + 
    ylab("Frequency")
)

# total CAM cover

temp <- test2[[4]] %>% 
  terra::aggregate(fact = 8, fun = "max", na.rm = TRUE) %>% 
  terra::crop(ext(-2060750, 2855250, -2395500, 1284500)) %>% 
  terra::app(fun = function(x) {
    if(sum(!is.na(x))>0 ) {
      if(max(x, na.rm = TRUE) > 0) {
        tempTemp <- max(x, na.rm = TRUE)
      } else {
        tempTemp <- NA
      }
    } else {
      tempTemp <- NA
    }
    return(tempTemp)
  })


(camN_plot_full <- ggplot() + 
    geom_spatraster(data = temp, aes(), na.rm = TRUE) +
    theme_minimal() + 
    scale_fill_viridis_c(option = "H", guide = guide_colorbar(title = "% cover"), 
                         limits = c(1,36)) +
    #facet_wrap(~lyr) +
    ggtitle("Maximum # of plots in a grid cell with CAM Cover data", subtitle = "full range of values" )+ 
    theme_minimal())

(camN_plot_trimmed <- ggplot() + 
    geom_spatraster(data = temp, aes(), na.rm = TRUE) +
    theme_minimal() + 
    scale_fill_viridis_c(option = "H", guide = guide_colorbar(title = "trimmed scale"), 
                         limits = c(1,5)) +
    #facet_wrap(~lyr) +
    ggtitle("Maximum # of plots in a grid cell with CAM Cover data", subtitle = "truncated" ) + 
    theme_minimal())

temp2 <- data.frame(terra::values(temp), "order" = 1:nrow(terra::values(temp)))
(camN_hist_full <- ggplot(temp2) + 
    geom_histogram(aes(lyr.1), binwidth = 1)+ 
    theme_minimal()  + 
    xlab("Number of plots") + 
    ylab("Frequency")
)
(camN_hist_trimmed <- ggplot(temp2) + 
    geom_histogram(aes(lyr.1), binwidth = 1) + 
    xlim(c(0.5,5))+ 
    theme_minimal() + 
    xlab("Number of plots") + 
    ylab("Frequency")
)

# total bare ground cover
temp <- test2[[5]] %>% 
  terra::aggregate(fact = 8, fun = "max", na.rm = TRUE) %>% 
  terra::crop(ext(-2060750, 2855250, -2395500, 1284500)) %>% 
  terra::app(fun = function(x) {
    if(sum(!is.na(x))>0 ) {
      if(max(x, na.rm = TRUE) > 0) {
        tempTemp <- max(x, na.rm = TRUE)
      } else {
        tempTemp <- NA
      }
    } else {
      tempTemp <- NA
    }
    return(tempTemp)
  })


(bareGroundN_plot_full <- ggplot() + 
    geom_spatraster(data = temp, aes(), na.rm = TRUE) +
    theme_minimal() + 
    scale_fill_viridis_c(option = "H", guide = guide_colorbar(title = "% cover"), 
                         limits = c(1,166)) +
    #facet_wrap(~lyr) +
    ggtitle("Maximum # of plots in a grid cell with Bare Ground Cover data", subtitle = "full range of values" )+ 
    theme_minimal())


(bareGroundN_plot_trimmed <- ggplot() + 
    geom_spatraster(data = temp, aes(), na.rm = TRUE) +
    theme_minimal() + 
    scale_fill_viridis_c(option = "H", guide = guide_colorbar(title = "trimmed scale"), 
                         limits = c(1,5)) +
    #facet_wrap(~lyr) +
    ggtitle("Maximum # of plots in a grid cell with Bare Ground Cover data", subtitle = "truncated" ) + 
    theme_minimal())

temp2 <- data.frame(terra::values(temp), "order" = 1:nrow(terra::values(temp)))
(bareGroundN_hist_full <- ggplot(temp2) + 
    geom_histogram(aes(lyr.1), binwidth = 1)+ 
    theme_minimal() + 
    xlab("Number of plots") + 
    ylab("Frequency")
)
(bareGroundN_hist_trimmed <- ggplot(temp2) + 
    geom_histogram(aes(lyr.1), binwidth = 1) + 
    xlim(c(0.5,5))+ 
    theme_minimal() + 
    xlab("Number of plots") + 
    ylab("Frequency")
)

# 

# put figures together into one -------------------------------------------

bigPlot_N <- ggarrange(ggarrange(shrubN_plot_full,    shrubN_hist_full, 
          shrubN_plot_trimmed, shrubN_hist_trimmed, 
          ncol = 2, nrow = 2, 
          widths = c(2,1)),
ggarrange(treeN_plot_full,    TreeN_hist_full, 
          treeN_plot_trimmed, TreeN_hist_trimmed, 
          ncol = 2, nrow = 2, 
          widths = c(2,1)),
ggarrange(herbN_plot_full,    herbN_hist_full, 
          herbN_plot_trimmed, herbN_hist_trimmed, 
          ncol = 2, nrow = 2, 
          widths = c(2,1)),
ggarrange(camN_plot_full,    camN_hist_full, 
          camN_plot_trimmed, camN_hist_trimmed, 
          ncol = 2, nrow = 2, 
          widths = c(2,1)),
ggarrange(bareGroundN_plot_full, bareGroundN_hist_full, 
          bareGroundN_plot_trimmed, bareGroundN_hist_trimmed, 
          ncol = 2, nrow = 2, 
          widths = c(2,1)), 
ncol = 1)

# forbs, herbaceous, Shrub, and CAM figures
bigPlot_N %>% 
  ggexport(
    filename = "./Figures/coverDatFigures/NumberOfPlotsPerGridcellFigure.pdf",
    width = 14, height = 35)
