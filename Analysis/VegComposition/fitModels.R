#///////////////////
# Fitting models to vegetation composition data
# Alice Stears
# 6/26/24
#///////////////////


# load packages -----------------------------------------------------------

library(tidyverse)
library(terra)
library(statmod)
library(betareg)
library(glmmTMB)
library(corrplot)
library(sf)

# load data ---------------------------------------------------------------

# load vegetation data
vegDat <- read.csv("./data/DataForAnalysis.csv")
# load meteorological data
dayMet <- read.csv("./data/dayMet/climateValuesForAnalysis_final.csv")
# remove monthly values 
dayMet2 <- dayMet %>% 
  select(-names(dayMet)[c(2:13, 16:63)])
# make dayMet spatial 
dayMet3 <- dayMet2 %>% 
  st_as_sf(coords = c("Long", "Lat"))

# add met data to veg data ------------------------------------------------
# make veg data projection the same as raster data
test <-  rast("./data/dayMet/rawMonthlyData/orders/70e0da02b9d2d6e8faa8c97d211f3546/Daymet_Monthly_V4R1/data/daymet_v4_prcp_monttl_na_1980.tif")
points_sf <- st_read(dsn = "./data/DataForAnalysisPoints/", layer = "vegCompPoints")
v <- vect(vegDat[,c("Lon", "Lat")], geom = c("Lon", "Lat"), crs=crs(points_sf))
y <- project(v, crs(test)) 
vegDat[,c("Lon", "Lat")] <- st_coordinates(st_as_sf(y))
plot(vegDat$Lon, vegDat$Lat)
vegDat2 <- st_as_sf(vegDat, coords = c("Lon", "Lat"))
vegDat2 <- st_set_crs(vegDat2, value = st_crs(y))

dayMet3 <- st_set_crs(dayMet3, value = st_crs(y))

allDat <- vegDat2 %>% 
  st_buffer(.001) %>% 
  st_join(dayMet3, by = c("Year" = "year", left = TRUE)) %>% 
  filter(Year == year)

#AES missing 600 points... not too sure why?? address later (after ESA)
# 
# # plot those that have missing data ... not sure why? there are points that don't have corresponding climate data
# good <- allDat[!is.na(allDat$tmax_annAvg),]
# 
# bad <- allDat[is.na(allDat$tmax_annAvg),]
# ## add in the clim data for rounded lat/long points
# bad <- bad %>% 
#   select(-names(bad)[25:82]) %>% 
#   mutate(Lat = round(Lat, 1),
#          Lon = round(Lon, 1)) %>% 
#   left_join((climVar %>% 
#                mutate(Lat = round(Lat, 1),
#                       Long = round(Long, 1))), by = c("Year" = "year", "Lon" = "Long", "Lat" = "Lat")) %>% 
#   unique()
# ## are still some messed up variables...will need to figure out later
# badBad <- bad[!is.na(bad$tmin_annAvg),]
# 
# allDat <- rbind(bad, good)


# Convert %covers to proportions of cover for each class ------------------

allDat$TotalCover <- rowSums(st_drop_geometry(allDat[,c("ShrubCover", "HerbCover", "TotalGramCover", "TotalTreeCover")]))
allDat <- allDat %>% 
  mutate(ShrubProportion = ShrubCover/TotalCover,
         HerbProportion = HerbCover/TotalCover,
         TotalGramProportion = TotalGramCover/TotalCover, 
         C3GramProportion = C3GramCover/TotalCover,
         C4GramProportion = C4GramCover/TotalCover,
         TotalTreeProportion = TotalTreeCover/TotalCover, 
         ConifTreeProportion = ConifTreeCover/TotalCover, 
         AngioTreeProportion = AngioTreeCover/TotalCover
         )
# fit basic models  -------------------------------------------------------

## for these models, I think we want to use a beta
# distribution... ? gamma distribution doesn't allow zeros 
#This paper may be a good option for a more complex modelling approach: https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/2041-210X.13262
# This is another, multivariate option!? (Damgaard, 2015) in Ecological Informatics
# This is a simpler approach... (Damgaard, 2018) in Journal of Ecology

# tree cover
# get tree data w/ all NAs removed
treeDat <- allDat[!is.na(allDat$TotalTreeCover) & 
                    #!is.na(allDat$TotalTreeProportion) &
                    !is.na(allDat$swe_meanAnnAvg_5yr) & 
                    !is.na(allDat$tmin_meanAnnAvg_5yr) &
                    !is.na(allDat$tmax_meanAnnAvg_5yr) &
                    !is.na(allDat$vp_meanAnnAvg_5yr) &
                    !is.na(allDat$prcp_meanAnnTotal_5yr) & 
                    !is.na(allDat$T_warmestMonth_meanAnnAvg_5yr) & 
                    !is.na(allDat$T_coldestMonth_meanAnnAvg_5yr) & 
                    !is.na(allDat$precip_wettestMonth_meanAnnAvg_5yr) & 
                    !is.na(allDat$precip_driestMonth_meanAnnAvg_5yr) & 
                    !is.na(allDat$precip_Seasonality_meanAnnAvg_5yr) & 
                    !is.na(allDat$PrecipTempCorr_meanAnnAvg_5yr)& 
                    !is.na(allDat$aboveFreezing_month_meanAnnAvg_5yr)& 
                    !is.na(allDat$isothermality_meanAnnAvg_5yr),] %>% 
  st_drop_geometry()
treeDat$Year <- as.factor(treeDat$Year)
## make correlation matrix of covariates
corTree <- cor(treeDat[,c("swe_meanAnnAvg_5yr", "tmin_meanAnnAvg_5yr", "tmax_meanAnnAvg_5yr",
                          "vp_meanAnnAvg_5yr", "prcp_meanAnnTotal_5yr", "T_warmestMonth_meanAnnAvg_5yr", "T_coldestMonth_meanAnnAvg_5yr",
                          "precip_wettestMonth_meanAnnAvg_5yr", "precip_driestMonth_meanAnnAvg_5yr", "precip_Seasonality_meanAnnAvg_5yr",
                          "PrecipTempCorr_meanAnnAvg_5yr", "aboveFreezing_month_meanAnnAvg_5yr", "isothermality_meanAnnAvg_5yr")])
corrplot::corrplot(corTree, method = "number", )
# remove "aboveFreezing_month_meanAnnAvg_5yr", which is highly correlated w/ other temp variables
corTree <- cor(treeDat[,c("swe_meanAnnAvg_5yr", "tmin_meanAnnAvg_5yr", "tmax_meanAnnAvg_5yr",
                          "vp_meanAnnAvg_5yr", "prcp_meanAnnTotal_5yr", "T_warmestMonth_meanAnnAvg_5yr", "T_coldestMonth_meanAnnAvg_5yr",
                          "precip_wettestMonth_meanAnnAvg_5yr", "precip_driestMonth_meanAnnAvg_5yr", "precip_Seasonality_meanAnnAvg_5yr",
                          "PrecipTempCorr_meanAnnAvg_5yr",  "isothermality_meanAnnAvg_5yr")])
corrplot::corrplot(corTree, method = "number", )

# convert 0s in tree proportion to .0001
treeDat[treeDat$TotalTreeProportion==0, "TotalTreeProportion"] <- .0001
# convert 1s in tree proportion to .9999
treeDat[treeDat$TotalTreeProportion==1, "TotalTreeProportion"] <- .9999
treeDat[treeDat$TotalTreeCover==0,"TotalTreeCover"] <- .0001

## convert values for cover from percentages to proportions
treeDat$TotalTreeCover_dec <- treeDat$TotalTreeCover/100
#adjust to have no 0s and 1s (so glm w/ beta distribution works)
treeDat[treeDat$TotalTreeCover_dec==0, "TotalTreeCover_dec"] <- .0001
treeDat[treeDat$TotalTreeCover_dec==1 | (treeDat$TotalTreeCover_dec>1 & treeDat$TotalTreeCover_dec<1.5), "TotalTreeCover_dec"] <- .999
# change really large values to NAs... 
treeDat[treeDat$TotalTreeCover_dec>1, "TotalTreeCover_dec"] 


treeDat[treeDat$TotalTreeCover_dec>=1.50,c("TotalTreeCover_dec")] <- NA
hist(treeDat$TotalTreeCover_dec)

# logit transform tree proportion data
treeDat$TotalTreeProportion_logit <- car::logit(treeDat$TotalTreeCover_dec, percents = FALSE)

## fit a tree model w/ all covariates
# tree beta distribution
tree.glm.beta_all <- betareg(formula = TotalTreeCover_dec ~ swe_meanAnnAvg_5yr + tmin_meanAnnAvg_5yr+tmax_meanAnnAvg_5yr+ 
vp_meanAnnAvg_5yr+prcp_meanAnnTotal_5yr + T_warmestMonth_meanAnnAvg_5yr + T_coldestMonth_meanAnnAvg_5yr + 
precip_wettestMonth_meanAnnAvg_5yr + precip_driestMonth_meanAnnAvg_5yr + precip_Seasonality_meanAnnAvg_5yr+ 
PrecipTempCorr_meanAnnAvg_5yr+isothermality_meanAnnAvg_5yr ,
                         data = treeDat, link = c("logit"), link.phi = NULL,
                         type = c("ML"))

#betareg:::summary.betareg(tree.glm.beta)
coefficients(tree.glm.beta_all)

# tree beta distribution w/ allowing a fixed effect of year
tree.glm.beta2 <- betareg(formula = TotalTreeProportion ~ 
                            totalAnnPrecip + meanAnnTmax + meanAnnTmin + as.factor(Year),
                          data = treeDat, link = c("logit"), link.phi = NULL,
                          type = c("ML"))
coefficients(tree.glm.beta2)

AIC(tree.glm.beta, tree.glm.beta2)

#tree beta distribution w/ allowing phi to vary across years (using glmmTMB)
tree.glm.beta3 <- glmmTMB::glmmTMB(TotalTreeProportion ~ 
                                     totalAnnPrecip + meanAnnTmax + meanAnnTmin + (1|Year)
                                   , data = treeDat, family=beta_family(link="logit"))

summary(tree.glm.beta3)


# For comparison this applies a logit-transformation to the empirical proportions and
# then uses a standard linear regression model.
tree.lm.logit<-lm(TotalTreeProportion_logit~totalAnnPrecip + meanAnnTmax + meanAnnTmin,data=treeDat)
summary(tree.lm.logit)

#this is code for applying a naive linear model applied to the raw proportions as done
#in the simulation study
tree.lm.raw<-lm(TotalTreeProportion~totalAnnPrecip + meanAnnTmax + meanAnnTmin,data=treeDat)
summary(tree.lm.raw)

plot(treeDat$TotalTreeProportion, treeDat$meanAnnTmax)

# shrub cover
# forb cover
# gram cover


