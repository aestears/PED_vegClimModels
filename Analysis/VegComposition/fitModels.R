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
# load data ---------------------------------------------------------------

# load vegetation data
vegDat <- read.csv("./data/DataForAnalysis.csv")
# load meteorological data
dayMet <- read.csv("./data/dayMet/sampledDataForAnalysis.csv")

# calculate climate variables ---------------------------------------------
climVar <- dayMet %>% 
  mutate(totalAnnPrecip = rowSums(.[52:63]),
         totalAnnSwe = rowSums(.[28:39]),
         meanAnnTmax = rowMeans(.[2:13]),
         meanAnnTmin = rowMeans(.[16:27]),
         annMaxTmax = pmap_dbl(.[2:13], max),
         annMinTmin = pmap_dbl(.[16:27], min)) %>% 
  select(year, Long, Lat, totalAnnPrecip, totalAnnSwe, meanAnnTmax, meanAnnTmin, annMaxTmax, annMinTmin) 
  
# add met data to veg data ------------------------------------------------
# make veg data projection the same as raster data
test <-  rast("./data/dayMet/rawMonthlyData/orders/70e0da02b9d2d6e8faa8c97d211f3546/Daymet_Monthly_V4R1/data/daymet_v4_prcp_monttl_na_1980.tif")
points_sf <- st_read(dsn = "./data/DataForAnalysisPoints/", layer = "vegCompPoints")
v <- vect(vegDat[,c("Lon", "Lat")], geom = c("Lon", "Lat"), crs=crs(points_sf))
y <- project(v, crs(test)) 
vegDat[,c("Lon", "Lat")] <- st_coordinates(st_as_sf(y))
plot(vegDat$Lon, vegDat$Lat)

allDat <- vegDat %>% 
  left_join(climVar, by = c("Year" = "year", "Lon" = "Long", "Lat" = "Lat"))
  
# plot those that have missing data ... not sure why? there are points that don't have corresponding cliamate data
good <- allDat[!is.na(allDat$meanAnnTmax),]

bad <- allDat[is.na(allDat$meanAnnTmax),]
## add in the clim data for rounded lat/long points
bad <- bad %>% 
  select(-totalAnnPrecip, 
         -totalAnnSwe, 
         -meanAnnTmax, 
         -meanAnnTmin,
         -annMaxTmax, 
         -annMinTmin) %>% 
  mutate(Lat = round(Lat, 1),
         Lon = round(Lon, 1)) %>% 
  left_join((climVar %>% 
               mutate(Lat = round(Lat, 1),
                      Long = round(Long, 1))), by = c("Year" = "year", "Lon" = "Long", "Lat" = "Lat")) %>% 
  unique()
## are still some messed up variables...will need to figure out later
badBad <- bad[!is.na(bad$annMinTmin),]

allDat <- rbind(bad, good)


# Convert %covers to proportions of cover for each class ------------------

allDat$TotalCover <- rowSums(allDat[,c("ShrubCover", "HerbCover", "TotalGramCover", "TotalTreeCover")])
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

## for these models, I think we want to use a tweedie 
# distribution... ? gamma distribution doesn't allow zeros 
#This paper may be a good option for a more complex modelling approach: https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/2041-210X.13262
# This is another, multivariate option!? (Damgaard, 2015) in Ecological Informatics
# This is a simpler approach... (Damgaard, 2018) in Journal of Ecology

# tree cover
# get tree data w/ all NAs removed
treeDat <- allDat[!is.na(allDat$TotalTreeCover) & 
                    !is.na(allDat$TotalTreeProportion) &
                    !is.na(allDat$annMinTmin) & 
                    !is.na(allDat$annMaxTmax) &
                    !is.na(allDat$meanAnnTmin) &
                    !is.na(allDat$meanAnnTmax) &
                    !is.na(allDat$totalAnnSwe) & 
                    !is.na(allDat$totalAnnPrecip),]
treeDat$Year <- as.factor(treeDat$Year)
## make correlation matrix of covariates
corTree <- cor(treeDat[,c("annMinTmin", "annMaxTmax", "meanAnnTmin", "meanAnnTmax", "totalAnnSwe", "totalAnnPrecip", "Year")])
corrplot::corrplot(corTree, method = "number")

# convert 0s in tree proportion to .0001
treeDat[treeDat$TotalTreeProportion==0, "TotalTreeProportion"] <- .0001
# convert 1s in tree proportion to .9999
treeDat[treeDat$TotalTreeProportion==1, "TotalTreeProportion"] <- .9999
treeDat[treeDat$TotalTreeCover==0,"TotalTreeCover"] <- .0001
# logit transform tree proportion data
treeDat$TotalTreeProportion_logit <- car::logit(treeDat$TotalTreeProportion, percents = FALSE)

# tree beta distribution
tree.glm.beta <- betareg(formula = TotalTreeProportion ~ totalAnnPrecip + meanAnnTmax + meanAnnTmin ,
                         data = treeDat, link = c("logit"), link.phi = NULL,
                         type = c("ML"))

#betareg:::summary.betareg(tree.glm.beta)
coefficients(tree.glm.beta)
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


